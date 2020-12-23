package synthesis

import com.typesafe.scalalogging.Logger
import synthesis.rulebuilder.ConstantBuilder

import scala.math.log
import scala.util.Random

case class TupleInstance(tuples: Set[Tuple], instanceId: Int)
object TupleInstance {
  def apply(instanceId: Int): TupleInstance = new TupleInstance(Set(), instanceId)
}
case class ExampleInstance(input: Set[Tuple], output: Set[Tuple], instanceId: Int)
object ExampleInstance{
  def apply(): ExampleInstance = new ExampleInstance(Set(), Set(), -1)
}

case class ExamplePool(instances: Set[TupleInstance]) {
  require(noDuplicateId())
  def noDuplicateId(): Boolean = {
    val idList = instances.toList.map(_.instanceId)
    idList.distinct.size == idList.size
  }

  def toExampleMap: Examples = {
    val allTuples: Set[Tuple] = instances.flatMap(_.tuples)
    val map: Map[Relation, Set[List[Constant]]] = allTuples.groupBy(_.relation).map {
      case (rel, ts) => rel -> ts.map(_.fields)
    }
    Examples(map)
  }
}

class ExampleTranslator(inputRels: Set[Relation], outputRels: Set[Relation])  {

  val instanceIdType: Type = NumberType(s"InstanceId")
  private val instanceIdIndices: Map[Relation, Int] = {
    val allRelations: Set[Relation] = inputRels++outputRels
    def getInx(relation: Relation): Int = {
      val sig = relation.signature
      require(sig.contains(instanceIdType))
      sig.indexOf(instanceIdType)
    }
    allRelations.map (r => r->getInx(r)).toMap
  }

  def getInstanceIdIndex(relation: Relation): Int = instanceIdIndices(relation)

  def assignInstanceId(tupleInstance: TupleInstance, id: Int) :TupleInstance = {
    val tuples = tupleInstance.tuples
    val newTuples: Set[Tuple] = tuples.map(t => assignTupleId(t, id))
    tupleInstance.copy(tuples=newTuples, instanceId=id)
  }

  def assignTupleId(tuple: Tuple, id: Int): Tuple = {
    val idx = instanceIdIndices(tuple.relation)
    val idConstant: Constant = Constant(id.toString, instanceIdType)
    val newFields: List[Constant] = tuple.fields.updated(idx, idConstant)
    tuple.copy(fields = newFields)
  }

  def getInstanceId(tuple:Tuple): Int = {
    val i = getInstanceIdIndex(tuple.relation)
    tuple.fields(i).name.toInt
  }

  def getInstanceId(relation: Relation, fields:List[Constant]): Int = {
    val i = getInstanceIdIndex(relation)
    fields(i).name.toInt
  }
}

class ExampleGenerator(inputRels: Set[Relation], edb: Examples) {
  private val exampleTranslator = new ExampleTranslator(inputRels, Set())
  private val constantMap: Map[Type, Set[Constant]] = ConstantBuilder.getConstantPool(edb, maxConstantPoolSize = 10)
  private val tupleSizes: Map[Relation, (Int, Int)] = edb.elems map {
    /** Count the number of tuples per relation per instance */
    case (rel, tuples) =>
      val tuplesById = tuples.groupBy(ts => exampleTranslator.getInstanceId(rel, ts))
      val sizes = tuplesById.map {
        case (_, ts) => ts.size
      }
      rel -> (sizes.min, sizes.max)
  }

  private val random: Random = new Random()
  val instanceIdType: Type = NumberType(s"InstanceId")

  def sampleFromMap(_type: Type, seed: Int): Constant  = {
    // random.setSeed(seed)
    val constantList = constantMap(_type).toVector
    constantList(random.nextInt(constantList.size))
  }

  def generateRandomInputs(numNewExamples: Int): ExamplePool = {
    def genNewInstance(id: Int): TupleInstance = {
      val tuples = inputRels.flatMap(rel => genNewTuples(rel, id))
      TupleInstance(tuples, id)
    }

    def genNewTuples(relation: Relation, instanceId: Int): Set[Tuple] = {
      val (min, max) = tupleSizes(relation)
      val sizes = min to max

      /** sample a size */
      random.setSeed(instanceId)
      val numTuples: Int = sizes(random.nextInt(sizes.size))

      def newTuple(relation: Relation, instanceId: Int): Tuple = {
        val sig = relation.signature
        val fields: List[Constant] = sig.zipWithIndex.map{
          case (_type, i) => _type match {
            case NumberType(s"InstanceId") => Constant(instanceId.toString, instanceIdType)
            case t: Type => {
              val seed = instanceId * i
              sampleFromMap(t, seed)
            }
          }
        }
        Tuple(relation, fields)
      }

      (1 to numTuples).map(_ => newTuple(relation, instanceId)).toSet
    }

    /** Generate N input instances */
    val instanceIds = (1 to numNewExamples).toList
    val newInstances: List[TupleInstance] = instanceIds.map(genNewInstance)

    ExamplePool(newInstances.toSet)
  }


}

class ActiveLearning(p0: Problem, numNewExamples: Int = 20) {
  private val logger = Logger("Active-learning")

  private val exampleTranslator = new ExampleTranslator(p0.inputRels, p0.outputRels)

  private val exampleGenerator = new ExampleGenerator(p0.inputRels, p0.edb)
  private val edbPool: ExamplePool = exampleGenerator.generateRandomInputs(numNewExamples)

  def go(): Program = {
    /** Handle one output relation at a time */
    val solutions: Set[Program] = p0.outputRels map {
      rel => interactiveLearning(rel)
    }
    /** Merge solutions altogether */
    solutions.foldLeft(Program())((p1,p2)=>Program(p1.rules++p2.rules))
  }

  def interactiveLearning(outRel: Relation): Program = {
    /** Only do synthesis on one output relation. */
    val relevantIdb: Examples = p0.idb.filterByRelation(outRel)
    var problem = p0.copy(outputRels=Set(outRel), idb=relevantIdb)

    var candidates: Set[Program] = Set()
    var newExamples: Option[ExampleInstance] = Some(ExampleInstance())

    do {
      // add new examples
      problem = updateProgram(problem, newExamples.get)

      candidates = synthesize(problem, outRel, candidates)
      logger.debug(s"${candidates.size} candidate programs")

      if (candidates.size > 1) {
        newExamples = disambiguate(candidates)
        if (newExamples.isEmpty) logger.debug(s"Failed to differentiate candidate programs.")
      }
    }
    while (candidates.size > 1 && newExamples.isDefined)

    candidates.maxBy(scoreProgram)
  }

  def updateProgram(problem: Problem, newExamples: ExampleInstance): Problem = {
    problem.addEdb(newExamples.input).addIdb(newExamples.output)
  }

  def synthesize(problem: Problem, outRel: Relation, candidates: Set[Program], minPrograms: Int = 3): Set[Program] = {
    val evaluator = Evaluator(problem)

    def isProgramValid(program: Program, problem: Problem): Boolean = {
      val idb = evaluator.eval(program)
      idb.subsetOf(problem.idb.toTuples())
    }

    val validCandidates: Set[Program] = candidates.filter(p=>isProgramValid(p, problem))

    if (validCandidates.size < minPrograms) {
      val newCandidates = SynthesisAllPrograms(problem).go()(outRel)
      require(newCandidates.forall(p=>isProgramValid(p, problem)))
      newCandidates
    }
    else {
      validCandidates
    }
  }

  def differentiate(candidates: Set[Program]): Option[TupleInstance] = {
    require(candidates.size > 1)
    def entropy(tuples: List[TupleInstance]): Double = {
      val counts: List[Int] = tuples.groupBy(identity).map{
        case (_, l) => l.size
      }.toList
      val hist: List[Double] = counts.map(c => 1.0 * c / counts.sum)
      hist.map(p => - p * log(p)).sum
    }
    assert(edbPool.instances.size == numNewExamples)

    val evalResults: Set[(TupleInstance, List[TupleInstance])] = evalCandidatePrograms(edbPool, candidates)
    val edbEntropy: Set[(TupleInstance, Double)] = evalResults.map {
      case (t1, ts) => (t1, entropy(ts))
    }
    val bestEdb = edbEntropy.maxBy(_._2)

    if (bestEdb._2 > 0) {
      logger.debug(s"New edb: $bestEdb")
      Some(bestEdb._1)
    }
    else None
  }

  def evalCandidatePrograms(edbPool: ExamplePool, candidates: Set[Program]): Set[(TupleInstance, List[TupleInstance])] = {
    val newProblem = p0.copy(edb = edbPool.toExampleMap, idb=Examples())
    def evalProgram(program: Program): Map[Int, TupleInstance] = {
      val evaluator = Evaluator(newProblem)
      val idb = evaluator.eval(program)
      idb.groupBy(exampleTranslator.getInstanceId) map {
        case (i,ts) => i -> TupleInstance(ts, i)
      }
    }

    val evalResults: List[Map[Int, TupleInstance]] = candidates.toList.map(evalProgram)

    def getResults(instance: TupleInstance): List[TupleInstance] = {
      val tid = instance.instanceId
      evalResults.map(m => m.getOrElse(tid, TupleInstance(tid)))
    }
    edbPool.instances.map(t => (t, getResults(t)))
  }

  def disambiguate(candidates: Set[Program]): Option[ExampleInstance] = {
    require(candidates.size > 1)
    val nextEdb = differentiate(candidates)
    if (nextEdb.isDefined) {
      /** todo: need to pass the oracle program here. */
      val nextIdb: Examples = ???
      Some(())
    }
    None
  }

  def scoreProgram(program: Program): Int = {
    /** todo: use better metric. */
    -program.rules.size
  }

}
