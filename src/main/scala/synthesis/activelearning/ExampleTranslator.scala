package synthesis.activelearning

import synthesis.{Constant, NumberType, Problem, Relation, Tuple, Type}

class ExampleTranslator(inputRels: Set[Relation], outputRels: Set[Relation])  {
  private val instanceIdType: Type = ExampleTranslator.instanceIdType

  private val instanceIdIndices: Map[Relation, Int] = {
    val allRelations: Set[Relation] = inputRels++outputRels
    def getInx(relation: Relation): Int = {
      val sig = relation.signature
      require(sig.contains(instanceIdType))
      sig.indexOf(instanceIdType)
    }
    allRelations.map (r => r->getInx(r)).toMap
  }

  def updateProblem(problem: Problem, newExamples: ExampleInstance): Problem = {
    require(newExamples.nonEmpty)
    val nextId: Int = if (problem.edb.elems.nonEmpty) {
      problem.edb.toTuples().map(t => getInstanceId(t)).max + 1
    }
    else 0
    val nextExample = assignInstanceId(newExamples, nextId)
    val relevantIdb = nextExample.output.filter(t => problem.outputRels.contains(t.relation))
    problem.addEdb(nextExample.input).addIdb(relevantIdb)
  }

  def getInstanceIdIndex(relation: Relation): Int = instanceIdIndices(relation)

  def assignInstanceId(tupleInstance: TupleInstance, id: Int) :TupleInstance = {
    val tuples = tupleInstance.tuples
    assignInstanceId(tuples, id)
  }

  def assignInstanceId(tuples: Set[Tuple], id: Int): TupleInstance = {
    val newTuples: Set[Tuple] = tuples.map(t => assignTupleId(t, id))
    TupleInstance(newTuples, id)
  }

  def assignInstanceId(example: ExampleInstance, id: Int) :ExampleInstance = {
    val newEdb = assignInstanceId(example.input, id)
    val newIdb = assignInstanceId(example.output, id)
    new ExampleInstance(newEdb.tuples, newIdb.tuples, id)
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

object ExampleTranslator {
  val instanceIdType: Type = NumberType(s"InstanceId")
}