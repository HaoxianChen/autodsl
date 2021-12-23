package synthesis.activelearning

import synthesis.{Constant, Examples, NumberType, Relation, SymbolType, Tuple, Type}
import synthesis.rulebuilder.ConstantBuilder

import scala.util.Random

class ExampleGenerator(inputRels: Set[Relation], staticConfigRelations: Set[Relation],
                       edb: Examples, idb: Examples, minConstantSize: Int = 3) {
  private val exampleTranslator = new ExampleTranslator(inputRels, Set())
  private val constantMap = buildConstantMap()

  private val staticConfigTuples: Set[Tuple] = {
    if (staticConfigRelations.isEmpty) {
      Set()
    }
    else {
      require(staticConfigRelations.forall(_.signature.last==ExampleTranslator.instanceIdType))
      val allStaticTuples = edb.toTuples().filter(t => staticConfigRelations.contains(t.relation))
      // Keep only one instance
      val groupByInstanceId = allStaticTuples.groupBy(_.fields.last.name.toInt)
      groupByInstanceId.values.head
    }
  }
  private val tupleSizes: Map[Relation, (Int, Int)] = edb.elems map {
    /** Count the number of tuples per relation per instance */
    case (rel, tuples) =>
      val tuplesById = tuples.groupBy(ts => exampleTranslator.getInstanceId(rel, ts))
      val sizes = tuplesById.map {
        case (_, ts) => ts.size
      }
      // rel -> (sizes.min, sizes.max)
      val maxSize: Int = if (sizes.nonEmpty) sizes.max else 0
      rel -> (0, maxSize)
  }

  private val random: Random = new Random()
  val instanceIdType: Type = NumberType(s"InstanceId")

  def buildConstantMap(): Map[Type, Set[Constant]] = {
    def makeNewConstants(constantSet: Set[Constant], n: Int): Set[Constant] = {
      val _type: Type = constantSet.head._type
      _type match {
        case _: NumberType => {
          val m = constantSet.map(_.name.toInt).max
          (m+1 to m+n).map(i => Constant(i.toString, _type)).toSet
        }
        case _: SymbolType => {
          (1 to n).map(i => Constant(s"${'"'}_${_type.name.toLowerCase()}${i}${'"'}", _type)).toSet
        }
      }
    }

    def expandConstantSet(constantSet: Set[Constant]): Set[Constant] = {
      if (constantSet.size < minConstantSize) {
        /** Create new elements */
        val n = minConstantSize - constantSet.size
        val newConstants: Set[Constant] =constantSet ++ makeNewConstants(constantSet, n)
        assert(newConstants.size == minConstantSize, s"${newConstants}")
        newConstants
      }
      else constantSet
    }

    val edbMap = ConstantBuilder.getAllConstantPool(edb, idb)

    val allTypes: Set[Type] = inputRels.flatMap(_.signature).filterNot(_.name == s"InstanceId")
    require(allTypes == edbMap.keySet, s"$allTypes, ${edbMap.keySet}")

    edbMap map {
      case (_type, constantSet) => {_type -> expandConstantSet(constantSet)}
    }
  }

  def sampleFromMap(_type: Type): Constant  = {
    // random.setSeed(seed)
    val constantList = constantMap(_type).toVector
    constantList(random.nextInt(constantList.size))
  }

  def generateRandomInputs(numNewExamples: Int): ExamplePool = {
    def genNewInstance(id: Int): TupleInstance = {
      // val tuples = inputRels.flatMap(rel => genNewTuples(rel, id))
      val newTuples: Set[Tuple] = inputRels.diff(staticConfigRelations).flatMap(rel => genNewTuples(rel, id))
      val tuples: Set[Tuple] = newTuples ++ staticConfigTuples.map(t => exampleTranslator.assignTupleId(t,id))
      TupleInstance(tuples, id)
    }

    def genNewTuples(relation: Relation, instanceId: Int): Set[Tuple] = {
      val (min, max) = tupleSizes(relation)
      val sizes = min to max

      /** sample a size */
      //random.setSeed(instanceId)
      val numTuples: Int = sizes(random.nextInt(sizes.size))

      def newTuple(relation: Relation, instanceId: Int): Tuple = {
        val sig = relation.signature
        val fields: List[Constant] = sig.zipWithIndex.map{
          case (_type, i) => _type match {
            case NumberType(s"InstanceId") => Constant(instanceId.toString, instanceIdType)
            case t: Type => {
              // val seed = instanceId * i
              sampleFromMap(t)
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
