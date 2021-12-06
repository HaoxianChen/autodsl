package synthesis.rulebuilder

import synthesis.{Evaluator, Literal, NumberType, Parameter, Problem, Program, Relation, Rule, SimpleLiteral, Type, Variable}

abstract class InputAggregator() extends Aggregator {
  def getOutputType: Type

  def preprocess(problem: Problem): Problem = {
    val preProcessRules: Set[Rule] = getAggRules
    val newRel: Relation = getAggHeadRel
    require(preProcessRules.forall(_.head.relation == newRel))

    /** If every instance only has fewer than three elements, skip aggregation.  */
    val aggRel = this.relation
    val tuples = problem.edb.toInstances(aggRel)
    val tupleCounts = tuples.map(_.tuples.size)
    val minTuplesPerInstance = 3
    if (tupleCounts.max >= minTuplesPerInstance) {
      val p1 = problem.addType(getOutputType)
      val evaluator = Evaluator(p1)
      val newEdb = evaluator.eval(Program(preProcessRules))

      // val newProblem = p1.addInputRelation(newRel).addEdb(newEdb).addInputAggregator(this)
      val newProblem = p1.addInputRelation(newRel).addEdb(newEdb)
      newProblem
    }
    else {
      problem
    }

  }

  def getOutputVar: Variable

  def getAggLiteral(toAgg: Literal): AggregateLiteral = {
    /** Signature the agg relation's signature plus the output variable's type */
    val sig: List[Type] = relation.signature :+ getOutputType
    val rel = Relation(this.name, sig)

    /** fields, only bind to the agg index, leaving others as new variable */
    // val fields: List[Parameter] = toAgg.fields.zipWithIndex.map {
    //   case (v, i) => {
    //     if (this.indices.contains(i)) v
    //     else SimpleRuleBuilder.newVar(Set(toAgg), v._type) // new variable here
    //   }
    // } :+ getOutputVar
    def getNewVar(params: List[Parameter], _type: Type): Variable = {
      val paramCounts: Map[Type, Int] = params.groupBy(_._type).map {
        case (t, ps) => t -> ps.size
      }
      val c = paramCounts(_type)
      val v = Variable(_type, c)
      require(!params.contains(v))
      v
    }
    var fields: List[Parameter] = List()
    for ((v,i) <- toAgg.fields.zipWithIndex) {
      if (this.indices.contains(i)) fields :+= v
      else {
        val newVar: Variable = getNewVar(toAgg.fields ++ fields, v._type)
        fields :+= newVar
      }
    }
    fields :+= getOutputVar

    AggregateLiteral(rel, fields, this)
  }

  def getAggRules: Set[Rule] = {
    val toAgg: Literal = SimpleRuleBuilder.newUnboundedLiteral(Set(), this.relation)
    val aggLit: AggregateLiteral = getAggLiteral(toAgg)

    val headFields: List[Parameter] = indices.map(i => toAgg.fields(i)) :+ aggLit.getOutput
    val head = SimpleLiteral(getAggHeadRel, headFields)
    val aggRule = Rule(head, Set(toAgg, aggLit))
    Set(aggRule)
  }
}

case class AggCount(relation: Relation, indices: List[Int]) extends InputAggregator {
  def name: String = s"AggCount"
  def getOutputType: Type = AggCount.countType

  def aggHeadRelName: String = s"${relation.name}_cnt${indices.mkString("")}"
  def getAggHeadRel: Relation = {
    val outputType: Type = AggCount.countType
    val sig: List[Type] = indices.map(i => relation.signature(i)) :+ outputType
    Relation(aggHeadRelName, sig)
  }

  def literalToString(aggLit: AggregateLiteral): String = {
    val output = aggLit.getOutput

    val aggregator = aggLit.aggregator match {
      case a: InputAggregator => a
      case _: OutputAggregator => throw new Exception(s"Unexpected OutputAggregator type.")
    }
    s"${output} = count : ${aggregator.relation.name}(${aggLit.toAggFields.mkString(",")})"
  }

  def getOutputVar: Variable = Variable(s"max_", getOutputType)
}
object AggCount {
  val countType: Type = NumberType(s"Count")

  def allInstances(problem: Problem): Set[InputAggregator] = {
    problem.inputRels.flatMap(_allInstances)
  }
  def _allInstances(relation: Relation): Set[InputAggregator] = {
    val instanceType = NumberType("InstanceId")
    val hasInstanceId = relation.signature.contains(instanceType)
    def getIsntanceIdIdx(sig: List[Type]): Int = sig.indexOf(instanceType)
    val signature = relation.signature.filterNot(_ == instanceType)

    def _allInstances(relation: Relation, numIndices: Int): Set[InputAggregator] = {
      val allIndices: Set[List[Int]] = signature.indices.toList.combinations(numIndices).toSet
      val allIndices2 = if (hasInstanceId) {
        val idx = getIsntanceIdIdx(relation.signature)
        allIndices.map(_:+idx)
      }
      else allIndices
      allIndices2.map(ix => AggCount(relation, ix))
    }
    val minIndex = 0
    val maxIndex = signature.size - 1
    (minIndex to maxIndex).toSet.flatMap(i => _allInstances(relation,i))
  }
}

case class AggMax(relation: Relation, indices: List[Int], aggIndex: Int) extends InputAggregator {
  require(!indices.contains(aggIndex))
  def getOutputType: Type = relation.signature(aggIndex)
  require(getOutputType.isInstanceOf[NumberType])

  def name: String = s"AggMax"

  def literalToString(aggLit: AggregateLiteral): String = {
    val output = aggLit.getOutput

    val aggregator = aggLit.aggregator match {
      case a: InputAggregator => a
      case _: OutputAggregator => throw new Exception(s"Unexpected OutputAggregator type.")
    }

    val aggParam: Parameter = aggLit.fields(aggIndex)
    val ans = s"${output} = max $aggParam: ${aggregator.relation.name}(${aggLit.toAggFields.mkString(",")})"
    ans
  }

  def getOutputVar: Variable = Variable(s"max_", getOutputType)

  def getAggHeadRel: Relation = {
    val sig: List[Type] = indices.map(i => relation.signature(i)) :+ getOutputType
    Relation(aggHeadRelName, sig)
  }

  def aggHeadRelName: String = {
    val idx = indices :+ aggIndex
    s"${relation.name}_max${idx.mkString("")}"
  }
}

object AggMax {
  def allInstances(problem: Problem): Set[InputAggregator] = {
    problem.inputRels.flatMap(_allInstances)
  }

  def splitList[T](seq: Seq[T]): Seq[(Seq[T], T)] = {
    require(seq.toSet.size == seq.size, s"input contains duplicate: $seq")
    val ans = seq.zipWithIndex.map{
      case (x, i) => {
        val h = seq.take(i)
        val t =  seq.takeRight(seq.size-1-i)
        val l = h++t
        assert(!l.contains(x))
        (l, x)
      }
    }
    ans
  }

  def _allInstances(relation: Relation): Set[InputAggregator] = {
    val instanceType = NumberType("InstanceId")
    val hasInstanceId = relation.signature.contains(instanceType)
    require(!hasInstanceId || relation.signature.last == instanceType,
      s"instance ID type must be at the end of the signature.")
    def getIsntanceIdIdx(sig: List[Type]): Int = sig.indexOf(instanceType)
    val signature = relation.signature.filterNot(_ == instanceType)

    def _allInstances(relation: Relation, numIndices: Int): Set[InputAggregator] = {
      val allIndices: Set[List[Int]] = signature.indices.toList.combinations(numIndices).toSet
      val allIndices2 = if (hasInstanceId) {
        val idx = getIsntanceIdIdx(relation.signature)
        allIndices.map(_:+idx)
      }
      else allIndices
      // allIndices2.map(ix => AggMax(relation, ix, aggIndex))
      def _allInstances2(relation: Relation, indices: List[Int]): Set[InputAggregator] = {
        def f: (Seq[Int], Int) => Option[AggMax] = {
          case (idx, aggIndex) => {
            val aggType = relation.signature(aggIndex)
            if (aggType.isInstanceOf[NumberType] && aggType != instanceType) {
              Some(AggMax(relation, idx.toList, aggIndex))
            }
            else {
              None
            }
          }
        }
        splitList(indices).toSet.flatMap(f.tupled)
      }
      allIndices2.flatMap(ix => _allInstances2(relation, ix))
    }
    val minIndex = 1
    val maxIndex = 2
    (minIndex to maxIndex).toSet.flatMap(i => _allInstances(relation,i))
  }

}