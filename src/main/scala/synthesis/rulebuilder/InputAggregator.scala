package synthesis.rulebuilder

import synthesis.{Evaluator, Literal, NumberType, Parameter, Problem, Program, Relation, Rule, SimpleLiteral, Type, Variable}

abstract class InputAggregator() extends Aggregator {
  def getOutputType: Type

  def preprocess(problem: Problem): Problem = {
    val preProcessRules: Set[Rule] = getAggRules
    val newRel: Relation = getAggHeadRel
    require(preProcessRules.forall(_.head.relation == newRel))

    val p1 = problem.addType(getOutputType)
    val evaluator = Evaluator(p1)
    val newEdb = evaluator.eval(Program(preProcessRules))

    val newProblem = p1.addInputRelation(newRel).addEdb(newEdb)
    newProblem
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

  def getAggRules: Set[Rule] = {
    val toAgg: Literal = SimpleRuleBuilder.newUnboundedLiteral(Set(), this.relation)
    val aggLit: AggregateLiteral = getAggLiteral(toAgg)

    val headFields: List[Parameter] = this.indices.map(i => toAgg.fields(i)) :+ aggLit.getOutput
    val head = SimpleLiteral(getAggHeadRel, headFields)
    val aggRule = Rule(head, Set(toAgg, aggLit))
    Set(aggRule)
  }

  def literalToString(aggLit: AggregateLiteral): String = {
    val output = aggLit.getOutput

    val aggregator = aggLit.aggregator match {
      case a: InputAggregator => a
      case _: OutputAggregator => throw new Exception(s"Unexpected OutputAggregator type.")
    }
    s"${output} = count : ${aggregator.relation.name}(${aggLit.toAggFields.mkString(",")})"
  }

  def getAggLiteral(toAgg: Literal): AggregateLiteral = {
    /** Signature the agg relation's signature plus the output variable's type */
    val sig: List[Type] = relation.signature :+ AggCount.countType
    val rel = Relation(this.name, sig)

    /** fields, only bind to the agg index, leaving others as new variable */
    val output: Variable = Variable(s"c_", AggCount.countType)

    val fields: List[Parameter] = toAgg.fields.zipWithIndex.map {
      case (v, i) => {
        if (this.indices.contains(i)) v
        else SimpleRuleBuilder.newVar(Set(toAgg), v._type) // new variable here
      }
    } :+ output

    AggregateLiteral(rel, fields, this)
  }

}
object AggCount {
  val countType: Type = NumberType(s"Count")

  def allInstances(problem: Problem): Set[InputAggregator] = {
    problem.inputRels.flatMap(_allInstances)
  }
  def _allInstances(relation: Relation): Set[InputAggregator] = {
    def _allInstances(relation: Relation, numIndices: Int): Set[InputAggregator] = {
      val allIndices: Set[List[Int]] = relation.signature.indices.toList.combinations(numIndices).toSet
      allIndices.map(ix => AggCount(relation, ix))
    }
    val minIndex = 0
    val maxIndex = relation.signature.size - 1
    (minIndex to maxIndex).toSet.flatMap(i => _allInstances(relation,i))
  }
}