package synthesis.rulebuilder

import synthesis._
import synthesis.activelearning.ExampleTranslator
import synthesis.rulebuilder.OutputAggregator.{allAggIndices, allIndexIndices}

case class AggregateLiteral(relation: Relation, fields: List[Parameter], aggregator: Aggregator, indices: List[Int], outputIndex: Int) extends Literal {
  require(relation.signature.size == fields.size)
  require(indices.size + 1 == relation.signature.size)
  require(indices.forall(i => i>=0 && i<fields.size))
  require(outputIndex>=0 && outputIndex<fields.size)
  require(!indices.contains(outputIndex))

  override def toString: String = aggregator.literalToString(this)

  def rename(binding: Map[Parameter, Parameter]): Literal = {
    val newFields = _rename(binding)
    this.copy(fields=newFields)
  }
  def renameRelation(newRels: Map[Relation, Relation]): Literal = {
    val newRel = newRels.getOrElse(this.relation, this.relation)
    this.copy(relation=newRel)
  }

  def getOutput: Variable = fields(outputIndex) match {
    case v: Variable => v
    case _: Constant => throw new Exception(s"output field cannot be Constant instance")
  }

  def toAggFields: List[Parameter] = indices.map(i => fields(i))
}
object  AggregateLiteral {
  def apply(relation: Relation, fields: List[Parameter], aggregator: OutputAggregator): AggregateLiteral  = {
    require(relation.signature.size == fields.size)
    val n = relation.signature.size
    val indices = (0 until n-1).toList
    val aggIndex = n-1
    AggregateLiteral(relation, fields, aggregator, indices, aggIndex)
  }

  def apply(relation: Relation, fields: List[Parameter], aggregator: InputAggregator): AggregateLiteral  = {
    require(relation.signature.size == fields.size)
    val n = relation.signature.size
    val indices = (0 until n-1).toList
    val outputIndex = n-1
    AggregateLiteral(relation, fields, aggregator, indices, outputIndex)
  }
}

abstract class Aggregator {
  def name: String
  def relation: Relation
  def indices: List[Int]

  require(relation.signature.contains(ExampleTranslator.instanceIdType))
  val indexTypes: List[Type] = indices.map(i=>relation.signature(i))
  require(indexTypes.contains(ExampleTranslator.instanceIdType))

  def literalToString(literal: AggregateLiteral): String

  def getAggRules: Set[Rule]
  def getAggLiteral(toAgg: Literal): AggregateLiteral
  def getAggHeadRel: Relation
  def aggHeadRelName: String
}

abstract class OutputAggregator extends Aggregator {
  def aggIndex: Int
  require(!indices.contains(aggIndex))
  require(indices.forall(i => i<relation.signature.size && i >= 0))
  require(aggIndex<relation.signature.size && aggIndex >= 0)


  def relToAgg: Relation = Relation(s"_${this.relation.name}", this.relation.signature)

  def getAggProgram(p0: Program): Program = {
    val renamedProgram = p0.renameRelation(Map(this.relation -> relToAgg))
    val aggRules = getAggRules
    Program(renamedProgram.rules++aggRules)
  }

  def _getAggRule: Rule = {
    val toAgg: Literal = SimpleRuleBuilder.newUnboundedLiteral(Set(), relToAgg)
    val aggLit: AggregateLiteral = getAggLiteral(toAgg)
    val body: Set[Literal] = Set(toAgg, aggLit)

    val fields = indices.map(i => toAgg.fields(i)) :+ aggLit.getOutput
    val head: Literal = SimpleLiteral(getAggHeadRel, fields)
    Rule(head, body)
  }

  def _getSelectRule: Rule = {
    val toAgg: Literal = SimpleRuleBuilder.newUnboundedLiteral(Set(), relToAgg)

    /** Bind the toAgg fields to the aggHead fields */
    val aggFields = (this.indices :+ this.aggIndex).map(i => toAgg.fields(i))
    val aggHead: Literal = SimpleLiteral(getAggHeadRel, aggFields)

    val head = SimpleLiteral(this.relation, toAgg.fields)
    val body = Set(toAgg, aggHead)
    Rule(head, body)
  }

  def getAggLiteral(toAgg: Literal): AggregateLiteral = {
    /** Signature the agg relation's signature plus the output variable's type */
    val aggType = relation.signature(aggIndex)
    val sig: List[Type] = relation.signature :+ aggType
    val rel = Relation(this.name, sig)

    /** fields, only bind to the agg index, leaving others as new variable */
    val output: Variable = Variable(s"min_${aggType.name.toLowerCase()}", aggType)

    // val fields: List[Parameter] = indices.map(i=>toAgg.fields(i)) :+ output
    val fields: List[Parameter] = toAgg.fields.zipWithIndex.map {
      case (v, i) => {
        if (this.indices.contains(i)) v
        else SimpleRuleBuilder.newVar(Set(toAgg), v._type) // new variable here
      }
    } :+ output

    AggregateLiteral(rel, fields, this)
  }

  def getAggHeadRel: Relation = {
    val signature = (this.indices :+ this.aggIndex).map(i => this.relation.signature(i))
    Relation(aggHeadRelName, signature)
  }

}

case class ArgMin(relation: Relation, indices: List[Int], aggIndex: Int) extends OutputAggregator {
  val name: String = "argMin"

  def getAggRules: Set[Rule] = Set(_getAggRule, _getSelectRule)
  def aggHeadRelName: String = s"min${relation.signature(aggIndex).name}"

  def literalToString(aggLit: AggregateLiteral): String = {
    val output = aggLit.getOutput
    val aggType = relation.signature(aggIndex)
    val aggVar = Variable("c", aggType)

    val aggregator = aggLit.aggregator match {
      case a: OutputAggregator => a
      case _: InputAggregator => throw new Exception(s"Unexpected InputAggregator type.")
    }
    val toAggField: List[Parameter] = aggLit.toAggFields.zipWithIndex.map {
      case (f,i) => if (i==aggregator.aggIndex) aggVar else f
    }
    s"${output} = min ${aggVar} : ${aggregator.relToAgg.name}(${toAggField.mkString(",")})"
  }
}
object ArgMin {
  def allInstances(problem: Problem): Set[OutputAggregator] = {
    problem.outputRels.flatMap(_allInstancesByRel)
  }

  def _allInstancesByRel(relation: Relation): Set[ArgMin] = {
    val allAggregators =  allAggIndices(relation).flatMap(i => {
      val allIndices = allIndexIndices(relation, i)
      allIndices.map(ix => ArgMin(relation, ix, i))
    })
    allAggregators
  }
}
case class ArgMax(relation: Relation, indices: List[Int], aggIndex: Int) extends OutputAggregator {
  val name: String = "argMax"

  def getAggRules: Set[Rule] = Set(_getAggRule, _getSelectRule)
  def aggHeadRelName: String = s"max${relation.signature(aggIndex).name}"

  def literalToString(aggLit: AggregateLiteral): String = {
    val output = aggLit.getOutput
    val aggType = relation.signature(aggIndex)
    val aggVar = Variable("c", aggType)

    val aggregator = aggLit.aggregator match {
      case a: OutputAggregator => a
      case _: InputAggregator => throw new Exception(s"Unexpected InputAggregator type.")
    }

    val toAggField: List[Parameter] = aggLit.toAggFields.zipWithIndex.map {
      case (f,i) => if (i==aggregator.aggIndex) aggVar else f
    }
    s"${output} = max ${aggVar} : ${aggregator.relToAgg.name}(${toAggField.mkString(",")})"
  }
}
object ArgMax {
  def allInstances(problem: Problem): Set[OutputAggregator] = {
    problem.outputRels.flatMap(_allInstancesByRel)
  }

  def _allInstancesByRel(relation: Relation): Set[ArgMax] = {
    val allAggregators =  allAggIndices(relation).flatMap(i => {
      val allIndices = allIndexIndices(relation, i)
      allIndices.map(ix => ArgMax(relation, ix, i))
    })
    allAggregators
  }
}

object OutputAggregator {
  private val instanceIdType = ExampleTranslator.instanceIdType
  def getInstanceIdIndex(relation: Relation): Int = relation.signature.indexOf(instanceIdType)
  def allAggIndices(relation: Relation): Set[Int] = {
    relation.signature.zipWithIndex.flatMap {
      case (t, i) => if (t.isInstanceOf[NumberType] && t != instanceIdType) Some(i) else None
    }.toSet
  }
  def _allIndexIndices(relation: Relation, aggIndex: Int, nIndices: Int): Set[List[Int]] = {
    require(nIndices>=1)
    val n = relation.signature.size
    require(n>=2)
    val instanceIdIndex: Int = getInstanceIdIndex(relation)
    require(instanceIdIndex != aggIndex)

    val remainingIndices: List[Int] = (0 until n).
          filter(i => i!=aggIndex).
          filter(i => i !=instanceIdIndex).
          toList
    assert(remainingIndices.size == n-2, s"$relation, $aggIndex, $instanceIdIndex, ${remainingIndices}")
    val comb = remainingIndices.combinations(nIndices-1)
    // Add instanceId to each combination
    comb.map(l => l :+ instanceIdIndex).toSet
  }
  def allIndexIndices(relation: Relation, aggIndex: Int): Set[List[Int]] = {
    val n = relation.signature.size
    (1 to n-2).flatMap(i => _allIndexIndices(relation, aggIndex, i)).toSet
  }
}
