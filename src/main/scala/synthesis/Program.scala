package synthesis

import synthesis.rulebuilder.{AggregateLiteral, FunctorLiteral, FunctorSpec, AggMax}

import scala.collection.mutable

sealed abstract class Type() {
  def name: String
  def declString: String
  override def toString: String = s"$name"
  override val hashCode: Int = this.##
}

case class NumberType(name: String) extends Type {
  override def declString: String = s".type $name <: number\n"
}
case class SymbolType(name: String) extends Type {
  override def declString: String = s".type $name <: symbol\n"
}

sealed abstract class Parameter {
  def name: String
  def _type: Type

  override def toString: String = s"$name"
}
case class Variable(name: String, _type: Type) extends Parameter
object Variable {
  def apply(_type: Type, id: Int): Variable = {
    val varName = s"${_type.name.toLowerCase()}${id}"
    Variable(varName, _type)
  }

}
case class Constant(name: String, _type: Type) extends Parameter

abstract class Literal {
  def relation: Relation
  def fields: List[Parameter]

  require(relation.signature.size == fields.size)

  def toString: String
  def rename(binding: Map[Parameter, Parameter]): Literal
  def renameRelation(newRels: Map[Relation, Relation]): Literal

  def _rename(binding: Map[Parameter, Parameter]): List[Parameter] = fields.map(p => binding.getOrElse(p, p))
}

case class SimpleLiteral(relation: Relation, fields: List[Parameter]) extends Literal {
  override def toString: String = {
    val field_str: String = fields.map {
      case v: Variable => v.toString
      case c: Constant => c.toString
    }.mkString(",")
    s"${relation.name}($field_str)"
  }
  def rename(binding: Map[Parameter, Parameter]): Literal = {
    val newFields = _rename(binding)
    this.copy(fields=newFields)
  }
  def renameRelation(newRels: Map[Relation, Relation]): Literal = {
    val newRel = newRels.getOrElse(this.relation, this.relation)
    this.copy(relation=newRel)
  }
}

case class Tuple(relation: Relation, fields: List[Constant]) {
  override def toString: String = {
    val field_str: String = fields.map(_.toString).mkString(",")
    s"${relation.name}($field_str)"
  }
}

/* Relations */
case class Relation(name: String, signature: List[Type]) {
  override def toString: String = {
    val sig_str = signature.map(_.toString).mkString(",")
    s"${name}(${sig_str})"
  }
}

case class Rule(head: Literal, body: Set[Literal], negations: Set[Literal]=Set()) extends Ordered[Rule] {
  require(negations.subsetOf(body), s"negations: ${negations},\n body: ${body}")

  override def compare(that: Rule): Int = {
    if (this.body.size < that.body.size) -1
    else if (this.body.size > that.body.size) 1
    else 0
  }

  // def containsConstants: Boolean = {
  //   def _containsConstants(literal: Literal) = literal.fields.exists(_.isInstanceOf[Constant])
  //   _containsConstants(head) || body.exists(_containsConstants)
  // }

  override def toString: String = {
    val mr = this.maskUngroundVars()
    if (mr.body.nonEmpty) {
      // val simpleLiterals: Set[Literal] = mr.getPositiveLiterals()
      val simpleLiterals: Set[Literal] = mr.body.diff(mr.negations)
      val body_str: String = {
        (simpleLiterals.map(_.toString) ++ mr.negations.map("!" + _.toString)).mkString(",")
      }
      s"${mr.head} :- ${body_str}."
    }
    else {
      s"${mr.head}."
    }
  }

  def maskUngroundVars(): Rule = {
    // replace unground variables in the body with "_"
    // val ungroundVars: Set[Variable] = freeVariables().diff(getHeadVars().toSet)
    // replace unground variables with "_"
    val ungroundVars: Set[Variable] = freeVariables()
    val bindings = mutable.Map.empty[Parameter, Parameter]
    for (uv <- ungroundVars) {
      val maskedVar = Variable("_", uv._type)
      bindings.update(uv, maskedVar)
    }
    rename(bindings.toMap)
  }

  def getPositiveLiterals(): Set[Literal] = body.diff(negations).filter(_.isInstanceOf[SimpleLiteral])

  def addLiteral(literal: Literal): Rule = this.copy(body=this.body+literal)
  def removeLiteral(literal: Literal): Rule = {
    require(body.contains(literal))
    this.copy(body=this.body-literal, negations=this.negations-literal)
  }
  def addNegatedLiteral(literal: Literal): Rule = this.copy(negations=this.negations + literal, body = this.body+literal)
  def updateNegatedLiteral(oldLit: Literal, newLit: Literal): Rule = {
    val otherLits = this.body - oldLit
    val otherNegatedLits = this.negations - oldLit
    assert(otherLits.size == this.body.size - 1)
    assert(otherNegatedLits.size == this.negations.size - 1)
    val newBody = otherLits + newLit
    val newNegation = otherNegatedLits + newLit
    require(newBody.size == this.body.size)
    require(newNegation.size == this.negations.size)
    this.copy(body = newBody, negations=newNegation)
  }

  def rename(binding: Map[Parameter, Parameter]): Rule = {
    val newHead = head.rename(binding)
    val newBody = body.map(_.rename(binding))
    val newNeg = negations.map(_.rename(binding))
    Rule(newHead, newBody, newNeg)
  }

  def renameRelation(newRels: Map[Relation,Relation]): Rule = {
    val newHead = head.renameRelation(newRels)
    val newBody = body.map(_.renameRelation(newRels))
    val newNeg = negations.map(_.renameRelation(newRels))
    Rule(newHead, newBody, newNeg)
  }

  def getVarSet(): Set[Variable] = _getVarList(body.toList :+ head).toSet
  def getHeadVars(): List[Variable] = _getVarList(List(head))
  def _getGroundedVars(): List[Variable] = {
    val v1 = getPositiveLiterals().toList.flatMap(_.fields).flatMap {
      case _: Constant => None
      case v: Variable => Some(v)
    }
    v1 ++ getFunctorOutputs()
  }
  def getUngroundHeadVariables(): List[Variable] = {
    getHeadVars().filterNot(v => _getGroundedVars().contains(v))
  }
  def getBoundParams(): Set[Parameter] = {
    val allParams = body.flatMap(_.fields)
    val freeParams: Set[Parameter] = freeVariables().map(_.asInstanceOf[Parameter])
    allParams.diff(freeParams)
  }

  def _getVarList(literals: List[Literal]): List[Variable] = literals.flatMap(_.fields).flatMap {
      case _: Constant => None
      case v: Variable => Some(v)
    }
  def getConstantList: List[Constant] = (body+head).toList.flatMap(_.fields).flatMap {
    case _: Variable => None
    case c: Constant => Some(c)
  }

  def getAllRelations(): Set[Relation] = (body+head).map(_.relation)

  def isHeadBounded(): Boolean = {
    getHeadVars().toSet.subsetOf(_getGroundedVars().toSet)
  }

  def getFunctorOutputs(): Set[Variable] = {
    val allFields = body.flatMap {
      case lit: FunctorLiteral => lit.abstractFunctorSpec match {
        case f: FunctorSpec => Some(f.getOutput(lit).asInstanceOf[Variable])
        case _ => None
      }
      case lit: AggregateLiteral => Some(lit.getOutput)
      case _ => None
    }
    allFields map {
      case v: Variable => v
      case _ => throw new Exception(s"Functor output should not be types other than Variable.")
    }
  }

  def _getAggVars(): Set[Variable] = {
    body.flatMap {
      case aggLit: AggregateLiteral => aggLit.aggregator match {
        case agg: AggMax => Some(aggLit.fields(agg.aggIndex).asInstanceOf[Variable])
        case _ => None
      }
      case _ => None
    }
  }

  def freeVariables(): Set[Variable] = {
    val allVars = _getVarList(body.toList :+ head)
    val posLits = getPositiveLiterals()
    val functorOutputs: Set[Variable] = getFunctorOutputs()
    // val posVars = _getVarList(posLits.toList :+ head) ++ functorOutputs
    val posVars = _getVarList(posLits.toList) ++ functorOutputs
    val paramCounts = allVars.groupBy(identity) map {case (p, ps) => p ->  ps.size}
    val boundVars = allVars.filter(v => paramCounts(v) > 1).toSet.intersect(posVars.toSet)

    val aggVars = _getAggVars()
    allVars.toSet.diff(boundVars++aggVars)
  }

  def normalize(): Rule = {
    var binding: Map[Parameter, Parameter] = Map()
    var varCounts: Map[Type, Int] = Map()
    val allVars: List[Variable] = _getVarList(body.toList :+ head)
    for (v <- allVars) {
      if (!binding.contains(v)) {
        // Count variables
        val _type = v._type
        val c = varCounts.getOrElse(_type, 0)
        varCounts = varCounts.updated(_type,c+1)

        val newVar = Variable(_type, c)
        binding = binding.updated(v,newVar)
      }
    }
    val normalized = rename(binding)
    assert(normalized.getConstantList.size == this.getConstantList.size)
    normalized
  }

  def isRecursive(): Boolean = this.body.map(_.relation).contains(head.relation)

  def isValid(): Boolean = isHeadBounded()
}

// object Rule {
//   def renameConstantSymbols(rule: Rule): Rule = {
//     def addDoubleQuotes(p: Parameter): Parameter = p match {
//       case v:Variable => v
//       case c:Constant => {
//         if (c.name.forall(_.isDigit)) {c}
//         else {Constant("\""+c.name+"\"",c._type)}
//       }
//     }
//     def addDoubleQuotesToLiterals(literal: Literal): Literal = {
//       val newFields = literal.fields.map(addDoubleQuotes)
//       literal match {
//         case sl: SimpleLiteral => sl.copy(fields = newFields)
//         case al: AggregateLiteral => al.copy(fields = newFields)
//         case fl: FunctorLiteral => fl.copy(fields = newFields)
//       }
//     }
//     val newHead = addDoubleQuotesToLiterals(rule.head)
//     val newBody = rule.body.map(addDoubleQuotesToLiterals)
//     Rule(newHead, newBody)
//   }
// }

case class Program(rules: Set[Rule]) extends Ordered[Program] {
  override def toString: String = rules.mkString("\n")
  val literalCounts: Int = rules.toList.map(_.body.size).sum
  val fieldCounts: Int = rules.toList.flatMap(_.body.toList.map(_.fields.size)).sum
  val constantCounts: Int = rules.toList.map(_.getConstantList.size).sum
  val incompleteRules: Set[Rule] = rules.filterNot(_.isHeadBounded())
  val isComplete: Boolean = incompleteRules.isEmpty
  def getAllRelations: Set[Relation] = rules.flatMap(_.getAllRelations())

  def renameRelation(newRels :Map[Relation, Relation]): Program = Program(rules.map(_.renameRelation(newRels)))

  override def compare(that: Program): Int = {
    if (this.rules.size < that.rules.size) -1
    else if (this.rules.size > that.rules.size) 1
    else if (this.literalCounts < that.literalCounts) -1
    else if (this.literalCounts > that.literalCounts) 1
    else if (this.constantCounts < that.constantCounts) -1
    else if (this.constantCounts > that.constantCounts) 1
    else 0
  }
}
object Program {
  def apply(): Program = Program(Set())
}
