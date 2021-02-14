package synthesis

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

  def toString: String
  def rename(binding: Map[Parameter, Parameter]): Literal

  def _rename(binding: Map[Parameter, Parameter]): List[Parameter] = fields.map(p => binding.getOrElse(p, p))
}
object Literal {
  def apply(relation: Relation, fields: List[Parameter]): Literal = SimpleLiteral(relation, fields)
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
}

case class Tuple(relation: Relation, fields: List[Constant]) {
  override def toString: String = {
    val field_str: String = fields.map(_.toString).mkString(",")
    s"${relation.name}($field_str)"
  }
}

/* Relations */
case class Relation(name: String, signature: List[Type]) {
  def declString: String = {
    val sig_str: String = {
      val s: List[String] = for ((t,i) <- signature.zipWithIndex) yield s"x$i: $t"
      s.mkString(",")
    }
    s".decl $name($sig_str)"
  }
  override def toString: String = {
    val sig_str = signature.map(_.toString).mkString(",")
    s"${name}(${sig_str})"
  }
}

case class Rule(head: Literal, body: Set[Literal], negations: Set[Literal]=Set()) {
  require(negations.subsetOf(body), s"negations: ${negations},\n body: ${body}")

  override def toString: String = {
    val mr = this.maskUngroundVars()
    if (mr.body.nonEmpty) {
      val simpleLiterals: Set[Literal] = mr.getPositiveLiterals()
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

  def getPositiveLiterals(): Set[Literal] = body.diff(negations)

  def addLiteral(literal: Literal): Rule = this.copy(body=this.body+literal)
  def addNegatedLiteral(literal: Literal): Rule = this.copy(negations=this.negations + literal, body = this.body+literal)
  def updateNegatedLiteral(oldLit: Literal, newLit: Literal): Rule = {
    val otherLits = this.body - oldLit
    val otherNegatedLits = this.negations - oldLit
    assert(otherLits.size == this.body.size - 1)
    assert(otherNegatedLits.size == this.negations.size - 1)
    val newBody = otherLits + newLit
    val newNegation = otherNegatedLits + newLit
    this.copy(body = newBody, negations=newNegation)
  }

  def rename(binding: Map[Parameter, Parameter]): Rule = {
    val newHead = head.rename(binding)
    val newBody = body.map(_.rename(binding))
    val newNeg = negations.map(_.rename(binding))
    Rule(newHead, newBody, newNeg)
  }

  def getVarSet(): Set[Variable] = _getVarList(body.toList :+ head).toSet
  def getHeadVars(): List[Variable] = _getVarList(List(head))
  def _getGroundedVars(): List[Variable] = {
    getPositiveLiterals().toList.flatMap(_.fields).flatMap {
      case _: Constant => None
      case v: Variable => Some(v)
    }
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

  def freeVariables(): Set[Variable] = {
    val allVars = _getVarList(body.toList :+ head)
    val posVars = _getVarList(getPositiveLiterals().toList :+ head)
    val paramCounts = allVars.groupBy(identity) map {case (p, ps) => p ->  ps.size}
    val boundVars = allVars.filter(v => paramCounts(v) > 1).toSet.intersect(posVars.toSet)
    allVars.toSet.diff(boundVars)
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

case class Program(rules: Set[Rule]) {
  override def toString: String = rules.mkString("\n")
  def getAllRelations: Set[Relation] = rules.flatMap(_.getAllRelations())
}
object Program {
  def apply(): Program = Program(Set())
}
