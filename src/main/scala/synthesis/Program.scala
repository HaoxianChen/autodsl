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
case class Constant(name: String, _type: Type) extends Parameter

case class Literal(relation: Relation, fields: List[Parameter]) {
  override def toString: String = {
    val field_str: String = fields.map(_.toString).mkString(",")
    s"${relation.name}($field_str)"
  }

  def rename(binding: Map[Parameter, Parameter]): Literal = {
    val newFields = fields.map(p => binding.getOrElse(p, p))
    Literal(relation, newFields)
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
    if (body.nonEmpty) {
      val simpleLiterals = getPositiveLiterals()
      val body_str: String = {
        (simpleLiterals.map(_.toString) ++ negations.map("!" + _.toString)).mkString(",")
      }
      s"${head} :- ${body_str}."
    }
    else {
      s"${head}."
    }
  }

  def maskUngroundVars(): Rule = {
    // replace unground variables in the body with "_"
    val ungroundVars: Set[Variable] = freeVariables().diff(getHeadVars().toSet)
    val bindings = mutable.Map.empty[Parameter, Parameter]
    for (uv <- ungroundVars) {
      val maskedVar = Variable("_", uv._type)
      bindings.update(uv, maskedVar)
    }
    rename(bindings.toMap)
  }

  def getPositiveLiterals(): Set[Literal] = body.diff(negations)

  def addLiteral(literal: Literal): Rule = {
    this.copy(body=this.body+literal)
  }

  def rename(binding: Map[Parameter, Parameter]): Rule = {
    val newHead = head.rename(binding)
    val newBody = body.map(_.rename(binding))
    val newNeg = negations.map(_.rename(binding))
    Rule(newHead, newBody, newNeg)
  }

  def getVarSet(): Set[Variable] = _getVarList(body+head).toSet
  def getHeadVars(): List[Variable] = _getVarList(Set(head))
  def getFreeHeadVariables(): List[Variable] = getHeadVars().filter(v => freeVariables().contains(v))

  def _getVarList(literals: Set[Literal]): List[Variable] = literals.toList.flatMap(_.fields).flatMap {
      case _: Constant => None
      case v: Variable => Some(v)
    }

  def getAllRelations(): Set[Relation] = (body+head).map(_.relation)

  def isHeadBounded(): Boolean = {
    val bodyParams = getPositiveLiterals().flatMap(_.fields)
    head.fields.toSet.subsetOf(bodyParams)
  }

  def freeVariables(): Set[Variable] = {
    val allVars = _getVarList(body+head)
    val paramCounts = allVars.groupBy(identity) map {case (p, ps) => p ->  ps.size}
    allVars.filter(v => paramCounts(v)==1).toSet
  }

  def normalize(): Rule = {
    var binding: Map[Parameter, Parameter] = Map()
    var varCounts: Map[Type, Int] = Map()
    val allVars = getVarSet()
    for (v <- allVars) {
      if (!binding.contains(v)) {
        // Count variables
        val _type = v._type
        val c = varCounts.getOrElse(_type, 0)
        varCounts = varCounts.updated(_type,c+1)

        val newVar = Variable(s"${_type.name.toLowerCase()}$c", _type)
        binding = binding.updated(v,newVar)
      }
    }
    rename(binding)
  }

  def isValid(): Boolean = isHeadBounded()
}

case class Program(rules: Set[Rule]) {
  override def toString: String = rules.map(_.maskUngroundVars().toString).mkString("\n")
  def getAllRelations: Set[Relation] = rules.flatMap(_.getAllRelations())
}
