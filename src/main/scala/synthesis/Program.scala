package synthesis

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
case class Constant(name: String, _type: Type) extends Parameter {
  override def toString: String = {
    _type match {
      case _: NumberType => s"$name"

      // Wrap symbol constants within double quotes.
      case _: SymbolType => s"${'"'}${name}${'"'}"
    }
  }
}

case class Literal(relation: Relation, fields: List[Parameter]) {
  override def toString: String = {
    val field_str: String = fields.map(_.toString).mkString(",")
    s"${relation.name}($field_str)"
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

case class Rule(head: Literal, body: Set[Literal]) {
  override def toString: String = {
    val body_str: String = body.map(_.toString).mkString(",")
    s"${head} :- ${body_str}."
  }
}

case class Program(rules: Set[Rule]) {
  override def toString: String = rules.map(_.toString).mkString("\n")
}
