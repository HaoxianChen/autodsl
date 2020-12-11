package synthesis

import scala.util.parsing.combinator._
import scala.util.matching.Regex

class Parser extends JavaTokenParsers{
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comments and identifiers
  // All valid Java identifiers may be used: See documentation for JavaTokenParsers.ident
  // Ignore C and C++-style comments. See: https://stackoverflow.com/a/5954831
  protected override val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def typeDecl: Parser[Type] = (".type" ~> ident) ~ ("<:" ~> ident) ^^ {
    case name~"symbol" => SymbolType(name)
    case name~"number" => NumberType(name)
    case _ => ???
  }

  def domainDecl: Parser[String] = ".domain" ~> ident

  def fieldDecl: Parser[String] = ident ~> ":" ~> ident
  def fieldDeclList: Parser[List[String]] = repsep(fieldDecl, ",")

  private val emptyProblem: Problem = Problem()
  def problem: Parser[Problem] = (domainDeclLine | typeDeclLine | inputRelationDeclLine
    | outputRelationDeclLine).* ^^ {
    f => f.foldLeft(emptyProblem) {case (problem, transformer) => transformer(problem)}
  }

  def domainDeclLine: Parser[Problem => Problem] = domainDecl ^^ {domain =>
    problem => problem.addDomain(domain)}
  def typeDeclLine: Parser[Problem => Problem] = typeDecl ^^ {_type =>
    problem => problem.addType(_type)}
  def inputRelationDeclLine: Parser[Problem => Problem] =
    (".decl" ~> "*" ~> ident) ~ ("(" ~> fieldDeclList <~")") ^^ {
      case name ~ typeNames => {
        problem =>  problem.addInputRelation(name, typeNames)
      }
    }
  def outputRelationDeclLine: Parser[Problem => Problem] =
    (".decl" ~> ident) ~ ("(" ~> fieldDeclList <~")") ^^ {
      case name ~ typeNames => {
        problem =>  problem.addOutputRelation(name, typeNames)
      }
  }

}
