package synthesis.util

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

class Example {}

class ExampleParser extends JavaTokenParsers {
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comments and identifiers
  // All valid Java identifiers may be used: See documentation for JavaTokenParsers.ident
  // Ignore C and C++-style comments. See: https://stackoverflow.com/a/5954831
  protected override val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  /** examples return a set of example instances, where
   *  each instance is a set of tuples.
   *  Tuples are represented as (tuple_name, [args])
   * */
  def examples: Parser[(List[(String, List[String])], List[List[(String, List[String])]])] = {
    ((("Static"~>"{"~> tupleList <~"}")) ~ ("Examples"~>"{"~>exampleInstanceList<~"}") ^^ {
      case s~e => (s,e)
    })
  }

  def tuple: Parser[(String, List[String])] = {
    (ident ~ ("("~> repsep(wholeNumber|stringLiteral,",")<~")")) ^^ {
      case name ~ fields => (name, fields.map(_.replaceAll("^\"|\"$", "")))
    }
  }
  def tupleList: Parser[List[(String, List[String])]] = repsep(tuple, ",")

  def exampleInstance: Parser[List[(String, List[String])]]  = {
    ("input"~>"{"~>tupleList<~"}") ~ ("output"~>"{"~>tupleList<~"}") ^^ {
      case in~out => in++out
    }
  }
  def exampleInstanceList: Parser[List[List[(String, List[String])]]] = exampleInstance.+
}
