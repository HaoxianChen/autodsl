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
  def examples: Parser[(List[(String, List[Int])], List[List[(String, List[Int])]])] = {
    ((("Static"~>"{"~> tupleList <~"}")) ~ ("Examples"~>"{"~>exampleInstanceList<~"}") ^^ {
      case s~e => (s,e)
    })
  }

  def tuple: Parser[(String, List[Int])] = {
    (ident ~ ("("~> repsep(wholeNumber,",")<~")")) ^^ {
      case name ~ fields => (name, fields.map(_.toInt))
    }
  }
  def tupleList: Parser[List[(String, List[Int])]] = repsep(tuple, ",")

  def exampleInstance: Parser[List[(String, List[Int])]]  = {
    ("input"~>"{"~>tupleList<~"}") ~ ("output"~>"{"~>tupleList<~"}") ^^ {
      case in~out => in++out
    }
  }
  def exampleInstanceList: Parser[List[List[(String, List[Int])]]] = exampleInstance.+
}
