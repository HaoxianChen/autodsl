package synthesis.search

import synthesis._

class ProgramSynthesizer(problem: Problem) extends Synthesis(problem) {
  /** Synthesize program on a program basis */

  def learnNPrograms(idb: Set[Tuple]): List[Program] = {
    var validPrograms: Set[Program] = Set()

    while (validPrograms.isEmpty) {
      val baseProgram: Program = ???

      val candidatePrograms: Set[Program] = refineProgram(baseProgram)

      validPrograms = candidatePrograms.filter(isProgramValid)
    }
    /** todo: order the valid programs */
    validPrograms.toList
  }

  def refineProgram(program: Program): Set[Program] = ???

  def isProgramValid(program: Program): Boolean = ???
}

class ProgramBuilder(inRels: Set[Relation], outRels: Set[Relation], aggSpecs: Set[AggregateSpec]) {
  /** Two top level interface: refine or add a rule to the program. */
  def refine(program: Program): Set[Program] = {
    val refined = refineARule(program)
    val aggregated = aggregateOutput(program)
    refined ++ aggregated
  }

  def addARule(program: Program): Set[Program] = ???

  def refineARule(program: Program): Set[Program] = ???
  def aggregateOutput(program: Program): Set[Program] = ???

}

abstract class AggregateSpec {
  def name: String
  def relation: Relation
  def indices: List[Int]
  def aggIndex: Int
  require(!indices.contains(aggIndex))
  require(indices.forall(i => i<relation.signature.size && i >= 0))
  require(aggIndex<relation.signature.size && aggIndex >= 0)

  def getRules: Set[Rule]
}

case class ArgMin(relation: Relation, indices: List[Int], aggIndex: Int) extends AggregateSpec {
  val name: String = "argMin"

  def getRules: Set[Rule] = {
    ???
  }
}