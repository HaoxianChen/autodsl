package synthesis.search

import synthesis.{Problem, Program, Relation, Tuple}

abstract class Synthesis(problem: Problem) {
  def learnNPrograms(idb: Set[Tuple]): List[Program]
  def go(): Map[Relation, List[Program]] = {
    /** Strip the problem into learning different output */
    val examples: Set[Tuple] = problem.idb.toTuples()
    val exampleGroup: Map[Relation, Set[Tuple]] = examples.groupBy(_.relation)

    val programsByOutRels: Map[Relation, List[Program]] = exampleGroup.map {
      case (rel, idb) => rel -> learnNPrograms(idb)
    }
    programsByOutRels
  }
}

