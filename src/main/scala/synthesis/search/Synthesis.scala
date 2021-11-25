package synthesis.search

import synthesis.rulebuilder.{AggCount, InputAggregator}
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
  def getConfigSpace: SynthesisConfigSpace
}

object Synthesis {
  def apply(problem: Problem): Synthesis = problem.domain match {
    case "SDN" => SynthesisAllPrograms(problem)
    case "routing" => new ProgramSynthesizer(problem)
    case "NIB" => SynthesisAllPrograms(problem)
    case "sensor" => SynthesisAllPrograms(problem)
    case "consensus" => {
      /** Use the count aggregator */
      val preprocessors: Set[InputAggregator] = AggCount.allInstances(problem)
      val newProblem = preprocessors.foldLeft(problem)((p1, agg) => agg.preprocess(p1))
      SynthesisAllPrograms(newProblem)
    }
    case "overlay" => SynthesisAllPrograms(problem)
    case "routingProto" => SynthesisAllPrograms(problem)
    case _ => ???
  }
}
