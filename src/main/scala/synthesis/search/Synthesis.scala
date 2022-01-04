package synthesis.search

import synthesis.activelearning.ActiveLearning
import synthesis.rulebuilder.{AggCount, AggMax, InputAggregator}
import synthesis.{Problem, Program, Relation, Rule, Tuple}


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
  def apply(problem: Problem,
            initConfigSpace: SynthesisConfigSpace = SynthesisConfigSpace.emptySpace()
           ): Synthesis = problem.domain match {
    case "SDN" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    case "routing" => new ProgramSynthesizer(problem)
    case "NIB" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    case "sensor" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    case "consensus" => {
      SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
      // new ProgramSynthesizer(problem)
    }
    case "consensusagg" => new ProgramSynthesizer(problem)
    case "consensusbarrier" => {
      /** Use the count aggregator */
      val preprocessors: Set[InputAggregator] = getPreprocessors(problem)
      val newProblem = preprocessors.foldLeft(problem)((p1, agg) => agg.preprocess(p1))
      SynthesisAllPrograms(newProblem, initConfigSpace = initConfigSpace)
    }
    case "overlay" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    case "routingProto" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    case _ => ???
  }

  def getPreprocessors(problem: Problem): Set[InputAggregator] = problem.domain match {
    case "consensusbarrier" => {
      // AggCount.allInstances(problem) ++ AggMax.allInstances(problem)
      AggCount.allInstances(problem)
    }
    case _ => Set()
  }
}

case class SolutionChecker(problem: Problem, staticConfigs: Set[Relation]) {
  private val activeLearner = new ActiveLearning(problem, staticConfigs, numNewExamples = 400)
  def check(allPrograms: Map[Relation, List[Program]]): Boolean = {
    var correct: Boolean = true
    for ((rel,ps) <- allPrograms) {
      val (validated, _) = activeLearner.differentiateFromOracle(ps.head, outRels=Set(rel))
      if (!validated) {
        println(s"Incorrect solution $rel.")
        correct = false
      }
    }
    correct
  }
  def check(program: Program): Boolean = {
    val (validated, _) = activeLearner.differentiateFromOracle(program)
    validated
  }
}