package synthesis.search

import synthesis.activelearning.ActiveLearning
import synthesis.experiment.ProgramValidator
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
    // case "SDN" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    // case "NIB" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    // case "sensor" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    // case "consensus" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    // case "consensusagg" => new ProgramSynthesizer(problem)
    // case "overlay" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    // case "routingProto" => SynthesisAllPrograms(problem, initConfigSpace = initConfigSpace)
    case "consensusbarrier" => {
      /** Use the count aggregator */
      val preprocessors: Set[InputAggregator] = getPreprocessors(problem)
      val newProblem = preprocessors.foldLeft(problem)((p1, agg) => agg.preprocess(p1))
      // SynthesisAllPrograms(newProblem, initConfigSpace = initConfigSpace)
      new ProgramSynthesizer(newProblem, initConfigSpace)
    }
    // case _ => ???
    case _ => new ProgramSynthesizer(problem, initConfigSpace)
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
  private val programValidator = ProgramValidator(problem, staticConfigs)
  def check(allPrograms: Map[Relation, List[Program]]): Boolean = {
    var correct: Boolean = true
    for ((rel,ps) <- allPrograms) {
      val (validated, _) = programValidator.differentiateFromReference(ps.head, outRels=Set(rel))
      if (!validated) {
        println(s"Incorrect solution $rel.")
        correct = false
      }
    }
    correct
  }
  def check(program: Program): Boolean = {
    val (validated, _) = programValidator.differentiateFromReference(program)
    validated
  }
}