package synthesis

import synthesis.rulebuilder.{AggregateLiteral, OutputAggregator, ArgMax, ArgMin, RuleBuilder}
import synthesis.search.SynthesisConfigSpace

class ProgramBuilder(ruleBuilder: RuleBuilder, aggregators: Set[OutputAggregator]) {
  def getAggregators: Set[OutputAggregator] = aggregators
  /** Three top level interface: most general programs, refine, and add a rule to the program. */
  def mostGeneralPrograms(): Set[Program] = {
    val mostGeneralRules = ruleBuilder.mostGeneralRules()
    mostGeneralRules.map(r => Program(Set(r)))
  }

  def refine(program: Program): Set[Program] = {
    val refined = refineARule(program)
    refined
  }

  def addARule(program: Program): Set[Program] = {
    val newRules = ruleBuilder.mostGeneralRules()
    newRules.map(r => Program(program.rules+r))
  }

  def refineARule(program: Program): Set[Program] = {
    def _refineARule(program: Program, rule:Rule): Set[Program] = {
      require(program.rules.contains(rule))
      val otherRules = program.rules.diff(Set(rule))
      val newRules = ruleBuilder.refineRule(rule)
      val newPrograms = newRules.map(r => Program(otherRules+r))
      assert(newPrograms.forall(_.rules.size<=program.rules.size))
      newPrograms
    }
    val nonAggregateRules = program.rules.filterNot(isAggregateRule)
    nonAggregateRules.flatMap(r => _refineARule(program,r))
  }

  def aggregateOutput(program: Program): Set[Program] = {
    // aggregators.map(agg => agg.getAggProgram(program))
    val outRels: Set[Relation] = program.rules.map(_.head.relation)
    val applicableAggregators: Set[OutputAggregator] = aggregators.filter(
      agg => outRels.contains(agg.relation)
    )
    applicableAggregators.map(agg => agg.getAggProgram(program))
  }

  def isAggregateRule(rule: Rule): Boolean = {
    val allAggRels = aggregators.map(_.getAggHeadRel)
    val allRels = rule.getAllRelations()
    allRels.intersect(allAggRels).nonEmpty
  }
}

object ProgramBuilder {
  def apply(problem: Problem): ProgramBuilder = {
    val configSpace = SynthesisConfigSpace.getConfigSpace(problem)
    val ruleBuilder = configSpace.get_config().get_rule_builder(problem)
    problem.domain match {
      case "routing" => {
        val aggregators: Set[OutputAggregator] = ArgMin.allInstances(problem) ++ ArgMax.allInstances(problem)
        new ProgramBuilder(ruleBuilder, aggregators)
      }
      case "consensusagg" => {
        val aggregators: Set[OutputAggregator] = ArgMax.allInstances(problem)
        new ProgramBuilder(ruleBuilder, aggregators)
      }
      case "NIB" => {
        new ProgramBuilder(ruleBuilder, Set())
      }
      case _ => ???
    }

  }
}