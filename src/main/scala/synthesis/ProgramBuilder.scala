package synthesis

import synthesis.rulebuilder.{AggregateLiteral, ArgMax, ArgMin, OutputAggregator, RuleBuilder}
import synthesis.search.{SynthesisConfigSpace, SynthesisConfigs}

class ProgramBuilder(ruleBuilder: RuleBuilder, aggregators: Set[OutputAggregator]) {
  def getAggregators: Set[OutputAggregator] = aggregators
  /** Three top level interface: most general programs, refine, and add a rule to the program. */
  def mostGeneralPrograms(): Set[Program] = {
    val mostGeneralRules = ruleBuilder.mostGeneralRules()
    mostGeneralRules.map(r => Program(Set(r)))
  }

  def refineRule(rule: Rule): Set[Rule] = ruleBuilder.refineRule(rule)

  // def refine(program: Program): Set[Program] = {
  //   val refined = refineARule(program)
  //   refined
  // }

  // def addARule(program: Program): Set[Program] = {
  //   val newRules = ruleBuilder.mostGeneralRules()
  //   newRules.map(r => Program(program.rules+r))
  // }

  // def refineARule(program: Program): Set[Program] = {
  //   // val nonAggregateRules = program.rules.filterNot(isAggregateRule)
  //   // nonAggregateRules.flatMap(r => _refineARule(program,r))
  //   if (program.isComplete) {
  //     /** If the program is complete, refine all non aggregate rules. */
  //     val nonAggregateRules = program.rules.filterNot(isAggregateRule)
  //     nonAggregateRules.flatMap(r => _refineARule(program,r))
  //   }
  //   else {
  //     /** If rule is incomplete, only refine the incomplete rule. */
  //     require(program.incompleteRules.size==1)
  //     val incompleteRule = program.incompleteRules.head
  //     _refineARule(program,incompleteRule)
  //   }
  // }

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
  def apply(problem: Problem, config: SynthesisConfigs): ProgramBuilder = {
    // val configSpace = SynthesisConfigSpace.getConfigSpace(problem)
    val ruleBuilder = config.get_rule_builder(problem)
    problem.domain match {
      case "routing" => {
        val aggregators: Set[OutputAggregator] = ArgMin.allInstances(problem) ++ ArgMax.allInstances(problem)
        new ProgramBuilder(ruleBuilder, aggregators)
      }
      case "consensusagg" => {
        val aggregators: Set[OutputAggregator] = ArgMax.allInstances(problem)
        new ProgramBuilder(ruleBuilder, aggregators)
      }
      // case "NIB" => {
      //   new ProgramBuilder(ruleBuilder, Set())
      // }
      // case _ => ???
      case _ => {
        new ProgramBuilder(ruleBuilder, Set())
      }
    }

  }
}