package synthesis.rulebuilder

import synthesis._

abstract class RuleBuilder() {
  // Top level interface
  def mostGeneralRules(): Set[Rule]
  def refineRule(rule: Rule): Set[Rule]

  def candidateRelations(rule: Rule): Set[Relation]

  // refine steps
  def addGeneralLiteral(rule: Rule): Set[Rule]
  def addBinding(rule: Rule): Set[Rule]
  def addNegation(rule: Rule): Set[Rule]
}

