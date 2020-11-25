package synthesis.rulebuilder

import synthesis._

abstract class RuleBuilder() {
  def mostGeneralRules(): Set[Rule]
  def refineRule(rule: Rule): Set[Rule]
}

