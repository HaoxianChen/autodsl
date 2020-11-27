package synthesis.rulebuilder

import synthesis._

class RecursionBuilder(inputRels: Set[Relation], outputRels: Set[Relation])
  extends SimpleRuleBuilder(inputRels, outputRels) {

  override def addGeneralLiteral(rule: Rule): Set[Rule] = {
    var newRules: Set[Rule] = Set()
    val bodyRels = rule.body.map(_.relation)
    val rels = inputRels.diff(bodyRels) + rule.head.relation
    for (rel <- rels) {
      newRules += _addGeneralLiteral(rule, rel)
    }
    newRules
  }
}
