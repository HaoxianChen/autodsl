package synthesis.rulebuilder

import synthesis._

class RecursionBuilder(inputRels: Set[Relation], outputRels: Set[Relation], recursion: Boolean=true)
  extends SimpleRuleBuilder(inputRels, outputRels) {

  override def addGeneralLiteral(rule: Rule): Set[Rule] = {
    if (!recursion) {
      super.addGeneralLiteral(rule)
    }
    else {
      var newRules: Set[Rule] = Set()
      val bodyRels = rule.body.map(_.relation)
      val rels = (inputRels + rule.head.relation).diff(bodyRels)
      for (rel <- rels) {
        newRules += _addGeneralLiteral(rule, rel)
      }
      newRules
    }
  }

}
