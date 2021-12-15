package synthesis.rulebuilder

import synthesis._

abstract class RuleBuilder() {
  // Top level interface
  def mostGeneralRules(): Set[Rule]
  def refineRule(rule: Rule): Set[Rule]

  def candidateRelations(rule: Rule): Set[Relation]
  def candidateNegRelations(rule: Rule): Set[Relation]

  // refine steps
  def addGeneralLiteral(rule: Rule): Set[Rule]
  def addBinding(rule: Rule): Set[Rule]
  def addNegation(rule: Rule): Set[Rule]

  def bindInstanceIds(rule: Rule): Rule = {
    val instanceType = NumberType("InstanceId")
    val allInstanceIds = (rule.body + rule.head).flatMap(_.fields).filter(_._type == instanceType)
    val instanceIdVar = Variable("instanceid0", instanceType)
    val newBindings: Map[Parameter, Parameter] = allInstanceIds.map(p=>(p->instanceIdVar)).toMap
    val nr = rule.rename(newBindings)
    require(nr.body.map(_.relation) == rule.body.map(_.relation))
    nr
  }

}

