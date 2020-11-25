package synthesis.rulebuilder
import synthesis._

import scala.collection.mutable

case class SimpleRuleBuilder(inputRels: Set[Relation], outputRels: Set[Relation]) extends RuleBuilder {
  def paramMapByType(literals: Set[Literal]): Map[Type, Set[Parameter]] = {
    val allParams = literals.flatMap(_.fields)
    allParams.groupBy(_._type)
  }

  def newUnboundedLiteral(literals: Set[Literal], relation: Relation): Literal = {
    /** Create a new literal without binding any variables from existing literals. */
    val params = paramMapByType(literals)
    var fields: mutable.ListBuffer[Variable] = mutable.ListBuffer()
    var paramCounts: Map[Type, Int] = params.map {case (t, ps) => t -> ps.size}
    for (_type <- relation.signature) {
      val c = paramCounts.getOrElse(_type,0)
      paramCounts = paramCounts.updated(_type, c+1)
      val varName = s"${_type.name.toLowerCase()}${c}"
      fields += Variable(varName, _type)
    }
    require(fields.size == relation.signature.size)
    Literal(relation, fields.toList)
  }

  def addGeneralLiteral(rule: Rule, relation: Relation) : Rule = {
    /** Add relation to rule, without binding the variables */
    val allLiterals = rule.body + rule.head
    val newLiteral = newUnboundedLiteral(allLiterals, relation)
    rule.addLiteral(newLiteral)
  }

  def mostGeneralRules(): Set[Rule] = {
    outputRels.flatMap(rel => {
      val head = newUnboundedLiteral(Set(), rel)

      /** All possible bodies with one literal. */
      val bodies: Set[Literal] = inputRels.map(inputRel => newUnboundedLiteral(Set(head), inputRel))
      val unBoundRules = bodies.map(lit => Rule(head, Set(lit)))

      /** At least add one binding to the head. */
      def bindHead(rule: Rule): Set[Rule] = {
        rule.getFreeHeadVariables().flatMap(v => bindVariableToBody(rule, v)).toSet
      }
      val rules = unBoundRules.flatMap(bindHead)
      rules.map(_.normalize())
    })
  }

  def bindVariableToBody(rule:Rule, variable: Variable): Set[Rule] = {
    require(rule.getVarSet().contains(variable))
    val paramMap = paramMapByType(rule.body)
    val availableVars = paramMap.getOrElse(variable._type, Set()) - variable
    val bindings: Set[Map[Parameter, Parameter]] = availableVars.map(v => Map(variable -> v))
    bindings.map(b => rule.rename(b))
  }

  def refineRule(rule: Rule): Set[Rule] = {
    def addBinding(rule: Rule): Set[Rule] = {
      val freeVars: Set[Variable] = rule.freeVariables()
      freeVars.flatMap(v => bindVariableToBody(rule, v))
    }

    def addNegation(rule: Rule): Set[Rule] = {
      val posLits = rule.getPositiveLiterals()

      if (posLits.size >= 2) {
        // Only negate when body relation has more than 1 positive literals.
        val negatedRules = posLits.map {
          l => Rule(rule.head, rule.body, rule.negations + l)
        }
        negatedRules
      }
      else Set()
    }

    var refinedRules: Set[Rule] = Set()
    for (rel <- inputRels.diff(rule.body.map(_.relation))) {
      refinedRules += addGeneralLiteral(rule, rel)
    }
    refinedRules ++= addBinding(rule)
    refinedRules ++= addNegation(rule)
    refinedRules.map(_.normalize())
  }
}
