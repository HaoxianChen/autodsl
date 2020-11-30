package synthesis.rulebuilder
import synthesis._

import scala.collection.mutable

class SimpleRuleBuilder(inputRels: Set[Relation], outputRels: Set[Relation]) extends RuleBuilder {
  def _paramMapByType(literals: Set[Literal]): Map[Type, Set[Parameter]] = {
    val allParams = literals.flatMap(_.fields)
    allParams.groupBy(_._type)
  }

  def _newUnboundedLiteral(literals: Set[Literal], relation: Relation): Literal = {
    /** Create a new literal without binding any variables from existing literals. */
    val params = _paramMapByType(literals)
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

  def _addGeneralLiteral(rule: Rule, relation: Relation) : Rule = {
    /** Add relation to rule, without binding the variables */
    val allLiterals = rule.body + rule.head
    val newLiteral = _newUnboundedLiteral(allLiterals, relation)
    rule.addLiteral(newLiteral)
  }

  def mostGeneralRules(): Set[Rule] = {
    outputRels.flatMap(rel => {
      val head = _newUnboundedLiteral(Set(), rel)

      /** All possible bodies with one literal. */
      val bodies: Set[Literal] = inputRels.map(inputRel => _newUnboundedLiteral(Set(head), inputRel))
      val unBoundRules = bodies.map(lit => Rule(head, Set(lit)))

      /** At least add one binding to the head. */
      def bindHead(rule: Rule): Set[Rule] = {
        rule.getUngroundHeadVariables().flatMap(v => _bindVariableToBody(rule, v)).toSet
      }
      val rules = unBoundRules.flatMap(bindHead)
      rules.map(_.normalize())
    })
  }

  def _bindVariableToBody(rule:Rule, variable: Variable): Set[Rule] = {
    /** Only bind to variables in the rule body */
    require(rule.getVarSet().contains(variable))
    val paramMap = _paramMapByType(rule.body)
    val availableVars: Set[Variable] = {
      val params = paramMap.getOrElse(variable._type, Set()) - variable
      params.flatMap {
        case _: Constant => None
        case v: Variable => Some(v)
      }
    }
    val bindings: Set[Map[Parameter, Parameter]] = availableVars.map(v => Map(variable -> v))
    bindings.map(b => rule.rename(b))
  }

  def addGeneralLiteral(rule: Rule) : Set[Rule] = {
    var newRules: Set[Rule] = Set()
    for (rel <- inputRels.diff(rule.body.map(_.relation))) {
      newRules += _addGeneralLiteral(rule, rel)
    }
    newRules
  }

  def addBinding(rule: Rule): Set[Rule] = {
    val freeVars: Set[Variable] = rule.freeVariables()
    freeVars.flatMap(v => _bindVariableToBody(rule, v))
  }

  def addNegation(rule: Rule): Set[Rule] = {
    /** Only negate on input relations */
    val posLits = rule.getPositiveLiterals().filter(l => inputRels.contains(l.relation))

    if (posLits.size >= 2) {
      // Only negate when body relation has more than 1 positive literals.
      val negatedRules = posLits.map {
        l => Rule(rule.head, rule.body, rule.negations + l)
      }
      val headVarLen = rule.head.fields.size
      // filter rules whose head is completely ungrounded.
      negatedRules.filter(_.getUngroundHeadVariables().size < headVarLen)
    }
    else Set()
  }

  def refineRule(rule: Rule): Set[Rule] = {
    val refinedRules: Set[Rule] = addGeneralLiteral(rule) ++ addBinding(rule) ++ addNegation(rule)
    refinedRules.map(_.normalize())
  }
}
