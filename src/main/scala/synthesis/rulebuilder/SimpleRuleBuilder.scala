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
      fields += Variable(_type, c)
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

      val rules2 = rules.map(bindInstanceIds)
      rules2.map(_.normalize())
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

  // def addNegation(rule: Rule): Set[Rule] = {
  //   /** Only negate on input relations */
  //   val posLits = rule.getPositiveLiterals().filter(l => inputRels.contains(l.relation))

  //   if (posLits.size >= 2) {
  //     // Only negate when body relation has more than 1 positive literals.
  //     val negatedRules = posLits.map {
  //       l => Rule(rule.head, rule.body, rule.negations + l)
  //     }
  //     val headVarLen = rule.head.fields.size
  //     // filter rules whose head is completely ungrounded.
  //     negatedRules.filter(_.getUngroundHeadVariables().size < headVarLen)
  //   }
  //   else Set()
  // }

  def allBindings(rel: Relation, paramMap: Map[Type, Set[Parameter]]): Set[Literal] = {
    def _allBindings(sig: List[Type], paramMap: Map[Type, Set[Parameter]]): Set[List[Parameter]] = {
        sig match {
          case Nil => Set(List())
          case head :: tail => {
            /** Bind to existing parameters, if none, create a new one */
            val headBindings: Set[Parameter] = paramMap.getOrElse(head, Set(Variable(head, 0)))
            val tailBindings: Set[List[Parameter]] = _allBindings(tail, paramMap)
            headBindings.flatMap(
              f => tailBindings.map( t => f +: t)
            )
          }
        }
    }
    _allBindings(rel.signature, paramMap).map(fields => Literal(rel, fields))
  }

  def addNegation(rule: Rule): Set[Rule] = {
    /** The same as add literal, but have all of them binded instead */
    // The relations that add negated literal to
    val posRels: Set[Relation] = rule.getPositiveLiterals().map(_.relation)
    val negRels: Set[Relation] = inputRels.diff(posRels)
    val posParams: Map[Type, Set[Parameter]] = _paramMapByType(rule.getPositiveLiterals())

    def addNegationByRel(rule: Rule, rel: Relation): Set[Rule] = {
      /** All possible bindings to the literal from posLit */
      val negatedLits: Set[Literal] = allBindings(rel, posParams)
      negatedLits.map(l => rule.addNegatedLiteral(l))
    }
    negRels.flatMap(r => addNegationByRel(rule, r))
  }

  def relaxOneBindingFromNegation(rule: Rule, lit: Literal): Set[Rule] = {
    val params = _paramMapByType(rule.body)
    val paramCounts: Map[Type, Int] = params.map {case (t, ps) => t -> ps.size}

    val boundParams: Set[Parameter] = rule.getBoundParams()

    def _relaxBinding(sig: List[Parameter]) : Set[List[Parameter]]= sig match {
        case Nil => Set()
        case head :: tail => {
          /** don't update the head */
          val unbindTail: Set[List[Parameter]]  = _relaxBinding(tail).map(fs => head +: fs)

          if (boundParams.contains(head)) {
            /** unbind the head if head is originally bound */
            val unbindHead: List[Parameter] = {
              val newVar: Variable = Variable(head._type, paramCounts(head._type))
              newVar +: tail
            }
            unbindTail + unbindHead
          }
          else  {
            unbindTail
          }
        }
      }

    require(rule.negations.contains(lit))
    val relaxedLits: Set[Literal] = _relaxBinding(lit.fields).map(fs => Literal(lit.relation, fs))
    relaxedLits.map(nl => rule.updateNegatedLiteral(lit,nl))
  }

  def relaxNegation(rule: Rule): Set[Rule] = {
    /** Relax one negated literal parameter bindings */
    rule.negations.flatMap(lit => relaxOneBindingFromNegation(rule, lit))
  }

  def bindInstanceIds(rule: Rule): Rule = {
    val instanceType = NumberType("InstanceId")
    val allInstanceIds = (rule.body + rule.head).flatMap(_.fields).filter(_._type == instanceType)
    val instanceIdVar = Variable("instanceid0", instanceType)
    val newBindings: Map[Parameter, Parameter] = allInstanceIds.map(p=>(p->instanceIdVar)).toMap
    rule.rename(newBindings)
  }

  def refineRule(rule: Rule): Set[Rule] = {
    val refinedRules: Set[Rule] = addGeneralLiteral(rule) ++ addBinding(rule) ++
                                  addNegation(rule) ++ relaxNegation(rule)
    val refined2 = refinedRules.map(bindInstanceIds)
    refined2.map(_.normalize())
  }
}
