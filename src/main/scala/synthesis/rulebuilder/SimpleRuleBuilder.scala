package synthesis.rulebuilder
import synthesis._
import synthesis.rulebuilder.SimpleRuleBuilder.{newUnboundedLiteral, newVar, paramMapByType}

import scala.collection.mutable

class SimpleRuleBuilder(inputRels: Set[Relation], outputRels: Set[Relation],
                       maxRelCount: Int=1) extends RuleBuilder {

  def _bodyRelCounts(rule: Rule): Map[Relation, Int] = {
    val allRelsList: Seq[Relation] = rule.body.toSeq.map(_.relation)
    allRelsList.groupBy(identity).map {
      case (rel,allRels) => rel -> allRels.size
    }
  }
  def candidateRelations(rule: Rule): Set[Relation] = {
    val relCounts = _bodyRelCounts(rule)
    inputRels.filter(rel => relCounts.getOrElse(rel,0) < maxRelCount)
    // inputRels
  }
  def candidateNegRelations(rule: Rule): Set[Relation] = inputRels.diff(rule.body.map(_.relation))

  def _addGeneralLiteral(rule: Rule, relation: Relation) : Set[Rule] = {
    /** Add relation to rule, without binding the variables */
    val allLiterals = rule.body + rule.head
    val newLiteral = newUnboundedLiteral(allLiterals, relation)
    val r1 = rule.addLiteral(newLiteral)
    addOneBindingLiteral(r1, newLiteral)
  }

  def mostGeneralRules(): Set[Rule] = {
    outputRels.flatMap(rel => {
      val head = newUnboundedLiteral(Set(), rel)

      /** All possible bodies with one literal. */
      val bodies: Set[Literal] = inputRels.map(inputRel => newUnboundedLiteral(Set(head), inputRel))
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
    // val paramMap = _paramMapByType(rule.body)
    val paramMap = paramMapByType(rule.getPositiveLiterals())
    val availableVars: Set[Variable] = {
      val params = paramMap.getOrElse(variable._type, Set()) - variable
      params.flatMap {
        case _: Constant => None
        case v: Variable => Some(v)
      }
    }
    val bindings: Set[Map[Parameter, Parameter]] = availableVars.map(v => Map(variable -> v))
    val boundRules = bindings.map(b => rule.rename(b))
    /** Some binding will end up with two identical literals, filter out these cases */
    boundRules.filter(r => r.body.size == rule.body.size)
  }

  def addGeneralLiteral(rule: Rule) : Set[Rule] = {
    var newRules: Set[Rule] = Set()
    val candidateRels = candidateRelations(rule)
    for (rel <- candidateRels) {
      newRules ++= _addGeneralLiteral(rule, rel)
    }
    newRules
  }

  def addBinding(rule: Rule): Set[Rule] = {
    val freeVars: Set[Variable] = rule.freeVariables()
    freeVars.flatMap(v => _bindVariableToBody(rule, v))
  }

  def addOneBindingLiteral(rule: Rule, lit: Literal): Set[Rule] = {
    require(rule.getPositiveLiterals().contains(lit))
    val ruleFv: Set[Variable] = rule.freeVariables()
    val litVars: Set[Variable] = lit.fields.flatMap {
      case v: Variable => Some(v)
      case _: Constant => None
    }.toSet
    val freeVars = ruleFv.intersect(litVars)
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
    _allBindings(rel.signature, paramMap).map(fields => SimpleLiteral(rel, fields))
  }

  def addNegation(rule: Rule): Set[Rule] = {
    /** The same as add literal, but have all of them binded instead */

    /** Only add negation after head is bound */
    if (!rule.isHeadBounded())  {
      Set()
    }
    else {
      // The relations that add negated literal to
      val negRels: Set[Relation] = candidateNegRelations(rule)
      // val posParams: Map[Type, Set[Parameter]] = paramMapByType(rule.getPositiveLiterals())

      def addNegationByRel(rule: Rule, rel: Relation): Set[Rule] = {
        // /** All possible bindings to the literal from posLit */
        // val negatedLits: Set[Literal] = allBindings(rel, posParams)
        /** Bind at most 2 variables in the negated body */
        val MAX_NEG_VARS: Int = 2
        val negatedLits: Set[Literal] = bindNVariables(rel, rule, MAX_NEG_VARS)
        val nl = negatedLits.diff(rule.body)
        nl.map(l => rule.addNegatedLiteral(l))
      }
      negRels.flatMap(r => addNegationByRel(rule, r))
    }
  }

  def bindNVariables(relation: Relation, rule: Rule, n: Int): Set[Literal] = {
    def _bindNVariables(sig:List[Type], posParams: Map[Type, Set[Parameter]], n: Int): Set[List[Parameter]] = {
      require(n >= 0)
      require(sig.size >= n)
      sig match {
        case Nil => Set(List())
        case head::tail => {
          val bindHead: Set[List[Parameter]] = if (n > 0) {
            /** bind head variable */
            val hv: Set[Parameter] = posParams.getOrElse(head,Set())
            val tailBindings = _bindNVariables(tail,posParams,n-1)
            hv.flatMap(h => tailBindings.map(h::_))
          } else { Set() }
          val freeHead: Set[List[Parameter]] = if (sig.size > n) {
            /** do not bind head variable */
            val hv: Variable = newVar(rule.body, head)
            val tailBindings = _bindNVariables(tail, posParams, n)
            tailBindings.map(l => hv::l)
          } else {Set()}
          bindHead++freeHead
        }
      }
    }
    val posParams: Map[Type, Set[Parameter]] = paramMapByType(rule.getPositiveLiterals())
    val fields: Set[List[Parameter]] = _bindNVariables(relation.signature,posParams,n)
    fields.map(SimpleLiteral(relation,_))
  }

  def relaxOneBindingFromNegation(rule: Rule, lit: Literal): Set[Rule] = {
    val params = paramMapByType(rule.body+rule.head)
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
    val relaxedLits: Set[Literal] = _relaxBinding(lit.fields).map(fs => SimpleLiteral(lit.relation, fs))
    relaxedLits.map(nl => rule.updateNegatedLiteral(lit,nl))
  }

  def relaxNegation(rule: Rule): Set[Rule] = {
    /** Relax one negated literal parameter bindings */
    val simpleNegations = rule.negations.filter(_.isInstanceOf[SimpleLiteral])
    simpleNegations.flatMap(lit => relaxOneBindingFromNegation(rule, lit))
  }

  def bindInstanceIds(rule: Rule): Rule = {
    val instanceType = NumberType("InstanceId")
    val allInstanceIds = (rule.body + rule.head).flatMap(_.fields).filter(_._type == instanceType)
    val instanceIdVar = Variable("instanceid0", instanceType)
    val newBindings: Map[Parameter, Parameter] = allInstanceIds.map(p=>(p->instanceIdVar)).toMap
    val nr = rule.rename(newBindings)
    require(nr.body.map(_.relation) == rule.body.map(_.relation))
    nr
  }

  def refineRule(rule: Rule): Set[Rule] = {
    val addLiterals = addGeneralLiteral(rule)
    val addNegations = addNegation(rule)
    val refinedRules: Set[Rule] = (addBinding(rule) ++  relaxNegation(rule)).
      filter(_.maskUngroundVars().body.size==rule.body.size)

    val refined2 = (refinedRules++addLiterals++addNegations).map(bindInstanceIds)
    refined2.map(_.normalize()).map(removeShadowedLiteral)
  }

  def removeShadowedLiteral(rule: Rule): Rule = {
    var shadowed: Set[Literal] = Set()
    val ungroundVars: Set[Parameter] = rule.freeVariables().map(_.asInstanceOf[Parameter])
    def isShaowed(lit1: Literal, lit2: Literal): Boolean = {
      var _shadowed = true
      for ((f1,f2) <- lit1.fields zip lit2.fields) {
        if (f1 != f2 && !ungroundVars.contains(f1)) _shadowed =false
      }
      _shadowed
    }
    val allPosLits = rule.getPositiveLiterals()
    for (lit <- allPosLits) {
      val others = (allPosLits - lit).diff(shadowed).filter(_.relation == lit.relation)
      if (others.exists(l => isShaowed(lit,l))) shadowed += lit
    }
    var newRule: Rule = rule
    for (lit <- shadowed) {
      newRule = newRule.removeLiteral(lit)
    }
    newRule
  }
}

object SimpleRuleBuilder {
  def newUnboundedLiteral(literals: Set[Literal], relation: Relation): Literal = {
    /** Create a new literal without binding any variables from existing literals. */
    var fields: mutable.ListBuffer[Variable] = mutable.ListBuffer()
    var paramCounts: Map[Type, Int] = getParamCounts(literals)
    for (_type <- relation.signature) {
      val c = paramCounts.getOrElse(_type,0)
      paramCounts = paramCounts.updated(_type, c+1)
      fields += Variable(_type, c)
    }
    require(fields.size == relation.signature.size)
    SimpleLiteral(relation, fields.toList)
  }

  def paramMapByType(literals: Set[Literal]): Map[Type, Set[Parameter]] = {
    val allParams = literals.flatMap(_.fields)
    allParams.groupBy(_._type)
  }

  def getParamCounts(literals: Set[Literal]): Map[Type, Int] = {
    val params = paramMapByType(literals)
    params.map {case (t, ps) => t -> ps.size}
  }

  def newVar(literals: Set[Literal], _type: Type): Variable = {
    /** return variable of _type whose name is unseen in the set of literals */
    val paramCounts: Map[Type, Int] = getParamCounts(literals)
    val c = paramCounts.getOrElse(_type,0)
    val v = Variable(_type, c)
    require(!literals.flatMap(_.fields).contains(v))
    v
  }

}