package synthesis

import com.typesafe.scalalogging.Logger

import scala.collection.mutable

case class PartialRuleEvaluator(problem: Problem) {
  private val evaluator = Evaluator(problem)

  def getRefIdb(rule: Rule, idb: Set[Tuple]): Set[Tuple] = {
    if (rule.isHeadBounded()){
      idb
    }
    else {
      val newRel = _getStrippedRelation(rule)
      val indices = _getBoundedIndices(rule)

      def getStripedTuple(tuple: Tuple): Tuple = {
        val newFields = indices map tuple.fields
        Tuple(newRel, newFields)
      }

      val relevantIdb = idb.filter(_.relation == rule.head.relation)
      relevantIdb.map(getStripedTuple)
    }
  }

  def eval(rule: Rule): Set[Tuple] = {
    val program = if (rule.isHeadBounded()) {
      Program(Set(rule))
    }
    else {
      val stripedRule = getStripedRule(rule)
      Program(Set(stripedRule))
    }
    evaluator.eval(program)
  }

  def _getBoundedIndices(rule: Rule): List[Int] = {
    val freeVars = rule.getFreeHeadVariables()
    val indices = mutable.ListBuffer.empty[Int]
    for ((f,i) <- rule.head.fields.zipWithIndex) {
      f match {
        case v: Variable => if (!freeVars.contains(v)) indices.append(i)
        case _ => ()
      }
    }
    require(indices.max < rule.head.fields.size, s"indices: $indices,\n rule: $rule")
    indices.toList
  }

  def getStripedRule(unBoundRule: Rule): Rule = {
    val indices = _getBoundedIndices(unBoundRule)
    assert(indices.nonEmpty)
    val newFields = indices map unBoundRule.head.fields

    val newOutRel = _getStrippedRelation(unBoundRule)
    assert(newOutRel.signature.size == newFields.size)
    val newHead = Literal(newOutRel, newFields)
    Rule(newHead, unBoundRule.body)
  }

  def _getStrippedRelName(unBoundRule: Rule): String = s"${unBoundRule.head.relation.name}_"

  def _getStrippedRelation(unBoundRule: Rule): Relation = {
    val indices = _getBoundedIndices(unBoundRule)
    val newSig = indices map unBoundRule.head.relation.signature
    val newRelName = _getStrippedRelName(unBoundRule)
    Relation(newRelName, newSig)
  }
}

case class RuleConstructor(inputRels: Set[Relation], outputRels: Set[Relation]) {
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

sealed abstract class Synthesis(problem: Problem) {
  def go(): Set[Program]
}

case class BasicSynthesis(problem: Problem,
                          maxIters: Int = 200,
                         maxRefineIters: Int = 200,
                         ) extends Synthesis(problem) {
  def go(): Set[Program] = Set(learnAProgram())

  private val logger = Logger("Synthesis")

  private val ruleConstructor = RuleConstructor(problem.inputRels, problem.outputRels)
  private val evaluator = PartialRuleEvaluator(problem)


  case class ScoredRule(rule: Rule, score: Double) {
    def isValid(): Boolean = score >= 1-1e-3

    def isTooGeneral(): Boolean = score > 0 && !isValid()

  }
  object ScoredRule {
    def apply(rule: Rule, idb: Set[Tuple]): ScoredRule = new ScoredRule(rule, scoreRule(rule, idb))

    def scoreRule(rule: Rule, allIdb: Set[Tuple]): Double = _ioScore(rule, allIdb) * _completenessScore(rule)

    def _ioScore(rule: Rule, allIdb: Set[Tuple]): Double = {
      val refIdb = getRefIdb(rule, allIdb)
      val idb = evalRule(rule)

      if (idb.nonEmpty) {
        val pos: Set[Tuple] = idb.intersect(refIdb)
        val neg: Set[Tuple] = idb.diff(refIdb)

        val npos = pos.size
        val nneg = neg.size
        require(npos + nneg == idb.size)

        npos.toDouble / (npos + nneg).toDouble
      }
      else 0
    }

    def _completenessScore(rule: Rule): Double = {
      /** The fraction of fields in the head being bound to body variables. */
      val m = rule.getFreeHeadVariables().size
      val n = rule.getHeadVars().size
      assert(n>=m)
      1.0 * (n-m) / n
    }

  }

  def learnAProgram(): Program = {
    var examples: Set[Tuple] = problem.idb.toTuples()
    var rules: Set[Rule] = Set()
    var iters: Int = 0

    // Init the rule pool with the most general rules
    var generalRules: Set[Rule] = ruleConstructor.mostGeneralRules()

    while (examples.nonEmpty && iters < maxIters ) {
      val (coveredExamples, newRule, remainingRules) = learnARule(examples, generalRules)
      examples = examples -- coveredExamples
      rules = rules + newRule
      generalRules = remainingRules
      iters += 1
    }
    Program(rules)
  }

  def learnARule(idb: Set[Tuple], generalSimpleRules: Set[Rule]): (Set[Tuple], Rule, Set[Rule]) = {
    var iters: Int = 0

    // score the rules based on current idb set
    val generalRules = {
      val curOutRels = idb.map(_.relation)
      val relevantRules = generalSimpleRules.filter(r => curOutRels.contains(r.head.relation))
      relevantRules.map(r => ScoredRule(r, idb))
    }

    // Set up the pool of rules to be refine
    var rulePool: mutable.PriorityQueue[ScoredRule] = new mutable.PriorityQueue()(Ordering.by(_.score))
    rulePool ++= generalRules.filter(r => r.isValid() || r.isTooGeneral())

    var validRules: Set[ScoredRule] = generalRules.filter(_.isValid())

    while (iters < maxRefineIters && validRules.isEmpty) {

      // pop highest scored rule from pool
      val baseRule: Rule = rulePool.dequeue().rule

      // refine the rules
      val refinedRules = ruleConstructor.refineRule(baseRule)
      val candidateRules: Set[ScoredRule] = refinedRules.map(r => ScoredRule(r, idb))

      // keep the valid ones
      validRules ++= candidateRules.filter(_.isValid())

      // Put the too general ones into the pool
      val tooGeneral = candidateRules.filter(_.isTooGeneral())
      rulePool ++= tooGeneral

      iters += 1
    }
    require(validRules.nonEmpty, s"Synthesis failed: empty valid rules.")
    val bestRule = validRules.maxBy(_.score).rule
    val remainingRules: Set[Rule] = {
      val rs = rulePool.toSet
      rs.map(_.rule)
    }
    logger.info(s"Found a rule after $iters iterations.\n$bestRule")
    (evalRule(bestRule), bestRule, remainingRules)
  }

  def evalRule(rule: Rule): Set[Tuple] = evaluator.eval(rule)
  def getRefIdb(rule: Rule, allIdb: Set[Tuple]): Set[Tuple] = evaluator.getRefIdb(rule, allIdb)

}
