package synthesis

import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import rulebuilder.{ConstantBuilder, RecursionBuilder, SimpleRuleBuilder}


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

  def evalRule(rule: Rule, learnedRules: Set[Rule]): Set[Tuple] = {
    val newRule = if (rule.isHeadBounded()) rule else getStripedRule(rule)
    val oldIdb = evaluator.eval(Program(learnedRules))
    val newIdb = evaluator.eval(Program(learnedRules+newRule))
    newIdb.diff(oldIdb)
  }

  def evalRules(rules: Set[Rule], learnedRules: Set[Rule]): Set[Tuple] = {
    require(rules.forall(_.isHeadBounded()))
    val oldIdb = evaluator.eval(Program(learnedRules))
    val newIdb = evaluator.eval(Program(learnedRules++rules))
    newIdb.diff(oldIdb)
  }

  def _getBoundedIndices(rule: Rule): List[Int] = {
    val freeVars = rule.getUngroundHeadVariables()
    val indices = mutable.ListBuffer.empty[Int]
    for ((f,i) <- rule.head.fields.zipWithIndex) {
      f match {
        case v: Variable => if (!freeVars.contains(v)) indices.append(i)
        case _ => ()
      }
    }
    require(indices.nonEmpty, s"$rule")
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

case class ScoredRule(rule: Rule, score: Double) extends Ordered[ScoredRule]{
  def isValid(): Boolean = score >= 1-1e-3

  def isTooGeneral(): Boolean = score > 0 && !isValid()

  override def compare(that: ScoredRule): Int = {
    if (this.score < that.score) -1
    else if (this.score > that.score) 1
    else if (this.rule.getConstantList.size < that.rule.getConstantList.size) 1
    else if (this.rule.getConstantList.size > that.rule.getConstantList.size) -1
    else 0
  }

}
object ScoredRule {
  def apply(rule: Rule, idb: Set[Tuple], ruleEvaluator: Rule=>Set[Tuple]): ScoredRule = {
    new ScoredRule(rule, scoreRule(rule, idb, ruleEvaluator))
  }

  def scoreRule(rule: Rule, refIdb: Set[Tuple], ruleEvaluator: Rule=>Set[Tuple]): Double = {
    _ioScore(rule, refIdb, ruleEvaluator) * _completenessScore(rule)
  }

  def _ioScore(rule: Rule, refIdb: Set[Tuple], ruleEvaluator: Rule=>Set[Tuple]): Double = {
    val idb = ruleEvaluator(rule)

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
    val m = rule.getUngroundHeadVariables().size
    val n = rule.getHeadVars().size
    assert(n>=m)
    1.0 * (n-m) / n
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

  // private val ruleConstructor = new SimpleRuleBuilder(problem.inputRels, problem.outputRels)
  // private val ruleConstructor = new RecursionBuilder(problem.inputRels, problem.outputRels)
  private val ruleConstructor = ConstantBuilder(problem.inputRels, problem.outputRels, problem.edb, problem.idb)
  private val evaluator = PartialRuleEvaluator(problem)


  def learnAProgram(): Program = {
    var examples: Set[Tuple] = problem.idb.toTuples()
    var rules: Set[Rule] = Set()
    var iters: Int = 0

    // Init the rule pool with the most general rules
    var generalRules: Set[Rule] = ruleConstructor.mostGeneralRules()

    while (examples.nonEmpty && iters < maxIters ) {
      val (coveredExamples, newRule, remainingRules) = learnARule(examples, generalRules, rules)
      val nextExamples = examples -- coveredExamples
      require(nextExamples.size < examples.size)
      examples = nextExamples
      rules = rules + newRule
      generalRules = remainingRules
      iters += 1
    }
    Program(rules)
  }

  def learnARule(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule]): (Set[Tuple], Rule, Set[Rule]) = {
    var iters: Int = 0

    // score the rules based on current idb set
    val generalRules = {
      val curOutRels = idb.map(_.relation)
      val relevantRules = generalSimpleRules.filter(r => curOutRels.contains(r.head.relation))
      relevantRules.map(r => scoreRule(r, idb, learnedRules))
    }

    // Set up the pool of rules to be refine
    var rulePool: mutable.PriorityQueue[ScoredRule] = new mutable.PriorityQueue()
    rulePool ++= generalRules.filter(r => r.isValid() || r.isTooGeneral())

    var validRules: Set[ScoredRule] = generalRules.filter(_.isValid())

    while (iters < maxRefineIters && validRules.isEmpty) {

      // pop highest scored rule from pool
      val baseRule: Rule = rulePool.dequeue().rule

      // refine the rules
      val refinedRules = ruleConstructor.refineRule(baseRule)
      val candidateRules: Set[ScoredRule] = refinedRules.map(r => scoreRule(r, idb, learnedRules))

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
    val newIdb = evaluator.evalRule(bestRule, learnedRules)
    assert(newIdb.nonEmpty)
    (newIdb, bestRule, remainingRules)
  }

  def scoreRule(rule: Rule, allIdb: Set[Tuple], learnedRules: Set[Rule]): ScoredRule = {
    val refIdb = evaluator.getRefIdb(rule, allIdb)
    def f_eval: Rule => Set[Tuple] = r => evaluator.evalRule(r, learnedRules)
    ScoredRule(rule, refIdb, f_eval)
  }

}

case class SynthesisNPrograms(problem: Problem,
                           maxIters: Int = 20,
                           maxRefineIters: Int = 25,
                          ) extends Synthesis(problem) {

  def go(): Set[Program] = Set(learnAProgram())

  private val logger = Logger("Synthesis")

  private val ruleConstructor = ConstantBuilder(problem.inputRels, problem.outputRels, problem.edb, problem.idb)
  private val evaluator = PartialRuleEvaluator(problem)


  def learnAProgram(): Program = {
    var examples: Set[Tuple] = problem.idb.toTuples()
    var rules: Set[Rule] = Set()
    var iters: Int = 0

    // Init the rule pool with the most general rules
    var generalRules: Set[Rule] = ruleConstructor.mostGeneralRules()

    while (examples.nonEmpty && iters < maxIters ) {
      val (coveredExamples, newRules, remainingRules) = learnNRules(examples, generalRules, rules)
      val nextExamples = examples -- coveredExamples
      require(nextExamples.size < examples.size)
      examples = nextExamples
      rules = rules ++ newRules
      generalRules = remainingRules
      iters += 1
    }
    Program(rules)
  }

  def _getRelevantScoredRules(idb: Set[Tuple], rules: Set[Rule], learnedRules: Set[Rule]): Set[ScoredRule] = {
    val curOutRels = idb.map(_.relation)
    val relevantRules = rules.filter(r => curOutRels.contains(r.head.relation))
    relevantRules.map(r => scoreRule(r, idb, learnedRules))
  }

  def learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule]): (Set[Tuple], Set[Rule], Set[Rule]) = {

    /** Iteratively increase the number of constants */

    var newRules: Set[Rule] = Set()
    var coveredExamples: Set[Tuple] = Set()
    var remainingRules: Set[Rule] = Set()

    var maxConstants = 0
    while (newRules.isEmpty && maxConstants <= 2) {
      logger.info(s"searching rules with at most $maxConstants constants.")
      val ruleBuilder = ConstantBuilder(problem.inputRels, problem.outputRels, problem.edb, problem.idb, maxConstants)
      val (_coveredExamples, _newRules, _remainingRules) = _learnNRules(idb, generalSimpleRules, learnedRules, ruleBuilder.refineRule)

      newRules = _newRules
      coveredExamples = _coveredExamples
      remainingRules = _remainingRules

      maxConstants += 1
    }
    (coveredExamples, newRules, remainingRules)
  }

  def _learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule],
                   refineRule: Rule => Set[Rule],
                   maxExtraIters: Int = 1): (Set[Tuple], Set[Rule], Set[Rule]) = {
    var iters: Int = 0
    var extraIters: Int = 0

    // score the rules based on current idb set
    val generalRules = _getRelevantScoredRules(idb, generalSimpleRules, learnedRules)

    var forbiddenRules: Set[ScoredRule] = Set()

    // Set up the pool of rules to be refine
    var rulePool: mutable.PriorityQueue[ScoredRule] = new mutable.PriorityQueue()
    rulePool ++= generalRules.filter(r => r.isValid() || r.isTooGeneral())

    var validRules: Set[ScoredRule] = generalRules.filter(_.isValid())

    while (iters < maxRefineIters && extraIters < maxExtraIters && rulePool.nonEmpty) {

      // pop highest scored rule from pool
      val baseRule: Rule = rulePool.dequeue().rule

      // refine the rules
      val refinedRules = refineRule(baseRule)
      val candidateRules: Set[ScoredRule] = refinedRules.map(r => scoreRule(r, idb, learnedRules))

      // keep the valid ones
      validRules ++= candidateRules.filter(_.isValid())

      // Put the too general ones into the pool, and forbid anything else from exploring again
      val tooGeneral = candidateRules.filter(_.isTooGeneral())
      forbiddenRules ++= candidateRules.diff(validRules++tooGeneral)
      rulePool ++= tooGeneral.diff(forbiddenRules)

      iters += 1
      if (validRules.nonEmpty) extraIters += 1
    }
    // require(validRules.nonEmpty, s"Synthesis failed: empty valid rules.")
    logger.info(s"Found ${validRules.size} rules after $iters iterations.\n")

    val remainingRules: Set[Rule] = {
      val rs = rulePool.toSet
      rs.map(_.rule)
    }
    val newLearnedRules: Set[Rule] = validRules.map(_.rule)
    val newIdb = if(newLearnedRules.nonEmpty) {
      evaluator.evalRules(newLearnedRules, learnedRules)
    }
    else {Set[Tuple]()}
    // assert(newIdb.nonEmpty)
    (newIdb, newLearnedRules, remainingRules)
  }

  def scoreRule(rule: Rule, allIdb: Set[Tuple], learnedRules: Set[Rule]): ScoredRule = {
    val refIdb = evaluator.getRefIdb(rule, allIdb)
    def f_eval: Rule => Set[Tuple] = r => evaluator.evalRule(r, learnedRules)
    ScoredRule(rule, refIdb, f_eval)
  }

}