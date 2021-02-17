package synthesis.search

import com.typesafe.scalalogging.Logger
import synthesis._
import synthesis.rulebuilder.ConstantBuilder

import scala.collection.mutable

case class BasicSynthesis(problem: Problem,
                          maxIters: Int = 200,
                          maxRefineIters: Int = 200,
                         ) extends Synthesis(problem) {
  def learnNPrograms(idb: Set[Tuple]) = List(learnAProgram(idb))

  private val logger = Logger("Synthesis")

  private val ruleConstructor = ConstantBuilder(problem.inputRels, problem.outputRels, problem.edb, problem.idb)
  private val evaluator = PartialRuleEvaluator(problem)


  def learnAProgram(idb: Set[Tuple]): Program = {
    var examples: Set[Tuple] = idb
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

  def learnARule(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule],
                 validCondition: ScoredRule => Boolean = ScoredRule.isValid,
                 refineCondition: ScoredRule => Boolean = ScoredRule.isTooGeneral,
                ): (Set[Tuple], Rule, Set[Rule]) = {
    var iters: Int = 0

    // score the rules based on current idb set
    val generalRules = {
      val curOutRels = idb.map(_.relation)
      val relevantRules = generalSimpleRules.filter(r => curOutRels.contains(r.head.relation))
      relevantRules.map(r => scoreRule(r, idb, learnedRules))
    }

    // Set up the pool of rules to be refine
    var rulePool: mutable.PriorityQueue[ScoredRule] = new mutable.PriorityQueue()
    rulePool ++= generalRules.filter(r => validCondition(r) || refineCondition(r))

    var validRules: Set[ScoredRule] = generalRules.filter(validCondition)

    while (iters < maxRefineIters && validRules.isEmpty) {

      // pop highest scored rule from pool
      val baseRule: Rule = rulePool.dequeue().rule

      // refine the rules
      val refinedRules = ruleConstructor.refineRule(baseRule)
      val candidateRules: Set[ScoredRule] = refinedRules.map(r => scoreRule(r, idb, learnedRules))

      // keep the valid ones
      validRules ++= candidateRules.filter(validCondition)

      // Put the too general ones into the pool
      val tooGeneral = candidateRules.filter(refineCondition)
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

