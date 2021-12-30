package synthesis.search

import com.typesafe.scalalogging.Logger
import synthesis.activelearning.ExampleInstance
import synthesis.util.Misc
import synthesis.{Problem, Program, Relation, Rule, Tuple}

case class SynthesisAllPrograms(problem: Problem,
                                maxIters: Int = 20,
                                maxRefineIters: Int = 3000,
                                maxRules: Int = 10,
                                initConfigSpace: SynthesisConfigSpace = SynthesisConfigSpace.emptySpace()
                               ) extends Synthesis(problem) {

  private val logger = Logger("Synthesis")

  private val evaluator = PartialRuleEvaluator(problem)
  private val configSpace: SynthesisConfigSpace = {
    if (initConfigSpace.isEmpty) SynthesisConfigSpace.getConfigSpace(problem) else initConfigSpace
  }

  private val syntaxConstraint = SyntaxConstraint()
  private val exampleInstances: Set[ExampleInstance] = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)

  def getConfigSpace: SynthesisConfigSpace = configSpace

  def getMostGenearlRules(): Set[Rule] = {
    val ruleBuilder = configSpace.get_config().get_rule_builder(problem)
    ruleBuilder.mostGeneralRules()
  }

  def learnNPrograms(idb: Set[Tuple]): List[Program] = {
    require(idb.map(_.relation).size == 1, s"Only idb of one relation at a time.")

    /** Learn all possible rules, then combine them. */
    var examples: Set[Tuple] = idb
    var rules: Set[Rule] = Set()
    var iters: Int = 0

    // Init the rule pool with the most general rules
    var generalRules: Set[Rule] = getMostGenearlRules()

    /** The goal of each iteration is to recover one perfect precision rule at a time. */
    def getScore(sr: ScoredRule): Double = sr.precision * sr.completeness
    def validCondition(sr: ScoredRule): Boolean = getScore(sr) >= 1.0
    def refineCondition(sr: ScoredRule): Boolean = getScore(sr) > 0

    while (examples.nonEmpty && iters < maxIters ) {
      val (coveredExamples, newRules, remainingRules) = learnNRules(examples, generalRules, rules,
        getScore = getScore, validCondition = validCondition, refineCondition=refineCondition)
      val nextExamples = examples -- coveredExamples
      require(nextExamples.size < examples.size)
      examples = nextExamples
      rules = rules ++ newRules
      if (!configSpace.get_config().recursion) generalRules = remainingRules
      iters += 1
    }
    assert(examples.isEmpty)
    val programs = combineRules(rules, idb)
    logger.debug(s"Found ${programs.size} programs.")
    /** sort programs */
    programs.sorted
  }

  def combineRules(rules: Set[Rule], idb: Set[Tuple]): List[Program] = {
    /** Return all combinations of rules that cover all idb
     * how to handle recursion? */

    val maxPrograms = 10

    def sortScoredRules(a: ScoredRule, b: ScoredRule) :Boolean = {
      /** rule a is more preferable than rule b if:
       * 1. rule a produces more idb */
      if (a.idb.size > b.idb.size) true
      else if (a.idb.size < b.idb.size) false

      else if (a.rule.body.size < b.rule.body.size) true
      else if (a.rule.body.size > b.rule.body.size) false

      else if (a.rule.freeVariables().size > b.rule.freeVariables().size) true
      else if (a.rule.freeVariables().size < b.rule.freeVariables().size) false

      else true
    }

    def _combineRules(learnedRules: List[Rule], remainingRules: List[Rule], remainingIdb: Set[Tuple]): Set[List[Rule]] = {
      if (remainingIdb.isEmpty) {
        Set(List())
      }
      else {
        remainingRules match {
          case Nil => Set()
          case head :: tail => {
            // keep this rule
            val ruleSet1: Set[List[Rule]] = {
              val coveredIdb = evaluator.evalRule(head, learnedRules.toSet, getConfigSpace.get_config().recursion)
              val nextIdb = remainingIdb.diff(coveredIdb)

              if (coveredIdb.intersect(remainingIdb).nonEmpty) {
                val nextLearnedRules = learnedRules :+ head
                val nextRules = _combineRules(nextLearnedRules, tail, nextIdb)
                nextRules.map(rl => head :: rl)
              }
              else {
                Set()
              }
            }

            if (ruleSet1.size > maxPrograms) {
              // If already reaches maximum programs, discard the other branch
              ruleSet1
            }
            else{
              // explore the other branch: not keeping this rule
              ruleSet1 ++ _combineRules(learnedRules, tail, remainingIdb)
            }
          }
        }
      }
    }

    // sort: non-recursive first, recursion later
    // val ruleList: List[Rule] = rules.toList.sorted
    val ruleList: List[Rule] = {
      val recursiveRules = rules.filter(_.isRecursive())
      val nonRecursiveRules = rules.diff(recursiveRules)

      val nonRecursiveSorted: List[Rule] = {
        // def outputCounts(rule: Rule): Int = {
        //   val idb = evaluator.evalRule(rule, Set())
        //   idb.size
        // }
        // nonRecursiveRules.toList.sortBy(outputCounts)(Ordering[Int].reverse)
        if (recursiveRules.nonEmpty){
          // If exists recursive rules, sort by rule size
          nonRecursiveRules.toList.sortBy(_.body.size)
        }
        else {
          // If non-recursive, sort by output size
          val scoredRules: List[ScoredRule] = nonRecursiveRules.map(r => scoreRule(r, idb, Set())).toList
          // val ans = scoredRules.sorted(Ordering[ScoredRule].reverse).map(_.rule)
          val ans = scoredRules.sortWith(sortScoredRules).map(_.rule)
          ans
        }
      }
      nonRecursiveSorted ::: recursiveRules.toList.sorted
    }
    logger.debug(s"Combine ${ruleList.size} rules into programs")
    val programs = _combineRules(List(),ruleList, idb).map(
      rs=>Program(rs.toSet)
    )
    programs.toList.sortBy(_.rules.size)(Ordering[Int])
  }

  def _getRelevantScoredRules(idb: Set[Tuple], rules: Set[Rule], learnedRules: Set[Rule]): Set[ScoredRule] = {
    val curOutRels = idb.map(_.relation)
    val relevantRules = rules.filter(r => curOutRels.contains(r.head.relation))
    relevantRules.map(r => scoreRule(r, idb, learnedRules))
  }

  def learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule],
                  getScore: ScoredRule => Double,
                  validCondition: ScoredRule => Boolean,
                  refineCondition: ScoredRule => Boolean,
                  ): (Set[Tuple], Set[Rule], Set[Rule]) = {

    /** Lookup the configuration for the synthesizer*/
    val relevantOutRel: Set[Relation] = idb.map(_.relation)
    require(relevantOutRel.subsetOf(problem.outputRels), s"${relevantOutRel}, ${problem.outputRels}")

    /** This while loop incrementally increase the program space */
    var config = configSpace.get_config()
    var ans: (Set[Tuple], Set[Rule], Set[Rule]) = (Set(), Set(), Set())

    do {
      // val ruleBuilder = ConstantBuilder(problem.inputRels, relevantOutRel, problem.edb, problem.idb, config.recursion,
      //   config.maxConstants)
      val ruleBuilder = config.get_rule_builder(problem, relevantOutRels = relevantOutRel)
      ans = _learnNRules(idb, generalSimpleRules, learnedRules, ruleBuilder.refineRule,
        getScore = getScore,
        validCondition = validCondition,
        refineCondition = refineCondition,
        _maxExtraIters = 20,
      )

      if (ans._2.isEmpty) {
        config = configSpace.next_config()
        logger.info(s"Increase config space ${config}")
      }
    } while (ans._2.isEmpty)
    ans
  }

  def _learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule],
                   refineRule: Rule => Set[Rule],
                   getScore: ScoredRule => Double,
                   validCondition: ScoredRule => Boolean,
                   refineCondition: ScoredRule => Boolean,
                   _maxExtraIters: Int = 20,
                   disambiguate: Boolean = true): (Set[Tuple], Set[Rule], Set[Rule]) = {
    var iters: Int = 0
    var extraIters: Int = 0
    var maxExtraIters = 0
    logger.info(s"max extra iterations: ${_maxExtraIters}.")

    /*** Keep the maximum number of candidate programs to be evaluated at one iteration.
    * So that each iteration is not too long. */
    val maxBranching: Int = 50

    /** score the rules based on current idb set */
    val generalRules = if (generalSimpleRules.nonEmpty) {
      _getRelevantScoredRules(idb, generalSimpleRules, learnedRules)
    }
    else {
      _getRelevantScoredRules(idb, getMostGenearlRules(), learnedRules)
    }

    /** Remember rules that have been evaluated, so do not evaluate them twice. */
    var evaluated: Set[Rule] = generalRules.map(_.rule)
    /** Remember rules whose offspring have been explored. */
    var expanded: Set[Rule] = Set()
    var expandedValidRules: Set[ScoredRule] = Set()
    val maxExpandedValidRules: Int = 5

    // Set up the pool of rules to be refine
    // var rulePool: mutable.PriorityQueue[ScoredRule] = new mutable.PriorityQueue()
    var rulePool: Set[ScoredRule] = Set()
    rulePool ++= generalRules.filter(r => validCondition(r) || refineCondition(r))
    // var next: ScoredRule = sampleNextCandidate(rulePool)
    var next: ScoredRule = SimulatedAnnealing.sample(rulePool, getScore)

    var validRules: Set[ScoredRule] = generalRules.filter(validCondition)

    while (iters < maxRefineIters && extraIters <= maxExtraIters && rulePool.nonEmpty && validRules.size < maxRules) {
      if (iters % 20 == 0) logger.info(s"iteration $iters")

      // pop highest scored rule from pool
      // val baseRule = rulePool.dequeue()
      val baseRule = next

      /** Expand the current rule */
      val allRefinedRules = refineRule(baseRule.rule).diff(evaluated)
      val (refinedRules,exhaustedBaseRule) = if (allRefinedRules.size > maxBranching) {
        (Misc.sampleSet(allRefinedRules, maxBranching), false)
      }
      else (allRefinedRules, true)
      require(refinedRules.size <= maxBranching)
      /** Drop baseRule from the pool if all its offspring have been explored */
      if (exhaustedBaseRule) {
        // pop the current rule from the rule pool
        rulePool -= baseRule
        expanded += baseRule.rule
        if (validRules.contains(baseRule)) expandedValidRules += baseRule
      }

      val candidateRules: Set[ScoredRule] = refinedRules
          .filter(syntaxConstraint.filter)
          .map(r => scoreRule(r, idb, learnedRules, Some(baseRule)))
      /** Remember rules that have been evaluated */
      evaluated ++= refinedRules

      // keep the valid ones
      val newValidRules = candidateRules.filter(validCondition)
      validRules ++= newValidRules

      // Put the too general ones into the pool, and forbid anything else from exploring again
      val tooGeneral = candidateRules.filter(refineCondition)

      /** Update rule pool */
      // Remember the ones that have been explored
      rulePool ++= (newValidRules ++ tooGeneral)

      /** Sample next rule */
      val baseScore = getScore(baseRule)
      val higherScore = tooGeneral.filter(sr => getScore(sr)>baseScore)
      val unexpandedValidRules = validRules.filter(refineCondition)
        .filterNot(sr => expanded.contains(sr.rule))
      val toExpandValidRules: Boolean = {
        unexpandedValidRules.nonEmpty && expandedValidRules.size < maxExpandedValidRules
      }
      next = if (toExpandValidRules) {
        /** Keep exploring alternative valid rules. */
        // assert(higherScore.isEmpty, s"${higherScore}")
        SimulatedAnnealing.sample(unexpandedValidRules, getScore, baseScore = baseScore)
      }
      else if (higherScore.nonEmpty) {
      // if (higherScore.nonEmpty) {
        /** Always go higher score along the current path if such option exists. */
        // sampleNextCandidate(higherScore, baseScore = baseRule.score)
        SimulatedAnnealing.sample(higherScore, getScore, baseScore=baseScore)
      }
      else {
        SimulatedAnnealing.sample(rulePool, getScore, baseScore=baseScore)
      }
      assert(evaluated.contains(next.rule))
      assert(!expanded.contains(next.rule))

      /** Remember the number of iterations to find a rule.  */
      if (maxExtraIters==0 && validRules.nonEmpty) {
        maxExtraIters = if (iters > _maxExtraIters) iters else _maxExtraIters
        assert(maxExtraIters>0)
        logger.debug(s"Find first rule after $iters iterations. " +
          s"Keep exploring for $maxExtraIters iterations.")
      }

      iters += 1
      if (validRules.nonEmpty && !toExpandValidRules) extraIters += 1
    }
    if (rulePool.isEmpty) logger.info(s"Exhausted all rules")
    if (iters == maxRefineIters) logger.info(s"Hit maximum iterations ${maxExtraIters}")
    if (extraIters == maxExtraIters) logger.info(s"Hit maximum extra iterations ${maxExtraIters}")
    if (validRules.size == maxRules) logger.info(s"Hit maximum rules ${maxRules}")
    // require(validRules.nonEmpty, s"Synthesis failed: empty valid rules.")
    logger.info(s"Found ${validRules.size} rules after $iters iterations.\n")

    val remainingRules: Set[Rule] = Set()
    // val remainingRules = rulePool.map(_.rule)
    val newLearnedRules: Set[Rule] = validRules.map(_.rule)
    val newIdb = if(newLearnedRules.nonEmpty) {
      evaluator.evalRules(newLearnedRules, learnedRules, this.getConfigSpace.get_config().recursion)
    }
    else {Set[Tuple]()}
    // assert(newIdb.nonEmpty)
    (newIdb, newLearnedRules, remainingRules)
  }

  def scoreRule(rule: Rule, allIdb: Set[Tuple], learnedRules: Set[Rule], parentRule: Option[ScoredRule]=None): ScoredRule = {
    val refIdb = evaluator.getRefIdb(rule, allIdb)
    def f_eval: Rule => Set[Tuple] = r => evaluator.evalRule(r, learnedRules,
      this.getConfigSpace.get_config().recursion)
    ScoredRule(rule, refIdb, f_eval, parentRule)
  }

}