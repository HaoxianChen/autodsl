package synthesis.search

import com.typesafe.scalalogging.Logger
import synthesis.{Literal, Misc, Problem, Program, Relation, Rule, Tuple}

import scala.collection.mutable
import scala.math.abs

case class SynthesisAllPrograms(problem: Problem,
                                recursion: Boolean = true,
                                maxIters: Int = 20,
                                maxRefineIters: Int = 100,
                                maxRules: Int = 5,
                                initConfigSpace: SynthesisConfigSpace = SynthesisConfigSpace.emptySpace()
                               ) extends Synthesis(problem) {

  private val logger = Logger("Synthesis")

  private val evaluator = PartialRuleEvaluator(problem)
  private val configSpace: SynthesisConfigSpace = {
    if (initConfigSpace.isEmpty) SynthesisConfigSpace.getConfigSpace(problem) else initConfigSpace
  }

  private val syntaxConstraint = SyntaxConstraint()

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

    while (examples.nonEmpty && iters < maxIters ) {
      val (coveredExamples, newRules, remainingRules) = learnNRules(examples, generalRules, rules)
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
          val ans = scoredRules.sorted(Ordering[ScoredRule].reverse).map(_.rule)
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
                  validCondition: ScoredRule => Boolean = ScoredRule.isValid): (Set[Tuple], Set[Rule], Set[Rule]) = {

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
      ans = _learnNRules(idb, generalSimpleRules, learnedRules, ruleBuilder.refineRule, maxExtraIters = 10,
      validCondition = validCondition)

      if (ans._2.isEmpty) {
        config = configSpace.next_config()
        logger.info(s"Increase config space ${config}")
      }
    } while (ans._2.isEmpty)
    ans
  }

  def _learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule],
                   refineRule: Rule => Set[Rule],
                   validCondition: ScoredRule => Boolean = ScoredRule.isValid,
                   refineCondition: ScoredRule => Boolean = ScoredRule.isTooGeneral,
                   maxExtraIters: Int = 1): (Set[Tuple], Set[Rule], Set[Rule]) = {
    var iters: Int = 0
    var extraIters: Int = 0

    val maxBranching: Int = 50

    // score the rules based on current idb set
    val generalRules = _getRelevantScoredRules(idb, generalSimpleRules, learnedRules)

    // var forbiddenRules: Set[ScoredRule] = Set()
    var exploredRules: Set[Rule] = Set()

    // Set up the pool of rules to be refine
    var rulePool: mutable.PriorityQueue[ScoredRule] = new mutable.PriorityQueue()
    rulePool ++= generalRules.filter(r => validCondition(r) || refineCondition(r))

    var validRules: Set[ScoredRule] = generalRules.filter(validCondition)

    while (iters < maxRefineIters && extraIters < maxExtraIters && rulePool.nonEmpty && validRules.size < maxRules) {

      // pop highest scored rule from pool
      val baseRule = rulePool.dequeue()

      // refine the rules
      val allRefinedRules = refineRule(baseRule.rule).diff(exploredRules)
      val refinedRules = if (allRefinedRules.size > maxBranching) {
        Misc.sampleSet(allRefinedRules, maxBranching)
      }
      else allRefinedRules

      require(refinedRules.size <= maxBranching)
      val candidateRules: Set[ScoredRule] = refinedRules
          .filter(syntaxConstraint.filter)
          .map(r => scoreRule(r, idb, learnedRules, Some(baseRule)))

      // Remember the ones that have been explored
      exploredRules ++= refinedRules

      // keep the valid ones
      validRules ++= candidateRules.filter(validCondition)
      rulePool ++= candidateRules.filter(validCondition) // see if more specific option is possible

      // Put the too general ones into the pool, and forbid anything else from exploring again
      val tooGeneral = candidateRules.filter(refineCondition)

      val staled = tooGeneral.filter(isRuleStaled)
      rulePool ++= tooGeneral.diff(staled)
      // If not all branches are exhausted yet
      if (refinedRules.size < allRefinedRules.size) rulePool += baseRule

      iters += 1
      if (validRules.nonEmpty) extraIters += 1
    }
    if (rulePool.isEmpty) logger.info(s"Exhausted all rules")
    if (iters == maxRefineIters) logger.info(s"Hit maximum iterations ${maxExtraIters}")
    if (extraIters == maxExtraIters) logger.info(s"Hit maximum extra iterations ${maxExtraIters}")
    if (validRules.size == maxRules) logger.info(s"Hit maximum rules ${maxRules}")
    // require(validRules.nonEmpty, s"Synthesis failed: empty valid rules.")
    logger.info(s"Found ${validRules.size} rules after $iters iterations.\n")

    val remainingRules: Set[Rule] = {
      val rs = rulePool.toSet
      rs.map(_.rule)
    }
    val newLearnedRules: Set[Rule] = validRules.map(_.rule)
    val newIdb = if(newLearnedRules.nonEmpty) {
      evaluator.evalRules(newLearnedRules, learnedRules, this.getConfigSpace.get_config().recursion)
    }
    else {Set[Tuple]()}
    // assert(newIdb.nonEmpty)
    (newIdb, newLearnedRules, remainingRules)
  }

  def isRuleStaled(rule: ScoredRule): Boolean = {
    if (rule.score_history.size == ScoredRule.maxHistSize) {
      rule.diff().map(d=>abs(d)).max < 1e-3
    }
    else {
      false
    }
  }

  def scoreRule(rule: Rule, allIdb: Set[Tuple], learnedRules: Set[Rule], parentRule: Option[ScoredRule]=None): ScoredRule = {
    val refIdb = evaluator.getRefIdb(rule, allIdb)
    def f_eval: Rule => Set[Tuple] = r => evaluator.evalRule(r, learnedRules,
      this.getConfigSpace.get_config().recursion)
    ScoredRule(rule, refIdb, f_eval, parentRule)
  }

}

case class SyntaxConstraint() {
  def filter(rule: Rule): Boolean = {
    /** The negated use of != filter is redundant */
    val negLits: Set[Literal] = rule.negations
    val hasNegInEq: Boolean = negLits.exists(_.relation.name == s"UnEqual")

    /** Relations reserved for events, i.e., relation name with
     * prefix 'recv' or 'send', should appear in body once, positively. */
    val hasNegEventRel: Boolean = negLits.exists(_.relation.name.startsWith("recv"))

    /** Inequal and greater cannot apply to same parameters. */
    // todo.
    (!hasNegInEq) && (!hasNegEventRel)
  }
}