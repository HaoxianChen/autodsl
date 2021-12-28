package synthesis.search

import com.typesafe.scalalogging.Logger
import synthesis.activelearning.ExampleInstance
import synthesis.search.SynthesisAllPrograms.sample
import synthesis.util.Misc
import synthesis.{Literal, Problem, Program, Relation, Rule, Tuple}

import scala.util.Random

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
      ans = _learnNRules(idb, generalSimpleRules, learnedRules, ruleBuilder.refineRule, _maxExtraIters = 20,
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
    var next: ScoredRule = sample(rulePool)

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
      val higherScore = tooGeneral.filter(_.score > baseRule.score)
      val unexpandedValidRules = validRules.filterNot(sr => expanded.contains(sr.rule))
      val toExpandValidRules: Boolean = unexpandedValidRules.nonEmpty && expandedValidRules.size < maxExpandedValidRules
      next = if (toExpandValidRules) {
        /** Keep exploring alternative valid rules. */
        // assert(higherScore.isEmpty, s"${higherScore}")
        sample(unexpandedValidRules, baseScore = baseRule.score)
      }
      else if (higherScore.nonEmpty) {
      // if (higherScore.nonEmpty) {
        /** Always go higher score along the current path if such option exists. */
        sample(higherScore, baseScore = baseRule.score)
      }
      else {
        sample(rulePool, baseScore = baseRule.score)
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

object SynthesisAllPrograms {
  val rnd = new Random()
  def uniformSample[T](set: Set[T]): T = set.toList(rnd.nextInt(set.size))

  /** T is the temperature function.
   * Always 1 when e2 > e1
   * When e2 < e1, T the higher, the acceptanceProbability is higher
   *  */
  def acceptanceProbability(e1: Double, e2: Double, T: Double)  :Double = {
    require(T>0)
    if (e2 > e1) {
      1
    }
    else {
      math.pow(math.E,-(e1-e2)/T)
    }
  }

  def sample(candidates: Set[ScoredRule], baseScore :Double = 0): ScoredRule = {
    def temperature(r: Double): Double = {
      require(r>0 && r<=1)
      1*r
    }
    val K = 20 // Max number of random walks to sample next candidate
    var i: Int = 0
    var next: Option[ScoredRule] = None
    while (next.isEmpty && i<K) {
      val n = uniformSample(candidates)
      val T = temperature(1.0  - i.toDouble/K)
      if (acceptanceProbability(baseScore,n.score,T) > rnd.nextInt(1)) {
        next = Some(n)
      }
      i += 1
    }
    assert(next.isDefined, s"Failed to sample next candidate after $K random walks.")
    next.get
  }


}

case class SyntaxConstraint() {
  def filter(rule: Rule): Boolean = {
    /** The negated use of != filter is redundant */
    val negLits: Set[Literal] = rule.negations
    val hasNegInEq: Boolean = negLits.exists(_.relation.name == s"UnEqual")

    /** Relations reserved for events, i.e., relation name with
     * prefix 'recv' or 'send', should appear in body once, positively. */
    def isEventLiteral(lit: Literal): Boolean = {
      lit.relation.name.startsWith("recv") ||
        lit.relation.name.contains("packet_in")
    }
    val hasNegEventRel: Boolean = negLits.exists(isEventLiteral)

    val redundantEventRel: Boolean = {
      val eventLits = rule.body.filter(isEventLiteral)
      eventLits.size > 1
    }

    val redundantAgg = hasRedundantAggRelations(rule)
    val unusedAgg = unusedAggOutput(rule)
    val negatedAgg = negatedAggregate(rule)

    /** Inequal and greater cannot apply to same parameters. */
    // todo.
    (!hasNegInEq) && (!hasNegEventRel) && (!redundantEventRel) &&
      (!redundantAgg) && (!unusedAgg) &&
      (!negatedAgg)
  }

  def negatedAggregate(rule: Rule): Boolean = {
    rule.negations.intersect(aggLits(rule)).nonEmpty
  }

  val aggSubString: Set[String] = Set("cnt", "max")

  def aggLits(rule: Rule): Set[Literal] = {
    rule.body.filter(lit => {
      aggSubString.exists(s => lit.relation.name.contains(s"_${s}"))
    })
  }

  def unusedAggOutput(rule: Rule): Boolean = {
    val ret = aggLits(rule).exists(lit => lit.fields.last.name == "_")
    ret
  }

  def hasRedundantAggRelations(rule: Rule): Boolean = {
    /** Don't have multiple aggregate predicates in the rule */
    val aggSubString: List[String] = List("cnt", "max")

    // val aggLits: Set[Literal] = rule.body.filter(lit => {
    //   aggSubString.exists(s => lit.relation.name.contains(s))
    // })

    val aggRelList = aggLits(rule).toList.map ( lit => {
      val relName = lit.relation.name
      aggSubString.flatMap(key => {
        if (relName.contains(key)) Some(relName.split(key).head)
        else (None)
      })
    })
    /** Check duplicates in aggregated relations */
    aggRelList.toSet.size < aggRelList.size

  }
}