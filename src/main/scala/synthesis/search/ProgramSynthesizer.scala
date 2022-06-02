package synthesis.search

import com.typesafe.scalalogging.Logger
import synthesis._
import synthesis.rulebuilder.AggregateLiteral
import synthesis.util.Misc

import scala.collection.parallel.CollectionConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, MILLISECONDS, SECONDS}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

class ProgramSynthesizer(problem: Problem, initConfigSpace: SynthesisConfigSpace,
                        threads: Int=5) extends Synthesis(problem) {
  /** Synthesize program on a program basis */

  // private val ruleLearner = SynthesisAllPrograms(problem)
  private val evaluator = new PartialProgramEvaluator(problem)
  private val syntaxConstraint = SyntaxConstraint()
  private val logger = Logger(s"ProgramSynthesizer")
  private val ruleHelper = SynthesisAllPrograms(problem)

  private val maxBranching: Int = 50
  private val maxIters: Int = 1000

  private val configSpace: SynthesisConfigSpace = {
    if (initConfigSpace.isEmpty) SynthesisConfigSpace.getConfigSpace(problem) else initConfigSpace
  }
  private var programBuilder: ProgramBuilder = ProgramBuilder(problem, configSpace.get_config())
  logger.info(s"${programBuilder.getAggregators.size} aggreators.")

  def getConfigSpace: SynthesisConfigSpace = configSpace

  def learnNPrograms(idb: Set[Tuple]): List[Program] = learnNProgramsSingleThread(idb)

  def learnNProgramsMultiThreads(idb: Set[Tuple]): List[Program] = {
    logger.info(s"$threads threads.")
    val idbList: List[Set[Tuple]] = List.fill(threads)(idb)
    val allResFuture = idbList.par.map(ts => {
      Future(learnNProgramsSingleThread(ts))
    }).toList
    val allPrograms: Set[Program] = {
      var remainingResults: List[Future[List[Program]]] = allResFuture
      var solutions: List[Program] = List()
      var remainingTime: Int = 30 * 60
      val timeStep: Int = 1
      val completionThreshold = 0.3
      var nFinished: Int = 0
      assert(threads*completionThreshold >= 1)
      while (nFinished < threads*completionThreshold &&
        remainingResults.nonEmpty &&
        remainingTime > 0
        ) {
        Thread.sleep(timeStep * 1000)
        var nextRemainingResults: List[Future[List[Program]]] = List()
        for (f <- remainingResults) {
          f.value match {
            case Some(Success(x)) => {
              solutions ++:= x
              nFinished += 1
            }
            case Some(Failure(exception)) => logger.warn(s"$exception")
            case None => nextRemainingResults :+= f
          }
        }
        remainingTime -= timeStep
        if (solutions.nonEmpty && remainingTime>20*timeStep) remainingTime = 20*timeStep
        remainingResults = nextRemainingResults
        logger.info(s"${nFinished} threads finished, ${remainingResults.size} running, remaining time ${remainingTime} s.")
      }
      /** kill everything still running */
      for (f <- remainingResults) {
        Await.result(f, Duration(0,MILLISECONDS))
      }
      solutions.toSet
    }
    allPrograms.toList.sortWith(_<_)
  }

  def learnNProgramsSingleThread(idb: Set[Tuple]): List[Program] = {
    /** This while loop incrementally increase the program space */
    var config = configSpace.get_config()
    var ans: List[Program] = List()

    do {
      ans = _learnNPrograms(idb)

      if (ans.isEmpty) {
        config = configSpace.next_config()
        logger.info(s"Increase config space ${config}")
        programBuilder = ProgramBuilder(problem, config)
      }
    } while (ans.isEmpty)
    ans
  }

  def _learnNPrograms(idb: Set[Tuple]): List[Program] = {
    var validPrograms: Set[ScoredProgram] = Set()
    // var evaluatedPrograms: Set[Program] = Set()
    var evaluatedPrograms: Set[Program] = Set()
    var expandedPrograms: Set[Program] = Set()
    var remainingIdb: Set[Tuple] = idb

    val refRel: Set[Relation] = idb.map(_.relation)

    // var programPool: mutable.PriorityQueue[ScoredProgram] = new mutable.PriorityQueue()
    var programPool: Set[ScoredProgram] = Set()
    var validRules: Set[Rule] = Set()
    var newValidRules: Set[Rule] = Set()

    // init program Pool with the most general programs
    val mostGeneralPrograms = programBuilder.mostGeneralPrograms().filter(
      p => p.rules.forall(r => refRel.contains(r.head.relation))
    )
    programPool ++= mostGeneralPrograms.map(p => evalProgram(p, validRules, idb)).filter(needsRefinement)
    evaluatedPrograms ++= mostGeneralPrograms

    var extraIters: Int = 0
    val maxExtraIters: Int = 20
    val maxSolutionSize: Int = 10
    var iters: Int = 0
    assert(programPool.nonEmpty, s"$idb")
    var next: Option[ScoredProgram] = {
      def _getScore(sp: ScoredProgram): Double = sp.score
      Some(SimulatedAnnealing.sample(programPool, _getScore))
    }
    while (next.isDefined && validPrograms.size < maxSolutionSize && iters<maxIters
      // && extraIters < maxExtraIters
      && remainingIdb.nonEmpty
    ) {
      val baseProgram: ScoredProgram = next.get
      assert(!expandedPrograms.contains(baseProgram.program))

      val allRefinedPrograms: Set[Program] = refineProgram(baseProgram, remainingIdb, validRules).diff(evaluatedPrograms)
      val (refinedPrograms, isBaseProgramExhausted) = if (allRefinedPrograms.size > maxBranching) {
        (Misc.sampleSet(allRefinedPrograms, maxBranching), false)
      } else (allRefinedPrograms, true)
      if (isBaseProgramExhausted) {
        expandedPrograms += baseProgram.program
        programPool -= baseProgram
      }

      val candidatePrograms: Set[ScoredProgram] = refinedPrograms.
        diff(expandedPrograms).
        map(p => Program(p.rules++validRules)).
        map(p => evalProgram(p, validRules, remainingIdb))
      val toRefine = candidatePrograms.filter(needsRefinement)

      /** Rule sharing */
      newValidRules ++= {
        val vr = getValidRules(toRefine).diff(validRules)
        if (vr.nonEmpty) logger.info(s"Found ${vr.size} valid rules.")
        vr
      }

      /** Update the pool of valid programs */
      if (newValidRules.nonEmpty &&
        (extraIters >= maxExtraIters || newValidRules.size > 10)) {
        validRules ++= newValidRules
        newValidRules = Set()
        /** Reset everything else */
        programPool = mostGeneralPrograms
          .map(p=> Program(p.rules++validRules))
          // .diff(expandedPrograms)
          // .map(p => evalProgram(p, validRules, idb))
          .map(p => evalProgram(p, validRules, remainingIdb))
          .filter(needsRefinement)
        expandedPrograms = Set()
        evaluatedPrograms = Set()
        remainingIdb = updateIdb(validRules, idb)
        logger.info(s"Update remaining idb ${extraIters} iterations, and reset program pool")
        extraIters = 0
        next = if (programPool.nonEmpty) {
            def _getScore(sp: ScoredProgram): Double = sp.score
            Some(SimulatedAnnealing.sample(programPool, _getScore, _getScore(baseProgram)))
          }
          else {
            None
          }
      }
      else {
        val pnext = toRefine
        programPool ++= toRefine

        /** Sample next candidate program to explore */
        val higherScore: Set[ScoredProgram] = pnext.filter(_.score>baseProgram.score)
        val higherRecall = pnext.filter(_.recall > baseProgram.recall)
        // val perfectRecall: Set[ScoredProgram] = programPool.filter(p => p.recall >= 1.0 && p.completeness >= 1.0)
        val maxExpandedValidPrograms: Int = 5
        val unexpandedValidPrograms: Set[ScoredProgram] = validPrograms.filterNot(sp=>expandedPrograms.contains(sp.program))
        val toExpandValidPrograms: Boolean = {
          val expandedValidPrograms: Set[Program] = expandedPrograms.intersect(validPrograms.map(_.program))
          unexpandedValidPrograms.nonEmpty && expandedValidPrograms.size < maxExpandedValidPrograms
        }
        next =
        // if (perfectRecall.nonEmpty) {
        //   sampleNextCandidateProgram(perfectRecall, baseProgram.score)
        // }
        // else if(higherScore.nonEmpty) {
          if (toExpandValidPrograms) {
            def _getScore(sp: ScoredProgram): Double = sp.score
            Some(SimulatedAnnealing.sample(unexpandedValidPrograms, _getScore, _getScore(baseProgram)))
          }
          else if(higherScore.nonEmpty) {
            /** Always go along the current path if higher score available */
            def _getScore(sp: ScoredProgram): Double = sp.score
            Some(SimulatedAnnealing.sample(higherScore, _getScore, _getScore(baseProgram)))
            // Some(higherScore.maxBy(_.score))
          }
          else if (higherRecall.nonEmpty) {
            def _getScore(sp: ScoredProgram): Double = sp.recall
            Some(SimulatedAnnealing.sample(higherRecall, _getScore, _getScore(baseProgram)))
          }
          else if (programPool.nonEmpty){
            def _getScore(sp: ScoredProgram): Double = sp.score
            Some(SimulatedAnnealing.sample(programPool, _getScore, _getScore(baseProgram)))
          }
          else {
            None
          }

          if (newValidRules.nonEmpty && !toExpandValidPrograms) {
            extraIters += 1
          }
      }

      evaluatedPrograms ++= refinedPrograms
      validPrograms ++= candidatePrograms.filter(isProgramValid)

      if (next.isDefined) {
        if (expandedPrograms.contains(next.get.program)) {
          assert(false)
        }
      }

      // if (refinedPrograms.size < allRefinedPrograms.size) programPool += baseProgram
      iters += 1
      if (iters % 20 == 0) logger.info(s"iteration $iters")
    }

    logger.info(s"Found ${validPrograms.size} programs after ${iters} iterations.")
    if (programPool.isEmpty) logger.debug(s"Runs out of candidate programs. Explored ${evaluatedPrograms.size}.")
    if (iters==maxIters) logger.debug(s"Stop after maximum iterations (${maxIters}).")
    // assert(validPrograms.nonEmpty)

    // logger.info(s"Found ${validPrograms.size} programs after ${iters} iterations.")
    // validPrograms.toList.map(_.program).sortWith(_<_)

    // val allValidPrograms = simplerAlternatives(validPrograms.map(_.program), idb)
    val allValidPrograms = simplerAlternatives(validPrograms.map(p => Program(p.program.rules++validRules)), idb)
    logger.info(s"Found ${allValidPrograms.size} programs after ${iters} iterations.")
    val programsSorted = allValidPrograms.toList.sortWith(_<_)
    programsSorted
  }

  def updateIdb(validRules: Set[Rule], idb: Set[Tuple]): Set[Tuple] = {
    val covered = evaluator.eval(Program(validRules))
    idb.diff(covered)
  }

  // def sampleNextCandidateProgram(candidates: Set[ScoredProgram], baseScore: Double): ScoredProgram = {
  //   // def _getScore(program: ScoredProgram): Double = program.score
  //   def _getScore(program: ScoredProgram): Double = program.recall * program.completeness
  //   SimulatedAnnealing.sample(candidates,_getScore,baseScore)
  // }

  def refineARule(program: Program, idb: Set[Tuple], validRules: Set[Rule]): Set[Program] = {

    def _refineARule(program: Program, rule:Rule): Set[Program] = {
      require(program.rules.contains(rule))
      val otherRules = program.rules.diff(Set(rule))
      val newRules = programBuilder.refineRule(rule)
        .map(_.normalize())
        .filter(syntaxConstraint.filter)
      val newPrograms = newRules.map(r => Program(otherRules+r))
      assert(newPrograms.forall(_.rules.size<=program.rules.size))

      def _isValid(p1: Program): Boolean = {
        val alternativeRules = otherRules.filter(_.head.relation == rule.head.relation)
        if (alternativeRules.isEmpty) {
          true
        }
        else {
          val p0 = evalProgram(Program(otherRules), validRules, idb)
          val baseRecall = p0.recall
          val p1Scored = evalProgram(p1, validRules,idb)
          val newRecall = p1Scored.recall
          newRecall > baseRecall
        }
      }

      val validRefinedPrograms = newPrograms.filter(_isValid)
      validRefinedPrograms
    }

    /** Only keep a candididate program it the refined rule produces unique output */
    if (program.isComplete) {
      /** If the program is complete, refine all non aggregate rules. */
      val nonAggregateRules = program.rules.filterNot(programBuilder.isAggregateRule)
      nonAggregateRules
        .diff(validRules)
        .flatMap(r => _refineARule(program,r))
    }
    else {
      /** If rule is incomplete, only refine the incomplete rule. */
      require(program.incompleteRules.size==1)
      val incompleteRule = program.incompleteRules.head
      _refineARule(program,incompleteRule)
    }
  }

  def refineProgram(scoredProgram: ScoredProgram, idb: Set[Tuple], validRules: Set[Rule]): Set[Program] = {
    val program = scoredProgram.program
    val hasAggregator = programBuilder.getAggregators.nonEmpty
    // val refinedPrograms: Set[Program] = programBuilder.refine(program)
    val refinedPrograms: Set[Program] = refineARule(program, idb, validRules)

    // if (isComplete(program) && scoredProgram.recall <= 1-1e-4 && !isAggProgram(program) && !isRecursive(program)) {
    if (isComplete(program) && scoredProgram.recall <= 1-1e-4 && !isAggProgram(program)) {
      if (hasAggregator || scoredProgram.precision >= 1.0) {
        logger.info(s"Add a rule. ${program.rules.size} rules.")
        val addedRule = addARule(program, validRules, idb)
        refinedPrograms ++ addedRule
      }
      else {
        refinedPrograms
      }
    }
    else if (isComplete(program) && scoredProgram.recall > 1-1e-4 && !isAggProgram(program) &&
        programBuilder.getAggregators.nonEmpty) {
      logger.info(s"Aggregate output")
      val aggregated = programBuilder.aggregateOutput(program)
      refinedPrograms ++ aggregated
    }
    else {
      refinedPrograms
    }
  }

  def isRecursive(program: Program): Boolean = program.rules.exists(_.isRecursive())

  def isAggProgram(program: Program): Boolean = {
    program.rules.exists(_.body.exists(_.isInstanceOf[AggregateLiteral]))
  }

  def addARule(program: Program, learnedRules: Set[Rule], idb: Set[Tuple]): Set[Program] = {
    val refRel = idb.map(_.relation)
    val generalPrograms = programBuilder.mostGeneralPrograms().filter(
      p => p.rules.forall(r => refRel.contains(r.head.relation))
    )
    require(generalPrograms.forall(_.rules.size==1))
    val addedRules = generalPrograms.map(gp=>Program(program.rules+gp.rules.head))

    val addedRulesEvaluated = addedRules.map(p => evalProgram(p, learnedRules, idb))
    val baseRecall = evalProgram(program, learnedRules, idb).recall
    val improvedRecalls = addedRulesEvaluated.filter(_.recall > baseRecall)
    improvedRecalls.map(_.program)
  }

  def needsRefinement(scoredProgram: ScoredProgram): Boolean = if (!scoredProgram.program.isComplete) {
      scoredProgram.recall > scoredProgram.completeRecall
    }
    else {
      scoredProgram.score > 1e-4
    }

  def isComplete(program: Program): Boolean = program.rules.forall(_.isHeadBounded())

  def evalProgram(program: Program, learnedRules: Set[Rule], refIdb: Set[Tuple]): ScoredProgram = {
    require(program.rules.nonEmpty)
    // val (idb0,partialRelationMap) = evaluator.evalAndPartialRelations(program)
    val covered = evaluator.eval(Program(learnedRules))
    val p1 = Program(program.rules++learnedRules)
    val (idb0,partialRelationMap) = evaluator.evalAndPartialRelations(p1)
    /** Filter only relevant idb */
    // val idb = getRelevantIdb(idb0)
    val idb = getRelevantIdb(idb0).diff(covered)

    def getPartialTuple(tuple: Tuple): Tuple = {
      val partialRelation: PartialRelation = partialRelationMap(tuple.relation)
      partialRelation.getPartialIdb(tuple)
    }
    ScoredProgram(program, idb, refIdb, getPartialTuple)
  }

  def getRelevantIdb(idb: Set[Tuple]): Set[Tuple] = {
    val aggRels = programBuilder.getAggregators.map(_.getAggHeadRel) ++
      programBuilder.getAggregators.map(_.relToAgg)
    idb.filterNot(t => aggRels.contains(t.relation))
  }

  def isProgramValid(program: ScoredProgram): Boolean = program.score >= 1-1e-3

  def dropOneRule(program: Program): Set[Program] = {
    def programWithout(p0: Program, r0: Rule): Option[Program] = {
      val otherRules = p0.rules.diff(Set(r0))
      val p1 = Program(otherRules)
      val idb0 = evaluator.eval(p0)
      val idb1 = evaluator.eval(p1)
      if (idb0 == idb1)  {
        Some(p1)
      }
      else {
        None
      }
    }

    val rulesWithAlternatives = getRulesWithAlternatives(program)
    if (rulesWithAlternatives.nonEmpty) {
      rulesWithAlternatives.flatMap(r=>programWithout(program,r))
    }
    else {
      Set()
    }
  }

  def getRulesWithAlternatives(program: Program) :Set[Rule] = {
    val relationWithAlternatives: Set[Relation] = program.rules.groupBy(_.head.relation).flatMap{
      case (rel, _ruleGroup) => {
        if (_ruleGroup.size>1) Some(rel)
        else None
      }
    }.toSet
    program.rules.filter(r => relationWithAlternatives.contains(r.head.relation))
  }

  def simplerAlternatives(programs: Set[Program], idb: Set[Tuple]): Set[Program] = {
    if (programs.exists(isAggProgram)) {
      programs ++ programs.flatMap(simplerAlternatives)
    }
    else {
      val allRules = programs.flatMap(_.rules)
      require(allRules.map(_.head.relation).size==1)
      val sorted = ruleHelper.combineRules(allRules, idb)
      sorted.take(25).flatMap(p=>(simplerAlternatives(p) + p)).toSet
    }
  }

  def simplerAlternatives(program: Program): Set[Program] = {
    val rulesWithAlternatives = getRulesWithAlternatives(program)
    var nDrop = 1
    var alternatives: Set[Program] = Set()
    var dropN: Set[Program] = Set(program)
    while (nDrop < rulesWithAlternatives.size && dropN.nonEmpty) {
      assert(dropN.forall(_.rules.size == program.rules.size - nDrop + 1))
      dropN = dropN.flatMap(dropOneRule)
      assert(dropN.forall(_.rules.size == program.rules.size - nDrop))
      alternatives ++= dropN
      nDrop += 1
    }
    alternatives
  }

  def getValidRules(scoredPrograms: Set[ScoredProgram]): Set[Rule] = {
    val perfectPrecision = scoredPrograms.filterNot(p=>isAggProgram(p.program))
      .filter(_.completeness >= 1.0)
      .filter(_.precision >= 1.0)
    perfectPrecision.flatMap(_.program.rules)
  }
}


