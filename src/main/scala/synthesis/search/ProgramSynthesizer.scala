package synthesis.search

import com.typesafe.scalalogging.Logger
import synthesis._
import synthesis.rulebuilder.AggregateLiteral
import synthesis.util.Misc

class ProgramSynthesizer(problem: Problem) extends Synthesis(problem) {
  /** Synthesize program on a program basis */

  private val programBuilder: ProgramBuilder = ProgramBuilder(problem)
  private val ruleLearner = SynthesisAllPrograms(problem)
  private val evaluator = new PartialProgramEvaluator(problem)
  private val logger = Logger(s"ProgramSynthesizer")

  logger.info(s"${programBuilder.getAggregators.size} aggreators.")

  private val maxBranching: Int = 50

  def getConfigSpace: SynthesisConfigSpace = ruleLearner.getConfigSpace

  def learnNPrograms(idb: Set[Tuple]): List[Program] = {
    var validPrograms: Set[ScoredProgram] = Set()
    // var evaluatedPrograms: Set[Program] = Set()
    var evaluatedPrograms: Set[Program] = Set()
    var expandedPrograms: Set[Program] = Set()

    val refRel: Set[Relation] = idb.map(_.relation)

    // var programPool: mutable.PriorityQueue[ScoredProgram] = new mutable.PriorityQueue()
    var programPool: Set[ScoredProgram] = Set()

    // init program Pool with the most general programs
    val mostGeneralPrograms = programBuilder.mostGeneralPrograms().filter(
      p => p.rules.forall(r => refRel.contains(r.head.relation))
    )
    programPool ++= mostGeneralPrograms.map(p => evalProgram(p, idb)).filter(needsRefinement)
    evaluatedPrograms ++= mostGeneralPrograms

    var extraIters: Int = 0
    val maxExtraIters: Int = 10
    val maxSolutionSize: Int = 10
    var iters: Int = 0
    // while (validPrograms.isEmpty) {
    assert(programPool.nonEmpty, s"$idb")
    var next: Option[ScoredProgram] = Some(sampleNextCandidateProgram(programPool, baseScore = 0))
    // while (programPool.nonEmpty && validPrograms.size < maxSolutionSize && extraIters < maxExtraIters) {
    while (next.isDefined && validPrograms.size < maxSolutionSize && extraIters < maxExtraIters) {
      // val baseProgram: ScoredProgram = programPool.dequeue()
      val baseProgram: ScoredProgram = next.get
      assert(evaluatedPrograms.contains(baseProgram.program))
      assert(!expandedPrograms.contains(baseProgram.program))

      val allRefinedPrograms: Set[Program] = refineProgram(baseProgram, idb).diff(evaluatedPrograms)
      val (refinedPrograms, isBaseProgramExhausted) = if (allRefinedPrograms.size > maxBranching) {
        (Misc.sampleSet(allRefinedPrograms, maxBranching), false)
      } else (allRefinedPrograms, true)
      if (isBaseProgramExhausted) {
        expandedPrograms += baseProgram.program
        programPool -= baseProgram
      }

      val candidatePrograms: Set[ScoredProgram] = refinedPrograms.
        map(p => evalProgram(p, idb))

      evaluatedPrograms ++= refinedPrograms
      validPrograms ++= candidatePrograms.filter(isProgramValid)

      val toRefine = candidatePrograms.filter(needsRefinement)
      // val staled = toRefine.filter(isStaled)
      // val pnext = toRefine.diff(staled)
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
        // sampleNextCandidateProgram(higherScore, baseProgram.score)
        def _getScore(sp: ScoredProgram): Double = sp.score
        Some(SimulatedAnnealing.sample(higherScore, _getScore, _getScore(baseProgram)))
      }
      else if (higherRecall.nonEmpty) {
          def _getScore(sp: ScoredProgram): Double = sp.recall
          Some(SimulatedAnnealing.sample(higherRecall, _getScore, _getScore(baseProgram)))
      }
      else {
        def _getScore(sp: ScoredProgram): Double = sp.score
        Some(SimulatedAnnealing.sample(programPool, _getScore, _getScore(baseProgram)))
      }

      // if (refinedPrograms.size < allRefinedPrograms.size) programPool += baseProgram
      iters += 1
      if (iters % 20 == 0) logger.info(s"iteration $iters")
      if (validPrograms.nonEmpty && !toExpandValidPrograms) {
        extraIters += 1
      }
    }

    if (programPool.isEmpty) logger.debug(s"Runs out of candidate programs. Explored ${evaluatedPrograms.size}.")
    logger.info(s"Found ${validPrograms.size} programs after ${iters} iterations.")
    assert(validPrograms.nonEmpty)
    validPrograms.toList.map(_.program).sortWith(_<_)
  }

  def sampleNextCandidateProgram(candidates: Set[ScoredProgram], baseScore: Double): ScoredProgram = {
    // def _getScore(program: ScoredProgram): Double = program.score
    def _getScore(program: ScoredProgram): Double = program.recall * program.completeness
    SimulatedAnnealing.sample(candidates,_getScore,baseScore)
  }

  def refineProgram(scoredProgram: ScoredProgram, idb: Set[Tuple]): Set[Program] = {
    val program = scoredProgram.program
    val refinedPrograms: Set[Program] = programBuilder.refine(program)

    // if (isComplete(program) && scoredProgram.recall <= 1-1e-4 && !isAggProgram(program) && !isRecursive(program)) {
    if (isComplete(program) && scoredProgram.recall <= 1-1e-4 && !isAggProgram(program)) {
      logger.info(s"Add a rule. ${program.rules.size} rules.")
      val addedRule = addARule(program, idb)
      refinedPrograms ++ addedRule
    }
    else if (isComplete(program) && scoredProgram.recall > 1-1e-4 && !isAggProgram(program)) {
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

  def addARule(program: Program, idb: Set[Tuple]): Set[Program] = {
    val refRel = idb.map(_.relation)
    val generalPrograms = programBuilder.mostGeneralPrograms().filter(
      p => p.rules.forall(r => refRel.contains(r.head.relation))
    )
    require(generalPrograms.forall(_.rules.size==1))
    val addedRules = generalPrograms.map(gp=>Program(program.rules+gp.rules.head))

    val addedRulesEvaluated = addedRules.map(p => evalProgram(p,idb))
    val baseRecall = evalProgram(program, idb).recall
    val improvedRecalls = addedRulesEvaluated.filter(_.recall > baseRecall)
    improvedRecalls.map(_.program)
  }

  // def addARule(program: Program, idb: Set[Tuple]): Set[Program] = {
  //   val generalRules: Set[Rule] = ruleLearner.getMostGenearlRules()
  //   val coveredIdb: Set[Tuple] = evaluator.eval(program)
  //   val remainingIdb = idb.diff(coveredIdb)
  //   /** Find a complete rule with non-zero recall. */
  //   def _getRuleScore(sr: ScoredRule): Double = sr.completeness * sr.recall * sr.precision
  //   def _validCondition(sr: ScoredRule): Boolean = sr.recall > 0 && sr.completeness >= 1.0
  //   /** Do not further refine valid rules */
  //   def _refineCondition(sr: ScoredRule): Boolean = _getRuleScore(sr) > 0 && !_validCondition(sr)
  //   val (_,newRules,_) = ruleLearner.learnNRules(remainingIdb, generalRules, learnedRules=program.rules,
  //     getScore = _getRuleScore, validCondition = _validCondition, refineCondition = _refineCondition)
  //   newRules.map(r => Program(program.rules+r))
  // }

  // def isStaled(scoredProgram: ScoredProgram): Boolean = {
  //   val isComplete: Boolean = scoredProgram.completeness > 1-1e-3
  //   if (scoredProgram.score_history.size == ScoredProgram.maxHistSize && isComplete) {
  //     scoredProgram.diff().map(d=>abs(d)).max < 1e-3
  //   }
  //   else false
  // }

  def needsRefinement(scoredProgram: ScoredProgram): Boolean = if (!scoredProgram.program.isComplete) {
      scoredProgram.recall > scoredProgram.completeRecall
    }
    else {
      scoredProgram.score > 1e-4
    }

  def isComplete(program: Program): Boolean = program.rules.forall(_.isHeadBounded())

  def evalProgram(program: Program, refIdb: Set[Tuple]): ScoredProgram = {
    val (idb0,partialRelationMap)  = evaluator.evalAndPartialRelations(program)
    /** Filter only relevant idb */
    val idb = getRelevantIdb(idb0)

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
}


