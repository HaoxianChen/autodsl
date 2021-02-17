package synthesis.search

import com.typesafe.scalalogging.Logger
import synthesis._

import scala.collection.mutable

class ProgramSynthesizer(problem: Problem) extends Synthesis(problem) {
  /** Synthesize program on a program basis */

  private val programBuilder: ProgramBuilder = ProgramBuilder(problem)
  private val ruleLearner = SynthesisAllPrograms(problem)
  private val evaluator = new PartialProgramEvaluator(problem)
  private val logger = Logger(s"ProgramSynthesizer")

  logger.info(s"${programBuilder.getAggregators.size} aggreators.")

  def learnNPrograms(idb: Set[Tuple]): List[Program] = {
    var validPrograms: Set[ScoredProgram] = Set()
    var exploredPrograms: Set[Program] = Set()
    var programPool: mutable.PriorityQueue[ScoredProgram] = new mutable.PriorityQueue()

    // init program Pool with the most general programs
    val mostGeneralPrograms = programBuilder.mostGeneralPrograms()
    programPool ++= mostGeneralPrograms.map(p => evalProgram(p, idb))

    while (validPrograms.isEmpty) {
      val baseProgram: ScoredProgram = programPool.dequeue()

      val refinedPrograms: Set[Program] = refineProgram(baseProgram, idb)

      val candidatePrograms: Set[ScoredProgram] = refinedPrograms.diff(exploredPrograms).map(p => evalProgram(p, idb))

      validPrograms ++= candidatePrograms.filter(isProgramValid)
      exploredPrograms ++= refinedPrograms

      val pnext = candidatePrograms.filter(needsRefinement)
      programPool ++= pnext
    }
    /** todo: order the valid programs */
    validPrograms.toList.map(_.program)
  }

  def refineProgram(scoredProgram: ScoredProgram, idb: Set[Tuple]): Set[Program] = {
    val program = scoredProgram.program
    val refinedPrograms: Set[Program] = programBuilder.refine(program)

    if (isComplete(program) && scoredProgram.recall <= 1-1e-4) {
      logger.info(s"Add a rule")
      val addedRule = addARule(program, idb)
      refinedPrograms ++ addedRule
    }
    else if (isComplete(program) && scoredProgram.recall > 1-1e-4) {
      logger.info(s"Aggregate output")
      val aggregated = programBuilder.aggregateOutput(program)
      refinedPrograms ++ aggregated
    }
    else {
      refinedPrograms
    }
  }

  def addARule(program: Program, idb: Set[Tuple]): Set[Program] = {
    val generalRules: Set[Rule] = ruleLearner.getMostGenearlRules()
    val coveredIdb: Set[Tuple] = evaluator.eval(program)
    val remainingIdb = idb.diff(coveredIdb)
    def validCondition(scoredRule: ScoredRule): Boolean = {
      val isComplete = scoredRule.rule.isHeadBounded()
      val isGeneral = scoredRule.score > 0
      isComplete && isGeneral
    }
    val (_,newRules,_) = ruleLearner.learnNRules(remainingIdb, generalRules, learnedRules=program.rules, validCondition=validCondition)
    newRules.map(r => Program(program.rules+r))
  }

  def needsRefinement(scoredProgram: ScoredProgram): Boolean = {
    scoredProgram.score > 1e-4
  }

  def isComplete(program: Program): Boolean = program.rules.forall(_.isHeadBounded())

  def evalProgram(program: Program, refIdb: Set[Tuple]): ScoredProgram = {
    val (idb0,partialRelationMap)  = evaluator.evalAndPartialRelations(program)
    /** Filter only relevant idb */
    val idb = getRelevantIdb(idb0)

    def getPartialIdb(rel: Relation, tuple: Tuple): Tuple = {
      val partialRelation: PartialRelation = partialRelationMap(rel)
      partialRelation.getPartialIdb(tuple)
    }
    ScoredProgram(program, idb, refIdb, getPartialIdb)
  }

  def getRelevantIdb(idb: Set[Tuple]): Set[Tuple] = {
    val aggRels = programBuilder.getAggregators.map(_._getAggHeadRel) ++
      programBuilder.getAggregators.map(_.relToAgg)
    idb.filterNot(t => aggRels.contains(t.relation))
  }

  def isProgramValid(program: ScoredProgram): Boolean = program.score >= 1-1e-3
}

case class ScoredProgram(program: Program, idb: Set[Tuple], precision: Double, recall: Double, completeness: Double) extends Ordered[ScoredProgram] {
  val score = precision * recall * completeness
  override def compare(that: ScoredProgram): Int = {
    if (this.score < that.score) -1
    else if (this.score > that.score) 1

    else if (this.completeness < that.completeness) -1
    else if (this.completeness > that.completeness) 1

    else if (this.recall < that.recall) -1
    else if (this.recall > that.recall) 1

    else 0
  }

  override def toString(): String = s"${this.score} ${this.program.toString}"

}
object ScoredProgram {
  def apply(program: Program, idb: Set[Tuple], refIdb: Set[Tuple], getPartialIdb: (Relation, Tuple)=>Tuple): ScoredProgram = {
    val (precision, recall) = getScore(idb, refIdb, getPartialIdb)
    ScoredProgram(program, idb, precision, recall, completeness(program))
  }

  def completeness(program: Program): Double = {
    val ruleCompleteness = program.rules.map(ScoredRule._completenessScore)
    ruleCompleteness.min
  }

  def getPrecision(idb: Set[Tuple], refIdb: Set[Tuple]): Double = {
    val pos = idb.intersect(refIdb)
    val precision: Double = 1.0 * pos.size / idb.size
    precision
  }

  def getScore(idb: Set[Tuple], refIdb: Set[Tuple], getPartialIdb: (Relation, Tuple) => Tuple): (Double, Double) = {
    /** Return precision and recall */
    val idbByRel = idb.groupBy(_.relation)
    assert(idbByRel.keySet.size <= 2, idbByRel.keySet)

    var precision: Double = 1.0

    val refIdbList = refIdb.toList
    var isCovered: List[Boolean] = List.fill(refIdbList.size)(false)

    for ((rel, tuples) <- idbByRel) {
      val refPartialIdb: List[Tuple] = refIdbList.map(t => getPartialIdb(rel, t))
      val _c = refPartialIdb.map(t => tuples.contains(t))
      isCovered = isCovered.zip(_c).map {case (x,y) => x || y}

      val p = getPrecision(tuples, refPartialIdb.toSet)
      precision *= p
    }

    val nCovered = isCovered.count(identity)
    val recall: Double = 1.0 * nCovered / isCovered.size
    (precision, recall)
  }
}

class PartialProgramEvaluator(problem: Problem) {
  private val evaluator = Evaluator(problem)
  private val partialRuleEvaluator = PartialRuleEvaluator(problem)

  def getStrippedProgram(program: Program): (Program, Program) = {
    val incompleteRules: Set[Rule] = program.rules.filter(r => !r.isHeadBounded())
    val completeRules = program.rules.diff(incompleteRules)
    val strippedRules = incompleteRules.map(r => partialRuleEvaluator.getStripedRule(r))
    (Program(completeRules), Program(completeRules++strippedRules))
  }

  def eval(p0: Program): Set[Tuple] = {
    val (_, p_stripped) = getStrippedProgram(p0)
    evaluator.eval(p_stripped)
  }

  def getPartialRelation(rule: Rule): PartialRelation = {
    val newRel = if (rule.isHeadBounded()) rule.head.relation else partialRuleEvaluator._getStrippedRelation(rule)
    val indices = partialRuleEvaluator._getBoundedIndices(rule)
    PartialRelation(newRel, rule.head.relation, indices)
  }

  def evalAndPartialRelations(p0: Program): (Set[Tuple], Map[Relation, PartialRelation]) = {
    val idb = eval(p0)
    val partialRelations = p0.rules.map(getPartialRelation)
    val partialRelationMap: Map[Relation, PartialRelation] = partialRelations.map(pr => pr.relation -> pr).toMap
    require(idb.map(_.relation).subsetOf(partialRelationMap.keySet))
    (idb, partialRelationMap)
  }
}

case class PartialRelation(relation: Relation, refRel: Relation, indices: List[Int]) {
  require(indices.forall(i => i>=0 && i < refRel.signature.size))
  require(indices.toSet.size == indices.size)
  require(indices.size == relation.signature.size)

  def getPartialIdb(tuple: Tuple): Tuple = {
    require(tuple.relation == this.refRel)
    val newFields = this.indices.map (i => tuple.fields(i))
    Tuple(relation, newFields)
  }

}

