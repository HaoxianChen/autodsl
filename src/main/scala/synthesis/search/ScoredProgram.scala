package synthesis.search

import synthesis.{Evaluator, Problem, Program, Relation, Rule, Tuple}

case class ScoredProgram(program: Program, idb: Set[Tuple], precision: Double, recall: Double, completeness: Double,
                         completeRecall: Double
                        ) extends Ordered[ScoredProgram] {
  // val score = precision * recall * completeness
  val score = precision * recall
  override def compare(that: ScoredProgram): Int = {
    if (this.completeness < that.completeness) -1
    else if (this.completeness > that.completeness) 1

    else if (this.recall < that.recall) -1
    else if (this.recall > that.recall) 1

    else if (this.score < that.score) -1
    else if (this.score > that.score) 1

    else if (this.program > that.program) -1
    else if (this.program < that.program) 1

    else 0
  }

  override def toString(): String = s"${this.score} ${this.program.toString}"
}
object ScoredProgram {
  val maxHistSize: Int = 2
  def apply(program: Program, idb: Set[Tuple], refIdb: Set[Tuple],
            // getPartialIdb: (Relation, Tuple)=>Tuple
            getPartialIdb: Tuple =>Tuple
           ): ScoredProgram = {
    // val (precision, recall) = getScore(idb, refIdb, getPartialIdb)
    val (precision, recall, completeRecall) = ioScore(program, idb, refIdb, getPartialIdb)

    ScoredProgram(program, idb, precision, recall, completeness(program), completeRecall)
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

  def ioScore(program: Program, idb: Set[Tuple], refIdb: Set[Tuple],
              //getPartialIdb: (Relation, Tuple) => Tuple
              getPartialTuple: Tuple => Tuple
             ):
  (Double, Double, Double) = {
    if (idb.isEmpty) {
      (0,0,0)
    }
    else {
      val covered = idb.intersect(refIdb)
      val (nCovered, precision): (Double,Double) = if (!program.isComplete) {
        require(program.incompleteRules.size==1)
        val completeness: Double = ScoredRule._completenessScore(program.incompleteRules.head)

        val _nCovered = {
          val covered = refIdb.intersect(idb)
          val remaining = refIdb.diff(idb)
          val partialRefIdbs = remaining.map(getPartialTuple)
          val coveredPartially = partialRefIdbs.intersect(idb)
          covered.size + coveredPartially.size * completeness
        }
        val precision = {
          val completeRelations = refIdb.map(_.relation)
          val completeTuples = idb.filter(t=>completeRelations.contains(t.relation))

          val partialTuples = idb.diff(completeTuples)
          val _partialRefIdb = refIdb.map(getPartialTuple)
          val partialCovered = idb.intersect(_partialRefIdb)

          // completeTuples.size + partialTuples.diff(_partialIdb).size * completeness
          val _allCoveredSize: Double = covered.size + partialCovered.size * completeness
          val _allTupleSize: Double = completeTuples.size + partialTuples.size * completeness
          if (_allTupleSize < _allCoveredSize) {
            assert(false)
          }
          _allCoveredSize / _allTupleSize
        }
        (_nCovered, precision)
      }
      else {
        (covered.size, covered.size.toDouble / idb.size)
      }

      val recall: Double = nCovered / refIdb.size
      val completeRecall: Double = covered.size.toDouble / refIdb.size
      assert(recall <= 1)
      assert(precision <= 1, s"idb:$idb\nrefIdb:$refIdb")
      (precision, recall, completeRecall)
    }
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
    /** This map is from the original relation object to the PartialRelation Object */
    val partialRelationMap: Map[Relation, PartialRelation] = partialRelations.map(pr => pr.refRel -> pr).toMap
    // require(idb.map(_.relation).subsetOf(partialRelationMap.keySet))
    (idb, partialRelationMap)
  }
}

case class PartialRelation(relation: Relation, refRel: Relation, indices: List[Int]) {
  require(indices.forall(i => i>=0 && i < refRel.signature.size))
  require(indices.toSet.size == indices.size)
  require(indices.size == relation.signature.size)

  def getPartialIdb(tuple: Tuple): Tuple = {
    require(tuple.relation == this.refRel, s"${tuple.relation} ${this.refRel}")
    val newFields = this.indices.map (i => tuple.fields(i))
    Tuple(relation, newFields)
  }

}
