package synthesis.search

import synthesis.util.Misc
import synthesis.{Rule, Tuple}

case class ScoredRule(rule: Rule, idb: Set[Tuple],
                      precision: Double,
                      recall: Double,
                      completeness: Double
                     // score_history: List[Double]
                     ) {
  // extends Ordered[ScoredRule]{
  // override def compare(that: ScoredRule): Int = {
  //   if (this.getScore() < that.getScore()) -1
  //   else if (this.getScore() > that.getScore()) 1
  //   /*** Prioritize rules with more outputs */
  //   else if (this.idb.size > that.idb.size) 1
  //   else if (this.idb.size < that.idb.size) -1
  //   /*** Prioritize rules with fewer literals */
  //   else if (this.rule.body.size < that.rule.body.size) 1
  //   else if (this.rule.body.size > that.rule.body.size) -1
  //   /*** Prioritize rules with more free variables */
  //   else if (this.rule.freeVariables().size > that.rule.freeVariables().size) 1
  //   else if (this.rule.freeVariables().size < that.rule.freeVariables().size) -1
  //   /*** Prioritize rules with fewer constants */
  //   else if (this.rule.getConstantList.size < that.rule.getConstantList.size) 1
  //   else if (this.rule.getConstantList.size > that.rule.getConstantList.size) -1
  //   else 0
  // }

  val isPerfectPrecision: Boolean = precision >= 1.0
  val isPerfectRecall: Boolean = recall >= 1.0
  val isComplete: Boolean = completeness >= 1.0

  override def toString(): String = s"${this.rule.toString}"

}
object ScoredRule {
  val maxHistSize: Int = 3
  def apply(rule: Rule, refIdb: Set[Tuple], ruleEvaluator: Rule=>Set[Tuple], parentRule: Option[ScoredRule]=None): ScoredRule = {
    val idb = ruleEvaluator(rule)
    val (precision, recall) = _ioScore(refIdb, idb)
    val completeness = _completenessScore(rule)
    new ScoredRule(rule, idb, precision, recall, completeness)
  }

  def _ioScore(refIdb: Set[Tuple], idb: Set[Tuple]): (Double,Double) = {
    if (idb.nonEmpty) {
      val pos: Set[Tuple] = idb.intersect(refIdb)
      val neg: Set[Tuple] = idb.diff(refIdb)

      val npos = pos.size
      val nneg = neg.size
      require(npos + nneg == idb.size)

      val precision = npos.toDouble / idb.size.toDouble
      val recall = npos.toDouble / refIdb.size.toDouble
      (precision, recall)
    }
    else (0,0)
  }

  def _completenessScore(rule: Rule): Double = {
    /** The fraction of fields in the head being bound to body variables. */
    val m = rule.getUngroundHeadVariables().size
    val n = rule.getHeadVars().size
    assert(n>=m)
    1.0 * (n-m) / n
  }

}

