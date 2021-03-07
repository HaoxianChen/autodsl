package synthesis.search

import synthesis.{Misc, Rule, Tuple}

case class ScoredRule(rule: Rule, idb: Set[Tuple],
                     score: Double, score_history: List[Double]) extends Ordered[ScoredRule]{
  override def compare(that: ScoredRule): Int = {
    if (this.score < that.score) -1
    else if (this.score > that.score) 1
    /*** Prioritize rules with more outputs */
    else if (this.idb.size > that.idb.size) 1
    else if (this.idb.size < that.idb.size) -1
    /*** Prioritize rules with fewer literals */
    else if (this.rule.body.size < that.rule.body.size) 1
    else if (this.rule.body.size > that.rule.body.size) -1
    /*** Prioritize rules with more free variables */
    else if (this.rule.freeVariables().size > that.rule.freeVariables().size) 1
    else if (this.rule.freeVariables().size < that.rule.freeVariables().size) -1
    /*** Prioritize rules with fewer constants */
    else if (this.rule.getConstantList.size < that.rule.getConstantList.size) 1
    else if (this.rule.getConstantList.size > that.rule.getConstantList.size) -1
    else 0
  }

  override def toString(): String = s"${this.rule.toString} ${this.score}"

  def diff(): List[Double] = Misc.listDiff(this.score_history)
}
object ScoredRule {
  val maxHistSize: Int = 3
  def apply(rule: Rule, refIdb: Set[Tuple], ruleEvaluator: Rule=>Set[Tuple], parentRule: Option[ScoredRule]=None): ScoredRule = {
    val idb = ruleEvaluator(rule)
    val precision = _ioScore(refIdb, idb)
    val score = precision * _completenessScore(rule)
    val history: List[Double] = updateScoreHistory(parentRule, score)
    new ScoredRule(rule, idb, score, history)
  }

  def updateScoreHistory(parentRule: Option[ScoredRule], score: Double): List[Double] = {
    val history = if (parentRule.isDefined) {
      parentRule.get.score_history
    }
    else {List()}
    Misc.slidingWindowUpdate(history, score, maxHistSize)
  }

  def isValid(scoredRule: ScoredRule): Boolean = scoredRule.score >= 1-1e-3

  def isTooGeneral(scoredRule: ScoredRule): Boolean = scoredRule.score > 0 && !isValid(scoredRule)

  def _ioScore(refIdb: Set[Tuple], idb: Set[Tuple]): Double = {
    if (idb.nonEmpty) {
      val pos: Set[Tuple] = idb.intersect(refIdb)
      val neg: Set[Tuple] = idb.diff(refIdb)

      val npos = pos.size
      val nneg = neg.size
      require(npos + nneg == idb.size)

      val precision = npos.toDouble / idb.size.toDouble
     precision
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

