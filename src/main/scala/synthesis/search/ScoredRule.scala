package synthesis.search

import synthesis.{Rule, Tuple}

case class ScoredRule(rule: Rule, idb: Set[Tuple], score: Double) extends Ordered[ScoredRule]{
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

}
object ScoredRule {
  def apply(rule: Rule, refIdb: Set[Tuple], ruleEvaluator: Rule=>Set[Tuple]): ScoredRule = {
    val idb = ruleEvaluator(rule)
    new ScoredRule(rule, idb, scoreRule(rule, refIdb, idb))
  }

  def isValid(scoredRule: ScoredRule): Boolean = scoredRule.score >= 1-1e-3

  def isTooGeneral(scoredRule: ScoredRule): Boolean = scoredRule.score > 0 && !isValid(scoredRule)


  def scoreRule(rule: Rule, refIdb: Set[Tuple], idb: Set[Tuple]): Double = {
    _ioScore(rule, refIdb, idb) * _completenessScore(rule)
  }

  def _ioScore(rule: Rule, refIdb: Set[Tuple], idb: Set[Tuple]): Double = {
    if (idb.nonEmpty) {
      val pos: Set[Tuple] = idb.intersect(refIdb)
      val neg: Set[Tuple] = idb.diff(refIdb)

      val npos = pos.size
      val nneg = neg.size
      require(npos + nneg == idb.size)

      npos.toDouble / (npos + nneg).toDouble
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

