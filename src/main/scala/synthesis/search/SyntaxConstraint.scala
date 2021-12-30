package synthesis.search

import synthesis.{Literal, Rule}

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
