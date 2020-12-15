package synthesis

import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import rulebuilder.{ConstantBuilder, RecursionBuilder, SimpleRuleBuilder}


case class PartialRuleEvaluator(problem: Problem) {
  private val evaluator = Evaluator(problem)

  def getRefIdb(rule: Rule, idb: Set[Tuple]): Set[Tuple] = {
    val relevantIdb = idb.filter(_.relation == rule.head.relation)
    if (rule.isHeadBounded()){
      relevantIdb
    }
    else {
      val newRel = _getStrippedRelation(rule)
      val indices = _getBoundedIndices(rule)

      def getStripedTuple(tuple: Tuple): Tuple = {
        val newFields = indices map tuple.fields
        Tuple(newRel, newFields)
      }

      relevantIdb.map(getStripedTuple)
    }
  }

  def evalRule(rule: Rule, learnedRules: Set[Rule]): Set[Tuple] = {
    val newRule = if (rule.isHeadBounded()) rule else getStripedRule(rule)
    val oldIdb = evaluator.eval(Program(learnedRules))
    val newIdb = evaluator.eval(Program(learnedRules+newRule))
    newIdb.diff(oldIdb)
  }

  def evalRules(rules: Set[Rule], learnedRules: Set[Rule]): Set[Tuple] = {
    require(rules.forall(_.isHeadBounded()))
    val oldIdb = evaluator.eval(Program(learnedRules))
    val newIdb = evaluator.eval(Program(learnedRules++rules))
    newIdb.diff(oldIdb)
  }

  def _getBoundedIndices(rule: Rule): List[Int] = {
    val freeVars = rule.getUngroundHeadVariables()
    val indices = mutable.ListBuffer.empty[Int]
    for ((f,i) <- rule.head.fields.zipWithIndex) {
      f match {
        case v: Variable => if (!freeVars.contains(v)) indices.append(i)
        case _ => ()
      }
    }
    require(indices.nonEmpty, s"$rule")
    require(indices.max < rule.head.fields.size, s"indices: $indices,\n rule: $rule")
    indices.toList
  }

  def getStripedRule(unBoundRule: Rule): Rule = {
    val indices = _getBoundedIndices(unBoundRule)
    assert(indices.nonEmpty)
    val newFields = indices map unBoundRule.head.fields

    val newOutRel = _getStrippedRelation(unBoundRule)
    assert(newOutRel.signature.size == newFields.size)
    val newHead = Literal(newOutRel, newFields)
    Rule(newHead, unBoundRule.body)
  }

  def _getStrippedRelName(unBoundRule: Rule): String = s"${unBoundRule.head.relation.name}_"

  def _getStrippedRelation(unBoundRule: Rule): Relation = {
    val indices = _getBoundedIndices(unBoundRule)
    val newSig = indices map unBoundRule.head.relation.signature
    val newRelName = _getStrippedRelName(unBoundRule)
    Relation(newRelName, newSig)
  }
}

case class ScoredRule(rule: Rule, score: Double) extends Ordered[ScoredRule]{
  def isValid(): Boolean = score >= 1-1e-3

  def isTooGeneral(): Boolean = score > 0 && !isValid()

  override def compare(that: ScoredRule): Int = {
    if (this.score < that.score) -1
    else if (this.score > that.score) 1
    else if (this.rule.getConstantList.size < that.rule.getConstantList.size) 1
    else if (this.rule.getConstantList.size > that.rule.getConstantList.size) -1
    else 0
  }

}
object ScoredRule {
  def apply(rule: Rule, idb: Set[Tuple], ruleEvaluator: Rule=>Set[Tuple]): ScoredRule = {
    new ScoredRule(rule, scoreRule(rule, idb, ruleEvaluator))
  }

  def scoreRule(rule: Rule, refIdb: Set[Tuple], ruleEvaluator: Rule=>Set[Tuple]): Double = {
    _ioScore(rule, refIdb, ruleEvaluator) * _completenessScore(rule)
  }

  def _ioScore(rule: Rule, refIdb: Set[Tuple], ruleEvaluator: Rule=>Set[Tuple]): Double = {
    val idb = ruleEvaluator(rule)

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

abstract class Synthesis(problem: Problem) {
  def go(): Set[Program]
}

case class BasicSynthesis(problem: Problem,
                          maxIters: Int = 200,
                         maxRefineIters: Int = 200,
                         ) extends Synthesis(problem) {
  def go(): Set[Program] = Set(learnAProgram())

  private val logger = Logger("Synthesis")

  // private val ruleConstructor = new SimpleRuleBuilder(problem.inputRels, problem.outputRels)
  // private val ruleConstructor = new RecursionBuilder(problem.inputRels, problem.outputRels)
  private val ruleConstructor = ConstantBuilder(problem.inputRels, problem.outputRels, problem.edb, problem.idb)
  private val evaluator = PartialRuleEvaluator(problem)


  def learnAProgram(): Program = {
    var examples: Set[Tuple] = problem.idb.toTuples()
    var rules: Set[Rule] = Set()
    var iters: Int = 0

    // Init the rule pool with the most general rules
    var generalRules: Set[Rule] = ruleConstructor.mostGeneralRules()

    while (examples.nonEmpty && iters < maxIters ) {
      val (coveredExamples, newRule, remainingRules) = learnARule(examples, generalRules, rules)
      val nextExamples = examples -- coveredExamples
      require(nextExamples.size < examples.size)
      examples = nextExamples
      rules = rules + newRule
      generalRules = remainingRules
      iters += 1
    }
    Program(rules)
  }

  def learnARule(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule]): (Set[Tuple], Rule, Set[Rule]) = {
    var iters: Int = 0

    // score the rules based on current idb set
    val generalRules = {
      val curOutRels = idb.map(_.relation)
      val relevantRules = generalSimpleRules.filter(r => curOutRels.contains(r.head.relation))
      relevantRules.map(r => scoreRule(r, idb, learnedRules))
    }

    // Set up the pool of rules to be refine
    var rulePool: mutable.PriorityQueue[ScoredRule] = new mutable.PriorityQueue()
    rulePool ++= generalRules.filter(r => r.isValid() || r.isTooGeneral())

    var validRules: Set[ScoredRule] = generalRules.filter(_.isValid())

    while (iters < maxRefineIters && validRules.isEmpty) {

      // pop highest scored rule from pool
      val baseRule: Rule = rulePool.dequeue().rule

      // refine the rules
      val refinedRules = ruleConstructor.refineRule(baseRule)
      val candidateRules: Set[ScoredRule] = refinedRules.map(r => scoreRule(r, idb, learnedRules))

      // keep the valid ones
      validRules ++= candidateRules.filter(_.isValid())

      // Put the too general ones into the pool
      val tooGeneral = candidateRules.filter(_.isTooGeneral())
      rulePool ++= tooGeneral

      iters += 1
    }
    require(validRules.nonEmpty, s"Synthesis failed: empty valid rules.")
    val bestRule = validRules.maxBy(_.score).rule
    val remainingRules: Set[Rule] = {
      val rs = rulePool.toSet
      rs.map(_.rule)
    }
    logger.info(s"Found a rule after $iters iterations.\n$bestRule")
    val newIdb = evaluator.evalRule(bestRule, learnedRules)
    assert(newIdb.nonEmpty)
    (newIdb, bestRule, remainingRules)
  }

  def scoreRule(rule: Rule, allIdb: Set[Tuple], learnedRules: Set[Rule]): ScoredRule = {
    val refIdb = evaluator.getRefIdb(rule, allIdb)
    def f_eval: Rule => Set[Tuple] = r => evaluator.evalRule(r, learnedRules)
    ScoredRule(rule, refIdb, f_eval)
  }

}

case class SynthesisConfigs(recursion: Boolean, maxConstants: Int)
object SynthesisConfigs {
  def getConfig(problem: Problem): SynthesisConfigs = {
    problem.domain match {
      case "SDN" => SynthesisConfigs(recursion = false, maxConstants = 2)
      case "NIB" => SynthesisConfigs(recursion = true, maxConstants = 0)
      case "routing" => SynthesisConfigs(recursion = true, maxConstants = 0)
      case "consensus" => SynthesisConfigs(recursion = false, maxConstants = 0)
      // case _ => SynthesisConfigs(recursion = true, maxConstants = 2)
      case _ => ???
    }
  }
}

case class SynthesisAllPrograms(problem: Problem,
                              recursion: Boolean = true,
                              maxIters: Int = 20,
                              maxRefineIters: Int = 100,
                             ) extends Synthesis(problem) {

  def go(): Set[Program] = learnNPrograms()

  private val logger = Logger("Synthesis")

  private val ruleConstructor = ConstantBuilder(problem.inputRels, problem.outputRels, problem.edb, problem.idb)
  private val evaluator = PartialRuleEvaluator(problem)
  private val config: SynthesisConfigs = SynthesisConfigs.getConfig(problem)

  def learnNPrograms(): Set[Program] = {
    /** Strip the problem into learning different output */
    val examples: Set[Tuple] = problem.idb.toTuples()
    val exampleGroup: Map[Relation, Set[Tuple]] = examples.groupBy(_.relation)


    /** cross join the programs learnt for each relation */
    val programsByOutRels: List[Set[Program]] = exampleGroup.map {
      case (_, idb) => _learnNPrograms(idb)
    }.toList

    Misc.crossJoin(programsByOutRels).map {
      ps => ps.fold(Program(Set()))((p1,p2)=> Program(p1.rules++p2.rules))
    }.toSet
  }

  def _learnNPrograms(idb: Set[Tuple]): Set[Program] = {
    /** Learn all possible rules, then combine them. */
    var examples: Set[Tuple] = idb
    var rules: Set[Rule] = Set()
    var iters: Int = 0

    // Init the rule pool with the most general rules
    var generalRules: Set[Rule] = ruleConstructor.mostGeneralRules()

    while (examples.nonEmpty && iters < maxIters ) {
      val (coveredExamples, newRules, remainingRules) = learnNRules(examples, generalRules, rules)
      val nextExamples = examples -- coveredExamples
      require(nextExamples.size < examples.size)
      examples = nextExamples
      rules = rules ++ newRules
      generalRules = remainingRules
      iters += 1
    }
    assert(examples.isEmpty)
    combineRules(rules, idb)
  }

  def combineRules(rules: Set[Rule], idb: Set[Tuple]): Set[Program] = {
    /** Return all combinations of rules that cover all idb
     * how to handle recursion? */

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
              val coveredIdb = evaluator.evalRule(head, learnedRules.toSet)
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

            // not keeping this rule
            ruleSet1 ++ _combineRules(learnedRules, tail, remainingIdb)
          }
        }
      }
    }

    // sort: non-recursive first, recursion later
    val ruleList: List[Rule] = {
      val recursiveRules = rules.filter(_.isRecursive())
      val nonRecursiveRules = rules.diff(recursiveRules)
      recursiveRules.toList ::: nonRecursiveRules.toList
    }
    _combineRules(List(),ruleList, idb).map(
      rs=>Program(rs.toSet)
    )
  }

  def _getRelevantScoredRules(idb: Set[Tuple], rules: Set[Rule], learnedRules: Set[Rule]): Set[ScoredRule] = {
    val curOutRels = idb.map(_.relation)
    val relevantRules = rules.filter(r => curOutRels.contains(r.head.relation))
    relevantRules.map(r => scoreRule(r, idb, learnedRules))
  }

  def learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule]): (Set[Tuple], Set[Rule], Set[Rule]) = {

    /** Lookup the configuration for the synthesizer*/
    val relevantOutRel: Set[Relation] = idb.map(_.relation)
    require(relevantOutRel.subsetOf(problem.outputRels))
    val ruleBuilder = ConstantBuilder(problem.inputRels, relevantOutRel, problem.edb, problem.idb, config.recursion,
      config.maxConstants)
    _learnNRules(idb, generalSimpleRules, learnedRules, ruleBuilder.refineRule, maxExtraIters = 100)
  }

  def _learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule],
                   refineRule: Rule => Set[Rule],
                   maxExtraIters: Int = 1): (Set[Tuple], Set[Rule], Set[Rule]) = {
    var iters: Int = 0
    var extraIters: Int = 0

    // score the rules based on current idb set
    val generalRules = _getRelevantScoredRules(idb, generalSimpleRules, learnedRules)

    // var forbiddenRules: Set[ScoredRule] = Set()
    var exploredRules: Set[Rule] = Set()

    // Set up the pool of rules to be refine
    var rulePool: mutable.PriorityQueue[ScoredRule] = new mutable.PriorityQueue()
    rulePool ++= generalRules.filter(r => r.isValid() || r.isTooGeneral())

    var validRules: Set[ScoredRule] = generalRules.filter(_.isValid())

    while (iters < maxRefineIters && extraIters < maxExtraIters && rulePool.nonEmpty) {

      // pop highest scored rule from pool
      val baseRule = rulePool.dequeue()

      // refine the rules
      val refinedRules = refineRule(baseRule.rule).diff(exploredRules)

      val candidateRules: Set[ScoredRule] = refinedRules.map(r => scoreRule(r, idb, learnedRules))

      // Remember the ones that have been explored
      exploredRules ++= refinedRules

      // keep the valid ones
      validRules ++= candidateRules.filter(_.isValid())

      // Put the too general ones into the pool, and forbid anything else from exploring again
      val tooGeneral = candidateRules.filter(_.isTooGeneral())
      rulePool ++= tooGeneral

      iters += 1
      if (validRules.nonEmpty) extraIters += 1
    }
    if (rulePool.isEmpty) logger.info(s"Exhausted all rules")
    if (iters == maxRefineIters) logger.info(s"Hit maximum iterations ${maxExtraIters}")
    if (extraIters == maxExtraIters) logger.info(s"Hit maximum extra iterations ${maxExtraIters}")
    // require(validRules.nonEmpty, s"Synthesis failed: empty valid rules.")
    logger.info(s"Found ${validRules.size} rules after $iters iterations.\n")

    val remainingRules: Set[Rule] = {
      val rs = rulePool.toSet
      rs.map(_.rule)
    }
    val newLearnedRules: Set[Rule] = validRules.map(_.rule)
    val newIdb = if(newLearnedRules.nonEmpty) {
      evaluator.evalRules(newLearnedRules, learnedRules)
    }
    else {Set[Tuple]()}
    // assert(newIdb.nonEmpty)
    (newIdb, newLearnedRules, remainingRules)
  }

  def scoreRule(rule: Rule, allIdb: Set[Tuple], learnedRules: Set[Rule]): ScoredRule = {
    val refIdb = evaluator.getRefIdb(rule, allIdb)
    def f_eval: Rule => Set[Tuple] = r => evaluator.evalRule(r, learnedRules)
    ScoredRule(rule, refIdb, f_eval)
  }

}
case class SynthesisNPrograms(problem: Problem,
                             recursion: Boolean = true,
                           maxIters: Int = 20,
                           maxRefineIters: Int = 25,
                          ) extends Synthesis(problem) {

  def go(): Set[Program] = Set(learnAProgram())

  private val logger = Logger("Synthesis")

  private val ruleConstructor = ConstantBuilder(problem.inputRels, problem.outputRels, problem.edb, problem.idb)
  private val evaluator = PartialRuleEvaluator(problem)
  private val config: SynthesisConfigs = SynthesisConfigs.getConfig(problem)


  def learnAProgram(): Program = {
    var examples: Set[Tuple] = problem.idb.toTuples()
    var rules: Set[Rule] = Set()
    var iters: Int = 0

    // Init the rule pool with the most general rules
    var generalRules: Set[Rule] = ruleConstructor.mostGeneralRules()

    while (examples.nonEmpty && iters < maxIters ) {
      val (coveredExamples, newRules, remainingRules) = learnNRules(examples, generalRules, rules)
      val nextExamples = examples -- coveredExamples
      require(nextExamples.size < examples.size)
      examples = nextExamples
      rules = rules ++ newRules
      generalRules = remainingRules
      iters += 1
    }
    Program(rules)
  }

  def _getRelevantScoredRules(idb: Set[Tuple], rules: Set[Rule], learnedRules: Set[Rule]): Set[ScoredRule] = {
    val curOutRels = idb.map(_.relation)
    val relevantRules = rules.filter(r => curOutRels.contains(r.head.relation))
    relevantRules.map(r => scoreRule(r, idb, learnedRules))
  }

  def learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule]): (Set[Tuple], Set[Rule], Set[Rule]) = {

    /** Lookup the configuration for the synthesizer*/
    val relevantOutRel: Set[Relation] = idb.map(_.relation)
    require(relevantOutRel.subsetOf(problem.outputRels))
    val ruleBuilder = ConstantBuilder(problem.inputRels, relevantOutRel, problem.edb, problem.idb, config.recursion,
      config.maxConstants)
    _learnNRules(idb, generalSimpleRules, learnedRules, ruleBuilder.refineRule)

    // /** Iteratively increase the number of constants */
    // var newRules: Set[Rule] = Set()
    // var coveredExamples: Set[Tuple] = Set()
    // var remainingRules: Set[Rule] = Set()

    // var nConstants = 0
    // while (newRules.isEmpty && nConstants <= config.maxConstants) {
    //   logger.info(s"searching rules with at most $nConstants constants.")
    //   val ruleBuilder = ConstantBuilder(problem.inputRels, problem.outputRels, problem.edb, problem.idb,
    //     config.recursion, nConstants)
    //   val (_coveredExamples, _newRules, _remainingRules) = _learnNRules(idb, generalSimpleRules, learnedRules, ruleBuilder.refineRule)

    //   newRules = _newRules
    //   coveredExamples = _coveredExamples
    //   remainingRules = _remainingRules

    //   nConstants += 1
    // }
    // (coveredExamples, newRules, remainingRules)
  }

  def _learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule],
                   refineRule: Rule => Set[Rule],
                   maxExtraIters: Int = 1): (Set[Tuple], Set[Rule], Set[Rule]) = {
    var iters: Int = 0
    var extraIters: Int = 0

    // score the rules based on current idb set
    val generalRules = _getRelevantScoredRules(idb, generalSimpleRules, learnedRules)

    var forbiddenRules: Set[ScoredRule] = Set()

    // Set up the pool of rules to be refine
    var rulePool: mutable.PriorityQueue[ScoredRule] = new mutable.PriorityQueue()
    rulePool ++= generalRules.filter(r => r.isValid() || r.isTooGeneral())

    var validRules: Set[ScoredRule] = generalRules.filter(_.isValid())

    while (iters < maxRefineIters && extraIters < maxExtraIters && rulePool.nonEmpty) {

      // pop highest scored rule from pool
      val baseRule = rulePool.dequeue()

      // refine the rules
      val refinedRules = refineRule(baseRule.rule)
      val candidateRules: Set[ScoredRule] = refinedRules.map(r => scoreRule(r, idb, learnedRules))

      // keep the valid ones
      validRules ++= candidateRules.filter(_.isValid())

      // Put the too general ones into the pool, and forbid anything else from exploring again
      val tooGeneral = candidateRules.filter(_.isTooGeneral())
      forbiddenRules ++= candidateRules.diff(validRules++tooGeneral)
      rulePool ++= tooGeneral.diff(forbiddenRules)

      iters += 1
      if (validRules.nonEmpty) extraIters += 1
    }
    // require(validRules.nonEmpty, s"Synthesis failed: empty valid rules.")
    logger.info(s"Found ${validRules.size} rules after $iters iterations.\n")

    val remainingRules: Set[Rule] = {
      val rs = rulePool.toSet
      rs.map(_.rule)
    }
    val newLearnedRules: Set[Rule] = validRules.map(_.rule)
    val newIdb = if(newLearnedRules.nonEmpty) {
      evaluator.evalRules(newLearnedRules, learnedRules)
    }
    else {Set[Tuple]()}
    // assert(newIdb.nonEmpty)
    (newIdb, newLearnedRules, remainingRules)
  }

  def scoreRule(rule: Rule, allIdb: Set[Tuple], learnedRules: Set[Rule]): ScoredRule = {
    val refIdb = evaluator.getRefIdb(rule, allIdb)
    def f_eval: Rule => Set[Tuple] = r => evaluator.evalRule(r, learnedRules)
    ScoredRule(rule, refIdb, f_eval)
  }

}