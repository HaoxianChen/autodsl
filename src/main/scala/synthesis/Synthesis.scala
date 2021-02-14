package synthesis

import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import rulebuilder.{ConstantBuilder, FunctorBuilder, RecursionBuilder, RuleBuilder, SimpleRuleBuilder}


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
        case _: Constant => indices.append(i)
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

case class ScoredRule(rule: Rule, idb: Set[Tuple], score: Double) extends Ordered[ScoredRule]{
  def isValid(): Boolean = score >= 1-1e-3

  def isTooGeneral(): Boolean = score > 0 && !isValid()

  override def compare(that: ScoredRule): Int = {
    if (this.score < that.score) -1
    else if (this.score > that.score) 1
    /*** Prioritize rules with more outputs */
    else if (this.idb.size > that.idb.size) 1
    else if (this.idb.size < that.idb.size) -1
    /*** Prioritize rules with more free variables */
    else if (this.rule.freeVariables().size > that.rule.freeVariables().size) 1
    else if (this.rule.freeVariables().size < that.rule.freeVariables().size) -1
    /*** Prioritize rules with fewer literals */
    else if (this.rule.body.size < that.rule.body.size) 1
    else if (this.rule.body.size > that.rule.body.size) -1
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

abstract class Synthesis(problem: Problem) {
  def learnNPrograms(idb: Set[Tuple]): List[Program]
  def go(): Map[Relation, List[Program]] = {
    /** Strip the problem into learning different output */
    val examples: Set[Tuple] = problem.idb.toTuples()
    val exampleGroup: Map[Relation, Set[Tuple]] = examples.groupBy(_.relation)

    val programsByOutRels: Map[Relation, List[Program]] = exampleGroup.map {
      case (rel, idb) => rel -> learnNPrograms(idb)
    }
    programsByOutRels
  }
}

case class BasicSynthesis(problem: Problem,
                          maxIters: Int = 200,
                         maxRefineIters: Int = 200,
                         ) extends Synthesis(problem) {
  def learnNPrograms(idb: Set[Tuple]) = List(learnAProgram(idb))

  private val logger = Logger("Synthesis")

  private val ruleConstructor = ConstantBuilder(problem.inputRels, problem.outputRels, problem.edb, problem.idb)
  private val evaluator = PartialRuleEvaluator(problem)


  def learnAProgram(idb: Set[Tuple]): Program = {
    var examples: Set[Tuple] = idb
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

case class SynthesisConfigSpace(allConfigs: List[SynthesisConfigs]) {
  private var current_config_id: Int = 0

  def get_config(): SynthesisConfigs = allConfigs(current_config_id)

  def next_config(): SynthesisConfigs = {
    current_config_id += 1
    assert(current_config_id < allConfigs.size, s"Run out of config space")
    get_config()
  }

  def isEmpty: Boolean = allConfigs.isEmpty
}
object SynthesisConfigSpace {
  def emptySpace(): SynthesisConfigSpace = SynthesisConfigSpace(List())
  def getConfigSpace(problem: Problem): SynthesisConfigSpace = {
    problem.domain match {
      case "SDN" => _getConfigSpace(recursion = false, maxConstants = List(0,5))
      case "NIB" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          PrependList.allInstances, MakeList.allInstances
        )
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(recursion = true, functors = functors)
      }
      case "routing" => _getConfigSpace(recursion = true, maxConstants = List(0))
      case "consensus" => _getConfigSpace(recursion = false, maxConstants = List(0))
      case _ => ???
    }
  }
  def _getConfigSpace(recursion: Boolean, maxConstants: List[Int]): SynthesisConfigSpace = {
    val allConfigs: List[SynthesisConfigs] = maxConstants.map (c => SynthesisConfigs(recursion, maxConstants = c))
    SynthesisConfigSpace(allConfigs)
  }
  def _getConfigSpace(recursion: Boolean, functors: Set[AbstractFunctorSpec]): SynthesisConfigSpace = {
    val synthesisConfigs = SynthesisConfigs(recursion, maxConstants = 0, functors=functors)
    SynthesisConfigSpace(List(synthesisConfigs))
  }
}

case class SynthesisConfigs(recursion: Boolean, maxConstants: Int,
                           functors: Set[AbstractFunctorSpec]) {
  def get_rule_builder(problem: Problem, relevantOutRels: Set[Relation] = Set()): RuleBuilder = {
    val inputRels = problem.inputRels
    val outputRels = if (relevantOutRels.nonEmpty) relevantOutRels else problem.outputRels

    val builder = if (recursion) {
      require(maxConstants==0)
      if (functors.nonEmpty) {
        FunctorBuilder(inputRels, outputRels, recursion, functors)
      }
      else {
        new RecursionBuilder(inputRels, outputRels)
      }
    }
    else if (maxConstants > 0) {
      ConstantBuilder(inputRels, outputRels, problem.edb, problem.idb)
    }
    else {
      new SimpleRuleBuilder(inputRels, outputRels)
    }
    builder
  }
}
object SynthesisConfigs {
  def apply(recursion: Boolean, maxConstants: Int): SynthesisConfigs = new SynthesisConfigs(recursion, maxConstants, Set())
}

case class SynthesisAllPrograms(problem: Problem,
                              recursion: Boolean = true,
                              maxIters: Int = 20,
                              maxRefineIters: Int = 100,
                              initConfigSpace: SynthesisConfigSpace = SynthesisConfigSpace.emptySpace()
                             ) extends Synthesis(problem) {

  private val logger = Logger("Synthesis")

  private val evaluator = PartialRuleEvaluator(problem)
  private val configSpace: SynthesisConfigSpace = {
    if (initConfigSpace.isEmpty) SynthesisConfigSpace.getConfigSpace(problem) else initConfigSpace
  }

  def getConfigSpace: SynthesisConfigSpace = configSpace

  def learnNPrograms(idb: Set[Tuple]): List[Program] = {
    require(idb.map(_.relation).size == 1, s"Only idb of one relation at a time.")

    /** Learn all possible rules, then combine them. */
    var examples: Set[Tuple] = idb
    var rules: Set[Rule] = Set()
    var iters: Int = 0

    // Init the rule pool with the most general rules
    val ruleBuilder = configSpace.get_config().get_rule_builder(problem)
    var generalRules: Set[Rule] = ruleBuilder.mostGeneralRules()

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
    programs
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
    val ruleList: List[Rule] = {
      val recursiveRules = rules.filter(_.isRecursive())
      val nonRecursiveRules = rules.diff(recursiveRules)

      // sort nonrecursive rules by the output sizes
      val nonRecursiveSorted: List[Rule] = {
        // def outputCounts(rule: Rule): Int = {
        //   val idb = evaluator.evalRule(rule, Set())
        //   idb.size
        // }
        // nonRecursiveRules.toList.sortBy(outputCounts)(Ordering[Int].reverse)
        val scoredRules: List[ScoredRule] = nonRecursiveRules.map(r => scoreRule(r, idb, Set())).toList
        val ans = scoredRules.sorted(Ordering[ScoredRule].reverse).map(_.rule)
        ans
      }
      nonRecursiveSorted ::: recursiveRules.toList
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

  def learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule]): (Set[Tuple], Set[Rule], Set[Rule]) = {

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
      ans = _learnNRules(idb, generalSimpleRules, learnedRules, ruleBuilder.refineRule, maxExtraIters = 10)

      if (ans._2.isEmpty) {
        config = configSpace.next_config()
        logger.info(s"Increase config space ${config}")
      }
    } while (ans._2.isEmpty)
    ans
  }

  def _learnNRules(idb: Set[Tuple], generalSimpleRules: Set[Rule], learnedRules: Set[Rule],
                   refineRule: Rule => Set[Rule],
                   maxExtraIters: Int = 1,
                  maxRules: Int = 5): (Set[Tuple], Set[Rule], Set[Rule]) = {
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

    while (iters < maxRefineIters && extraIters < maxExtraIters && rulePool.nonEmpty && validRules.size < maxRules) {

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
    if (validRules.size == maxRules) logger.info(s"Hit maximum rules ${maxRules}")
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
