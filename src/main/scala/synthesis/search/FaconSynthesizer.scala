package synthesis.search

import com.typesafe.scalalogging.Logger
import synthesis.rulebuilder.{SimpleRuleBuilder}
import synthesis.{Evaluator, Literal, Parameter, Problem, Program, Relation, Rule, SimpleLiteral, Tuple, Type, Variable}

class FaconSynthesizer(problem: Problem) extends SynthesisAllPrograms(problem) {
  private val logger = Logger("Facon")

  private val evaluator = Evaluator(problem)

  private val configSpace = SynthesisConfigSpace.getConfigSpace(problem)
  override def getConfigSpace: SynthesisConfigSpace = configSpace

  override def learnNPrograms(idb: Set[Tuple]): List[Program] = {
    require(idb.map(_.relation).size == 1, s"Only idb of one relation at a time.")

    /** Learn all possible rules, then combine them. */
    var examples: Set[Tuple] = idb
    var rules: Set[Rule] = Set()
    var covered: Set[Tuple] = Set()
    var iters: Int = 0

    while (examples.nonEmpty && iters < maxIters ) {
      val (coveredExamples, newRules) = learnNRules(examples, rules, covered)
      val nextExamples = examples -- coveredExamples
      require(nextExamples.size < examples.size)
      examples = nextExamples
      rules = rules ++ newRules
      covered = covered ++ coveredExamples
      // if (!configSpace.get_config().recursion) generalRules = remainingRules
      iters += 1
    }
    assert(examples.isEmpty)
    val programs = combineRules(rules, idb)
    logger.debug(s"Found ${programs.size} programs.")
    /** sort programs */
    programs.sorted
  }

   def learnNRules(idb: Set[Tuple], learnedRules: Set[Rule], coveredIdb: Set[Tuple]): (Set[Tuple], Set[Rule]) = {

    /** This while loop incrementally increase the program space */
    var config = configSpace.get_config()
    var ans: (Set[Tuple], Set[Rule]) = (Set(), Set())

    /** Lookup the configuration for the synthesizer*/
    val relevantOutRel: Set[Relation] = idb.map(_.relation)
    require(relevantOutRel.subsetOf(problem.outputRels), s"${relevantOutRel}, ${problem.outputRels}")
    require(relevantOutRel.size == 1)

    var allRules: Set[Rule] = iterateAllRules(relevantOutRel.head).toSet
     logger.debug(s"${allRules.size} candidate rules.")

    do {
      // val ruleBuilder = config.get_rule_builder(problem, relevantOutRels = relevantOutRel)
      ans = _learnNRules(idb, allRules, learnedRules, coveredIdb)

      if (ans._2.isEmpty) {
        config = configSpace.next_config()
        logger.info(s"Increase config space ${config}")
        /** Update rule sets. */
        allRules = iterateAllRules(relevantOutRel.head).toSet
      }
    } while (ans._2.isEmpty)
    ans
  }

  def _learnNRules(refIdb: Set[Tuple],
                   allRules: Set[Rule],
                   learnedRules: Set[Rule],
                   coveredIdb: Set[Tuple],
                   ): (Set[Tuple], Set[Rule]) = {
    def evalRule(rule: Rule, learnedRules: Set[Rule]): Set[Tuple] = {
      evaluator.eval(Program(learnedRules+rule)).diff(coveredIdb)
    }

    def validCondition(idb: Set[Tuple]): Boolean = {
      val pos = idb.intersect(refIdb)
      val neg = idb.diff(refIdb)
      pos.nonEmpty && neg.isEmpty
    }

    // Iterate on all rules
    for (rule <- allRules) {
      val output = evalRule(rule, learnedRules)
      if (validCondition(output)) {
        return (output, Set(rule))
      }
    }
    // val evalResults: Set[(Rule, Set[Tuple])] = allRules.map(
    //   r => (r,evalRule(r,learnedRules)))
    // val validRules: Set[Rule] = evalResults.flatMap {
    //   case (rule, output) => if (validCondition(output)) Some(rule) else None
    // }
    // val newCoveredIdb = evalResults.flatMap(_._2)
    // (newCoveredIdb, validRules)
    (Set(), Set())
  }

  def iterateAllRules(outRel: Relation): Iterable[Rule] = {
    val ruleBuilder: SimpleRuleBuilder = configSpace.get_config().get_rule_builder(problem)

    def bindOneLiteral(body: Set[Literal], relation: Relation, allowNewVariable: Boolean): Set[Literal] = {
      val paramMap: Map[Type,Set[Parameter]] = SimpleRuleBuilder.paramMapByType(body)
      def _bindFields(body: Set[Literal], bounded: List[Parameter], remainSig: List[Type]): Set[List[Parameter]] = {
        remainSig match {
          case Nil => Set(List())
          case nextType::tail => {
            val ps0 = paramMap.getOrElse(nextType, Set()) ++ bounded.toSet.filter(_._type==nextType)

            val paramSet: Set[Parameter] = if (allowNewVariable) {
              val newVar = Variable(nextType,ps0.size)
              require(!ps0.contains(newVar))
              ps0 + newVar
            }
            else ps0

            paramSet.flatMap {p =>
              val remainingFields = _bindFields(body, bounded:+p,tail)
              remainingFields.map(fields => p::fields)
            }
          }
        }
      }
      // val newLits: Set[Iterable[Parameter]] = Misc.crossJoin(paramList).toSet
      val newLits: Set[List[Parameter]] = _bindFields(body, List(), relation.signature)
      newLits.map(params => SimpleLiteral(relation,params))
    }

    def _allBindings(body: Set[Literal], rels: List[Relation]): Set[Set[Literal]] = {
      rels match {
        case Nil => Set(Set())
        case nextRel::tail => {
          val allNewLiterals: Set[Literal] = bindOneLiteral(body,nextRel,allowNewVariable=true)

          val withThisLit: Set[Set[Literal]] = {
            val nextBodies: Set[Set[Literal]] = allNewLiterals.map(lit => body+lit)
            allNewLiterals.flatMap {
              lit => _allBindings(body+lit, tail).map(tailLits => (body + lit) ++ tailLits)
            }
          }
          val withoutThisLit = _allBindings(body,tail).map(tailLits => body ++ tailLits)
          withThisLit ++ withoutThisLit
        }
      }
    }

    val candidateRelations = if(getConfigSpace.get_config().recursion) problem.inputRels+outRel else problem.inputRels
    val allBodies: Set[Set[Literal]] = _allBindings(Set(), candidateRelations.toList)
    // val relLists = {
    //   var ret: List[Relation] = List()
    //   for (i <- 1 to getConfigSpace.get_config().maxRelCount) {
    //     ret ++= candidateRelations.toList
    //   }
    //   ret
    // }
    // val allBodies: Set[Set[Literal]] = _allBindings(Set(), relLists)

    def bindHead(body: Set[Literal]): Set[Rule] = {
      /** Bind head to body literals */
      val heads: Set[Literal] = bindOneLiteral(body, outRel, allowNewVariable=false)
      heads.map(h => Rule(h,body))
    }

    val rules = allBodies.flatMap(bindHead).map(ruleBuilder.bindInstanceIds)
    val normaized = rules.map(_.normalize())
    normaized
  }
}
