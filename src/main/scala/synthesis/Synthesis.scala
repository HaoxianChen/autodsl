package synthesis

import scala.collection.mutable

case class RuleConstructor(inputRels: Set[Relation], outputRels: Set[Relation]) {
  def paramMapByType(literals: Set[Literal]): Map[Type, Set[Parameter]] = {
    val allParams = literals.flatMap(_.fields)
    allParams.groupBy(_._type)
  }

  def addGeneralLiteral(rule: Rule, relation: Relation) : Rule = {
    /** Add relation to rule, without binding the variables */
    val params = paramMapByType(rule.body + rule.head)
    var fields: mutable.ListBuffer[Variable] = mutable.ListBuffer()
    var paramCounts: Map[Type, Int] = params.map {case (t, ps) => t -> ps.size}
    for (_type <- relation.signature) {
      val c = paramCounts.getOrElse(_type,0)
      paramCounts = paramCounts.updated(_type, c+1)
      val varName = s"${_type.name.toLowerCase()}${c}"
      fields += Variable(varName, _type)
    }
    require(fields.size == relation.signature.size)
    val newLiteral = Literal(relation, fields.toList)
    rule.addLiteral(newLiteral)
  }

  def mostGeneralRules(): Set[Rule] = {
    def makeHead(rel: Relation): Literal = {
      var typeCounts: Map[Type, Int] = Map()
      var fields: mutable.ListBuffer[Variable] = mutable.ListBuffer()
      for (_type <- rel.signature) {
        val c = typeCounts.getOrElse(_type, 0)
        typeCounts = typeCounts.updated(_type, c+1)
        fields += Variable(s"${_type.name.toLowerCase()}${c}", _type)
      }
      require(fields.size == rel.signature.size)
      Literal(rel, fields.toList)
    }

    def bindHead(rule: Rule): Set[Rule] = {
      val VarsByType = paramMapByType(rule.body)

      def bindHead(signature: List[Type]): Set[List[Parameter]] = {
        signature match {
          case Nil => Set(List())
          case head::tail => {
            // bind with any one of the variables of the same type in the body
            val newVars: Set[Parameter] = VarsByType(head)
            val allSubsequentFields = bindHead(tail)
            newVars.flatMap(v => allSubsequentFields.map(fs => v::fs))
          }
        }
      }
      val newHeads: Set[Literal] = {
        val allFields = bindHead(rule.head.relation.signature)
        allFields.map(fs => Literal(rule.head.relation, fs))
      }
      newHeads.map(h => Rule(h, rule.body))
    }

    def isHeadBounded(head: Literal, body: Set[Literal]): Boolean = {
      val allTypes = body.flatMap(_.relation.signature.toSet)
      head.relation.signature.forall(t => allTypes.contains(t))
    }

    def extendBody(rule: Rule, rels: List[Relation]): Set[Rule] = {
      rels match {
        case Nil => Set()
        case first :: rest => {
          val newRule = addGeneralLiteral(rule, first)
          if (isHeadBounded(rule.head, newRule.body)) extendBody(rule,rest) + newRule
          else extendBody(newRule, rest) ++ extendBody(rule, rest)
        }
      }
    }

    // Generate most general rules for each output relation
    outputRels.flatMap {
      rel =>
        val head = makeHead(rel)
        val headOnlyRule = Rule(head, Set())
        val unboundedRules = extendBody(headOnlyRule, inputRels.toList)
        unboundedRules.flatMap(bindHead)
    }
  }
  def refineRule(rule: Rule): Set[Rule] = ???
}

sealed abstract class Synthesis(problem: Problem) {
  def go(): Set[Program]
}

case class BasicSynthesis(problem: Problem,
                          maxIters: Int = 200,
                         maxRefineIters: Int = 200,
                         ) extends Synthesis(problem) {
  def go(): Set[Program] = Set(learnAProgram())

  private val ruleConstructor = RuleConstructor(problem.inputRels, problem.outputRels)
  private val evaluator = Evaluator(problem)

  case class ScoredRule(rule: Rule, score: Double)
  object ScoredRule {
    def apply(rule: Rule, idb: Set[Tuple]): ScoredRule = new ScoredRule(rule, scoreRule(rule, idb))
  }

  def learnAProgram(): Program = {
    var examples: Set[Tuple] = problem.idb.toTuples()
    var rules: Set[Rule] = Set()
    var iters: Int = 0

    while (examples.nonEmpty && iters < maxIters ) {
      val (coveredExamples, newRule) = learnARule(examples)
      examples = examples -- coveredExamples
      rules = rules + newRule
      iters += 1
    }
    Program(rules)
  }

  def learnARule(idb: Set[Tuple]): (Set[Tuple], Rule) = {
    var iters: Int = 0
    var rulePool: mutable.PriorityQueue[ScoredRule] = mutable.PriorityQueue()(Ordering.by(_.score))
    val rules = ruleConstructor.mostGeneralRules()
    // rulePool ++= ruleConstructor.mostGeneralRules().map(r => ScoredRule(r))
    rulePool ++= rules.map(r => ScoredRule(r, idb))
    var validRules: Set[ScoredRule] = Set()

    while (iters < maxRefineIters && validRules.isEmpty) {

      // pop highest scored rule from pool
      val baseRule: Rule = rulePool.dequeue().rule

      // refine the rules
      val candidateRules: Set[Rule] = ruleConstructor.refineRule(baseRule)

      // keep the valid ones
      validRules ++= candidateRules.filter(isRuleValid).map(r => ScoredRule(r, idb))

      // Put the too general ones into the pool
      rulePool ++= candidateRules.filter(isRuleTooGeneral).map(r => ScoredRule(r, idb))

      iters += 1
    }

    val bestRule = validRules.maxBy(_.score).rule
    (evalRule(bestRule), bestRule)
  }

  def isRuleValid(rule: Rule): Boolean = ???

  def isRuleTooGeneral(rule: Rule): Boolean = ???

  def evalRule(rule: Rule): Set[Tuple] = evaluator.eval(Program(Set(rule)))

  def scoreRule(rule: Rule, allIdb: Set[Tuple]): Double = {
    val refIdb = allIdb.filter(_.relation == rule.head.relation)
    require(refIdb.nonEmpty)

    val idb = evalRule(rule)
    require(idb.nonEmpty)

    val pos: Set[Tuple] = idb.intersect(refIdb)
    val neg: Set[Tuple] = idb.diff(refIdb)

    val npos = pos.size
    val nneg = neg.size
    require(npos+nneg == idb.size)

    npos.toDouble / (npos + nneg).toDouble
  }
}
