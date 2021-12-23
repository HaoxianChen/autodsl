package synthesis.rulebuilder

import synthesis._
import synthesis.rulebuilder.ConstantBuilder.getConstantPool
import synthesis.rulebuilder.SimpleRuleBuilder.paramMapByType

class RecursionBuilder(inputRels: Set[Relation], outputRels: Set[Relation],
                      maxRelCount: Int = 1,
                       recursion: Boolean=true)
  extends SimpleRuleBuilder(inputRels, outputRels, maxRelCount = maxRelCount) {

  override def candidateRelations(rule: Rule): Set[Relation] = {
    val rels = super.candidateRelations(rule)
    val bodyRels = rule.body.map(_.relation)
    if (recursion && !bodyRels.contains(rule.head.relation)) {
      rels + rule.head.relation
    }
    else rels
  }

  // override def addGeneralLiteral(rule: Rule): Set[Rule] = {
  //   if (!recursion) {
  //     super.addGeneralLiteral(rule)
  //   }
  //   else {
  //     var newRules: Set[Rule] = Set()
  //     val bodyRels = rule.body.map(_.relation)
  //     val rels = (inputRels + rule.head.relation).diff(bodyRels)
  //     for (rel <- rels) {
  //       newRules += _addGeneralLiteral(rule, rel)
  //     }
  //     newRules
  //   }
  // }

}

class FunctorBuilder(inputRels: Set[Relation], outputRels: Set[Relation],
                     maxRelCount: Int = 1,
                    recursion: Boolean,
                    functors: Set[FunctorSpec], filters: Set[FilterSpec],
                     constantPool: Map[Type, Set[Constant]] = Map(),
                    maxConstants: Int = 0,
                    inputAggregators: Set[InputAggregator] = Set())
  extends ConstantBuilder(inputRels, outputRels,
    maxRelCount=maxRelCount,
    constantPool,
    recursion=recursion, maxConstants = maxConstants) {

  private val aggToRelMap: Map[Relation, Relation] = inputAggregators.map(ia => ia.getAggHeadRel -> ia.relation).toMap
  private val relToAggMap: Map[Relation, Set[Relation]] = inputAggregators.groupBy(_.relation) map {
    case (rel, ias) => rel -> ias.map(_.getAggHeadRel)
  }

  override def candidateRelations(rule: Rule): Set[Relation] = {
    val preAggRels: Set[Relation] = rule.body.flatMap(lit => aggToRelMap.get(lit.relation))
    val aggRels = preAggRels.flatMap(rel => relToAggMap.getOrElse(rel,Set()))
    val candidateRels = super.candidateRelations(rule)
    candidateRels.diff(aggRels)
  }
  override def candidateNegRelations(rule: Rule): Set[Relation] = {
    val preAggRels: Set[Relation] = rule.body.flatMap(lit => aggToRelMap.get(lit.relation))
    val aggRels = preAggRels.flatMap(rel => relToAggMap.getOrElse(rel,Set()))
    val candidateRels = super.candidateNegRelations(rule)
    candidateRels.diff(aggRels)
  }

  def allGroundedBindings(sig: List[Type], paramMap: Map[Type, Set[Parameter]]): Set[List[Parameter]] = {
    sig match {
      case Nil => Set(List())
      case head :: tail => {
        /** Bind to existing parameters, if none, create a new one */
        val headBindings: Set[Parameter] = paramMap.getOrElse(head, Set())
        val tailBindings: Set[List[Parameter]] = allGroundedBindings(tail, paramMap)
        headBindings.flatMap(
          f => tailBindings.map( t => f +: t)
        )
      }
    }
  }

  def addOneFunctor(rule: Rule, functorSpec: FunctorSpec): Set[Rule] = {
    /** Bind input variables */
    val posLits = rule.getPositiveLiterals()
    val posParams: Map[Type, Set[Parameter]] = paramMapByType(posLits)
    val inputSig = functorSpec.inputSig
    val inputBindings: Set[List[Parameter]] = allGroundedBindings(inputSig, posParams)

    /** All input parameters should have appeared in the body */
    val allParams = posLits.flatMap(_.fields)
    assert(inputBindings.forall(_.forall(p => allParams.contains(p))))

    /** Bind output variable to the head */
    val unboundHeadVars: Set[Parameter] = rule.getUngroundHeadVariables().toSet
    val outputBindings: Set[Parameter] = unboundHeadVars.filter(v => v._type == functorSpec.outputSig)

    /** Cross product of input and output bindings */
    val functorLits: Set[FunctorLiteral] = inputBindings.flatMap(ib => outputBindings.map(
      ob => functorSpec.makeLiteral(ib, ob)))

    functorLits.filter(_.isValid).map(lit => rule.addLiteral(lit))
  }

  def addOneFilter(rule: Rule, filterSpec: FilterSpec): Set[Rule] = {
    /** Bind input variables */
    val posLits = rule.getPositiveLiterals()
    val posParams: Map[Type, Set[Parameter]] = paramMapByType(posLits)
    val inputBindings0: Set[List[Parameter]] = allGroundedBindings(filterSpec.signature, posParams)

    /** Filters should have unique inputs */
    val inputBindings = inputBindings0.filter(l => l.toSet.size == l.size)

    /** All input parameters should have appeared in the body */
    val allParams = posLits.flatMap(_.fields)
    assert(inputBindings.forall(_.forall(p => allParams.contains(p))))

    val filterLits = inputBindings.map(ps => FunctorLiteral(filterSpec.getRelation, ps, filterSpec))
    val r1 = filterLits.map(lit => rule.addLiteral(lit))

    /** Filter can be negated as well */
    val r2 = filterLits.map(lit => rule.addNegatedLiteral(lit))
    r1 ++ r2
  }

  def addFunctor(rule: Rule): Set[Rule] = {
    /** Don't add duplicated functors */
    val legitFunctors = functors.filterNot(f => rule.getAllRelations().contains(f.getRelation))
    legitFunctors.flatMap(f => addOneFunctor(rule, f))
  }
  def addFilter(rule: Rule): Set[Rule] = {
    val legitFilters = filters.filterNot(f => rule.getAllRelations().contains(f.getRelation))
    legitFilters.flatMap(f => addOneFilter(rule, f))
  }

  override def refineRule(rule: Rule): Set[Rule] = {
    val simpleRules = super.refineRule(rule)
    val functorRules = addFunctor(rule)
    /** Only add filters when the all head variable is bound. */
    val filterRules = if (rule.isHeadBounded()) addFilter(rule) else Set()
    // simpleRules ++ functorRules
    val rules = simpleRules ++ functorRules ++ filterRules
    def conditions : Literal => Boolean = {lit => inputRels.contains(lit.relation) ||
      outputRels.contains(lit.relation) ||
      lit.isInstanceOf[FunctorLiteral] ||
      lit.relation == rule.head.relation
    }
    val invalidRules = rules.filterNot(_.body.forall(conditions))
    // require(rules.flatMap(_.body).forall(lit => inputRels.contains(lit.relation)
    //   || outputRels.contains(lit.relation)
    //   || lit.isInstanceOf[FunctorLiteral]), s"${invalidRules}")
    assert(invalidRules.isEmpty, s"${outputRels}\n${invalidRules}")
    rules
  }
}

object FunctorBuilder {
  def apply(inputRels: Set[Relation], outputRels: Set[Relation],
            maxRelCount: Int,
            recursion: Boolean,
             abstractFunctorSpecs: Set[AbstractFunctorSpec],
           ): FunctorBuilder = {
    val functors = getFunctors(abstractFunctorSpecs)
    val filters = getFfilters(abstractFunctorSpecs)
    new FunctorBuilder(inputRels, outputRels, maxRelCount, recursion, functors, filters)
  }

  def getFunctors(abstractFunctorSpecs: Set[AbstractFunctorSpec]): Set[FunctorSpec] = abstractFunctorSpecs.flatMap {
      case f: FunctorSpec => Some(f)
      case _: FilterSpec => None
    }

  def getFfilters(abstractFunctorSpecs: Set[AbstractFunctorSpec]): Set[FilterSpec] = abstractFunctorSpecs.flatMap {
    case f: FilterSpec => Some(f)
    case _: FunctorSpec => None
  }

  def apply(inputRels: Set[Relation], outputRels: Set[Relation],
           maxRelCount: Int,
            recursion: Boolean,
            abstractFunctorSpecs: Set[AbstractFunctorSpec],
            edb: Examples, idb: Examples,
            maxConstants: Int,
           inputAggregators: Set[InputAggregator], maxConstantPoolSize: Int=5
  ): FunctorBuilder = {
    val functors = getFunctors(abstractFunctorSpecs)
    val filters = getFfilters(abstractFunctorSpecs)
    val constantPool = getConstantPool(edb, idb, maxConstantPoolSize)
    new FunctorBuilder(inputRels, outputRels, maxRelCount, recursion, functors, filters, constantPool, maxConstants,
      inputAggregators=inputAggregators)
  }
}