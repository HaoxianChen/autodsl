package synthesis.rulebuilder

import synthesis._
import synthesis.activelearning.ExampleInstance

class ConstantBuilder(inputRels: Set[Relation], outputRels: Set[Relation],
                      maxRelCount: Int = 1,
                      constantPool: Map[Type, Set[Constant]],
                      recursion: Boolean = true,
                      maxConstants: Int = 2)
  extends RecursionBuilder(inputRels, outputRels, recursion=recursion, maxRelCount = maxRelCount) {

  def _bindToConstant(rule: Rule, variable: Variable): Set[Rule] = {
    require(rule.getVarSet().contains(variable))
    val constants = constantPool.getOrElse(variable._type, Set())
    val bindings: Set[Map[Parameter, Parameter]] = constants.map(c => Map(variable -> c))
    bindings.map(b => rule.rename(b))
  }

  override def addBinding (rule: Rule): Set[Rule] = {

    /** bindBody should only bind free variables to another variables,
     * therefore the number of constants should remain the same. */
    val bindBody = super.addBinding(rule)
    val numConstants = rule.getConstantList.size
    assert(bindBody.forall(_.getConstantList.size == numConstants))

    /** only bind constants if: (1) the rule doesn't consist more constants than threshold;
     * and (2) the rule has just small enough free variables. This will bias the search to
     * more general rules first. */
    val freeVars = rule.freeVariables()
    if (numConstants < maxConstants
      && freeVars.size <= maxConstants
    ) {
      // val bindConstants: Set[Rule] = freeVars.flatMap(v => _bindToConstant(rule, v))
      val allVars: Set[Variable] = rule.getVarSet()
      val bindConstants: Set[Rule] = allVars.flatMap(v => _bindToConstant(rule, v))
      bindBody ++ bindConstants
    }
    else {
      bindBody
    }
  }
}

object ConstantBuilder {
  def apply(inputRels: Set[Relation], outputRels: Set[Relation],
            maxRelCount: Int,
            edb: Examples, idb: Examples,
            recursion: Boolean = true,
            maxConstants: Int = 2,
            // maxConstantPoolSize: Int=5
            maxConstantPoolSize: Int=4
           ): ConstantBuilder = {
    val constantPool = getConstantPool(edb, idb, maxConstantPoolSize)
    new ConstantBuilder(inputRels, outputRels, maxRelCount, constantPool, recursion, maxConstants)
  }

  def getConstantPool(edb: Examples, idb: Examples, maxConstantPoolSize:Int = 0): Map[Type, Set[Constant]] = {
    /** filter out responsive instances */
    // val allConstants = edb.getConstantSet ++ idb.getConstantSet
    val allConstants = filterResponsiveExamples(edb, idb)
    _getConstantPool(allConstants, maxConstantPoolSize)
  }

  def filterResponsiveExamples(edb: Examples, idb:Examples): Set[Constant] = {
    val instances: Set[ExampleInstance] = ExampleInstance.fromEdbIdb(edb,idb)
    val responsive: Set[ExampleInstance] = instances.filter(_.output.nonEmpty)
    responsive.flatMap(_.getConstants)
  }

  // def getConstantPool(examples: Examples, maxConstantPoolSize: Int ): Map[Type, Set[Constant]] = {
  //   val allConstants = examples.getConstantSet
  //   _getConstantPool(allConstants, maxConstantPoolSize)
  // }

  def _getConstantPool(allConstants: Set[Constant], maxConstantPoolSize: Int = 0 ): Map[Type, Set[Constant]] = {
    // Remove Instance Id from constant map
    val excludeTypes: Set[Type] = Set(NumberType("InstanceId"), NumberType("Count"), SymbolType("IP"), SymbolType("Mac"),
    NumberType("Buf"))
    val constantMap: Map[Type, Set[Constant]] = allConstants.filterNot(c => excludeTypes.contains(c._type)).groupBy(_._type)

    if (maxConstantPoolSize > 0) {
      constantMap.filter(t => t._2.size < maxConstantPoolSize)
    }
    else {
      constantMap
    }
  }

  def getAllConstantPool(edb: Examples, idb: Examples): Map[Type, Set[Constant]] = {

    val allConstants = filterResponsiveExamples(edb, idb)

    val excludeTypes: Set[Type] = Set(NumberType("InstanceId"))
    val inRels: Set[Relation] = edb.elems.keySet
    val includeTypes: Set[Type] = inRels.flatMap(_.signature)
    val constantMap: Map[Type, Set[Constant]] = allConstants.
      filterNot(c => excludeTypes.contains(c._type)).
      filter(c => includeTypes.contains(c._type)).
      groupBy(_._type)
    constantMap
  }
}
