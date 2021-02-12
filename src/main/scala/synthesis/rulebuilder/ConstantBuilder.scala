package synthesis.rulebuilder

import synthesis._

class ConstantBuilder(inputRels: Set[Relation], outputRels: Set[Relation],
                      constantPool: Map[Type, Set[Constant]],
                     recursion: Boolean = true,
                      maxConstant: Int = 2)
  extends RecursionBuilder(inputRels, outputRels, recursion) {

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
  def apply(inputRels: Set[Relation], outputRels: Set[Relation], edb: Examples, idb: Examples,
            recursion: Boolean = true,
           maxConstant: Int = 2, maxConstantPoolSize: Int=5): ConstantBuilder = {
    val constantPool = getConstantPool(edb, idb, maxConstantPoolSize)
    new ConstantBuilder(inputRels, outputRels, constantPool, recursion, maxConstant)
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
    val instanceType = NumberType("InstanceId")
    val constantMap: Map[Type, Set[Constant]] = allConstants.groupBy(_._type).removed(instanceType)

    if (maxConstantPoolSize > 0) {
      constantMap.filter(t => t._2.size < maxConstantPoolSize)
    }
    else {
      constantMap
    }
  }
}
