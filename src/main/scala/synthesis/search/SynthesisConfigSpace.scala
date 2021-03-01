package synthesis.search

import synthesis.{Problem, Relation}
import synthesis.rulebuilder.{AbstractFunctorSpec, Add, AppendList, ConstantBuilder, FunctorBuilder, Greater, Increment, MakeList, Max, Min, PrependList, Quorum, RecursionBuilder, RuleBuilder, SimpleRuleBuilder}

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
          PrependList.allInstances, MakeList.allInstances,
          AppendList.allInstances,
          Add.allInstances, Min.allInstances, Max.allInstances
        )
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(recursion = true, functors = functors)
      }
      case "routing" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          PrependList.allInstances, MakeList.allInstances,
          AppendList.allInstances,
          Add.allInstances, Min.allInstances, Max.allInstances
        )
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(recursion = true, functors=functors)
      }
      case "consensus" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          Quorum.allInstances
        )
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(recursion = false, functors=functors)
      }
      case "overlay" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          Increment.allInstances, Greater.allInstances
        )
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(recursion = false, functors=functors, maxConstants = 1)
      }
      case _ => ???
    }
  }
  def _getConfigSpace(recursion: Boolean, maxConstants: List[Int]): SynthesisConfigSpace = {
    val allConfigs: List[SynthesisConfigs] = maxConstants.map (c => SynthesisConfigs(recursion, maxConstants = c))
    SynthesisConfigSpace(allConfigs)
  }
  def _getConfigSpace(recursion: Boolean, functors: Set[AbstractFunctorSpec], maxConstants: Int=0): SynthesisConfigSpace = {
    val synthesisConfigs = SynthesisConfigs(recursion, maxConstants = maxConstants, functors=functors)
    SynthesisConfigSpace(List(synthesisConfigs))
  }
}

case class SynthesisConfigs(recursion: Boolean, maxConstants: Int,
                            functors: Set[AbstractFunctorSpec]) {
  def get_rule_builder(problem: Problem, relevantOutRels: Set[Relation] = Set()): RuleBuilder = {
    val inputRels = problem.inputRels
    val outputRels = if (relevantOutRels.nonEmpty) relevantOutRels else problem.outputRels

    val builder =
      if (recursion) {
        require(maxConstants == 0)
        if (functors.nonEmpty) {
          FunctorBuilder(inputRels, outputRels, recursion, functors)
        }
        else {
          new RecursionBuilder(inputRels, outputRels)
        }
      }
      else if (maxConstants > 0 && functors.isEmpty) {
        ConstantBuilder(inputRels, outputRels, problem.edb, problem.idb, maxConstants=maxConstants)
      }
      else if (functors.nonEmpty) {
        FunctorBuilder(inputRels, outputRels, recursion, functors, problem.edb, problem.idb, maxConstants=maxConstants)
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

