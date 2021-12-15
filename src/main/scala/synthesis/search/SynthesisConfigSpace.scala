package synthesis.search

import synthesis.{Problem, Relation}
import synthesis.rulebuilder.{AbstractFunctorSpec, Add, AggCount, AppendList, ConstantBuilder, FunctorBuilder, Greater, UnEqual, Increment, InputAggregator, ListContain, MakeList, Max, Min, PrependList, Quorum, RecursionBuilder, RuleBuilder, SimpleRuleBuilder, TimeOut}

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
      case "SDN" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          UnEqual.allInstances
        )
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(maxRelCount=1,recursion = false, maxConstants = List(0,5), functors=functors)
      }
      case "NIB" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          PrependList.allInstances, MakeList.allInstances,
          AppendList.allInstances,
          Add.allInstances, Min.allInstances, Max.allInstances,
          UnEqual.allInstances
        )
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(maxRelCount=2, recursion = true, functors = functors)
      }
      case "routing" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          PrependList.allInstances, MakeList.allInstances,
          AppendList.allInstances,
          Add.allInstances, Min.allInstances, Max.allInstances,
          UnEqual.allInstances
        )
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(maxRelCount=1, recursion = true, functors=functors)
      }
      case "consensus" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          Greater.allInstances
        )
        val inputAggregators = AggCount.allInstances(problem)
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(maxRelCount=1, recursion = false, functors=functors, inputAggregators=inputAggregators)
      }
      case "consensusbarrier" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          Quorum.allInstances
        )
        val inputAggregators = AggCount.allInstances(problem)
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(maxRelCount=1, recursion = false, functors=functors, inputAggregators=inputAggregators)
      }
      case "overlay" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          Increment.allInstances, Greater.allInstances, TimeOut.allInstances
        )
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(maxRelCount=2, recursion = false, functors=functors, maxConstants = List(1))
      }
      case "sensor" => {
        _getConfigSpace(maxRelCount=1,recursion = false, maxConstants = List(0))
      }
      case "routingProto" => {
        val functorConstructors: Set[Problem => Set[AbstractFunctorSpec]] = Set(
          Max.allInstances, Increment.allInstances, Greater.allInstances,
          AppendList.allInstances, PrependList.allInstances, ListContain.allInstances,
          UnEqual.allInstances
        )
        val functors: Set[AbstractFunctorSpec] = functorConstructors.flatMap(f => f(problem))
        _getConfigSpace(maxRelCount=1, recursion = false, functors = functors)
      }
      case _ => ???
    }
  }
  // def _getConfigSpace(maxRelCount:Int, recursion: Boolean, maxConstants: List[Int],
  //                     functors: Set[AbstractFunctorSpec]): SynthesisConfigSpace = {
  //   val allConfigs: List[SynthesisConfigs] = maxConstants.map (
  //     c => SynthesisConfigs(maxRelCount,recursion, maxConstants = c, functors=functors))
  //   SynthesisConfigSpace(allConfigs)
  // }
  def _getConfigSpace(maxRelCount:Int,
                      recursion: Boolean,
                      functors: Set[AbstractFunctorSpec] = Set(),
                      inputAggregators: Set[InputAggregator]=Set(),
                      maxConstants: List[Int] = List(0))
  : SynthesisConfigSpace = {
    val allConfigs: List[SynthesisConfigs] = maxConstants.map (
         c => SynthesisConfigs(maxRelCount, recursion, maxConstants = c, functors=functors,
      inputAggregators=inputAggregators)
      )
    SynthesisConfigSpace(allConfigs)
  }
}

case class SynthesisConfigs(maxRelCount:Int, recursion: Boolean, maxConstants: Int,
                            functors: Set[AbstractFunctorSpec],
                           inputAggregators: Set[InputAggregator]) {
  def get_rule_builder(problem: Problem, relevantOutRels: Set[Relation] = Set()): SimpleRuleBuilder = {
    val inputRels = problem.inputRels
    val outputRels = if (relevantOutRels.nonEmpty) relevantOutRels else problem.outputRels

    val builder =
      if (recursion) {
        require(maxConstants == 0)
        if (functors.nonEmpty) {
          FunctorBuilder(inputRels, outputRels, maxRelCount, recursion, functors)
        }
        else {
          new RecursionBuilder(inputRels, outputRels, maxRelCount)
        }
      }
      else if (maxConstants > 0 && functors.isEmpty) {
        ConstantBuilder(inputRels, outputRels, maxRelCount, problem.edb, problem.idb, maxConstants=maxConstants, recursion=recursion)
      }
      else if (functors.nonEmpty) {
        FunctorBuilder(inputRels, outputRels, maxRelCount, recursion, functors, problem.edb, problem.idb, maxConstants=maxConstants,
        inputAggregators=inputAggregators)
      }
      else if (maxConstants > 0 ) {
        FunctorBuilder(inputRels, outputRels, maxRelCount, recursion, functors,
          problem.edb, problem.idb, maxConstants, inputAggregators)
      }
      else {
        new SimpleRuleBuilder(inputRels, outputRels, maxRelCount)
      }
    builder
  }
}
object SynthesisConfigs {
  def apply(maxRelCount: Int, recursion: Boolean, maxConstants: Int): SynthesisConfigs = new SynthesisConfigs(maxRelCount,recursion, maxConstants, Set(), Set())
  def apply(maxRelCount: Int, recursion: Boolean, maxConstants: Int, functors: Set[AbstractFunctorSpec]): SynthesisConfigs = new SynthesisConfigs(maxRelCount,recursion, maxConstants, functors, Set())
}

