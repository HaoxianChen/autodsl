package synthesis

import synthesis.activelearning.ActiveLearning
import synthesis.experiment.{ActiveLearningExperiment, AllSynthesisExperiments, Experiment, FaconExperiment, SynthesisExperiment}

import java.nio.file.Paths
import synthesis.search.{FaconSynthesizer, SolutionChecker, Synthesis, SynthesisAllPrograms, SynthesisConfigSpace, SynthesisConfigs}
import synthesis.util.{ExampleConvertor, Misc}

object Main extends App {

  val benchmarkDir: String = s"/Users/hxc/projects/autodsl-bench"

  def displayResults(problem: Problem, programs: Map[Relation, List[Program]]) = {

    /*** Gather problem specs */
    val nInputTuples: Int = problem.edb.toTuples().size
    val nOutputTuples: Int =  problem.idb.toTuples().size
    val nTotalTuples = nInputTuples + nOutputTuples
    println(s"${problem.name}")
    val nInputRels = problem.inputRels.size
    val nOutputRels = problem.outputRels.size
    val nTotal = nInputRels + nOutputRels
    println(s"$nTotal Relations ($nInputRels, $nOutputRels)")
    println(s"$nTotalTuples examples ($nInputTuples, $nOutputTuples).")
    println(s"${problem.getNumExampleInstances} example instances.")

    /*** Display examples */
    // val exampleInstances = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)
    // for (e <- exampleInstances) {
    //   println(s"Input: ${e.input.mkString(",")}")
    //   println(s"Output: ${e.output.mkString(",")}\n")
    // }

    /*** Display results */
    var literalCounts: Int = 0
    var fieldCounts: Int = 0
    for ((rel,ps)<-programs) {
      val p = ps.head
      literalCounts += p.literalCounts
      fieldCounts += p.fieldCounts
      println(s"$rel: ${ps.size} programs, smallest one contains ${p.literalCounts} literals")
      println(p)
    }
    println(s"${literalCounts} literals, ${fieldCounts} fields.")
  }

  if (args(0)== "parse") {
    val problem = Misc.readProblem(args(1))
    println(problem)
  }
  else if (args(0)== "learn") {
    val problem = Misc.readProblem(args(1))
    val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(args(1))

    val t1 = System.nanoTime

    val synthesizer = Synthesis(problem)
    val programs = synthesizer.go()
    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s")

    val checker = SolutionChecker(problem,staticConfigRelations)
    if (checker.check(programs)) println(s"Correct solution.") else println(s"Incorrect solution.")
    displayResults(problem, programs)
  }
  else if (args(0) == "tab1") {
    require(args.size == 3)
    val repeats = args(1).toInt
    val _benchmarkDir = args(2)
    // val experiment = new AllSynthesisExperiments(_benchmarkDir, outDir="results")
    val experiment = new AllSynthesisExperiments(_benchmarkDir, outDir="results-2", repeats=repeats)
    experiment.run()
  }
  else if (args(0) == "tab2") {
    require(args.size == 3)
    val repeats = args(1).toInt
    val _benchmarkDir = args(2)
    val experiment = new ActiveLearningExperiment(_benchmarkDir,
      // outDir = s"results/active-learning")
      outDir = s"results-2/active-learning")
    experiment.run(Experiment.activelearningProblems, repeats)
  }
  else if (args(0) == "tab2full") {
    require(args.size == 3)
    val repeats = args(1).toInt
    val _benchmarkDir = args(2)
    val experiment = new ActiveLearningExperiment(_benchmarkDir,
      // outDir = s"results/active-learning-full")
      outDir = s"results-2/active-learning-full")
    experiment.run(Experiment.activeLearningWithOracle ,repeats = repeats)
  }
  else if (args(0) == "tab3") {
    require(args.size == 3)
    val repeats = args(1).toInt
    val _benchmarkDir = args(2)
    val experiment = new ActiveLearningExperiment(_benchmarkDir,
      // outDir = s"results/random-drop")
      outDir = s"results-2/random-drop")
    // val nDrops = List(1,2,3,4,5)
    experiment.runRandomDrops(repeats = repeats)
    // experiment.runRandomDrops(repeats = repeats, nDrops=nDrops)
  }
  else if (args(0) == "maketable2") {
    require(args.size == 3)
    val _benchmarkDir = args(1)
    val _resultDir = args(2)
    ActiveLearningExperiment.makeAllTables(_benchmarkDir,_resultDir)
    // SynthesisExperiment.generateTable(_benchmarkDir,_resultDir)
  }
  else if (args(0) == "tune-sample-size") {
    require(args.size == 3)
    val repeats = args(1).toInt
    val _benchmarkDir = args(2)
    val experiment = new ActiveLearningExperiment(_benchmarkDir,
      outDir = s"results/tune-sample-size")
    experiment.tuneSampleParameter(Experiment.activeLearningWithOracle, repeats=repeats,
      numSamples = List(10))
  }
  else if (args(0) == "maketable1") {
    require(args.size == 3)
    val _benchmarkDir = args(1)
    val _resultDir = args(2)
    SynthesisExperiment.generateTable(_benchmarkDir,_resultDir)
  }
  else if (args(0) == "active") {
    val problem = Misc.readProblem(args(1))
    val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(args(1))
    val maxExamples: Int = args(2).toInt
    val learner = new ActiveLearning(problem, staticConfigRelations, maxExamples)
    val (program, numRuns, nQueries, durations, correctness, isTimeOut, hasError, finalProblem) = learner.go()
    if (correctness) println(s"Correct solution") else println(s"Incorrect solution.")
    println(s"${nQueries} queries.")
    println(program)
  }
  else if (args(0) == "drop") {
    /** Random drop some example from the complete example pool. */
    val problem = Misc.readProblem(args(1))
    val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(args(1))
    val nDrop: Int = args(2).toInt
    val repeats: Int = args(3).toInt
    require(repeats <= 10)
    val benchmarkDir: String = s""
    val experiment = new ActiveLearningExperiment(benchmarkDir)
    experiment.go(problem, staticConfigRelations, nDrop=nDrop, repeats=repeats)
  }
  else if (args(0) == "debloat") {
    require(args.size == 3)
    val repeats = args(1).toInt
    val _benchmarkDir = args(2)
    val experiment = new ActiveLearningExperiment(_benchmarkDir,
      outDir="results-2/debloat")
    experiment.run(Experiment.debloatingExperiments,repeats = repeats)
    // val problem = Misc.readProblem(args(1))
    // val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(args(1))
    // val repeats: Int = args(2).toInt
    // require(repeats <= 10)
    // val experiment = new DebloatingExperiment()
    // experiment.go(problem, staticConfigRelations, repeats=repeats)
  }
  else if (args(0) == "foil") {
    val problem = Misc.readProblem(args(1))

    val t1 = System.nanoTime

    val config = SynthesisConfigs(maxRelCount = 2, recursion = true, maxConstants = 0, functors = Set(),
      inputAggregators = Set())
    val synthesizer = SynthesisAllPrograms(problem,maxRules = 1,
                                           initConfigSpace = SynthesisConfigSpace(List(config)))
    val programs = synthesizer.go()

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s")
    displayResults(problem, programs)
  }

  else if (args(0)== "regression-test") {
    val benchmarkDir = "/Users/hxc/projects/autodsl-bench"
    val allProblems = Experiment.regressionTests.map(s => Paths.get(benchmarkDir, s))
    for (problemFile <- allProblems) {
      println(problemFile)
      val problem = Misc.readProblem(problemFile.toString)
      val t1 = System.nanoTime

      val synthesizer = Synthesis(problem)
      val programs = synthesizer.go()

      val duration = (System.nanoTime - t1) / 1e9d
      println(s"Finished in ${duration}s")
      displayResults(problem ,programs)
      assert(programs.nonEmpty, s"Test failed: ${problemFile}.")
    }
  }

  else if (args(0)=="parse-examples") {
    val dir = args(1)
    val exampleCovertor = new ExampleConvertor()
    exampleCovertor.parse(dir)
  }
  else {
    assert(false, s"Unrecognized command ${args(0)}")
  }
}


