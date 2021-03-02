package synthesis

import java.nio.file.Paths

import synthesis.search.{Synthesis, SynthesisAllPrograms}

object Main extends App {

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

    val t1 = System.nanoTime

    val synthesizer = Synthesis(problem)
    val programs = synthesizer.go()

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s")
    displayResults(problem, programs)
  }
  else if (args(0) == "active") {
    val problem = Misc.readProblem(args(1))
    val maxExamples: Int = args(2).toInt
    val learner = new ActiveLearning(problem, maxExamples)
    val (program, nQueries) = learner.go()
    println(s"${nQueries} queries.")
    println(program)
  }
  else if (args(0) == "drop") {
    /** Random drop some example from the complete example pool. */
    val problem = Misc.readProblem(args(1))
    val repeats: Int = args(2).toInt
    require(repeats <= 10)
    val experiment = new ActiveLearningExperiment()
    experiment.go(problem, repeats=repeats)
  }
  else if (args(0) == "debloat") {
    val problem = Misc.readProblem(args(1))
    val repeats: Int = args(2).toInt
    require(repeats <= 10)
    val experiment = new DebloatingExperiment()
    experiment.go(problem, repeats=repeats)
  }

  else if (args(0)== "regression-test") {
    val benchmarkDir = "/Users/hxc/projects/autodsl-bench"
    val allProblems = List("forwarding/learning-switch",
      "firewall/stateless-firewall",
      "firewall/stateful-firewall",
      "nib/reachable",
      "routing/shortest-path"
    ).map(s => Paths.get(benchmarkDir, s))
    for (problemFile <- allProblems) {
      println(problemFile)
      val problem = Misc.readProblem(problemFile.toString)
      // val programs = SynthesisNPrograms(problem).go()
      val t1 = System.nanoTime

      val programs = SynthesisAllPrograms(problem).go()
      // val programs = BasicSynthesis(problem).go()

      val duration = (System.nanoTime - t1) / 1e9d
      println(s"Finished in ${duration}s")
      displayResults(problem ,programs)
      assert(programs.nonEmpty, s"Test failed: ${problemFile}.")
    }
  }
}


