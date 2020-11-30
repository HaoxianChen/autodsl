package synthesis

import java.nio.file.Paths

object Main extends App {

  if (args(0)== "parse") {
    val problem = Misc.readProblem(args(1))
    println(problem)
  }
  else if (args(0)== "learn") {
    val problem = Misc.readProblem(args(1))
    // val programs = BasicSynthesis(problem).go()
    val programs = SynthesisNPrograms(problem).go()
    println(programs)
  }
  else if (args(0)== "regression-test") {
    val benchmarkDir = "/Users/hxc/projects/autodsl-bench"
    val allProblems = List("forwarding/learning-switch",
      "nib/reachable"
    ).map(s => Paths.get(benchmarkDir, s))
    for (problemFile <- allProblems) {
      println(problemFile)
      val problem = Misc.readProblem(problemFile.toString)
      // val programs = BasicSynthesis(problem).go()
      val programs = SynthesisNPrograms(problem).go()
      assert(programs.nonEmpty, s"Test failed: ${problemFile}.")
      println(programs)
    }
  }
}


