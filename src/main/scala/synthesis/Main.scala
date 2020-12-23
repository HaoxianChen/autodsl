package synthesis

import java.nio.file.Paths

object Main extends App {

  def displayResults(programs: Map[Relation, Set[Program]]) = {
    for ((rel,ps)<-programs) {
      println(s"$rel: ${ps.size} programs:")
      for (p <- ps) {
        println(p+"\n")
      }
    }
  }

  if (args(0)== "parse") {
    val problem = Misc.readProblem(args(1))
    println(problem)
  }
  else if (args(0)== "learn") {
    val problem = Misc.readProblem(args(1))
    val programs = SynthesisAllPrograms(problem).go()
    displayResults(programs)
  }
  else if (args(0) == "active") {
    val problem = Misc.readProblem(args(1))
    val maxExamples: Int = args(2).toInt
    val learner = new ActiveLearning(problem, maxExamples)
    val program = learner.go()
    println(program)
  }
  else if (args(0)== "regression-test") {
    val benchmarkDir = "/Users/hxc/projects/autodsl-bench"
    val allProblems = List("forwarding/learning-switch",
      "nib/reachable",
      "firewall/stateless-firewall"
    ).map(s => Paths.get(benchmarkDir, s))
    for (problemFile <- allProblems) {
      println(problemFile)
      val problem = Misc.readProblem(problemFile.toString)
      // val programs = BasicSynthesis(problem).go()
      // val programs = SynthesisNPrograms(problem).go()
      val programs = SynthesisAllPrograms(problem).go()
      displayResults(programs)
      assert(programs.nonEmpty, s"Test failed: ${problemFile}.")
    }
  }
}


