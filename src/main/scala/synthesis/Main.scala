package synthesis

object Main extends App {

  if (args(0)== "parse") {
    val problem = Misc.readProblem(args(1))
    println(problem)
  }
  else if (args(0)== "learn") {
    val problem = Misc.readProblem(args(1))
    val programs = BasicSynthesis(problem).go()
    println(programs)
  }
}


