package synthesis

class ActiveLearning(p0: Problem) {

  def go(): Program = {
    /** Handle one output relation at a time */
    val solutions: Set[Program] = p0.outputRels map {
      rel => interactiveLearning(rel)
    }
    /** Merge solutions altogether */
    solutions.foldLeft(Program())((p1,p2)=>Program(p1.rules++p2.rules))
  }

  def interactiveLearning(outRel: Relation): Program = {
    val relevantIdb: Examples = ???
    var problem = p0.copy(outputRels=Set(outRel), idb=relevantIdb)
    var candidates: Set[Program] = Set()
    var newExamples: Option[(Set[Tuple], Set[Tuple])] = Some((Set(), Set()))

    do {
      // add new examples
      problem = problem.addEdb(newExamples.get._1)
      problem = problem.addIdb(newExamples.get._2)

      candidates = synthesize(problem, outRel)
      newExamples = disambiguate(candidates)
    }
    while (candidates.size > 1 && newExamples.isDefined)

    candidates.maxBy(scoreProgram)
  }

  def synthesize(problem: Problem, outRel: Relation): Set[Program] = {
    SynthesisAllPrograms(problem).go()(outRel)
  }

  def disambiguate(candidates: Set[Program]): Option[(Set[Tuple], Set[Tuple])] = ???

  def scoreProgram(program: Program): Int = {
    /** todo: use better metric. */
    -program.rules.size
  }

}
