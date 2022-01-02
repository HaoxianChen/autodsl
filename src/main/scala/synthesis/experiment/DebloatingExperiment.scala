package synthesis.experiment

import com.typesafe.scalalogging.Logger
import synthesis.{Problem, Relation}
import synthesis.activelearning.{ActiveLearning, ExampleInstance}

class DebloatingExperiment(maxExamples: Int =100, outDir: String = "results/debloat")
    extends Experiment(outDir) {
  private val logger = Logger("Debloating")
  def go(problem: Problem, staticConfigRelations: Set[Relation], repeats: Int = 1): Unit = {
    for (i <- 1 to repeats) {
      logger.info(s"iteration $i")
      debloat(problem, staticConfigRelations)
    }
  }

  def debloat(problem: Problem, staticConfigRelations: Set[Relation]): Unit = {
    val t1 = System.nanoTime
    val learner = new ActiveLearning(problem, staticConfigRelations, maxExamples)
    val (program, nQueries, correctness, isTimeOut, hasError) = learner.go()

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s, ${nQueries} queries.")

    val exampleInstances = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)

    if (!hasError) {
      val record = ExperimentRecord(Map("problem"->problem.name,
        "exp_name" -> s"random_trace",
        "trace_length" -> exampleInstances.size,
        "numQuereis" -> nQueries,
        "time"->duration,
        "correctness"->correctness),
        program
      )
      record.dump(outDir)
      logger.info(s"Finished in ${duration}s, trace length ${exampleInstances.size}, ${nQueries} queries.")
    }
  }

}

