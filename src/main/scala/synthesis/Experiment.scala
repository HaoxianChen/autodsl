package synthesis
import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.Logger

import scala.util.Random
import scala.util.parsing.json.JSONObject


case class ExperimentRecord(results: Map[String, Any], program: Program) {
  require(results.contains("exp_name"))
  require(results.contains("problem"))

  def dump(outDir: String = "results"): Unit = {
    val s = JSONObject(results).toString()

    val problemDir = Paths.get(outDir, results("problem").toString)
    Misc.makeDir(problemDir)

    val timestamp: String = Misc.getTimeStamp

    val filename: String = s"${results("exp_name")}_result[$timestamp].log"
    val file = Paths.get(problemDir.toString, filename)
    Misc.writeFile(s, file)

    val solution: String = s"${results("exp_name")}_solution[$timestamp].log"
    val f2 = Paths.get(problemDir.toString, solution)
    Misc.writeFile(program.toString, f2)

  }

}

abstract class Experiment

class ActiveLearningExperiment(maxExamples: Int =100, outDir: String = "results/active-learning") extends Experiment {
  private val logger = Logger("Experiment")

  def go(problem: Problem, repeats: Int = 1): Unit = {
    // Randomly drop one example
    for (i <- 1 to repeats) {
      logger.info(s"iteration $i")
      randomDrop(problem, nDrop=1)
      randomDrop(problem, nDrop=3)
    }
  }

  def randomDrop(problem: Problem, nDrop: Int): (Program, Int, Double) = {
    logger.info(s"Randomly drop ${nDrop} examples.")
    val examples: Set[ExampleInstance] = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)
    val n_remains = examples.size - nDrop
    assert(n_remains > 0, s"${examples.size}")

    val incompleteExamples = sampleExamples(examples, n_remains)
    logger.info(s"${incompleteExamples.size} examples left.")
    val (newEdb, newIdb) = ExampleInstance.toEdbIdb(incompleteExamples)
    val newProblem: Problem = problem.copy(edb=newEdb, idb=newIdb)

    val t1 = System.nanoTime
    val learner = new ActiveLearning(newProblem, maxExamples)
    val (program, nQueries) = learner.go()

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s, ${nQueries} queries.")

    val record = ExperimentRecord(Map("problem"->problem.name,
      "exp_name" -> s"drop_${nDrop}_example",
      "numDrop" -> nDrop,
      "numQuereis" -> nQueries,
      "time"->duration),
      program
    )
    record.dump(outDir)

    (program, nQueries, duration)
  }

  def sampleExamples(examples: Set[ExampleInstance], n_samples: Int, seed: Int =42): Set[ExampleInstance] = {
    val rnd = new Random(seed)
    rnd.shuffle(examples.toList).take(n_samples).toSet
  }
}

class DebloatingExperiment(maxExamples: Int =100, outDir: String = "results/debloat") extends Experiment {
  private val logger = Logger("Debloating")
  def go(problem: Problem, repeats: Int = 1): Unit = {
    for (i <- 1 to repeats) {
      logger.info(s"iteration $i")
      debloat(problem)
    }
  }

  def debloat(problem: Problem): Unit = {
    val t1 = System.nanoTime
    val learner = new ActiveLearning(problem, maxExamples)
    val (program, nQueries) = learner.go()

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s, ${nQueries} queries.")

    val exampleInstances = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)

    val record = ExperimentRecord(Map("problem"->problem.name,
      "exp_name" -> s"random_trace",
      "trace_length" -> exampleInstances.size,
      "numQuereis" -> nQueries,
      "time"->duration),
      program
    )
    record.dump(outDir)
    logger.info(s"Finished in ${duration}s, trace length ${exampleInstances.size}, ${nQueries} queries.")
  }

}
