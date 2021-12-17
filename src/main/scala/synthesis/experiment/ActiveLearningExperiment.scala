package synthesis.experiment

import com.typesafe.scalalogging.Logger
import synthesis.activelearning.{ActiveLearning, ExampleInstance}
import synthesis.{Problem, Program}

import scala.util.Random

class ActiveLearningExperiment(maxExamples: Int = 400, outDir: String = "results/active-learning") extends Experiment {
  private val logger = Logger("Experiment")

  def go(problem: Problem, nDrop: Int ,repeats: Int = 1): Unit = {
    // Randomly drop one example
    for (i <- 1 to repeats) {
      logger.info(s"iteration $i")
      // randomDrop(problem, nDrop=1)
      randomDrop(problem, nDrop=nDrop)
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

  // def sampleExamples(examples: Set[ExampleInstance], n_samples: Int, seed: Option[Int]=None): Set[ExampleInstance] = {
  def sampleExamples(examples: Set[ExampleInstance], n_samples: Int): Set[ExampleInstance] = {
    // val rnd = new Random(seed)
    val rnd = new Random()
    rnd.shuffle(examples.toList).take(n_samples).toSet
  }
}

