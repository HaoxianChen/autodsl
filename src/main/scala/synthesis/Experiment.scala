package synthesis
import com.typesafe.scalalogging.Logger
import scala.util.Random

abstract class Experiment

class ActiveLearningExperiment(maxExamples: Int =100) extends Experiment {
  private val logger = Logger("Experiment")

  def go(problem: Problem): Unit = {
    // Randomly drop one example
    randomDrop(problem, nDrop=1)

    randomDrop(problem, nDrop=3)
  }

  def randomDrop(problem: Problem, nDrop: Int): Unit = {
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
    val program = learner.go()

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s")
    println(program)
  }

  def sampleExamples(examples: Set[ExampleInstance], n_samples: Int, seed: Int =42): Set[ExampleInstance] = {
    val rnd = new Random(seed)
    rnd.shuffle(examples.toList).take(n_samples).toSet
  }
}
