package synthesis.experiment

import com.typesafe.scalalogging.Logger
import synthesis.activelearning.{ActiveLearning, ExampleInstance}
import synthesis.experiment.ActiveLearningExperiment.sampleFromSet
import synthesis.experiment.Experiment.{checkSolution, getSolution}
import synthesis.util.Misc
import synthesis.{Problem, Program, Relation, Tuple}

import java.nio.file.{Files, Path, Paths}
import scala.util.Random

class ActiveLearningExperiment(benchmarkDir: String, maxExamples: Int = 2000, outDir: String = "results/active-learning",
                               _logRootDir: String = "/var/tmp/netspec/")
    extends Experiment(outDir) {
  private val logger = Logger("ActiveLearningExperiment")

  private val logRootDir = Paths.get(_logRootDir)
  Misc.makeDir(logRootDir)

  def allProblems: List[Path] = Experiment.activelearningProblems.map(s => Paths.get(benchmarkDir, s))
  def randomDropProblems: List[Path] = Experiment.randomDropExperiments.map(s => Paths.get(benchmarkDir, s))

  def runAll(repeats: Int) :Unit = {
    /** Run without droping examples */
    val nDrop: Int = 0
    require(repeats >= 1)
    for (problemFile <- allProblems) {
      val problem = Misc.readProblem(problemFile.toString)
      val rc = ExperimentRecord.recordCount(outDir, problem, getProblemSignature(problem), nDrop=nDrop)
      if (rc < repeats) {
        logger.info(s"Run ${problem.name} for ${repeats-rc} times.")
        val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(problemFile.toString)
        go(problem,staticConfigRelations,nDrop = nDrop, repeats=repeats-rc)
      }
      else {
        logger.info(s"${problem.name} have $rc results already. Skip.")
      }
    }
  }

  def runRandomDrops(repeats: Int, nDrops: List[Int]) :Unit = {
    for (problemFile <- randomDropProblems) {
      val problem = Misc.readProblem(problemFile.toString)
      val sig = getProblemSignature(problem)
      val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(problemFile.toString)
      for (nDrop <- nDrops) {
        /** Execute the remaining runs */
        val rc = ExperimentRecord.recordCount(outDir, problem, sig, nDrop)
        if (rc < repeats) {
          /** Run each dropping point */
          go(problem,staticConfigRelations,nDrop = nDrop, repeats=repeats-rc)
        }
        else {
          logger.info(s"${problem.name} have $rc results already. Skip.")
        }
      }
    }
  }

  def go(problem: Problem, staticConfigRelations: Set[Relation], nDrop: Int ,repeats: Int = 1): Unit = {
    logger.info(s"$repeats runs.")
    // Randomly drop one example
    for (i <- 1 to repeats) {
      logger.info(s"iteration $i")
      // randomDrop(problem, nDrop=1)
      randomDrop(problem, staticConfigRelations, nDrop=nDrop)
    }
  }

  def randomDrop(problem: Problem, staticConfigRelations: Set[Relation], nDrop: Int): (Program, Int, Double) = {
    logger.info(s"Randomly drop ${nDrop} examples.")
    val examples: Set[ExampleInstance] = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)
    val n_remains = examples.size - nDrop
    assert(n_remains > 0, s"${examples.size}")

    val incompleteExamples = sampleExamples(examples, n_remains)
    logger.info(s"${incompleteExamples.size} examples left.")
    val (newEdb, newIdb) = ExampleInstance.toEdbIdb(incompleteExamples)
    val newProblem: Problem = problem.copy(edb=newEdb, idb=newIdb)

    val t1 = System.nanoTime
    val logDir: Path = Paths.get(logRootDir.toString, problem.name, Misc.getTimeStamp(sep = "-"))
    val learner = new ActiveLearning(newProblem, staticConfigRelations, maxExamples, logDir=logDir.toString)
    val (program, nQueries, correctness) = learner.go()

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s, ${nQueries} queries.")

    val record = ExperimentRecord(Map("problem"->problem.name,
      "exp_name" -> s"drop_${nDrop}_example",
      "numDrop" -> nDrop,
      "numQuereis" -> nQueries,
      "time"->duration,
      "sig"->getProblemSignature(problem),
      "correctness"->correctness,
      "logDir"->logDir
    ),
      program
    )
    record.dump(outDir)

    (program, nQueries, duration)
  }

  // def sampleExamples(examples: Set[ExampleInstance], n_samples: Int, seed: Option[Int]=None): Set[ExampleInstance] = {
  def sampleExamples(examples: Set[ExampleInstance], n_samples: Int): Set[ExampleInstance] = {
    require(n_samples > 0 && n_samples <= examples.size, s"${examples.size} $n_samples")

    def _sampleByRelation(examples: Set[ExampleInstance], relation: Relation): ExampleInstance = {
      val positiveExamples = examples.filter(_.outRels.contains(relation))
      require(positiveExamples.nonEmpty)
      sampleFromSet(positiveExamples,n=1).head
    }

    if (n_samples == examples.size) {
      /** no need to sample */
      examples
    }
    else {
      /** Make sure every output relation has at least one positive example.
       * By first sample one tuple for each edb, then sample the rest from the remaining pool of examples.
       * */
      // val rnd = new Random(seed)
      val outRels: Set[Relation] = examples.flatMap(_.output).map(_.relation)
      var newSamples: Set[ExampleInstance] = Set()

      // Sample one example for each output relation first
      for (rel <- outRels) {
        newSamples += _sampleByRelation(examples, rel)
      }

      // Sample the rest of them
      val n_remains = n_samples - newSamples.size
      require(n_remains>=0)
      val remaining = sampleFromSet(examples.diff(newSamples), n_remains)
      newSamples ++ remaining
    }
  }
}

object ActiveLearningExperiment {
  def sampleFromSet[T](set: Set[T], n: Int): Set[T] = {
    // todo: control the seed to random object
    val rnd = new Random()
    rnd.shuffle(set.toList).take(n).toSet
  }
}

