package synthesis.experiment

import com.typesafe.scalalogging.Logger
import synthesis.activelearning.{ActiveLearning, ExampleInstance}
import synthesis.experiment.ActiveLearningExperiment.sampleFromSet
import synthesis.experiment.ExperimentRecord.fromFile

import scala.concurrent.ExecutionContext.Implicits.global
import synthesis.util.Misc
import synthesis.{Problem, Program, Relation}

import java.nio.file.{Path, Paths}
import scala.concurrent.duration.MINUTES
import scala.concurrent.{Await, Future, TimeoutException, duration}
import scala.util.{Random, Try}

class ActiveLearningExperiment(benchmarkDir: String, maxExamples: Int = 400, outDir: String = "results/active-learning",
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

    /** Keep looping until all is done. */
    var areAllResultsReady: Boolean = false
    var iters: Int = 0
    while(!areAllResultsReady && iters<10) {
      areAllResultsReady = true
      iters += 1

      for (problemFile <- allProblems) {
        val problem = Misc.readProblem(problemFile.toString)
        val rc = ExperimentRecord.recordCount(outDir, problem, getProblemSignature(problem), nDrop=nDrop)
        if (rc < repeats) {
          areAllResultsReady = false
          logger.info(s"Run ${problem.name} for ${repeats-rc} times.")
          val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(problemFile.toString)
          go(problem,staticConfigRelations,nDrop = nDrop, repeats=repeats-rc)
        }
        else {
          logger.info(s"${problem.name} have $rc results already. Skip.")
        }
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
    val logDir = Paths.get(logRootDir.toString, problem.name)
    Misc.makeDir(logDir)
    // Randomly drop one example
    for (i <- 1 to repeats) {
      logger.info(s"iteration $i")
      // randomDrop(problem, nDrop=1)
      // val ret: Try[Unit] = Try {
      //   randomDrop(problem, staticConfigRelations, nDrop=nDrop, _logDir = logDir.toString)
      // }
      val waitTime = duration.Duration(20,MINUTES)
      val ret = Future {
        randomDrop(problem, staticConfigRelations, nDrop=nDrop, _logDir = logDir.toString)
      }
      try {
        Await.result(ret, waitTime)
      }
      catch {
        case te: TimeoutException => logger.warn(s"$te")
        case e: Exception => logger.warn(s"$e")
      }
    }
  }

  def randomDrop(problem: Problem, staticConfigRelations: Set[Relation], nDrop: Int, _logDir: String): (Program, Int, Double) = {
    logger.info(s"Randomly drop ${nDrop} examples.")
    val examples: Set[ExampleInstance] = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)
    val n_remains = examples.size - nDrop
    assert(n_remains > 0, s"${examples.size}")

    val incompleteExamples = sampleExamples(examples, n_remains)
    logger.info(s"${incompleteExamples.size} examples left.")
    val (newEdb, newIdb) = ExampleInstance.toEdbIdb(incompleteExamples)
    val newProblem: Problem = problem.copy(edb=newEdb, idb=newIdb)

    val t1 = System.nanoTime
    // val logSubDir: Path = Paths.get(logRootDir.toString, problem.name, Misc.getTimeStamp(sep = "-"))
    val logSubDir: Path = Paths.get(_logDir, Misc.getTimeStamp(sep = "-"))
    val learner = new ActiveLearning(newProblem, staticConfigRelations, maxExamples, logDir=logSubDir.toString)
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
      "logDir"->logSubDir
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

  def makeTable(benchmarkDir: String, problemDirs: List[String], resultRootDir: String, outFileName: String): Unit = {
    // var allRecords: List[List[String]] = List()
    var allRecords: List[Map[String,String]] = List()
    val allProblems = problemDirs.map(f=>Paths.get(benchmarkDir,f))
    for (problemFile <- allProblems) {
      val problem = Misc.readProblem(problemFile.toString)
      val resultDir = Paths.get(resultRootDir, problem.name)
      val allFiles: List[String] = Misc.getListOfFiles(resultDir.toString)
      val logFiles: List[String] = allFiles.filter(_.contains("result"))
      val records: List[Map[String,String]] = logFiles.map(f => Paths.get(resultDir.toString,f).toString).
        map(fromFile)
      allRecords ++=  records
    }

    def _strToInt(_s: String): Int = _s match {
      case "true" => 1
      case "false" => 0
      case _ => {
        assert(false)
        -1
      }
    }

    val statLines = allRecords.map { rc =>
      val correctness = _strToInt(rc("correctness") )
      List(rc("problem"), rc("numDrop"), rc("numQuereis"), rc("time"), correctness)
    }.toList

    val aggLines = allRecords.groupBy(rc => (rc("problem"), rc("numDrop"))).map {
      case (k,group) => {
        val N = group.size
        val avgQueries: Double = group.map(_("numQuereis")).map(_.toInt).sum.toDouble / N
        val avgTime: Double = group.map(_("time")).map(_.toDouble).sum.toDouble / N
        val correctRatio: Double = group.map(_("correctness")).map(_strToInt).sum.toDouble / N
        List(k._1, k._2, avgQueries, avgTime, correctRatio, N)
      }
    }.toList

    val rawHeader = List("spec", "numDropExamples", "numQueries", "time", "validated")
    val aggHeader = rawHeader :+ "count"

    val rawFile = Paths.get(resultRootDir, s"${outFileName}_raw.csv")
    Misc.writeFile((rawHeader +: statLines).map(_.mkString("\t")).mkString("\n"), rawFile)
    val aggFile = Paths.get(resultRootDir, s"${outFileName}_agg.csv")
    Misc.writeFile((aggHeader +: aggLines).map(_.mkString("\t")).mkString("\n"), aggFile)
  }

  def makeAllTables(benchmarkDir: String, resultRootDir: String): Unit = {

    makeTable(benchmarkDir,Experiment.activelearningProblems, resultRootDir, "active_learning")

    makeTable(benchmarkDir,Experiment.randomDropExperiments, resultRootDir, "drop_examples")

  }
}

