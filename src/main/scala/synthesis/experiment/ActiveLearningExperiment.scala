package synthesis.experiment

import com.typesafe.scalalogging.Logger
import sun.rmi.log.ReliableLog.LogFile
import synthesis.activelearning.{ActiveLearning, ExampleInstance, ExampleTranslator}
import synthesis.experiment.ActiveLearningExperiment.sampleFromSet
import synthesis.experiment.ExperimentRecord.fromFile
import synthesis.util.Misc
import synthesis.{Problem, Program, Relation}

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.util.{Random, Try}

class ActiveLearningExperiment(benchmarkDir: String, maxExamples: Int = 400, outDir: String = "results/active-learning",
                               /** timeout in seconds */
                               timeout: Int= 60 * 60,
                               _logRootDir: String = "/var/tmp/netspec/")
    extends Experiment(outDir) {
  private val logger = Logger("ActiveLearningExperiment")

  private val logRootDir = Paths.get(_logRootDir)
  Misc.makeDir(logRootDir)

  def runAll(repeats: Int) :Unit = run(Experiment.activelearningProblems, repeats)

  def run(problemPaths: List[String], repeats: Int, numDropExamples: List[Int]=List(0)) :Unit = {
    require(repeats >= 1)

    /** Keep looping until all is done. */
    var areAllResultsReady: Boolean = false
    var iters: Int = 0
    while(!areAllResultsReady && iters<10) {
      areAllResultsReady = true
      iters += 1

      for (problemFile <- problemPaths.map(s => Paths.get(benchmarkDir, s))) {
        val problem = Misc.readProblem(problemFile.toString)
        for (nDrop <- numDropExamples) {
          val rc = ExperimentRecord.recordCount(outDir, problem, getProblemSignature(problem), nDrop=nDrop)
          if (rc < repeats) {
            areAllResultsReady = false
            logger.info(s"Run ${problem.name} drop ${nDrop} examples for ${repeats-rc} times.")
            val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(problemFile.toString)
            go(problem,staticConfigRelations,nDrop = nDrop, repeats=repeats-rc)
          }
          else {
            logger.info(s"${problem.name} drop ${nDrop} examples have $rc results already. Skip.")
          }
        }
      }
    }
  }

  // def runRandomDrops(repeats: Int, nDrops: List[Int]) :Unit = {
  //   val randomDropProblems: List[Path] = Experiment.randomDropExperiments.map(s => Paths.get(benchmarkDir, s))
  //   for (problemFile <- randomDropProblems) {
  //     val problem = Misc.readProblem(problemFile.toString)
  //     val sig = getProblemSignature(problem)
  //     val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(problemFile.toString)
  //     for (nDrop <- nDrops) {
  //       /** Execute the remaining runs */
  //       val rc = ExperimentRecord.recordCount(outDir, problem, sig, nDrop)
  //       if (rc < repeats) {
  //         /** Run each dropping point */
  //         go(problem,staticConfigRelations,nDrop = nDrop, repeats=repeats-rc)
  //       }
  //       else {
  //         logger.info(s"${problem.name} have $rc results already. Skip.")
  //       }
  //     }
  //   }
  // }

  def go(problem: Problem, staticConfigRelations: Set[Relation], nDrop: Int ,repeats: Int = 1): Unit = {
    logger.info(s"$repeats runs.")
    val logDir = Paths.get(logRootDir.toString, problem.name)
    Misc.makeDir(logDir)
    // Randomly drop one example
    for (i <- 1 to repeats) {
      logger.info(s"iteration $i")
      randomDrop(problem, staticConfigRelations, nDrop=nDrop, _logDir = logDir.toString)
      // try {
      //   randomDrop(problem, staticConfigRelations, nDrop=nDrop, _logDir = logDir.toString)
      // }
      // catch {
      //   case e: Exception => logger.error(s"$e")
      // }
      // val waitTime = duration.Duration(60,MINUTES)
      // val ret = Future {
      //   randomDrop(problem, staticConfigRelations, nDrop=nDrop, _logDir = logDir.toString)
      // }
      // try {
      //   Await.result(ret, waitTime)
      // }
      // catch {
      //   case te: TimeoutException => logger.warn(s"$te")
      //   case e: Exception => logger.warn(s"$e")
      // }
    }
  }

  def progressLogFile(problem: Problem): Path = Paths.get(outDir, problem.name, s"progress.log")

  def loadProgressFromCache(problem: Problem, _logFile: Path
                           ): (Problem, Int, List[Int],List[Int], Boolean) = {
    def readLogFile(logFile: Path) : (String ,Int, List[Int], List[Int], Boolean) = {
      val src = Source.fromFile(logFile.toString)
      val allLines: List[String] = src.getLines().toList
      src.close()
      def getField(key: String): String = {
        val targetLines = allLines.filter(_.startsWith(key))
        assert(targetLines.size==1)
        targetLines.head.split(":")(1).trim()
      }
      val exampleDir:String = getField("exampleDir")
      val iter0 = getField("iter").toInt
      val queries = getField("queries").split(",").toList.map(_.toInt)
      val durations = getField("durations").split(",").toList.map(_.toInt)
      val correctness = getField("correctness").toBoolean
      (exampleDir, iter0, queries, durations, correctness)
    }

    /** check if progress log exists */
    if (Files.exists(_logFile)) {
      /** Load progress from file */
      val (exampleDir, iter0, queries0, durations0, correctness0) = readLogFile(_logFile)
      val p0 = Misc.readExamples(problem, exampleDir)
      logger.info(s"Load progress cache from ${_logFile}")
      (p0, iter0, queries0, durations0, correctness0)
    }
    else {
      (problem, 0, List(), List(), false)
    }

  }

  def logProgress(logFile: Path, problem: Problem, lastIter:Int,
                  queries: List[Int], durations: List[Int], correctness: Boolean): Unit = {
    if (lastIter > 0) {
      val timestamp = Misc.getTimeStamp(sep = "-")
      val _logDir: Path = Paths.get(logRootDir.toString, problem.name, timestamp)
      Misc.makeDir(_logDir)
      Misc.dumpExamples(problem, _logDir.toString)

      val qStr = queries.mkString(",")
      val dStr = durations.mkString(",")
      val logStr = List(s"exampleDir:${_logDir}",
        s"iter:$lastIter",
        s"queries:$qStr",
        s"durations:$dStr",
        s"correctness:$correctness").mkString("\n")
      Misc.writeFile(logStr, logFile)
      logger.info(s"Log progress at ${logFile}")
    }
  }

  def clearProgressLog(logFile: Path): Unit = {
    if (Files.exists(logFile)) {
      Files.delete(logFile)
      logger.info(s"Remove progress cache at $logFile")
    }
  }

  def randomDrop(problem: Problem, staticConfigRelations: Set[Relation], nDrop: Int, _logDir: String): (Option[Program], Int, Double) = {
    logger.info(s"Randomly drop ${nDrop} examples.")
    val examples: Set[ExampleInstance] = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)
    val n_remains = examples.size - nDrop
    assert(n_remains > 0, s"${examples.size}")

    val incompleteExamples = sampleExamples(examples, n_remains)
    logger.info(s"${incompleteExamples.size} examples left.")
    val (newEdb, newIdb) = ExampleInstance.toEdbIdb(incompleteExamples)
    val newProblem: Problem = problem.copy(edb=newEdb, idb=newIdb)

    // val logSubDir: Path = Paths.get(logRootDir.toString, problem.name, Misc.getTimeStamp(sep = "-"))
    val logSubDir: Path = Paths.get(_logDir, Misc.getTimeStamp(sep = "-"))
    val learner = new ActiveLearning(newProblem, staticConfigRelations, maxExamples, timeout=timeout,
      logDir=logSubDir.toString)

    val progressCache = progressLogFile(problem)
    Misc.makeDir(progressCache.getParent)
    val (_p0, _iter, _queries, _durations, _correctness0) = loadProgressFromCache(newProblem, progressCache)

    val t1 = System.nanoTime
    val (program, nRuns, nQueries, _allDurations, correctness, isTimeOut, hasError, finalProblem) =
      learner.go(_p0,_iter, _queries, _durations)

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s, ${nQueries} queries.")

    if (!hasError) {
      assert(program.isDefined)
      val record = ExperimentRecord(Map("problem"->problem.name,
        "exp_name" -> s"drop_${nDrop}_example",
        "numDrop" -> nDrop,
        "numRuns" -> nRuns,
        "numQuereis" -> nQueries.sum,
        "time"->duration,
        "sig"->getProblemSignature(problem),
        "correctness"->correctness,
        "logDir"->logSubDir,
        "isTimeOut"->isTimeOut,
        "timeout"->timeout
      ),
        program.get,
        nQueries,_allDurations
      )
      record.dump(outDir)
      /** Remove progress log, if any */
      clearProgressLog(progressCache)
    }
    else {
      /** Dump the progress */
      logProgress(progressCache, finalProblem, nRuns, nQueries, _allDurations, correctness)
    }
    (program, nQueries.sum, duration)
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
      val numRuns = rc.getOrElse("numRuns","1")
      List(rc("problem"), rc("numDrop"), rc("numQuereis"), rc("time"), numRuns, correctness)
    }.toList

    val aggLines = allRecords.groupBy(rc => (rc("problem"), rc("numDrop"))).map {
      case (k,group) => {
        val N = group.size
        val avgQueries: Double = group.map(_("numQuereis")).map(_.toInt).sum.toDouble / N
        val avgTime: Double = group.map(_("time")).map(_.toDouble).sum.toDouble / N
        val numRuns: List[Int] = group.map(_.getOrElse("numRuns","1")).map(_.toInt)
        val avgRuns: Double = numRuns.sum.toDouble / N
        val correctRatio: Double = numRuns.count(_==1).toDouble / N
        List(k._1, k._2, avgQueries, avgTime, correctRatio, avgRuns, N)
      }
    }.toList

    val rawHeader = List("spec", "numDropExamples", "numQueries", "time", "validated", "numRuns")
    val aggHeader = rawHeader :+ "count"

    val rawFile = Paths.get(resultRootDir, s"${outFileName}_raw.csv")
    Misc.writeFile((rawHeader +: statLines).map(_.mkString("\t")).mkString("\n"), rawFile)
    val aggFile = Paths.get(resultRootDir, s"${outFileName}_agg.csv")
    Misc.writeFile((aggHeader +: aggLines).map(_.mkString("\t")).mkString("\n"), aggFile)
  }

  def makeAllTables(benchmarkDir: String, resultRootDir: String): Unit = {

    val resultDir = Paths.get(resultRootDir, "active-learning").toString
    makeTable(benchmarkDir,Experiment.activelearningProblems, resultDir, "active-learning")

    // val resultDir2 = Paths.get(resultRootDir, "active-learning-full").toString
    // makeTable(benchmarkDir,Experiment.activelearningProblems, resultDir2, "active-learning-full")

    makeTable(benchmarkDir,Experiment.randomDropExperiments, resultDir, "drop-examples")

  }
}

