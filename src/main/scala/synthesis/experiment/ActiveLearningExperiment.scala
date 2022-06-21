package synthesis.experiment

import com.typesafe.scalalogging.Logger
import synthesis.activelearning.{ActiveLearning, ExampleGenerator, ExampleInstance, ExampleTranslator}
import synthesis.experiment.ActiveLearningExperiment.sampleFromSet
import synthesis.experiment.ExperimentRecord.{fromFile, recordsBySamples}
import synthesis.util.Misc
import synthesis.{Examples, Problem, Program, Relation}

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.util.Random

class ActiveLearningExperiment(benchmarkDir: String, maxExamples: Int = 100000, outDir: String = "results/active-learning",
                               /** timeout in seconds */
                               timeout: Int= 60 * 60,
                               _logRootDir: String = "/tmp/netspec/")
    extends Experiment(outDir) {
  private val logger = Logger("ActiveLearningExperiment")

  logger.info(s"max examples: $maxExamples.")

  private val logRootDir = Paths.get(_logRootDir)
  Misc.makeDir(logRootDir)
  Misc.makeDir(Paths.get(outDir))

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

  /** Change the number of random samples in active learning,
   *  Measure the number of success benchmarks
   * */
  def tuneSampleParameter(problemPaths: List[String], repeats: Int, numSamples: List[Int]): Unit = {
    require(repeats >= 1)

    /** Keep looping until all is done. */
    var areAllResultsReady: Boolean = false
    var iters: Int = 0
    while(!areAllResultsReady && iters<10) {
      areAllResultsReady = true
      iters += 1

      for (problemFile <- problemPaths.map(s => Paths.get(benchmarkDir, s))) {
        val problem = Misc.readProblem(problemFile.toString)

        val logDir = Paths.get(logRootDir.toString, problem.name).toString

        var isStop: Boolean = false
        var hasTimeOut: Boolean = false

        for (nSamples <- numSamples) {

          /** If consistently succeed with smaller sample number */

          if (!isStop) {
            val records = ExperimentRecord.recordsBySamples(outDir,problem,getProblemSignature(problem),nSamples=nSamples)
            var nSuccessRuns: Int = records.count(_("correctness")=="true")
            // var nFailedRuns: Int = records.count(_("correctness")=="false")
            var nFailedRuns: Int = records.count(r=>r("correctness")=="false"&&r("isTimeOut")=="false")
            hasTimeOut = records.exists(_("isTimeOut")=="true")
            // assert(nSuccessRuns + nFailedRuns == records.size)
            val rc = records.size

            if (rc < repeats && nFailedRuns <= 0) {
              areAllResultsReady = false
              logger.info(s"Run ${problem.name} with ${nSamples} random samples for ${repeats-rc} times.")

              val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(problemFile.toString)

              /** how many examples can be sustain? */

              // for (i <- 1 to repeats-rc) {
              var i = 0
              while (i < repeats - rc && nFailedRuns <= 0) {
                logger.info(s"iteration $i")
                val (_,_,_,_isTimeOut,_correctness) = runActiveLearning(problem, staticConfigRelations, nDrop = 0, logDir, getProblemSignature(problem),
                  _exampleGenerator = None, _maxExamples = nSamples)

                if (_correctness) nSuccessRuns += 1
                else if (!_isTimeOut) nFailedRuns += 1

                if (_isTimeOut) hasTimeOut = true

                i += 1
              }
            }
            else if (rc >= repeats) {
              logger.info(s"${problem.name} with ${nSamples} random examples have $rc results already. Skip.")
            }

            if (nFailedRuns > 0) logger.info(s"${problem.name} with ${nSamples} random examples has ${nFailedRuns} failed runs already. Skip.")

            isStop = (nSuccessRuns == repeats) || (nFailedRuns <= 0 && hasTimeOut)

          }
        }
      }
    }
  }

  def runRandomDrops(repeats: Int) :Unit = {
    val randomDropProblems: List[Path] = Experiment.randomDropExperiments.map(s => Paths.get(benchmarkDir, s))

    var allFinish = true
    var retries = 0

    do {
      allFinish = true
      for (problemFile <- randomDropProblems) {

        val initProblem = Misc.readProblem(problemFile.toString)

        val problemOutDir = Paths.get(outDir, initProblem.name)
        Misc.makeDir(problemOutDir)

        val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(problemFile.toString)
        val initExamples: Set[ExampleInstance] = ExampleInstance.fromEdbIdb(initProblem.edb, initProblem.idb)

        val programValidator = ProgramValidator(initProblem, staticConfigRelations)

        /** Use all original example to build example pool. */
        val exampleGenerator = new ExampleGenerator(initProblem.inputRels, staticConfigRelations,
          initProblem.edb, initProblem.idb)

        def nextProblemAndExamples(prevProblem: Problem, initExamples: Set[ExampleInstance],
                                   nDrop: Int, step: Int, runId: Int): (Problem, Set[ExampleInstance]) = {
          val exampleDir = Paths.get(problemOutDir.toString, s"drop_${nDrop}_examples_run${runId}")
          if (Files.notExists(exampleDir)) {
            /** Sample from previous examples */
            Misc.makeDir(exampleDir)

            val prevExampleDir = Paths.get(problemOutDir.toString, s"drop_${nDrop-step}_examples_run${runId}")
            val prevExamples = if (Files.exists(prevExampleDir)) {
              val p0 = initProblem.copy(edb = Examples(), idb = Examples())
              val next_problem = Misc.readExamples(p0, prevExampleDir.toString)
              logger.debug(s"read previous examples from $prevExampleDir")
              ExampleInstance.fromEdbIdb(next_problem.edb, next_problem.idb)
            }
            else initExamples

            val nextExampleSize = initExamples.size - nDrop
            assert(nextExampleSize < prevExamples.size)
            assert(nextExampleSize > 0)
            val next_examples = sampleExamples(prevExamples, nextExampleSize)
            val (newEdb, newIdb) = ExampleInstance.toEdbIdb(next_examples)
            val next_problem = prevProblem.copy(edb = newEdb, idb = newIdb)
            Misc.dumpExamples(next_problem, exampleDir.toString)
            (next_problem, next_examples)
          }
          else {
            /** Load from file instead */
            val p0 = initProblem.copy(edb = Examples(), idb = Examples())
            val next_problem = Misc.readExamples(p0, exampleDir.toString)
            val next_examples = ExampleInstance.fromEdbIdb(next_problem.edb, next_problem.idb)
            assert(next_examples.size == initExamples.size - nDrop)
            assert(next_examples.subsetOf(initExamples))
            logger.info(s"Load examples from ${exampleDir}")
            (next_problem, next_examples)
          }
        }

        val minExampleSize: Int = initProblem.outputRels.size
        val range = initExamples.size - minExampleSize
        val step: Int = if (range > 5) range / 5 else 1
        val allExampleSizes = (initExamples.size-1 to minExampleSize by -step).toList
        for (nExamples <- allExampleSizes) {
          for (i <- 0 until repeats) {
            var problem = initProblem
            var examples = initExamples

            logger.info(s"Init example size ${initExamples.size}, Output relations: ${problem.outputRels.size} " +
              s",all example sizes: $allExampleSizes.")
            val nDrop = initExamples.size - nExamples
            val (pnext, enext) = nextProblemAndExamples(problem, examples, nDrop, step, i)
            assert(enext.subsetOf(examples))
            assert(enext.size < examples.size)
            assert(enext.size == initExamples.size - nDrop)
            problem = pnext
            examples = enext

            // val sig = getProblemSignature(problem)
            val sig = getProblemSignature(problem) + i // different runs may share the same set of examples
            val rc = ExperimentRecord.recordCount(outDir, initProblem, sig, nDrop = nDrop)
            if (rc < 1) {
              allFinish = false
              logger.info(s"Run ${initProblem.name}, drop ${nDrop} example, run ${i}.")

              val logDir = Paths.get(logRootDir.toString, problem.name)
              runActiveLearning(problem, staticConfigRelations, nDrop = nDrop, logDir.toString, sig,
                _exampleGenerator = Some(exampleGenerator), _programValidator = Some(programValidator))
            }
            else {
              logger.info(s"${initProblem.name} drop ${nDrop} run ${i} has results already. Skip.")
            }
          }
        }
      }
      retries +=1
    } while (retries < 10 && !allFinish)
  }

  def go(problem: Problem, staticConfigRelations: Set[Relation], nDrop: Int ,repeats: Int = 1): Unit = {
    logger.info(s"$repeats runs.")
    val logDir = Paths.get(logRootDir.toString, problem.name)
    Misc.makeDir(logDir)
    // Randomly drop one example
    for (i <- 1 to repeats) {
      logger.info(s"iteration $i")
      randomDrop(problem, staticConfigRelations, nDrop=nDrop, _logDir = logDir.toString)
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

  def randomDrop(problem: Problem, staticConfigRelations: Set[Relation], nDrop: Int, _logDir: String):
      (Option[Program], Int, Double, Boolean, Boolean) = {
    logger.info(s"Randomly drop ${nDrop} examples.")
    val examples: Set[ExampleInstance] = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)
    val n_remains = examples.size - nDrop
    assert(n_remains > 0, s"${examples.size}")

    /** Use all original example to build example pool. */
    val exampleGenerator = new ExampleGenerator(problem.inputRels, staticConfigRelations,
      problem.edb, problem.idb)

    val incompleteExamples = sampleExamples(examples, n_remains)
    logger.info(s"${incompleteExamples.size} examples left.")
    val (newEdb, newIdb) = ExampleInstance.toEdbIdb(incompleteExamples)
    val newProblem: Problem = problem.copy(edb=newEdb, idb=newIdb)
    runActiveLearning(newProblem, staticConfigRelations, nDrop, _logDir, getProblemSignature(problem),
      _exampleGenerator = Some(exampleGenerator))
  }

  /** returns (program, nQueries.sum, duration, isTimeOut, correctness) */
  def runActiveLearning(problem: Problem, staticConfigRelations: Set[Relation], nDrop: Int, _logDir: String,
                        sig: Int,
                       _exampleGenerator: Option[ExampleGenerator]=None,
                        _programValidator: Option[ProgramValidator]=None,
                        _maxExamples: Int = maxExamples): (Option[Program], Int, Double, Boolean, Boolean) = {
    Misc.makeDir(Paths.get(_logDir))

    // val logSubDir: Path = Paths.get(logRootDir.toString, problem.name, Misc.getTimeStamp(sep = "-"))
    val logSubDir: Path = Paths.get(_logDir, Misc.getTimeStamp(sep = "-"))
    val learner = new ActiveLearning(problem, staticConfigRelations, _maxExamples, timeout=timeout,
      logDir=logSubDir.toString, _exampleGenerator=_exampleGenerator, _programValidator=_programValidator)

    val progressCache = progressLogFile(problem)
    Misc.makeDir(progressCache.getParent)
    val (_p0, _iter, _queries, _durations, _correctness0) = loadProgressFromCache(problem, progressCache)

    val t1 = System.nanoTime
    val (program, nRuns, nQueries, _allDurations, correctness, isTimeOut, hasError, finalProblem) =
      learner.go(_p0,_iter, _queries, _durations)

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s, ${nQueries} queries.")

    if (!hasError) {
      assert(program.isDefined)
      val record = ExperimentRecord(Map("problem"->problem.name,
        // "exp_name" -> s"drop_${nDrop}_example",
        "exp_name" -> s"drop_${nDrop}_example_${_maxExamples}_samples",
        "numDrop" -> nDrop,
        "numRuns" -> nRuns,
        "numQuereis" -> nQueries.sum,
        "time"->duration,
        "sig"->sig,
        "correctness"->correctness,
        "logDir"->logSubDir,
        "isTimeOut"->isTimeOut,
        "timeout"->timeout,
        "maxExamples" -> _maxExamples,
      ),
        program.get,
        nQueries=_queries, durations = _allDurations
      )
      record.dump(outDir)
      /** Remove progress log, if any */
      clearProgressLog(progressCache)
    }
    else {
      /** Dump the progress */
      logProgress(progressCache, finalProblem, nRuns, nQueries, _allDurations, correctness)
    }
    (program, nQueries.sum, duration, isTimeOut, correctness)
  }


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
      // val allFiles: List[String] = Misc.getListOfFiles(resultDir.toString)
      // val logFiles: List[String] = allFiles.filter(_.contains("result"))
      // val records: List[Map[String,String]] = logFiles.map(f => Paths.get(resultDir.toString,f).toString).
      //   map(fromFile)
      allRecords ++=  ExperimentRecord.allRecords(resultRootDir, problem)
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
      List(rc("problem"), rc("numDrop"), rc("numQuereis"), rc("time"), correctness, numRuns)
    }.toList
    val rawHeader = List("spec", "numDropExamples", "numQueries", "time", "validated", "numRuns")

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
    val aggHeader = rawHeader :+ "count"

    val rawFile = Paths.get(resultRootDir, s"${outFileName}_raw.csv")
    Misc.writeFile((rawHeader +: statLines).map(_.mkString("\t")).mkString("\n"), rawFile)
    val aggFile = Paths.get(resultRootDir, s"${outFileName}_agg.csv")
    Misc.writeFile((aggHeader +: aggLines).map(_.mkString("\t")).mkString("\n"), aggFile)
  }

  def makeAllTables(benchmarkDir: String, resultRootDir: String): Unit = {

    val resultDir = Paths.get(resultRootDir, "active-learning").toString
    makeTable(benchmarkDir,Experiment.activelearningProblems, resultDir, "active-learning")

    val resultDir2 = Paths.get(resultRootDir, "active-learning-full").toString
    makeTable(benchmarkDir,Experiment.activeLearningWithOracle, resultDir2, "active-learning")

    val resultDir3 = Paths.get(resultRootDir, "debloat").toString
    makeTable(benchmarkDir,Experiment.debloatingExperiments, resultDir3, "debloat")

    val resultDir4 = Paths.get(resultRootDir, "random-drop").toString
    makeTable(benchmarkDir,Experiment.randomDropExperiments, resultDir4, "drop-examples")

  }
}

