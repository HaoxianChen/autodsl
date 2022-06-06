package synthesis.activelearning

import com.typesafe.scalalogging.Logger
import synthesis._
import synthesis.experiment.ProgramValidator
import synthesis.rulebuilder.InputAggregator
import synthesis.search.{Synthesis, SynthesisConfigSpace}
import synthesis.util.Misc

import scala.concurrent.ExecutionContext.Implicits.global
import java.nio.file.{Path, Paths}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future, TimeoutException}
import scala.math.log
import scala.util.{Failure, Success, Try}

case class TupleInstance(tuples: Set[Tuple], instanceId: Int)
object TupleInstance {
  def apply(instanceId: Int): TupleInstance = new TupleInstance(Set(), instanceId)
}
case class ExampleInstance(input: Set[Tuple], output: Set[Tuple], instanceId: Int) {
  def nonEmpty: Boolean = input.nonEmpty
  def getConstants: Set[Constant] = (input++output).flatMap(_.fields)
  def outRels: Set[Relation] = output.map(_.relation)
}
object ExampleInstance{
  def apply(instanceId: Int): ExampleInstance = new ExampleInstance(Set(), Set(), instanceId)
  def apply(): ExampleInstance = new ExampleInstance(Set(), Set(), -1)
  def fromEdbIdb(edb: Examples, idb: Examples): Set[ExampleInstance] = {
    val translator = new ExampleTranslator(edb.elems.keySet, idb.elems.keySet)

    val edbMap = edb.toTuples().groupBy(translator.getInstanceId)
    val idbMap = idb.toTuples().groupBy(translator.getInstanceId)

    edbMap.keySet.map(i => {
      val edb: Set[Tuple] = edbMap(i)
      val idb: Set[Tuple] = idbMap.getOrElse(i, Set())
      ExampleInstance(edb, idb, i)
    })
  }


  def toEdbIdb(instances: Set[ExampleInstance]): (Examples, Examples) = {
    val inputTuples: Set[Tuple] = instances.flatMap(_.input)
    val outputTuples: Set[Tuple] = instances.flatMap(_.output)

    def tupleToMap(tuples: Set[Tuple]): Map[Relation, Set[List[Constant]]]= tuples.groupBy(t => t.relation) map {
        case (rel, tuples) => (rel, tuples.map(_.fields))
      }

    val edb = Examples(tupleToMap(inputTuples))
    val idb = Examples(tupleToMap(outputTuples))
    (edb, idb)
  }
}

case class ExamplePool(instances: Set[TupleInstance]) {
  require(noDuplicateId())
  def noDuplicateId(): Boolean = {
    val idList = instances.toList.map(_.instanceId)
    idList.distinct.size == idList.size
  }

  def toExampleMap: Examples = {
    val allTuples: Set[Tuple] = instances.flatMap(_.tuples)
    val map: Map[Relation, Set[List[Constant]]] = allTuples.groupBy(_.relation).map {
      case (rel, ts) => rel -> ts.map(_.fields)
    }
    Examples(map)
  }
}

case class EvaluatorWrapper (problem: Problem)  {
  private val preprocessors: Set[InputAggregator] = Synthesis.getPreprocessors(problem)
  private val newProblem = preprocessors.foldLeft(problem)((p1, agg) => agg.preprocess(p1))
  private val evaluator = Evaluator(newProblem)
  def eval(program: Program): Set[Tuple] = evaluator.eval(program)
  def eval(program: Program, outRel: Relation): Set[Tuple] = eval(program, Set(outRel))
  def eval(program: Program, outRels: Set[Relation]): Set[Tuple] = evaluator.eval(program).filter(
    t => outRels.contains(t.relation)
  )
  def eval(programSpec: String, outRels: Set[Relation]): Set[Tuple] = evaluator.eval(programSpec, outRels)
}

class ActiveLearning(p0: Problem, staticConfigRelations: Set[Relation], numNewExamples: Int = 20,
                     maxQueries: Int = 10,
                     /** timeout in seconds */
                     timeout: Int = 60*60,
                     logDir: String = s"/tmp/netspec",
                     _exampleGenerator: Option[ExampleGenerator] = None) {
  private val logger = Logger("Active-learning")
  private val logRootDir: Path = Paths.get(logDir)
  Misc.makeDir(logRootDir)

  private val exampleTranslator = new ExampleTranslator(p0.inputRels, p0.outputRels)
  private val programValidator = ProgramValidator(p0,staticConfigRelations)

  private val exampleGenerator = if (_exampleGenerator.isDefined) _exampleGenerator.get
    else new ExampleGenerator(p0.inputRels, staticConfigRelations, p0.edb, p0.idb)

  private val edbPool: ExamplePool = exampleGenerator.generateRandomInputs(numNewExamples)
  logger.info(s"Example pool size: ${edbPool.instances.size}.")

  private val oracle = p0.oracleSpec.get
  private var configSpace = SynthesisConfigSpace.getConfigSpace(p0)

  /** Returns: Solution, number of runs, number of queries, validated, timeout, error  */
  def go(initProblem: Problem = p0,
          lastIters: Int = 0,
          lastQueries: List[Int] = List(),
          lastDurations: List[Int] = List(),
        ): (Option[Program], Int, List[Int], List[Int], Boolean, Boolean, Boolean, Problem) = {
    interactiveLearningWithOracle(initProblem, lastIters, lastQueries, lastDurations)
  }

  /** Return: solution, number of runs, number of queries at each run, new examples,
   * validated, timeout, error */
  def interactiveLearningWithOracle(initProblem: Problem,
                                    lastIters: Int = 0,
                                    lastQueries: List[Int] = List(),
                                    lastDurations: List[Int] = List(),
                                    reTry: Boolean = false):
  (Option[Program], Int, List[Int], List[Int], Boolean, Boolean, Boolean, Problem)= {
    require(lastQueries.forall(_ <= maxQueries*initProblem.outputRels.size),
      s"Query number too big. Check the cache file. ${lastQueries}.")
    var isValidated = false
    val maxIters = 10
    var problem = initProblem
    var hasTimeOut = false
    var hasError = true
    var solution: Option[Program] = None
    var newExamples: Set[ExampleInstance] = Set()

    var iters = lastIters
    var nQueries: List[Int] = lastQueries
    var durations: List[Int] = lastDurations

    // while (!isValidated && iters < maxIters && !hasError) {
    while (iters < maxIters && hasError) {
      assert(nQueries.size==iters)
      assert(durations.size==iters)

      val (_p, _newExamples, _duration, _timeout, _error) = interactiveLearningAllRels(problem)
      hasTimeOut = _timeout
      hasError = _error

      if (!_error) {
        problem = _newExamples.foldLeft(problem)(exampleTranslator.updateProblem)
        solution = Some(_p)
        newExamples ++= _newExamples
        nQueries :+= _newExamples.size
        durations :+= _duration

        /** Distinguish from oracle and add new example if retry is enabled */
        if (reTry) {
          val (_valid, _optNextExample) = programValidator.differentiateFromReference(_p, problem.outputRels)
          isValidated = _valid
          if (_optNextExample.isDefined) {
            newExamples += _optNextExample.get
            problem = exampleTranslator.updateProblem(problem, _optNextExample.get)
          }
        }

      }

      iters += 1
    }
    val latestProblem = newExamples.foldLeft(initProblem)(exampleTranslator.updateProblem)
    (solution, iters, nQueries, durations, isValidated, hasTimeOut, hasError, latestProblem)
  }

  /** Returns: solution, newExamples, timeout, error */
  def interactiveLearningAllRels(initProblem: Problem): (Program, Set[ExampleInstance], Int, Boolean, Boolean) = {
    /** Handle one output relation at a time */
    var solutions: Set[Program] = Set()
    var problem: Problem = initProblem
    var isTimeOut: Boolean = false
    var hasError: Boolean = false
    var newExamples: Set[ExampleInstance] = Set()
    var duration: Int = 0

    for (rel <- p0.outputRels) {
      if (!hasError) {
        val (sol, _newExamples, _duration, _to, _err) = interactiveLearning(rel, problem)
        if (_to) isTimeOut = true
        if (_err) hasError = true
        solutions += sol
        duration += _duration
        problem = _newExamples.foldLeft(problem)(exampleTranslator.updateProblem)
        newExamples ++= _newExamples
      }
    }
    /** Merge solutions altogether */
    assert(solutions.nonEmpty)
    val finalSolution = solutions.foldLeft(Program())((p1,p2)=>Program(p1.rules++p2.rules))
    (finalSolution, newExamples, duration, isTimeOut, hasError)
  }


  def interactiveLearning(outRel: Relation, initProblem: Problem,
                         ):
                         (Program, Set[ExampleInstance], Int, Boolean, Boolean) = {
    require(initProblem.outputRels.contains(outRel))
    logger.info(s"Solve ${outRel}")
    /** Only do synthesis on one output relation. */
    val relevantIdb: Examples = initProblem.idb.filterByRelation(outRel)
    var problem = initProblem.copy(outputRels=Set(outRel), idb=relevantIdb)

    var candidates: List[Program] = List()
    var nextExample: Option[ExampleInstance] = None
    var newExamples: Set[ExampleInstance] = Set()

    var remainingTime: Int = timeout
    var isTimeOut: Boolean = false
    var hasError: Boolean = false
    logger.info(s"Timeout in $remainingTime seconds.")

    do {
      // add new examples
      if (nextExample.isDefined) {
        problem = exampleTranslator.updateProblem(problem, nextExample.get)
        newExamples += nextExample.get
      }

      logProblem(problem)
      val start = System.nanoTime()
      val (_candidates, _timeout, _error) = trySynthesize(problem, outRel, candidates, remainingTime)
      candidates = _candidates
      isTimeOut = _timeout
      hasError=_error
      val duration: Int = ((System.nanoTime()- start) / 1e9).toInt
      logger.debug(s"Last iteration lasted $duration s, ${newExamples.size} new examples.")
      remainingTime -= duration

      if (candidates.size > 1 && !hasError) {
        nextExample = disambiguate(candidates, outRel)
        if (nextExample.isEmpty) logger.debug(s"Failed to differentiate candidate programs.")
        else logger.debug(s"new example: ${nextExample.get}")
      }
    }
    while (candidates.size > 1 && nextExample.isDefined && newExamples.size < maxQueries &&
        remainingTime > 10 && !hasError)

    if (newExamples.size >= maxQueries) logger.warn(s"Stopped at max queries ${maxQueries}.")
    if (isTimeOut) logger.warn(s"Timeout after $timeout seconds.")
    val totalDuration = timeout - remainingTime

    if (candidates.nonEmpty && !hasError) {
      val bestProgram = candidates.maxBy(scoreProgram)
      logger.info(s"Solution: $bestProgram")
      (bestProgram, newExamples, totalDuration, isTimeOut, hasError)
    }
    else {
      logger.warn(s"No solution found.")
      (Program(), newExamples, totalDuration, isTimeOut, hasError)
    }
  }

  def logProblem(problem: Problem, errorMessage: String = s""): Path = {
  // def logProblem(problem: Problem): Path = {
    val evaluator = Evaluator(problem)
    val timestamp = Misc.getTimeStamp()
    val _logDir: Path = Paths.get(logRootDir.toString, timestamp)
    Misc.makeDir(_logDir)
    evaluator.dumpProgram(problem.oracleSpec.get, _logDir)
    val errLog = Paths.get(_logDir.toString, s"error.log")
    Misc.writeFile(errorMessage, errLog)
    logger.info(s"Problem log at ${_logDir}")
    _logDir
  }

  def trySynthesize(problem: Problem, outRel: Relation, _candidates: List[Program],
                    timeout: Int,
                    maxRetries: Int = 3,
                    ):
  (List[Program], Boolean, Boolean) = {
    var candidates: List[Program] = _candidates

    var isTimeOut: Boolean = false
    var hasError: Boolean = false

    var iters: Int = 0

    do {
      hasError = false
      val candidatesFuture = Future {
        synthesize(problem, outRel, candidates)
      }
      Try(Await.result(candidatesFuture, Duration(timeout,SECONDS))) match {
        case Success(value) => {
          candidates = value
          logger.debug(s"${candidates.size} candidate programs")
        }
        case Failure(exception) => {
          exception match {
            case _: TimeoutException => {
              isTimeOut = true
            }
            case _: Exception => {
              hasError = true
              logger.error(s"$exception")
              logProblem(problem, exception.getStackTrace.mkString("\n"))
            }
          }
        }
      }
      iters += 1
    } while (iters<maxRetries && hasError)

    (candidates, isTimeOut, hasError)
  }

  def synthesize(problem: Problem, outRel: Relation, candidates: List[Program], minPrograms: Int = 50): List[Program] = {
    val evaluator = EvaluatorWrapper(problem)
    val maxNewCandidates = 100

    def isProgramValid(program: Program, problem: Problem): Boolean = {
      val idb = evaluator.eval(program,outRel)
      val refIdb = problem.idb.toTuples()

      val allOutRels = program.rules.map(_.head.relation)
      val relevantIdb = refIdb.filter(t => allOutRels.contains(t.relation))

      val isValid = idb == relevantIdb
      isValid
    }

    val validCandidates: List[Program] = candidates.filter(p=>isProgramValid(p, problem))

    // todo: Why these assertions fail?
    require(validCandidates.size < candidates.size || candidates.isEmpty, s"${validCandidates.size}")
    val inValidCandidates = candidates.diff(validCandidates)
    assert(inValidCandidates.nonEmpty || candidates.isEmpty)

    if (validCandidates.size < minPrograms) {
      // val synthesizer = SynthesisAllPrograms(problem, initConfigSpace = configSpace)
      val synthesizer = Synthesis(problem, initConfigSpace = configSpace)
      // Keep at most 100 new candidates
      val newCandidates = synthesizer.go()(outRel).take(maxNewCandidates)
      configSpace = synthesizer.getConfigSpace
      require(newCandidates.forall(p=>isProgramValid(p, problem)))
      validCandidates ++ newCandidates
    }
    else {
      validCandidates
    }
  }

  def differentiate(candidates: List[Program], outRel: Relation): Option[TupleInstance] = {
    require(candidates.size > 1)
    def entropy(tuples: List[TupleInstance]): Double = {
      val counts: List[Int] = tuples.groupBy(identity).map{
        case (_, l) => l.size
      }.toList
      val hist: List[Double] = counts.map(c => 1.0 * c / counts.sum)
      hist.map(p => - p * log(p)).sum
    }
    assert(edbPool.instances.size == numNewExamples)

    val evalResults: Set[(TupleInstance, List[TupleInstance])] = evalCandidatePrograms(edbPool, candidates, outRel)
    val edbEntropy: Set[(TupleInstance, Double)] = evalResults.map {
      case (t1, ts) => (t1, entropy(ts))
    }
    val bestEdb = edbEntropy.maxBy(_._2)

    if (bestEdb._2 > 0) {
      logger.debug(s"New edb: $bestEdb")
      Some(bestEdb._1)
    }
    else None
  }

  def evalCandidatePrograms(edbPool: ExamplePool, candidates: List[Program], outRel:Relation): Set[(TupleInstance, List[TupleInstance])] = {
    val newProblem = p0.copy(edb = edbPool.toExampleMap, idb=Examples())
    def evalProgram(program: Program): Map[Int, TupleInstance] = {
      // val evaluator = Evaluator(newProblem)
      val evaluator = EvaluatorWrapper(newProblem)
      // val idb = evaluator.eval(program)
      val idb = evaluator.eval(program,outRel = outRel)
      idb.groupBy(exampleTranslator.getInstanceId) map {
        case (i,ts) => i -> TupleInstance(ts, i)
      }
    }

    val evalResults: List[Map[Int, TupleInstance]] = candidates.map(evalProgram)

    def getResults(instance: TupleInstance): List[TupleInstance] = {
      val tid = instance.instanceId
      evalResults.map(m => m.getOrElse(tid, TupleInstance(tid)))
    }
    edbPool.instances.map(t => (t, getResults(t)))
  }

  def disambiguate(candidates: List[Program], outRel: Relation): Option[ExampleInstance] = {
    require(candidates.size > 1)
    val nextEdb = differentiate(candidates,outRel)
    if (nextEdb.isDefined) {
      val edb = nextEdb.get
      val edbPool = ExamplePool(Set(edb))
      val problem = p0.copy(edb=edbPool.toExampleMap)

      // val evaluator = Evaluator(problem)
      val evaluator = EvaluatorWrapper(problem)
      // val nextIdb = evaluator.eval(oracle, Set(outRel))
      val nextIdb = evaluator.eval(oracle, p0.outputRels)

      Some(new ExampleInstance(edb.tuples, nextIdb, edb.instanceId))
    }
    else {
      None
    }
  }

  def scoreProgram(program: Program): Int = {
    /** todo: use better metric. */
    -program.rules.size
  }

}

