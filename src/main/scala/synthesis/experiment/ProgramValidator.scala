package synthesis.experiment

import synthesis.activelearning.{EvaluatorWrapper, ExampleGenerator, ExampleInstance, ExamplePool}
import synthesis.{Examples, Problem, Program, Relation, Tuple}
import com.typesafe.scalalogging.Logger

case class ProgramValidator(p0: Problem, staticConfigRelations: Set[Relation], numNewExamples: Int = 10000) {
  private val logger = Logger("Program-validator")

  private val reference = p0.oracleSpec.get

  private val exampleGenerator = new ExampleGenerator(p0.inputRels, staticConfigRelations, p0.edb, p0.idb)
  private val edbPool: ExamplePool = exampleGenerator.generateRandomInputs(numNewExamples)

  def differentiateFromReference(solution: Program, outRels: Set[Relation]= p0.outputRels):
  (Boolean, Option[ExampleInstance]) = {
    val newProblem = p0.copy(edb = edbPool.toExampleMap, idb=Examples())
    val evaluator = EvaluatorWrapper(newProblem)
    val idb = evaluator.eval(solution, outRels)
    val refIdb = evaluator.eval(reference, outRels)

    var optExample: Option[ExampleInstance] = None
    /** Find the next differentiating examples */
    if (idb!=refIdb) {
      logger.debug(s"Static relations: $staticConfigRelations.")
      val idbById: Map[Int,Set[Tuple]] = idb.groupBy(_.fields.last.name.toInt)
      val refById: Map[Int,Set[Tuple]] = refIdb.groupBy(_.fields.last.name.toInt)
      val edbById: Map[Int,Set[Tuple]] = newProblem.edb.toTuples().groupBy(_.fields.last.name.toInt)
      for ((i,out) <- idb.diff(refIdb).groupBy(_.fields.last.name.toInt) ) {
        if (optExample.isEmpty) {
          val ref = refById.getOrElse(i, Set())
          val in = edbById(i)
          optExample = Some(ExampleInstance(in,ref, i))
          logger.debug(s"Differentiating example: $in, $ref, $out.")
        }
      }
      for ((i,ref) <- refIdb.diff(idb).groupBy(_.fields.last.name.toInt) ) {
        if (optExample.isEmpty) {
          val out = idbById.getOrElse(i, Set())
          val in = edbById(i)
          optExample = Some(ExampleInstance(in,ref, i))
          logger.debug(s"Differentiating example: $in, $ref, $out.")
        }
      }
    }
    val validated: Boolean = idb==refIdb
    (validated, optExample)
  }
}
