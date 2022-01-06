package synthesis

import java.nio.file.{Files, Path, Paths}

import com.typesafe.scalalogging.Logger
import synthesis.util.Misc

case class Evaluator(problem: Problem) {
  private var cache: Map[Program, Examples] = Map()
  private val logger = Logger(s"Evaluator")

  private val name = problem.name
  private val types = problem.types
  private val inputRels = problem.inputRels
  private val originalOutRels = problem.outputRels
  private val edb = problem.edb

  // Prepare tmp directory
  // private val tmpdir = Paths.get("/var/tmp/souffle/")
  private val tmpdir = Paths.get("/tmp/souffle/")
  if (Files.notExists(tmpdir)) tmpdir.toFile.mkdir()

  def _eval(program: Program): Examples = {
    if (program.rules.isEmpty) Examples.empty()
    // check if results are cached
    else if (cache.contains(program)) cache(program)
    else {
      // val specStr = getSpecString(program)
      val outRels: Set[Relation] = getOutRels(program)
      // val idb = _eval(specStr, outRels)

      // make temporary directory
      // val dirName: String = s"${name}_${program.hashCode()}_${edb.hashCode()}"
      val dirName: String = s"${name}_${program.hashCode()}"
      val problemDir = Paths.get(tmpdir.toString, dirName)
      Misc.makeDir(problemDir)

      // dump problem to file
      val programPath = dumpProgram(program, problemDir)

      // run souffle to derive output
      // runSouffle(problemDir, programPath)

      // // load results from file
      // val idb: Examples = loadOutput(problemDir, outRels)
      val idb = runAndGetResults(problemDir, programPath, outRels)

      // update cache
      cache = cache.updated(program, idb)

      idb
    }
  }

  def _eval(programSpec: String, outRels: Set[Relation]): Examples = {
    // make temporary directory
    val dirName: String = s"${name}_${programSpec.hashCode()}"
    val problemDir = Paths.get(tmpdir.toString, dirName)
    Misc.makeDir(problemDir)

    // dump program and edb to files
    val programPath: Path  = dumpProgram(programSpec, problemDir)

    // runSouffle(problemDir, programPath)
    // val idb: Examples = loadOutput(problemDir, outRels)
    val idb = runAndGetResults(problemDir, programPath, outRels)
    idb
  }

  def runAndGetResults(problemDir: Path, programPath: Path, outRels: Set[Relation]): Examples = {
    var i = 0
    var success = false
    var idb = Examples()
    while (i < 3 && !success) {
      val runSucess = runSouffle(problemDir, programPath)
      if (runSucess) {
        val (_idb, _success) = loadOutput(problemDir, outRels)
        idb = _idb
        success = _success
      }
      i += 1
    }
    if (!success) logger.warn(s"Failed to get results after $i trails: $problemDir")
    idb
  }

  def runSouffle(problemDir: Path, programPath: Path): Boolean = {
    val cmd = s"souffle ${programPath.toString} -F ${problemDir.toString} -D ${problemDir.toString}"
    // val timeOut = 100 // timout after 100 milliseconds
    val timeOut = 500 // timout after 500 milliseconds
    val (exitcode, stdout, stderr, isTimeOut) = Misc.runWithTimeOut(cmd, timeOut)
    if (isTimeOut) {
      logger.warn(s"$cmd time out after $timeOut ms.")
    }
    else {
      require(exitcode == 0, s"Non-zero exit value: ${problemDir}\n$exitcode,\n$stdout,\n$stderr")
    }
    require(!stderr.contains("Error"), s"$programPath, $stderr")
    val success: Boolean = !isTimeOut && (exitcode==0)
    success
  }


  def eval(program: Program): Set[Tuple] = _eval(program).toTuples()
  def eval(programSpec: String, outRels: Set[Relation]): Set[Tuple] = _eval(programSpec, outRels).toTuples()

  def dumpProgram(program: Program, dir: Path): Path = {
    val specStr = getSpecString(program)
    dumpProgram(specStr,dir)
  }

  def dumpProgram(specStr: String, dir: Path) : Path = {
    val programPath = Paths.get(dir.toString, s"$name.dl")
    Misc.writeFile(specStr, programPath)

    // Write Input examples
    // for (rel <- edb.elems.keys) {
    for (rel <- problem.inputRels) {
      val str = if (edb.elems.contains(rel)) {
         edb.toFileStr(rel)
      }
      else {
        s""
      }
      Misc.writeFile(str, Paths.get(dir.toString, s"${rel.name}.facts"))
    }
    programPath
  }

  def relDeclString(relation: Relation): String = {
    val sig_str: String = {
      val s: List[String] = for ((t,i) <- relation.signature.zipWithIndex) yield {
        t match {
          case _: NumberType => s"x$i:number"
          case _: SymbolType => s"x$i:symbol"
        }
      }
      s.mkString(",")
    }
    s".decl ${relation.name}($sig_str)"
  }

  def getSpecString(program: Program): String = {
    // type specification
    // val typeSpec = types.map(_.declString).mkString("\n")

    // input relation specification
    val inputRelSpec = inputRels.map(relDeclString).mkString("\n")
    val inputDecl = inputRels.map(rel => s".input ${rel.name}").mkString("\n")

    // output relation specification
    val outRels = getOutRels(program)
    val outputRelSpec = outRels.map(relDeclString).mkString("\n")
    val outputDecl = outRels.map(rel => s".output ${rel.name}").mkString("\n")

    // val programStr = List(typeSpec, inputRelSpec, outputRelSpec, inputDecl, program.toString,
    val programStr = List(inputRelSpec, outputRelSpec, inputDecl, program.toString,
      outputDecl).mkString("\n")
    programStr
  }


  def getOutRels(program: Program): Set[Relation] = {
    val ruleBodyRels = program.getAllRelations.intersect(originalOutRels)
    val ruleHeadRels = program.rules.map(_.head.relation)
    ruleHeadRels ++ ruleBodyRels
  }

  def loadOutput(dir: Path, outRels: Set[Relation]): (Examples, Boolean) = {
    var idb = Examples()
    var success = true
    for (rel <- outRels) {
      val outFile = Paths.get(dir.toString, s"${rel.name}.csv")

      // wait until file is written
      // var i = 0
      // while (i < 10 && Files.notExists(outFile)) {
      //   Thread.sleep(100) // sleep for 100 milliseconds
      //   i+=1
      // }
      // require(Files.exists(outFile), s"output file not produced ${outFile} after $i trails.")
      if (Files.exists(outFile)) {
        val facts = Misc.readCsv(outFile.toString)
        var tuples: Set[List[Constant]] = Set()
        try {
          tuples = facts.map(Examples.strToTuple(rel, _)).toSet
        }
        catch {
          case _: Exception => logger.error(s"failed to load tuples from ${outFile}.")
            assert(false)
        }
        idb = idb.addTuples(rel, tuples)
     }
      else {
        // logger.warn(s"output file not produced ${outFile} after $i trails.")
        logger.warn(s"output file ${outFile} not found.")
        success = false
      }
    }
    (idb, success)
  }
}
