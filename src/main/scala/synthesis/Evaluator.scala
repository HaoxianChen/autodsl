package synthesis

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}

import scala.collection.mutable
import sys.process._

case class Evaluator(problem: Problem) {
  private var cache: Map[Program, Examples] = Map()

  private val name = problem.name
  private val types = problem.types
  private val inputRels = problem.inputRels
  private val originalOutRels = problem.outputRels
  private val edb = problem.edb

  // Prepare tmp directory
  private val tmpdir = Paths.get("tmp")
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
      makeDir(problemDir)

      // dump problem to file
      val programPath = dumpProgram(program, problemDir)

      // run souffle to derive output
      runSouffle(problemDir, programPath)

      // load results from file
      val idb: Examples = loadOutput(problemDir, outRels)

      // update cache
      cache = cache.updated(program, idb)

      idb
    }
  }

  def _eval(programSpec: String, outRels: Set[Relation]): Examples = {
    // make temporary directory
    val dirName: String = s"${name}_${programSpec.hashCode()}"
    val problemDir = Paths.get(tmpdir.toString, dirName)
    makeDir(problemDir)

    // dump program and edb to files
    val programPath: Path  = dumpProgram(programSpec, problemDir)

    runSouffle(problemDir, programPath)
    val idb: Examples = loadOutput(problemDir, outRels)
    idb
  }

  def makeDir(problemDir: Path): Unit = {
    if (Files.notExists(problemDir)) {
      if (!problemDir.toFile.mkdir()) {
        throw new RuntimeException(s"Failed to create problem tmp directory ${problemDir.toString}.")
      }
    }
  }

  def runSouffle(problemDir: Path, programPath: Path): Unit = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val cmd = s"souffle ${programPath.toString} -F ${problemDir.toString} -D ${problemDir.toString}"
    val exitcode = cmd ! ProcessLogger(stdout append _, stderr append _)
    require(exitcode == 0, s"Non-zero exit value: ${problemDir}\n$exitcode,\n$stdout,\n$stderr")
    require(!stderr.contains("Error"))
  }


  def eval(program: Program): Set[Tuple] = _eval(program).toTuples()
  def eval(programSpec: String, outRels: Set[Relation]): Set[Tuple] = _eval(programSpec, outRels).toTuples()

  def dumpProgram(program: Program, dir: Path): Path = {
    val specStr = getSpecString(program)
    dumpProgram(specStr,dir)
  }

  def dumpProgram(specStr: String, dir: Path) : Path = {
    val programPath = Paths.get(dir.toString, s"$name.dl")
    _writeFile(specStr, programPath)

    // Write Input examples
    for (rel <- edb.elems.keys) {
      val str = edb.toFileStr(rel)
      _writeFile(str, Paths.get(dir.toString, s"${rel.name}.facts"))
    }
    programPath
  }

  def getSpecString(program: Program): String = {
    // type specification
    val typeSpec = types.map(_.declString).mkString("\n")

    // input relation specification
    val inputRelSpec = inputRels.map(_.declString).mkString("\n")
    val inputDecl = inputRels.map(rel => s".input ${rel.name}").mkString("\n")

    // output relation specification
    val outRels = getOutRels(program)
    val outputRelSpec = outRels.map(_.declString).mkString("\n")
    val outputDecl = outRels.map(rel => s".output ${rel.name}").mkString("\n")

    val programStr = List(typeSpec, inputRelSpec, outputRelSpec, inputDecl, program.toString,
      outputDecl).mkString("\n")
    programStr
  }


  def getOutRels(program: Program): Set[Relation] = {
    val ruleBodyRels = program.getAllRelations.intersect(originalOutRels)
    val ruleHeadRels = program.rules.map(_.head.relation)
    ruleHeadRels ++ ruleBodyRels
  }

  def loadOutput(dir: Path, outRels: Set[Relation]): Examples = {
    var idb = Examples()
    for (rel <- outRels) {
      val outFile = Paths.get(dir.toString, s"${rel.name}.csv")

      // wait until file is written
      var i = 0
      while (i < 1000 && Files.notExists(outFile)) {
        Thread.sleep(100) // sleep for 100 milliseconds
        i+=1
      }
      require(Files.exists(outFile), s"output file not produced ${outFile} after $i trails.")

      val facts = Misc.readCsv(outFile.toString)
      val tuples = facts.map(Examples.strToTuple(rel, _)).toSet
      idb = idb.addTuples(rel, tuples)
    }
    idb
  }

  def _writeFile(string: String, path: Path): Any = {
    val bw = new BufferedWriter(new FileWriter(path.toFile))
    bw.write(string)
    bw.close()
  }
}
