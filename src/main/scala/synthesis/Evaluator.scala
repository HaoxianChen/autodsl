package synthesis

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}
import sys.process._

case class Evaluator(problem: Problem) {
  private var cache: Map[Program, Examples] = Map()

  // Prepare tmp directory
  private val tmpdir = Paths.get("tmp")
  if (Files.notExists(tmpdir)) tmpdir.toFile.mkdir()

  def _eval(program: Program): Examples = {
    // check if results are cached
    if (cache.contains(program)) cache(program)
    else {
      // make temporary directory
      val dirName: String = s"${problem.name}_${program.hashCode()}"
      val problemDir = Paths.get(tmpdir.toString, dirName)

      if (Files.notExists(problemDir)) {
        if (!problemDir.toFile.mkdir()) {
          throw new RuntimeException(s"Failed to create problem tmp directory ${problemDir.toString}.")
        }
      }
      // dump problem to file
      val programPath = dumpProgram(program, problemDir)

      // run
      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val cmd = s"souffle ${programPath.toString} -F ${problemDir.toString} -D ${problemDir.toString}"
      val status = cmd ! ProcessLogger(stdout append _, stderr append _)
      require(!stderr.contains("Error"))

      // load results from file
      val idb: Examples = loadOutput(problemDir)

      // update cache
      cache = cache.updated(program, idb)

      idb
    }
  }

  def eval(program: Program): Set[Tuple] = _eval(program).toTuples()

  def dumpProgram(program: Program, dir: Path): Path = {
    // remove the * annotation from problem spec
    val specStr = problem.specStr.replace("*", "")
    val inputDecl = problem.inputRels.map(rel => s".input ${rel.name}").mkString("\n")
    val outputDecl = problem.outputRels.map(rel => s".output ${rel.name}").mkString("\n")
    val programStr = List(specStr, inputDecl, program.toString, outputDecl).mkString("\n")

    val programPath = Paths.get(dir.toString, s"${problem.name}.dl")
    _writeFile(programStr, programPath)

    for (rel <- problem.edb.elems.keys) {
      val str = problem.edb.toFileStr(rel)
      _writeFile(str, Paths.get(dir.toString, s"${rel.name}.facts"))
    }
    programPath
  }

  def loadOutput(dir: Path): Examples = {
    var idb = Examples()
    for (rel <- problem.outputRels) {
      val facts = Misc.readCsv(Paths.get(dir.toString, s"${rel.name}.csv").toString)
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
