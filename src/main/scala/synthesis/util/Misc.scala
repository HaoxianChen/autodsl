package synthesis.util

import synthesis.activelearning.ExampleInstance

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import synthesis.{NumberType, Parser, Problem, Relation, SymbolType}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.io.Source
import scala.sys.process.{ProcessLogger, _}
import scala.util.Random


object Misc {
  def getListOfFiles(dir: String): List[String] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).map(_.getName).toList
    } else {
      List[String]()
    }
  }

  def fileToString(filename: String): String = {
    val src = Source.fromFile(filename)
    val s = src.mkString
    src.close()
    s
  }

  def readCsv(filename: String): List[List[String]] = {
    val src = Source.fromFile(filename)
    def lineToList(l: String): List[String] = l.split("[ \t]").map(_.trim).toList
    val allLines = src.getLines().filterNot(_.startsWith("//"))
    val s: List[List[String]] = allLines.map(lineToList).toList
    src.close()
    //println(filename)
    // println(s)
    s
  }

  def readProblem(dir: String): Problem = {
    val problemFile: Path = {
      val allFiles = getListOfFiles(dir)
      val f: List[String] = allFiles.filter(_.endsWith("problem"))
      require(f.size==1)
      Paths.get(dir, f.head)
    }
    val problemName = problemFile.getFileName().toString.split('.').head
    val inputString = fileToString(problemFile.toString)
    val parser = new Parser()
    val problem = parser.parseAll(parser.problem, inputString).get
      .rename(problemName)

    // Read oracle program
    val oracleSpec = readDatalogProgram(dir, problemName)
    val p1 = if(oracleSpec.isDefined) {
      val _os = renameTypes(problem,oracleSpec.get)
      problem.addOracleSpec(_os)
    } else problem

    // Read Input Output examples
    // def relToProblem(problem: Problem, relation:Relation): Problem = {
    //   val isInput: Boolean = problem.inputRels.contains(relation)
    //   val suffix: String = if (isInput) "facts" else "csv"
    //   val filename = Paths.get(dir, s"${relation.name}.$suffix").toString
    //   val facts = readCsv(filename)
    //   if (isInput) problem.addEdb(relation, facts) else problem.addIdb(relation, facts)
    // }
    // (problem.inputRels ++ problem.outputRels).foldLeft(p1)(relToProblem)
    readExamples(p1, dir)
  }

  def readExamples(problem: Problem, _dir: String): Problem = {
    def relToProblem(_p0: Problem, _rel:Relation): Problem = {
      val isInput: Boolean = _p0.inputRels.contains(_rel)
      val suffix: String = if (isInput) "facts" else "csv"
      val filename = Paths.get(_dir, s"${_rel.name}.$suffix").toString
      val facts = readCsv(filename)
      if (isInput) _p0.addEdb(_rel, facts) else _p0.addIdb(_rel, facts)
    }
    (problem.inputRels++problem.outputRels).foldLeft(problem)(relToProblem)
  }

  def dumpExamples(problem: Problem, outDir: String): Unit = {
    for (rel <- problem.inputRels) {
      val str = if (problem.edb.elems.contains(rel)) {
        problem.edb.toFileStr(rel)
      }
      else {
        s""
      }
      Misc.writeFile(str, Paths.get(outDir, s"${rel.name}.facts"))
    }
    for (rel <- problem.outputRels) {
      val str = if (problem.idb.elems.contains(rel)) {
        problem.idb.toFileStr(rel)
      }
      else {
        s""
      }
      Misc.writeFile(str, Paths.get(outDir, s"${rel.name}.csv"))
    }
  }

  def readStaticRelations(dir: String): Set[Relation] = {
    val problem = readProblem(dir)
    val configFile: Path = Paths.get(dir,ExampleConvertor.staticRelationFile)
    if (configFile.toFile.exists()) {
      val line: String = fileToString(configFile.toString)
      val staticRelationNames: Set[String] = line.split("\n").toSet
      problem.inputRels.filter(rel => staticRelationNames.contains(rel.name))
    }
    else {
      Set()
    }
  }
  def readDatalogProgram(dir: String, problemName: String): Option[String] = {
    val solutionFile: Path = Paths.get(dir, s"$problemName-sol.dl")
    if (Files.exists(solutionFile)) {
      Some(fileToString(solutionFile.toString))
    }
    else {
      None
    }
  }

  def crossJoin[T](list: Iterable[Iterable[T]]): Iterable[Iterable[T]] =
    list match {
      case xs :: Nil => xs map (Iterable(_))
      case x :: xs => for {
        i <- x
        j <- crossJoin(xs)
      } yield Iterable(i) ++ j
    }

  def makeDir(dir: Path): Unit = {
    if (Files.notExists(dir)) {
      if (!dir.toFile.mkdir()) {
        throw new RuntimeException(s"Failed to create directory ${dir.toString}.")
      }
    }
  }

  def getTimeStamp(sep: String = ":"): String = {
    val pattern = sep match {
      case ":" => "yyyy-MM-dd_HH:mm:ss"
      case "-" => "yyyy-MM-dd_HH-mm-ss"
    }
    DateTimeFormatter.ofPattern(pattern).format(LocalDateTime.now)
  }

  def writeFile(string: String, path: Path): Any = {
    val bw = new BufferedWriter(new FileWriter(path.toFile))
    bw.write(string)
    bw.close()
  }

  def sampleList[T](list: List[T], n: Int): List[T] = {
    Random.shuffle(list).take(n)
  }
  def sampleSet[T](set: Set[T], n: Int): Set[T] = sampleList(set.toList, n).toSet

  def listDiff(list: List[Double]): List[Double] = {
    if (list.size >= 2) {
      list.sliding(2).map { case Seq(x, y, _*) => y - x }.toList
    }
    else if (list.size == 1) {
      List(list.head)
    }
    else {
      List()
    }
  }

  def slidingWindowUpdate[T](list: List[T], a: T, N: Int): List[T] = {
    val left = if (list.size < N) list else list.takeRight(N-1)
    val newList = left :+ a
    require(newList.size <= N)
    newList
  }

  def runWithTimeOut(cmd: String, timeOut: Int): (Int, String, String, Boolean) = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val p = cmd.run(ProcessLogger(stdout append _, stderr append _)) // start asynchronously
    val f = Future(blocking(p.exitValue())) // wrap in Future

    var isTimeOut = false
    val exitcode = try {
      Await.result(f, duration.Duration(timeOut, "millis"))
    } catch {
      case _: TimeoutException =>
        isTimeOut = true
        p.destroy()
        p.exitValue()
    }
    (exitcode, stdout.toString(), stderr.toString(), isTimeOut)
  }

  def renameTypes(problem: Problem, _specStr: String): String = {
    // rename all types to their base type
    val renamed: String = {
      var _s: String = _specStr
      for (_type <- problem.types) {
        val baseTypeName = _type match {
          case _:NumberType => "number"
          case _:SymbolType => "symbol"
        }
        _s = _s.replaceAll(s"\\b${_type.name}\\b", baseTypeName)
      }
      _s
    }
    val allLines: Array[String] = renamed.split("\n")
    def isTypeDecl(line: String): Boolean = line.startsWith(".type")
    allLines.filterNot(isTypeDecl).mkString("\n")
  }

}
