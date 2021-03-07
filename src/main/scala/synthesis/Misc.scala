package synthesis

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.io.Source
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
    val oracleSpec = readOracle(dir, problemName)
    val p1 = if(oracleSpec.isDefined) problem.addOracleSpec(oracleSpec.get) else problem

    // Read Input Output examples
    def relToProblem(problem: Problem, relation:Relation): Problem = {
      val isInput: Boolean = problem.inputRels.contains(relation)
      val suffix: String = if (isInput) "facts" else "csv"
      val filename = Paths.get(dir, s"${relation.name}.$suffix").toString
      val facts = readCsv(filename)
      if (isInput) problem.addEdb(relation, facts) else problem.addIdb(relation, facts)
    }
    (problem.inputRels ++ problem.outputRels).foldLeft(p1)(relToProblem)
  }

  def readOracle(dir: String, problemName: String): Option[String] = {
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

  def getTimeStamp: String = DateTimeFormatter.ofPattern("yyyy-MM-dd_HH:mm:ss").format(LocalDateTime.now)

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
}
