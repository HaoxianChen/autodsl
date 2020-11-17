package synthesis

import java.io.File
import java.nio.file.{Path, Paths}

import scala.io.Source


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
      .addSpecStr(inputString)

    // Read Input Output examples
    def relToProblem(problem: Problem, relation:Relation): Problem = {
      val isInput: Boolean = problem.inputRels.contains(relation)
      val suffix: String = if (isInput) "facts" else "csv"
      val filename = Paths.get(dir, s"${relation.name}.$suffix").toString
      val facts = readCsv(filename)
      if (isInput) problem.addEdb(relation, facts) else problem.addIdb(relation, facts)
    }
    (problem.inputRels ++ problem.outputRels).foldLeft(problem)(relToProblem)
  }

}
