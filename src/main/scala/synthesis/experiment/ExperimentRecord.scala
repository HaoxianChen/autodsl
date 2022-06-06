package synthesis.experiment

import synthesis._
import synthesis.util.Misc

import java.nio.file.Paths
import scala.io.Source
import scala.util.parsing.json.JSONObject


case class ExperimentRecord(results: Map[String, Any], program: Program,
                            nQueries: List[Int] = List(), durations: List[Int] = List()) {
  require(results.contains("exp_name"))
  require(results.contains("problem"))

  for ((k,v) <- results) {
    require(!k.contains(":"), k)
    require(!v.toString.contains(":"), v.toString)
  }

  def dump(outDir: String = "results"): Unit = {
    val s = JSONObject(results).toString().replace(",",",\n")

    val problemDir = Paths.get(outDir, results("problem").toString)
    Misc.makeDir(problemDir)

    val timestamp: String = Misc.getTimeStamp()

    val filename: String = s"${results("exp_name")}_result[$timestamp].log"
    val file = Paths.get(problemDir.toString, filename)
    Misc.writeFile(s, file)

    val solution: String = s"${results("exp_name")}_solution[$timestamp].log"
    val f2 = Paths.get(problemDir.toString, solution)
    Misc.writeFile(program.toString, f2)

    if (nQueries.nonEmpty) {
      val queryLogs: String = {
        var _s = s"#numQueries\n"
        _s += nQueries.mkString(",") + "\n"
        _s += s"#Durations (s)\n"
        _s += durations.mkString(",") + "\n"
        _s
      }
      val queryLogFile: String = s"${results("exp_name")}_queries[$timestamp].log"
      val f3 = Paths.get(problemDir.toString, queryLogFile)
      Misc.writeFile(queryLogs, f3)
    }
  }

}

object ExperimentRecord {
  def recordCount(outDir: String, problem: Problem, sig: Int, nDrop: Int): Int = {
    val validRecords = allRecords(outDir,problem).filter(_("sig").toInt == sig)
                              .filter(_("numDrop").toInt == nDrop)
    validRecords.size
  }

  def recordCount(outDir: String, problem: Problem, sig: Int): Int = {
    val validRecords = allRecords(outDir,problem).filter(_("sig").toInt == sig)
    validRecords.size
  }

  def recordsBySamples(outDir: String, problem: Problem, sig: Int, nSamples: Int): List[Map[String,String]] = {
    val validRecords = allRecords(outDir,problem).filter(_("sig").toInt == sig).
      filter(_.getOrElse("maxExamples","-1").toInt == nSamples)
    validRecords
  }

  def allRecords(outDir: String, problem: Problem): List[Map[String,String]] = {
    val problemDir = Paths.get(outDir, problem.name)
    val allFiles: List[String] = Misc.getListOfFiles(problemDir.toString)
    val logFiles: List[String] = allFiles.filter(_.contains("result"))
    logFiles.map(f => Paths.get(problemDir.toString,f).toString).
      map(fromFile)
  }

  def fromFile(filename: String): Map[String,String] = {
    val s: String = Source.fromFile(filename).getLines.mkString
    strToMap(s)
  }

  def strToMap(str: String): Map[String, String] = {
    val s1 = str.substring(1, str.length - 1)
      .split(",")
      .map(_.split(":"))

      s1
      .map { case Array(k, v) => {
        // trim the value
        val kt = k.trim()
        // remove double quote from key
        (kt.substring(1, kt.length-1), v.trim())
      }}
      .toMap
  }
}