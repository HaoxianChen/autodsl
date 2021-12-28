package synthesis.experiment

import synthesis._
import synthesis.util.Misc

import java.nio.file.Paths
import scala.io.Source
import scala.util.parsing.json.JSONObject


case class ExperimentRecord(results: Map[String, Any], program: Program) {
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

  }

}

object ExperimentRecord {
  def recordCount(outDir: String, problem: Problem, sig: Int, nDrop: Int): Int = {
    val problemDir = Paths.get(outDir, problem.name)
    val allFiles: List[String] = Misc.getListOfFiles(problemDir.toString)
    val logFiles: List[String] = allFiles.filter(_.contains("result"))
    val records: List[Map[String,String]] = logFiles.map(f => Paths.get(problemDir.toString,f).toString).
                                                     map(fromFile)
    val validRecords = records.filter(_("sig").toInt == sig)
                              .filter(_("numDrop").toInt == nDrop)
    validRecords.size
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