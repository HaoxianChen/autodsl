package synthesis.experiment

import synthesis._
import synthesis.util.Misc

import java.nio.file.{Paths}
import scala.util.parsing.json.JSONObject


case class ExperimentRecord(results: Map[String, Any], program: Program) {
  require(results.contains("exp_name"))
  require(results.contains("problem"))

  def dump(outDir: String = "results"): Unit = {
    val s = JSONObject(results).toString().replace(",",",\n")

    val problemDir = Paths.get(outDir, results("problem").toString)
    Misc.makeDir(problemDir)

    val timestamp: String = Misc.getTimeStamp

    val filename: String = s"${results("exp_name")}_result[$timestamp].log"
    val file = Paths.get(problemDir.toString, filename)
    Misc.writeFile(s, file)

    val solution: String = s"${results("exp_name")}_solution[$timestamp].log"
    val f2 = Paths.get(problemDir.toString, solution)
    Misc.writeFile(program.toString, f2)

  }

}