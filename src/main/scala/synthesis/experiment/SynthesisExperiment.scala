package synthesis.experiment

import com.typesafe.scalalogging.Logger
import synthesis.experiment.SynthesisExperiment.allProblemDirStr
import synthesis.{Problem, Program, Relation}
import synthesis.search.{FaconSynthesizer, Synthesis}
import synthesis.util.Misc

import java.nio.file.{Path, Paths}
import scala.io.Source

abstract class Experiment

class SynthesisExperiment(benchmarkDir: String = "/Users/hxc/projects/autodsl-bench",
                          outDir: String = "results/synthesis",
                         ) extends Experiment {
  private val logger = Logger("Synthesis")
  def getBenchmarkDir = benchmarkDir
  def allProblems: List[Path] = allProblemDirStr.map(s => Paths.get(benchmarkDir, s))
  def getSynthesizer(p: Problem): Synthesis = Synthesis(p)

  def run(update: Boolean): Unit = {
    for (problemFile <- allProblems) {
      val problem = Misc.readProblem(problemFile.toString)
      if (!isResultExist(problem) || update) {
        logger.info(s"run ${problem.name}")
        val t1 = System.nanoTime
        val synthesizer = getSynthesizer(problem)
        val programs = synthesizer.go()
        val duration = (System.nanoTime - t1) / 1e9d
        logger.info(s"Finished in ${duration}s")
        assert(programs.nonEmpty, s"Test failed: ${problemFile}.")
        writeResults(problem, programs, duration.toInt)
      }
      else {
        logger.info(s"${problem.name} result exists. Skip.")
      }
    }
    generateTable()
  }

  def generateTable(): Unit = {
    def getLogFile(dir: String): String = {
      val problem = Misc.readProblem(dir)
      getOutFile(problem).toString
    }
    val statLineNum: Int = 5
    val stats = allProblems.map { problemDir =>
      // Read the log file
      val filename: String = getLogFile(problemDir.toString)
      Source.fromFile(filename).getLines().toList(statLineNum)
    }
    val fileStr = stats.mkString("\n") + "\n"
    val outFile: Path = Paths.get(outDir, s"all.log")
    Misc.writeFile(fileStr, outFile)
  }

  def getOutFile(problem: Problem): Path = Paths.get(outDir, problem.domain,s"${problem.name}.log")

  def getProblemSignature(problem: Problem): Int = problem.hashCode()

  def isResultExist(problem: Problem): Boolean = {
    val outFile: Path = getOutFile(problem)
    if (outFile.toFile.exists()) {
      // Read and compare the problem signature
      val sigLines  = Source.fromFile(outFile.toFile).getLines().filter(_.startsWith(s"sig:")).toList
      if (sigLines.nonEmpty) {
        assert(sigLines.size==1)
        val sig = sigLines.head.split(s":")(1).trim().toInt
        sig == getProblemSignature(problem)
      }
      else {
        false
      }
    }
    else {
      false
    }
  }

  def writeResults(problem: Problem, programs: Map[Relation, List[Program]], duration: Int): Unit = {
    /*** Gather problem specs */
    val nInputTuples: Int = problem.edb.toTuples().size
    val nOutputTuples: Int =  problem.idb.toTuples().size
    val nTotalTuples = nInputTuples + nOutputTuples
    val nInputRels = problem.inputRels.size
    val nOutputRels = problem.outputRels.size
    val nTotal = nInputRels + nOutputRels

    /*** Display results */
    var literalCounts: Int = 0
    var fieldCounts: Int = 0
    var ruleCounts: Int = 0
    for ((rel,ps)<-programs) {
      val p = ps.head
      ruleCounts += p.rules.size
      literalCounts += p.literalCounts
      fieldCounts += p.fieldCounts
    }
    println(s"${literalCounts} literals, ${fieldCounts} fields.")

    var fileStr: String = s"# domain name features(recur agg udf) \n" +
      "# relations(in out) rules literals variables \n" +
      "# rules literals variables \n" +
      "# examples(instances in out rows) \n" +
      "# time\n"
    val fields: Seq[Any] = Seq(problem.domain, problem.name, s"", s"", s"") ++
      Seq(nInputRels, nOutputRels) ++
      Seq(ruleCounts, literalCounts, fieldCounts) ++
      Seq(problem.getNumExampleInstances, nInputTuples, nOutputTuples, nTotalTuples) ++
      Seq(duration)
    fileStr += fields.mkString("\t") + "\n\n"

    fileStr += s"${problem.name}\n"
    fileStr += s"$nTotal Relations ($nInputRels, $nOutputRels)\n"
    fileStr += s"$nTotalTuples examples ($nInputTuples, $nOutputTuples).\n"
    fileStr += s"${problem.getNumExampleInstances} example instances.\n"

    for ((rel,ps)<-programs) {
      val p = ps.head
      fileStr += s"$rel: ${ps.size} programs, smallest one contains ${p.literalCounts} literals.\n"
      fileStr += s"$p\n"
    }
    fileStr += s"sig:${getProblemSignature(problem)}\n"
    Misc.makeDir(getOutFile(problem).getParent)
    Misc.writeFile(fileStr, getOutFile(problem))
  }
}

class FaconExperiment(outDir: String = "results/facon") extends SynthesisExperiment(outDir=outDir) {
  private val benchmarkDir = "/Users/hxc/projects/autodsl-bench"

  override def getSynthesizer(p: Problem): Synthesis = new FaconSynthesizer(p)
  override def allProblems: List[Path] = List(
    // Network analysis
    "nib/reachable",
    "aws/publicIP",
    "aws/subnet",
    "aws/sshTunnel",
    "nod/protection",
    // "nod/locality",
    // SDN
    // "forwarding/learning-switch",
    "forwarding/learning-switch-no-flood",
    // "forwarding/l2-pairs",
    "firewall/stateless-firewall",
    "firewall/stateful-firewall",
    // "firewall/l3-firewall",
    // "firewall/l3-stateful-firewall",
    // Sensor network
    "sensor/evidence",
    "sensor/store",
    // "sensor/temperature-report",
  ).map(s => Paths.get(benchmarkDir, s))
}

class AllSynthesisExperiments() {
  val netspec = new SynthesisExperiment()
  val facon = new FaconExperiment()
  val allProblems = netspec.allProblems

  val recursion = Set(
    "nib/reachable",
    "nib/path",
    "nib/path-cost",
    "aws/sshTunnel",
    "routing/shortest-path",
    "routing/least-congestion"
  ).map(s => Paths.get(netspec.getBenchmarkDir, s))
  val aggregation = Set(
    // consensus
    "consensus/2pc-no-timer",
    "consensus/paxos/paxos-quorum",
    "consensus/paxos/paxos-value",
    // routing
    "routing/shortest-path",
    "routing/least-congestion",
    "routing/ospf-synnet",
    "routing/bgp",
    "routing/tree",
    "routing/min-admin",
    "routing/rip",
  ).map(s => Paths.get(netspec.getBenchmarkDir, s))
  val udf = Set(
    "nib/path",
    "nib/path-cost",
    // consensus
    "consensus/paxos/paxos-quorum",
    // routing
    "routing/shortest-path",
    "routing/least-congestion",
    "routing/ospf-synnet",
    "routing/bgp",
    "routing/tree",
    "routing/min-admin",
    "routing/rip",
    // Wireless
    "wireless/aodv/aodv-route",
    "wireless/aodv/aodv-route-source",
    "wireless/aodv/aodv-rrep",
    "wireless/aodv/aodv-rreq",
    "wireless/aodv/aodv-seq",
    "wireless/dsdv",
    "wireless/dsr"
  ).map(s => Paths.get(netspec.getBenchmarkDir, s))

  val renaming = Map(
    "consensusbarrier" -> "consensus",
    "routingProto" -> "wireless",
  )

  def run(): Unit = {
    netspec.run(update = false)
    facon.run(update = false)
    generateTable()
  }

  def generateTable(): Unit = {
    val statLineNum: Int = 5
    val stats = allProblems.map { problemDir =>
      val problem: Problem = Misc.readProblem(problemDir.toString)

      // Read netspec's log file
      val netspecStat: List[String] = {
        val logfile: String = netspec.getOutFile(problem).toString
        val statLine = Source.fromFile(logfile).getLines().toList(statLineNum)
        statLine.split("\t").toList
      }
      assert(netspecStat.length==15)
      val names: List[String] = netspecStat.take(2)
      val features: List[String] = netspecStat.slice(5,14)
      val netSpecTime: String = if (netspecStat.last.toInt >0) netspecStat.last else 1.toString

      val recur: String =  if (recursion.contains(problemDir)) "\\cmark" else " "
      val agg: String =  if (aggregation.contains(problemDir)) "\\cmark" else " "
      val _udf: String =  if (udf.contains(problemDir)) "\\cmark" else " "

      val faconTime: String = {
        if (facon.isResultExist(problem)) {
          val logfile: String = facon.getOutFile(problem).toString
          val stat: String = Source.fromFile(logfile).getLines().toList(statLineNum)
          val time = stat.split("\t").last
          if (time.toInt>0) time else 1.toString
        }
        else "-"
      }
      val allStats: List[String] = (names ++ List(recur,agg,_udf) ++ features) :+ netSpecTime :+ faconTime
      assert(allStats.length==netspecStat.length+1)
      allStats.mkString("\t")
    }
    val fileStr = stats.mkString("\n") + "\n"
    val outFile: Path = Paths.get("results", s"synthesis_all.log")
    Misc.writeFile(fileStr, outFile)
  }
}

object SynthesisExperiment {
  val allProblemDirStr: List[String] = List(
    // Network analysis
    "nib/reachable",
    "nib/path",
    "nib/path-cost",
    "aws/publicIP",
    "aws/subnet",
    "aws/sshTunnel",
    "nod/protection",
    "nod/locality",
    // SDN
    "forwarding/learning-switch",
    "forwarding/l2-pairs",
    "firewall/stateless-firewall",
    "firewall/stateful-firewall",
    "firewall/l3-firewall",
    "firewall/l3-stateful-firewall",
    // consensus
    "consensus/2pc-no-timer",
    "consensus/paxos/paxos-acceptor",
    "consensus/paxos/paxos-proposer",
    "consensus/paxos/paxos-quorum",
    // "consensus/paxos/paxos-value",
    "consensus/paxos/paxos-maxballot",
    "consensus/paxos/paxos-decide",
    // routing
    "routing/shortest-path",
    "routing/least-congestion",
    "routing/ospf-synnet",
    "routing/bgp",
    "routing/tree",
    "routing/min-admin",
    "routing/rip",
    // Sensor network
    "sensor/evidence",
    "sensor/store",
    "sensor/temperature-report",
    // Wireless
    "wireless/aodv/aodv-route",
    "wireless/aodv/aodv-route-source",
    "wireless/aodv/aodv-rrep",
    // "wireless/aodv/aodv-rreq",
    "wireless/aodv/aodv-seq",
    "wireless/dsdv",
    "wireless/dsr"
  )

  val regressionTests :List[String] = List(
    // Network analysis
    "nib/reachable",
    "nib/path",
    "nib/path-cost",
    "aws/publicIP",
    "aws/subnet",
    "aws/sshTunnel",
    "nod/protection",
    "nod/locality",
    // SDN
    "forwarding/learning-switch",
    "forwarding/l2-pairs",
    "firewall/stateless-firewall",
    "firewall/stateful-firewall",
    "firewall/l3-firewall",
    "firewall/l3-stateful-firewall",
    // consensus
    "consensus/2pc-no-timer",
    "consensus/paxos/paxos-acceptor",
    "consensus/paxos/paxos-proposer",
    "consensus/paxos/paxos-quorum",
    // "consensus/paxos/paxos-value",
    "consensus/paxos/paxos-maxballot",
    "consensus/paxos/paxos-decide",
    // routing
    "routing/shortest-path",
    "routing/least-congestion",
    "routing/ospf-synnet",
    "routing/bgp",
    "routing/tree",
    "routing/min-admin",
    "routing/rip",
    // Sensor network
    "sensor/evidence",
    "sensor/store",
    "sensor/temperature-report",
    // Wireless
    "wireless/aodv/aodv-route",
    "wireless/aodv/aodv-route-source",
    "wireless/aodv/aodv-rrep",
    "wireless/aodv/aodv-rreq",
    "wireless/aodv/aodv-seq",
    "wireless/dsdv",
    "wireless/dsr"
  )

}
