package synthesis
import java.nio.file.{Path, Paths}
import com.typesafe.scalalogging.Logger
import synthesis.search.{FaconSynthesizer, Synthesis}
import synthesis.util.Misc

import scala.io.Source
import scala.util.Random
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

abstract class Experiment

class SynthesisExperiment(benchmarkDir: String = "/Users/hxc/projects/autodsl-bench",
                          outDir: String = "results/synthesis",
                         ) extends Experiment {
  private val logger = Logger("Synthesis")
  def getBenchmarkDir = benchmarkDir
  def allProblems: List[Path] = List(
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
    "consensus/paxos/paxos-value",
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
  ).map(s => Paths.get(benchmarkDir, s))
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

  def isResultExist(problem: Problem): Boolean = getOutFile(problem).toFile.exists()

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

class ActiveLearningExperiment(maxExamples: Int = 400, outDir: String = "results/active-learning") extends Experiment {
  private val logger = Logger("Experiment")

  def go(problem: Problem, nDrop: Int ,repeats: Int = 1): Unit = {
    // Randomly drop one example
    for (i <- 1 to repeats) {
      logger.info(s"iteration $i")
      // randomDrop(problem, nDrop=1)
      randomDrop(problem, nDrop=nDrop)
    }
  }

  def randomDrop(problem: Problem, nDrop: Int): (Program, Int, Double) = {
    logger.info(s"Randomly drop ${nDrop} examples.")
    val examples: Set[ExampleInstance] = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)
    val n_remains = examples.size - nDrop
    assert(n_remains > 0, s"${examples.size}")

    val incompleteExamples = sampleExamples(examples, n_remains)
    logger.info(s"${incompleteExamples.size} examples left.")
    val (newEdb, newIdb) = ExampleInstance.toEdbIdb(incompleteExamples)
    val newProblem: Problem = problem.copy(edb=newEdb, idb=newIdb)

    val t1 = System.nanoTime
    val learner = new ActiveLearning(newProblem, maxExamples)
    val (program, nQueries) = learner.go()

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s, ${nQueries} queries.")

    val record = ExperimentRecord(Map("problem"->problem.name,
      "exp_name" -> s"drop_${nDrop}_example",
      "numDrop" -> nDrop,
      "numQuereis" -> nQueries,
      "time"->duration),
      program
    )
    record.dump(outDir)

    (program, nQueries, duration)
  }

  // def sampleExamples(examples: Set[ExampleInstance], n_samples: Int, seed: Option[Int]=None): Set[ExampleInstance] = {
  def sampleExamples(examples: Set[ExampleInstance], n_samples: Int): Set[ExampleInstance] = {
    // val rnd = new Random(seed)
    val rnd = new Random()
    rnd.shuffle(examples.toList).take(n_samples).toSet
  }
}

class DebloatingExperiment(maxExamples: Int =100, outDir: String = "results/debloat") extends Experiment {
  private val logger = Logger("Debloating")
  def go(problem: Problem, repeats: Int = 1): Unit = {
    for (i <- 1 to repeats) {
      logger.info(s"iteration $i")
      debloat(problem)
    }
  }

  def debloat(problem: Problem): Unit = {
    val t1 = System.nanoTime
    val learner = new ActiveLearning(problem, maxExamples)
    val (program, nQueries) = learner.go()

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Finished in ${duration}s, ${nQueries} queries.")

    val exampleInstances = ExampleInstance.fromEdbIdb(problem.edb, problem.idb)

    val record = ExperimentRecord(Map("problem"->problem.name,
      "exp_name" -> s"random_trace",
      "trace_length" -> exampleInstances.size,
      "numQuereis" -> nQueries,
      "time"->duration),
      program
    )
    record.dump(outDir)
    logger.info(s"Finished in ${duration}s, trace length ${exampleInstances.size}, ${nQueries} queries.")
  }

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
