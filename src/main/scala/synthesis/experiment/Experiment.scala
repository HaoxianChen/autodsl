package synthesis.experiment

import synthesis.{Problem, Program, Relation, Rule}
import synthesis.activelearning.ActiveLearning
import synthesis.util.Misc

import java.nio.file.{Path, Paths}
import scala.io.Source

abstract class Experiment(outDir: String) {
  def getOutDir(problem: Problem): Path = Paths.get(outDir, problem.domain)
  def getOutFile(problem: Problem): Path = Paths.get(getOutDir(problem).toString,s"${problem.name}.log")
  def getSolutionFile(problem: Problem): Path = Paths.get(getOutDir(problem).toString,s"${problem.name}-sol.dl")

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

}

object Experiment {
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
    "wireless/aodv/aodv-rreq",
    "wireless/aodv/aodv-seq",
    "wireless/dsdv",
    // "wireless/dsr"
    "wireless/dsr/dsr-rreq",
    "wireless/dsr/dsr-rrep",
    "wireless/dsr/dsr-rerr",
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
    // "wireless/aodv/aodv-route",
    // "wireless/aodv/aodv-route-source",
    // "wireless/aodv/aodv-rrep",
    // "wireless/aodv/aodv-rreq",
    // "wireless/aodv/aodv-seq",
    "wireless/dsdv",
    "wireless/dsr/dsr-rreq",
    "wireless/dsr/dsr-rreq",
    "wireless/dsr/dsr-rerr",
  )

  val activelearningProblems :List[String] = List(
    // SDN
    "forwarding/l2-pairs",
    "firewall/stateful-firewall",
    "firewall/l3-stateful-firewall",
    "consensus/2pc-no-timer",
    // Consensus
    "consensus/paxos/paxos-proposer",
    // routing
    "routing/ospf-synnet",
    "routing/bgp",
    "routing/tree",
    "routing/min-admin",
    // "routing/rip",
    // Wireless
    // "wireless/aodv/aodv-route",
    // "wireless/aodv/aodv-route-source",
    // "wireless/aodv/aodv-rrep",
    // "wireless/aodv/aodv-rreq",
    // "wireless/aodv/aodv-seq",
    "wireless/dsdv",
    // "wireless/dsr",
  )

  val activeLearningWithOracle: List[String] = List(
    // SDN
    "firewall/l3-stateful-firewall",
    // Consensus
    "consensus/paxos/paxos-proposer",
    // routing
    "routing/min-admin",
    "consensus/2pc-no-timer",
    // Wireless
    "wireless/aodv/aodv-route",
    // "wireless/aodv/aodv-route-source",
    "wireless/aodv/aodv-rrep",
    "wireless/aodv/aodv-rreq",
    // "wireless/aodv/aodv-seq",
    "wireless/dsdv",
    // "wireless/dsr",
    "routing/rip",
  )

  val randomDropExperiments: List[String] = List(
    "aws/subnet",
    "firewall/l3-firewall",
    "sensor/temperature-report",
  )

  val debloatingExperiments: List[String] = List(
    "firewall/random-traces/floodlight-firewall",
    "firewall/random-traces/floodlight-stateful-firewall",
    "firewall/random-traces/pox-l3-firewall",
    "firewall/random-traces/pox-l3-stateful-firewall",
    "forwarding/random-traces/pox-l2-pairs",
  )

  val recursion = Set(
    "nib/reachable",
    "nib/path",
    "nib/path-cost",
    "aws/sshTunnel",
    "routing/shortest-path",
    "routing/least-congestion"
  )
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
  )
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
  )


  def checkSolution(problemDir: String, solution: Program): Boolean = {
    val problem = Misc.readProblem(problemDir)
    val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(problemDir)
    val programValidator = ProgramValidator(problem, staticConfigRelations)
    val (validated, _) = programValidator.differentiateFromReference(solution)
    validated
  }

  def getSolution(programs: Map[Relation, List[Program]]): Program = {
    var _rules: Set[Rule] = Set()
    for ((rel,ps)<-programs) {
      _rules ++= ps.head.rules
    }
    Program(_rules)
  }

}
