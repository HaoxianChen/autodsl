package synthesis.experiment

import synthesis.{Problem, Program, Relation}
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

  val activelearningProblems :List[String] = List(
    // Network analysis
    // "nib/reachable",
    // "nib/path",
    // "nib/path-cost",
    // "aws/publicIP",
    // "aws/subnet",
    // "aws/sshTunnel",
    // "nod/protection",
    // "nod/locality",
    // SDN
    // "forwarding/learning-switch",
    // "forwarding/l2-pairs",
    // "firewall/stateless-firewall",
    "firewall/stateful-firewall",
    // "firewall/l3-firewall",
    "firewall/l3-stateful-firewall",
    // consensus
    // "consensus/2pc-no-timer",
    // "consensus/paxos/paxos-acceptor",
    // "consensus/paxos/paxos-proposer",
    // "consensus/paxos/paxos-quorum",
    // // "consensus/paxos/paxos-value",
    // "consensus/paxos/paxos-maxballot",
    // "consensus/paxos/paxos-decide",
    // routing
    // "routing/shortest-path",
    // "routing/least-congestion",
    // "routing/ospf-synnet",
    // "routing/bgp",
    // "routing/tree",
    // "routing/min-admin",
    // "routing/rip",
    // // Sensor network
    // "sensor/evidence",
    // "sensor/store",
    // "sensor/temperature-report",
    // Wireless
    "wireless/aodv/aodv-route",
    "wireless/aodv/aodv-route-source",
    "wireless/aodv/aodv-rrep",
    "wireless/aodv/aodv-rreq",
    "wireless/aodv/aodv-seq",
    // "wireless/dsdv",
    // "wireless/dsr"
  )

  val randomDropExperiments: List[String] = List()

  def checkSolution(problemDir: String, solution: Program): Boolean = {
    val problem = Misc.readProblem(problemDir)
    val staticConfigRelations: Set[Relation] = Misc.readStaticRelations(problemDir)
    val learner = new ActiveLearning(problem, staticConfigRelations, numNewExamples = 400)
    learner.differentiateFromOracle(solution)
  }
}
