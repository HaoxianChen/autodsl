package synthesis.experiment

import synthesis.Problem

import java.nio.file.{Path, Paths}

abstract class Experiment(outDir: String) {
  def getOutFile(problem: Problem): Path = Paths.get(outDir, problem.domain,s"${problem.name}.log")

  def getProblemSignature(problem: Problem): Int = problem.hashCode()
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
