# Validation results for specifications synthesized by NetSpec

To illustrate our validation method, for each benchmark, 
we choose one solution output (links in the "Synthesis solution" column), 
and demonstrate how we compare
it against the reference ouput (links in the "Specification" column)
in the "Comments" column.

|Specification|Synthesis solution|Comments|
|-------------|---------|--------|
[reachable](../benchmarks/nib/reachable/reachable-sol.dl)|[solution](NIB/reachable/synthesis_solution%5B2022-06-20_15:41:57%5D.log)|A redundant predicate in the second rule ```node2 != node1```.|
|[path](../benchmarks/nib/path/path-sol.dl)|[solution](NIB/path/synthesis_solution%5B2022-06-20_15:44:18%5D.log)|Variable renaming only.|
|[path-cost](../benchmarks/nib/path/path-cost-sol.dl)|[solution](NIB/path-cost/synthesis_solution%5B2022-06-20_16:34:24%5D.log)|Variable renaming only.|
|[publicIP](../benchmarks/aws/publicIP/publicIP-sol.dl)|[solution](NIB/publicIP/[v]synthesis_solution[2022-01-12_11:26:31].log)|Variable renaming only.|
|[subnet](../benchmarks/aws/subnet/subnet-sol.dl)|[solution](NIB/subnet/[v]synthesis_solution[2022-01-12_11:27:25].log)|Variable renaming only.|
|[sshTunnel](../benchmarks/aws/sshTunnel/sshTunnel-sol.dl)|[solution](NIB/sshTunnel/[v]synthesis_solution[2022-01-13_18:50:25].log)|A redundant predicate in the second rule ```canSsh(_,node1,instanceid0)```.|
|[protection](../benchmarks/nod/protection/protection-sol.dl)|[solution](NIB/protection/[v]synthesis_solution[2022-01-12_11:31:07].log)|Variable renaming only.|
|[locality](../benchmarks/nod/locality/locality-sol.dl)|[solution](NIB/locality/[v]synthesis_solution[2022-01-12_11:32:03].log)|Variable renaming only.|
|[learning-switch](../benchmarks/forwarding/learning-switch/learning-switch-sol.dl)|[solution](SDN/learning-switch/synthesis_solution[2022-01-12_11:32:40].log)|Variable renaming only.|
|[l2-pairs](../benchmarks/forwarding/l2-pairs/l2-pairs-sol.dl)|[solution](active-learning/l2-pairs/drop_0_example_100000_samples_solution[2022-06-20_19:42:07].log)|Variable renaming only.|
|[stateless-firewall](../benchmarks/firewall/stateless-firewall/stateless-firewall-sol.dl)|[solution](SDN/stateless-firewall/synthesis_solution[2022-01-12_11:33:33].log)|Variable renaming only.|
|[stateful-firewall](../benchmarks/firewall/stateful-firewall/stateful-firewall-sol.dl)|[solution](active-learning/stateful-firewall/drop_0_example_100000_samples_solution[2022-06-20_20:02:13].log)|Variable renaming only.|
|[firewall-l3](../benchmarks/firewall/l3-firewall/firewall-l3-sol.dl)|[solution](SDN/firewall-l3/[v]synthesis_solution[2022-01-12_11:34:11].log)|A redundant predicate in the first rule ```port1 != port0```.|
|[firewall-l3-stateful](../benchmarks/firewall/l3-stateful-firewall/firewall-l3-stateful-sol.dl)|[solution](active-learning/firewall-l3-stateful/drop_0_example_100000_samples_solution[2022-06-22_19:38:53].log)|Variable renaming only.|
|[paxos-acceptor](../benchmarks/consensus/paxos/paxos-acceptor/acceptor-sol.dl)|[solution](active-learning/acceptor/drop_0_example_100000_samples_solution[2022-06-21_09:11:26].log)|Variable renaming only.|
|[paxos-propose](../benchmarks/consensus/paxos/paxos-proposer/proposer-sol.dl)|[solution](active-learning/proposer/drop_0_example_100000_samples_solution[2022-06-21_08:52:06].log)|Variable renaming only.|
|[paxos-quorum](../benchmarks/consensus/paxos/paxos-quorum/quorum-sol.dl)|[solution](consensusbarrier/quorum/[v]synthesis_solution[2022-01-12_11:37:42].log)|Variable renaming only.|
|[paxos-maxballot](../benchmarks/consensus/paxos/paxos-maxballot/paxos-maxballot-sol.dl)|[solution](consensusagg/paxos-maxballot/[v]synthesis_solution[2022-01-12_11:43:40].log)|Variable renaming only.|
|[paxos-decide](../benchmarks/consensus/paxos/paxos-decide/paxos-decide-sol.dl)|[solution](consensus/paxos-decide/[v]synthesis_solution[2022-01-12_11:44:10].log)|Variable renaming only.|
|[shortest-path](../benchmarks/routing/shortest-path/shortest-path-sol.dl)|[solution](routing/shortest-path/[v]synthesis_solution[2022-01-13_10:09:04].log)|Variable renaming only.|
|[least-congestion](../benchmarks/routing/least-congestion/least-congestion-sol.dl)|[solution](routing/least-congestion/[v]synthesis_solution[2022-01-12_12:03:05].log)|Variable renaming only.|
|[ospf](../benchmarks/routing/ospf-synnet/ospf-sol.dl)|[solution](active-learning/ospf/drop_0_example_100000_samples_solution[2022-06-24_03:51:43].log)|Variable renaming only.|
|[bgp](../benchmarks/routing/bgp/bgp-sol.dl)|[solution](active-learning/bgp/drop_0_example_100000_samples_solution[2022-06-23_10:17:57].log)|Variable renaming only.|
|[tree](../benchmarks/routing/tree/tree-sol.dl)|[solution](active-learning/tree/drop_0_example_100000_samples_solution[2022-06-22_20:22:47].log)|Variable renaming only.|
|[min-admin](../benchmarks/routing/min-admin/min-admin-sol.dl)|[solution](active-learning/min-admin/drop_0_example_100000_samples_solution[2022-06-23_16:35:35].log)|Variable renaming only.|
|[evidence](../benchmarks/sensor/evidence/evidence-sol.dl)|[solutio](sensor/evidence/synthesis_solution[2022-01-12_12:04:31].log)|Variable renaming only.|
|[store](../benchmarks/sensor/store/store-sol.dl)|[solution](sensor/store/synthesis_solution[2022-01-12_12:04:34].log)|Variable renaming only.|
|[temperature-report](../benchmarks/sensor/temperature-report/temperature-report-sol.dl)|[solution](active-learning/temperature-report/drop_0_example_100000_samples_solution[2022-06-20_21:28:40].log)|Variable renaming only.|
|[dsr-rreq](../benchmarks/wireless/dsr/dsr-rreq/dsr-rreq-sol.dl)|[solution](routingProto/dsr-rreq/synthesis_solution[2022-01-12_12:04:48].log)|Variable renaming only.|
|[dsr-rrep](../benchmarks/wireless/dsr/dsr-rrep/dsr-rrep-sol.dl)|[solution](routingProto/dsr-rrep/synthesis_solution[2022-01-12_12:04:38].log)|Variable renaming only.|
|[dsr-rerr](../benchmarks/wireless/dsr/dsr-rerr/dsr-rerr-sol.dl)|[solution](routingProto/dsr-rerr/synthesis_solution[2022-01-12_12:05:16].log)|Variable renaming only.|


## All syntheiss results

On the top-level are the benchmarks.
files with ```[benchmark]-sol.log``` is the reference specification
in Datalog.
In each directory named after the benchmark names,
stores the generated benchmark.
The synthesis specification is stored in files named ```synthesis_solution[timestamp].log```
Files with prefix [x] indicates it is different from the reference
specification.
Otherwise, it is correct.
For example, this [file](random-drop/firewall-l3/%5Bx%5Ddrop_11_example_100000_samples_solution%5B2022-06-21_18:16:50%5D.log)
stores an incorrect solution.
And this [file](NIB/reachable/synthesis_solution[2022-06-20_15:41:57].log) stores a correct solution.

Checkout synthesis results for the other experiments: 
- [active learning](active-learning)
- [randomly drop examples](random-drop)
- [learning from execution traces](debloat).
