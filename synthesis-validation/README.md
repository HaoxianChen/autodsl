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

Checkout synthesis results for the other experiments: [active learning](active-learning),
    [randomly drop examples](random-drop), and [learning from execution traces](debloat).
