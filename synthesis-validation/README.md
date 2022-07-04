# Validation results for specifications synthesized by NetSpec

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
