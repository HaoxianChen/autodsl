# Synthesizing Formal Network Specifications from Input-Output Examples

## Software requirements

Install souffle 2.0.1 and sbt following these instructions.
1. [souffle 2.0.1](https://github.com/souffle-lang/souffle/releases/). 
2. [sbt](https://www.scala-sbt.org/release/docs/Setup.html).

## Run all benchmarks in paper
Benchmarks are listed in [benchmarks](benchmarks). 
Files in each benchmakrs are explained [here](benchmarks/README.md).

Synthesize a program:
```
sbt run learn <path to benchmark directory> 
```

Synthesize with active learning:

1. Prepare an oracle Datalog program in the benchmark directory, named <benchmark-name>-sol.dl,
  which answers the query questions.
  See an [example](benchmarks/nib/reachable/reachable-sol.dl).
2. Run command: ``sbt run active <path to benchmark directory> ``
