# Synthesizing Formal Network Specifications from Input-Output Examples

## Software requirements

Install souffle 2.0.1 and sbt following these instructions.
1. [souffle 2.0.1](https://github.com/souffle-lang/souffle/releases/). 
2. [sbt](https://www.scala-sbt.org/release/docs/Setup.html).

## Run all benchmarks in paper
(todo)

## Run FOIL

1. Prepare the problem directory. 
See [this](benchmarks/README.md) for instructions, and
[benchmarks](benchmarks) for complete examples.
2. Start sbt by command ``sbt``. 
3. In the sbt console, 
```run foil <problem directory> ```.
For example, to synthesize the reachability problem,
```run foil benchmarks/reachability ```.

