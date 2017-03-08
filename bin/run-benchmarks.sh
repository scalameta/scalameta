#!/bin/bash
prefix=${1-Micro}
iterations=${2-3}
sbt "benchmarks/jmh:run -rf csv -rff target/jmh-results.csv -i $iterations -wi $iterations -f1 -t1 org.scalameta.benchmarks.$prefix*"
