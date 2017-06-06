package org.scalameta.benchmarks

import org.scalatest.FunSuite

class BenchmarkOK extends FunSuite {
  List(
    new Micro.ExtraLarge,
    new Micro.Large,
    new Micro.Medium,
    new Micro.Small
  ).foreach { formatBenchmark =>
    val name = s"microBenchmark: ${formatBenchmark.getClass}"
    test(name) {
      formatBenchmark.testMe()
    }
  }
}

