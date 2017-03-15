package org.scalameta.benchmarks

import org.scalameta.logger
import org.scalatest.FunSuite

class BenchmarkOK extends FunSuite {

  Seq(
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

// benchmarks/test:runMain org.scalameta.benchmarks.KeepBenchmarkRunning
// will start this process, making it easy to attach to for profiling purposes.
object KeepBenchmarkRunning {
  def main(args: Array[String]): Unit = {
    val extraLarge = new Micro.ExtraLarge
    1.to(250).foreach { x =>
      if (x % 10 == 0) logger.debug(x)
      extraLarge.testMe()
    }
  }
}
