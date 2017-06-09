package org.scalameta.benchmarks

import scala.meta._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.Warmup

/**
  * To run benchmark:
  *
  * > benchmarks/jmh:run -i 10 -wi 10 -f1 -t1 org.scalameta.*
  */
@org.openjdk.jmh.annotations.State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class MicroBenchmark(path: String) {
  var code: String = _

  @Setup
  def setup(): Unit = {
    code = scala.io.Source
      .fromInputStream(getClass.getResourceAsStream(s"/$path"))("UTF-8")
      .getLines()
      .mkString("\n")
  }

  def scalametaParser(): Unit = {
    import scala.meta._
    code.parse[Source].get
  }

  @Benchmark
  def transform(): Tree = {
    import scala.meta._
    val tree = code.parse[Source].get
    tree.transform {
      case Term.Name(name) => Term.Name(name.toLowerCase)
    }
  }

  def testMe(): Unit = {
    setup()
    transform()
  }

}

object Micro {
  class Small extends MicroBenchmark("EventSerializers.scala")
  class Medium extends MicroBenchmark("PrintStreamTest.scala")
  class Large extends MicroBenchmark("HiveMetastoreCatalog.scala")
  class ExtraLarge extends MicroBenchmark("GenJSCode.scala")
}
