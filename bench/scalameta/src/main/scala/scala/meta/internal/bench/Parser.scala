package scala.meta.internal.bench

import scala.meta._

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

/**
 * To run benchmark:
 *
 * > sbt benchScalameta/jmh:run > possible options (olafurpg): -i 10 -wi 10 -f1 -t1 org.scalameta.*
 */
@State(Scope.Benchmark) @Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime)) @OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class ParserBenchmark(path: String) {
  import dialects.Scala212

  var code: String = _
  private def doParseCode(): Source = code.parse[Source].get

  @Setup
  def setup(): Unit = code = scala.io.Source
    .fromInputStream(getClass.getResourceAsStream(s"/$path"))("UTF-8").getLines().mkString("\n")

  @Benchmark
  def traverse(): Int = {
    var n = 0
    doParseCode().traverse { case Term.Name(name) => n += 1 }
    n
  }

  @Benchmark
  def transform(): Tree = doParseCode().transform { case Term.Name(name) =>
    Term.Name(name.toLowerCase)
  }

  def testMe(): Unit = {
    setup()
    transform()
  }
}

object Micro {
  class ExtraLarge extends ParserBenchmark("GenJSCode.scala")
}
