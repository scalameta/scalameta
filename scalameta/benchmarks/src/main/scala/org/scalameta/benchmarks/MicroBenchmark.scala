package org.scalameta.benchmarks

import scala.meta._
import contrib._
import scala.meta.internal.classifiers.classifier

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.Warmup
import org.scalameta.logger

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

  @Benchmark
  def isToken(): Int = {
    import scala.meta._
    val len = code.tokenize.get.collect {
      case x if x.is[Keyword] => 1
      case x if x.is[Delim] => 1
      case x if x.is[Literal] => 1
    }.sum
    len
  }

  @Benchmark
  def isTree(): Int = {
    import scala.meta._
    val len = code.parse[Source].get.collect {
      case x if x.is[ValOrVar] => 1
      case x if x.is[Term] => 1
      case x if x.is[Type] => 1
      case x if x.is[Pat] => 1
    }.sum
    len
  }

  def testMe(): Unit = {
    setup()
    transform()
    isToken()
    isTree()
  }

}


@classifier
trait ValOrVar
object ValOrVar {
  def unapply(tree: Tree): Boolean = {
    tree.is[Defn.Val] || tree.is[Decl.Val]||
    tree.is[Defn.Var] || tree.is[Decl.Var]
  }
}

object Micro {
  class Small extends MicroBenchmark("EventSerializers.scala")
  class Medium extends MicroBenchmark("PrintStreamTest.scala")
  class Large extends MicroBenchmark("HiveMetastoreCatalog.scala")
  class ExtraLarge extends MicroBenchmark("GenJSCode.scala")
}
