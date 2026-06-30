package scala.meta.internal.bench

import scala.meta._
import scala.meta.tokens.Tokens

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

/**
 * Benchmarks for the front end: tokenization and parsing.
 *
 * The phases are intentionally split into separate `@Benchmark` methods so each can be attributed
 * independently:
 *   - `tokenize` -- lexing only (String/Input -> Tokens)
 *   - `parse` -- full parse (String -> Source), includes tokenization
 *   - `traverse` -- parse + a full tree walk (exercises classifiers/`.is[T]`)
 *   - `transform` -- parse + a rewriting tree walk (allocates new nodes)
 *
 * Recommended runs:
 * {{{
 * // wall-clock + per-op allocation (gc.alloc.rate.norm is the stable signal):
 * sbt 'benchScalameta/Jmh/run -wi 5 -i 10 -f1 -t1 -prof gc ".*ParserBenchmark.*"'
 *
 * // allocation flame graph via async-profiler (Homebrew v4.x):
 * sbt 'benchScalameta/Jmh/run -wi 5 -i 10 -f1 -t1 \
 *   -prof "async:libPath=/opt/homebrew/lib/libasyncProfiler.dylib;event=alloc;output=flamegraph;dir=/tmp/ap" \
 *   ".*ParserBenchmark.parse"'
 *
 * // CPU flame graph (event=cpu):
 * sbt 'benchScalameta/Jmh/run ... -prof "async:...;event=cpu;..." ".*parse"'
 * }}}
 */
@State(Scope.Benchmark) @Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime)) @OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class ParserBenchmark(path: String) {
  import dialects.Scala212

  // NOTE: tokens are memoized per (Input, dialect) via `input.tokenCache`, so we
  // must hand each invocation a FRESH Input (here, the raw String, converted by
  // the `tokenize`/`parse` extension each call) -- otherwise we'd measure a cache
  // hit, not the front end. See ScalametaTokenizer#tokenCache.getOrElseUpdate.
  var code: String = _

  @Setup
  def setup(): Unit = code = scala.io.Source
    .fromInputStream(getClass.getResourceAsStream(s"/$path"))("UTF-8").getLines().mkString("\n")

  @Benchmark
  def tokenize(): Tokens = code.tokenize.get

  @Benchmark
  def parse(): Source = code.parse[Source].get

  @Benchmark
  def traverse(): Int = {
    var n = 0
    code.parse[Source].get.traverse { case Term.Name(_) => n += 1 }
    n
  }

  @Benchmark
  def transform(): Tree = code.parse[Source].get.transform { case Term.Name(name) =>
    Term.Name(name.toLowerCase)
  }
}

object Micro {
  class ExtraLarge extends ParserBenchmark("GenJSCode.scala")
}
