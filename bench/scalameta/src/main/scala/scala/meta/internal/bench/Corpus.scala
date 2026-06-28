package scala.meta.internal.bench

import scala.meta._

import java.io.File
import java.nio.file.Files
import java.util.concurrent.TimeUnit
import java.util.stream.Collectors

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

/**
 * Macro-benchmark over real-world corpora checked out under `community-projects/` -- the same
 * sources the `communitytest/test` CI job parses. Unlike the single-file [[Micro]] benchmark, this
 * exercises the realistic distribution of many small/medium files, where per-parse fixed overhead
 * dominates.
 *
 * Corpora must be present locally (cloned by `communitytest/test`, or already on disk):
 * `community-projects/{munit,spark,dotty}`. `munit` (~150 files) is the default since it is small
 * enough for quick iteration; pass `-p corpus=spark` (or `dotty`) and bump `-p limit=...` for
 * heavier, more representative runs.
 *
 * {{{
 * // per-op allocation over the munit corpus:
 * sbt 'benchScalameta/Jmh/run -wi 3 -i 5 -f1 -t1 -prof gc ".*CorpusBenchmark.*"'
 *
 * // alloc flame graph over a 1500-file spark sample:
 * sbt 'benchScalameta/Jmh/run -wi 3 -i 5 -f1 -t1 -p corpus=spark -p limit=1500 \
 *   -prof "async:libPath=/opt/homebrew/lib/libasyncProfiler.dylib;event=alloc;output=flamegraph;dir=/tmp/ap" \
 *   ".*CorpusBenchmark.parse"'
 * }}}
 */
@State(Scope.Benchmark) @Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime)) @OutputTimeUnit(TimeUnit.MILLISECONDS)
class CorpusBenchmark {
  @Param(Array("munit"))
  var corpus: String = _

  @Param(Array("2000"))
  var limit: Int = _

  // Pre-read sources, so file I/O is not measured; dialect is resolved per file
  // the same way CommunityDottySuite does. We store the raw String (not an Input)
  // and build a fresh Input per op, because tokens are memoized per (Input,
  // dialect) -- reusing Inputs would measure cache hits, not the front end.
  private var inputs: Array[(String, Dialect)] = _

  @Setup
  def setup(): Unit = {
    val root = new File(BuildInfo.sourceroot, s"community-projects/$corpus").toPath
    require(
      Files.isDirectory(root),
      s"missing corpus: $root (run `sbt communitytest/test` once to clone it)",
    )
    val baseDialect: Dialect = corpus match {
      case "dotty" => dialects.Scala33
      case _ => dialects.Scala213
    }
    val files = {
      val stream = Files.walk(root)
      try stream.filter(p => p.toString.endsWith(".scala") && Files.isRegularFile(p)).sorted()
          .limit(limit.toLong).collect(Collectors.toList[java.nio.file.Path])
      finally stream.close()
    }
    val it = files.iterator
    val buf = Array.newBuilder[(String, Dialect)]
    while (it.hasNext) {
      val p = it.next()
      val pathStr = p.toString
      val dialect =
        if (baseDialect.allowSignificantIndentation)
          if (!pathStr.contains("/scala-2")) baseDialect else dialects.Scala213
        else if (!pathStr.contains("/scala-3")) baseDialect
        else dialects.Scala3
      buf += ((new String(Files.readAllBytes(p), "UTF-8"), dialect))
    }
    inputs = buf.result()
    require(inputs.nonEmpty, s"no .scala files found under $root")
  }

  @Benchmark
  def tokenize(bh: Blackhole): Unit = {
    var i = 0
    while (i < inputs.length) {
      val (src, dialect) = inputs(i)
      bh.consume((dialect, Input.String(src)).tokenize.get)
      i += 1
    }
  }

  @Benchmark
  def parse(bh: Blackhole): Unit = {
    var i = 0
    while (i < inputs.length) {
      val (src, dialect) = inputs(i)
      bh.consume((dialect, Input.String(src)).parse[Source].get)
      i += 1
    }
  }
}
