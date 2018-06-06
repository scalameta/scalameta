package scala.meta.internal.bench

import java.nio.file._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import scala.meta.internal.bench.Scalac._
import scala.tools.nsc._
import scala.tools.nsc.reporters._

object Scalac {
  @State(Scope.Benchmark)
  class BenchmarkState extends FileFixtures
}

trait Scalac {
  def runImpl(bs: BenchmarkState): Unit = {
    val settings = mkSettings(bs)
    val reporter = new StoreReporter
    val global = Global(settings, reporter)
    val run = new global.Run
    run.compile(bs.scalapFiles.map(_.toString))
    if (reporter.hasErrors) {
      reporter.infos.foreach(println)
      sys.error("compile failed")
    }
  }
  def mkSettings(bs: BenchmarkState): Settings = {
    val settings = new Settings
    settings.outdir.value = Files.createTempDirectory("scalac_").toString
    settings.usejavacp.value = true
    settings
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class ScalacBaseline extends Scalac {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class ScalacRangepos extends Scalac {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
  override def mkSettings(bs: BenchmarkState): Settings = {
    val settings = super.mkSettings(bs)
    settings.Yrangepos.value = true
    settings
  }
}
