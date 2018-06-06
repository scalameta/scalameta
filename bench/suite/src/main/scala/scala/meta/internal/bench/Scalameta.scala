package scala.meta.internal.bench

import java.nio.file._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import scala.meta.internal.bench.Scalameta._
import scala.tools.nsc._
import scala.tools.nsc.reporters._

object Scalameta {
  @State(Scope.Benchmark)
  class BenchmarkState extends FileFixtures {
    @Param(value = Array(""))
    var semanticdbScalacJar: String = _
  }
}

trait Scalameta {
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
    val outdir = Paths.get(settings.outdir.value)
    if (!Files.exists(outdir.resolve("META-INF/semanticdb"))) {
      sys.error(s"no .semanticdb files found in $outdir")
    }
  }
  def mkSettings(bs: BenchmarkState): Settings = {
    val settings = new Settings
    settings.outdir.value = Files.createTempDirectory("scalac_").toString
    settings.usejavacp.value = true
    settings.Yrangepos.value = true
    settings.plugin.value ::= bs.semanticdbScalacJar
    settings.require.value ::= "semanticdb"
    settings.pluginOptions.value ::= s"semanticdb:failures:error"
    settings.pluginOptions.value ::= s"semanticdb:sourceroot:${bs.scalapDir}"
    settings
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class ScalametaBaseline extends Scalameta {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
  override def mkSettings(bs: BenchmarkState): Settings = {
    val settings = super.mkSettings(bs)
    settings.pluginOptions.value ::= s"semanticdb:text:off"
    settings.pluginOptions.value ::= s"semanticdb:symbols:on"
    settings.pluginOptions.value ::= s"semanticdb:occurrences:on"
    settings.pluginOptions.value ::= s"semanticdb:diagnostics:on"
    settings.pluginOptions.value ::= s"semanticdb:synthetics:off"
    settings
  }
}
