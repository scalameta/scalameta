package org.scalameta.bench

import java.nio.file._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import scala.tools.nsc._
import scala.tools.nsc.reporters._
import org.scalameta.bench.Scalameta._

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
    settings.pluginOptions.value ::= s"semanticdb:sourceroot:${bs.scalapDir}"
    settings.pluginOptions.value ::= s"semanticdb:failures:error"
    settings
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class QuickScalametaFullSynthetics extends Scalameta {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
  override def mkSettings(bs: BenchmarkState): Settings = {
    val settings = super.mkSettings(bs)
    settings.pluginOptions.value ::= "semanticdb:synthetics:all"
    settings.pluginOptions.value ::= "semanticdb:denotations:all"
    settings.pluginOptions.value ::= "semanticdb:mode:fat"
    settings
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class QuickScalametaFullDenotations extends Scalameta {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
  override def mkSettings(bs: BenchmarkState): Settings = {
    val settings = super.mkSettings(bs)
    settings.pluginOptions.value ::= "semanticdb:synthetics:none"
    settings.pluginOptions.value ::= "semanticdb:denotations:all"
    settings.pluginOptions.value ::= "semanticdb:mode:fat"
    settings
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class QuickScalametaFullContents extends Scalameta {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
  override def mkSettings(bs: BenchmarkState): Settings = {
    val settings = super.mkSettings(bs)
    settings.pluginOptions.value ::= "semanticdb:synthetics:none"
    settings.pluginOptions.value ::= "semanticdb:denotations:definitions"
    settings.pluginOptions.value ::= "semanticdb:mode:fat"
    settings
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class QuickScalametaBaseline extends Scalameta {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
  override def mkSettings(bs: BenchmarkState): Settings = {
    val settings = super.mkSettings(bs)
    settings.pluginOptions.value ::= "semanticdb:synthetics:none"
    settings.pluginOptions.value ::= "semanticdb:denotations:definitions"
    settings.pluginOptions.value ::= "semanticdb:mode:slim"
    settings
  }
}
