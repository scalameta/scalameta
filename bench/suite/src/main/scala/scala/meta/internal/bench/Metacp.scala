package scala.meta.internal.bench

import java.nio.file._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import scala.meta.cli._
import scala.meta.io._
import scala.meta.internal.bench.Metacp._
import scala.meta.metacp._
import scala.meta.tests.metacp._

object Metacp {
  @State(Scope.Benchmark)
  class BenchmarkState extends FileFixtures {
    val jdk = Library.jdk.classpath()
    val scalaLibrary = Library.scalaLibrary.classpath()
  }
}

trait Metacp {
  def runImpl(classpath: Classpath, dependencyClasspath: Classpath): Unit = {
    val tmp = Files.createTempDirectory("metacp_")
    val settings = Settings()
      .withOut(AbsolutePath(tmp))
      .withDependencyClasspath(dependencyClasspath)
      .withClasspath(classpath)
      .withScalaLibrarySynthetics(false)
    val reporter = Reporter().withSilentOut().withErr(System.err)
    val result = scala.meta.cli.Metacp.process(settings, reporter)
    if (!result.isSuccess) sys.error("conversion failed")
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class MetacpJDK extends Metacp {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs.jdk, Classpath(Nil))
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class MetacpScalaLibrary extends Metacp {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs.scalaLibrary, bs.jdk)
  }
}
