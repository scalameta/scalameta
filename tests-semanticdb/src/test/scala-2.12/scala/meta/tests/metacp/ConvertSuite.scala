package scala.meta.tests.metacp

import scala.meta.cli._
import scala.meta.io._
import scala.meta.metacp._
import scala.meta.tests._
import scala.meta.tests.semanticdb.ScalaVersion

import java.nio.file._

import scala.concurrent.duration

import munit.FunSuite

class ConvertSuite extends FunSuite {

  override val munitTimeout = new duration.FiniteDuration(3, duration.MINUTES)

  val tmp = AbsolutePath(Files.createTempDirectory("metacp"))
  tmp.toFile.deleteOnExit()

  def runConversion(name: String, classpath: Classpath): Unit = {
    val (scalaOrg, toProcess) = classpath.entries.partition(_.toString.contains("scala-lang"))
    val filename = name.replaceAll("[^a-zA-Z0-9]", "_")
    val settings = Settings().withClasspath(Classpath(toProcess))
      .withDependencyClasspath(Classpath(scalaOrg)).withOut(tmp.resolve(filename)).withPar(true)
      .withUsejavacp(true)
    val reporter = Reporter().withSilentOut().withErr(System.err)
    val output = Metacp.process(settings, reporter)
    assert(output.isSuccess)
  }
  private def checkConversionSucceeds(library: Library): Unit =
    test(library.name)(runConversion(library.name, library.classpath()))

  Libraries.suite.foreach(checkConversionSucceeds)
}
