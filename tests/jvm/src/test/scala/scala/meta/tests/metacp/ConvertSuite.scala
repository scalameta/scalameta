package scala.meta.tests.metacp

import java.nio.file._
import org.scalatest._
import org.scalatest.tagobjects._
import scala.meta.cli._
import scala.meta.io._
import scala.meta.metacp._

class ConvertSuite extends FunSuite {
  val tmp = AbsolutePath(Files.createTempDirectory("metacp"))
  tmp.toFile.deleteOnExit()

  def runConversion(name: String, classpath: Classpath): Unit = {
    val (scalaOrg, toProcess) = classpath.entries.partition(_.toString.contains("scala-lang"))
    val filename = name.replaceAll("[^a-zA-Z0-9]", "_")
    val settings = Settings()
      .withClasspath(Classpath(toProcess))
      .withDependencyClasspath(Library.jdk.classpath() ++ Classpath(scalaOrg))
      .withOut(tmp.resolve(filename))
      .withPar(true)
    val reporter = Reporter().withSilentOut().withErr(System.err)
    val output = Metacp.process(settings, reporter)
    assert(output.isSuccess)
  }
  private def checkConversionSucceeds(library: Library): Unit = {
    test(library.name, Slow) {
      runConversion(library.name, library.classpath())
    }
  }

  Libraries.suite.foreach(checkConversionSucceeds)
}
