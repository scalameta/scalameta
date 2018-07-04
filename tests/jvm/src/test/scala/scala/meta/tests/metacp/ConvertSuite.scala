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

  def runConversion(classpath: Classpath): Unit = {
    val (scalaOrg, toProcess) = classpath.entries.partition(_.toString.contains("scala-lang"))
    val settings = Settings()
      .withClasspath(Classpath(toProcess))
      .withDependencyClasspath(Library.jdk.classpath() ++ Classpath(scalaOrg))
      .withCacheDir(tmp)
      .withPar(true)
    val reporter = Reporter().withOut(System.out).withErr(System.err)
    assert(Metacp.process(settings, reporter).nonEmpty)
  }
  private def checkConversionSucceeds(library: Library): Unit = {
    test(library.name, Slow) {
      runConversion(library.classpath())
    }
  }

  Libraries.suite.foreach(checkConversionSucceeds)
}
