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

  private def checkConversionSucceeds(library: Library): Unit = {
    test(library.name, Slow) {
      val settings = Settings().withClasspath(library.classpath()).withCacheDir(tmp).withPar(true)
      val reporter = Reporter().withOut(System.out).withErr(System.err)
      assert(Metacp.process(settings, reporter).nonEmpty)
    }
  }

  Libraries.suite.foreach(checkConversionSucceeds)
}
