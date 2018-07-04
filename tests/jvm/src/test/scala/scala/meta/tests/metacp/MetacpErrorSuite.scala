package scala.meta.tests.metacp

import java.nio.file._
import org.scalatest.FunSuite
import org.scalatest.tagobjects.Slow
import scala.meta.cli.Metacp
import scala.meta.io._
import scala.meta.metacp.Settings
import scala.meta.testkit.DiffAssertions
import scala.meta.tests.cli.CliSuite

class MetacpErrorSuite extends FunSuite with DiffAssertions {

  private val tmp = AbsolutePath(Files.createTempDirectory("metacp"))
  tmp.toFile.deleteOnExit()
  private val settings = Settings().withCacheDir(tmp)

  test("missing symbol", Slow) {
    val (classpath, out, err) = CliSuite.withReporter { reporter =>
      val scalametaSettings = settings.withClasspath(
        Library(
          "org.scalameta",
          "scalameta_2.12",
          "3.2.0",
          // Scalameta has public signatures that reference scala-reflect APIs even if scala-reflect not
          // declared as a provided dependency in pom.xml.
          provided = Nil
        ).classpath()
      )
      Metacp.process(scalametaSettings, reporter)
    }
    assert(classpath.isEmpty)
    assert(err.isEmpty)
    assertNoDiffOrPrintExpected(
      out,
      """
        |missing symbol: scala.reflect.macros.whitebox
        |missing symbol: scala.reflect.macros.blackbox
        |missing symbol: scala.reflect.api
        |NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. The provided classpath should also include JDK jars such as rt.jar
      """.stripMargin
    )
  }

}
