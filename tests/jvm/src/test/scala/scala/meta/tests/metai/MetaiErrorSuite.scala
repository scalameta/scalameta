package scala.meta.tests.metai

import java.nio.file.Files
import java.nio.file.StandardCopyOption
import org.scalatest.FunSuite
import scala.meta.cli.Metai
import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.metai.Settings
import scala.meta.testkit.DiffAssertions
import scala.meta.tests.BuildInfo
import scala.meta.tests.cli.CliSuite
import scala.meta.tests.metacp.Library

class MetaiErrorSuite extends FunSuite with DiffAssertions {
  test("no META-INF/semanticdb should error") {
    val scalaLibrary = Library.scalaLibrary.classpath().entries.head
    val tmp1 = Files.createTempFile("metai", "_scala-library.jar")
    Files.copy(scalaLibrary.toNIO, tmp1, StandardCopyOption.REPLACE_EXISTING)
    val tmp2 = Files.createTempDirectory("metai")
    Files.createDirectories(tmp2.resolve("META-INF/semanticdb"))
    val cp = Classpath(List(AbsolutePath(tmp1), AbsolutePath(tmp2)))
    val settings = Settings().withClasspath(cp)
    val (result, out, err) = CliSuite.withReporter { reporter =>
      Metai.process(settings, reporter)
    }
    assert(!result.isSuccess, "metai should error for non-META-INF/semanticdb entries")
    val expectedStdout =
      s"""|{
          |  "status": {
          |    "$tmp1": false,
          |    "$tmp2": true
          |  }
          |}
          |""".stripMargin
    val expectedStderr =
      s"""|No META-INF/semanticdb found in $tmp1
          |""".stripMargin
    assertNoDiffOrPrintExpected(out, expectedStdout)
    assertNoDiffOrPrintExpected(err, expectedStderr)
    FileIO.withJarFileSystem(AbsolutePath(tmp1), create = false, close = true) { root =>
      val semanticidx1 = root.resolve("META-INF/semanticdb.semanticidx")
      assert(!semanticidx1.isFile, semanticidx1)
    }
    val semanticidx2 = AbsolutePath(tmp2.resolve("META-INF/semanticdb.semanticidx"))
    assert(semanticidx2.isFile, semanticidx2)
  }
}
