package scala.meta.tests.metai

import java.nio.file.Files
import java.nio.file.StandardCopyOption
import org.scalatest.FunSuite
import scala.meta.cli.Metai
import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.metai.Settings
import scala.meta.tests.cli.CliSuite
import scala.meta.tests.metacp.Library

class MetaiErrorSuite extends FunSuite {
  test("no SemanticDB should error") {
    val scalaLibrary = Library.scalaLibrary.classpath().entries.head
    val tmp = Files.createTempFile("metai", "scala-library.jar")
    Files.copy(scalaLibrary.toNIO, tmp, StandardCopyOption.REPLACE_EXISTING)
    val settings = Settings().withClasspath(Classpath(tmp))
    val (out, _, stderr) = CliSuite.withReporter { reporter =>
      Metai.process(settings, reporter)
    }
    assert(out.entries.isEmpty, "metai should error for non-SemanticDB classpath")
    assert(stderr.contains("No SemanticDB: "))
    assert(stderr.contains("scala-library.jar"))
    FileIO.withJarFileSystem(AbsolutePath(tmp), create = false, close = true) { root =>
      val semanticidx = root.resolve("META-INF/semanticdb.semanticidx")
      assert(!semanticidx.isFile, semanticidx)
    }
  }
}
