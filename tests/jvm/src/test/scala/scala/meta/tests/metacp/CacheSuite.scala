package scala.meta.tests.metacp

import java.nio.file.Files
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Minute
import org.scalatest.time.Span
import org.scalatest.FunSuite
import org.scalatest.Ignore
import org.scalatest.tagobjects.Slow
import scala.meta.cli._
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PlatformFileIO
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.metacp.Settings
import scala.meta.testkit.DiffAssertions
import scala.meta.tests._

class CacheSuite extends FunSuite with DiffAssertions with TimeLimitedTests {

  private val tmp: AbsolutePath = AbsolutePath(Files.createTempDirectory("metacp"))
  tmp.toFile.deleteOnExit()
  override val timeLimit = Span(1L, Minute)

  def assertDirectoryListingMatches(jar: AbsolutePath, expected: String): Unit = {
    PlatformFileIO.withJarFileSystem(jar, create = false) { root =>
      val obtained = FileIO
        .listAllFilesRecursively(root)
        .files
        .map(_.toString)
        .sorted
        .mkString("\n")
      assertNoDiff(obtained, expected)
    }
  }

  test("scala-library-synthetics", Slow) {
    val settings = Settings()
      .withCacheDir(tmp.resolve("scala-library-synthetics"))
      .withScalaLibrarySynthetics(true)
    val reporter = Reporter()
    Metacp.process(settings, reporter) match {
      case Some(Classpath(List(scalaLibrarySyntheticsJar))) =>
        assertDirectoryListingMatches(
          scalaLibrarySyntheticsJar,
          """
            |META-INF/semanticdb/scala/Any.class.semanticdb
            |META-INF/semanticdb/scala/AnyRef.class.semanticdb
            |META-INF/semanticdb/scala/AnyVal.class.semanticdb
            |META-INF/semanticdb/scala/Nothing.class.semanticdb
            |META-INF/semanticdb/scala/Null.class.semanticdb
            |META-INF/semanticdb/scala/Singleton.class.semanticdb
            |META-INF/semanticdb/semanticdb.index
            |""".stripMargin
        )
      case other =>
        fail(s"unexpected metacp result: $other")
    }
  }

  test("scala-library", Slow) {
    val settings = Settings()
        .withCacheDir(tmp.resolve("scala-library"))
        .withClasspath(Library.scalaLibrary.classpath())
        .withDependencyClasspath(Library.jdk.classpath())
        .withScalaLibrarySynthetics(false)
    val reporter = Reporter()
    Metacp.process(settings, reporter) match {
      case result @ Some(Classpath(List(scalaLibrarySemanticdbJar))) =>
        PlatformFileIO.withJarFileSystem(scalaLibrarySemanticdbJar, create = false) { root =>
          assert(root.resolve("META-INF/semanticdb/scala/Predef.class.semanticdb").isFile)
          assert(root.resolve("META-INF/semanticdb/scala/package.class.semanticdb").isFile)
          assert(root.resolve("META-INF/semanticdb/scala/Function16.class.semanticdb").isFile)
          // Without caching, this test would fail because the test suite must run within 1 minute.
          1.to(100).foreach { _ =>
            assert(result == Metacp.process(settings, reporter))
          }
        }
      case other =>
        fail(s"unexpected metacp result: $other")
    }
  }

}
