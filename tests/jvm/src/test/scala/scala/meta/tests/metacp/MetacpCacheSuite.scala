package scala.meta.tests.metacp

import org.langmeta.internal.io.FileIO
import org.langmeta.internal.io.PlatformFileIO
import org.langmeta.io.AbsolutePath
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Minute
import org.scalatest.time.Span

import scala.meta.cli.Metacp
import scala.meta.internal.metacp.Settings
import scala.meta.tests.BuildInfo

class MetacpCacheSuite extends BaseMetacpSuite with TimeLimitedTests {

  override val timeLimit = Span(1L, Minute)

  def assertDirectoryListingMatches(jar: AbsolutePath, expected: String): Unit = {
    PlatformFileIO.withJarFileSystem(jar) { root =>
      val obtained = FileIO
        .listAllFilesRecursively(root)
        .files
        .map(_.toString)
        .sorted
        .mkString("\n")
      assertNoDiff(obtained, expected)
    }
  }

  test("scala-library-synthetics") {
    val settings = Settings(d = tmp.resolve("scala-library-synthetics"))
    val obtained = Metacp.scalaLibrarySynthetics(BuildInfo.scalaVersion, settings)
    assertDirectoryListingMatches(
      obtained,
      """
        |META-INF/semanticdb.semanticidx
        |META-INF/semanticdb/scala/Any.class.semanticdb
        |META-INF/semanticdb/scala/AnyRef.class.semanticdb
        |META-INF/semanticdb/scala/AnyVal.class.semanticdb
        |META-INF/semanticdb/scala/Nothing.class.semanticdb
        |""".stripMargin
    )
  }

  test("scala-library") {
    val settings = Settings(d = tmp.resolve("scala-library"))
    val jar = Metacp.processPath(AbsolutePath(scalaLibraryJar), settings)
    PlatformFileIO.withJarFileSystem(jar) { root =>
      assert(root.resolve("META-INF/semanticdb/scala/Predef.class.semanticdb").isFile)
      assert(root.resolve("META-INF/semanticdb/scala/package.class.semanticdb").isFile)
      assert(root.resolve("META-INF/semanticdb/scala/Function16.class.semanticdb").isFile)
      // Without caching, this test would fail because the test suite must run within 1 minute.
      1.to(100).foreach { _ =>
        assert(jar == Metacp.processPath(AbsolutePath(scalaLibraryJar), settings))
      }
    }
  }

}
