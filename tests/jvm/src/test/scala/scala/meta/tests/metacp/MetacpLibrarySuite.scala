package scala.meta.tests.metacp

import org.langmeta.internal.io.FileIO
import org.langmeta.io.AbsolutePath
import org.langmeta.io.RelativePath
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Minute
import org.scalatest.time.Span

import scala.meta.cli.Metacp
import scala.meta.tests.BuildInfo

class MetacpLibrarySuite extends BaseMetacpSuite with TimeLimitedTests {

  override val timeLimit = Span(1L, Minute)

  def assertDirectoryListingMatches(directory: AbsolutePath, expected: String): Unit = {
    val obtained = FileIO.listAllFilesRecursively(directory).files.mkString("\n")
    assertNoDiff(obtained, expected)
  }

  // This test generates a lot of semanticdb files, which is unnecessary for every test run.
  ignore("bootClasspath") {
    val obtained = Metacp.bootClasspath(tmp.resolve("jdk"))
    val Java = RelativePath("META-INF").resolve("semanticdb").resolve("java").resolve("lang")
    val rt = obtained.find(_.resolve(Java).resolve("Object.class.semanticdb").isFile).getOrElse {
      fail("No JDK path contains java/lang/Object")
    }
    assert(rt.resolve(Java).resolve("String.class.semanticdb").isFile)
    assert(rt.resolve(Java).resolve("Compiler.class.semanticdb").isFile)
    assert(rt.resolve(Java).resolve("Double.class.semanticdb").isFile)
    assert(rt.resolve(Java).resolve("IllegalArgumentException.class.semanticdb").isFile)
  }

  test("scala-library-synthetics") {
    val out = tmp.resolve("scala-library-synthetics")
    val obtained = Metacp.scalaLibrarySynthetics(BuildInfo.scalaVersion, out)
    assertDirectoryListingMatches(
      obtained,
      """
        |META-INF/semanticdb.semanticidx
        |META-INF/semanticdb/scala/Nothing.class.semanticdb
        |META-INF/semanticdb/scala/AnyVal.class.semanticdb
        |META-INF/semanticdb/scala/AnyRef.class.semanticdb
        |META-INF/semanticdb/scala/Any.class.semanticdb
        |""".stripMargin
    )
  }

  test("scala-library") {
    val out = tmp.resolve("scala-library")
    val obtained = Metacp.process(AbsolutePath(scalaLibraryJar), out)
    val Scala = RelativePath("META-INF").resolve("semanticdb").resolve("scala")
    assert(obtained.resolve(Scala).resolve("Predef.class.semanticdb").isFile)
    assert(obtained.resolve(Scala).resolve("package.class.semanticdb").isFile)
    assert(obtained.resolve(Scala).resolve("Function16.class.semanticdb").isFile)
    // Without caching, this test would fail because the test suite must run within 1 minute.
    1.to(100).foreach { _ =>
      assert(obtained == Metacp.process(AbsolutePath(scalaLibraryJar), out))
    }
  }

}
