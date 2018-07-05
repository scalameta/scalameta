package scala.meta.tests.metacp

import java.nio.file._
import org.scalatest.FunSuite
import org.scalatest.tagobjects.Slow
import scala.collection.JavaConverters._
import scala.meta.cli.Metacp
import scala.meta.io._
import scala.meta.metacp.Settings
import scala.meta.testkit.DiffAssertions
import scala.meta.tests._
import scala.meta.tests.cli.CliSuite

class MetacpErrorSuite extends FunSuite with DiffAssertions {

  private val tmp = AbsolutePath(Files.createTempDirectory("metacp"))
  tmp.toFile.deleteOnExit()
  private val settings = Settings().withCacheDir(tmp)

  test("missing symbol 1", Slow) {
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
      """|missing symbol: java
         |missing symbol: scala.reflect.macros.whitebox
         |missing symbol: scala.reflect.macros.blackbox
         |missing symbol: scala.reflect.macros.Aliases
         |missing symbol: scala.reflect.api
         |missing symbol: scala.reflect.macros.Universe
         |NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. The provided classpath or classpaths should include the Scala library as well as JDK jars such as rt.jar.
      """.stripMargin
    )
  }

  test("missing symbol 2") {
    val tmp = Files.createTempDirectory("a_")
    tmp.toFile.deleteOnExit()
    val aclassFrom = Paths.get(BuildInfo.databaseClasspath).resolve("A.class")
    val aclassTo = tmp.resolve("A.class")
    Files.copy(aclassFrom, aclassTo)

    val (classpath, out, err) = CliSuite.withReporter { reporter =>
      val classpath = Classpath(AbsolutePath(tmp))
      Metacp.process(settings.withClasspath(classpath), reporter)
    }
    assert(classpath.isEmpty)
    assert(err.isEmpty)
    assertNoDiffOrPrintExpected(
      out,
      """|missing symbol: scala
         |NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. The provided classpath or classpaths should include the Scala library as well as JDK jars such as rt.jar.
      """.stripMargin
    )
  }

  test("missing symbol 3") {
    val cacheDir = Files.createTempDirectory("metacp")
    cacheDir.toFile.deleteOnExit()
    val resources = Paths.get("tests", "jvm", "src", "test", "resources")
    val manifest = resources.resolve("manifest.jar")
    val settings = Settings()
      .withCacheDir(AbsolutePath(cacheDir))
      .withClasspath(Classpath(AbsolutePath(manifest)))

    assert(!Files.list(cacheDir).iterator.hasNext)
    val (classpath, out, err) = CliSuite.withReporter { reporter =>
      Metacp.process(settings, reporter)
    }
    assert(classpath.isEmpty)
    assert(err.isEmpty)
    assertNoDiffOrPrintExpected(
      out,
      """|missing symbol: scala
         |NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. The provided classpath or classpaths should include the Scala library as well as JDK jars such as rt.jar.
      """.stripMargin
    )
    assert(!Files.list(cacheDir).iterator.hasNext)
  }
}
