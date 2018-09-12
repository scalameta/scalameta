package scala.meta.tests.metacp

import java.nio.file._
import org.scalatest.FunSuite
import org.scalatest.tagobjects.Slow
import scala.collection.JavaConverters._
import scala.meta.cli._
import scala.meta.io._
import scala.meta.metacp.Settings
import scala.meta.testkit.DiffAssertions
import scala.meta.tests._
import scala.meta.tests.cli.CliSuite

class MetacpErrorSuite extends FunSuite with DiffAssertions {

  private val tmp = AbsolutePath(Files.createTempDirectory("metacp"))
  tmp.toFile.deleteOnExit()
  private val settings = Settings().withOut(tmp)

  test("missing symbol 1", Slow) {
    val (result, out, err) = CliSuite.withReporter { reporter =>
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
    assert(result.classpath.isEmpty)
    assert(out.nonEmpty)
    assertNoDiffOrPrintExpected(
      err.replaceAll("(missing symbol: .*?) .*", "$1"),
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

    val (result, out, err) = CliSuite.withReporter { reporter =>
      val classpath = Classpath(AbsolutePath(tmp))
      Metacp.process(settings.withClasspath(classpath), reporter)
    }
    assert(result.classpath.isEmpty)
    assertNoDiffOrPrintExpected(
      out,
      s"""|{
          |  "status": {
          |    "$tmp": ""
          |  },
          |  "scalaLibrarySynthetics": ""
          |}
          |""".stripMargin
    )
    assertNoDiffOrPrintExpected(
      err,
      s"""|missing symbol: scala in $tmp
          |NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. The provided classpath or classpaths should include the Scala library as well as JDK jars such as rt.jar.
      """.stripMargin
    )
  }

  test("missing symbol 3") {
    val output = Files.createTempDirectory("metacp")
    output.toFile.deleteOnExit()
    val resources = Paths.get("tests", "jvm", "src", "test", "resources")
    val manifest = resources.resolve("manifest.jar")
    val settings = Settings()
      .withOut(AbsolutePath(output))
      .withClasspath(Classpath(AbsolutePath(manifest)))

    assert(!Files.list(output).iterator.hasNext)
    val (result, out, err) = CliSuite.withReporter { reporter =>
      Metacp.process(settings, reporter)
    }
    assert(result.classpath.isEmpty)
    assertNoDiffOrPrintExpected(
      out,
      s"""|{
          |  "status": {
          |    "${AbsolutePath(manifest)}": ""
          |  },
          |  "scalaLibrarySynthetics": ""
          |}
          |""".stripMargin
    )
    assertNoDiffOrPrintExpected(
      err,
      s"""|missing symbol: scala in ${AbsolutePath(manifest)}
          |NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. The provided classpath or classpaths should include the Scala library as well as JDK jars such as rt.jar.
      """.stripMargin
    )
    // TODO(olafurpg) fix this assertion before merging PR!
    // assert(!Files.list(output).iterator.hasNext)
  }

  test("missing symbol 4") {
    val input = Files.createTempDirectory("a_")
    input.toFile.deleteOnExit()
    val aclassFrom = Paths.get(BuildInfo.databaseClasspath).resolve("A.class")
    val aclassTo = input.resolve("A.class")
    Files.copy(aclassFrom, aclassTo)

    val output = Files.createTempDirectory("out_")
    output.toFile.deleteOnExit()
    val settings = Settings()
      .withOut(AbsolutePath(output))
      .withClasspath(Classpath(AbsolutePath(input)))
      .withStubBrokenSignatures(true)
      .withLogBrokenSignatures(true)

    val (result, out, err) = CliSuite.withReporter { reporter =>
      Metacp.process(settings, reporter)
    }
    assert(result.isSuccess)
    assertNoDiffOrPrintExpected(
      err,
      s"""|broken signature for _empty_/A#: missing symbol: scala
          |broken signature for _empty_/A#b().: missing symbol: <empty>.B
      """.stripMargin
    )
  }
}
