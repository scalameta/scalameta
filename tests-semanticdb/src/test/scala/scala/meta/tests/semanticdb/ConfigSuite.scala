package scala.meta.tests.semanticdb

import scala.meta.cli.Metac
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.scalac.SemanticdbPaths
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, RelativePath}
import scala.meta.metac
import scala.meta.testkit.StringFS
import scala.meta.tests.cli.CliTestUtils
import scala.meta.tests.metacp.Library

import java.nio.file.Files

import munit.FunSuite

class ConfigSuite extends FunSuite {

  val A = RelativePath("A.scala")

  def generateTargetroot(): AbsolutePath = {
    val target = AbsolutePath(Files.createTempDirectory("semanticdb"))
    target.toFile.deleteOnExit()
    target
  }

  def check(
      topt: munit.TestOptions,
      scalacArgs: List[String],
      input: String,
      fn: s.TextDocument => Unit,
      targetroot: AbsolutePath = generateTargetroot(),
  )(implicit loc: munit.Location): Unit = test(topt) {
    val sourceroot = StringFS.fromString(input)
    val (metacIsSuccess, metacOut, metacErr) = CliTestUtils.withReporter { reporter =>
      val settings = metac.Settings().withScalacArgs(
        scalacArgs ++ List(
          "-d",
          targetroot.toString,
          "-P:semanticdb:sourceroot:" + sourceroot.toString,
          "-cp",
          Library.scalaLibrary.classpath().syntax,
          sourceroot.resolve(A).toString,
        ),
      )
      Metac.process(settings, reporter)
    }
    assert(metacIsSuccess, metacErr)
    assert(metacOut.isEmpty)
    assert(metacErr.isEmpty)
    val semanticdbPath = SemanticdbPaths.toSemanticdb(A, targetroot)
    val docs = s.TextDocuments.parseFrom(semanticdbPath.readAllBytes)
    assertEquals(docs.documents.length, 1, docs.toProtoString)
    fn(docs.documents.head)
  }

  check(
    "symbols:local-only",
    List("-P:semanticdb:symbols:local-only"),
    """|
       |/A.scala
       |object A {
       |  val x = {
       |    val y = 1 // local0
       |    val z = 2 // local1
       |    y + z
       |  }
       |}
       |""".stripMargin,
    { doc =>
      val symbols = doc.symbols.map(_.symbol).sorted.mkString("\n")
      assertNoDiff(
        symbols,
        """|local0
           |local1
           |""".stripMargin,
      )
    },
  )

  check(
    "symbols:none",
    List("-P:semanticdb:symbols:none"),
    """|
       |/A.scala
       |object A {
       |}
       |""".stripMargin,
    doc => assert(doc.symbols.isEmpty),
  )

  private def symbolsOf(doc: s.TextDocument): String = doc.symbols.map(_.symbol).sorted
    .mkString("\n")

  check(
    "symbols:local-only, a local case class",
    List("-P:semanticdb:symbols:local-only"),
    """|
       |/A.scala
       |object A {
       |  def m = {
       |    case class L(a: Int) { var v = 1 }
       |    L(1).a
       |  }
       |}
       |""".stripMargin,
    { doc =>
      assert(doc.symbols.forall(_.symbol.isLocal))
      assert(doc.symbols.exists(_.displayName == "copy"))
    },
  )

  check(
    "symbols:none, a local case class",
    List("-P:semanticdb:symbols:none"),
    """|
       |/A.scala
       |object A {
       |  def m = {
       |    case class L(a: Int) { var v = 1 }
       |    L(1).a
       |  }
       |}
       |""".stripMargin,
    doc => assert(doc.symbols.isEmpty),
  )

  check(
    "symbols:local-only, a SAM lambda",
    List("-P:semanticdb:symbols:local-only"),
    """|
       |/A.scala
       |object A {
       |  val r: Runnable = () => ()
       |}
       |""".stripMargin,
    { doc =>
      val expected =
        """|local0
           |local1
           |""".stripMargin
      assertNoDiff(symbolsOf(doc), expected)
    },
  )

  check(
    "symbols:local-only, an implicit class",
    List("-P:semanticdb:symbols:local-only"),
    """|
       |/A.scala
       |object A {
       |  implicit class R(x: Int) { def r = x }
       |}
       |""".stripMargin,
    doc => assertNoDiff(symbolsOf(doc), ""),
  )

  check(
    "symbols:all, an implicit class",
    List("-P:semanticdb:symbols:all"),
    """|
       |/A.scala
       |object A {
       |  implicit class R(x: Int) { def r = x }
       |}
       |""".stripMargin,
    { doc =>
      val expected =
        """|_empty_/A.
           |_empty_/A.R#
           |_empty_/A.R#`<init>`().
           |_empty_/A.R#`<init>`().(x)
           |_empty_/A.R#r().
           |_empty_/A.R#x.
           |_empty_/A.R().
           |_empty_/A.R().(x)
           |""".stripMargin
      assertNoDiff(symbolsOf(doc), expected)
    },
  )

  check(
    "symbols:none, an implicit class",
    List("-P:semanticdb:symbols:none"),
    """|
       |/A.scala
       |object A {
       |  implicit class R(x: Int) { def r = x }
       |}
       |""".stripMargin,
    doc => assertNoDiff(symbolsOf(doc), ""),
  )

  check(
    "symbols:local-only, a default argument",
    List("-P:semanticdb:symbols:local-only"),
    """|
       |/A.scala
       |object A {
       |  def f(a: Int = 1) = a
       |}
       |""".stripMargin,
    doc => assertNoDiff(symbolsOf(doc), ""),
  )

  check(
    "symbols:none, a default argument",
    List("-P:semanticdb:symbols:none"),
    """|
       |/A.scala
       |object A {
       |  def f(a: Int = 1) = a
       |}
       |""".stripMargin,
    doc => assertNoDiff(symbolsOf(doc), ""),
  )

  check(
    "symbols:local-only, a tuple pattern",
    List("-P:semanticdb:symbols:local-only"),
    """|
       |/A.scala
       |object A {
       |  val (a, b) = (1, 2)
       |}
       |""".stripMargin,
    doc => assertNoDiff(symbolsOf(doc), ""),
  )

  check(
    "symbols:none, a tuple pattern",
    List("-P:semanticdb:symbols:none"),
    """|
       |/A.scala
       |object A {
       |  val (a, b) = (1, 2)
       |}
       |""".stripMargin,
    doc => assertNoDiff(symbolsOf(doc), ""),
  )

  check(
    "symbols:all",
    List("-P:semanticdb:symbols:all"),
    """|
       |/A.scala
       |object A {
       |  val x = {
       |    val y = 2
       |    2
       |  }
       |}
       |""".stripMargin,
    { doc =>
      val obtained = doc.symbols.map(i => i.symbol + " " + i.displayName).sorted.mkString("\n")
      val expected =
        """|_empty_/A. A
           |_empty_/A.x. x
           |local0 y
           |""".stripMargin

      assertNoDiff(obtained, expected)
    },
  )

  private val customTargetroot = generateTargetroot()
  check(
    "targetroot:<custom>",
    List(s"-P:semanticdb:targetroot:$customTargetroot"),
    """|/A.scala
       |object A
       |""".stripMargin,
    { doc =>
      val Seq(occurrence) = doc.occurrences
      assertEquals(occurrence.symbol, "_empty_/A.")
    },
    targetroot = customTargetroot,
  )

  check(
    "buildtarget:<custom>",
    List("-P:semanticdb:buildtarget:my-build-target"),
    """|/A.scala
       |object A
       |""".stripMargin,
    doc => assertEquals(doc.buildTarget, "my-build-target"),
  )

  check(
    "buildtarget:<unset>",
    Nil,
    """|/A.scala
       |object A
       |""".stripMargin,
    doc => assert(doc.buildTarget.isEmpty),
  )

  check(
    "md5:off",
    List(s"-P:semanticdb:md5:off"),
    """|/A.scala
       |object A
       |""".stripMargin,
    doc => assert(doc.md5.isEmpty),
  )

  check(
    "synthetics:off",
    List(s"-P:semanticdb:synthetics:off"),
    """|/A.scala
       |object A {
       |   List(1).map(_ + 1)
       |}
       |""".stripMargin,
    doc => assert(doc.synthetics.isEmpty),
  )

  check(
    "synthetics:on",
    List(s"-P:semanticdb:synthetics:on"),
    """|/A.scala
       |object A {
       |   List(1).map(_ + 1)
       |}
       |""".stripMargin,
    doc => assert(doc.synthetics.nonEmpty),
  )

}
