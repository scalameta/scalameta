package scala.meta.tests.semanticdb

import scala.meta.cli.Metacp
import scala.meta.internal.classpath.ClasspathUtils
import scala.meta.internal.io.{FileIO, PathIO}
import scala.meta.internal.semanticdb.scalac.SemanticdbPlugin
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.testkit.StringFS
import scala.meta.tests.cli.CliTestUtils
import scala.meta.tests.metacp.Library

import java.io.File
import java.nio.file.Files

import scala.tools.nsc.{Main => ScalacMain}

import munit.FunSuite

// https://github.com/scalameta/scalameta/issues/1544
class Issue1544Suite extends FunSuite {

  private def symtab(root: AbsolutePath): Map[String, s.SymbolInformation] = FileIO
    .listAllFilesRecursively(root).iterator
    .filter(file => PathIO.extension(file.toNIO) == "semanticdb")
    .flatMap(file => s.TextDocuments.parseFrom(file.readAllBytes).documents).flatMap(_.symbols)
    .map(info => info.symbol -> info).toMap

  private def declarations(infos: Map[String, s.SymbolInformation], symbol: String) =
    infos.get(symbol).map(_.signature) match {
      case Some(sig: s.ClassSignature) => sig.declarations.toList.flatMap(_.symlinks)
      case other => fail(s"expected ClassSignature for $symbol, obtained $other")
    }

  // compile the source with the semanticdb plugin, then convert its classfiles with metacp
  private def check(
      name: String,
      source: String,
      symbol: String,
      metac: List[String],
      metacp: List[String],
  )(implicit loc: munit.Location): Unit = test(name) {
    val sourceroot = StringFS.fromString("/a.scala\npackage a\n" + source)
    val classes = AbsolutePath(Files.createTempDirectory("semanticdb"))
    val plugin = ClasspathUtils.getClassPathEntries(classOf[SemanticdbPlugin].getClassLoader)
      .mkString(File.pathSeparator)
    val (_, _, compileErr) = CliTestUtils.communicate((out, err) =>
      Console.withOut(out)(Console.withErr(err)(ScalacMain.process(Array(
        "-d",
        classes.toString,
        "-Xplugin:" + plugin,
        "-Xplugin-require:semanticdb",
        "-Yrangepos",
        "-P:semanticdb:sourceroot:" + sourceroot.toString,
        "-cp",
        Library.scalaLibrary.classpath().syntax,
        sourceroot.resolve("a.scala").toString,
      )))),
    )
    assert(!ScalacMain.reporter.hasErrors, compileErr)
    val out = AbsolutePath(Files.createTempDirectory("metacp"))
    val (converted, _, convertErr) = CliTestUtils.withReporter(reporter =>
      Metacp.process(
        scala.meta.metacp.Settings().withOut(out).withClasspath(Classpath(classes))
          .withUsejavacp(true).withScalaLibrarySynthetics(false),
        reporter,
      ),
    )
    assert(converted.classpath.nonEmpty, convertErr)
    assertEquals(declarations(symtab(classes), symbol), metac, "metac")
    assertEquals(declarations(symtab(out), symbol), metacp, "metacp")
  }

  check(
    "case class, class, companion",
    """|object X {
       |  case class A()
       |  class B
       |  object A
       |}
       |""".stripMargin,
    "a/X.",
    List("a/X.A#", "a/X.B#", "a/X.A."),
    List("a/X.A#", "a/X.A.", "a/X.B#"),
  )

  check(
    "case class, def, companion",
    """|object X {
       |  case class A()
       |  def m: Int = 1
       |  object A
       |}
       |""".stripMargin,
    "a/X.",
    List("a/X.A#", "a/X.m().", "a/X.A."),
    List("a/X.A#", "a/X.A.", "a/X.m()."),
  )

  check(
    "class with a default, class, companion",
    """|object X {
       |  class A(x: Int = 1)
       |  class B
       |  object A
       |}
       |""".stripMargin,
    "a/X.",
    List("a/X.A#", "a/X.B#", "a/X.A."),
    List("a/X.A#", "a/X.A.", "a/X.B#"),
  )

  check(
    "two case classes, companions reversed",
    """|object X {
       |  case class A()
       |  case class B()
       |  object B
       |  object A
       |}
       |""".stripMargin,
    "a/X.",
    List("a/X.A#", "a/X.B#", "a/X.B.", "a/X.A."),
    List("a/X.A#", "a/X.A.", "a/X.B#", "a/X.B."),
  )

  check(
    "case class, class, companion in a class",
    """|class X {
       |  case class A()
       |  class B
       |  object A
       |}
       |""".stripMargin,
    "a/X#",
    List("a/X#`<init>`().", "a/X#A#", "a/X#B#", "a/X#A."),
    List("a/X#`<init>`().", "a/X#A#", "a/X#A.", "a/X#B#"),
  )

  check(
    "case class, companion, class",
    """|object X {
       |  case class A()
       |  object A
       |  class B
       |}
       |""".stripMargin,
    "a/X.",
    List("a/X.A#", "a/X.A.", "a/X.B#"),
    List("a/X.A#", "a/X.A.", "a/X.B#"),
  )

  check(
    "case class, class",
    """|object X {
       |  case class A()
       |  class B
       |}
       |""".stripMargin,
    "a/X.",
    List("a/X.A#", "a/X.A.", "a/X.B#"),
    List("a/X.A#", "a/X.A.", "a/X.B#"),
  )

  check(
    "class, class, companion",
    """|object X {
       |  class A
       |  class B
       |  object A
       |}
       |""".stripMargin,
    "a/X.",
    List("a/X.A#", "a/X.B#", "a/X.A."),
    List("a/X.A#", "a/X.B#", "a/X.A."),
  )

  check(
    "companion, case class, class",
    """|object X {
       |  object A
       |  case class A()
       |  class B
       |}
       |""".stripMargin,
    "a/X.",
    List("a/X.A.", "a/X.A#", "a/X.B#"),
    List("a/X.A.", "a/X.A#", "a/X.B#"),
  )

}
