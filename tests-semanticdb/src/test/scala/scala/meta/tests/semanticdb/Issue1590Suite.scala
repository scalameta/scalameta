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

// https://github.com/scalameta/scalameta/issues/1590
class Issue1590Suite extends FunSuite {

  private def symtab(root: AbsolutePath): Map[String, s.SymbolInformation] = FileIO
    .listAllFilesRecursively(root).iterator
    .filter(file => PathIO.extension(file.toNIO) == "semanticdb")
    .flatMap(file => s.TextDocuments.parseFrom(file.readAllBytes).documents).flatMap(_.symbols)
    .map(info => info.symbol -> info).toMap

  // with `params`, a method shows its parameters, so that two overloads cannot swap unseen
  private def declarations(
      infos: Map[String, s.SymbolInformation],
      symbol: String,
      params: Boolean,
  ) = infos.get(symbol).map(_.signature) match {
    case Some(sig: s.ClassSignature) => sig.declarations.toList.flatMap(_.symlinks).map(decl =>
        infos.get(decl).map(_.signature) match {
          case Some(m: s.MethodSignature) if params =>
            decl + m.parameterLists.flatMap(_.symlinks).mkString("[", ",", "]")
          case _ => decl
        },
      )
    case other => fail(s"expected ClassSignature for $symbol, obtained $other")
  }

  // compile the source with the semanticdb plugin, then convert its classfiles with metacp
  private def check(
      name: String,
      source: String,
      symbol: String,
      metac: List[String],
      metacp: List[String],
      pkg: String = "package a\n",
      params: Boolean = false,
  )(implicit loc: munit.Location): Unit = test(name) {
    val sourceroot = StringFS.fromString("/a.scala\n" + pkg + source)
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
    assertEquals(declarations(symtab(classes), symbol, params), metac, "metac")
    assertEquals(declarations(symtab(out), symbol, params), metacp, "metacp")
  }

  check(
    "defs with each access modifier",
    """|class Acc {
       |  private def m1 = ???
       |  private[this] def m2 = ???
       |  private[Acc] def m3 = ???
       |  protected def m4 = ???
       |  protected[this] def m5 = ???
       |  protected[Acc] def m6 = ???
       |  def m7 = ???
       |}
       |""".stripMargin,
    "a/Acc#",
    List(
      "a/Acc#`<init>`().",
      "a/Acc#m1().",
      "a/Acc#m2().",
      "a/Acc#m3().",
      "a/Acc#m4().",
      "a/Acc#m5().",
      "a/Acc#m6().",
      "a/Acc#m7().",
    ),
    List(
      "a/Acc#`<init>`().",
      "a/Acc#m1().",
      "a/Acc#m2().",
      "a/Acc#m4().",
      "a/Acc#m5().",
      "a/Acc#m6().",
      "a/Acc#m7().",
      "a/Acc#m3().",
    ),
  )

  check(
    "qualified-private members of each kind",
    """|class Acc {
       |  private[Acc] val v = 1
       |  private[Acc] lazy val l = 1
       |  private[Acc] class C
       |  private[Acc] object O
       |  private[Acc] type T = Int
       |  def m = 1
       |}
       |""".stripMargin,
    "a/Acc#",
    List(
      "a/Acc#`<init>`().",
      "a/Acc#v.",
      "a/Acc#l.",
      "a/Acc#C#",
      "a/Acc#O.",
      "a/Acc#T#",
      "a/Acc#m().",
    ),
    List(
      "a/Acc#`<init>`().",
      "a/Acc#m().",
      "a/Acc#v.",
      "a/Acc#l.",
      "a/Acc#C#",
      "a/Acc#O.",
      "a/Acc#T#",
    ),
  )

  check(
    "qualified-private var",
    """|class Acc {
       |  def m1 = 1
       |  private[Acc] var w = 1
       |  def m2 = 1
       |}
       |""".stripMargin,
    "a/Acc#",
    List("a/Acc#`<init>`().", "a/Acc#m1().", "a/Acc#w().", "a/Acc#`w_=`().", "a/Acc#m2()."),
    if (ScalaVersion.is212)
      List("a/Acc#`<init>`().", "a/Acc#m1().", "a/Acc#`w_=`().", "a/Acc#m2().", "a/Acc#w().")
    else List("a/Acc#`<init>`().", "a/Acc#m1().", "a/Acc#m2().", "a/Acc#w().", "a/Acc#`w_=`()."),
  )

  check(
    "qualified-private overload",
    """|class Acc {
       |  def m0 = 1
       |  private[Acc] def m(x: Int) = 1
       |  def m(s: String) = 2
       |  def n = 3
       |}
       |""".stripMargin,
    "a/Acc#",
    List("a/Acc#`<init>`().", "a/Acc#m0().", "a/Acc#m().", "a/Acc#m(+1).", "a/Acc#n()."),
    List("a/Acc#`<init>`().", "a/Acc#m0().", "a/Acc#m().", "a/Acc#n().", "a/Acc#m(+1)."),
  )

  check(
    "qualified-private val and lazy val between defs",
    """|class Acc {
       |  def m0 = 1
       |  private[Acc] val v = 1
       |  def m1 = 1
       |  private[Acc] lazy val l = 1
       |  def m2 = 1
       |}
       |""".stripMargin,
    "a/Acc#",
    List("a/Acc#`<init>`().", "a/Acc#m0().", "a/Acc#v.", "a/Acc#m1().", "a/Acc#l.", "a/Acc#m2()."),
    List("a/Acc#`<init>`().", "a/Acc#m0().", "a/Acc#m1().", "a/Acc#m2().", "a/Acc#v.", "a/Acc#l."),
  )

  check(
    "qualified-private object between defs",
    """|class Acc {
       |  def m0 = 1
       |  private[Acc] object O
       |  def m1 = 1
       |}
       |""".stripMargin,
    "a/Acc#",
    List("a/Acc#`<init>`().", "a/Acc#m0().", "a/Acc#O.", "a/Acc#m1()."),
    List("a/Acc#`<init>`().", "a/Acc#m0().", "a/Acc#m1().", "a/Acc#O."),
  )

  check(
    "member of an inner class qualified by the outer class",
    """|class Outer {
       |  class Inner {
       |    def m1 = 1
       |    private[Outer] def m2 = 1
       |    def m3 = 1
       |  }
       |}
       |""".stripMargin,
    "a/Outer#Inner#",
    List(
      "a/Outer#Inner#`<init>`().",
      "a/Outer#Inner#m1().",
      "a/Outer#Inner#m2().",
      "a/Outer#Inner#m3().",
    ),
    List(
      "a/Outer#Inner#`<init>`().",
      "a/Outer#Inner#m1().",
      "a/Outer#Inner#m3().",
      "a/Outer#Inner#m2().",
    ),
  )

  check(
    "member of an object qualified by its companion class",
    """|class Acc
       |object Acc {
       |  def m1 = 1
       |  private[Acc] def m2 = 1
       |  def m3 = 1
       |}
       |""".stripMargin,
    "a/Acc.",
    List("a/Acc.m1().", "a/Acc.m2().", "a/Acc.m3()."),
    List("a/Acc.m1().", "a/Acc.m2().", "a/Acc.m3()."),
  )

  check(
    "member of a trait qualified by the trait",
    """|trait T {
       |  def m1 = 1
       |  private[T] def m2 = 1
       |  def m3 = 1
       |}
       |""".stripMargin,
    "a/T#",
    List("a/T#`<init>`().", "a/T#m1().", "a/T#m2().", "a/T#m3()."),
    List("a/T#`<init>`().", "a/T#m1().", "a/T#m3().", "a/T#m2()."),
  )

  check(
    "member qualified by its package",
    """|class Acc {
       |  def m1 = 1
       |  private[a] def m2 = 1
       |  def m3 = 1
       |}
       |""".stripMargin,
    "a/Acc#",
    List("a/Acc#`<init>`().", "a/Acc#m1().", "a/Acc#m2().", "a/Acc#m3()."),
    List("a/Acc#`<init>`().", "a/Acc#m1().", "a/Acc#m2().", "a/Acc#m3()."),
  )

  check(
    "member of an object qualified by the object",
    """|object X {
       |  def m1 = 1
       |  private[X] def m2 = 1
       |  def m3 = 1
       |}
       |""".stripMargin,
    "a/X.",
    List("a/X.m1().", "a/X.m2().", "a/X.m3()."),
    List("a/X.m1().", "a/X.m2().", "a/X.m3()."),
  )

  check(
    "protected-qualified overload after a private one used by the companion",
    """|class Acc {
       |  private def m(x: Int) = 1
       |  protected[Acc] def m(s: String) = 2
       |  def n = 3
       |}
       |object Acc { def f(a: Acc) = a.m(1) }
       |""".stripMargin,
    "a/Acc#",
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m().[a/Acc#m().(x)]",
      "a/Acc#m(+1).[a/Acc#m(+1).(s)]",
      "a/Acc#n().[]",
    ),
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m().[a/Acc#m().(x)]",
      "a/Acc#m(+1).[a/Acc#m(+1).(s)]",
      "a/Acc#n().[]",
    ),
    params = true,
  )

  check(
    "package-qualified overload after a private one used by the companion",
    """|class Acc {
       |  private def m(x: Int) = 1
       |  private[a] def m(s: String) = 2
       |  def n = 3
       |}
       |object Acc { def f(a: Acc) = a.m(1) }
       |""".stripMargin,
    "a/Acc#",
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m().[a/Acc#m().(x)]",
      "a/Acc#m(+1).[a/Acc#m(+1).(s)]",
      "a/Acc#n().[]",
    ),
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m().[a/Acc#m().(x)]",
      "a/Acc#m(+1).[a/Acc#m(+1).(s)]",
      "a/Acc#n().[]",
    ),
    params = true,
  )

  check(
    "qualified-private overload after a private one used by the companion",
    """|class Acc {
       |  private def m(s: String) = 1
       |  private[Acc] def m(x: Int) = 2
       |  def n = 3
       |}
       |object Acc { def f(a: Acc) = a.m("") }
       |""".stripMargin,
    "a/Acc#",
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m().[a/Acc#m().(s)]",
      "a/Acc#m(+1).[a/Acc#m(+1).(x)]",
      "a/Acc#n().[]",
    ),
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m().[a/Acc#m().(s)]",
      "a/Acc#n().[]",
      "a/Acc#m(+1).[a/Acc#m(+1).(x)]",
    ),
    params = true,
  )

  check(
    "qualified-private overload before a package-qualified one",
    """|class Acc {
       |  private[Acc] def m(s: String) = 1
       |  private[a] def m(x: Int) = 2
       |  def n = 3
       |}
       |""".stripMargin,
    "a/Acc#",
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m().[a/Acc#m().(s)]",
      "a/Acc#m(+1).[a/Acc#m(+1).(x)]",
      "a/Acc#n().[]",
    ),
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m().[a/Acc#m().(x)]",
      "a/Acc#n().[]",
      "a/Acc#m(+1).[a/Acc#m(+1).(s)]",
    ),
    params = true,
  )

  check(
    "member of a class nested in an object",
    """|object X {
       |  class C {
       |    def m1 = 1
       |    private[C] def m2 = 1
       |    def m3 = 1
       |  }
       |}
       |""".stripMargin,
    "a/X.C#",
    List("a/X.C#`<init>`().", "a/X.C#m1().", "a/X.C#m2().", "a/X.C#m3()."),
    List("a/X.C#`<init>`().", "a/X.C#m1().", "a/X.C#m3().", "a/X.C#m2()."),
  )

  check(
    "member of an object nested in a class",
    """|class C {
       |  object Y {
       |    def m1 = 1
       |    private[C] def m2 = 1
       |    def m3 = 1
       |  }
       |}
       |""".stripMargin,
    "a/C#Y.",
    List("a/C#Y.m1().", "a/C#Y.m2().", "a/C#Y.m3()."),
    List("a/C#Y.m1().", "a/C#Y.m2().", "a/C#Y.m3()."),
  )

  check(
    "member of a class in a package object",
    """|package object p {
       |  class C {
       |    def m1 = 1
       |    private[C] def m2 = 1
       |    def m3 = 1
       |  }
       |}
       |""".stripMargin,
    "a/p/package.C#",
    List(
      "a/p/package.C#`<init>`().",
      "a/p/package.C#m1().",
      "a/p/package.C#m2().",
      "a/p/package.C#m3().",
    ),
    List(
      "a/p/package.C#`<init>`().",
      "a/p/package.C#m1().",
      "a/p/package.C#m3().",
      "a/p/package.C#m2().",
    ),
  )

  check(
    "member of a class in the empty package",
    """|class E {
       |  def m1 = 1
       |  private[E] def m2 = 1
       |  def m3 = 1
       |}
       |""".stripMargin,
    "_empty_/E#",
    List("_empty_/E#`<init>`().", "_empty_/E#m1().", "_empty_/E#m2().", "_empty_/E#m3()."),
    List("_empty_/E#`<init>`().", "_empty_/E#m1().", "_empty_/E#m3().", "_empty_/E#m2()."),
    pkg = "",
  )

  check(
    "two qualified-private overloads around a public one",
    """|class Acc {
       |  def m0 = 0
       |  private[Acc] def m(x: Int) = 1
       |  def m(b: Boolean) = 2
       |  private[Acc] def m(s: String) = 3
       |  def n = 4
       |}
       |""".stripMargin,
    "a/Acc#",
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m0().[]",
      "a/Acc#m().[a/Acc#m().(x)]",
      "a/Acc#m(+1).[a/Acc#m(+1).(b)]",
      "a/Acc#m(+2).[a/Acc#m(+2).(s)]",
      "a/Acc#n().[]",
    ),
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m0().[]",
      "a/Acc#m().[a/Acc#m().(b)]",
      "a/Acc#n().[]",
      "a/Acc#m(+1).[a/Acc#m(+1).(x)]",
      "a/Acc#m(+2).[a/Acc#m(+2).(s)]",
    ),
    params = true,
  )

  check(
    "member qualified by an enclosing object",
    """|object O {
       |  class C {
       |    def m1 = 1
       |    private[O] def m2 = 1
       |    def m3 = 1
       |  }
       |}
       |""".stripMargin,
    "a/O.C#",
    List("a/O.C#`<init>`().", "a/O.C#m1().", "a/O.C#m2().", "a/O.C#m3()."),
    List("a/O.C#`<init>`().", "a/O.C#m1().", "a/O.C#m2().", "a/O.C#m3()."),
  )

  check(
    "qualified-private overload between a private one used by the companion and a public one",
    """|class Acc {
       |  private def m(b: Boolean) = 1
       |  private[Acc] def m(x: Int) = 2
       |  def m(s: String) = 3
       |}
       |object Acc { def f(a: Acc) = a.m(true) }
       |""".stripMargin,
    "a/Acc#",
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m().[a/Acc#m().(b)]",
      "a/Acc#m(+1).[a/Acc#m(+1).(x)]",
      "a/Acc#m(+2).[a/Acc#m(+2).(s)]",
    ),
    List(
      "a/Acc#`<init>`().[]",
      "a/Acc#m().[a/Acc#m().(b)]",
      "a/Acc#m(+1).[a/Acc#m(+1).(s)]",
      "a/Acc#m(+2).[a/Acc#m(+2).(x)]",
    ),
    params = true,
  )

  check(
    "protected-qualified member",
    """|class Acc {
       |  def m1 = 1
       |  protected[Acc] def m2 = 1
       |  def m3 = 1
       |}
       |""".stripMargin,
    "a/Acc#",
    List("a/Acc#`<init>`().", "a/Acc#m1().", "a/Acc#m2().", "a/Acc#m3()."),
    List("a/Acc#`<init>`().", "a/Acc#m1().", "a/Acc#m2().", "a/Acc#m3()."),
  )

}
