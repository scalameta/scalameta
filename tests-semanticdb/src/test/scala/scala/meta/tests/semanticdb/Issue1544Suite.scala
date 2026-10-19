package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}

import munit.FunSuite

// https://github.com/scalameta/scalameta/issues/1544
class Issue1544Suite extends FunSuite with MetacpSnippets {

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
    val classes = compile("package a\n" + source)
    assertEquals(declarations(symtab(classes), symbol), metac, "metac")
    assertEquals(declarations(symtab(convert(classes)), symbol), metacp, "metacp")
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
