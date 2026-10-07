package scala.meta.tests.prettyprinters

import scala.meta._
import scala.meta.internal.prettyprinters.TreeSyntax
import scala.meta.prettyprinters.Show

/**
 * This class, unlike similar SyntacticSuite, does not reset origins. Instead it uses runTestAssert
 * to force reprinting of syntax.
 */

class TreeSyntaxSuite extends scala.meta.tests.parsers.ParseSuite {

  implicit val dialect: Dialect = dialects.Scala211

  private def testBlock(statStr: String, needNL: Boolean, syntaxStr: String = null)(implicit
      loc: munit.Location,
  ): Unit = {
    val stat = statStr.trim // make sure no trailing newlines
    def testWithSuffix(suffix: String): Unit = test(s"${loc.line}: $stat [$suffix]") {
      val statSyntax = Option(syntaxStr).getOrElse(stat).replace("\n", "\n  ")

      val indentedSuffix = suffix.replace("\n", "\n  ")
      val expectedSyntax =
        s"""|{
            |  $statSyntax${if (needNL) "\n" else ""}
            |  $indentedSuffix
            |}""".stripMargin.lf2nl

      val treeWithSemi = templStat(s"{$stat;$suffix}")
      val treeWithSemiStructure = treeWithSemi.structure
      assertNoDiff(TreeSyntax.reprint(treeWithSemi).toString, expectedSyntax)

      def getTreeWithNL() = templStat(s"{$stat\n$suffix}")
      if (needNL) scala.util.Try(getTreeWithNL())
        .foreach(treeWithNL => assertNotEquals(treeWithNL.structure, treeWithSemiStructure))
      else {
        val treeWithNL = getTreeWithNL()
        assertNoDiff(treeWithNL.reprint, expectedSyntax)
        assertNoDiff(treeWithNL.structure, treeWithSemiStructure)
      }
    }
    testWithSuffix(
      """|{
         |  a
         |}""".stripMargin,
    )
    testWithSuffix(
      """|{
         |  a
         |} + {
         |  b
         |}""".stripMargin,
    )
    testWithSuffix(
      """|{
         |  a
         |}.b""".stripMargin,
    )
  }

  private def testBlockAddNL(t: String, expected: String = null)(implicit loc: munit.Location) =
    testBlock(t, true, expected)
  private def testBlockNoNL(t: String, expected: String = null)(implicit loc: munit.Location) =
    testBlock(t, false, expected)

  private def testBlockAfterDef(f: String => Unit)(implicit loc: munit.Location): Unit =
    Seq("val", "var", "def").foreach(f)
  private def testBlockAfterClass(f: String => Unit)(implicit loc: munit.Location): Unit =
    Seq("class", "object", "trait").foreach(f)

  testBlockAfterDef(k => testBlockAddNL(s"$k foo: Int"))
  testBlockNoNL("class foo { self => }")
  testBlockNoNL("class foo { _: Int => }")
  testBlockNoNL("type foo")
  testBlockAfterDef(k => testBlockAddNL(s"$k foo: Int = 1"))
  testBlockAfterDef(k => testBlockNoNL(s"$k foo: Int = {1}", s"$k foo: Int = {\n  1\n}"))
  testBlockAddNL("def a = macro someMacro")
  testBlockNoNL("def a = macro return {\n  foo\n}")
  testBlockAddNL("type foo = Int")
  testBlockAfterClass(k => testBlockAddNL(s"$k Foo"))
  testBlockAfterClass(k => testBlockNoNL(s"$k Foo { val foo = 1 }"))
  testBlockAfterClass(k => testBlockAddNL(s"$k Foo extends Bar"))
  testBlockAfterClass(k =>
    testBlockAddNL(
      s"$k Foo extends { val foo = 1 } with Bar",
      s"""|$k Foo extends {
          |  val foo = 1
          |} with Bar""".stripMargin,
    ),
  )
  testBlockAfterClass(k =>
    testBlockNoNL(
      s"$k Foo extends { val foo = 1 } with Bar { val bar = 2 }",
      s"""|$k Foo extends {
          |  val foo = 1
          |} with Bar { val bar = 2 }""".stripMargin,
    ),
  )
  testBlockAddNL("this")
  testBlockAddNL("Foo")
  testBlockAddNL("Foo.bar")
  testBlockAddNL("Foo.bar")
  testBlockAddNL("10")
  testBlockAddNL("-10")
  testBlockAddNL("~10", "-11")
  testBlockAddNL("10.0d")
  testBlockAddNL("-10.0d")
  testBlockAddNL("true")
  testBlockAddNL("false")
  testBlockAddNL("!true", "false")
  testBlockAddNL("!false", "true")
  testBlockNoNL("-{10}", "-{\n  10\n}")
  testBlockAddNL("foo(bar)")
  testBlockAddNL("foo[Bar]")
  testBlockAddNL("foo {\n  bar\n}")
  testBlockAddNL("foo {\n  bar\n} {\n  baz\n}")
  testBlockAddNL("foo + ()")
  testBlockAddNL("foo + bar")
  testBlockNoNL("foo + {\n  bar\n}")
  testBlockAddNL("foo + (a, b)")
  testBlockAddNL("foo = bar")
  testBlockNoNL("foo = {bar}", "foo = {\n  bar\n}")
  testBlockAddNL("return foo")
  testBlockNoNL("return {foo}", "return {\n  foo\n}")
  testBlockAddNL("throw foo")
  testBlockNoNL("throw {foo}", "throw {\n  foo\n}")
  testBlockAddNL("foo: Int")
  testBlockNoNL("foo: @annotation")
  testBlockAddNL("(foo, bar)")
  testBlockNoNL("{foo}", "{\n  foo\n}")
  testBlockAddNL("if (cond) foo")
  testBlockNoNL("if (cond) {foo}", "if (cond) {\n  foo\n}")
  testBlockAddNL("if (cond) foo else bar")
  testBlockNoNL("if (cond) foo else {bar}", "if (cond) foo else {\n  bar\n}")
  testBlockNoNL("foo match { case _ => () }", "foo match {\n  case _ => ()\n}")
  testBlockAddNL("try foo finally bar")
  testBlockNoNL("try foo finally {bar}", "try foo finally {\n  bar\n}")
  testBlockNoNL("try foo catch { case _ => () }", "try foo catch {\n  case _ => ()\n}")
  testBlockAddNL("try foo")
  testBlockNoNL("try {foo}", "try {\n  foo\n}")
  testBlockAddNL("try foo catch bar finally baz")
  testBlockNoNL("try foo catch bar finally {baz}", "try foo catch bar finally {\n  baz\n}")
  testBlockAddNL("try foo catch bar")
  testBlockNoNL("try foo catch {bar}", "try foo catch {\n  bar\n}")
  testBlockAddNL("val func = foo => bar")
  testBlockNoNL("val func = { case foo => bar }", "val func = {\n  case foo => bar\n}")
  testBlockAddNL("while (foo) bar")
  testBlockNoNL("while (foo) {bar}", "while (foo) {\n  bar\n}")
  testBlockNoNL("do foo while (bar)")
  testBlockAddNL("for (foo <- bar) baz")
  testBlockNoNL("for (foo <- bar) {baz}", "for (foo <- bar) {\n  baz\n}")
  testBlockAddNL("for (foo <- bar) yield baz")
  testBlockNoNL("for (foo <- bar) yield {baz}", "for (foo <- bar) yield {\n  baz\n}")
  testBlockAddNL("new Foo")
  testBlockNoNL("new Foo { val bar = 1 }")
  testBlockNoNL("foo _")
  // Term.Repeated can only be in a block by itself, otherwise is invalid syntax
  testBlockAddNL("foo { bar: _* }", "foo {\n  bar: _*\n}")
  testBlockAddNL("s\"foo\"")
  testBlockAddNL("<h1>{Foo}</h1>", "<h1>{\n  Foo\n}</h1>")
  testBlockNoNL("import foo.Bar")
  Seq("true", "'a'", "1.0d", "1.0f", "1", "1L", "null", "\"foo\"", "'foo", "()")
    .foreach(testBlockAddNL(_))

  locally {
    import scala.meta.prettyprinters.Show
    import scala.meta.prettyprinters.Show.{blank, newline => n, sequence => s}
    def checkBlankLine(name: String, res: Show.Result, expected: String)(implicit
        loc: munit.Location,
    ): Unit = test(s"blank line: $name")(assertEquals(res.toString.replace("\r\n", "\n"), expected))
    checkBlankLine("after text", s("foo", blank(), "bar"), "foo\nbar")
    checkBlankLine("after a newline", s("foo", n(), blank(), "bar"), "foo\n\nbar")
    checkBlankLine("after text that ends its line", s("foo\n", blank(), "bar"), "foo\n\nbar")
    checkBlankLine(
      "after text that ends in a blank line",
      s("foo\n\n", blank(), "bar"),
      "foo\n\n\nbar",
    )
    checkBlankLine("after a line of spaces", s("foo\n  \n", blank(), "bar"), "foo\n  \n\nbar")
    checkBlankLine("after a blank line", s("foo", blank(), blank(), "bar"), "foo\n\nbar")
    checkBlankLine("then a newline", s("foo", blank(), n("bar")), "foo\n\nbar")
    checkBlankLine("at the start", s(blank(), "bar"), "\nbar")
    checkBlankLine("at the end", s("foo", blank()), "foo\n")
    checkBlankLine("at the end, after a newline", s("foo", n(), blank()), "foo\n\n")
  }

  test("empty block with a comment, printed without comments") {
    val tree = Term.Block.newBuilder(Nil).endComment(Seq("/* c */")).result()
    assertNoDiff(tree.syntax, "{}")
  }

  test("case: one-stat block body, printed without comments") {
    val tree = templStat(
      """|x match {
         |  case 1 => { a }
         |}
         |""".stripMargin,
    )
    val printed = TreeSyntax.reprint(tree, false).toString
    assertNoDiff(printed, "x match {\n  case 1 =>\n    a\n}")
    assertEquals(
      templStat(printed).collect { case c: Case => c.body.productPrefix },
      List("Term.Name"),
    )
  }

  test("case: block body with one definition, printed without comments") {
    val tree = templStat(
      """|x match {
         |  case 1 => { val a = 1 }
         |}
         |""".stripMargin,
    )
    val printed = TreeSyntax.reprint(tree, false).toString
    assertNoDiff(
      printed,
      """|x match {
         |  case 1 =>
         |    val a = 1
         |}
         |""".stripMargin,
    )
    assertEquals(
      templStat(printed).collect { case c: Case => c.body.productPrefix },
      List("Term.Block"),
    )
  }

  test("interpolation: spaces at the ends of the parts") {
    val parts = List(Lit.String(" a "), Lit.String("  "), Lit.String(" b "))
    val tree = Term.Interpolate(Term.Name("s"), parts, List(Term.Name("x"), Term.Name("y")))
    assertEquals(TreeSyntax.reprint(tree).toString, "s\" a $x  $y b \"")
  }

  test("xml: spaces at the ends of the parts") {
    val tree = Term.Xml(List(Lit.String("<a> "), Lit.String(" </a>")), List(Term.Name("x")))
    assertEquals(TreeSyntax.reprint(tree).toString, "<a> {x} </a>")
  }

  test("show: trailing space, then leading space")(
    assertEquals(Show.sequence("a ", " (", "b)").toString, "a  (b)"),
  )

  test("show: trailing spaces, then leading spaces")(
    assertEquals(Show.sequence("a  ", "  (", "b)").toString, "a    (b)"),
  )

  test("show: trailing space, then an empty function, then the end")(
    assertEquals(Show.sequence("a ", Show.function(_ => Show.None)).toString, "a "),
  )

  test("show: trailing space, then an empty function, then a newline")(assertEquals(
    Show.sequence("a ", Show.function(_ => Show.None), Show.newline("b")).toString,
    "a \nb".lf2nl,
  ))

  test("show: trailing space, then a function that starts with a newline")(
    assertEquals(Show.sequence("a ", Show.function(_ => Show.newline("b"))).toString, "a \nb".lf2nl),
  )

  test("show: delayed separator, then leading space")(
    assertEquals(Show.repeat(", ")("a", " b").toString, "a,  b"),
  )

  test("show: trailing space, then delayed separator")(
    assertEquals(Show.repeat(", ")("a ", "b").toString, "a , b"),
  )

  test("interpolation: this in a part") {
    val code =
      """|object A {
         |  val s = s"($this)"
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val s = s"(${this})" }
         |""".stripMargin,
    )()
  }

  test("self type: function type") {
    val code =
      """|class A {
         |  this: (Int => Unit) =>
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { this: Int => Unit => }
         |""".stripMargin,
      reprintError = "<input>:1: error: `;` expected but `=>` found",
    )()
  }

  test("param: function type of no args") {
    val code =
      """|object A {
         |  def g(f: (() => a.R)) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def g(f: () => a.R) = 1 }
         |""".stripMargin,
    )()
  }
}
