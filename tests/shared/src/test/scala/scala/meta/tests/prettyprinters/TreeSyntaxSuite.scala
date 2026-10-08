package scala.meta.tests.prettyprinters

import scala.meta._
import scala.meta.internal.prettyprinters.TreeSyntax

/**
 * This class, unlike similar SyntacticSuite, does not reset origins. Instead it uses runTestAssert
 * to force reprinting of syntax.
 */

class TreeSyntaxSuite extends scala.meta.tests.parsers.ParseSuite {

  implicit val dialect: Dialect = dialects.Scala211

  private def reprintTwice(
      code: String,
      d: Dialect = dialect,
      comments: Boolean = true,
  ): (String, String) = {
    def reprint(code: String) = TreeSyntax.reprint(source(code)(d), comments)(d).toString
    val first = reprint(code)
    val second =
      try reprint(first)
      catch { case e: ParseException => e.shortMessage }
    (first, second)
  }

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

  test("interpolation: this in a part, printed twice") {
    val code = """object A { val s = s"($this)" }"""
    val printed = """object A { val s = s"(${this})" }"""
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("interpolation: braced name before a letter") {
    val code = """object A { val s = s"${a}b" }"""
    assertEquals(reprintTwice(code), (code, code))
  }

  test("ascription: context function type in parens") {
    val code = "object A { val x = true: (Int ?=> Boolean) }"
    assertEquals(reprintTwice(code, dialects.Scala3), (code, code))
  }

  test("ascription: context function type") {
    val code = "object A { val x = true: Int ?=> Boolean }"
    val printed =
      """|object A {
         |  val x = true {
         |    Int ?=> Boolean
         |  }
         |}""".stripMargin
    assertEquals(reprintTwice(code, dialects.Scala3), (printed, printed))
  }

  test("unary: plus on a literal in parens") {
    val code = "object A { val x = +(6) }"
    assertEquals(reprintTwice(code), (code, code))
  }

  test("unary: minus on a literal in parens") {
    val code = "object A { val x = -(6) }"
    val (first, _) = reprintTwice(code)
    assertEquals((first, source(first).collect { case x: Lit.Int => x.value }), (code, List(6)))
  }

  test("unary: tilde on a double in parens") {
    val code = "object A { val x = ~(1.0) }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), (code, List("~")))
  }

  test("postfix: select on a postfix in parens") {
    val code = "object A { val x = (a b).c }"
    assertEquals(reprintTwice(code), (code, code))
  }

  test("postfix: postfix on a postfix in parens") {
    val code = "object A { val x = (a b) c }"
    val printed = "object A { val x = (a b) c }"
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("two if-then statements in a block argument") {
    val code =
      """|object A:
         |  def g =
         |    xs foreach {
         |      if a then b += p
         |      if c then d += p
         |    }
         |""".stripMargin
    val first =
      """|object A {
         |  def g = xs foreach {
         |    if (a) b += p
         |    if (c) d += p
         |  }
         |}""".stripMargin
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3, comments = false), (first, second))
  }

  test("two if statements with conditions in parens in a block argument") {
    val code =
      """|object A {
         |  def g = xs foreach {
         |    if (a) b += p
         |    if (c) d += p
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  def g = xs foreach {
         |    if (a) b += p
         |    if (c) d += p
         |  }
         |}""".stripMargin
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3), (first, second))
  }

}
