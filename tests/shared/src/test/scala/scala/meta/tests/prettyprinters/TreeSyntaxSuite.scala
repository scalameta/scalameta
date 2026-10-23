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
    val code = "object A { val x = tname: Int ?=> Boolean }"
    val printed =
      """|object A {
         |  val x = tname {
         |    Int ?=> Boolean
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (printed, printed))
  }

  test("case body: a block with its own comment, printed without comments") {
    val body = Term.Block.newBuilder(List(tname("a"), tname("b"))).begComment(Seq("// c")).result()
    val tree = Term.Match(tname("x"), List(Case(int(1), None, body)), Nil)
    val printed =
      """|x match {
         |  case 1 =>
         |    a
         |    b
         |}""".stripMargin
    assertEquals(TreeSyntax.reprint(tree, comments = false).toString, printed.lf2nl)
  }

  test("case body: a block with its own comment") {
    val body = Term.Block.newBuilder(List(tname("a"), tname("b"))).begComment(Seq("// c")).result()
    val tree = Term.Match(tname("x"), List(Case(int(1), None, body)), Nil)
    val printed =
      """|x match {
         |  case 1 =>
         |    // c
         |    {
         |      a
         |      b
         |    }
         |}""".stripMargin
    assertEquals(TreeSyntax.reprint(tree).toString, printed.lf2nl)
  }

  test("interpolation: a braced arg with its own comment, printed without comments") {
    val arg = Term.Block.newBuilder(List(tname("a"))).begComment(Seq("/* c */")).result()
    val tree = Term.Interpolate(tname("s"), List(str(""), str("")), List(arg))
    assertEquals(TreeSyntax.reprint(tree, comments = false).toString, "s\"${a}\"")
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

  test("unary: bang on a boolean in parens") {
    val code = "object A { val x = !(true) }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), ("object A { val x = !(true) }", List("!")))
  }

  test("unary: minus on a string in parens") {
    val code = "object A { val x = -(\"a\") }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), ("object A { val x = -(\"a\") }", List("-")))
  }

  test("unary: minus on a select on a name in parens") {
    val code = "object A { val x = -(x).y }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), ("object A { val x = -x.y }", List("-")))
  }

  test("unary: minus on a select on a literal") {
    val code = "object A { val x = -(1).x }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), ("object A { val x = -(1.x) }", List("-")))
  }

  test("unary: minus on an apply on a literal") {
    val code = "object A { val x = -(1).max(2) }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), ("object A { val x = -(1.max(2)) }", List("-")))
  }

  test("unary: plus on a select on a literal") {
    val code = "object A { val x = +(1).x }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), ("object A { val x = +1.x }", List("+")))
  }

  test("unary: tilde on a select on a double") {
    val code = "object A { val x = ~(1.0).x }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), ("object A { val x = ~1.0.x }", List("~")))
  }

  test("unary: bang on a select on a literal") {
    val code = "object A { val x = !(1).x }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), ("object A { val x = !1.x }", List("!")))
  }

  test("unary: plus on a select on a negative literal") {
    val code = "object A { val x = +(-1).x }"
    val first = "object A { val x = +(-1.x) }"
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("unary: minus on an operator name") {
    val code = "object A { val x = -(+) }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), ("object A { val x = -(+) }", List("-")))
  }

  test("unary: bang on an operator name") {
    val code = "object A { val x = !(!) }"
    val (first, _) = reprintTwice(code)
    val ops = source(first).collect { case t: Term.ApplyUnary => t.op.value }
    assertEquals((first, ops), ("object A { val x = !(!) }", List("!")))
  }

  test("unary: minus on a built anonymous term") {
    val tree = Term.ApplyUnary(Term.Name("-"), Term.Anonymous())
    assertEquals(TreeSyntax.reprint(tree).toString, "-")
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

  locally {
    import scala.meta.prettyprinters.Show
    import scala.meta.prettyprinters.Show.{blank, newline => n, sequence => s}
    def checkBlankLine(name: String, res: Show.Result, expected: String)(implicit
        loc: munit.Location,
    ): Unit = test(s"blank line: $name")(assertEquals(res.toString.replace("\r\n", "\n"), expected))
    checkBlankLine("after text", s("foo", blank(), "bar"), "foo\n\nbar")
    checkBlankLine("after a newline", s("foo", n(), blank(), "bar"), "foo\n\nbar")
    checkBlankLine("after text that ends its line", s("foo\n", blank(), "bar"), "foo\n\nbar")
    checkBlankLine(
      "after text that ends in a blank line",
      s("foo\n\n", blank(), "bar"),
      "foo\n\nbar",
    )
    checkBlankLine("after a line of spaces", s("foo\n  \n", blank(), "bar"), "foo\n  \nbar")
    checkBlankLine("after a blank line", s("foo", blank(), blank(), "bar"), "foo\n\nbar")
    checkBlankLine("then a newline", s("foo", blank(), n("bar")), "foo\n\nbar")
    checkBlankLine("at the start", s(blank(), "bar"), "bar")
    checkBlankLine("at the end", s("foo", blank()), "foo\n")
    checkBlankLine("at the end, after a newline", s("foo", n(), blank()), "foo\n")
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
    assertNoDiff(
      printed,
      """|x match {
         |  case 1 => a
         |}
         |""".stripMargin,
    )
    assertNoDiff(TreeSyntax.reprint(templStat(printed), false).toString, printed)
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
    assertEquals(Show.sequence("a ", " (", "b)").toString, "a (b)"),
  )

  test("show: trailing spaces, then leading spaces")(
    assertEquals(Show.sequence("a  ", "  (", "b)").toString, "a (b)"),
  )

  test("show: delayed separator, then leading space")(
    assertEquals(Show.repeat(", ")("a", " b").toString, "a, b"),
  )

  test("show: trailing space, then delayed separator")(
    assertEquals(Show.repeat(", ")("a ", "b").toString, "a, b"),
  )

  test("show: keyword after a letter")(
    assertEquals(Show.sequence("a", Show.keyword(":")).toString, "a:"),
  )

  test("show: keyword after an operator")(
    assertEquals(Show.sequence("+", Show.keyword(":")).toString, "+ :"),
  )

  test("show: keyword after a name that ends in an underscore")(
    assertEquals(Show.sequence("a_", Show.keyword(":")).toString, "a_ :"),
  )

  test("show: leading comments at the start of a line")(assertEquals(
    Show.sequence(Show.LeadingComments(Show.Comment("// c"), breakAtLineStart = true), "a").toString,
    "// c\na".lf2nl,
  ))

  test("show: leading comments within a line")(assertEquals(
    Show.sequence("x ", Show.LeadingComments(Show.Comment("/* c */"), breakAtLineStart = true), "a")
      .toString,
    "x /* c */ a",
  ))

  test("show: no blank line before")(
    assertEquals(Show.sequence("a", Show.blank(), Show.noBlankBefore("b")).toString, "a\nb".lf2nl),
  )

  test("show: no blank line before, after a newline")(
    assertEquals(Show.sequence("a", Show.newline(Show.noBlankBefore("b"))).toString, "a\nb".lf2nl),
  )

  test("show: a deferred result is built once") {
    var built = 0
    val res = Show.defer {
      built += 1
      Show.Str("a")
    }
    assertEquals(Show.sequence(res, res).toString, "aa")
    assertEquals(built, 1)
  }

  test("show: first syntax element, past a comment and a newline")(assertEquals(
    Show.firstSyntaxElem(Show.sequence(Show.Comment("// c"), Show.newline(Show.Str("(a)")))),
    "(a)",
  ))

  test("show: first syntax element of a keyword")(
    assertEquals(Show.firstSyntaxElem(Show.sequence(" ", Show.keyword("val"))), "val"),
  )

  test("show: first syntax element of nothing")(
    assertEquals(Show.firstSyntaxElem(Show.sequence(Show.Comment("// c"), " ")), null),
  )

  test("show: leading space within a line")(
    assertEquals(Show.sequence("a", " = b").toString, "a = b"),
  )

  test("show: newline, then a leading space")(
    assertEquals(Show.sequence("a", Show.newline(" = b")).toString, "a\n  = b".lf2nl),
  )

  test("show: newline, then a space before a string")(assertEquals(
    Show.sequence("a", Show.newline(Show.sequence(" ", "= b"))).toString,
    "a\n  = b".lf2nl,
  ))

  test("show: leading and trailing space, then a newline")(
    assertEquals(Show.sequence("a", " = ", Show.newline("b")).toString, "a =\n  b".lf2nl),
  )

  test("show: newline on a condition") {
    assertEquals(Show.sequence("a", Show.newline("b", cond = false)).toString, "ab")
    assertEquals(Show.sequence("a", Show.newline("b", cond = true)).toString, "a\nb".lf2nl)
  }

  test("show: space, then a newline")(
    assertEquals(Show.sequence("a", " ", Show.newline("b")).toString, "a\n  b".lf2nl),
  )

  test("show: space, then an outer newline")(
    assertEquals(Show.sequence("a", " ", Show.outerNewline("b")).toString, "a\nb".lf2nl),
  )

  test("show: first syntax element past an outer newline")(
    assertEquals(Show.firstSyntaxElem(Show.outerNewline(Show.Str("(a)"))), "(a)"),
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
      """|class A { this: (Int => Unit) => }
         |""".stripMargin,
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

  test("existential type in a function parameter") {
    val code =
      """|object A {
         |  def f(g: ((S[X], X) forSome { type X }) => B): B = g
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  def f(g: ((S[X], X) forSome {
         |    type X
         |  }) => B): B = g
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("type lambda in a function parameter") {
    val code = "object A { def f(g: ([X] =>> F[X]) => B): B = g }"
    val printed = "object A { def f(g: ([X] =>> F[X]) => B): B = g }"
    assertEquals(reprintTwice(code, dialects.Scala3), (printed, printed))
  }

  test("statement in parens after a case class, blank line between") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a, b).c(d)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a, b).c(d)
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: infix qualifier") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a + b).c
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a + b).c
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: infix callee") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a + b)(c)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a + b)(c)
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: unary qualifier") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (-a).b
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (-a).b
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: new callee") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (new A)(b)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (new A)(b)
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("match type in a function parameter type") {
    val code = "type T = (X match { case Int => String }) => C"
    val first =
      """|type T = (X match {
         |  case Int => String
         |}) => C""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("statement in parens after a case class: block select-match") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  { a }.match { case _ => b }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  {
         |    a
         |  }.match {
         |    case _ => b
         |  }
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3), (first, second))
  }

  test("statement in parens after a case class: infix select-match") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a + b).match { case _ => c }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a + b).match {
         |    case _ => c
         |  }
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3), (first, second))
  }

  test("statement in parens after a case class: name select-match") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  x.match { case _ => c }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |  x.match {
         |    case _ => c
         |  }
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3), (first, second))
  }

  test("statement in parens after a case class: negative literal qualifier") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (-1).abs
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |  -1.abs
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: if qualifier") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (if (a) b else c).d
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (if (a) b else c).d
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: match qualifier") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a match { case _ => b }).c
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a match {
         |    case _ => b
         |  }).c
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: lambda callee") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  ((x: Int) => x)(1)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  ((x: Int) => x)(1)
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: ascribed qualifier") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a: A).b
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a: A).b
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: postfix qualifier") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a b).c
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a b).c
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: assignment qualifier") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a = b).c
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a = b).c
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: new qualifier") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (new A).b
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (new A).b
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: if infix lhs") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (if (a) b else c) + d
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (if (a) b else c) + d
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: ascribed match selector") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a: A) match { case _ => b }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a: A) match {
         |    case _ => b
         |  }
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: infix eta") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a + b) _
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  (a + b) _
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("statement in parens after a case class: block qualifier") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  { a }.b
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  {
         |    a
         |  }.b
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("typed pattern in parens with a declared type") {
    val code =
      """|object A {
         |  val (x: Int): Int = 1
         |}
         |""".stripMargin
    val first = "object A { val (x: Int): Int = 1 }"
    val second = "object A { val (x: Int): Int = 1 }"
    assertEquals(reprintTwice(code), (first, second))
  }

  test("alternative pattern in a tuple pattern") {
    val code = "object A { x match { case (a | b, c) => d } }"
    val printed =
      """|object A {
         |  x match {
         |    case (a | b, c) => d
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("typed pattern in an extractor pattern") {
    val code = "object A { x match { case Some(y: Int) => y } }"
    val printed =
      """|object A {
         |  x match {
         |    case Some(y: Int) => y
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("typed pattern in parens, no declared type") {
    val code = "object A { val (x: Int) = 1 }"
    val printed = "object A { val (x: Int) = 1 }"
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("typed var pattern in parens, no declared type") {
    val code = "object A { var (x: Int) = 1 }"
    val printed = "object A { var (x: Int) = 1 }"
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("two typed patterns in parens") {
    val code = "object A { val (x: Int), (y: Int) = 1 }"
    val first = "object A { val (x: Int), (y: Int) = 1 }"
    val second = "object A { val (x: Int), (y: Int) = 1 }"
    assertEquals(reprintTwice(code), (first, second))
  }

  test("block: built, a comment as the last statement") {
    val stat = Term.Anonymous.newBuilder().begComment(Seq("// c")).result()
    val tree = Term.Block(List(Term.Name("a"), stat))
    val printed =
      """|{
         |  a
         |  // c
         |}""".stripMargin.lf2nl
    assertEquals(TreeSyntax.reprint(tree).toString, printed)
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
         |}""".stripMargin.lf2nl
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
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3), (first, second))
  }

  test("backquoted operator name starts a statement, blank line before") {
    val code =
      """|object A {
         |  val b = f(c)
         |
         |  `(` + d
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val b = f(c)
         |
         |  `(` + d
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala213Source3, comments = false), (first, second))
  }

  test("unit statement after a case class, blank line between") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  ().toString
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  ().toString
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("empty block statement after a case class, blank line between") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  {}
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  {}
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("lambda without params after a case class, blank line between") {
    val code =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  () => 1
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  case class Foo(a: Int)
         |
         |  () => 1
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code), (first, second))
  }

  test("backquoted operator name, then a word operator, blank line before") {
    val code =
      """|object A {
         |  val b = f(c)
         |
         |  `(` max d
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val b = f(c)
         |
         |  `(` max d
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala213Source3, comments = false), (first, second))
  }

  test("backquoted operator name starts a statement in Scala 3, blank line before") {
    val code =
      """|object A:
         |  val b = f(c)
         |
         |  `(` + d
         |""".stripMargin
    val first =
      """|object A {
         |  val b = f(c)
         |
         |  `(` + d
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3, comments = false), (first, second))
  }

  test("symbolic name starts a statement, blank line before") {
    val code =
      """|object A {
         |  val b = f(c)
         |
         |  ++ max d
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val b = f(c)
         |
         |  ++ max d
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala213Source3, comments = false), (first, second))
  }

  private def reprintAfterA(stat: String, d: Dialect): String = {
    val quasiDialect = d.unquoteTerm(multiline = true)
    val block = term(s"{ a; $stat }")(quasiDialect).asInstanceOf[Term.Block]
    TreeSyntax.reprint(Term.Block(block.stats))(quasiDialect).toString
  }
  private def reprintQuasiOpAfterA(lhs: Term.Name, d: Dialect): String = {
    val quasiDialect = d.unquoteTerm(multiline = true)
    val infix = term("x $op b")(quasiDialect).asInstanceOf[Term.ApplyInfix]
    TreeSyntax.reprint(Term.Block(List(tname("a"), infix.copy(lhs = lhs))))(quasiDialect).toString
  }

  test("quasi operator after a symbolic name starts a statement") {
    def reprint() = reprintQuasiOpAfterA(tname("+"), dialects.Scala3)
    assertEquals(
      reprint(),
      """|{
         |  a
         |
         |  + $op b
         |}""".stripMargin.lf2nl,
    )
  }

  test("quasi operator after a backquoted name starts a statement") {
    def reprint() = reprintQuasiOpAfterA(tname("x y"), dialects.Scala3)
    assertEquals(
      reprint(),
      """|{
         |  a
         |
         |  `x y` $op b
         |}""".stripMargin.lf2nl,
    )
  }

  test("quasi operator after a symbolic name starts a statement in Scala 2.13") {
    def reprint() = reprintQuasiOpAfterA(tname("+"), dialects.Scala213)
    assertEquals(
      reprint(),
      """|{
         |  a
         |  + $op b
         |}""".stripMargin.lf2nl,
    )
  }

  test("quasi starts a statement") {
    def reprint() = reprintAfterA("$x + b", dialects.Scala3)
    assertEquals(
      reprint(),
      """|{
         |  a
         |
         |  $x + b
         |}""".stripMargin.lf2nl,
    )
  }

  test("quasi ellipsis starts a statement") {
    def reprint() = reprintAfterA("..$xs", dialects.Scala3)
    assertEquals(
      reprint(),
      """|{
         |  a
         |
         |  ..$xs
         |}""".stripMargin.lf2nl,
    )
  }

  test("args: a lambda with a braced body after another arg") {
    val code =
      """|object A {
         |  f(a, x => {
         |    b
         |    c
         |  })
         |}
         |""".stripMargin
    val printed =
      """|object A {
         |  f(a, x => {
         |    b
         |    c
         |  })
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("args: using, then a lambda with a braced body") {
    val code =
      """|object A {
         |  f(using x => {
         |    b
         |    c
         |  })
         |}
         |""".stripMargin
    val printed =
      """|object A {
         |  f(using x => {
         |    b
         |    c
         |  })
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (printed, printed))
  }

  test("infix in parens: blank line and DSLC before the operator") {
    val code =
      """|object A {
         |  val y = (a
         |
         |      // c1
         |      && b)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val y = (a
         |    // c1
         |    && b)
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("infix in parens: DSLC before the operator") {
    val code =
      """|object A {
         |  val y = (a
         |      // c1
         |      && b)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val y = (a
         |    // c1
         |    && b)
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("infix in parens: blank line and DSLC before the operator in Scala 3") {
    val code =
      """|object A {
         |  val y = (a
         |
         |      // c1
         |      && b)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val y = a
         |    // c1
         |    && b
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("infix in parens: DSLC before the operator in Scala 3") {
    val code =
      """|object A {
         |  val y = (a
         |      // c1
         |      && b)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val y = a
         |    // c1
         |    && b
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("infix in parens: MLC before the operator") {
    val code = "object A { val y = (a /* c1 */ && b) }"
    val first = "object A { val y = a /* c1 */ && b }"
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("infix type in parens: blank line and DSLC before the operator in Scala 3") {
    val code =
      """|object A {
         |  type T = (A
         |
         |    // c1
         |    | B)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  type T = A
         |    // c1
         |    | B
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("infix type in parens: blank line and DSLC before the operator") {
    val code =
      """|object A {
         |  type T = (A
         |
         |    // c1
         |    | B)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  type T = (A
         |    // c1
         |    | B)
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("infix type in parens: DSLC before the operator") {
    val code =
      """|object A {
         |  type T = (A
         |    // c1
         |    | B)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  type T = (A
         |    // c1
         |    | B)
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("infix pattern in parens: blank line and DSLC before the operator") {
    val code =
      """|object A {
         |  x match {
         |    case (a
         |
         |      // c1
         |      :: b) => c
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  x match {
         |    case (a
         |      // c1
         |      :: b) => c
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("infix pattern: DSLC and blank line after the operator") {
    val code =
      """|object A {
         |  x match {
         |    case a ::
         |
         |      // c1
         |      b => c
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  x match {
         |    case a ::
         |      // c1
         |      b => c
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("infix pattern: DSLC after the operator") {
    val code =
      """|object A {
         |  x match {
         |    case a ::
         |      // c1
         |      b => c
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  x match {
         |    case a ::
         |      // c1
         |      b => c
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("infix with type args: blank line and DSLC before them in Scala 3") {
    val code =
      """|object A {
         |  a op
         |
         |    // c1
         |    [T] b
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  a op
         |  // c1
         |    [T] b
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("infix type in parens: ASLC after the lhs") {
    val code =
      """|object A {
         |  type T = (A // c1
         |    | B)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  type T = (A // c1
         |  ) | B
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("infix pattern in parens: ASLC after the lhs") {
    val code =
      """|object A {
         |  val (a // c1
         |    :: b) = x
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val (a // c1
         |  ) :: b = x
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("infix pattern in a val, in parens: DSLC before the operator") {
    val code =
      """|object A {
         |  val (a
         |    // c1
         |    :: b) = x
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val (a
         |    // c1
         |    :: b) = x
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("try: DSLC above, body on the next line") {
    val code =
      """|object A:
         |  // c1
         |  try
         |    a
         |  finally c
         |""".stripMargin
    val first =
      """|object A {
         |  // c1
         |  try a finally c
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("while: DSLC above, condition on the next line") {
    val code =
      """|object A:
         |  // c1
         |  while
         |    a
         |  do b
         |""".stripMargin
    val first =
      """|object A {
         |  // c1
         |  while (a) b
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("return: DSLC above, expression on the next line") {
    val code =
      """|object A:
         |  def f: Int =
         |    // c1
         |    return
         |      a
         |""".stripMargin
    val first =
      """|object A {
         |  def f: Int =
         |    // c1
         |    return a
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("args: MLC before a lambda param in a braced arg") {
    val code =
      """|object A {
         |  f(a, xs.map { /* c1 */ x => g(x) })
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  f(a, xs.map {
         |    /* c1 */ x => g(x)
         |  })
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("args: MLC in a braced block arg") {
    val code =
      """|object A {
         |  f(a, { /* c1 */ b })
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  f(a, {
         |    /* c1 */ b
         |  })
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("tuple: MLC before a lambda param in a braced element") {
    val code =
      """|object A {
         |  (a, xs.map { /* c1 */ x => g(x) })
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  (a, xs.map {
         |    /* c1 */ x => g(x)
         |  })
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("args: MLC before a lambda param after a colon") {
    val code =
      """|object A:
         |  f(a, xs.map: /* c1 */ x =>
         |    g(x))
         |""".stripMargin
    val first =
      """|object A {
         |  f(a, xs.map {
         |    /* c1 */ x => g(x)
         |  })
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("match: DSLC after the last case, printed without comments") {
    val code =
      """|object A {
         |  x match {
         |    case 1 => b
         |    // c3
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  x match {
         |    case 1 => b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213, comments = false), (first, first))
  }

  test("catch: DSLC after the last case, printed without comments") {
    val code =
      """|object A {
         |  try a
         |  catch {
         |    case 1 => b
         |    // c3
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  try a catch {
         |    case 1 => b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213, comments = false), (first, first))
  }

  test("partial function: DSLC after the last case, printed without comments") {
    val code =
      """|object A {
         |  xs.map {
         |    case 1 => b
         |    // c3
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  xs.map {
         |    case 1 => b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213, comments = false), (first, first))
  }

  test("package with braces: DSLC in the body, printed without comments") {
    val code =
      """|package a {
         |  // c1
         |}
         |""".stripMargin
    val first = "package a"
    assertEquals(reprintTwice(code, dialects.Scala213, comments = false), (first, first))
  }

  test("tuple: leading comma with ASLC") {
    val code =
      """|object A {
         |  (
         |    a
         |    , // c1
         |    b
         |  )
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  (
         |    a, // c1
         |    b
         |  )
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("case: MLC between two statements on one line") {
    val code =
      """|object A {
         |  x match {
         |    case 1 => a /* c1 */; b
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  x match {
         |    case 1 =>
         |      a /* c1 */
         |      b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("case: MLC between two statements in braces") {
    val code =
      """|object A {
         |  x match {
         |    case 1 => { a /* c1 */; b }
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  x match {
         |    case 1 =>
         |      a /* c1 */
         |      b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("lambda in braces: two statements, DSLC at the brace column") {
    val code =
      """|object A {
         |  xs.map { x =>
         |    val y = x
         |    y
         |  // c1
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  xs.map { x =>
         |    val y = x
         |    y
         |  // c1
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("lambda in braces: MLC before the param, two statements") {
    val code =
      """|object A {
         |  xs.map { /* c1 */ x =>
         |    val y = x
         |    y
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  xs.map { /* c1 */ x =>
         |    val y = x
         |    y
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("lambda after a colon: MLC before the param, two statements") {
    val code =
      """|object A:
         |  xs.map: /* c1 */ x =>
         |    val y = x
         |    y
         |""".stripMargin
    val first =
      """|object A {
         |  xs.map { /* c1 */ x =>
         |    val y = x
         |    y
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("if-then: DSLC and blank line before else") {
    val code =
      """|object A:
         |  if a then
         |    b
         |  // c1
         |
         |  else
         |    c
         |""".stripMargin
    val first =
      """|object A {
         |  if (a) b
         |  // c1
         |
         |  else {
         |    c
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("if with condition in parens: DSLC and blank line before the body") {
    val code =
      """|object A:
         |  if (a)
         |  // c1
         |
         |  b
         |""".stripMargin
    val first =
      """|object A {
         |  if (a)
         |    // c1
         |
         |    b
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("if-then: DSLC and blank line before the body") {
    val code =
      """|object A:
         |  if a then
         |  // c1
         |
         |  b
         |""".stripMargin
    val first =
      """|object A {
         |  if (a)
         |    // c1
         |
         |    b
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("infix in parens: DSLC, then a blank line before the operator") {
    val code =
      """|object A {
         |  (a
         |    // c1
         |
         |    && b)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  (a
         |    // c1
         |
         |    && b)
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("import: MLC before the importees") {
    val code =
      """|import c. /* c1 */{d, e}
         |""".stripMargin
    val first = "import c. /* c1 */ {d, e}"
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("export: MLC before the importees") {
    val code =
      """|export c. /* c1 */{d, e}
         |""".stripMargin
    val first = "export c. /* c1 */ {d, e}"
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("args: MLC inside a sequence wildcard") {
    val code =
      """|object A {
         |  f(b: _ /* c1 */*)
         |}
         |""".stripMargin
    val first = "object A { f(b: _* /* c1 */ ) }"
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("singleton type: MLC before type") {
    val code =
      """|object A {
         |  type T = x. /* c1 */type
         |}
         |""".stripMargin
    val first = "object A { type T = x.type /* c1 */ }"
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("singleton type of Nil: MLC before type") {
    val code =
      """|object A {
         |  type T = Nil. /* c1 */type
         |}
         |""".stripMargin
    val first = "object A { type T = Nil.type /* c1 */ }"
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("secondary ctor: MLC before the args of this") {
    val code =
      """|class A {
         |  def this() = this /* c1 */(1, null)
         |}
         |""".stripMargin
    val first = "class A { def this() = this /* c1 */ (1, null) }"
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("pattern: MLC in empty args") {
    val code =
      """|object A {
         |  x match {
         |    case List( /* c1 */) =>
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  x match {
         |    case List( /* c1 */ ) =>
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("quote: MLC before the braces") {
    val code =
      """|val x = ' /* c1 */{ x }
         |""".stripMargin
    val first = "val x = /* c1 */ '{ x }"
    assertEquals(reprintTwice(code, dialects.Scala3), (first, first))
  }

  test("case: MLC after a body, DSLC on the next line, another case") {
    val code =
      """|object A {
         |  x match {
         |    case 1 => b /* c1 */
         |    // c2
         |    case 2 => d
         |  }
         |}
         |""".stripMargin
    val first = code.stripSuffix("\n").lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("case: body on its own line with DSLC, then a one-line case") {
    val code =
      """|object A {
         |  x match {
         |    case 1 =>
         |      b
         |      // c1
         |    case 2 => d
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  x match {
         |    case 1 =>
         |      b
         |      // c1
         |    case 2 =>
         |      d
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("try: DSLC before the expression on its own line, catch") {
    val code =
      """|object A {
         |  try
         |    // c1
         |    a
         |  catch { case _ => b }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  try (
         |    // c1
         |    a
         |  ) catch {
         |    case _ => b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("args: MLC after an arg, DSLC, leading comma") {
    val code =
      """|object A {
         |  f(a /* c1 */
         |    // c2
         |    , b)
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  f(
         |    a, /* c1 */
         |    // c2
         |    b
         |  )
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("lambda in braces: implicit param, two statements") {
    val code =
      """|object A {
         |  f { implicit x =>
         |    a
         |    b
         |  }
         |}
         |""".stripMargin
    val printed =
      """|object A {
         |  f { implicit x =>
         |    a
         |    b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("lambda in braces: typed implicit param, two statements") {
    val code =
      """|object A {
         |  f { implicit x: Int =>
         |    a
         |    b
         |  }
         |}
         |""".stripMargin
    val printed =
      """|object A {
         |  f { implicit x: Int =>
         |    a
         |    b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("lambda in braces: param, two statements") {
    val code =
      """|object A {
         |  f { x =>
         |    a
         |    b
         |  }
         |}
         |""".stripMargin
    val printed =
      """|object A {
         |  f { x =>
         |    a
         |    b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("lambda in braces: placeholder param, two statements") {
    val code =
      """|object A {
         |  f { _ =>
         |    a
         |    b
         |  }
         |}
         |""".stripMargin
    val printed =
      """|object A {
         |  f { _ =>
         |    a
         |    b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("lambda in braces: typed param, two statements") {
    val code =
      """|object A {
         |  f { (x: Int) =>
         |    a
         |    b
         |  }
         |}
         |""".stripMargin
    val printed =
      """|object A {
         |  f { (x: Int) =>
         |    a
         |    b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("lambda in braces: two params, two statements") {
    val code =
      """|object A {
         |  f { (x, y) =>
         |    a
         |    b
         |  }
         |}
         |""".stripMargin
    val printed =
      """|object A {
         |  f { (x, y) =>
         |    a
         |    b
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("lambda in braces: param, empty body") {
    val code =
      """|object A {
         |  f { x => }
         |}
         |""".stripMargin
    val printed =
      """|object A {
         |  f { x =>
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code), (printed, printed))
  }

  test("lambda with a braced body in a block argument") {
    val code =
      """|object A {
         |  f {
         |    (i: Int) => {
         |      g(i)
         |    }
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  f {
         |    (i: Int) => {
         |      g(i)
         |    }
         |  }
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala213Source3, comments = false), (first, second))
  }

  test("lambda with a braced body of two statements in a block argument") {
    val code =
      """|object A {
         |  f {
         |    (i: Int) => {
         |      g(i)
         |      h(i)
         |    }
         |  }
         |}
         |""".stripMargin
    val printed =
      """|object A {
         |  f { (i: Int) =>
         |    g(i)
         |    h(i)
         |  }
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213Source3, comments = false), (printed, printed))
  }

  test("lambda with a braced body in a block argument, with comments") {
    val code =
      """|object A {
         |  f {
         |    (i: Int) => {
         |      g(i)
         |    }
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  f {
         |    (i: Int) => {
         |      g(i)
         |    }
         |  }
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala213Source3), (first, second))
  }

  test("procedure syntax, brace on the next line") {
    val code =
      """|object A {
         |  def main(args: Array[String])
         |  {
         |    f()
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  def main(args: Array[String]): Unit
         |
         |  {
         |    f()
         |  }
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3, comments = false), (first, second))
  }

  test("val declaration, then a block, blank line between") {
    val code =
      """|object A {
         |  val x: Int
         |
         |  {
         |    f()
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val x: Int
         |
         |  {
         |    f()
         |  }
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3, comments = false), (first, second))
  }

  test("var declaration, then a block, blank line between") {
    val code =
      """|object A {
         |  var x: Int
         |
         |  {
         |    f()
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  var x: Int
         |
         |  {
         |    f()
         |  }
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3, comments = false), (first, second))
  }

  test("type alias, then a block, blank line between") {
    val code =
      """|object A {
         |  type T = Int
         |
         |  {
         |    f()
         |  }
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  type T = Int
         |
         |  {
         |    f()
         |  }
         |}""".stripMargin.lf2nl
    val second = first
    assertEquals(reprintTwice(code, dialects.Scala3, comments = false), (first, second))
  }

  test("postfix select, then a statement, blank line between") {
    val code =
      """|object A {
         |  a toString
         |
         |  b
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  a toString
         |
         |  b
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

  test("postfix operator in a val, then a statement, blank line between") {
    val code =
      """|object A {
         |  val x = a +
         |
         |  b * c
         |}
         |""".stripMargin
    val first =
      """|object A {
         |  val x = a +
         |
         |  b * c
         |}""".stripMargin.lf2nl
    assertEquals(reprintTwice(code, dialects.Scala213), (first, first))
  }

}
