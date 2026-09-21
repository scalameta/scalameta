package scala.meta.tests.parsers.dotty

import scala.meta._

class TrailingCommentSuite extends BaseDottySuite {

  test("class: comment after name, extends on the next line") {
    val code =
      """|class A // c
         |  extends B
         |""".stripMargin
    val layout =
      """|class A // c
         |  extends B
         |""".stripMargin
    val tree = Defn.Class(
      Nil,
      Type.Name.newBuilder("A").endComment(Seq("// c")).result(),
      Type.ParamClause(Nil),
      EmptyCtor(),
      tpl(List(init("B")), Nil),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("if: comment after thenp") {
    val code =
      """|if (a) b // c
         |else d
         |""".stripMargin
    val layout =
      """|if (a) b // c
         |else d
         |""".stripMargin
    val tree = Term.If(tname("a"), tnameComments("b")()("// c"), tname("d"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("try: comment after expr") {
    val code =
      """|try a // c
         |catch { case _ => b }
         |""".stripMargin
    val layout =
      """|try a // c
         |catch {
         |  case _ => b
         |}
         |""".stripMargin
    val tree = Term
      .Try(tnameComments("a")()("// c"), List(Case(Pat.Wildcard(), None, tname("b"))), None)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("try: comment after catch block") {
    val code =
      """|try a
         |catch { case _ => b } // c
         |finally d
         |""".stripMargin
    val layout =
      """|try a catch {
         |  case _ => b
         |} // c
         |finally d
         |""".stripMargin
    val tree = Term.Try(
      tname("a"),
      Some(
        Term.CasesBlock.newBuilder(List(Case(Pat.Wildcard(), None, tname("b"))))
          .endComment(Seq("// c")).result(),
      ),
      Some(tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("do: comment after body") {
    val code =
      """|do a // c
         |while (b)
         |""".stripMargin
    val layout =
      """|do a // c
         |while (b)
         |""".stripMargin
    val tree = Term.Do(tnameComments("a")()("// c"), tname("b"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("match: comment after expr") {
    val code =
      """|foo // c
         |  match { case _ => 1 }
         |""".stripMargin
    val layout =
      """|foo // c
         |match {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = tmatch(tnameComments("foo")()("// c"), Case(Pat.Wildcard(), None, int(1)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("type infix: comment after op") {
    val code =
      """|type T = A & // c
         |  B
         |""".stripMargin
    val layout =
      """|type T = A & // c
         |  B
         |""".stripMargin
    val tree = Defn.Type(
      Nil,
      pname("T"),
      Nil,
      Type.ApplyInfix(
        pname("A"),
        Type.Name.newBuilder("&").endComment(Seq("// c")).result(),
        pname("B"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("val: comment after =") {
    val code =
      """|val x = // c
         |  1
         |""".stripMargin
    val layout =
      """|val x =
         |  // c
         |  1
         |""".stripMargin
    val tree = Defn
      .Val(Nil, List(patvar("x")), None, Lit.Int.newBuilder(1).begComment(Seq("// c")).result())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("throw: comment after keyword") {
    val code =
      """|def f = throw // c
         |  e
         |""".stripMargin
    val layout =
      """|def f = throw
         |  // c
         |  e
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, Nil, None, Term.Throw(tnameComments("e")("// c")()))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("function: comment after arrow") {
    val code =
      """|(x: Int) => // c
         |  x
         |""".stripMargin
    val layout =
      """|(x: Int) =>
         |  // c
         |  x
         |""".stripMargin
    val tree = tfunc(tparam("x", "Int"))(tnameComments("x")("// c")())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case: comment after arrow, one-liner") {
    val code =
      """|x match {
         |  case 1 => // c
         |    2
         |}
         |""".stripMargin
    val layout =
      """|x match {
         |  case 1 =>
         |    // c
         |    2
         |}
         |""".stripMargin
    val tree =
      tmatch(tname("x"), Case(int(1), None, Lit.Int.newBuilder(2).begComment(Seq("// c")).result()))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("return: comment after keyword, expr on the next line") {
    val code =
      """|def f = return // c
         |  1
         |""".stripMargin
    val layout =
      """|def f = return
         |  // c
         |  1
         |""".stripMargin
    val body = Term.Return(Lit.Int.newBuilder(1).begComment(Seq("// c")).result())
    val tree = Defn.Def(Nil, tname("f"), Nil, Nil, None, body)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("return: comment after keyword, expr on the same line") {
    val code = "def f = return /* c */ 1"
    val layout =
      """|def f = return
         |  /* c */
         |  1
         |""".stripMargin
    val body = Term.Return(Lit.Int.newBuilder(1).begComment(Seq("/* c */")).result())
    val tree = Defn.Def(Nil, tname("f"), Nil, Nil, None, body)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("if: block comment after cond, body on the same line") {
    val code = "if (a) /* c */ b"
    val layout =
      """|if (a)
         |  /* c */
         |  b
         |""".stripMargin
    val tree = Term.If(tname("a"), tnameComments("b")("/* c */")(), Lit.Unit())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("while: block comment after cond, body on the same line") {
    val code = "while (a) /* c */ b"
    val layout =
      """|while (a)
         |  /* c */
         |  b
         |""".stripMargin
    val tree = Term.While(tname("a"), tnameComments("b")("/* c */")())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("if: comment after cond, body on the next line") {
    val code =
      """|if (a) // c
         |  b
         |else d
         |""".stripMargin
    val layout =
      """|if (a)
         |  // c
         |  b else d
         |""".stripMargin
    val tree = Term.If(tname("a"), tnameComments("b")("// c")(), tname("d"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("while: comment after cond, body on the next line") {
    val code =
      """|while (a) // c
         |  b
         |""".stripMargin
    val layout =
      """|while (a)
         |  // c
         |  b
         |""".stripMargin
    val tree = Term.While(tname("a"), tnameComments("b")("// c")())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("line comment at end of input") {
    val x = term("x // X")
    assertEquals(x.endComment.get.values.last.newlinesAfter, 1)
    assertSyntax("func(x // X\n)")(Term.Apply(tname("func"), Term.ArgClause(List(x))))
  }

  test("block comment at end of input") {
    val x = term("x /* X */")
    assertEquals(x.endComment.get.values.last.newlinesAfter, 1)
    assertSyntax("func(x /* X */\n)")(Term.Apply(tname("func"), Term.ArgClause(List(x))))
  }

  test("line comment at start of input") {
    val x = term("// X\nx")
    assertEquals(x.begComment.get.newlinesBefore, 0)
    assertSyntax("val y =\n  // X\n  x")(Defn.Val(Nil, List(patvar("y")), None, x))
  }

  test("block comment at start of input") {
    val x = term("/* X */ x")
    assertEquals(x.begComment.get.newlinesBefore, 0)
    assertSyntax("val y =\n  /* X */\n  x")(Defn.Val(Nil, List(patvar("y")), None, x))
  }

  test("infix: comment on its own line after op") {
    val code =
      """|a op
         |  // foo
         |  b
         |""".stripMargin
    val layout = "a op b"
    val tree = Term.ApplyInfix(
      tname("a"),
      tname("op"),
      Type.ArgClause(Nil),
      Term.ArgClause.createWithComments(List(tname("b")), begComment = Seq("// foo")),
    )
    parseAndCheckTree[Stat](code, layout)(tree)
    val reparsed = Term.ApplyInfix(tname("a"), tname("op"), Nil, List(tname("b")))
    runTestAssert[Stat](layout)(reparsed)
  }

  test("infix: block comment on its own line after op") {
    val code =
      """|a op
         |  /* foo */ b
         |""".stripMargin
    val layout = "a op b"
    val tree = Term.ApplyInfix(
      tname("a"),
      tname("op"),
      Type.ArgClause(Nil),
      Term.ArgClause.createWithComments(List(tname("b")), begComment = Seq("/* foo */")),
    )
    parseAndCheckTree[Stat](code, layout)(tree)
    val reparsed = Term.ApplyInfix(tname("a"), tname("op"), Nil, List(tname("b")))
    runTestAssert[Stat](layout)(reparsed)
  }

  test("infix: comment inside parens after op") {
    val code =
      """|a op (
         |  // foo
         |  b
         |)
         |""".stripMargin
    val layout =
      """|a op
         |  // foo
         |  b
         |""".stripMargin
    val tree = Term.ApplyInfix(tname("a"), tname("op"), Nil, List(tnameComments("b")("// foo")()))
    parseAndCheckTree[Stat](code, layout)(tree)
    val reparsed = Term.ApplyInfix(
      tname("a"),
      tname("op"),
      Type.ArgClause(Nil),
      Term.ArgClause.createWithComments(List(tname("b")), begComment = Seq("// foo")),
    )
    parseAndCheckTree[Stat](layout, "a op b")(reparsed)
  }

  test("comment before a blank line at the start of input") {
    val code =
      """|// c
         |
         |x
         |""".stripMargin
    val tree = tname("x")
    runTestAssert[Stat](code, "x")(tree)
  }

  test("comment before a brace body") {
    val code =
      """|def f = // c
         |  {
         |    x
         |  }
         |""".stripMargin
    val layout =
      """|def f = {
         |  x
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tname("x")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("comment inside a parameter clause") {
    val code =
      """|def f(
         |  // c
         |  x: Int
         |) = 1
         |""".stripMargin
    val layout = "def f(x: Int) = 1"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(Member.ParamClauseGroup(
        Type.ParamClause(Nil),
        List(List(
          Term.Param
            .createWithComments(Nil, tname("x"), Some(pname("Int")), None, begComment = Seq("// c")),
        )),
      )),
      None,
      int(1),
    )
    parseAndCheckTree[Stat](code, layout)(tree)
    val reparsed = Defn.Def(Nil, tname("f"), Nil, List(List(tparam("x", pname("Int")))), None, int(1))
    runTestAssert[Stat](layout)(reparsed)
  }

}
