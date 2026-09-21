package scala.meta.tests.parsers

import scala.meta._

class TrailingCommentSuite extends ParseSuite {
  implicit val dialect: Dialect = dialects.Scala213

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
      """|def f = { return // c
         |  1 }
         |""".stripMargin
    val layout =
      """|def f = {
         |  return // c
         |  1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      Nil,
      None,
      blk(Term.Return.newBuilder(Lit.Unit()).endComment(Seq("// c")).result(), int(1)),
    )
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
    parseAndCheckTree[Stat](code, layout)(tree)
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
    assertEquals(x.endComment.get.values.last.newlinesAfter, -1)
    assertSyntax("func(x // X\n)")(Term.Apply(tname("func"), Term.ArgClause(List(x))))
  }

  test("block comment at end of input") {
    val x = term("x /* X */")
    assertEquals(x.endComment.get.values.last.newlinesAfter, -1)
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

}
