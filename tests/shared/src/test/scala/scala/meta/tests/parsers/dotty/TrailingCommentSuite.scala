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
         | extends B
         |""".stripMargin
    val tree = Defn.Class(
      Nil,
      Type.Name.createWithComments("A", endComment = Seq("// c")),
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
         | else d
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
         | catch {
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
         | finally d
         |""".stripMargin
    val tree = Term.Try(
      tname("a"),
      Some(
        Term.CasesBlock
          .createWithComments(List(Case(Pat.Wildcard(), None, tname("b"))), endComment = Seq("// c")),
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
         | while (b)
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
         | match {
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
         | B
         |""".stripMargin
    val tree = Defn.Type(
      Nil,
      pname("T"),
      Nil,
      Type.ApplyInfix(
        pname("A"),
        Type.Name.createWithComments("&", endComment = Seq("// c")),
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
         |// c
         |1
         |""".stripMargin
    val tree = Defn
      .Val(Nil, List(patvar("x")), None, Lit.Int.createWithComments(1, begComment = Seq("// c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("throw: comment after keyword") {
    val code =
      """|def f = throw // c
         |  e
         |""".stripMargin
    val layout =
      """|def f = throw 
         |// c
         |e
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
         |// c
         |x
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
         |  // c
         |  2
         |}
         |""".stripMargin
    val tree =
      tmatch(tname("x"), Case(int(1), None, Lit.Int.createWithComments(2, begComment = Seq("// c"))))
    runTestAssert[Stat](code, layout)(tree)
  }

}
