package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class ControlSyntaxSuite extends BaseDottySuite {
  implicit val parseStat: String => Stat = code => templStat(code)(dialects.Dotty)
  implicit val parseSource: String => Source = code => source(code)(dialects.Dotty)

  // --------------------------
  // IF
  // --------------------------

  test("old-if-else-single1") {
    val code = """|if (cond) fx
                  |else gx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("if (cond) fx else gx"))(
      Term.If(Term.Name("cond"), Term.Name("fx"), Term.Name("gx"))
    )
  }

  test("old-if-else-single2") {
    val code = """|if (cond)
                  |  fx
                  |else
                  |  gx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.Name("cond"), Term.Name("fx"), Term.IndentedBlock(List(Term.Name("gx"))))
    )
  }

  test("old-if-else-braces") {
    val code = """|if (cond) { fa1; fa2 }
                  |else {
                  |  fb1; fb2
                  |}
                  |""".stripMargin
    val output = """|if (cond) {
                    |  fa1
                    |  fa2
                    |} else {
                    |  fb1
                    |  fb2
                    |}""".stripMargin

    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(
        Term.Name("cond"),
        Term.Block(List(Term.Name("fa1"), Term.Name("fa2"))),
        Term.Block(List(Term.Name("fb1"), Term.Name("fb2")))
      )
    )
  }

  test("new-if-else-single1") {
    val code = """|if cond then fx
                  |else gx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.Name("cond"), Term.Name("fx"), Term.Name("gx"))
    )
  }

  test("new-if-else-single2") {
    val code = """|if cond then
                  |  fx
                  |else 
                  |  gx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(
        Term.Name("cond"),
        Term.IndentedBlock(List(Term.Name("fx"))),
        Term.IndentedBlock(List(Term.Name("gx")))
      )
    )
  }

  test("new-if-else-multiple") {
    val code = """|if cond then
                  |  fx1
                  |  fx2
                  |else 
                  |  gx1
                  |  gx2
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(
        Term.Name("cond"),
        Term.IndentedBlock(List(Term.Name("fx1"), Term.Name("fx2"))),
        Term.IndentedBlock(List(Term.Name("gx1"), Term.Name("gx2")))
      )
    )
  }

  // --------------------------
  // TRY
  // --------------------------

  test("old-try-finally") {
    val code = """|try { fx }
                  |finally { ok }
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Try(Term.Block(List(Term.Name("fx"))), Nil, Some(Term.Block(List(Term.Name("ok")))))
    )
  }

  test("new-try-finally-single") {
    val code = """|try
                  |  fx
                  |finally
                  |  ok
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Try(
        Term.IndentedBlock(List(Term.Name("fx"))),
        Nil,
        Some(Term.IndentedBlock(List(Term.Name("ok"))))
      )
    )
  }

  test("new-try-finally-multiple") {
    val code = """|try
                  |  fx
                  |  fy
                  |finally
                  |  ok1
                  |  ok2
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Try(
        Term.IndentedBlock(List(Term.Name("fx"), Term.Name("fy"))),
        Nil,
        Some(Term.IndentedBlock(List(Term.Name("ok1"), Term.Name("ok2"))))
      )
    )
  }

  test("old-try-catch-single") {
    val code = """|try fx 
                  |catch { case x => }
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Try(Term.Name("fx"), List(Case(Pat.Var(Term.Name("x")), None, Term.Block(Nil))), None)
    )
  }

  test("new-try-catch-multi") {
    val code = """|try
                  |  fx 
                  |  fy
                  |catch { case x => }
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Try(
        Term.IndentedBlock(List(Term.Name("fx"), Term.Name("fy"))),
        List(Case(Pat.Var(Term.Name("x")), None, Term.Block(Nil))),
        None
      )
    )
  }

  test("new-catch-single1") {
    val code = """|try fx
                  |catch case x => ()
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Try(Term.Name("fx"), List(Case(Pat.Var(Term.Name("x")), None, Lit.Unit())), None)
    )
  }

  test("new-catch-single2") {
    val code = """|try fx
                  |catch
                  |  case x =>
                  |    fa
                  |    fb
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Try(
        Term.Name("fx"),
        List(
          Case(Pat.Var(Term.Name("x")), None, Term.Block(List(Term.Name("fa"), Term.Name("fb"))))
        ),
        None
      )
    )
  }

  test("new-catch-multi") {
    val code = """|try fx
                  |catch
                  |  case x =>
                  |    xa
                  |    xb
                  |  case y => yab
                  |  case z =>
                  |    za
                  |    zb
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Try(
        Term.Name("fx"),
        List(
          Case(Pat.Var(Term.Name("x")), None, Term.Block(List(Term.Name("xa"), Term.Name("xb")))),
          Case(Pat.Var(Term.Name("y")), None, Term.Name("yab")),
          Case(Pat.Var(Term.Name("z")), None, Term.Block(List(Term.Name("za"), Term.Name("zb"))))
        ),
        None
      )
    )
  }

  // --------------------------
  // FOR
  // --------------------------

  test("for-single") {
    val code = """|for a <- x do fx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.For(List(Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("x"))), Term.Name("fx"))
    )
  }

  test("for-single-newline") {
    val code = """|for
                  |  a <- x
                  |do fx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.For(List(Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("x"))), Term.Name("fx"))
    )
  }

  test("for-multi") {
    val code = """|for
                  |  a <- x
                  |  b <- y
                  |do fx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.For(
        List(
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("x")),
          Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("y"))
        ),
        Term.Name("fx")
      )
    )
  }

  test("for-yield-single") {
    val code = """|for
                  |  a <- x
                  |  b <- y
                  |yield fx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("x")),
          Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("y"))
        ),
        Term.Name("fx")
      )
    )
  }

  test("for-case") {
    val code = """|for case a: TP <- iter do
                  |  echo
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.For(
        List(
          Enumerator
            .CaseGenerator(Pat.Typed(Pat.Var(Term.Name("a")), Type.Name("TP")), Term.Name("iter"))
        ),
        Term.IndentedBlock(List(Term.Name("echo")))
      )
    )
  }

  test("for-yield-multi") {
    val code = """|for
                  |  a <- x
                  |  b <- y
                  |yield
                  |  fx
                  |  fy
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("x")),
          Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("y"))
        ),
        Term.IndentedBlock(List(Term.Name("fx"), Term.Name("fy")))
      )
    )
  }

  // --------------------------
  // WHILE
  // --------------------------

  test("old-while-single") {
    val code = """|while (cond) fx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.While(Term.Name("cond"), Term.Name("fx"))
    )
  }

  test("old-while-multi") {
    val code = """|while (cond) {
                  |  fx
                  |  fy
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.While(Term.Name("cond"), Term.Block(List(Term.Name("fx"), Term.Name("fy"))))
    )
  }

  test("new-while-single1") {
    val code = """|while cond do fx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.While(Term.Name("cond"), Term.Name("fx"))
    )
  }

  test("new-while-single2") {
    val code = """|while cond
                  |do fx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.While(Term.Name("cond"), Term.Name("fx"))
    )
  }

  test("new-while-multi") {
    val code = """|while cond
                  |do
                  |  fx
                  |  fy
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(code))(
      Term.While(Term.Name("cond"), Term.IndentedBlock(List(Term.Name("fx"), Term.Name("fy"))))
    )
  }

}
