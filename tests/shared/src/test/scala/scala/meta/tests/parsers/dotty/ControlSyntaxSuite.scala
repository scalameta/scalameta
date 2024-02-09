package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.internal.tokenizers.ScalametaTokenizer

class ControlSyntaxSuite extends BaseDottySuite {

  // --------------------------
  // IF
  // --------------------------

  test("old-if-single1") {
    val code = "if (cond) -a else a"
    runTestAssert[Stat](code)(
      Term.If(tname("cond"), Term.ApplyUnary(tname("-"), tname("a")), tname("a"))
    )
  }

  test("old-if-else-single1") {
    val code = """|if (cond) fx
                  |else gx
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("if (cond) fx else gx"))(
      Term.If(tname("cond"), tname("fx"), tname("gx"))
    )
  }

  test("old-if-else-single2") {
    val code = """|if (cond)
                  |  fx
                  |else
                  |  gx
                  |""".stripMargin
    val output = """|if (cond) fx else gx""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(tname("cond"), tname("fx"), tname("gx"))
    )
  }

  test("old-if-else-braces") {
    val code = """|if (cond) {
                  |  fa1
                  |  fa2
                  |} else {
                  |  fb1
                  |  fb2
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code)(
      Term.If(
        tname("cond"),
        Term.Block(List(tname("fa1"), tname("fa2"))),
        Term.Block(List(tname("fb1"), tname("fb2")))
      )
    )
  }

  test("new-if-else-single1") {
    val code = """|if cond then fx
                  |else gx
                  |""".stripMargin
    val output = "if (cond) fx else gx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(tname("cond"), tname("fx"), tname("gx"))
    )
  }

  test("new-if-else-single2") {
    val code = """|if cond then
                  |  fx
                  |else 
                  |  gx
                  |""".stripMargin
    val output = "if (cond) fx else gx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(
        tname("cond"),
        tname("fx"),
        tname("gx")
      )
    )
  }

  test("new-if-single1") {
    val code = """|if cond1
                  |   && (cond2)
                  |then
                  |  gx
                  |""".stripMargin
    val output = "if (cond1 && cond2) gx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(
        Term.ApplyInfix(tname("cond1"), tname("&&"), Nil, List(tname("cond2"))),
        tname("gx"),
        Lit.Unit()
      )
    )
  }

  test("new-if-single2") {
    val code = """|if (cond1) || cond2(a1) then ok
                  |""".stripMargin
    val output = "if (cond1 || cond2(a1)) ok"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(
        Term.ApplyInfix(
          tname("cond1"),
          tname("||"),
          Nil,
          List(Term.Apply(tname("cond2"), List(tname("a1"))))
        ),
        tname("ok"),
        Lit.Unit()
      )
    )
  }

  test("new-if-single3") {
    val code = "if (cond1).cont then ok"
    val output = "if (cond1.cont) ok"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(Term.Select(tname("cond1"), tname("cont")), tname("ok"), Lit.Unit())
    )
  }

  test("new-if-expr-without-then") {
    val code =
      """|{
         |  if (x > 0) && (y > 0)
         |    x += 1
         |}
         |""".stripMargin
    val layout =
      """|{
         |  if (x > 0) &&(y > 0)
         |  x += 1
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(
      Term.Block(
        List(
          Term.If(
            Term.ApplyInfix(tname("x"), tname(">"), Nil, List(int(0))),
            Term.Apply(
              tname("&&"),
              List(Term.ApplyInfix(tname("y"), tname(">"), Nil, List(int(0))))
            ),
            Lit.Unit(),
            Nil
          ),
          Term.ApplyInfix(tname("x"), tname("+="), Nil, List(int(1)))
        )
      )
    )
  }

  test("new-if-expr-without-then-2") {
    val code =
      """|{
         |  if (x > 0) && y > 0
         |    x += 1
         |}
         |""".stripMargin
    runTestError[Stat](
      code,
      """|error: ; expected but integer constant found
         |  if (x > 0) && y > 0
         |                    ^""".stripMargin
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
    val output = """|if (cond) {
                    |  fx1
                    |  fx2
                    |} else {
                    |  gx1
                    |  gx2
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(
        tname("cond"),
        Term.Block(List(tname("fx1"), tname("fx2"))),
        Term.Block(List(tname("gx1"), tname("gx2")))
      )
    )
  }

  test("if-else-in-parens-1") {
    val code = """|fx(
                  |  if (cond)
                  |    A
                  |  else
                  |    B)
                  |""".stripMargin
    val output = "fx(if (cond) A else B)"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Apply(
        tname("fx"),
        List(
          Term.If(tname("cond"), tname("A"), tname("B"))
        )
      )
    )
  }

  test("if-else-in-parens-2") {
    val code = """|fx(
                  |  if cond then
                  |    A1
                  |    A2
                  |  else
                  |    B1
                  |    B2)
                  |""".stripMargin
    val output = """|fx(if (cond) {
                    |  A1
                    |  A2
                    |} else {
                    |  B1
                    |  B2
                    |})
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Apply(
        tname("fx"),
        List(
          Term.If(
            tname("cond"),
            Term.Block(List(tname("A1"), tname("A2"))),
            Term.Block(List(tname("B1"), tname("B2")))
          )
        )
      )
    )
  }

  test("new-if-indented") {
    val code = """|if (cond)
                  |  fx1
                  |  fx2
                  |""".stripMargin
    val output = """|if (cond) {
                    |  fx1
                    |  fx2
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(
        tname("cond"),
        Term.Block(List(tname("fx1"), tname("fx2"))),
        Lit.Unit()
      )
    )
  }

  test("new-if-else-indented") {
    val code = """|if cond
                  |  fx1
                  |  fx2
                  |else
                  |  gx
                  |""".stripMargin
    runTestError[Stat](code, "then expected but identifier found")
  }

  test("if-else-in-parens-3") {
    val code = """|fx(
                  |  if cond then
                  |    A1
                  |    A2
                  |  else
                  |    B1
                  |    B2,
                  |  secondArg
                  |)
                  |""".stripMargin
    val output = """|fx(if (cond) {
                    |  A1
                    |  A2
                    |} else {
                    |  B1
                    |  B2
                    |}, secondArg)
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Apply(
        tname("fx"),
        List(
          Term.If(
            tname("cond"),
            Term.Block(List(tname("A1"), tname("A2"))),
            Term.Block(List(tname("B1"), tname("B2")))
          ),
          tname("secondArg")
        )
      )
    )
  }

  // --------------------------
  // TRY
  // --------------------------

  test("old-try-finally1") {
    val code = """|try { fx }
                  |finally { ok }
                  |""".stripMargin
    val output = """|try {
                    |  fx
                    |} finally {
                    |  ok
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(Term.Block(List(tname("fx"))), Nil, Some(Term.Block(List(tname("ok")))))
    )
  }

  test("old-try-finally2") {
    val code = """|try fx
                  |finally ok
                  |""".stripMargin
    val output = "try fx finally ok"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(tname("fx"), Nil, Some(tname("ok")))
    )
  }

  test("new-try-finally-single") {
    val code = """|try
                  |  fx
                  |finally
                  |  ok
                  |""".stripMargin
    val output = "try fx finally ok"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        tname("fx"),
        Nil,
        Some(tname("ok"))
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
    val output = """|try {
                    |  fx
                    |  fy
                    |} finally {
                    |  ok1
                    |  ok2
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        Term.Block(List(tname("fx"), tname("fy"))),
        Nil,
        Some(Term.Block(List(tname("ok1"), tname("ok2"))))
      )
    )
  }

  test("old-try-catch-single") {
    val code = """|try fx 
                  |catch { case x => 1 }
                  |""".stripMargin
    val output = """|try fx catch {
                    |  case x => 1
                    |}""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(tname("fx"), List(Case(Pat.Var(tname("x")), None, int(1))), None)
    )
  }

  test("old-try-catch-multi") {
    val code = """|try fx 
                  |catch {
                  |  case x => 1
                  |  case y => 2
                  |}
                  |""".stripMargin
    val output = """|try fx catch {
                    |  case x => 1
                    |  case y => 2
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        tname("fx"),
        List(
          Case(Pat.Var(tname("x")), None, int(1)),
          Case(Pat.Var(tname("y")), None, int(2))
        ),
        None
      )
    )
  }

  test("new-try-catch-multi") {
    val code = """|try
                  |  fx 
                  |  fy
                  |catch { case x => ct }
                  |""".stripMargin
    val output = """|try {
                    |  fx
                    |  fy
                    |} catch {
                    |  case x => ct
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        Term.Block(List(tname("fx"), tname("fy"))),
        List(Case(Pat.Var(tname("x")), None, tname("ct"))),
        None
      )
    )
  }

  test("new-catch-single1") {
    val code = """|try fx
                  |catch case x => ct
                  |""".stripMargin
    val output = """|try fx catch {
                    |  case x => ct
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(tname("fx"), List(Case(Pat.Var(tname("x")), None, tname("ct"))), None)
    )
  }

  test("new-catch-single2") {
    val code = """|try fx
                  |catch
                  |  case x =>
                  |    fa
                  |    fb
                  |""".stripMargin
    val output = """|try fx catch {
                    |  case x =>
                    |    fa
                    |    fb
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        tname("fx"),
        List(
          Case(Pat.Var(tname("x")), None, Term.Block(List(tname("fa"), tname("fb"))))
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
    val output = """|try fx catch {
                    |  case x =>
                    |    xa
                    |    xb
                    |  case y => yab
                    |  case z =>
                    |    za
                    |    zb
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        tname("fx"),
        List(
          Case(Pat.Var(tname("x")), None, Term.Block(List(tname("xa"), tname("xb")))),
          Case(Pat.Var(tname("y")), None, tname("yab")),
          Case(Pat.Var(tname("z")), None, Term.Block(List(tname("za"), tname("zb"))))
        ),
        None
      )
    )
  }

  test("new-catch-handler-nl") {
    val code = """|try
                  |  foo
                  |catch
                  |  bar
                  |""".stripMargin
    val output = "try foo catch bar"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.TryWithHandler(tname("foo"), tname("bar"), None)
    )
  }

  test("new-catch-handler-finally-nl") {
    val code = """|try
                  |  foo
                  |catch
                  |  bar
                  |finally
                  |  baz
                  |""".stripMargin
    val output = "try foo catch bar finally baz"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.TryWithHandler(tname("foo"), tname("bar"), Some(tname("baz")))
    )
  }

  test("new-catch-finally-single") {
    val code = """|try fx
                  |catch case x =>
                  |  ax
                  |  bx
                  |finally
                  |  fx
                  |""".stripMargin
    val output = """|try fx catch {
                    |  case x =>
                    |    ax
                    |    bx
                    |} finally fx
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        tname("fx"),
        List(
          Case(Pat.Var(tname("x")), None, Term.Block(List(tname("ax"), tname("bx"))))
        ),
        Some(tname("fx"))
      )
    )
  }

  test("new-catch-inside-catch") {
    val code = """|{
                  |  try fx
                  |  catch case x =>
                  |    try fy
                  |    catch case y =>
                  |    throw ex
                  |  finally fxclose
                  |}
                  |""".stripMargin
    val output = """|{
                    |  try fx catch {
                    |    case x =>
                    |      try fy catch {
                    |        case y =>
                    |          throw ex
                    |      }
                    |  } finally fxclose
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Block(
        List(
          Term.Try(
            tname("fx"),
            List(
              Case(
                Pat.Var(tname("x")),
                None,
                Term.Try(
                  tname("fy"),
                  List(Case(Pat.Var(tname("y")), None, Term.Throw(tname("ex")))),
                  None
                )
              )
            ),
            Some(tname("fxclose"))
          )
        )
      )
    )
  }

  // --------------------------
  // FOR
  // --------------------------

  test("old-for-single1") {
    val code = "for (i <- 1 to 3) work"
    runTestAssert[Stat](code)(
      Term.For(
        List(
          Enumerator.Generator(
            Pat.Var(tname("i")),
            Term.ApplyInfix(int(1), tname("to"), Nil, List(int(3)))
          )
        ),
        tname("work")
      )
    )
  }

  test("old-for-single2") {
    val code = "for (i <- 1 to 10 if i < 4) work"
    val output = "for (i <- 1 to 10; if i < 4) work"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(
          Enumerator.Generator(
            Pat.Var(tname("i")),
            Term.ApplyInfix(int(1), tname("to"), Nil, List(int(10)))
          ),
          Enumerator.Guard(Term.ApplyInfix(tname("i"), tname("<"), Nil, List(int(4))))
        ),
        tname("work")
      )
    )
  }

  test("old-for-multi") {
    val code = """|for {
                  |  i <- gen
                  |  if i < 4
                  |} work
                  |""".stripMargin
    val output = "for (i <- gen; if i < 4) work"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(
          Enumerator.Generator(Pat.Var(tname("i")), tname("gen")),
          Enumerator.Guard(Term.ApplyInfix(tname("i"), tname("<"), Nil, List(int(4))))
        ),
        tname("work")
      )
    )
  }

  test("old-for-yield-single1") {
    val code = "for (i <- 1 to 3) yield i"
    runTestAssert[Stat](code)(
      Term.ForYield(
        List(
          Enumerator.Generator(
            Pat.Var(tname("i")),
            Term.ApplyInfix(int(1), tname("to"), Nil, List(int(3)))
          )
        ),
        tname("i")
      )
    )
  }

  test("old-for-yield-single2") {
    val code = "for { i <- gen } yield i"
    val output = "for (i <- gen) yield i"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(Enumerator.Generator(Pat.Var(tname("i")), tname("gen"))),
        tname("i")
      )
    )
  }

  test("old-for-yield-multi1") {
    val code = "for (i <- gen) yield {a; b}"
    val output = """|for (i <- gen) yield {
                    |  a
                    |  b
                    |}""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(Enumerator.Generator(Pat.Var(tname("i")), tname("gen"))),
        Term.Block(List(tname("a"), tname("b")))
      )
    )
  }

  test("old-for-yield-multi2") {
    val code = """|for {
                  |  i <- gen
                  |  if i < 4
                  |} yield { aa; bb }
                  |""".stripMargin
    val output = """|for (i <- gen; if i < 4) yield {
                    |  aa
                    |  bb
                    |}""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(tname("i")), tname("gen")),
          Enumerator.Guard(Term.ApplyInfix(tname("i"), tname("<"), Nil, List(int(4))))
        ),
        Term.Block(List(tname("aa"), tname("bb")))
      )
    )
  }

  test("new-fordo-single1") {
    val code = "for a <- gen do fx"
    val output = "for (a <- gen) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(Enumerator.Generator(Pat.Var(tname("a")), tname("gen"))),
        tname("fx")
      )
    )
  }

  test("new-fordo-single2") {
    val code = """|for
                  |  a <- gen
                  |do fx
                  |""".stripMargin
    val output = "for (a <- gen) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(Enumerator.Generator(Pat.Var(tname("a")), tname("gen"))),
        tname("fx")
      )
    )
  }

  test("new-fordo-single3") {
    val code = """|for a <- gen if cnd
                  |do fx
                  |""".stripMargin
    val output = "for (a <- gen; if cnd) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("gen")),
          Enumerator.Guard(tname("cnd"))
        ),
        tname("fx")
      )
    )
  }

  test("new-fordo-single4") {
    val code = """|for a <- gen
                  |do
                  |  fx
                  |""".stripMargin
    val output = "for (a <- gen) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(Enumerator.Generator(Pat.Var(tname("a")), tname("gen"))),
        tname("fx")
      )
    )
  }

  test("new-fordo-multi1") {
    val code = """|for
                  |  a <- x
                  |  b <- y
                  |do fx
                  |""".stripMargin
    val output = "for (a <- x; b <- y) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("x")),
          Enumerator.Generator(Pat.Var(tname("b")), tname("y"))
        ),
        tname("fx")
      )
    )
  }

  test("new-fordo-multi2") {
    val code = """|for
                  |  a <- x
                  |  b <- y
                  |do
                  |  fx
                  |  fy
                  |""".stripMargin
    val output = """|for (a <- x; b <- y) {
                    |  fx
                    |  fy
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("x")),
          Enumerator.Generator(Pat.Var(tname("b")), tname("y"))
        ),
        Term.Block(List(tname("fx"), tname("fy")))
      )
    )
  }

  test("new-for-indented-without-do") {
    val code = """|for ( a <- x )
                  |  fx
                  |  fy
                  |""".stripMargin
    val output = """|for (a <- x) {
                    |  fx
                    |  fy
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("x"))
        ),
        Term.Block(List(tname("fx"), tname("fy")))
      )
    )
  }

  test("new-for-indented-without-do2") {
    val code = """|for { a <- x }
                  |  fx
                  |  fy
                  |""".stripMargin
    val output = """|for (a <- x) {
                    |  fx
                    |  fy
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("x"))
        ),
        Term.Block(List(tname("fx"), tname("fy")))
      )
    )
  }

  test("new-for-yield-single1") {
    val code = """|for
                  |  a <- x
                  |  b <- y
                  |yield fx
                  |""".stripMargin
    val output = "for (a <- x; b <- y) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("x")),
          Enumerator.Generator(Pat.Var(tname("b")), tname("y"))
        ),
        tname("fx")
      )
    )
  }

  test("new-for-yield-single2") {
    val code = """|for a <- gen if cnd
                  |yield fx
                  |""".stripMargin
    val output = "for (a <- gen; if cnd) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("gen")),
          Enumerator.Guard(tname("cnd"))
        ),
        tname("fx")
      )
    )
  }

  test("new-for-yield-single3") {
    val code = """|for a <- gen if cnd
                  |yield
                  |  fx
                  |""".stripMargin
    val output = "for (a <- gen; if cnd) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("gen")),
          Enumerator.Guard(tname("cnd"))
        ),
        tname("fx")
      )
    )
  }

  test("new-for-yield-multi") {
    val code = """|for
                  |  a <- x
                  |  b <- y
                  |yield
                  |  fx
                  |  fy
                  |""".stripMargin
    val output = """|for (a <- x; b <- y) yield {
                    |  fx
                    |  fy
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("x")),
          Enumerator.Generator(Pat.Var(tname("b")), tname("y"))
        ),
        Term.Block(List(tname("fx"), tname("fy")))
      )
    )
  }

  test("new-for-case1") {
    val code = """|for case a: TP <- iter do
                  |  echo
                  |""".stripMargin
    val output = """|for ( case a: TP <- iter) echo
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(
          Enumerator
            .CaseGenerator(Pat.Typed(Pat.Var(tname("a")), pname("TP")), tname("iter"))
        ),
        tname("echo")
      )
    )
  }

  test("new-for-case2") {
    val code = """|for case a: TP <- iter if cnd do
                  |  echo
                  |""".stripMargin
    val output = """|for ( case a: TP <- iter; if cnd) echo
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(
          Enumerator
            .CaseGenerator(Pat.Typed(Pat.Var(tname("a")), pname("TP")), tname("iter")),
          Enumerator.Guard(tname("cnd"))
        ),
        tname("echo")
      )
    )
  }

  test("new-for-case3") {
    val code = """|for
                  |  x <- gen
                  |  case a1: TP <- iter1
                  |  if cnd
                  |  case a2: TP <- iter2
                  |do fn
                  |""".stripMargin
    val output = "for (x <- gen;  case a1: TP <- iter1; if cnd;  case a2: TP <- iter2) fn"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(
          Enumerator.Generator(Pat.Var(tname("x")), tname("gen")),
          Enumerator.CaseGenerator(
            Pat.Typed(Pat.Var(tname("a1")), pname("TP")),
            tname("iter1")
          ),
          Enumerator.Guard(tname("cnd")),
          Enumerator
            .CaseGenerator(Pat.Typed(Pat.Var(tname("a2")), pname("TP")), tname("iter2"))
        ),
        tname("fn")
      )
    )
  }

  test("for-new") {
    val code = """|for i <- gen
                  |    x = 3
                  |    if (cnd1) && cnd2
                  |yield work
                  |""".stripMargin
    val output = "for (i <- gen; x = 3; if cnd1 && cnd2) yield work"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(tname("i")), tname("gen")),
          Enumerator.Val(Pat.Var(tname("x")), int(3)),
          Enumerator.Guard(
            Term.ApplyInfix(tname("cnd1"), tname("&&"), Nil, List(tname("cnd2")))
          )
        ),
        tname("work")
      )
    )
  }

  test("multiline-for") {
    val code = """|for (a,b) <- gen
                  |  if a < 5
                  |  c <- otherGen
                  |yield c
                  |""".stripMargin
    val output = "for ((a, b) <- gen; if a < 5; c <- otherGen) yield c"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(
          Enumerator.Generator(
            Pat.Tuple(List(Pat.Var(tname("a")), Pat.Var(tname("b")))),
            tname("gen")
          ),
          Enumerator.Guard(Term.ApplyInfix(tname("a"), tname("<"), Nil, List(int(5)))),
          Enumerator.Generator(Pat.Var(tname("c")), tname("otherGen"))
        ),
        tname("c")
      )
    )
  }

  test("oneline-for") {
    val code = """|for (arg, param) <- args.zip(vparams) yield
                  |  arg
                  |""".stripMargin
    val output = """|for ((arg, param) <- args.zip(vparams)) yield arg
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(
          Enumerator.Generator(
            Pat.Tuple(List(Pat.Var(tname("arg")), Pat.Var(tname("param")))),
            Term.Apply(Term.Select(tname("args"), tname("zip")), List(tname("vparams")))
          )
        ),
        tname("arg")
      )
    )
  }

  // --------------------------
  // WHILE
  // --------------------------

  test("old-while-single") {
    val code = "while (cond) fx"
    runTestAssert[Stat](code)(
      Term.While(tname("cond"), tname("fx"))
    )
  }

  test("old-while-multi") {
    val code = """|while (cond) {
                  |  fx
                  |  fy
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code)(
      Term.While(tname("cond"), Term.Block(List(tname("fx"), tname("fy"))))
    )
  }

  test("new-while-single1") {
    val code = """|while cond do fx
                  |""".stripMargin
    val output = "while (cond) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(tname("cond"), tname("fx"))
    )
  }

  test("new-while-single2") {
    val code = """|while cond
                  |do fx
                  |""".stripMargin
    val output = "while (cond) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(tname("cond"), tname("fx"))
    )
  }

  test("new-while-indented-witout-do") {
    val code = """|while (cond)
                  |  fx
                  |  gx
                  |""".stripMargin
    val output = """|while (cond) {
                    |  fx
                    |  gx
                    |}""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(tname("cond"), Term.Block(List(tname("fx"), tname("gx"))))
    )
  }

  test("new-while-multi") {
    val code = """|while
                  |  fx +
                  |  fy
                  |do
                  |  fx
                  |  fy
                  |""".stripMargin
    val output = """|while (fx + fy) {
                    |  fx
                    |  fy
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(
        Term.ApplyInfix(tname("fx"), tname("+"), Nil, List(tname("fy"))),
        Term.Block(List(tname("fx"), tname("fy")))
      )
    )
  }

  test("new-while-multistat") {
    val code = """|while
                  |  s1
                  |  s2
                  |do
                  |  fx
                  |  fy
                  |""".stripMargin
    val output = """|while ({
                    |  s1
                    |  s2
                    |}) {
                    |  fx
                    |  fy
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(
        Term.Block(List(tname("s1"), tname("s2"))),
        Term.Block(List(tname("fx"), tname("fy")))
      )
    )
  }

  test("while-parens-yet-do") {
    val code = """|def read(): String = {
                  |  while (cond) do {}
                  |  other()
                  |}
                  |""".stripMargin
    val output = """|def read(): String = {
                    |  while (cond) {}
                    |  other()
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Def(
        Nil,
        tname("read"),
        Nil,
        List(List()),
        Some(pname("String")),
        Term.Block(
          List(Term.While(tname("cond"), Term.Block(Nil)), Term.Apply(tname("other"), Nil))
        )
      )
    )
  }

  test("while-cond-expr-do") {
    val code =
      """|  while (x > 0) && (y > 0) do
         |    x += 1
         |""".stripMargin

    val output = "while (x > 0 && y > 0) x += 1"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(
        Term.ApplyInfix(
          Term.ApplyInfix(tname("x"), tname(">"), Nil, List(int(0))),
          tname("&&"),
          Nil,
          List(Term.ApplyInfix(tname("y"), tname(">"), Nil, List(int(0))))
        ),
        Term.ApplyInfix(tname("x"), tname("+="), Nil, List(int(1)))
      )
    )
  }

  test("while-cond-expr-lf-do") {
    val code =
      """|  while (x > 0) && (y > 0)
         |  do
         |    x += 1
         |""".stripMargin

    val output = "while (x > 0 && y > 0) x += 1"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(
        Term.ApplyInfix(
          Term.ApplyInfix(tname("x"), tname(">"), Nil, List(int(0))),
          tname("&&"),
          Nil,
          List(Term.ApplyInfix(tname("y"), tname(">"), Nil, List(int(0))))
        ),
        Term.ApplyInfix(tname("x"), tname("+="), Nil, List(int(1)))
      )
    )
  }

  test("while-cond-expr-do [non symbolic op]") {
    val output = "while (x > 0 and y > 0) x += 1"
    val expected = Term.While(
      Term.ApplyInfix(
        Term.ApplyInfix(tname("x"), tname(">"), Nil, List(int(0))),
        tname("and"),
        Nil,
        List(Term.ApplyInfix(tname("y"), tname(">"), Nil, List(int(0))))
      ),
      Term.ApplyInfix(tname("x"), tname("+="), Nil, List(int(1)))
    )

    val code1 =
      """|  while (x > 0) and (y > 0) do
         |    x += 1
         |""".stripMargin
    runTestAssert[Stat](code1, assertLayout = Some(output))(expected)

    val code2 =
      """|  while (x > 0) and (y > 0)
         |  do
         |    x += 1
         |""".stripMargin
    runTestAssert[Stat](code2, assertLayout = Some(output))(expected)
  }

  test("if-cond-expr-with-apply-type") {
    val code =
      """|if (sym == defn.BooleanClass) classOf[Boolean]
         |else if (sym == defn.ByteClass) classOf[Byte]
         |""".stripMargin
    val layout =
      "if (sym == defn.BooleanClass) classOf[Boolean] else if (sym == defn.ByteClass) classOf[Byte]"
    val expected = Term.If(
      Term.ApplyInfix(
        tname("sym"),
        tname("=="),
        Nil,
        List(Term.Select(tname("defn"), tname("BooleanClass")))
      ),
      Term.ApplyType(tname("classOf"), List(pname("Boolean"))),
      Term.If(
        Term.ApplyInfix(
          tname("sym"),
          tname("=="),
          Nil,
          List(Term.Select(tname("defn"), tname("ByteClass")))
        ),
        Term.ApplyType(tname("classOf"), List(pname("Byte"))),
        Lit.Unit(),
        Nil
      ),
      Nil
    )
    runTestAssert[Stat](code, Some(layout))(expected)
  }

  test("if-cond-expr-with-block") {
    val code =
      """|if (sym == defn.BooleanClass) { classOf[Boolean] }
         |else if (sym == defn.ByteClass) classOf[Byte]
         |""".stripMargin
    val layout =
      """|if (sym == defn.BooleanClass) {
         |  classOf[Boolean]
         |} else if (sym == defn.ByteClass) classOf[Byte]
         |""".stripMargin
    val expected = Term.If(
      Term.ApplyInfix(
        tname("sym"),
        tname("=="),
        Nil,
        List(Term.Select(tname("defn"), tname("BooleanClass")))
      ),
      Term.Block(Term.ApplyType(tname("classOf"), List(pname("Boolean"))) :: Nil),
      Term.If(
        Term.ApplyInfix(
          tname("sym"),
          tname("=="),
          Nil,
          List(Term.Select(tname("defn"), tname("ByteClass")))
        ),
        Term.ApplyType(tname("classOf"), List(pname("Byte"))),
        Lit.Unit(),
        Nil
      ),
      Nil
    )
    runTestAssert[Stat](code, Some(layout))(expected)
  }

  test("while-cond-expr-without-do") {
    val code =
      """|{
         |  while (x > 0) && (y > 0)
         |    x += 1
         |}
         |""".stripMargin
    val layout =
      """|{
         |  while (x > 0) &&(y > 0)
         |  x += 1
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(
      Term.Block(
        List(
          Term.While(
            Term.ApplyInfix(tname("x"), tname(">"), Nil, List(int(0))),
            Term.Apply(
              tname("&&"),
              List(Term.ApplyInfix(tname("y"), tname(">"), Nil, List(int(0))))
            )
          ),
          Term.ApplyInfix(tname("x"), tname("+="), Nil, List(int(1)))
        )
      )
    )
  }

  test("while-cond-expr-without-do-2") {
    val code =
      """|{
         |  while (x > 0) && y > 0
         |    x += 1
         |}
         |""".stripMargin
    runTestError[Stat](
      code,
      """|error: ; expected but integer constant found
         |  while (x > 0) && y > 0
         |                       ^""".stripMargin
    )
  }

  // --------------------------
  // OTHER
  // --------------------------

  test("right-arrow-indentation-block") {
    val code = """|class A { slf =>
                  |
                  |  val x = 3
                  |}
                  |""".stripMargin
    val output = "class A { slf => val x = 3 }"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Class(
        Nil,
        pname("A"),
        Nil,
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          self("slf"),
          List(
            Defn.Val(Nil, List(Pat.Var(tname("x"))), None, int(3))
          )
        )
      )
    )
  }

  // --------------------------
  // MATCH CASE
  // --------------------------

  test("old-match-case-empty") {
    val code = """|x match {
                  |  case 1 =>
                  |  case 2 =>
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code)(
      Term.Match(
        tname("x"),
        List(Case(int(1), None, Term.Block(Nil)), Case(int(2), None, Term.Block(Nil)))
      )
    )
  }

  test("old-match-case-oneline") {
    val code = """|x match {
                  |  case 1 => "1"
                  |  case 2 => "2"
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code)(
      Term.Match(
        tname("x"),
        List(Case(int(1), None, str("1")), Case(int(2), None, str("2")))
      )
    )
  }

  test("old-match-case-multiline") {
    val code = """|x match {
                  |  case 1 =>
                  |    a1
                  |    b1
                  |  case 2 =>
                  |    a2
                  |    b2
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code)(
      Term.Match(
        tname("x"),
        List(
          Case(int(1), None, Term.Block(List(tname("a1"), tname("b1")))),
          Case(int(2), None, Term.Block(List(tname("a2"), tname("b2"))))
        )
      )
    )
  }

  test("old-match-case-inside") {
    val code = """|x match {
                  |  case 1 =>
                  |    y match {
                  |      case 5 => "5"
                  |      case 6 => "6"
                  |    }
                  |  case 2 =>
                  |    "2"
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code)(
      Term.Match(
        tname("x"),
        List(
          Case(
            int(1),
            None,
            Term.Match(
              tname("y"),
              List(Case(int(5), None, str("5")), Case(int(6), None, str("6")))
            )
          ),
          Case(int(2), None, str("2"))
        )
      )
    )
  }

  test("new-match-case-empty") {
    val code = """|x match
                  |  case 1 =>
                  |  case 2 =>
                  |""".stripMargin
    val output = """|x match {
                    |  case 1 =>
                    |  case 2 =>
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Match(
        tname("x"),
        List(Case(int(1), None, Term.Block(Nil)), Case(int(2), None, Term.Block(Nil)))
      )
    )
  }

  test("new-match-case-oneline") {
    val code = """|x match
                  |  case 1 => "1"
                  |  case 2 => "2"
                  |""".stripMargin
    val output = """|x match {
                    |  case 1 => "1"
                    |  case 2 => "2"
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Match(
        tname("x"),
        List(Case(int(1), None, str("1")), Case(int(2), None, str("2")))
      )
    )
  }

  test("new-match-case-multiline") {
    val code = """|x match
                  |  case 1 =>
                  |    a1
                  |    b1
                  |  case 2 =>
                  |    a2
                  |    b2
                  |""".stripMargin
    val output = """|x match {
                    |  case 1 =>
                    |    a1
                    |    b1
                    |  case 2 =>
                    |    a2
                    |    b2
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Match(
        tname("x"),
        List(
          Case(int(1), None, Term.Block(List(tname("a1"), tname("b1")))),
          Case(int(2), None, Term.Block(List(tname("a2"), tname("b2"))))
        )
      )
    )
  }

  test("old-match-case-one-align") {
    val code = """|cond match {
                  |  case a =>
                  |  fa
                  |  case b =>
                  |  fb
                  |}
                  |""".stripMargin
    val output = """|cond match {
                    |  case a => fa
                    |  case b => fb
                    |}
                    |""".stripMargin

    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Match(
        tname("cond"),
        List(
          Case(Pat.Var(tname("a")), None, tname("fa")),
          Case(Pat.Var(tname("b")), None, tname("fb"))
        )
      )
    )
  }

  test("new-match-case-oneline-align") {
    val code = """|def fx: String = {
                  |  x match
                  |  case 1 => "OK"
                  |  case 2 => "ERROR"
                  |  val c = "123"
                  |  c
                  |}
                  |""".stripMargin
    val output = """|def fx: String = {
                    |  x match {
                    |    case 1 => "OK"
                    |    case 2 => "ERROR"
                    |  }
                    |  val c = "123"
                    |  c
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Def(
        Nil,
        tname("fx"),
        Nil,
        Nil,
        Some(pname("String")),
        Term.Block(
          List(
            Term.Match(
              tname("x"),
              List(
                Case(int(1), None, str("OK")),
                Case(int(2), None, str("ERROR"))
              )
            ),
            Defn.Val(Nil, List(Pat.Var(tname("c"))), None, str("123")),
            tname("c")
          )
        )
      )
    )
  }

  test("new-match-case-oneline-align-newline") {
    val code = """|def fx: String = {
                  |  x match
                  |  case 2 =>
                  |    "ERROR"
                  |  end match
                  |}
                  |""".stripMargin
    val output = """|def fx: String = {
                    |  x match {
                    |    case 2 => "ERROR"
                    |  }
                    |  end match
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Def(
        Nil,
        tname("fx"),
        Nil,
        Nil,
        Some(pname("String")),
        Term.Block(
          List(
            Term.Match(tname("x"), List(Case(int(2), None, str("ERROR")))),
            Term.EndMarker(tname("match"))
          )
        )
      )
    )
  }

  test("unsure-correct") {
    val code = """|try func match
                  |  case A => Accept
                  |  catch case ex => Error
                  |""".stripMargin
    val output = """|try func match {
                    |  case A => Accept
                    |} catch {
                    |  case ex => Error
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        Term.Match(tname("func"), List(Case(tname("A"), None, tname("Accept")))),
        List(Case(Pat.Var(tname("ex")), None, tname("Error"))),
        None
      )
    )
  }

  test("case-match-ignore-indent") {
    val expected = """|x match {
                      |  case x =>
                      |    a()
                      |    b()
                      |}
                      |""".stripMargin
    runTestAssert[Stat](
      """|x match {
         |  case x =>
         |     a()
         |   b()
         |}
         |""".stripMargin,
      assertLayout = Some(expected)
    )(
      Term.Match(
        tname("x"),
        List(
          Case(
            Pat.Var(tname("x")),
            None,
            Term.Block(List(Term.Apply(tname("a"), Nil), Term.Apply(tname("b"), Nil)))
          )
        )
      )
    )
  }

  test("match-chained") {
    val expected = Term.Match(
      Term.Match(
        tname("xs"),
        List(
          Case(tname("Nil"), None, str("empty")),
          Case(
            Pat.ExtractInfix(
              Pat.Var(tname("x")),
              tname("::"),
              List(Pat.Var(tname("xs1")))
            ),
            None,
            str("nonempty")
          )
        )
      ),
      List(
        Case(str("empty"), None, int(0)),
        Case(str("nonempty"), None, int(1))
      )
    )

    runTestAssert[Stat](
      """|xs match {
         |  case Nil => "empty"
         |  case x :: xs1 => "nonempty"
         |} match {
         |  case "empty" => 0
         |  case "nonempty" => 1
         |}
         |""".stripMargin,
      Some(
        """|(xs match {
           |  case Nil => "empty"
           |  case x :: xs1 => "nonempty"
           |}) match {
           |  case "empty" => 0
           |  case "nonempty" => 1
           |}
           |""".stripMargin
      )
    )(expected)

    runTestAssert[Stat](
      """|xs match
         |  case Nil => "empty"
         |  case x :: xs1 => "nonempty"
         |  match
         |    case "empty" => 0
         |    case "nonempty" => 1
         |
         |""".stripMargin,
      assertLayout = Some(
        """|(xs match {
           |  case Nil => "empty"
           |  case x :: xs1 => "nonempty"
           |}) match {
           |  case "empty" => 0
           |  case "nonempty" => 1
           |}
           |""".stripMargin
      )
    )(expected)
  }

  test("match-chained-complex") {
    runTestAssert[Stat](
      """|val hello = xs match {
         |  case Nil => "empty"
         |  case x :: xs1 => "nonempty"
         |} startsWith "empty" match {
         |  case true => 0
         |  case false => 1
         |}
         |""".stripMargin,
      Some(
        """|val hello = (xs match {
           |  case Nil => "empty"
           |  case x :: xs1 => "nonempty"
           |}) startsWith "empty" match {
           |  case true => 0
           |  case false => 1
           |}
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("hello"))),
        None,
        Term.Match(
          Term.ApplyInfix(
            Term.Match(
              tname("xs"),
              List(
                Case(tname("Nil"), None, str("empty")),
                Case(
                  Pat.ExtractInfix(
                    Pat.Var(tname("x")),
                    tname("::"),
                    List(Pat.Var(tname("xs1")))
                  ),
                  None,
                  str("nonempty")
                )
              )
            ),
            tname("startsWith"),
            Nil,
            List(str("empty"))
          ),
          List(
            Case(bool(true), None, int(0)),
            Case(bool(false), None, int(1))
          )
        )
      )
    )

  }

  test("match-chained-complex-operator 1") {
    runTestAssert[Stat](
      """|val hello = xs match
         |  case Nil => 0
         |  case x :: xs1 => 1
         |+ 1 match
         |  case 1 => true
         |  case 2 => false
         |""".stripMargin,
      Some(
        """|val hello = (xs match {
           |  case Nil => 0
           |  case x :: xs1 => 1
           |}) + 1 match {
           |  case 1 => true
           |  case 2 => false
           |}
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("hello"))),
        None,
        Term.Match(
          Term.ApplyInfix(
            Term.Match(
              tname("xs"),
              List(
                Case(tname("Nil"), None, int(0)),
                Case(
                  Pat.ExtractInfix(Pat.Var(tname("x")), tname("::"), List(Pat.Var(tname("xs1")))),
                  None,
                  int(1)
                )
              ),
              Nil
            ),
            tname("+"),
            Nil,
            List(int(1))
          ),
          List(Case(int(1), None, bool(true)), Case(int(2), None, bool(false))),
          Nil
        )
      )
    )
  }

  test("match-chained-complex-operator 2") {
    runTestAssert[Stat](
      """|val hello = 1 + xs match
         |  case Nil => 0
         |  case x :: xs1 => 1
         |+ 1 match
         |  case 1 => true
         |  case 2 => false
         |""".stripMargin,
      Some(
        """|val hello = (1 + xs match {
           |  case Nil => 0
           |  case x :: xs1 => 1
           |}) + 1 match {
           |  case 1 => true
           |  case 2 => false
           |}
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("hello"))),
        None,
        Term.Match(
          Term.ApplyInfix(
            Term.Match(
              Term.ApplyInfix(int(1), tname("+"), Nil, List(tname("xs"))),
              List(
                Case(tname("Nil"), None, int(0)),
                Case(
                  Pat.ExtractInfix(Pat.Var(tname("x")), tname("::"), List(Pat.Var(tname("xs1")))),
                  None,
                  int(1)
                )
              ),
              Nil
            ),
            tname("+"),
            Nil,
            List(int(1))
          ),
          List(Case(int(1), None, bool(true)), Case(int(2), None, bool(false))),
          Nil
        )
      )
    )
  }

  test("match-chained-complex-operator 3") {
    runTestAssert[Stat](
      """|val hello =
         |  1 + xs match
         |    case Nil => 0
         |    case x :: xs1 => 1
         |  + 1 match
         |    case 1 => true
         |    case 2 => false
         |""".stripMargin,
      Some(
        """|val hello = (1 + xs match {
           |  case Nil => 0
           |  case x :: xs1 => 1
           |}) + 1 match {
           |  case 1 => true
           |  case 2 => false
           |}
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("hello"))),
        None,
        Term.Match(
          Term.ApplyInfix(
            Term.Match(
              Term.ApplyInfix(int(1), tname("+"), Nil, List(tname("xs"))),
              List(
                Case(tname("Nil"), None, int(0)),
                Case(
                  Pat.ExtractInfix(Pat.Var(tname("x")), tname("::"), List(Pat.Var(tname("xs1")))),
                  None,
                  int(1)
                )
              ),
              Nil
            ),
            tname("+"),
            Nil,
            List(int(1))
          ),
          List(Case(int(1), None, bool(true)), Case(int(2), None, bool(false))),
          Nil
        )
      )
    )
  }

  test("match-chained-complex-operator 4") {
    runTestAssert[Stat](
      """|val hello = 1 foo xs match
         |  case Nil => 0
         |  case x :: xs1 => 1
         |`foo` 1 match
         |  case 1 => true
         |  case 2 => false
         |
         |""".stripMargin,
      Some(
        """|val hello = (1 foo xs match {
           |  case Nil => 0
           |  case x :: xs1 => 1
           |}) foo 1 match {
           |  case 1 => true
           |  case 2 => false
           |}""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("hello"))),
        None,
        Term.Match(
          Term.ApplyInfix(
            Term.Match(
              Term.ApplyInfix(int(1), tname("foo"), Nil, List(tname("xs"))),
              List(
                Case(tname("Nil"), None, int(0)),
                Case(
                  Pat.ExtractInfix(Pat.Var(tname("x")), tname("::"), List(Pat.Var(tname("xs1")))),
                  None,
                  int(1)
                )
              ),
              Nil
            ),
            tname("foo"),
            Nil,
            List(int(1))
          ),
          List(Case(int(1), None, bool(true)), Case(int(2), None, bool(false))),
          Nil
        )
      )
    )
  }

  test("match-chained-complex-operator 5") {
    runTestAssert[Stat](
      """|val hello = 1 foo xs match
         |case Nil => 0
         |case x :: xs1 => 1
         |`foo` 1 match
         |  case 1 => true
         |  case 2 => false
         |
         |""".stripMargin,
      Some(
        """|val hello = (1 foo xs match {
           |  case Nil => 0
           |  case x :: xs1 => 1
           |}) foo 1 match {
           |  case 1 => true
           |  case 2 => false
           |}""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("hello"))),
        None,
        Term.Match(
          Term.ApplyInfix(
            Term.Match(
              Term.ApplyInfix(int(1), tname("foo"), Nil, List(tname("xs"))),
              List(
                Case(tname("Nil"), None, int(0)),
                Case(
                  Pat.ExtractInfix(Pat.Var(tname("x")), tname("::"), List(Pat.Var(tname("xs1")))),
                  None,
                  int(1)
                )
              ),
              Nil
            ),
            tname("foo"),
            Nil,
            List(int(1))
          ),
          List(Case(int(1), None, bool(true)), Case(int(2), None, bool(false))),
          Nil
        )
      )
    )
  }

  test("match-non-chained-complex-operator 6") {
    runTestAssert[Stat](
      """|{
         |  val hello = 1 foo xs match
         |  case Nil => 0
         |  case x :: xs1 => 1
         |  end match
         |  `foo` match
         |  case 1 => true
         |  case 2 => false
         |}
         |""".stripMargin,
      Some(
        """|{
           |  val hello = 1 foo xs match {
           |    case Nil => 0
           |    case x :: xs1 => 1
           |  }
           |  end match
           |  foo match {
           |    case 1 => true
           |    case 2 => false
           |  }
           |}""".stripMargin
      )
    )(
      Term.Block(
        List(
          Defn.Val(
            Nil,
            List(Pat.Var(tname("hello"))),
            None,
            Term.Match(
              Term.ApplyInfix(int(1), tname("foo"), Nil, List(tname("xs"))),
              List(
                Case(tname("Nil"), None, int(0)),
                Case(
                  Pat.ExtractInfix(Pat.Var(tname("x")), tname("::"), List(Pat.Var(tname("xs1")))),
                  None,
                  int(1)
                )
              ),
              Nil
            )
          ),
          Term.EndMarker(tname("match")),
          Term.Match(
            tname("foo"),
            List(Case(int(1), None, bool(true)), Case(int(2), None, bool(false))),
            Nil
          )
        )
      )
    )
  }

  test("match-nested") {
    runTestAssert[Stat](
      """|foo match
         |  case a =>
         |    bar match
         |      case aa =>
         |  case b =>
         |    baz
         |""".stripMargin,
      Some(
        """|foo match {
           |  case a =>
           |    bar match {
           |      case aa =>
           |    }
           |  case b =>
           |    baz
           |}""".stripMargin
      )
    )(
      Term.Match(
        tname("foo"),
        List(
          Case(
            Pat.Var(tname("a")),
            None,
            Term.Match(tname("bar"), List(Case(Pat.Var(tname("aa")), None, Term.Block(Nil))), Nil)
          ),
          Case(Pat.Var(tname("b")), None, tname("baz"))
        ),
        Nil
      )
    )
  }

  test("if-then-else with parens in cond and leading infix") {
    runTestAssert[Stat](
      """|def foo =
         |  if (bar eq baz)
         |                && (qux != xyz)
         |  then
         |          found
         |  else
         |          notfound
         |""".stripMargin,
      Some(
        """|def foo = if ((bar eq baz) && qux != xyz) found else notfound
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        tname("foo"),
        None,
        None,
        Term.If(
          Term.ApplyInfix(
            Term.ApplyInfix(tname("bar"), tname("eq"), Nil, List(tname("baz"))),
            tname("&&"),
            Nil,
            Term.ApplyInfix(tname("qux"), tname("!="), Nil, List(tname("xyz"))) :: Nil
          ),
          tname("found"),
          tname("notfound"),
          Nil
        )
      )
    )
  }

  test("if-then-else with parens in cond and non-leading infix") {
    runTestAssert[Stat](
      """|def foo =
         |  if (bar eq baz)
         |                .qux
         |  then
         |          found
         |  else
         |          notfound
         |""".stripMargin,
      Some(
        """|def foo = if ((bar eq baz).qux) found else notfound
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        tname("foo"),
        None,
        None,
        Term.If(
          Term.Select(
            Term.ApplyInfix(tname("bar"), tname("eq"), Nil, List(tname("baz"))),
            tname("qux")
          ),
          tname("found"),
          tname("notfound"),
          Nil
        )
      )
    )
  }

  test("match-dot") {
    val expected = Term.If(
      Term.Match(
        tname("xs"),
        List(
          Case(tname("Nil"), None, bool(false)),
          Case(Pat.Wildcard(), None, bool(true))
        )
      ),
      str("nonempty"),
      str("empty")
    )
    runTestAssert[Stat](
      """|if xs.match {
         |  case Nil => false
         |  case _ => true
         |}
         |then "nonempty"
         |else "empty"
         |""".stripMargin,
      Some(
        """|if (xs match {
           |  case Nil => false
           |  case _ => true
           |}) "nonempty" else "empty"
           |""".stripMargin
      )
    )(expected)

    runTestAssert[Stat](
      """|if xs.match
         |  case Nil => false
         |  case _ => true
         |then "nonempty"
         |else "empty"
         |""".stripMargin,
      assertLayout = Some(
        """|if (xs match {
           |  case Nil => false
           |  case _ => true
           |}) "nonempty" else "empty"
           |""".stripMargin
      )
    )(expected)
  }

  test("match-dot-def") {
    val expected = Defn.Def(
      Nil,
      tname("mtch"),
      Nil,
      List(List(tparam("x", "Int"))),
      Some(pname("String")),
      Term.Apply(
        Term.Select(
          Term.Match(
            tname("x"),
            List(
              Case(int(1), None, str("1")),
              Case(Pat.Wildcard(), None, str("ERR"))
            )
          ),
          tname("trim")
        ),
        Nil
      )
    )

    runTestAssert[Stat](
      """|def mtch(x: Int): String =
         |   x.match {
         |     case 1 => "1"
         |     case _ => "ERR"
         |   }.trim()
         |""".stripMargin,
      Some(
        """|def mtch(x: Int): String = (x match {
           |  case 1 => "1"
           |  case _ => "ERR"
           |}).trim()
           |""".stripMargin
      )
    )(expected)

    runTestAssert[Stat](
      """|def mtch(x: Int): String =
         |   x.match
         |     case 1 => "1"
         |     case _ => "ERR"
         |   .trim()
         |""".stripMargin,
      assertLayout = Some(
        """|def mtch(x: Int): String = (x match {
           |  case 1 => "1"
           |  case _ => "ERR"
           |}).trim()
           |""".stripMargin
      )
    )(expected)

  }

  test("catch-case-in-paren") {
    val code = """|fx(p1,
                  |   try func()
                  |   catch case x => ok())
                  |""".stripMargin
    val expected = """|fx(p1, try func() catch {
                      |  case x =>
                      |    ok()
                      |})""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(expected))(
      Term.Apply(
        tname("fx"),
        List(
          tname("p1"),
          Term.Try(
            Term.Apply(tname("func"), Nil),
            List(Case(Pat.Var(tname("x")), None, Term.Apply(tname("ok"), Nil))),
            None
          )
        )
      )
    )
  }

  test("match-braces-LFLF") {
    val code =
      """|a match {
         |  case A() =>
         |    succ
         |
         |  case _ => fail
         |}
         |""".stripMargin
    val expected =
      """|a match {
         |  case A() => succ
         |  case _ => fail
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(expected))(
      Term.Match(
        tname("a"),
        List(
          Case(Pat.Extract(tname("A"), Nil), None, tname("succ")),
          Case(Pat.Wildcard(), None, tname("fail"))
        ),
        Nil
      )
    )
  }

  test("match-last-empty") {
    val code =
      """|object Z:
         |  a match
         |    case A() =>
         |      succ
         |    case _ =>
         |  
         |  val x = 0
         |""".stripMargin
    val expected =
      """|object Z {
         |  a match {
         |    case A() => succ
         |    case _ =>
         |  }
         |  val x = 0
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(expected))(
      Defn.Object(
        Nil,
        tname("Z"),
        tpl(
          Term.Match(
            tname("a"),
            List(
              Case(Pat.Extract(tname("A"), Nil), None, tname("succ")),
              Case(Pat.Wildcard(), None, Term.Block(Nil))
            ),
            Nil
          ),
          Defn.Val(Nil, List(Pat.Var(tname("x"))), None, int(0))
        )
      )
    )
  }

  test("if-infix") {
    runTestAssert[Stat](
      """|if (1) max 10 gt 0
         |
         |then
         |  1
         |else
         |  2
         |""".stripMargin,
      assertLayout = Some(
        "if (1 max 10 gt 0) 1 else 2"
      )
    )(
      Term.If(
        Term.ApplyInfix(
          Term.ApplyInfix(int(1), tname("max"), Nil, List(int(10))),
          tname("gt"),
          Nil,
          List(int(0))
        ),
        int(1),
        int(2),
        Nil
      )
    )
  }

  test("no-real-indentation") {
    runTestAssert[Stat](
      """|object Test:
         |  try List(1, 2, 3) match
         |  case x :: xs => println(x)
         |  case Nil => println("Nil")
         |  catch
         |  case ex: java.io.IOException => println(ex)
         |  case ex: Throwable => throw ex
         |  end try
         |""".stripMargin,
      assertLayout = Some(
        """|object Test {
           |  try List(1, 2, 3) match {
           |    case x :: xs =>
           |      println(x)
           |    case Nil =>
           |      println("Nil")
           |  } catch {
           |    case ex: java.io.IOException =>
           |      println(ex)
           |    case ex: Throwable =>
           |      throw ex
           |  }
           |  end try
           |}
           |""".stripMargin
      )
    )(
      Defn.Object(
        Nil,
        tname("Test"),
        tpl(
          Term.Try(
            Term.Match(
              Term.Apply(tname("List"), List(int(1), int(2), int(3))),
              List(
                Case(
                  Pat.ExtractInfix(
                    Pat.Var(tname("x")),
                    tname("::"),
                    List(Pat.Var(tname("xs")))
                  ),
                  None,
                  Term.Apply(tname("println"), List(tname("x")))
                ),
                Case(
                  tname("Nil"),
                  None,
                  Term.Apply(tname("println"), List(str("Nil")))
                )
              ),
              Nil
            ),
            List(
              Case(
                Pat.Typed(
                  Pat.Var(tname("ex")),
                  Type.Select(
                    Term.Select(tname("java"), tname("io")),
                    pname("IOException")
                  )
                ),
                None,
                Term.Apply(tname("println"), List(tname("ex")))
              ),
              Case(
                Pat.Typed(Pat.Var(tname("ex")), pname("Throwable")),
                None,
                Term.Throw(tname("ex"))
              )
            ),
            None
          ),
          Term.EndMarker(tname("try"))
        )
      )
    )
  }

  test("comma-indent") {
    runTestAssert[Stat](
      """|def f(x: Int) =
         |  assert(
         |    if x > 0 then
         |      true
         |    else
         |      if x < 0 then
         |        true
         |      else
         |        false, "fail")
         |""".stripMargin,
      assertLayout = Some(
        """|def f(x: Int) = assert(if (x > 0) true else if (x < 0) true else false, "fail")
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(List(tparam("x", "Int"))),
        None,
        Term.Apply(
          tname("assert"),
          List(
            Term.If(
              Term.ApplyInfix(tname("x"), tname(">"), Nil, List(int(0))),
              bool(true),
              Term.If(
                Term.ApplyInfix(tname("x"), tname("<"), Nil, List(int(0))),
                bool(true),
                bool(false),
                Nil
              ),
              Nil
            ),
            str("fail")
          )
        )
      )
    )
  }

  test("if-then 1") {
    runTestAssert[Stat](
      """|object foo:
         |  def bar =
         |    if map:
         |      op(c == true)
         |    then err(context)
         |""".stripMargin,
      assertLayout = Some(
        """|object foo { def bar = if (map(op(c == true))) err(context) }
           |""".stripMargin
      )
    )(
      Defn.Object(
        Nil,
        tname("foo"),
        tpl(
          Defn.Def(
            Nil,
            tname("bar"),
            Nil,
            None,
            Term.If(
              Term.Apply(
                tname("map"),
                Term.Apply(
                  tname("op"),
                  Term.ApplyInfix(tname("c"), tname("=="), Nil, List(bool(true))) :: Nil
                ) :: Nil
              ),
              Term.Apply(tname("err"), List(tname("context"))),
              Lit.Unit(),
              Nil
            )
          )
        )
      )
    )
  }

  test("if-then 2") {
    runTestAssert[Stat](
      """|object foo:
         |  def bar =
         |    if (a + b) { op(c == true) } then err(context)
         |""".stripMargin,
      assertLayout = Some(
        "object foo { def bar = if ((a + b)(op(c == true))) err(context) }"
      )
    )(
      Defn.Object(
        Nil,
        tname("foo"),
        tpl(
          Defn.Def(
            Nil,
            tname("bar"),
            Nil,
            None,
            Term.If(
              Term.Apply(
                Term.ApplyInfix(tname("a"), tname("+"), Nil, List(tname("b"))),
                Term.Apply(
                  tname("op"),
                  Term.ApplyInfix(tname("c"), tname("=="), Nil, List(bool(true))) :: Nil
                ) :: Nil
              ),
              Term.Apply(tname("err"), List(tname("context"))),
              Lit.Unit(),
              Nil
            )
          )
        )
      )
    )
  }

  test("if-then 3") {
    runTestAssert[Stat](
      """|object foo:
         |  def bar =
         |    if (a + b) op c == true then err(context)
         |""".stripMargin,
      assertLayout = Some(
        """|object foo { def bar = if (a + b op c == true) err(context) }
           |""".stripMargin
      )
    )(
      Defn.Object(
        Nil,
        tname("foo"),
        tpl(
          Defn.Def(
            Nil,
            tname("bar"),
            Nil,
            None,
            Term.If(
              Term.ApplyInfix(
                Term.ApplyInfix(tname("a"), tname("+"), Nil, List(tname("b"))),
                tname("op"),
                Nil,
                Term.ApplyInfix(tname("c"), tname("=="), Nil, List(bool(true))) :: Nil
              ),
              Term.Apply(tname("err"), List(tname("context"))),
              Lit.Unit(),
              Nil
            )
          )
        )
      )
    )
  }

  test("if-then 4") {
    runTestAssert[Stat](
      """|object foo:
         |  def bar =
         |    if (a + b) op c err(context)
         |""".stripMargin,
      assertLayout = Some(
        """|object foo { def bar = if (a + b) op c err(context) }
           |""".stripMargin
      )
    )(
      Defn.Object(
        Nil,
        tname("foo"),
        tpl(
          Defn.Def(
            Nil,
            tname("bar"),
            Nil,
            None,
            Term.If(
              Term.ApplyInfix(tname("a"), tname("+"), Nil, List(tname("b"))),
              Term.ApplyInfix(
                tname("op"),
                tname("c"),
                Nil,
                List(Term.Apply(tname("err"), List(tname("context"))))
              ),
              Lit.Unit(),
              Nil
            )
          )
        )
      )
    )
  }

  test("if-then 5") {
    runTestAssert[Stat](
      """|object foo:
         |  def bar =
         |    if (a + b) (op c err(context))
         |""".stripMargin,
      assertLayout = Some(
        """|object foo { def bar = if (a + b) op c err(context) }
           |""".stripMargin
      )
    )(
      Defn.Object(
        Nil,
        tname("foo"),
        tpl(
          Defn.Def(
            Nil,
            tname("bar"),
            Nil,
            None,
            Term.If(
              Term.ApplyInfix(tname("a"), tname("+"), Nil, List(tname("b"))),
              Term.ApplyInfix(
                tname("op"),
                tname("c"),
                Nil,
                List(Term.Apply(tname("err"), List(tname("context"))))
              ),
              Lit.Unit(),
              Nil
            )
          )
        )
      )
    )
  }

  test("if-then 6") {
    val code =
      """|object a {
         |  constraint.contains(tl) || other.isRemovable(tl) || {
         |          val tvars = tl.paramRefs.map(other.typeVarOfParam(_)).collect { case tv: TypeVar => tv }
         |          if this.isCommittable then
         |            tvars.foreach(tvar => if !tvar.inst.exists && !isOwnedAnywhere(this, tvar) then includeVar(tvar))
         |          typeComparer.addToConstraint(tl, tvars)
         |        }
         |}
         |""".stripMargin
    val output =
      """|object a {
         |  constraint.contains(tl) || other.isRemovable(tl) || {
         |    val tvars = tl.paramRefs.map(other.typeVarOfParam(_)).collect {
         |      case tv: TypeVar => tv
         |    }
         |    if (this.isCommittable) tvars.foreach(tvar => if (!tvar.inst.exists && !isOwnedAnywhere(this, tvar)) includeVar(tvar))
         |    typeComparer.addToConstraint(tl, tvars)
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Object(
        Nil,
        tname("a"),
        tpl(
          Term.ApplyInfix(
            Term.ApplyInfix(
              Term.Apply(
                Term.Select(tname("constraint"), tname("contains")),
                List(tname("tl"))
              ),
              tname("||"),
              Nil,
              Term.Apply(
                Term.Select(tname("other"), tname("isRemovable")),
                List(tname("tl"))
              ) :: Nil
            ),
            tname("||"),
            Nil,
            Term.Block(
              List(
                Defn.Val(
                  Nil,
                  List(Pat.Var(tname("tvars"))),
                  None,
                  Term.Apply(
                    Term.Select(
                      Term.Apply(
                        Term.Select(Term.Select(tname("tl"), tname("paramRefs")), tname("map")),
                        Term.AnonymousFunction(
                          Term.Apply(
                            Term.Select(tname("other"), tname("typeVarOfParam")),
                            List(Term.Placeholder())
                          )
                        ) :: Nil
                      ),
                      tname("collect")
                    ),
                    Term.PartialFunction(
                      Case(
                        Pat.Typed(Pat.Var(tname("tv")), pname("TypeVar")),
                        None,
                        tname("tv")
                      ) :: Nil
                    ) :: Nil
                  )
                ),
                Term.If(
                  Term.Select(Term.This(anon), tname("isCommittable")),
                  Term.Apply(
                    Term.Select(tname("tvars"), tname("foreach")),
                    Term.Function(
                      List(tparam("tvar")),
                      Term.If(
                        Term.ApplyInfix(
                          Term.ApplyUnary(
                            tname("!"),
                            Term.Select(
                              Term.Select(tname("tvar"), tname("inst")),
                              tname("exists")
                            )
                          ),
                          tname("&&"),
                          Nil,
                          Term.ApplyUnary(
                            tname("!"),
                            Term.Apply(
                              tname("isOwnedAnywhere"),
                              List(Term.This(anon), tname("tvar"))
                            )
                          ) :: Nil
                        ),
                        Term.Apply(
                          tname("includeVar"),
                          List(tname("tvar"))
                        ),
                        Lit.Unit(),
                        Nil
                      )
                    ) :: Nil
                  ),
                  Lit.Unit(),
                  Nil
                ),
                Term.Apply(
                  Term.Select(tname("typeComparer"), tname("addToConstraint")),
                  List(tname("tl"), tname("tvars"))
                )
              )
            ) :: Nil
          )
        )
      )
    )
  }

  test("if-then 7") {
    val code =
      """
  private def genScalaClass(td: TypeDef): js.ClassDef = {
    val hashedDefs = ir.Hashers.hashMemberDefs(allMemberDefs)

    val kind =
      if (isStaticModule(sym)) ClassKind.ModuleClass
      else if (isHijacked) ClassKind.HijackedClass
      else ClassKind.Class
  }
    """.stripMargin
    val output =
      """|private def genScalaClass(td: TypeDef): js.ClassDef = {
         |  val hashedDefs = ir.Hashers.hashMemberDefs(allMemberDefs)
         |  val kind = if (isStaticModule(sym)) ClassKind.ModuleClass else if (isHijacked) ClassKind.HijackedClass else ClassKind.Class
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Def(
        List(Mod.Private(anon)),
        tname("genScalaClass"),
        Nil,
        List(List(tparam("td", "TypeDef"))),
        Some(Type.Select(tname("js"), pname("ClassDef"))),
        Term.Block(
          List(
            Defn.Val(
              Nil,
              List(Pat.Var(tname("hashedDefs"))),
              None,
              Term.Apply(
                Term.Select(Term.Select(tname("ir"), tname("Hashers")), tname("hashMemberDefs")),
                List(tname("allMemberDefs"))
              )
            ),
            Defn.Val(
              Nil,
              List(Pat.Var(tname("kind"))),
              None,
              Term.If(
                Term.Apply(tname("isStaticModule"), List(tname("sym"))),
                Term.Select(tname("ClassKind"), tname("ModuleClass")),
                Term.If(
                  tname("isHijacked"),
                  Term.Select(tname("ClassKind"), tname("HijackedClass")),
                  Term.Select(tname("ClassKind"), tname("Class")),
                  Nil
                ),
                Nil
              )
            )
          )
        )
      )
    )
  }

  test("try catch with case guard") {
    runTestAssert[Stat](
      """|arg match {
         |  case arg: Showable =>
         |    try arg.show
         |    catch {
         |      case ex: CyclicReference => "... (caught cyclic reference) ..."
         |      case NonFatal(ex)
         |      if !ctx.mode.is(Mode.PrintShowExceptions) &&
         |         !ctx.settings.YshowPrintErrors.value =>
         |        val msg = ex match
         |          case te: TypeError => te.toMessage
         |          case _ => ex.getMessage
         |        s"[cannot display due to $msg, raw string = ${arg.toString}]"
         |    }
         |  case _ => String.valueOf(arg)
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|arg match {
           |  case arg: Showable =>
           |    try arg.show catch {
           |      case ex: CyclicReference => "... (caught cyclic reference) ..."
           |      case NonFatal(ex) if !ctx.mode.is(Mode.PrintShowExceptions) && !ctx.settings.YshowPrintErrors.value =>
           |        val msg = ex match {
           |          case te: TypeError =>
           |            te.toMessage
           |          case _ =>
           |            ex.getMessage
           |        }
           |        s"[cannot display due to $msg, raw string = ${arg.toString}]"
           |    }
           |  case _ =>
           |    String.valueOf(arg)
           |}
           |""".stripMargin
      )
    )(
      Term.Match(
        tname("arg"),
        List(
          Case(
            Pat.Typed(Pat.Var(tname("arg")), pname("Showable")),
            None,
            Term.Try(
              Term.Select(tname("arg"), tname("show")),
              List(
                Case(
                  Pat.Typed(Pat.Var(tname("ex")), pname("CyclicReference")),
                  None,
                  str("... (caught cyclic reference) ...")
                ),
                Case(
                  Pat.Extract(tname("NonFatal"), List(Pat.Var(tname("ex")))),
                  Some(
                    Term.ApplyInfix(
                      Term.ApplyUnary(
                        tname("!"),
                        Term.Apply(
                          Term.Select(Term.Select(tname("ctx"), tname("mode")), tname("is")),
                          List(Term.Select(tname("Mode"), tname("PrintShowExceptions")))
                        )
                      ),
                      tname("&&"),
                      Nil,
                      Term.ApplyUnary(
                        tname("!"),
                        Term.Select(
                          Term.Select(
                            Term.Select(tname("ctx"), tname("settings")),
                            tname("YshowPrintErrors")
                          ),
                          tname("value")
                        )
                      ) :: Nil
                    )
                  ),
                  Term.Block(
                    List(
                      Defn.Val(
                        Nil,
                        List(Pat.Var(tname("msg"))),
                        None,
                        Term.Match(
                          tname("ex"),
                          List(
                            Case(
                              Pat.Typed(Pat.Var(tname("te")), pname("TypeError")),
                              None,
                              Term.Select(tname("te"), tname("toMessage"))
                            ),
                            Case(
                              Pat.Wildcard(),
                              None,
                              Term.Select(tname("ex"), tname("getMessage"))
                            )
                          ),
                          Nil
                        )
                      ),
                      Term.Interpolate(
                        tname("s"),
                        List(str("[cannot display due to "), str(", raw string = "), str("]")),
                        List(tname("msg"), Term.Select(tname("arg"), tname("toString")))
                      )
                    )
                  )
                )
              ),
              None
            )
          ),
          Case(
            Pat.Wildcard(),
            None,
            Term.Apply(Term.Select(tname("String"), tname("valueOf")), List(tname("arg")))
          )
        ),
        Nil
      )
    )
  }

  test("partial function with guard") {
    runTestAssert[Stat](
      """|val tvars = targs.filter(_.isInstanceOf[InferredTypeTree]).tpes.collect {
         |  case tvar: TypeVar
         |  if !tvar.isInstantiated &&
         |     ctx.typerState.ownedVars.contains(tvar) &&
         |     !locked.contains(tvar) => tvar
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|val tvars = targs.filter(_.isInstanceOf[InferredTypeTree]).tpes.collect {
           |  case tvar: TypeVar if !tvar.isInstantiated && ctx.typerState.ownedVars.contains(tvar) && !locked.contains(tvar) => tvar
           |}
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("tvars"))),
        None,
        Term.Apply(
          Term.Select(
            Term.Select(
              Term.Apply(
                Term.Select(tname("targs"), tname("filter")),
                Term.AnonymousFunction(
                  Term.ApplyType(
                    Term.Select(Term.Placeholder(), tname("isInstanceOf")),
                    List(pname("InferredTypeTree"))
                  )
                ) :: Nil
              ),
              tname("tpes")
            ),
            tname("collect")
          ),
          Term.PartialFunction(
            Case(
              Pat.Typed(Pat.Var(tname("tvar")), pname("TypeVar")),
              Some(
                Term.ApplyInfix(
                  Term.ApplyInfix(
                    Term.ApplyUnary(
                      tname("!"),
                      Term.Select(tname("tvar"), tname("isInstantiated"))
                    ),
                    tname("&&"),
                    Nil,
                    Term.Apply(
                      Term.Select(
                        Term.Select(
                          Term.Select(tname("ctx"), tname("typerState")),
                          tname("ownedVars")
                        ),
                        tname("contains")
                      ),
                      List(tname("tvar"))
                    ) :: Nil
                  ),
                  tname("&&"),
                  Nil,
                  Term.ApplyUnary(
                    tname("!"),
                    Term.Apply(Term.Select(tname("locked"), tname("contains")), List(tname("tvar")))
                  ) :: Nil
                )
              ),
              tname("tvar")
            ) :: Nil
          ) :: Nil
        )
      )
    )
  }

  test("nested if else without indent") {
    runTestAssert[Stat](
      """|if columnsVar != null then columnsVar.toInt
         |else if Properties.isWin then
         |  if ansiconVar != null then
         |    ansiconVar.toInt
         |  else defaultWidth
         |else defaultWidth
         |""".stripMargin,
      assertLayout = Some(
        """|if (columnsVar != null) columnsVar.toInt else if (Properties.isWin) if (ansiconVar != null) ansiconVar.toInt else defaultWidth else defaultWidth
           |""".stripMargin
      )
    )(
      Term.If(
        Term.ApplyInfix(tname("columnsVar"), tname("!="), Nil, List(Lit.Null())),
        Term.Select(tname("columnsVar"), tname("toInt")),
        Term.If(
          Term.Select(tname("Properties"), tname("isWin")),
          Term.If(
            Term.ApplyInfix(tname("ansiconVar"), tname("!="), Nil, List(Lit.Null())),
            Term.Select(tname("ansiconVar"), tname("toInt")),
            tname("defaultWidth"),
            Nil
          ),
          tname("defaultWidth"),
          Nil
        ),
        Nil
      )
    )
  }

  test("if-then without else") {
    runTestAssert[Stat](
      """|object foo:
         |  private def assertBounds(context: String) =
         |    if idx >= query.length then err(context)
         |
         |  private def err(problem: String) =
         |    throw new QueryParseException(query, idx, problem)
         |""".stripMargin,
      assertLayout = Some(
        """|object foo {
           |  private def assertBounds(context: String) = if (idx >= query.length) err(context)
           |  private def err(problem: String) = throw new QueryParseException(query, idx, problem)
           |}
           |""".stripMargin
      )
    )(
      Defn.Object(
        Nil,
        tname("foo"),
        tpl(
          Defn.Def(
            List(Mod.Private(anon)),
            tname("assertBounds"),
            Nil,
            List(List(tparam("context", "String"))),
            None,
            Term.If(
              Term.ApplyInfix(
                tname("idx"),
                tname(">="),
                Nil,
                List(Term.Select(tname("query"), tname("length")))
              ),
              Term.Apply(tname("err"), List(tname("context"))),
              Lit.Unit(),
              Nil
            )
          ),
          Defn.Def(
            List(Mod.Private(anon)),
            tname("err"),
            Nil,
            List(List(tparam("problem", "String"))),
            None,
            Term.Throw(
              Term.New(
                Init(
                  pname("QueryParseException"),
                  anon,
                  List(List(tname("query"), tname("idx"), tname("problem")))
                )
              )
            )
          )
        )
      )
    )
  }

  test("old-style if") {
    runTestAssert[Stat](
      """|while (idx < str.length)
         |  if ((str charAt idx) != '$' || isEscaped)
         |    idx += 1
         |  else
         |    idx
         |""".stripMargin,
      assertLayout = Some(
        """|while (idx < str.length) if ((str charAt idx) != '$' || isEscaped) idx += 1 else idx
           |""".stripMargin
      )
    )(
      Term.While(
        Term.ApplyInfix(
          tname("idx"),
          tname("<"),
          Nil,
          List(Term.Select(tname("str"), tname("length")))
        ),
        Term.If(
          Term.ApplyInfix(
            Term.ApplyInfix(
              Term.ApplyInfix(tname("str"), tname("charAt"), Nil, List(tname("idx"))),
              tname("!="),
              Nil,
              List(Lit.Char('$'))
            ),
            tname("||"),
            Nil,
            List(tname("isEscaped"))
          ),
          Term.ApplyInfix(tname("idx"), tname("+="), Nil, List(int(1))),
          tname("idx"),
          Nil
        )
      )
    )
  }

  test("old-style for and if") {
    runTestAssert[Stat](
      """|for (c <- str)
         |    if (c == '\n') { line += 1; char = 0 } else { char += 1 }
         |""".stripMargin,
      assertLayout = Some(
        """|for (c <- str) if (c == '\n') {
           |  line += 1
           |  char = 0
           |} else {
           |  char += 1
           |}
           |""".stripMargin
      )
    )(
      Term.For(
        List(Enumerator.Generator(Pat.Var(tname("c")), tname("str"))),
        Term.If(
          Term.ApplyInfix(tname("c"), tname("=="), Nil, List(Lit.Char('\n'))),
          Term.Block(
            List(
              Term.ApplyInfix(tname("line"), tname("+="), Nil, List(int(1))),
              Term.Assign(tname("char"), int(0))
            )
          ),
          Term.Block(
            Term.ApplyInfix(tname("char"), tname("+="), Nil, List(int(1))) :: Nil
          ),
          Nil
        )
      )
    )
  }

  test("several if-then, some nested") {
    val code =
      """|classDef
         |      .foreach {
         |        case typeSymbol: Symbol =>
         |          if typ then {
         |            // foo
         |          }
         |          if typeDef then
         |            if typJava then {
         |              // foo
         |            }
         |        case _ =>
         |    }
         |""".stripMargin
    val output =
      """|classDef.foreach {
         |  case typeSymbol: Symbol =>
         |    if (typ) {}
         |    if (typeDef) if (typJava) {}
         |  case _ =>
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Apply(
        Term.Select(tname("classDef"), tname("foreach")),
        Term.PartialFunction(
          List(
            Case(
              Pat.Typed(Pat.Var(tname("typeSymbol")), pname("Symbol")),
              None,
              Term.Block(
                List(
                  Term.If(tname("typ"), Term.Block(Nil), Lit.Unit(), Nil),
                  Term.If(
                    tname("typeDef"),
                    Term.If(tname("typJava"), Term.Block(Nil), Lit.Unit(), Nil),
                    Lit.Unit(),
                    Nil
                  )
                )
              )
            ),
            Case(Pat.Wildcard(), None, Term.Block(Nil))
          )
        ) :: Nil
      )
    )
  }

  test("old-style if, with outdent") {
    val code =
      """|if (indexes.size > 1)
         |          val msg = s"ERROR: Multiple index pages for doc found ${indexes.map(_.file)}"
         |          report.error(msg)
         |""".stripMargin
    val output =
      """|if (indexes.size > 1) {
         |  val msg = s"ERROR: Multiple index pages for doc found ${indexes.map(_.file)}"
         |  report.error(msg)
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(
        Term.ApplyInfix(
          Term.Select(tname("indexes"), tname("size")),
          tname(">"),
          Nil,
          List(int(1))
        ),
        Term.Block(
          List(
            Defn.Val(
              Nil,
              List(Pat.Var(tname("msg"))),
              None,
              Term.Interpolate(
                tname("s"),
                List(str("ERROR: Multiple index pages for doc found "), str("")),
                Term.Apply(
                  Term.Select(tname("indexes"), tname("map")),
                  List(Term.AnonymousFunction(Term.Select(Term.Placeholder(), tname("file"))))
                ) :: Nil
              )
            ),
            Term.Apply(
              Term.Select(tname("report"), tname("error")),
              List(tname("msg"))
            )
          )
        ),
        Lit.Unit(),
        Nil
      )
    )
  }

  test("if-then in parens") {
    val code =
      """|{
         |  if this then
         |    tvars.foreach(tvar => if !tvar then includeVar(tvar))
         |  typeComparer.addToConstraint(tvars)
         |}
         |""".stripMargin
    val output =
      """|{
         |  if (this) tvars.foreach(tvar => if (!tvar) includeVar(tvar))
         |  typeComparer.addToConstraint(tvars)
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Block(
        List(
          Term.If(
            Term.This(anon),
            Term.Apply(
              Term.Select(tname("tvars"), tname("foreach")),
              Term.Function(
                Term.ParamClause(List(tparam("tvar"))),
                Term.If(
                  Term.ApplyUnary(tname("!"), tname("tvar")),
                  Term.Apply(tname("includeVar"), List(tname("tvar"))),
                  Lit.Unit(),
                  Nil
                )
              ) :: Nil
            ),
            Lit.Unit(),
            Nil
          ),
          Term.Apply(
            Term.Select(tname("typeComparer"), tname("addToConstraint")),
            List(tname("tvars"))
          )
        )
      )
    )
  }

  test("for-if in parens") {
    val code =
      """|{
         |  val abstractTypeNames =
         |    for (parent <- parents; mbr <- parent.abstractTypeMembers if qualifies(mbr.symbol))
         |    yield mbr.name.asTypeName
         |
         |  for name <- abstractTypeNames do
         |    foo
         |}
         |""".stripMargin
    val output =
      """|{
         |  val abstractTypeNames = for (parent <- parents; mbr <- parent.abstractTypeMembers; if qualifies(mbr.symbol)) yield mbr.name.asTypeName
         |  for (name <- abstractTypeNames) foo
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Block(
        List(
          Defn.Val(
            Nil,
            List(Pat.Var(tname("abstractTypeNames"))),
            None,
            Term.ForYield(
              List(
                Enumerator.Generator(Pat.Var(tname("parent")), tname("parents")),
                Enumerator.Generator(
                  Pat.Var(tname("mbr")),
                  Term.Select(tname("parent"), tname("abstractTypeMembers"))
                ),
                Enumerator.Guard(
                  Term.Apply(
                    tname("qualifies"),
                    List(Term.Select(tname("mbr"), tname("symbol")))
                  )
                )
              ),
              Term.Select(Term.Select(tname("mbr"), tname("name")), tname("asTypeName"))
            )
          ),
          Term.For(
            List(Enumerator.Generator(Pat.Var(tname("name")), tname("abstractTypeNames"))),
            tname("foo")
          )
        )
      )
    )
  }

  test("several nested if, with () as body") {
    val code =
      """
          if !other
          then
            if (member)
              ()
            else
              overrideError()
          else
            checkOverrideDeprecated()
    """.stripMargin
    val output =
      """|if (!other) if (member) () else overrideError() else checkOverrideDeprecated()
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(
        Term.ApplyUnary(tname("!"), tname("other")),
        Term.If(tname("member"), Lit.Unit(), Term.Apply(tname("overrideError"), Nil), Nil),
        Term.Apply(tname("checkOverrideDeprecated"), Nil),
        Nil
      )
    )
  }

  test("#3136: block catch handler, indented") {
    val code =
      """|try ???
         |catch
         |  val a = 10
         |  handler(a)
         |""".stripMargin
    val msg =
      """|<input>:3: error: illegal start of simple expression
         |  val a = 10
         |  ^""".stripMargin
    runTestError[Stat](code, msg)
  }

  test("match with dedented single case") {
    val code =
      """|val a = this match
         |      case a =>
         |         that match
         |        case b => bb
         |         end match
         |      case b =>
         |         that match
         |        case c => cc
         |""".stripMargin
    val output =
      """|val a = this match {
         |  case a =>
         |    that match {
         |      case b =>
         |        bb
         |        end match
         |    }
         |  case b =>
         |    that match {
         |      case c => cc
         |    }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("a"))),
        None,
        Term.Match(
          Term.This(anon),
          List(
            Case(
              Pat.Var(tname("a")),
              None,
              Term.Match(
                tname("that"),
                List(
                  Case(
                    Pat.Var(tname("b")),
                    None,
                    Term.Block(List(tname("bb"), Term.EndMarker(tname("match"))))
                  )
                ),
                Nil
              )
            ),
            Case(
              Pat.Var(tname("b")),
              None,
              Term.Match(tname("that"), List(Case(Pat.Var(tname("c")), None, tname("cc"))), Nil)
            )
          ),
          Nil
        )
      )
    )
  }

  test("single-line try within catch case, then another case") {
    val code =
      """|try foo
         |catch
         |case _ =>
         |    try bar finally qux
         |    try bar catch baz finally qux
         |    try bar catch case _ => baz finally qux
         |case xyz =>
         |""".stripMargin
    val output =
      """|try foo catch {
         |  case _ =>
         |    try bar finally qux
         |    try bar catch baz finally qux
         |    try bar catch {
         |      case _ => baz
         |    } finally qux
         |  case xyz =>
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        tname("foo"),
        List(
          Case(
            Pat.Wildcard(),
            None,
            Term.Block(
              List(
                Term.Try(tname("bar"), Nil, Some(tname("qux"))),
                Term.TryWithHandler(tname("bar"), tname("baz"), Some(tname("qux"))),
                Term.Try(
                  tname("bar"),
                  List(Case(Pat.Wildcard(), None, tname("baz"))),
                  Some(tname("qux"))
                )
              )
            )
          ),
          Case(Pat.Var(tname("xyz")), None, Term.Block(Nil))
        ),
        None
      )
    )
  }

  test("single-line try within catch case, then another case") {
    val code =
      """|try foo
         |catch
         |case _ =>
         |    try bar finally qux
         |    try bar catch baz finally qux
         |    try bar catch case _ => baz finally qux
         |finally xyz
         |""".stripMargin
    val output =
      """|try foo catch {
         |  case _ =>
         |    try bar finally qux
         |    try bar catch baz finally qux
         |    try bar catch {
         |      case _ => baz
         |    } finally qux
         |} finally xyz
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        tname("foo"),
        Case(
          Pat.Wildcard(),
          None,
          Term.Block(
            List(
              Term.Try(tname("bar"), Nil, Some(tname("qux"))),
              Term.TryWithHandler(tname("bar"), tname("baz"), Some(tname("qux"))),
              Term.Try(
                tname("bar"),
                List(Case(Pat.Wildcard(), None, tname("baz"))),
                Some(tname("qux"))
              )
            )
          )
        ) :: Nil,
        Some(tname("xyz"))
      )
    )
  }

  test("#3220") {
    val code =
      """|for {
         |  case (a, b) <- pairs
         |  x <- a to b
         |} yield x
         |""".stripMargin
    val layout = "for ( case (a, b) <- pairs; x <- a to b) yield x"
    runTestAssert[Stat](code, Some(layout))(
      Term.ForYield(
        List(
          Enumerator.CaseGenerator(
            Pat.Tuple(List(Pat.Var(tname("a")), Pat.Var(tname("b")))),
            tname("pairs")
          ),
          Enumerator.Generator(
            Pat.Var(tname("x")),
            Term.ApplyInfix(tname("a"), tname("to"), Nil, List(tname("b")))
          )
        ),
        tname("x")
      )
    )
  }

  test("#3224") {
    val code =
      """|for {
         |  x2 <- x1
         |} yield x2
         |  .x3 {
         |    case x4
         |        if x5.x6
         |          .x7(x8) =>
         |        x9
         |  }
         |""".stripMargin
    val layout =
      """|for (x2 <- x1) yield x2.x3 {
         |  case x4 if x5.x6.x7(x8) => x9
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(
      Term.ForYield(
        List(Enumerator.Generator(Pat.Var(tname("x2")), tname("x1"))),
        Term.Apply(
          Term.Select(tname("x2"), tname("x3")),
          Term.PartialFunction(
            Case(
              Pat.Var(tname("x4")),
              Some(
                Term.Apply(
                  Term.Select(Term.Select(tname("x5"), tname("x6")), tname("x7")),
                  List(tname("x8"))
                )
              ),
              tname("x9")
            ) :: Nil
          ) :: Nil
        )
      )
    )
  }

  test("while cond uses match") {
    val code1 =
      """|if sArr(last) == ')' then
         |  while (sArr(last): @switch).match
         |    case _ => false
         |  do last -= 1
         |""".stripMargin
    val code2 =
      """|if sArr(last) == ')' then
         |  while (sArr(last): @switch) match
         |    case _ => false
         |  do last -= 1
         |""".stripMargin
    val layout =
      """|if (sArr(last) == ')') while ((sArr(last): @switch) match {
         |  case _ => false
         |}) last -= 1
         |""".stripMargin
    assertNoDiff(parseStat(code1, dialect).reprint, layout)
    assertNoDiff(parseStat(code2, dialect).reprint, layout)
  }

}
