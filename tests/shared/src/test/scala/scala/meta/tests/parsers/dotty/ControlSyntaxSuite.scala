package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.internal.tokenizers.ScalametaTokenizer

class ControlSyntaxSuite extends BaseDottySuite {
  implicit val parseStat: String => Stat = code => templStat(code)(dialects.Dotty)
  implicit val parseSource: String => Source = code => source(code)(dialects.Dotty)

  // --------------------------
  // IF
  // --------------------------

  test("old-if-single1".only) {
    val code = "if (cond) -a else a"
    runTestAssert[Stat](code)(
      Term.If(Term.Name("cond"), Term.ApplyUnary(Term.Name("-"), Term.Name("a")), Term.Name("a"))
    )
  }

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
    val output = """|if (cond) fx else gx""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(Term.Name("cond"), Term.Name("fx"), Term.Name("gx"))
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
    val output = "if (cond) fx else gx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(Term.Name("cond"), Term.Name("fx"), Term.Name("gx"))
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
        Term.Name("cond"),
        Term.Name("fx"),
        Term.Name("gx")
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
        Term.ApplyInfix(Term.Name("cond1"), Term.Name("&&"), Nil, List(Term.Name("cond2"))),
        Term.Name("gx"),
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
          Term.Name("cond1"),
          Term.Name("||"),
          Nil,
          List(Term.Apply(Term.Name("cond2"), List(Term.Name("a1"))))
        ),
        Term.Name("ok"),
        Lit.Unit()
      )
    )
  }

  test("new-if-single3") {
    val code = "if (cond1).cont then ok"
    val output = "if (cond1.cont) ok"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(Term.Select(Term.Name("cond1"), Term.Name("cont")), Term.Name("ok"), Lit.Unit())
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
        Term.Name("cond"),
        Term.Block(List(Term.Name("fx1"), Term.Name("fx2"))),
        Term.Block(List(Term.Name("gx1"), Term.Name("gx2")))
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
        Term.Name("fx"),
        List(
          Term.If(Term.Name("cond"), Term.Name("A"), Term.Name("B"))
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
        Term.Name("fx"),
        List(
          Term.If(
            Term.Name("cond"),
            Term.Block(List(Term.Name("A1"), Term.Name("A2"))),
            Term.Block(List(Term.Name("B1"), Term.Name("B2")))
          )
        )
      )
    )
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
        Term.Name("fx"),
        List(
          Term.If(
            Term.Name("cond"),
            Term.Block(List(Term.Name("A1"), Term.Name("A2"))),
            Term.Block(List(Term.Name("B1"), Term.Name("B2")))
          ),
          Term.Name("secondArg")
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
      Term.Try(Term.Block(List(Term.Name("fx"))), Nil, Some(Term.Block(List(Term.Name("ok")))))
    )
  }

  test("old-try-finally2") {
    val code = """|try fx
                  |finally ok
                  |""".stripMargin
    val output = "try fx finally ok"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(Term.Name("fx"), Nil, Some(Term.Name("ok")))
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
        Term.Name("fx"),
        Nil,
        Some(Term.Name("ok"))
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
        Term.Block(List(Term.Name("fx"), Term.Name("fy"))),
        Nil,
        Some(Term.Block(List(Term.Name("ok1"), Term.Name("ok2"))))
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
      Term.Try(Term.Name("fx"), List(Case(Pat.Var(Term.Name("x")), None, Lit.Int(1))), None)
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
        Term.Name("fx"),
        List(
          Case(Pat.Var(Term.Name("x")), None, Lit.Int(1)),
          Case(Pat.Var(Term.Name("y")), None, Lit.Int(2))
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
        Term.Block(List(Term.Name("fx"), Term.Name("fy"))),
        List(Case(Pat.Var(Term.Name("x")), None, Term.Name("ct"))),
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
      Term.Try(Term.Name("fx"), List(Case(Pat.Var(Term.Name("x")), None, Term.Name("ct"))), None)
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
        Term.Name("fx"),
        List(
          Case(Pat.Var(Term.Name("x")), None, Term.Block(List(Term.Name("ax"), Term.Name("bx"))))
        ),
        Some(Term.Name("fx"))
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
            Term.Name("fx"),
            List(
              Case(
                Pat.Var(Term.Name("x")),
                None,
                Term.Try(
                  Term.Name("fy"),
                  List(Case(Pat.Var(Term.Name("y")), None, Term.Throw(Term.Name("ex")))),
                  None
                )
              )
            ),
            Some(Term.Name("fxclose"))
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
            Pat.Var(Term.Name("i")),
            Term.ApplyInfix(Lit.Int(1), Term.Name("to"), Nil, List(Lit.Int(3)))
          )
        ),
        Term.Name("work")
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
            Pat.Var(Term.Name("i")),
            Term.ApplyInfix(Lit.Int(1), Term.Name("to"), Nil, List(Lit.Int(10)))
          ),
          Enumerator.Guard(Term.ApplyInfix(Term.Name("i"), Term.Name("<"), Nil, List(Lit.Int(4))))
        ),
        Term.Name("work")
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
          Enumerator.Generator(Pat.Var(Term.Name("i")), Term.Name("gen")),
          Enumerator.Guard(Term.ApplyInfix(Term.Name("i"), Term.Name("<"), Nil, List(Lit.Int(4))))
        ),
        Term.Name("work")
      )
    )
  }

  test("old-for-yield-single1") {
    val code = "for (i <- 1 to 3) yield i"
    runTestAssert[Stat](code)(
      Term.ForYield(
        List(
          Enumerator.Generator(
            Pat.Var(Term.Name("i")),
            Term.ApplyInfix(Lit.Int(1), Term.Name("to"), Nil, List(Lit.Int(3)))
          )
        ),
        Term.Name("i")
      )
    )
  }

  test("old-for-yield-single2") {
    val code = "for { i <- gen } yield i"
    val output = "for (i <- gen) yield i"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(Enumerator.Generator(Pat.Var(Term.Name("i")), Term.Name("gen"))),
        Term.Name("i")
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
        List(Enumerator.Generator(Pat.Var(Term.Name("i")), Term.Name("gen"))),
        Term.Block(List(Term.Name("a"), Term.Name("b")))
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
          Enumerator.Generator(Pat.Var(Term.Name("i")), Term.Name("gen")),
          Enumerator.Guard(Term.ApplyInfix(Term.Name("i"), Term.Name("<"), Nil, List(Lit.Int(4))))
        ),
        Term.Block(List(Term.Name("aa"), Term.Name("bb")))
      )
    )
  }

  test("new-fordo-single1") {
    val code = "for a <- gen do fx"
    val output = "for (a <- gen) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("gen"))),
        Term.Name("fx")
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
        List(Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("gen"))),
        Term.Name("fx")
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
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("gen")),
          Enumerator.Guard(Term.Name("cnd"))
        ),
        Term.Name("fx")
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
        List(Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("gen"))),
        Term.Name("fx")
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
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("x")),
          Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("y"))
        ),
        Term.Name("fx")
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
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("x")),
          Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("y"))
        ),
        Term.Block(List(Term.Name("fx"), Term.Name("fy")))
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
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("x")),
          Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("y"))
        ),
        Term.Name("fx")
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
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("gen")),
          Enumerator.Guard(Term.Name("cnd"))
        ),
        Term.Name("fx")
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
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("gen")),
          Enumerator.Guard(Term.Name("cnd"))
        ),
        Term.Name("fx")
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
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("x")),
          Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("y"))
        ),
        Term.Block(List(Term.Name("fx"), Term.Name("fy")))
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
            .CaseGenerator(Pat.Typed(Pat.Var(Term.Name("a")), Type.Name("TP")), Term.Name("iter"))
        ),
        Term.Name("echo")
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
            .CaseGenerator(Pat.Typed(Pat.Var(Term.Name("a")), Type.Name("TP")), Term.Name("iter")),
          Enumerator.Guard(Term.Name("cnd"))
        ),
        Term.Name("echo")
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
          Enumerator.Generator(Pat.Var(Term.Name("x")), Term.Name("gen")),
          Enumerator.CaseGenerator(
            Pat.Typed(Pat.Var(Term.Name("a1")), Type.Name("TP")),
            Term.Name("iter1")
          ),
          Enumerator.Guard(Term.Name("cnd")),
          Enumerator
            .CaseGenerator(Pat.Typed(Pat.Var(Term.Name("a2")), Type.Name("TP")), Term.Name("iter2"))
        ),
        Term.Name("fn")
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
          Enumerator.Generator(Pat.Var(Term.Name("i")), Term.Name("gen")),
          Enumerator.Val(Pat.Var(Term.Name("x")), Lit.Int(3)),
          Enumerator.Guard(
            Term.ApplyInfix(Term.Name("cnd1"), Term.Name("&&"), Nil, List(Term.Name("cnd2")))
          )
        ),
        Term.Name("work")
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
            Pat.Tuple(List(Pat.Var(Term.Name("a")), Pat.Var(Term.Name("b")))),
            Term.Name("gen")
          ),
          Enumerator.Guard(Term.ApplyInfix(Term.Name("a"), Term.Name("<"), Nil, List(Lit.Int(5)))),
          Enumerator.Generator(Pat.Var(Term.Name("c")), Term.Name("otherGen"))
        ),
        Term.Name("c")
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
            Pat.Tuple(List(Pat.Var(Term.Name("arg")), Pat.Var(Term.Name("param")))),
            Term.Apply(Term.Select(Term.Name("args"), Term.Name("zip")), List(Term.Name("vparams")))
          )
        ),
        Term.Name("arg")
      )
    )
  }

  // --------------------------
  // WHILE
  // --------------------------

  test("old-while-single") {
    val code = "while (cond) fx"
    runTestAssert[Stat](code)(
      Term.While(Term.Name("cond"), Term.Name("fx"))
    )
  }

  test("old-while-multi") {
    val code = """|while (cond) {
                  |  fx
                  |  fy
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code)(
      Term.While(Term.Name("cond"), Term.Block(List(Term.Name("fx"), Term.Name("fy"))))
    )
  }

  test("new-while-single1") {
    val code = """|while cond do fx
                  |""".stripMargin
    val output = "while (cond) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(Term.Name("cond"), Term.Name("fx"))
    )
  }

  test("new-while-single2") {
    val code = """|while cond
                  |do fx
                  |""".stripMargin
    val output = "while (cond) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(Term.Name("cond"), Term.Name("fx"))
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
        Term.ApplyInfix(Term.Name("fx"), Term.Name("+"), Nil, List(Term.Name("fy"))),
        Term.Block(List(Term.Name("fx"), Term.Name("fy")))
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
        Term.Block(List(Term.Name("s1"), Term.Name("s2"))),
        Term.Block(List(Term.Name("fx"), Term.Name("fy")))
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
        Term.Name("read"),
        Nil,
        List(List()),
        Some(Type.Name("String")),
        Term.Block(
          List(Term.While(Term.Name("cond"), Term.Block(Nil)), Term.Apply(Term.Name("other"), Nil))
        )
      )
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
        Type.Name("A"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Term.Name("slf"), None),
          List(
            Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.Int(3))
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
        Term.Name("x"),
        List(Case(Lit.Int(1), None, Term.Block(Nil)), Case(Lit.Int(2), None, Term.Block(Nil)))
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
        Term.Name("x"),
        List(Case(Lit.Int(1), None, Lit.String("1")), Case(Lit.Int(2), None, Lit.String("2")))
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
        Term.Name("x"),
        List(
          Case(Lit.Int(1), None, Term.Block(List(Term.Name("a1"), Term.Name("b1")))),
          Case(Lit.Int(2), None, Term.Block(List(Term.Name("a2"), Term.Name("b2"))))
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
        Term.Name("x"),
        List(
          Case(
            Lit.Int(1),
            None,
            Term.Match(
              Term.Name("y"),
              List(Case(Lit.Int(5), None, Lit.String("5")), Case(Lit.Int(6), None, Lit.String("6")))
            )
          ),
          Case(Lit.Int(2), None, Lit.String("2"))
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
        Term.Name("x"),
        List(Case(Lit.Int(1), None, Term.Block(Nil)), Case(Lit.Int(2), None, Term.Block(Nil)))
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
        Term.Name("x"),
        List(Case(Lit.Int(1), None, Lit.String("1")), Case(Lit.Int(2), None, Lit.String("2")))
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
        Term.Name("x"),
        List(
          Case(Lit.Int(1), None, Term.Block(List(Term.Name("a1"), Term.Name("b1")))),
          Case(Lit.Int(2), None, Term.Block(List(Term.Name("a2"), Term.Name("b2"))))
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
        Term.Name("cond"),
        List(
          Case(Pat.Var(Term.Name("a")), None, Term.Name("fa")),
          Case(Pat.Var(Term.Name("b")), None, Term.Name("fb"))
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
        Term.Name("fx"),
        Nil,
        Nil,
        Some(Type.Name("String")),
        Term.Block(
          List(
            Term.Match(
              Term.Name("x"),
              List(
                Case(Lit.Int(1), None, Lit.String("OK")),
                Case(Lit.Int(2), None, Lit.String("ERROR"))
              )
            ),
            Defn.Val(Nil, List(Pat.Var(Term.Name("c"))), None, Lit.String("123")),
            Term.Name("c")
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
        Term.Name("fx"),
        Nil,
        Nil,
        Some(Type.Name("String")),
        Term.Block(
          List(
            Term.Match(Term.Name("x"), List(Case(Lit.Int(2), None, Lit.String("ERROR")))),
            Term.EndMarker(Term.Name("match"))
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
        Term.Match(Term.Name("func"), List(Case(Term.Name("A"), None, Term.Name("Accept")))),
        List(Case(Pat.Var(Term.Name("ex")), None, Term.Name("Error"))),
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
        Term.Name("x"),
        List(
          Case(
            Pat.Var(Term.Name("x")),
            None,
            Term.Block(List(Term.Apply(Term.Name("a"), Nil), Term.Apply(Term.Name("b"), Nil)))
          )
        )
      )
    )
  }

  test("match-chained") {
    val expected = Term.Match(
      Term.Match(
        Term.Name("xs"),
        List(
          Case(Term.Name("Nil"), None, Lit.String("empty")),
          Case(
            Pat.ExtractInfix(
              Pat.Var(Term.Name("x")),
              Term.Name("::"),
              List(Pat.Var(Term.Name("xs1")))
            ),
            None,
            Lit.String("nonempty")
          )
        )
      ),
      List(
        Case(Lit.String("empty"), None, Lit.Int(0)),
        Case(Lit.String("nonempty"), None, Lit.Int(1))
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
      assertLayout = None
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
      assertLayout = None
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("hello"))),
        None,
        Term.Match(
          Term.ApplyInfix(
            Term.Match(
              Term.Name("xs"),
              List(
                Case(Term.Name("Nil"), None, Lit.String("empty")),
                Case(
                  Pat.ExtractInfix(
                    Pat.Var(Term.Name("x")),
                    Term.Name("::"),
                    List(Pat.Var(Term.Name("xs1")))
                  ),
                  None,
                  Lit.String("nonempty")
                )
              )
            ),
            Term.Name("startsWith"),
            Nil,
            List(Lit.String("empty"))
          ),
          List(
            Case(Lit.Boolean(true), None, Lit.Int(0)),
            Case(Lit.Boolean(false), None, Lit.Int(1))
          )
        )
      )
    )

  }

  test("match-chained-complex-operator") {
    runTestAssert[Stat](
      """|val hello = xs match
         |  case Nil => 0
         |  case x :: xs1 => 1
         |+ 1 match
         |  case 1 => true
         |  case 2 => false
         |
         |""".stripMargin,
      assertLayout = None
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("hello"))),
        None,
        Term.Match(
          Term.Name("xs"),
          List(
            Case(Term.Name("Nil"), None, Lit.Int(0)),
            Case(
              Pat.ExtractInfix(
                Pat.Var(Term.Name("x")),
                Term.Name("::"),
                List(Pat.Var(Term.Name("xs1")))
              ),
              None,
              Term.Match(
                Term.ApplyInfix(Lit.Int(1), Term.Name("+"), Nil, List(Lit.Int(1))),
                List(
                  Case(Lit.Int(1), None, Lit.Boolean(true)),
                  Case(Lit.Int(2), None, Lit.Boolean(false))
                )
              )
            )
          )
        )
      )
    )

  }

  test("match-dot") {
    val expected = Term.If(
      Term.Match(
        Term.Name("xs"),
        List(
          Case(Term.Name("Nil"), None, Lit.Boolean(false)),
          Case(Pat.Wildcard(), None, Lit.Boolean(true))
        )
      ),
      Lit.String("nonempty"),
      Lit.String("empty")
    )
    runTestAssert[Stat](
      """|if xs.match {
         |  case Nil => false
         |  case _ => true
         |}
         |then "nonempty"
         |else "empty"
         |""".stripMargin,
      assertLayout = None
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
      Term.Name("mtch"),
      Nil,
      List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None))),
      Some(Type.Name("String")),
      Term.Apply(
        Term.Select(
          Term.Match(
            Term.Name("x"),
            List(
              Case(Lit.Int(1), None, Lit.String("1")),
              Case(Pat.Wildcard(), None, Lit.String("ERR"))
            )
          ),
          Term.Name("trim")
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
      assertLayout = None
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
        Term.Name("fx"),
        List(
          Term.Name("p1"),
          Term.Try(
            Term.Apply(Term.Name("func"), Nil),
            List(Case(Pat.Var(Term.Name("x")), None, Term.Apply(Term.Name("ok"), Nil))),
            None
          )
        )
      )
    )
  }
}
