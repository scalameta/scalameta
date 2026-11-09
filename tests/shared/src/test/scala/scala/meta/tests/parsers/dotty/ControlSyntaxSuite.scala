package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.internal.parsers.{LazyTokenIterator, ScannerTokens}
import scala.meta.tests.TestHelpers
import scala.meta.tokenizers.TokenizerOptions

import scala.annotation.nowarn

class ControlSyntaxSuite extends BaseDottySuite {

  // --------------------------
  // IF
  // --------------------------

  test("old-if-single1") {
    val code = "if (cond) -a else a"
    runTestAssert[Stat](code)(
      Term.If(tname("cond"), Term.ApplyUnary(tname("-"), tname("a")), tname("a")),
    )
  }

  test("old-if-else-single1") {
    val code =
      """|if (cond) fx
         |else gx
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("if (cond) fx else gx"))(
      Term.If(tname("cond"), tname("fx"), tname("gx")),
    )
  }

  test("old-if-else-single2") {
    val code =
      """|if (cond)
         |  fx
         |else
         |  gx
         |""".stripMargin
    val output = """|if (cond) fx else gx""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(tname("cond"), tname("fx"), tname("gx")),
    )
  }

  test("old-if-else-braces") {
    val code =
      """|if (cond) {
         |  fa1
         |  fa2
         |} else {
         |  fb1
         |  fb2
         |}
         |""".stripMargin
    runTestAssert[Stat](code)(
      Term.If(tname("cond"), blk(tname("fa1"), tname("fa2")), blk(tname("fb1"), tname("fb2"))),
    )
  }

  test("new-if-else-single1") {
    val code =
      """|if cond then fx
         |else gx
         |""".stripMargin
    val output = "if (cond) fx else gx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(tname("cond"), tname("fx"), tname("gx")),
    )
  }

  test("new-if-else-single2") {
    val code =
      """|if cond then
         |  fx
         |else 
         |  gx
         |""".stripMargin
    val output = "if (cond) fx else gx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(tname("cond"), tname("fx"), tname("gx")),
    )
  }

  test("new-if-single1") {
    val code =
      """|if cond1
         |   && (cond2)
         |then
         |  gx
         |""".stripMargin
    val output = "if (cond1 && cond2) gx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(tinfix(tname("cond1"), "&&", tname("cond2")), tname("gx"), Lit.Unit()),
    )
  }

  test("new-if-single2") {
    val code =
      """|if (cond1) || cond2(a1) then ok
         |""".stripMargin
    val output = "if (cond1 || cond2(a1)) ok"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term
        .If(tinfix(tname("cond1"), "||", tapply(tname("cond2"), tname("a1"))), tname("ok"), Lit.Unit()),
    )
  }

  test("new-if-single3") {
    val code = "if (cond1).cont then ok"
    val output = "if (cond1.cont) ok"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(tselect("cond1", "cont"), tname("ok"), Lit.Unit()),
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
    runTestAssert[Stat](code, Some(layout))(blk(
      Term.If(
        tinfix(tname("x"), ">", int(0)),
        tapply(tname("&&"), tinfix(tname("y"), ">", int(0))),
        Lit.Unit(),
        Nil,
      ),
      tinfix(tname("x"), "+=", int(1)),
    ))
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
      """|error: `}` expected but `integer constant` found
         |  if (x > 0) && y > 0
         |                    ^""".stripMargin,
    )
  }

  test("new-if-else-multiple") {
    val code =
      """|if cond then
         |  fx1
         |  fx2
         |else 
         |  gx1
         |  gx2
         |""".stripMargin
    val output =
      """|if (cond) {
         |  fx1
         |  fx2
         |} else {
         |  gx1
         |  gx2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(tname("cond"), blk(tname("fx1"), tname("fx2")), blk(tname("gx1"), tname("gx2"))),
    )
  }

  test("if-else-in-parens-1") {
    val code =
      """|fx(
         |  if (cond)
         |    A
         |  else
         |    B)
         |""".stripMargin
    val output = "fx(if (cond) A else B)"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      tapply(tname("fx"), Term.If(tname("cond"), tname("A"), tname("B"))),
    )
  }

  test("if-else-in-parens-2") {
    val code =
      """|fx(
         |  if cond then
         |    A1
         |    A2
         |  else
         |    B1
         |    B2)
         |""".stripMargin
    val output =
      """|fx(if (cond) {
         |  A1
         |  A2
         |} else {
         |  B1
         |  B2
         |})
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(tapply(
      tname("fx"),
      Term.If(tname("cond"), blk(tname("A1"), tname("A2")), blk(tname("B1"), tname("B2"))),
    ))
  }

  test("new-if-indented") {
    val code =
      """|if (cond)
         |  fx1
         |  fx2
         |""".stripMargin
    val output =
      """|if (cond) {
         |  fx1
         |  fx2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(tname("cond"), blk(tname("fx1"), tname("fx2")), Lit.Unit()),
    )
  }

  test("new-if-else-indented") {
    val code =
      """|if cond
         |  fx1
         |  fx2
         |else
         |  gx
         |""".stripMargin
    runTestError[Stat](code, "`then` expected but `else` found")
  }

  test("if-else-in-parens-3") {
    val code =
      """|fx(
         |  if cond then
         |    A1
         |    A2
         |  else
         |    B1
         |    B2,
         |  secondArg
         |)
         |""".stripMargin
    val output =
      """|fx(if (cond) {
         |  A1
         |  A2
         |} else {
         |  B1
         |  B2
         |}, secondArg)
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(tapply(
      tname("fx"),
      Term.If(tname("cond"), blk(tname("A1"), tname("A2")), blk(tname("B1"), tname("B2"))),
      tname("secondArg"),
    ))
  }

  // --------------------------
  // TRY
  // --------------------------

  test("old-try-finally1") {
    val code =
      """|try { fx }
         |finally { ok }
         |""".stripMargin
    val output =
      """|try {
         |  fx
         |} finally {
         |  ok
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(blk(tname("fx")), Nil, Some(blk(tname("ok")))),
    )
  }

  test("old-try-finally2") {
    val code =
      """|try fx
         |finally ok
         |""".stripMargin
    val output = "try fx finally ok"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(tname("fx"), Nil, Some(tname("ok"))),
    )
  }

  test("new-try-finally-single") {
    val code =
      """|try
         |  fx
         |finally
         |  ok
         |""".stripMargin
    val output = "try fx finally ok"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(tname("fx"), Nil, Some(tname("ok"))),
    )
  }

  test("new-try-finally-multiple") {
    val code =
      """|try
         |  fx
         |  fy
         |finally
         |  ok1
         |  ok2
         |""".stripMargin
    val output =
      """|try {
         |  fx
         |  fy
         |} finally {
         |  ok1
         |  ok2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(blk(tname("fx"), tname("fy")), Nil, Some(blk(tname("ok1"), tname("ok2")))),
    )
  }

  test("old-try-catch-single") {
    val code =
      """|try fx 
         |catch { case x => 1 }
         |""".stripMargin
    val output =
      """|try fx catch {
         |  case x => 1
         |}""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(tname("fx"), List(Case(patvar("x"), None, int(1))), None),
    )
  }

  test("old-try-catch-multi") {
    val code =
      """|try fx 
         |catch {
         |  case x => 1
         |  case y => 2
         |}
         |""".stripMargin
    val output =
      """|try fx catch {
         |  case x => 1
         |  case y => 2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.Try(
      tname("fx"),
      List(Case(patvar("x"), None, int(1)), Case(patvar("y"), None, int(2))),
      None,
    ))
  }

  test("new-try-catch-multi") {
    val code =
      """|try
         |  fx 
         |  fy
         |catch { case x => ct }
         |""".stripMargin
    val output =
      """|try {
         |  fx
         |  fy
         |} catch {
         |  case x => ct
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(blk(tname("fx"), tname("fy")), List(Case(patvar("x"), None, tname("ct"))), None),
    )
  }

  test("new-catch-single1") {
    val code =
      """|try fx
         |catch case x => ct
         |""".stripMargin
    val output =
      """|try fx catch {
         |  case x => ct
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(tname("fx"), List(Case(patvar("x"), None, tname("ct"))), None),
    )
  }

  test("new-catch-single2") {
    val code =
      """|try fx
         |catch
         |  case x =>
         |    fa
         |    fb
         |""".stripMargin
    val output =
      """|try fx catch {
         |  case x =>
         |    fa
         |    fb
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(tname("fx"), List(Case(patvar("x"), None, blk(tname("fa"), tname("fb")))), None),
    )
  }

  test("new-catch-multi") {
    val code =
      """|try fx
         |catch
         |  case x =>
         |    xa
         |    xb
         |  case y => yab
         |  case z =>
         |    za
         |    zb
         |""".stripMargin
    val output =
      """|try fx catch {
         |  case x =>
         |    xa
         |    xb
         |  case y =>
         |    yab
         |  case z =>
         |    za
         |    zb
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.Try(
      tname("fx"),
      List(
        Case(patvar("x"), None, blk(tname("xa"), tname("xb"))),
        Case(patvar("y"), None, tname("yab")),
        Case(patvar("z"), None, blk(tname("za"), tname("zb"))),
      ),
      None,
    ))
  }

  test("new-catch-handler-nl") {
    val code =
      """|try
         |  foo
         |catch
         |  bar
         |""".stripMargin
    val output = "try foo catch bar"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.TryWithHandler(tname("foo"), tname("bar"), None),
    )
  }

  test("new-catch-handler-finally-nl") {
    val code =
      """|try
         |  foo
         |catch
         |  bar
         |finally
         |  baz
         |""".stripMargin
    val output = "try foo catch bar finally baz"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.TryWithHandler(tname("foo"), tname("bar"), Some(tname("baz"))),
    )
  }

  test("new-catch-finally-single") {
    val code =
      """|try fx
         |catch case x =>
         |  ax
         |  bx
         |finally
         |  fx
         |""".stripMargin
    val output =
      """|try fx catch {
         |  case x =>
         |    ax
         |    bx
         |} finally fx
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.Try(
      tname("fx"),
      List(Case(patvar("x"), None, blk(tname("ax"), tname("bx")))),
      Some(tname("fx")),
    ))
  }

  test("new-catch-inside-catch") {
    val code =
      """|{
         |  try fx
         |  catch case x =>
         |    try fy
         |    catch case y =>
         |    throw ex
         |  finally fxclose
         |}
         |""".stripMargin
    val output =
      """|{
         |  try fx catch {
         |    case x =>
         |      try fy catch {
         |        case y =>
         |          throw ex
         |      }
         |  } finally fxclose
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(blk(Term.Try(
      tname("fx"),
      List(Case(
        patvar("x"),
        None,
        Term.Try(tname("fy"), List(Case(patvar("y"), None, Term.Throw(tname("ex")))), None),
      )),
      Some(tname("fxclose")),
    )))
  }

  test("catch after `end for` and `end match`") {
    val code =
      """|object a:
         |   try
         |      for (i <- foo)
         |        foo
         |      end for
         |      foo match
         |         case foo =>
         |      end match
         |   catch
         |      case f =>
         |""".stripMargin
    val layout =
      """|object a {
         |  try {
         |    for (i <- foo) foo
         |    end for
         |    foo match {
         |      case foo =>
         |    }
         |    end match
         |  } catch {
         |    case f =>
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Term.Try(
        blk(
          Term.For(List(Enumerator.Generator(patvar("i"), tname("foo"))), tname("foo")),
          Term.EndMarker(tname("for")),
          tmatch(tname("foo"), Case(patvar("foo"), None, blk())),
          Term.EndMarker(tname("match")),
        ),
        List(Case(patvar("f"), None, blk())),
        None,
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3532 nested try-finally on same line") {
    val code =
      """|try try Right(f()) finally iter.close()
         |finally arena.close()
         |""".stripMargin
    val layout = "try try Right(f()) finally iter.close() finally arena.close()"
    val tree = Term.Try(
      Term.Try(tapply(tname("Right"), tapply(tname("f"))), Nil, Some(tapply(tselect("iter", "close")))),
      Nil,
      Some(tapply(tselect("arena", "close"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3532 nested try-finally without catch") {
    val code =
      """|try
         |  try Right(f())
         |  finally iter.close()
         |finally arena.close()
         |""".stripMargin
    val layout = "try try Right(f()) finally iter.close() finally arena.close()"
    val tree = Term.Try(
      Term.Try(tapply(tname("Right"), tapply(tname("f"))), Nil, Some(tapply(tselect("iter", "close")))),
      Nil,
      Some(tapply(tselect("arena", "close"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3532 nested try-finally, oneline catch") {
    val code =
      """|try
         |  try Right(f())
         |  catch case NonFatal(ex) => Left(???)
         |  finally iter.close()
         |finally arena.close()
         |""".stripMargin
    val layout =
      """|try try Right(f()) catch {
         |  case NonFatal(ex) =>
         |    Left(???)
         |} finally iter.close() finally arena.close()
         |""".stripMargin
    val tree = Term.Try(
      Term.Try(
        tapply(tname("Right"), tapply(tname("f"))),
        Case(
          Pat.Extract(tname("NonFatal"), List(patvar("ex"))),
          None,
          tapply(tname("Left"), tname("???")),
        ) :: Nil,
        Some(tapply(tselect("iter", "close"))),
      ),
      Nil,
      Some(tapply(tselect("arena", "close"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3532 nested try-finally, catch with non-indented case") {
    val code =
      """|try
         |  try Right(f())
         |  catch
         |  case NonFatal(ex) => Left(???)
         |  finally iter.close()
         |finally arena.close()
         |""".stripMargin
    val layout =
      """|try try Right(f()) catch {
         |  case NonFatal(ex) =>
         |    Left(???)
         |} finally iter.close() finally arena.close()
         |""".stripMargin
    val tree = Term.Try(
      Term.Try(
        tapply(tname("Right"), tapply(tname("f"))),
        Case(
          Pat.Extract(tname("NonFatal"), List(patvar("ex"))),
          None,
          tapply(tname("Left"), tname("???")),
        ) :: Nil,
        Some(tapply(tselect("iter", "close"))),
      ),
      Nil,
      Some(tapply(tselect("arena", "close"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3532 nested try-finally, catch with indented case") {
    val code =
      """|try
         |  try Right(f())
         |  catch
         |    case NonFatal(ex) => Left(???)
         |  finally iter.close()
         |finally arena.close()
         |""".stripMargin
    val layout =
      """|try try Right(f()) catch {
         |  case NonFatal(ex) =>
         |    Left(???)
         |} finally iter.close() finally arena.close()
         |""".stripMargin
    val tree = Term.Try(
      Term.Try(
        tapply(tname("Right"), tapply(tname("f"))),
        Case(
          Pat.Extract(tname("NonFatal"), List(patvar("ex"))),
          None,
          tapply(tname("Left"), tname("???")),
        ) :: Nil,
        Some(tapply(tselect("iter", "close"))),
      ),
      Nil,
      Some(tapply(tselect("arena", "close"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3532 nested try, inner with catch, outer with finally") {
    val code =
      """|try
         |  try Right(f())
         |  catch
         |    case NonFatal(ex) => Left(???)
         |finally arena.close()
         |""".stripMargin
    val layout =
      """|try (
         |  try Right(f()) catch {
         |    case NonFatal(ex) =>
         |      Left(???)
         |  }
         |) finally arena.close()
         |""".stripMargin
    val tree = Term.Try(
      Term.Try(
        tapply(tname("Right"), tapply(tname("f"))),
        Case(
          Pat.Extract(tname("NonFatal"), List(patvar("ex"))),
          None,
          tapply(tname("Left"), tname("???")),
        ) :: Nil,
        None,
      ),
      Nil,
      Some(tapply(tselect("arena", "close"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3532 nested try-finally, try with multiline non-indented expr 1") {
    val code =
      """|try
         |  try { 1 + 2 }
         |    + 3
         |  finally iter.close()
         |finally arena.close()
         |""".stripMargin
    val layout =
      """|try try {
         |  1 + 2
         |} + 3 finally iter.close() finally arena.close()
         |""".stripMargin
    val tree = Term.Try(
      Term.Try(
        tinfix(blk(tinfix(int(1), "+", int(2))), "+", int(3)),
        Nil,
        Some(tapply(tselect("iter", "close"))),
      ),
      Nil,
      Some(tapply(tselect("arena", "close"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3532 nested try-finally, try with multiline non-indented expr 2") {
    val code =
      """|try
         |  try { 1 + 2 }
         |    .foo
         |  finally iter.close()
         |finally arena.close()
         |""".stripMargin
    val layout =
      """|try try {
         |  1 + 2
         |}.foo finally iter.close() finally arena.close()
         |""".stripMargin
    val tree = Term.Try(
      Term.Try(
        tselect(blk(tinfix(int(1), "+", int(2))), "foo"),
        Nil,
        Some(tapply(tselect("iter", "close"))),
      ),
      Nil,
      Some(tapply(tselect("arena", "close"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3532 nested try-finally, try with multiline non-indented expr 3") {
    val code =
      """|try
         |  try { 1 + 2 }
         |    (foo)
         |  finally iter.close()
         |finally arena.close()
         |""".stripMargin
    val error =
      """|<input>:4: error: `outdent` expected but `finally` found
         |  finally iter.close()
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("#3532 nested bare try, outer without catch") {
    val code =
      """|try
         |  try 1
         |    + 2
         |finally foo
         |""".stripMargin
    val layout =
      """|try (
         |  try 1 + 2
         |) finally foo
         |""".stripMargin
    val tree = Term.Try(Term.Try(tinfix(int(1), "+", int(2)), Nil, None), Nil, Some(tname("foo")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3532 nested bare try, outer with catch") {
    val code =
      """|try
         |  try 1
         |    + 2
         |catch
         |  case NonFatal(ex) => 3
         |finally foo
         |""".stripMargin
    val layout =
      """|try (
         |  try 1 + 2
         |) catch {
         |  case NonFatal(ex) => 3
         |} finally foo
         |""".stripMargin
    val tree = Term.Try(
      Term.Try(tinfix(int(1), "+", int(2)), Nil, None),
      List(Case(Pat.Extract(tname("NonFatal"), List(patvar("ex"))), None, int(3))),
      Some(tname("foo")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // --------------------------
  // FOR
  // --------------------------

  test("old-for-single1") {
    val code = "for (i <- 1 to 3) work"
    runTestAssert[Stat](code)(
      Term.For(List(Enumerator.Generator(patvar("i"), tinfix(int(1), "to", int(3)))), tname("work")),
    )
  }

  test("old-for-single2") {
    val code = "for (i <- 1 to 10 if i < 4) work"
    val output = "for (i <- 1 to 10; if i < 4) work"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.For(
      List(
        Enumerator.Generator(patvar("i"), tinfix(int(1), "to", int(10))),
        Enumerator.Guard(tinfix(tname("i"), "<", int(4))),
      ),
      tname("work"),
    ))
  }

  test("old-for-multi") {
    val code =
      """|for {
         |  i <- gen
         |  if i < 4
         |} work
         |""".stripMargin
    val output = "for (i <- gen; if i < 4) work"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.For(
      List(
        Enumerator.Generator(patvar("i"), tname("gen")),
        Enumerator.Guard(tinfix(tname("i"), "<", int(4))),
      ),
      tname("work"),
    ))
  }

  test("old-for-yield-single1") {
    val code = "for (i <- 1 to 3) yield i"
    runTestAssert[Stat](code)(
      Term.ForYield(List(Enumerator.Generator(patvar("i"), tinfix(int(1), "to", int(3)))), tname("i")),
    )
  }

  test("old-for-yield-single2") {
    val code = "for { i <- gen } yield i"
    val output = "for (i <- gen) yield i"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(List(Enumerator.Generator(patvar("i"), tname("gen"))), tname("i")),
    )
  }

  test("old-for-yield-multi1") {
    val code = "for (i <- gen) yield {a; b}"
    val output =
      """|for (i <- gen) yield {
         |  a
         |  b
         |}""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(List(Enumerator.Generator(patvar("i"), tname("gen"))), blk(tname("a"), tname("b"))),
    )
  }

  test("old-for-yield-multi2") {
    val code =
      """|for {
         |  i <- gen
         |  if i < 4
         |} yield { aa; bb }
         |""".stripMargin
    val output =
      """|for (i <- gen; if i < 4) yield {
         |  aa
         |  bb
         |}""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(
        Enumerator.Generator(patvar("i"), tname("gen")),
        Enumerator.Guard(tinfix(tname("i"), "<", int(4))),
      ),
      blk(tname("aa"), tname("bb")),
    ))
  }

  test("new-fordo-single1") {
    val code = "for a <- gen do fx"
    val output = "for (a <- gen) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(List(Enumerator.Generator(patvar("a"), tname("gen"))), tname("fx")),
    )
  }

  test("new-fordo-single2") {
    val code =
      """|for
         |  a <- gen
         |do fx
         |""".stripMargin
    val output = "for (a <- gen) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(List(Enumerator.Generator(patvar("a"), tname("gen"))), tname("fx")),
    )
  }

  test("new-fordo-single3") {
    val code =
      """|for a <- gen if cnd
         |do fx
         |""".stripMargin
    val output = "for (a <- gen; if cnd) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.For(
      List(Enumerator.Generator(patvar("a"), tname("gen")), Enumerator.Guard(tname("cnd"))),
      tname("fx"),
    ))
  }

  test("new-fordo-single4") {
    val code =
      """|for a <- gen
         |do
         |  fx
         |""".stripMargin
    val output = "for (a <- gen) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(List(Enumerator.Generator(patvar("a"), tname("gen"))), tname("fx")),
    )
  }

  test("new-fordo-multi1") {
    val code =
      """|for
         |  a <- x
         |  b <- y
         |do fx
         |""".stripMargin
    val output = "for (a <- x; b <- y) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.For(
      List(
        Enumerator.Generator(patvar("a"), tname("x")),
        Enumerator.Generator(patvar("b"), tname("y")),
      ),
      tname("fx"),
    ))
  }

  test("new-fordo-multi2") {
    val code =
      """|for
         |  a <- x
         |  b <- y
         |do
         |  fx
         |  fy
         |""".stripMargin
    val output =
      """|for (a <- x; b <- y) {
         |  fx
         |  fy
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.For(
      List(
        Enumerator.Generator(patvar("a"), tname("x")),
        Enumerator.Generator(patvar("b"), tname("y")),
      ),
      blk(tname("fx"), tname("fy")),
    ))
  }

  test("new-for-indented-without-do") {
    val code =
      """|for ( a <- x )
         |  fx
         |  fy
         |""".stripMargin
    val output =
      """|for (a <- x) {
         |  fx
         |  fy
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(List(Enumerator.Generator(patvar("a"), tname("x"))), blk(tname("fx"), tname("fy"))),
    )
  }

  test("new-for-indented-without-do2") {
    val code =
      """|for { a <- x }
         |  fx
         |  fy
         |""".stripMargin
    val output =
      """|for (a <- x) {
         |  fx
         |  fy
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(List(Enumerator.Generator(patvar("a"), tname("x"))), blk(tname("fx"), tname("fy"))),
    )
  }

  test("new-for-yield-single1") {
    val code =
      """|for
         |  a <- x
         |  b <- y
         |yield fx
         |""".stripMargin
    val output = "for (a <- x; b <- y) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(
        Enumerator.Generator(patvar("a"), tname("x")),
        Enumerator.Generator(patvar("b"), tname("y")),
      ),
      tname("fx"),
    ))
  }

  test("new-for-yield-single2") {
    val code =
      """|for a <- gen if cnd
         |yield fx
         |""".stripMargin
    val output = "for (a <- gen; if cnd) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(Enumerator.Generator(patvar("a"), tname("gen")), Enumerator.Guard(tname("cnd"))),
      tname("fx"),
    ))
  }

  test("new-for-yield-single3") {
    val code =
      """|for a <- gen if cnd
         |yield
         |  fx
         |""".stripMargin
    val output = "for (a <- gen; if cnd) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(Enumerator.Generator(patvar("a"), tname("gen")), Enumerator.Guard(tname("cnd"))),
      tname("fx"),
    ))
  }

  test("new-for-yield-multi") {
    val code =
      """|for
         |  a <- x
         |  b <- y
         |yield
         |  fx
         |  fy
         |""".stripMargin
    val output =
      """|for (a <- x; b <- y) yield {
         |  fx
         |  fy
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(
        Enumerator.Generator(patvar("a"), tname("x")),
        Enumerator.Generator(patvar("b"), tname("y")),
      ),
      blk(tname("fx"), tname("fy")),
    ))
  }

  test("new-for-case1") {
    val code =
      """|for case a: TP <- iter do
         |  echo
         |""".stripMargin
    val output =
      """|for ( case a: TP <- iter) echo
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.For(
      List(Enumerator.CaseGenerator(Pat.Typed(patvar("a"), pname("TP")), tname("iter"))),
      tname("echo"),
    ))
  }

  test("new-for-case2") {
    val code =
      """|for case a: TP <- iter if cnd do
         |  echo
         |""".stripMargin
    val output =
      """|for ( case a: TP <- iter; if cnd) echo
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.For(
      List(
        Enumerator.CaseGenerator(Pat.Typed(patvar("a"), pname("TP")), tname("iter")),
        Enumerator.Guard(tname("cnd")),
      ),
      tname("echo"),
    ))
  }

  test("new-for-case3") {
    val code =
      """|for
         |  x <- gen
         |  case a1: TP <- iter1
         |  if cnd
         |  case a2: TP <- iter2
         |do fn
         |""".stripMargin
    val output = "for (x <- gen; case a1: TP <- iter1; if cnd; case a2: TP <- iter2) fn"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.For(
      List(
        Enumerator.Generator(patvar("x"), tname("gen")),
        Enumerator.CaseGenerator(Pat.Typed(patvar("a1"), pname("TP")), tname("iter1")),
        Enumerator.Guard(tname("cnd")),
        Enumerator.CaseGenerator(Pat.Typed(patvar("a2"), pname("TP")), tname("iter2")),
      ),
      tname("fn"),
    ))
  }

  test("for-new") {
    val code =
      """|for i <- gen
         |    x = 3
         |    if (cnd1) && cnd2
         |yield work
         |""".stripMargin
    val output = "for (i <- gen; x = 3; if cnd1 && cnd2) yield work"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(
        Enumerator.Generator(patvar("i"), tname("gen")),
        Enumerator.Val(patvar("x"), int(3)),
        Enumerator.Guard(tinfix(tname("cnd1"), "&&", tname("cnd2"))),
      ),
      tname("work"),
    ))
  }

  test("for-case-placeholder") {
    val code = "for case x <- _ yield x"
    val output = "for ( case x <- _) yield x"
    val tree = Term.AnonymousFunction(
      Term.ForYield(List(Enumerator.CaseGenerator(patvar("x"), Term.Placeholder())), tname("x")),
    )
    runTestAssert[Stat](code, assertLayout = Some(output))(tree)
  }

  test("select-match-placeholder") {
    val code = "_.match { case s => s }"
    val output =
      """|_.match {
         |  case s => s
         |}
         |""".stripMargin
    val tree = Term
      .AnonymousFunction(tselectmatch(Term.Placeholder(), Case(patvar("s"), None, tname("s"))))
    runTestAssert[Stat](code, assertLayout = Some(output))(tree)
  }

  test("select-match-placeholder-select") {
    val code = "_.toString.match { case s => s }"
    val output =
      """|_.toString.match {
         |  case s => s
         |}
         |""".stripMargin
    val tree = Term.AnonymousFunction(
      tselectmatch(tselect(Term.Placeholder(), "toString"), Case(patvar("s"), None, tname("s"))),
    )
    runTestAssert[Stat](code, assertLayout = Some(output))(tree)
  }

  test("select-match-placeholder-guard") {
    val code = "y.match { case s if _ => s }"
    val output =
      """|y.match {
         |  case s if _ => s
         |}
         |""".stripMargin
    val tree = Term.AnonymousFunction(
      tselectmatch(tname("y"), Case(patvar("s"), Some(Term.Placeholder()), tname("s"))),
    )
    runTestAssert[Stat](code, assertLayout = Some(output))(tree)
  }

  test("sub-case: placeholder in a sub-match") {
    val code =
      """|d match {
         |  case m if _ match case c if _ => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Term.AnonymousFunction(tmatch(
      tname("d"),
      Case(
        patvar("m"),
        None,
        tsubmatch(Term.Placeholder(), Case(patvar("c"), Some(Term.Placeholder()), lit(1))),
      ),
      Case(patwildcard, None, lit(2)),
    ))
    runTestAssert[Stat](code, assertLayout = None)(tree)
  }

  test("multiline-for") {
    val code =
      """|for (a,b) <- gen
         |  if a < 5
         |  c <- otherGen
         |yield c
         |""".stripMargin
    val output = "for ((a, b) <- gen; if a < 5; c <- otherGen) yield c"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(
        Enumerator.Generator(Pat.Tuple(List(patvar("a"), patvar("b"))), tname("gen")),
        Enumerator.Guard(tinfix(tname("a"), "<", int(5))),
        Enumerator.Generator(patvar("c"), tname("otherGen")),
      ),
      tname("c"),
    ))
  }

  test("oneline-for") {
    val code =
      """|for (arg, param) <- args.zip(vparams) yield
         |  arg
         |""".stripMargin
    val output =
      """|for ((arg, param) <- args.zip(vparams)) yield arg
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(Enumerator.Generator(
        Pat.Tuple(List(patvar("arg"), patvar("param"))),
        tapply(tselect("args", "zip"), tname("vparams")),
      )),
      tname("arg"),
    ))
  }

  test("#3713 cond in parens within enums") {
    val code =
      """|for (m <- decls
         |    if oneCond
         |      && (cond)
         |      && satisfiable) {}
         |
         |""".stripMargin
    val layout = "for (m <- decls; if oneCond && cond && satisfiable) {}"
    val tree = Term.For(
      List(
        Enumerator.Generator(patvar("m"), tname("decls")),
        Enumerator
          .Guard(tinfix(tinfix(tname("oneCond"), "&&", tname("cond")), "&&", tname("satisfiable"))),
      ),
      blk(),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("enum-val-first-ok-1") {
    val code =
      """|for {
         |  a = 1
         |  b <- Some(2)
         |} yield a + b
         |""".stripMargin
    val layout = "for (a = 1; b <- Some(2)) yield a + b"
    val tree = Term.ForYield(
      List(
        Enumerator.Val(patvar("a"), lit(1)),
        Enumerator.Generator(patvar("b"), tapply(tname("Some"), lit(2))),
      ),
      tinfix(tname("a"), "+", tname("b")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("enum-val-first-ok-2") {
    val code =
      """|for {
         |  a = 1
         |  b <- Some(2)
         |  c = 3
         |} yield a + b + c
         |""".stripMargin
    val layout = "for (a = 1; b <- Some(2); c = 3) yield a + b + c"
    val tree = Term.ForYield(
      List(
        Enumerator.Val(patvar("a"), lit(1)),
        Enumerator.Generator(patvar("b"), tapply(tname("Some"), lit(2))),
        Enumerator.Val(patvar("c"), lit(3)),
      ),
      tinfix(tinfix(tname("a"), "+", tname("b")), "+", tname("c")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("enum-val-first-ok-3") {
    val code =
      """|for {
         |  a = 1
         |  b = 3
         |  c <- Some(4)
         |} yield a + b + c
         |""".stripMargin
    val layout = "for (a = 1; b = 3; c <- Some(4)) yield a + b + c"
    val tree = Term.ForYield(
      List(
        Enumerator.Val(patvar("a"), lit(1)),
        Enumerator.Val(patvar("b"), lit(3)),
        Enumerator.Generator(patvar("c"), tapply(tname("Some"), lit(4))),
      ),
      tinfix(tinfix(tname("a"), "+", tname("b")), "+", tname("c")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("enum-val-first-ok-4") {
    val code =
      """|for {
         |  a = 1
         |  b <- Some(2)
         |  c = 3
         |  d <- Some(4)
         |} yield a + b + c + d
         |""".stripMargin
    val layout = "for (a = 1; b <- Some(2); c = 3; d <- Some(4)) yield a + b + c + d"
    val tree = Term.ForYield(
      List(
        Enumerator.Val(patvar("a"), lit(1)),
        Enumerator.Generator(patvar("b"), tapply(tname("Some"), lit(2))),
        Enumerator.Val(patvar("c"), lit(3)),
        Enumerator.Generator(patvar("d"), tapply(tname("Some"), lit(4))),
      ),
      tinfix(tinfix(tinfix(tname("a"), "+", tname("b")), "+", tname("c")), "+", tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("enum-val-first-ok-5") {
    val code =
      """|for {
         |  a = 1
         |  b <- Some(2)
         |  if b > 0
         |  c = 3
         |  d <- Some(4)
         |} yield a + b + c + d
         |""".stripMargin
    val layout = "for (a = 1; b <- Some(2); if b > 0; c = 3; d <- Some(4)) yield a + b + c + d"
    val tree = Term.ForYield(
      List(
        Enumerator.Val(patvar("a"), lit(1)),
        Enumerator.Generator(patvar("b"), tapply(tname("Some"), lit(2))),
        Enumerator.Guard(tinfix(tname("b"), ">", lit(0))),
        Enumerator.Val(patvar("c"), lit(3)),
        Enumerator.Generator(patvar("d"), tapply(tname("Some"), lit(4))),
      ),
      tinfix(tinfix(tinfix(tname("a"), "+", tname("b")), "+", tname("c")), "+", tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("enum-val-first-ok-6") {
    val code =
      """|for {
         |  a = 1
         |  b <- Some(2)
         |  c = 3
         |  if b > 0
         |  d <- Some(4)
         |} yield a + b + c + d
         |""".stripMargin
    val layout = "for (a = 1; b <- Some(2); c = 3; if b > 0; d <- Some(4)) yield a + b + c + d"
    val tree = Term.ForYield(
      List(
        Enumerator.Val(patvar("a"), lit(1)),
        Enumerator.Generator(patvar("b"), tapply(tname("Some"), lit(2))),
        Enumerator.Val(patvar("c"), lit(3)),
        Enumerator.Guard(tinfix(tname("b"), ">", lit(0))),
        Enumerator.Generator(patvar("d"), tapply(tname("Some"), lit(4))),
      ),
      tinfix(tinfix(tinfix(tname("a"), "+", tname("b")), "+", tname("c")), "+", tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("enum-val-first-ok-7") {
    val code =
      """|for {
         |  a = 1
         |  if b > 0
         |  b <- Some(2)
         |} yield a + b
         |""".stripMargin
    val layout = "for (a = 1; if b > 0; b <- Some(2)) yield a + b"
    val tree = Term.ForYield(
      List(
        Enumerator.Val(patvar("a"), lit(1)),
        Enumerator.Guard(tinfix(tname("b"), ">", lit(0))),
        Enumerator.Generator(patvar("b"), tapply(tname("Some"), lit(2))),
      ),
      tinfix(tname("a"), "+", tname("b")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("enum-val-first-ok-8") {
    val code =
      """|for {
         |  if a > 0
         |  b <- Some(2)
         |} yield b
         |""".stripMargin
    runTestError[Stat](
      code,
      """|error: illegal start of simple pattern
         |  if a > 0
         |  ^""".stripMargin,
    )
  }

  test("enum-val-first-ok-9") {
    val code =
      """|for {
         |  a = 1
         |  b = 3
         |  if b > 0
         |  c <- Some(2)
         |} yield a + b + c
         |""".stripMargin
    val layout = "for (a = 1; b = 3; if b > 0; c <- Some(2)) yield a + b + c"
    val tree = Term.ForYield(
      List(
        Enumerator.Val(patvar("a"), lit(1)),
        Enumerator.Val(patvar("b"), lit(3)),
        Enumerator.Guard(tinfix(tname("b"), ">", lit(0))),
        Enumerator.Generator(patvar("c"), tapply(tname("Some"), lit(2))),
      ),
      tinfix(tinfix(tname("a"), "+", tname("b")), "+", tname("c")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("enum-val-first-ok-10") {
    val code =
      """|for {
         |  a = 1
         |} yield a
         |""".stripMargin
    val layout = "for (a = 1) yield a"
    val tree = Term.ForYield(List(Enumerator.Val(patvar("a"), lit(1))), tname("a"))
    runTestAssert[Stat](code, layout)(tree)
  }

  // --------------------------
  // WHILE
  // --------------------------

  test("old-while-single") {
    val code = "while (cond) fx"
    runTestAssert[Stat](code)(Term.While(tname("cond"), tname("fx")))
  }

  test("old-while-multi") {
    val code =
      """|while (cond) {
         |  fx
         |  fy
         |}
         |""".stripMargin
    runTestAssert[Stat](code)(Term.While(tname("cond"), blk(tname("fx"), tname("fy"))))
  }

  test("new-while-single1") {
    val code =
      """|while cond do fx
         |""".stripMargin
    val output = "while (cond) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.While(tname("cond"), tname("fx")))
  }

  test("new-while-single2") {
    val code =
      """|while cond
         |do fx
         |""".stripMargin
    val output = "while (cond) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.While(tname("cond"), tname("fx")))
  }

  test("new-while-indented-witout-do") {
    val code =
      """|while (cond)
         |  fx
         |  gx
         |""".stripMargin
    val output =
      """|while (cond) {
         |  fx
         |  gx
         |}""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(tname("cond"), blk(tname("fx"), tname("gx"))),
    )
  }

  test("new-while-multi") {
    val code =
      """|while
         |  fx +
         |  fy
         |do
         |  fx
         |  fy
         |""".stripMargin
    val output =
      """|while (fx + fy) {
         |  fx
         |  fy
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(tinfix(tname("fx"), "+", tname("fy")), blk(tname("fx"), tname("fy"))),
    )
  }

  test("new-while-multistat") {
    val code =
      """|while
         |  s1
         |  s2
         |do
         |  fx
         |  fy
         |""".stripMargin
    val output =
      """|while ({
         |  s1
         |  s2
         |}) {
         |  fx
         |  fy
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.While(blk(tname("s1"), tname("s2")), blk(tname("fx"), tname("fy"))),
    )
  }

  test("while-parens-yet-do") {
    val code =
      """|def read(): String = {
         |  while (cond) do {}
         |  other()
         |}
         |""".stripMargin
    val output =
      """|def read(): String = {
         |  while (cond) {}
         |  other()
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Def(
      Nil,
      tname("read"),
      Nil,
      List(List()),
      Some(pname("String")),
      blk(Term.While(tname("cond"), blk()), tapply(tname("other"))),
    ))
  }

  test("while-cond-expr-do") {
    val code =
      """|  while (x > 0) && (y > 0) do
         |    x += 1
         |""".stripMargin

    val output = "while (x > 0 && y > 0) x += 1"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.While(
      tinfix(tinfix(tname("x"), ">", int(0)), "&&", tinfix(tname("y"), ">", int(0))),
      tinfix(tname("x"), "+=", int(1)),
    ))
  }

  test("while-cond-expr-lf-do") {
    val code =
      """|  while (x > 0) && (y > 0)
         |  do
         |    x += 1
         |""".stripMargin

    val output = "while (x > 0 && y > 0) x += 1"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.While(
      tinfix(tinfix(tname("x"), ">", int(0)), "&&", tinfix(tname("y"), ">", int(0))),
      tinfix(tname("x"), "+=", int(1)),
    ))
  }

  test("while-cond-expr-do [non symbolic op]") {
    val output = "while (x > 0 and y > 0) x += 1"
    val expected = Term.While(
      tinfix(tinfix(tname("x"), ">", int(0)), "and", tinfix(tname("y"), ">", int(0))),
      tinfix(tname("x"), "+=", int(1)),
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
      tinfix(tname("sym"), "==", tselect("defn", "BooleanClass")),
      tapplytype(tname("classOf"), pname("Boolean")),
      Term.If(
        tinfix(tname("sym"), "==", tselect("defn", "ByteClass")),
        tapplytype(tname("classOf"), pname("Byte")),
        Lit.Unit(),
        Nil,
      ),
      Nil,
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
      tinfix(tname("sym"), "==", tselect("defn", "BooleanClass")),
      blk(tapplytype(tname("classOf"), pname("Boolean"))),
      Term.If(
        tinfix(tname("sym"), "==", tselect("defn", "ByteClass")),
        tapplytype(tname("classOf"), pname("Byte")),
        Lit.Unit(),
        Nil,
      ),
      Nil,
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
    runTestAssert[Stat](code, Some(layout))(blk(
      Term.While(tinfix(tname("x"), ">", int(0)), tapply(tname("&&"), tinfix(tname("y"), ">", int(0)))),
      tinfix(tname("x"), "+=", int(1)),
    ))
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
      """|error: `}` expected but `integer constant` found
         |  while (x > 0) && y > 0
         |                       ^""".stripMargin,
    )
  }

  // --------------------------
  // OTHER
  // --------------------------

  test("right-arrow-indentation-block") {
    val code =
      """|class A { slf =>
         |
         |  val x = 3
         |}
         |""".stripMargin
    val output = "class A { slf => val x = 3 }"
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(self("slf"), Defn.Val(Nil, List(patvar("x")), None, int(3))),
    ))
  }

  // --------------------------
  // MATCH CASE
  // --------------------------

  test("old-match-case-empty") {
    val code =
      """|x match {
         |  case 1 =>
         |  case 2 =>
         |}
         |""".stripMargin
    runTestAssert[Stat](code)(tmatch(tname("x"), Case(int(1), None, blk()), Case(int(2), None, blk())))
  }

  test("old-match-case-oneline") {
    val code =
      """|x match {
         |  case 1 => "1"
         |  case 2 => "2"
         |}
         |""".stripMargin
    runTestAssert[Stat](code)(
      tmatch(tname("x"), Case(int(1), None, str("1")), Case(int(2), None, str("2"))),
    )
  }

  test("old-match-case-multiline") {
    val code =
      """|x match {
         |  case 1 =>
         |    a1
         |    b1
         |  case 2 =>
         |    a2
         |    b2
         |}
         |""".stripMargin
    runTestAssert[Stat](code)(tmatch(
      tname("x"),
      Case(int(1), None, blk(tname("a1"), tname("b1"))),
      Case(int(2), None, blk(tname("a2"), tname("b2"))),
    ))
  }

  test("old-match-case-inside") {
    val code =
      """|x match {
         |  case 1 =>
         |    y match {
         |      case 5 => "5"
         |      case 6 => "6"
         |    }
         |  case 2 =>
         |    "2"
         |}
         |""".stripMargin
    runTestAssert[Stat](code)(tmatch(
      tname("x"),
      Case(int(1), None, tmatch(tname("y"), Case(int(5), None, str("5")), Case(int(6), None, str("6")))),
      Case(int(2), None, str("2")),
    ))
  }

  test("new-match-case-empty") {
    val code =
      """|x match
         |  case 1 =>
         |  case 2 =>
         |""".stripMargin
    val output =
      """|x match {
         |  case 1 =>
         |  case 2 =>
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      tmatch(tname("x"), Case(int(1), None, blk()), Case(int(2), None, blk())),
    )
  }

  test("new-match-case-oneline") {
    val code =
      """|x match
         |  case 1 => "1"
         |  case 2 => "2"
         |""".stripMargin
    val output =
      """|x match {
         |  case 1 => "1"
         |  case 2 => "2"
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      tmatch(tname("x"), Case(int(1), None, str("1")), Case(int(2), None, str("2"))),
    )
  }

  test("new-match-case-multiline") {
    val code =
      """|x match
         |  case 1 =>
         |    a1
         |    b1
         |  case 2 =>
         |    a2
         |    b2
         |""".stripMargin
    val output =
      """|x match {
         |  case 1 =>
         |    a1
         |    b1
         |  case 2 =>
         |    a2
         |    b2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(tmatch(
      tname("x"),
      Case(int(1), None, blk(tname("a1"), tname("b1"))),
      Case(int(2), None, blk(tname("a2"), tname("b2"))),
    ))
  }

  test("old-match-case-one-align") {
    val code =
      """|cond match {
         |  case a =>
         |  fa
         |  case b =>
         |  fb
         |}
         |""".stripMargin
    val output =
      """|cond match {
         |  case a => fa
         |  case b => fb
         |}
         |""".stripMargin

    runTestAssert[Stat](code, assertLayout = Some(output))(
      tmatch(tname("cond"), Case(patvar("a"), None, tname("fa")), Case(patvar("b"), None, tname("fb"))),
    )
  }

  test("new-match-case-oneline-align") {
    val code =
      """|def fx: String = {
         |  x match
         |  case 1 => "OK"
         |  case 2 => "ERROR"
         |  val c = "123"
         |  c
         |}
         |""".stripMargin
    val output =
      """|def fx: String = {
         |  x match {
         |    case 1 => "OK"
         |    case 2 => "ERROR"
         |  }
         |  val c = "123"
         |  c
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Def(
      Nil,
      tname("fx"),
      Nil,
      Nil,
      Some(pname("String")),
      blk(
        Term.Match(tname("x"), List(Case(int(1), None, str("OK")), Case(int(2), None, str("ERROR")))),
        Defn.Val(Nil, List(patvar("c")), None, str("123")),
        tname("c"),
      ),
    ))
  }

  test("new-match-case-oneline-align-newline") {
    val code =
      """|def fx: String = {
         |  x match
         |  case 2 =>
         |    "ERROR"
         |  end match
         |}
         |""".stripMargin
    val output =
      """|def fx: String = {
         |  x match {
         |    case 2 => "ERROR"
         |  }
         |  end match
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Def(
      Nil,
      tname("fx"),
      Nil,
      Nil,
      Some(pname("String")),
      blk(tmatch(tname("x"), Case(int(2), None, str("ERROR"))), Term.EndMarker(tname("match"))),
    ))
  }

  test("unsure-correct") {
    val code =
      """|try func match
         |  case A => Accept
         |  catch case ex => Error
         |""".stripMargin
    val output =
      """|try func match {
         |  case A => Accept
         |} catch {
         |  case ex => Error
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.Try(
      tmatch(tname("func"), Case(tname("A"), None, tname("Accept"))),
      List(Case(patvar("ex"), None, tname("Error"))),
      None,
    ))
  }

  test("case-match-ignore-indent") {
    val expected =
      """|x match {
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
      assertLayout = Some(expected),
    )(tmatch(tname("x"), Case(patvar("x"), None, blk(tapply(tname("a")), tapply(tname("b"))))))
  }

  test("match-chained unindented") {
    val layout =
      """|(xs match {
         |  case Nil => "empty"
         |  case x :: xs1 => "nonempty"
         |}) match {
         |  case "empty" => 0
         |  case "nonempty" => 1
         |}
         |""".stripMargin

    val tree = tmatch(
      tmatch(
        tname("xs"),
        Case(tname("Nil"), None, str("empty")),
        Case(patinfix(patvar("x"), "::", patvar("xs1")), None, str("nonempty")),
      ),
      Case(str("empty"), None, int(0)),
      Case(str("nonempty"), None, int(1)),
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
      layout,
    )(tree)

    runTestAssert[Stat](
      """|xs match
         |case Nil => "empty"
         |case x :: xs1 => "nonempty"
         |match
         |case "empty" => 0
         |case "nonempty" => 1
         |""".stripMargin,
      layout,
    )(tree)
  }

  test("match-chained indented") {
    val layout =
      """|xs match {
         |  case Nil =>
         |    "empty"
         |  case x :: xs1 =>
         |    "nonempty" match {
         |      case "empty" => 0
         |      case "nonempty" => 1
         |    }
         |}
         |""".stripMargin

    val tree = tmatch(
      tname("xs"),
      Case(tname("Nil"), None, str("empty")),
      Case(
        patinfix(patvar("x"), "::", patvar("xs1")),
        None,
        tmatch(str("nonempty"), Case(str("empty"), None, int(0)), Case(str("nonempty"), None, int(1))),
      ),
    )

    runTestAssert[Stat](
      """|xs match {
         |  case Nil => "empty"
         |  case x :: xs1 => "nonempty" match {
         |    case "empty" => 0
         |    case "nonempty" => 1
         |  }
         |}
         |""".stripMargin,
      layout,
    )(tree)

    runTestAssert[Stat](
      """|xs match
         |  case Nil => "empty"
         |  case x :: xs1 => "nonempty"
         |  match
         |    case "empty" => 0
         |    case "nonempty" => 1
         |
         |""".stripMargin,
      layout,
    )(tree)
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
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("hello")),
      None,
      tmatch(
        tinfix(
          tmatch(
            tname("xs"),
            Case(tname("Nil"), None, str("empty")),
            Case(patinfix(patvar("x"), "::", patvar("xs1")), None, str("nonempty")),
          ),
          "startsWith",
          str("empty"),
        ),
        Case(bool(true), None, int(0)),
        Case(bool(false), None, int(1)),
      ),
    ))

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
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("hello")),
      None,
      tmatch(
        tinfix(
          tmatch(
            tname("xs"),
            Case(tname("Nil"), None, int(0)),
            Case(patinfix(patvar("x"), "::", patvar("xs1")), None, int(1)),
          ),
          "+",
          int(1),
        ),
        Case(int(1), None, bool(true)),
        Case(int(2), None, bool(false)),
      ),
    ))
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
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("hello")),
      None,
      tmatch(
        tinfix(
          tmatch(
            tinfix(int(1), "+", tname("xs")),
            Case(tname("Nil"), None, int(0)),
            Case(patinfix(patvar("x"), "::", patvar("xs1")), None, int(1)),
          ),
          "+",
          int(1),
        ),
        Case(int(1), None, bool(true)),
        Case(int(2), None, bool(false)),
      ),
    ))
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
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("hello")),
      None,
      tmatch(
        tinfix(
          tmatch(
            tinfix(int(1), "+", tname("xs")),
            Case(tname("Nil"), None, int(0)),
            Case(patinfix(patvar("x"), "::", patvar("xs1")), None, int(1)),
          ),
          "+",
          int(1),
        ),
        Case(int(1), None, bool(true)),
        Case(int(2), None, bool(false)),
      ),
    ))
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
           |}""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("hello")),
      None,
      tmatch(
        tinfix(
          tmatch(
            tinfix(int(1), "foo", tname("xs")),
            Case(tname("Nil"), None, int(0)),
            Case(patinfix(patvar("x"), "::", patvar("xs1")), None, int(1)),
          ),
          "foo",
          int(1),
        ),
        Case(int(1), None, bool(true)),
        Case(int(2), None, bool(false)),
      ),
    ))
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
           |}""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("hello")),
      None,
      tmatch(
        tinfix(
          tmatch(
            tinfix(int(1), "foo", tname("xs")),
            Case(tname("Nil"), None, int(0)),
            Case(patinfix(patvar("x"), "::", patvar("xs1")), None, int(1)),
          ),
          "foo",
          int(1),
        ),
        Case(int(1), None, bool(true)),
        Case(int(2), None, bool(false)),
      ),
    ))
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
           |}""".stripMargin,
      ),
    )(blk(
      Defn.Val(
        Nil,
        List(patvar("hello")),
        None,
        tmatch(
          tinfix(int(1), "foo", tname("xs")),
          Case(tname("Nil"), None, int(0)),
          Case(patinfix(patvar("x"), "::", patvar("xs1")), None, int(1)),
        ),
      ),
      Term.EndMarker(tname("match")),
      tmatch(tname("foo"), Case(int(1), None, bool(true)), Case(int(2), None, bool(false))),
    ))
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
           |}""".stripMargin,
      ),
    )(tmatch(
      tname("foo"),
      Case(patvar("a"), None, tmatch(tname("bar"), Case(patvar("aa"), None, blk()))),
      Case(patvar("b"), None, tname("baz")),
    ))
  }

  // Plain nested matches (via case bodies), braceless: a single dedent from the
  // innermost match closes several indent regions, and the outer `case _` belongs to
  // the outer match.
  test("match-nested multi-level dedent") {
    val code =
      """|x match
         |  case a =>
         |    b match
         |      case c =>
         |        d match
         |          case e => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|x match {
         |  case a =>
         |    b match {
         |      case c =>
         |        d match {
         |          case e => 1
         |        }
         |    }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tmatch(
      tname("x"),
      Case(
        patvar("a"),
        None,
        tmatch(tname("b"), Case(patvar("c"), None, tmatch(tname("d"), Case(patvar("e"), None, lit(1))))),
      ),
      Case(patwildcard, None, lit(2)),
    ))
  }

  // --------------------------
  // MATCH SUB-CASES
  // https://docs.scala-lang.org/scala3/reference/experimental/sub-cases.html
  // --------------------------

  // The reported issue: outer match in braces, single sub-case inline.
  test("sub-case: single inline in braces") {
    val code =
      """|d match {
         |  case Some(m) if m.content match case c: T => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val layout =
      """|d match {
         |  case Some(m) if m.content match {
         |    case c: T => 1
         |  }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tmatch(
      tname("d"),
      Case(
        patextract(tname("Some"), patvar("m")),
        None,
        tsubmatch(tselect("m", "content"), Case(Pat.Typed(patvar("c"), pname("T")), None, lit(1))),
      ),
      Case(patwildcard, None, lit(2)),
    ))
  }

  // Same as above but braceless outer match: the single inline sub-case is delimited
  // and the dedented `case _` belongs to the outer match.
  test("sub-case: single inline, braceless outer") {
    val code =
      """|d match
         |  case Some(m) if m.content match case c: T => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|d match {
         |  case Some(m) if m.content match {
         |    case c: T => 1
         |  }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tmatch(
      tname("d"),
      Case(
        patextract(tname("Some"), patvar("m")),
        None,
        tsubmatch(tselect("m", "content"), Case(Pat.Typed(patvar("c"), pname("T")), None, lit(1))),
      ),
      Case(patwildcard, None, lit(2)),
    ))
  }

  test("sub-case: boolean guard then sub-match in braces") {
    val code =
      """|d match {
         |  case Some(m) if cond if m.content match case c: T => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val layout =
      """|d match {
         |  case Some(m) if cond if m.content match {
         |    case c: T => 1
         |  }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tmatch(
      tname("d"),
      Case(
        patextract(tname("Some"), patvar("m")),
        Some(tname("cond")),
        tsubmatch(tselect("m", "content"), Case(Pat.Typed(patvar("c"), pname("T")), None, lit(1))),
      ),
      Case(patwildcard, None, lit(2)),
    ))
  }

  test("sub-case: indented multi sub-case") {
    val code =
      """|d match
         |  case Some(x) if x.version match
         |    case Stable(m, n) if m > 2 => 1
         |    case Legacy => 2
         |  case _ => 3
         |""".stripMargin
    val layout =
      """|d match {
         |  case Some(x) if x.version match {
         |    case Stable(m, n) if m > 2 => 1
         |    case Legacy => 2
         |  }
         |  case _ =>
         |    3
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tmatch(
      tname("d"),
      Case(
        patextract(tname("Some"), patvar("x")),
        None,
        tsubmatch(
          tselect("x", "version"),
          Case(
            patextract(tname("Stable"), patvar("m"), patvar("n")),
            Some(tinfix(tname("m"), ">", lit(2))),
            lit(1),
          ),
          Case(tname("Legacy"), None, lit(2)),
        ),
      ),
      Case(patwildcard, None, lit(3)),
    ))
  }

  test("sub-case: nested sub-sub-match") {
    val code =
      """|x match {
         |  case a if b match {
         |    case c if d match {
         |      case e => 1
         |    }
         |  }
         |  case _ => 2
         |}
         |""".stripMargin
    val layout =
      """|x match {
         |  case a if b match {
         |    case c if d match {
         |      case e => 1
         |    }
         |  }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tmatch(
      tname("x"),
      Case(
        patvar("a"),
        None,
        tsubmatch(
          tname("b"),
          Case(patvar("c"), None, tsubmatch(tname("d"), Case(patvar("e"), None, lit(1)))),
        ),
      ),
      Case(patwildcard, None, lit(2)),
    ))
  }

  // A match-as-operator expression is still a valid boolean guard when followed by
  // `=>`; sub-cases must not change that (only a missing `=>`, or a second `if`, turns
  // the guard `match` into a sub-match). The reprint parenthesizes the guard match.
  test("sub-case: match-as-operator guard with => stays a guard") {
    val code = "x match { case foo if bar match { case baz => true } => 1 }"
    val layout =
      """|x match {
         |  case foo if (bar match {
         |    case baz => true
         |  }) => 1
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tmatch(
      tname("x"),
      Case(patvar("foo"), Some(tmatch(tname("bar"), Case(patvar("baz"), None, lit(true)))), lit(1)),
    ))
  }

  // Braceless nested sub-sub-match: a single dedent from the innermost sub-match to the
  // outer level closes several sub-match indent regions at once, so the outer `case _`
  // belongs to the outer match.
  test("sub-case: nested sub-sub-match, braceless") {
    val code =
      """|x match
         |  case a if b match
         |    case c if d match
         |      case e => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|x match {
         |  case a if b match {
         |    case c if d match {
         |      case e => 1
         |    }
         |  }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tmatch(
      tname("x"),
      Case(
        patvar("a"),
        None,
        tsubmatch(
          tname("b"),
          Case(patvar("c"), None, tsubmatch(tname("d"), Case(patvar("e"), None, lit(1)))),
        ),
      ),
      Case(patwildcard, None, lit(2)),
    ))
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
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("foo"),
      None,
      None,
      Term.If(
        tinfix(
          tinfix(tname("bar"), "eq", tname("baz")),
          "&&",
          tinfix(tname("qux"), "!=", tname("xyz")),
        ),
        tname("found"),
        tname("notfound"),
        Nil,
      ),
    ))
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
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("foo"),
      None,
      None,
      Term.If(
        tselect(tinfix(tname("bar"), "eq", tname("baz")), "qux"),
        tname("found"),
        tname("notfound"),
        Nil,
      ),
    ))
  }

  test("match-dot") {
    val expected = Term.If(
      tselectmatch("xs", Case("Nil", None, bool(false)), Case(patwildcard, None, bool(true))),
      str("nonempty"),
      str("empty"),
    )
    runTestAssert[Stat](
      """|if xs.match {
         |  case Nil => false
         |  case _ => true
         |}
         |then "nonempty"
         |else "empty"
         |""".stripMargin,
      """|if (xs.match {
         |  case Nil => false
         |  case _ => true
         |}) "nonempty" else "empty"
         |""".stripMargin,
    )(expected)
  }

  test("match-dot-def") {
    val expected = Defn.Def(
      Nil,
      tname("mtch"),
      Nil,
      List(List(tparam("x", "Int"))),
      Some(pname("String")),
      tapply(tselect(
        tselectmatch("x", Case(int(1), None, str("1")), Case(patwildcard, None, str("ERR"))),
        "trim",
      )),
    )

    runTestAssert[Stat](
      """|def mtch(x: Int): String =
         |   x.match {
         |     case 1 => "1"
         |     case _ => "ERR"
         |   }.trim()
         |""".stripMargin,
      """|def mtch(x: Int): String = x.match {
         |  case 1 => "1"
         |  case _ => "ERR"
         |}.trim()
         |""".stripMargin,
    )(expected)
  }

  test("catch-case-in-paren") {
    val code =
      """|fx(p1,
         |   try func()
         |   catch case x => ok())
         |""".stripMargin
    val expected =
      """|fx(p1, try func() catch {
         |  case x =>
         |    ok()
         |})""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(expected))(tapply(
      tname("fx"),
      tname("p1"),
      Term.Try(tapply(tname("func")), List(Case(patvar("x"), None, tapply(tname("ok")))), None),
    ))
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
    runTestAssert[Stat](code, assertLayout = Some(expected))(tmatch(
      tname("a"),
      Case(Pat.Extract(tname("A"), Nil), None, tname("succ")),
      Case(patwildcard, None, tname("fail")),
    ))
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
    runTestAssert[Stat](code, assertLayout = Some(expected))(Defn.Object(
      Nil,
      tname("Z"),
      tpl(
        tmatch(
          tname("a"),
          Case(Pat.Extract(tname("A"), Nil), None, tname("succ")),
          Case(patwildcard, None, blk()),
        ),
        Defn.Val(Nil, List(patvar("x")), None, int(0)),
      ),
    ))
  }

  test("if-infix")(
    runTestAssert[Stat](
      """|if (1) max 10 gt 0
         |
         |then
         |  1
         |else
         |  2
         |""".stripMargin,
      assertLayout = Some("if (1 max 10 gt 0) 1 else 2"),
    )(Term.If(tinfix(tinfix(int(1), "max", int(10)), "gt", int(0)), int(1), int(2), Nil)),
  )

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
      assertLayout = Some {
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
      },
    ) {
      Defn.Object(
        Nil,
        tname("Test"),
        tpl(
          Term.Try(
            tmatch(
              tapply(tname("List"), int(1), int(2), int(3)),
              Case(
                patinfix(patvar("x"), "::", patvar("xs")),
                None,
                tapply(tname("println"), tname("x")),
              ),
              Case(tname("Nil"), None, tapply(tname("println"), str("Nil"))),
            ),
            List(
              Case(
                Pat.Typed(patvar("ex"), Type.Select(tselect("java", "io"), pname("IOException"))),
                None,
                tapply(tname("println"), tname("ex")),
              ),
              Case(Pat.Typed(patvar("ex"), pname("Throwable")), None, Term.Throw(tname("ex"))),
            ),
            None,
          ),
          Term.EndMarker(tname("try")),
        ),
      )
    }
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
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      List(List(tparam("x", "Int"))),
      None,
      tapply(
        tname("assert"),
        Term.If(
          tinfix(tname("x"), ">", int(0)),
          bool(true),
          Term.If(tinfix(tname("x"), "<", int(0)), bool(true), bool(false), Nil),
          Nil,
        ),
        str("fail"),
      ),
    ))
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
        """|object foo {
           |  def bar = if (map {
           |    op(c == true)
           |  }) err(context)
           |}
           |""".stripMargin,
      ),
    )(Defn.Object(
      Nil,
      tname("foo"),
      tpl(Defn.Def(
        Nil,
        tname("bar"),
        Nil,
        None,
        Term.If(
          tapply(tname("map"), blk(tapply(tname("op"), tinfix(tname("c"), "==", bool(true))))),
          tapply(tname("err"), tname("context")),
          Lit.Unit(),
          Nil,
        ),
      )),
    ))
  }

  test("if-then 2") {
    runTestAssert[Stat](
      """|object foo:
         |  def bar =
         |    if (a + b) { op(c == true) } then err(context)
         |""".stripMargin,
      assertLayout = Some(
        """object foo {
          |  def bar = if ((a + b) {
          |    op(c == true)
          |  }) err(context)
          |}
          |""".stripMargin,
      ),
    )(Defn.Object(
      Nil,
      tname("foo"),
      tpl(Defn.Def(
        Nil,
        tname("bar"),
        Nil,
        None,
        Term.If(
          tapply(
            tinfix(tname("a"), "+", tname("b")),
            blk(tapply(tname("op"), tinfix(tname("c"), "==", bool(true)))),
          ),
          tapply(tname("err"), tname("context")),
          Lit.Unit(),
          Nil,
        ),
      )),
    ))
  }

  test("if-then 3") {
    runTestAssert[Stat](
      """|object foo:
         |  def bar =
         |    if (a + b) op c == true then err(context)
         |""".stripMargin,
      assertLayout = Some(
        """|object foo { def bar = if (a + b op c == true) err(context) }
           |""".stripMargin,
      ),
    )(Defn.Object(
      Nil,
      tname("foo"),
      tpl(Defn.Def(
        Nil,
        tname("bar"),
        Nil,
        None,
        Term.If(
          tinfix(tinfix(tname("a"), "+", tname("b")), "op", tinfix(tname("c"), "==", bool(true))),
          tapply(tname("err"), tname("context")),
          Lit.Unit(),
          Nil,
        ),
      )),
    ))
  }

  test("if-then 4") {
    runTestAssert[Stat](
      """|object foo:
         |  def bar =
         |    if (a + b) op c err(context)
         |""".stripMargin,
      assertLayout = Some(
        """|object foo { def bar = if (a + b) op c err(context) }
           |""".stripMargin,
      ),
    )(Defn.Object(
      Nil,
      tname("foo"),
      tpl(Defn.Def(
        Nil,
        tname("bar"),
        Nil,
        None,
        Term.If(
          tinfix(tname("a"), "+", tname("b")),
          tinfix(tname("op"), "c", tapply(tname("err"), tname("context"))),
          Lit.Unit(),
          Nil,
        ),
      )),
    ))
  }

  test("if-then 5") {
    runTestAssert[Stat](
      """|object foo:
         |  def bar =
         |    if (a + b) (op c err(context))
         |""".stripMargin,
      assertLayout = Some(
        """|object foo { def bar = if (a + b) op c err(context) }
           |""".stripMargin,
      ),
    )(Defn.Object(
      Nil,
      tname("foo"),
      tpl(Defn.Def(
        Nil,
        tname("bar"),
        Nil,
        None,
        Term.If(
          tinfix(tname("a"), "+", tname("b")),
          tinfix(tname("op"), "c", tapply(tname("err"), tname("context"))),
          Lit.Unit(),
          Nil,
        ),
      )),
    ))
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
    runTestAssert[Stat](code, assertLayout = Some(output)) {
      Defn.Object(
        Nil,
        tname("a"),
        tpl {
          tinfix(
            tinfix(
              tapply(tselect("constraint", "contains"), tname("tl")),
              "||",
              tapply(tselect("other", "isRemovable"), tname("tl")),
            ),
            "||",
            blk(
              Defn.Val(
                Nil,
                List(patvar("tvars")),
                None,
                tapply(
                  tselect(
                    tapply(
                      tselect("tl", "paramRefs", "map"),
                      Term.AnonymousFunction(
                        tapply(tselect("other", "typeVarOfParam"), Term.Placeholder()),
                      ),
                    ),
                    "collect",
                  ),
                  Term.PartialFunction(
                    Case(Pat.Typed(patvar("tv"), pname("TypeVar")), None, tname("tv")) :: Nil,
                  ),
                ),
              ),
              Term.If(
                tselect(Term.This(anon), "isCommittable"),
                tapply(
                  tselect("tvars", "foreach"),
                  tfunc(tparam("tvar"))(Term.If(
                    tinfix(
                      Term.ApplyUnary(tname("!"), tselect("tvar", "inst", "exists")),
                      "&&",
                      Term.ApplyUnary(
                        tname("!"),
                        tapply(tname("isOwnedAnywhere"), Term.This(anon), tname("tvar")),
                      ),
                    ),
                    tapply(tname("includeVar"), tname("tvar")),
                    Lit.Unit(),
                    Nil,
                  )),
                ),
                Lit.Unit(),
                Nil,
              ),
              tapply(tselect("typeComparer", "addToConstraint"), tname("tl"), tname("tvars")),
            ),
          )
        },
      )
    }
  }

  test("if-then 7") {
    val code =
      """|
         |  private def genScalaClass(td: TypeDef): js.ClassDef = {
         |    val hashedDefs = ir.Hashers.hashMemberDefs(allMemberDefs)
         |
         |    val kind =
         |      if (isStaticModule(sym)) ClassKind.ModuleClass
         |      else if (isHijacked) ClassKind.HijackedClass
         |      else ClassKind.Class
         |  }
         |""".stripMargin
    val output =
      """|private def genScalaClass(td: TypeDef): js.ClassDef = {
         |  val hashedDefs = ir.Hashers.hashMemberDefs(allMemberDefs)
         |  val kind = if (isStaticModule(sym)) ClassKind.ModuleClass else if (isHijacked) ClassKind.HijackedClass else ClassKind.Class
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output)) {
      Defn.Def(
        List(Mod.Private(anon)),
        tname("genScalaClass"),
        Nil,
        List(List(tparam("td", "TypeDef"))),
        Some(pselect("js", "ClassDef")),
        blk(
          Defn.Val(
            Nil,
            List(patvar("hashedDefs")),
            None,
            tapply(tselect("ir", "Hashers", "hashMemberDefs"), tname("allMemberDefs")),
          ),
          Defn.Val(
            Nil,
            List(patvar("kind")),
            None,
            Term.If(
              tapply(tname("isStaticModule"), tname("sym")),
              tselect("ClassKind", "ModuleClass"),
              Term.If(
                tname("isHijacked"),
                tselect("ClassKind", "HijackedClass"),
                tselect("ClassKind", "Class"),
                Nil,
              ),
              Nil,
            ),
          ),
        ),
      )
    }
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
      assertLayout = Some {
        """|arg match {
           |  case arg: Showable =>
           |    try arg.show catch {
           |      case ex: CyclicReference =>
           |        "... (caught cyclic reference) ..."
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
      },
    ) {
      tmatch(
        tname("arg"),
        Case(
          Pat.Typed(patvar("arg"), pname("Showable")),
          None,
          Term.Try(
            tselect("arg", "show"),
            List(
              Case(
                Pat.Typed(patvar("ex"), pname("CyclicReference")),
                None,
                str("... (caught cyclic reference) ..."),
              ),
              Case(
                Pat.Extract(tname("NonFatal"), List(patvar("ex"))),
                Some(tinfix(
                  Term.ApplyUnary(
                    tname("!"),
                    tapply(tselect("ctx", "mode", "is"), tselect("Mode", "PrintShowExceptions")),
                  ),
                  "&&",
                  Term.ApplyUnary(tname("!"), tselect("ctx", "settings", "YshowPrintErrors", "value")),
                )),
                blk(
                  Defn.Val(
                    Nil,
                    List(patvar("msg")),
                    None,
                    tmatch(
                      tname("ex"),
                      Case(
                        Pat.Typed(patvar("te"), pname("TypeError")),
                        None,
                        tselect("te", "toMessage"),
                      ),
                      Case(patwildcard, None, tselect("ex", "getMessage")),
                    ),
                  ),
                  Term.Interpolate(
                    tname("s"),
                    List(str("[cannot display due to "), str(", raw string = "), str("]")),
                    List(tname("msg"), blk(tselect("arg", "toString"))),
                  ),
                ),
              ),
            ),
            None,
          ),
        ),
        Case(patwildcard, None, tapply(tselect("String", "valueOf"), tname("arg"))),
      )
    }
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
           |""".stripMargin,
      ),
    ) {
      Defn.Val(
        Nil,
        List(patvar("tvars")),
        None,
        tapply(
          tselect(
            tselect(
              tapply(
                tselect("targs", "filter"),
                Term.AnonymousFunction(
                  tapplytype(tselect(Term.Placeholder(), "isInstanceOf"), pname("InferredTypeTree")),
                ),
              ),
              "tpes",
            ),
            "collect",
          ),
          Term.PartialFunction(
            Case(
              Pat.Typed(patvar("tvar"), pname("TypeVar")),
              Some(tinfix(
                tinfix(
                  Term.ApplyUnary(tname("!"), tselect("tvar", "isInstantiated")),
                  "&&",
                  tapply(tselect("ctx", "typerState", "ownedVars", "contains"), tname("tvar")),
                ),
                "&&",
                Term.ApplyUnary(tname("!"), tapply(tselect("locked", "contains"), tname("tvar"))),
              )),
              tname("tvar"),
            ) :: Nil,
          ),
        ),
      )
    }
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
           |""".stripMargin,
      ),
    )(Term.If(
      tinfix(tname("columnsVar"), "!=", Lit.Null()),
      tselect("columnsVar", "toInt"),
      Term.If(
        tselect("Properties", "isWin"),
        Term.If(
          tinfix(tname("ansiconVar"), "!=", Lit.Null()),
          tselect("ansiconVar", "toInt"),
          tname("defaultWidth"),
          Nil,
        ),
        tname("defaultWidth"),
        Nil,
      ),
      Nil,
    ))
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
           |""".stripMargin,
      ),
    ) {
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
              tinfix(tname("idx"), ">=", tselect("query", "length")),
              tapply(tname("err"), tname("context")),
              Lit.Unit(),
              Nil,
            ),
          ),
          Defn.Def(
            List(Mod.Private(anon)),
            tname("err"),
            Nil,
            List(List(tparam("problem", "String"))),
            None,
            Term.Throw(Term.New(
              init(pname("QueryParseException"), List(tname("query"), tname("idx"), tname("problem"))),
            )),
          ),
        ),
      )
    }
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
           |""".stripMargin,
      ),
    )(Term.While(
      tinfix(tname("idx"), "<", tselect("str", "length")),
      Term.If(
        tinfix(
          tinfix(tinfix(tname("str"), "charAt", tname("idx")), "!=", lit('$')),
          "||",
          tname("isEscaped"),
        ),
        tinfix(tname("idx"), "+=", int(1)),
        tname("idx"),
        Nil,
      ),
    ))
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
           |""".stripMargin,
      ),
    )(Term.For(
      List(Enumerator.Generator(patvar("c"), tname("str"))),
      Term.If(
        tinfix(tname("c"), "==", lit('\n')),
        blk(tinfix(tname("line"), "+=", int(1)), Term.Assign(tname("char"), int(0))),
        blk(tinfix(tname("char"), "+=", int(1))),
        Nil,
      ),
    ))
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
         |    if (typ) {
         |      // foo
         |    }
         |    if (typeDef) if (typJava) {
         |      // foo
         |    }
         |  case _ =>
         |}
         |""".stripMargin
    val commentedEmptyBlock = Term.Block.newBuilder(Nil).endComment(Seq("// foo")).result()
    def tifEmpty(cond: String) = tif(cond, commentedEmptyBlock)
    val body = blk(tifEmpty("typ"), tif("typeDef", tifEmpty("typJava")))
    runTestAssert[Stat](code, assertLayout = Some(output))(tapply(
      tselect("classDef", "foreach"),
      Term.PartialFunction(List(
        Case(Pat.Typed(patvar("typeSymbol"), pname("Symbol")), None, body),
        Case(patwildcard, None, blk()),
      )),
    ))
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
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.If(
      tinfix(tselect("indexes", "size"), ">", int(1)),
      blk(
        Defn.Val(
          Nil,
          List(patvar("msg")),
          None,
          Term.Interpolate(
            tname("s"),
            List(str("ERROR: Multiple index pages for doc found "), str("")),
            blk(tapply(
              tselect("indexes", "map"),
              Term.AnonymousFunction(tselect(Term.Placeholder(), "file")),
            )) :: Nil,
          ),
        ),
        tapply(tselect("report", "error"), tname("msg")),
      ),
      Lit.Unit(),
      Nil,
    ))
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
    runTestAssert[Stat](code, assertLayout = Some(output))(blk(
      Term.If(
        Term.This(anon),
        tapply(
          tselect("tvars", "foreach"),
          tfunc(tparam("tvar"))(Term.If(
            Term.ApplyUnary(tname("!"), tname("tvar")),
            tapply(tname("includeVar"), tname("tvar")),
            Lit.Unit(),
            Nil,
          )),
        ),
        Lit.Unit(),
        Nil,
      ),
      tapply(tselect("typeComparer", "addToConstraint"), tname("tvars")),
    ))
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
    runTestAssert[Stat](code, assertLayout = Some(output)) {
      blk(
        Defn.Val(
          Nil,
          List(patvar("abstractTypeNames")),
          None,
          Term.ForYield(
            List(
              Enumerator.Generator(patvar("parent"), tname("parents")),
              Enumerator.Generator(patvar("mbr"), tselect("parent", "abstractTypeMembers")),
              Enumerator.Guard(tapply(tname("qualifies"), tselect("mbr", "symbol"))),
            ),
            tselect("mbr", "name", "asTypeName"),
          ),
        ),
        Term.For(List(Enumerator.Generator(patvar("name"), tname("abstractTypeNames"))), tname("foo")),
      )
    }
  }

  test("several nested if, with () as body") {
    val code =
      """|
         |  if !other
         |  then
         |    if (member)
         |      ()
         |    else
         |      overrideError()
         |  else
         |    checkOverrideDeprecated()
         |""".stripMargin
    val output =
      """|if (!other) if (member) () else overrideError() else checkOverrideDeprecated()
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.If(
      Term.ApplyUnary(tname("!"), tname("other")),
      Term.If(tname("member"), Lit.Unit(), tapply(tname("overrideError")), Nil),
      tapply(tname("checkOverrideDeprecated")),
      Nil,
    ))
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
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Val(
      Nil,
      List(patvar("a")),
      None,
      tmatch(
        Term.This(anon),
        Case(
          patvar("a"),
          None,
          tmatch(
            tname("that"),
            Case(patvar("b"), None, blk(tname("bb"), Term.EndMarker(tname("match")))),
          ),
        ),
        Case(patvar("b"), None, tmatch(tname("that"), Case(patvar("c"), None, tname("cc")))),
      ),
    ))
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
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.Try(
      tname("foo"),
      List(
        Case(
          patwildcard,
          None,
          blk(
            Term.Try(tname("bar"), Nil, Some(tname("qux"))),
            Term.TryWithHandler(tname("bar"), tname("baz"), Some(tname("qux"))),
            Term.Try(tname("bar"), List(Case(patwildcard, None, tname("baz"))), Some(tname("qux"))),
          ),
        ),
        Case(patvar("xyz"), None, blk()),
      ),
      None,
    ))
  }

  test("single-line try within catch case, then finally") {
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
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.Try(
      tname("foo"),
      Case(
        patwildcard,
        None,
        blk(
          Term.Try(tname("bar"), Nil, Some(tname("qux"))),
          Term.TryWithHandler(tname("bar"), tname("baz"), Some(tname("qux"))),
          Term.Try(tname("bar"), List(Case(patwildcard, None, tname("baz"))), Some(tname("qux"))),
        ),
      ) :: Nil,
      Some(tname("xyz")),
    ))
  }

  test("#3220") {
    val code =
      """|for {
         |  case (a, b) <- pairs
         |  x <- a to b
         |} yield x
         |""".stripMargin
    val layout = "for ( case (a, b) <- pairs; x <- a to b) yield x"
    runTestAssert[Stat](code, Some(layout))(Term.ForYield(
      List(
        Enumerator.CaseGenerator(Pat.Tuple(List(patvar("a"), patvar("b"))), tname("pairs")),
        Enumerator.Generator(patvar("x"), tinfix(tname("a"), "to", tname("b"))),
      ),
      tname("x"),
    ))
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
    runTestAssert[Stat](code, Some(layout))(Term.ForYield(
      List(Enumerator.Generator(patvar("x2"), tname("x1"))),
      tapply(
        tselect("x2", "x3"),
        Term.PartialFunction(
          Case(patvar("x4"), Some(tapply(tselect("x5", "x6", "x7"), tname("x8"))), tname("x9")) ::
            Nil,
        ),
      ),
    ))
  }

  test("while cond uses match with dot") {
    val code =
      """|if sArr(last) == ')' then
         |  while (sArr(last): @switch).match
         |    case _ => false
         |  do last -= 1
         |""".stripMargin
    val layout =
      """|if (sArr(last) == ')') while ((sArr(last): @switch).match {
         |  case _ => false
         |}) last -= 1
         |""".stripMargin
    assertNoDiff(parseStat(code).reprint, layout)
  }

  test("while cond uses match without dot") {
    val code =
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
    assertNoDiff(parseStat(code).reprint, layout)
  }

  test("scalafmt #3790 case") {
    val code =
      """|case 1 =>
         |  println(2)
         |  (3, 4)
         |""".stripMargin
    val layout =
      """|case 1 =>
         |  println(2)
         |  (3, 4)
         |""".stripMargin
    val tree =
      Case(int(1), None, blk(tapply(tname("println"), int(2)), Term.Tuple(List(int(3), int(4)))))
    runTestAssert[Case](code, layout)(tree)
  }

  test("scalafmt #3790 match optional braces") {
    val code =
      """|def foo =
         |  bar match
         |  case 1 =>
         |    println(2)
         |    (3, 4)
         |  baz
         |""".stripMargin
    val layout =
      """|def foo = {
         |  bar match {
         |    case 1 =>
         |      println(2)
         |      (3, 4)
         |  }
         |  baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      None,
      blk(
        tmatch(
          tname("bar"),
          Case(int(1), None, blk(tapply(tname("println"), int(2)), Term.Tuple(List(int(3), int(4))))),
        ),
        tname("baz"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3790 match optional braces, and case class following in case body") {
    val code =
      """|def foo =
         |  bar match
         |  case 1 =>
         |    println(2)
         |    (3, 4)
         |    case class A(a: Int)
         |""".stripMargin
    val layout =
      """|def foo = bar match {
         |  case 1 =>
         |    println(2)
         |    (3, 4)
         |    case class A(a: Int)
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      None,
      tmatch(
        tname("bar"),
        Case(
          int(1),
          None,
          blk(
            tapply(tname("println"), int(2)),
            Term.Tuple(List(int(3), int(4))),
            Defn.Class(List(Mod.Case()), pname("A"), Nil, ctorp(tparam("a", "Int")), tplNoBody()),
          ),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3790 match optional braces, and case class following after case body") {
    val code =
      """|def foo =
         |  bar match
         |  case 1 =>
         |    println(2)
         |    (3, 4)
         |  case class A(a: Int)
         |""".stripMargin
    val layout =
      """|def foo = {
         |  bar match {
         |    case 1 =>
         |      println(2)
         |      (3, 4)
         |  }
         |  case class A(a: Int)
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      None,
      blk(
        tmatch(
          tname("bar"),
          Case(int(1), None, blk(tapply(tname("println"), int(2)), Term.Tuple(List(int(3), int(4))))),
        ),
        Defn.Class(List(Mod.Case()), pname("A"), Nil, ctorp(tparam("a", "Int")), tplNoBody()),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3790 match actual braces") {
    val code =
      """|def foo =
         |  bar match {
         |  case 1 =>
         |    println(2)
         |    (3, 4)
         |  baz}
         |""".stripMargin
    val layout =
      """|def foo = bar match {
         |  case 1 =>
         |    println(2)
         |    (3, 4)
         |    baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      None,
      tmatch(
        tname("bar"),
        Case(
          int(1),
          None,
          blk(tapply(tname("println"), int(2)), Term.Tuple(List(int(3), int(4))), tname("baz")),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3941 no significant indentation with quiet syntax: if-then 1") {
    implicit val dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def demo() = {
         |  if true then greet()
         |  else
         |    sayGoodbye()
         |    openDoor()
         |}
         |""".stripMargin
    val layout =
      """|def demo() = {
         |  if (true) greet() else sayGoodbye()
         |  openDoor()
         |}""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("demo"),
      Nil,
      List(Nil),
      None,
      blk(
        Term.If(lit(true), tapply(tname("greet")), tapply(tname("sayGoodbye")), Nil),
        tapply(tname("openDoor")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3941 no significant indentation with quiet syntax: if-then 2") {
    implicit val dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def demo() = {
         |  if (a + b)
         |    == (c + d)
         |  then greet()
         |  else
         |    sayGoodbye()
         |    openDoor()
         |}
         |""".stripMargin
    val layout =
      """|def demo() = {
         |  if (a + b == c + d) greet() else sayGoodbye()
         |  openDoor()
         |}""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("demo"),
      Nil,
      List(Nil),
      None,
      blk(
        Term.If(
          tinfix(tinfix(tname("a"), "+", tname("b")), "==", tinfix(tname("c"), "+", tname("d"))),
          tapply(tname("greet")),
          tapply(tname("sayGoodbye")),
          Nil,
        ),
        tapply(tname("openDoor")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3941 no significant indentation with quiet syntax: while-do 1") {
    implicit val dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def demo() = {
         |  while true do
         |    sayGoodbye()
         |    openDoor()
         |}
         |""".stripMargin
    val layout =
      """|def demo() = {
         |  while (true) sayGoodbye()
         |  openDoor()
         |}""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("demo"),
      Nil,
      List(Nil),
      None,
      blk(Term.While(lit(true), tapply(tname("sayGoodbye"))), tapply(tname("openDoor"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3941 no significant indentation with quiet syntax: while-do 2") {
    implicit val dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def demo() = {
         |  while (a + b)
         |    == (c + d)
         |  do
         |    sayGoodbye()
         |    openDoor()
         |}
         |""".stripMargin
    val layout =
      """|def demo() = {
         |  while (a + b == c + d) sayGoodbye()
         |  openDoor()
         |}""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("demo"),
      Nil,
      List(Nil),
      None,
      blk(
        Term.While(
          tinfix(tinfix(tname("a"), "+", tname("b")), "==", tinfix(tname("c"), "+", tname("d"))),
          tapply(tname("sayGoodbye")),
        ),
        tapply(tname("openDoor")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indent in enumerator") {
    val code =
      """|object a:
         |      val abstractTypeNames =
         |        for (
         |          parent <-
         |           parents;
         |          mbr <- parent.abstractTypeMembers if qualifies(mbr.symbol))
         |        yield mbr.name.asTypeName
         |""".stripMargin
    val layout =
      "object a { val abstractTypeNames = for (parent <- parents; mbr <- parent.abstractTypeMembers; if qualifies(mbr.symbol)) yield mbr.name.asTypeName }"
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Defn.Val(
        Nil,
        List(patvar("abstractTypeNames")),
        None,
        Term.ForYield(
          List(
            Enumerator.Generator(patvar("parent"), tname("parents")),
            Enumerator.Generator(patvar("mbr"), tselect("parent", "abstractTypeMembers")),
            Enumerator.Guard(tapply(tname("qualifies"), tselect("mbr", "symbol"))),
          ),
          tselect("mbr", "name", "asTypeName"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("terminate inline case on newline and same indent") {
    val code =
      """|object a:
         |   try foo
         |   catch case ex => new Bar()
         |   private[io] def baz = qux
         |""".stripMargin
    val layout =
      """|object a {
         |  try foo catch {
         |    case ex =>
         |      new Bar()
         |  }
         |  private[io] def baz = qux
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(
        Term.Try(tname("foo"), List(Case(patvar("ex"), None, Term.New(init("Bar", Nil)))), None),
        Defn.Def(List(Mod.Private(Name("io"))), tname("baz"), Nil, None, tname("qux")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("break after <- in for-yield") {
    val code =
      """|object a:
         |  for foo <-
         |    bar
         |  yield
         |    foo
         |""".stripMargin
    val layout = "object a { for (foo <- bar) yield foo }"
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Term.ForYield(List(Enumerator.Generator(patvar("foo"), tname("bar"))), tname("foo"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("outdent inlinw try-finally opt-braces") {
    val code =
      """|object a:
         |  def foo =
         |      try bar &&
         |          baz
         |      finally qux
         |""".stripMargin
    val layout = "object a { def foo = try bar && baz finally qux }"
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Defn.Def(
        Nil,
        tname("foo"),
        Nil,
        None,
        Term.Try(tinfix(tname("bar"), "&&", tname("baz")), None, Some(tname("qux"))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("outdent inlinw try-finally in braces") {
    val code =
      """|object a:
         |  def foo = {
         |      try bar &&
         |          baz
         |      finally qux
         |  }
         |""".stripMargin
    val layout =
      """|object a {
         |  def foo = {
         |    try bar && baz finally qux
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Defn.Def(
        Nil,
        tname("foo"),
        Nil,
        None,
        blk(Term.Try(tinfix(tname("bar"), "&&", tname("baz")), None, Some(tname("qux")))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3979 w/ grouped whitespace") {
    @nowarn("msg=never used|unused") // runTestAssert does not read it
    implicit def tokenizerOptions: TokenizerOptions = new TokenizerOptions(groupWhitespace = true)

    val code =
      """|for {
         |  _ <-
         |    if (a) {
         |      b
         |    } else {
         |      c
         |    }
         |
         |  _ <- d
         |} yield e
         |""".stripMargin

    val struct =
      """|BOF [0..0)
         |KwFor [0..3)
         |LeftBrace [4..5)
         |Underscore [8..9)
         |LeftArrow [10..12)
         |Indentation.Indent [12..12)
         |KwIf [17..19)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |LeftBrace [24..25)
         |Ident(b) [32..33)
         |LF [33..34)
         |RightBrace [38..39)
         |KwElse [40..44)
         |LeftBrace [45..46)
         |Ident(c) [53..54)
         |LF [54..55)
         |RightBrace [59..60)
         |Indentation.Outdent [61..61)
         |LFLF [61..62)
         |Underscore [64..65)
         |LeftArrow [66..68)
         |Ident(d) [69..70)
         |LF [70..71)
         |RightBrace [71..72)
         |KwYield [73..78)
         |Ident(e) [79..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])

    val layout =
      """|for (_ <- if (a) {
         |  b
         |} else {
         |  c
         |}; _ <- d) yield e
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(
        Enumerator.Generator(patwildcard, Term.If(tname("a"), blk(tname("b")), blk(tname("c")), Nil)),
        Enumerator.Generator(patwildcard, tname("d")),
      )),
      tname("e"),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3979 w/ granular whitespace") {
    @nowarn("msg=never used|unused") // runTestAssert does not read it
    implicit def tokenizerOptions: TokenizerOptions = new TokenizerOptions(groupWhitespace = false)

    val code =
      """|for {
         |  _ <-
         |    if (a) {
         |      b
         |    } else {
         |      c
         |    }
         |
         |  _ <- d
         |} yield e
         |""".stripMargin

    val struct =
      """|BOF [0..0)
         |KwFor [0..3)
         |LeftBrace [4..5)
         |Underscore [8..9)
         |LeftArrow [10..12)
         |Indentation.Indent [12..12)
         |KwIf [17..19)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |LeftBrace [24..25)
         |Ident(b) [32..33)
         |LF [33..34)
         |RightBrace [38..39)
         |KwElse [40..44)
         |LeftBrace [45..46)
         |Ident(c) [53..54)
         |LF [54..55)
         |RightBrace [59..60)
         |Indentation.Outdent [61..61)
         |LFLF [61..62)
         |Underscore [64..65)
         |LeftArrow [66..68)
         |Ident(d) [69..70)
         |LF [70..71)
         |RightBrace [71..72)
         |KwYield [73..78)
         |Ident(e) [79..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])

    val layout =
      """|for (_ <- if (a) {
         |  b
         |} else {
         |  c
         |}; _ <- d) yield e
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(
        Enumerator.Generator(patwildcard, Term.If(tname("a"), blk(tname("b")), blk(tname("c")), Nil)),
        Enumerator.Generator(patwildcard, tname("d")),
      )),
      tname("e"),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3979 w/ grouped whitespace in the input") {
    val code =
      """|for {
         |  _ <-
         |    if (a) {
         |      b
         |    } else {
         |      c
         |    }
         |
         |  _ <- d
         |} yield e
         |""".stripMargin
    val options = new TokenizerOptions(groupWhitespace = true)
    val tokenize: TestHelpers.Tokenize = new TestHelpers.Tokenize {
      override def apply(code: String)(implicit dialect: Dialect): Iterable[Token] = {
        val ti = LazyTokenIterator(
          ScannerTokens(Input.String(code).withTokenizerOptions(options), classOf[Stat]),
        )
        val tokens = List.newBuilder[Token]
        while (ti.hasCurr) {
          tokens += ti.currToken
          ti.next()
        }
        tokens.result()
      }
    }

    val struct =
      """|BOF [0..0)
         |KwFor [0..3)
         |LeftBrace [4..5)
         |Underscore [8..9)
         |LeftArrow [10..12)
         |Indentation.Indent [12..12)
         |KwIf [17..19)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |LeftBrace [24..25)
         |Ident(b) [32..33)
         |LF [33..34)
         |RightBrace [38..39)
         |KwElse [40..44)
         |LeftBrace [45..46)
         |Ident(c) [53..54)
         |LF [54..55)
         |RightBrace [59..60)
         |Indentation.Outdent [60..60)
         |MultiNL(2) [60..62)
         |Underscore [64..65)
         |LeftArrow [66..68)
         |Ident(d) [69..70)
         |LF [70..71)
         |RightBrace [71..72)
         |KwYield [73..78)
         |Ident(e) [79..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, dialect)(implicitly, tokenize)
  }

  test("#4008") {
    val code =
      """|for
         |    _ <- Option(42).map: _ =>
         |      ???
         |
         |    _ <- Option(43)
         |yield ()
         |
         |""".stripMargin

    val struct =
      """|BOF [0..0)
         |KwFor [0..3)
         |Indentation.Indent [3..3)
         |Underscore [8..9)
         |LeftArrow [10..12)
         |Ident(Option) [13..19)
         |LeftParen [19..20)
         |Constant.Int(42) [20..22)
         |RightParen [22..23)
         |Dot [23..24)
         |Ident(map) [24..27)
         |Colon [27..28)
         |Underscore [29..30)
         |RightArrow [31..33)
         |Indentation.Indent [33..33)
         |Ident(???) [40..43)
         |Indentation.Outdent [44..44)
         |LFLF [44..45)
         |Underscore [49..50)
         |LeftArrow [51..53)
         |Ident(Option) [54..60)
         |LeftParen [60..61)
         |Constant.Int(43) [61..63)
         |RightParen [63..64)
         |Indentation.Outdent [64..64)
         |KwYield [65..70)
         |LeftParen [71..72)
         |RightParen [72..73)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])

    val layout =
      """|for (_ <- Option(42).map {
         |  _ => ???
         |}; _ <- Option(43)) yield ()
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(
        Enumerator.Generator(
          patwildcard,
          tapply(
            tselect(tapply(tname("Option"), lit(42)), "map"),
            blk(tfunc(tparam("_"))(tname("???"))),
          ),
        ),
        Enumerator.Generator(patwildcard, tapply(tname("Option"), lit(43))),
      )),
      Lit.Unit(),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4177") {
    val code =
      """|val foo = test(
         |  try 1
         |  catch case _ => 2,
         |  "bar"
         |)
         |""".stripMargin
    val layout =
      """|val foo = test(try 1 catch {
         |  case _ => 2
         |}, "bar")
         |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(patvar("foo")),
      None,
      tapply(
        "test",
        Term.Try(lit(1), Some(Term.CasesBlock(List(Case(patwildcard, None, lit(2))))), None),
        lit("bar"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4214") {
    val code =
      """|package object scope:
         |  val result = for
         |    x <- twice:
         |         "4".toInt
         |  yield x
         |""".stripMargin
    val output =
      """|package object scope {
         |  val result = for (x <- twice {
         |    "4".toInt
         |  }) yield x
         |}
         |""".stripMargin
    val tree = Source(List(Pkg.Object(
      Nil,
      "scope",
      tpl(Defn.Val(
        Nil,
        List(patvar("result")),
        None,
        Term.ForYield(
          Term.EnumeratorsBlock(List(
            Enumerator.Generator(patvar("x"), tapply("twice", blk(tselect(lit("4"), "toInt")))),
          )),
          tname("x"),
        ),
      )),
    )))
    runTestAssert[Source](code, output)(tree)
  }

  test("#4421 unindented") {
    val code =
      """|for
         |given Foo <- frob
         |yield
         |  identity:
         |    sql""
         |""".stripMargin
    val layout =
      """|for (given Foo <- frob) yield identity {
         |  sql""
         |}
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(Enumerator.Generator(Pat.Given("Foo"), "frob"))),
      tapply("identity", blk(Term.Interpolate("sql", List(lit("")), Nil))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4421 indented") {
    val code =
      """|for
         |  given Foo <- frob
         |yield
         |  identity:
         |    sql""
         |""".stripMargin
    val layout =
      """|for (given Foo <- frob) yield identity {
         |  sql""
         |}
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(Enumerator.Generator(Pat.Given("Foo"), "frob"))),
      tapply("identity", blk(Term.Interpolate("sql", List(lit("")), Nil))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4421 in braces") {
    val code =
      """|for {
         |  given Foo <- frob
         |}
         |yield
         |  identity:
         |    sql""
         |""".stripMargin
    val layout =
      """|for (given Foo <- frob) yield identity {
         |  sql""
         |}
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(Enumerator.Generator(Pat.Given("Foo"), "frob"))),
      tapply("identity", blk(Term.Interpolate("sql", List(lit("")), Nil))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4421 in parens") {
    val code =
      """|for (
         |  given Foo <- frob
         |)
         |yield
         |  identity:
         |    sql""
         |""".stripMargin
    val layout =
      """|for (given Foo <- frob) yield identity {
         |  sql""
         |}
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(Enumerator.Generator(Pat.Given("Foo"), "frob"))),
      tapply("identity", blk(Term.Interpolate("sql", List(lit("")), Nil))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4515") {
    val code = "def f(b: Boolean): Int => Int = if (b) (a: Int) => a else identity"
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      List(List(tparam("b", "Boolean"))),
      Some(pfunc("Int")("Int")),
      Term.If("b", tfunc(tparam("a", "Int"))("a"), "identity", Nil),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("#4664 if") {
    val code = "if (a, b) == pair then ???"
    val layout = "if ((a, b) == pair) ???"
    val tree = Term.If(tinfix(Term.Tuple(List("a", "b")), "==", "pair"), "???", Lit.Unit(), Nil)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4664 while") {
    val code = "while (a, b) == pair do ???"
    val layout = "while ((a, b) == pair) ???"
    val tree = Term.While(tinfix(Term.Tuple(List("a", "b")), "==", "pair"), "???")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an alphanumeric operator after the parentheses, `then` on the same line") {
    val code = "def f = if (a) and b then c else d"
    val layout = "def f = if (a and b) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tname("a"), "and", tname("b")), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a block after the parentheses, `then` on the same line") {
    val code = "def f = if (a) {b} then c else d"
    val layout =
      """|def f = if (a {
         |  b
         |}) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tapply(tname("a"), blk(tname("b"))), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: type arguments after the parentheses, `then` on the same line") {
    val code = "def f = if (a)[Int] then c else d"
    val layout = "def f = if (a[Int]) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(Term.ApplyType(tname("a"), Type.ArgClause(List(pname("Int")))), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an argument in parentheses after the parentheses, `then` on the same line") {
    val code = "def f = if (a) (b) then c else d"
    val layout = "def f = if (a(b)) c else d"
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tif(tapply(tname("a"), tname("b")), tname("c"), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an underscore after the parentheses, `then` on the same line") {
    val code = "def f = if (a) _ then c else d"
    val layout = "def f = if (a _) c else d"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(Term.Eta(tname("a")), tname("c"), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `match` after the parentheses, `then` on the same line") {
    val code = "def f = if (a) match { case _ => true } then c else d"
    val layout =
      """|def f = if (a match {
         |  case _ => true
         |}) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tmatch(tname("a"), Case(patwildcard, None, lit(true))), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an assignment after the parentheses, `then` on the same line") {
    val code = "def f = if (a) = b then c else d"
    val layout = "def f = if (a = b) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(Term.Assign(tname("a"), tname("b")), tname("c"), tname("d")),
    )
    // the layout reprints as `if ((a = b))`
    parseAndCheckTree[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an ascription after the parentheses, `then` on the same line") {
    val code = "def f = if (a): Boolean then c else d"
    val layout = "def f = if (a: Boolean) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(Term.Ascribe(tname("a"), pname("Boolean")), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator and a `match` after the parentheses, `then` on the same line") {
    val code = "def f = if (a) && b match { case _ => true } then c else d"
    val error =
      """|<input>:1: error: `;` expected but `then` found
         |def f = if (a) && b match { case _ => true } then c else d
         |                                             ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "parenthesized condition: an operator and `new` after the parentheses, `then` on the same line",
  ) {
    val code = "def f = if (a) && new B().c then c else d"
    val error =
      """|<input>:1: error: `;` expected but `new` found
         |def f = if (a) && new B().c then c else d
         |                  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: an identifier after the parentheses, `else` on the same line") {
    val code = "def f = if (a) b else d"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(tname("a"), tname("b"), tname("d")))
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: a polymorphic lambda after the parentheses") {
    val code = "def f = if (a) [T] => (x: T) => x else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tname("a"), tpolyfunc(pparam("T"))(tfunc(tparam("x", pname("T")))(tname("x"))), tname("d")),
    )
    runTestAssert[Stat](code)(tree)
  }

  test(
    "parenthesized condition: a term in parentheses after the parentheses, `else` on the same line",
  ) {
    val code = "def f = if (a) (b) else d"
    val layout = "def f = if (a) b else d"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(tname("a"), tname("b"), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a placeholder lambda after the parentheses") {
    val code = "val f: Int => Int = if (a) _ + 1 else _ - 1"
    val tree = Defn.Val(
      Nil,
      List(patvar("f")),
      Some(Type.Function(Type.FuncParamClause(List(pname("Int"))), pname("Int"))),
      tif(
        tname("a"),
        Term.AnonymousFunction(tinfix(Term.Placeholder(), "+", lit(1))),
        Term.AnonymousFunction(tinfix(Term.Placeholder(), "-", lit(1))),
      ),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: a literal after the parentheses") {
    val code = "def f = if (a) 1 else d"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(tname("a"), lit(1), tname("d")))
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: `new` after the parentheses") {
    val code = "def f = if (a) new B else d"
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tif(tname("a"), Term.New(init(pname("B"))), tname("d")))
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: an argument with `if then` after the parentheses") {
    val code = "def f = if (a) b(if c then d else e) else d"
    val layout = "def f = if (a) b(if (c) d else e) else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tname("a"), tapply(tname("b"), tif(tname("c"), tname("d"), tname("e"))), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `if then` after the parentheses") {
    val code = "def f = if (a) if c then d else e"
    val layout = "def f = if (a) if (c) d else e"
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tif(tname("a"), tif(tname("c"), tname("d"), tname("e"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a `match` in braces after an identifier after the parentheses") {
    val code = "def f = if (a) b match { case _ => c }"
    val layout =
      """|def f = if (a) b match {
         |  case _ => c
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tname("a"), tmatch(tname("b"), Case(patwildcard, None, tname("c")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, deeper leading infix and `then`") {
    val code =
      """|def f = if (a) && b
         |    && c then d else e
         |""".stripMargin
    val layout = "def f = if (a && b && c) d else e"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, `then` on the next line") {
    val code =
      """|def f = if (a) && b
         |  then c else e
         |""".stripMargin
    val layout = "def f = if (a && b) c else e"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: the parentheses at a line end, deeper leading infix and `then`") {
    val code =
      """|def f = if (a)
         |    && b then c else e
         |""".stripMargin
    val layout = "def f = if (a && b) c else e"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, deeper leading infix, next statement") {
    val code =
      """|def f =
         |  if (a) b
         |    + c
         |  d
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) b + c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tif(tname("a"), tinfix(tname("b"), "+", tname("c"))), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, next statement") {
    val code =
      """|def f =
         |  if (a) b
         |  c
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) b
         |  c
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "parenthesized condition: `while` with an operator after the parentheses, `do` on the next line",
  ) {
    val code =
      """|def f = while (a) && b
         |  do c
         |""".stripMargin
    val layout = "def f = while (a && b) c"
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "parenthesized condition: an identifier after the parentheses inside an indented condition",
  ) {
    val code =
      """|def f =
         |  if
         |    if (a) b
         |  then c
         |  else d
         |""".stripMargin
    val layout = "def f = if (if (a) b) c else d"
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tif(tif(tname("a"), tname("b")), tname("c"), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, `then` on the next line") {
    val code =
      """|def f =
         |  if (a) b
         |  then c
         |""".stripMargin
    val layout = "def f = if (a b) c"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(tpostfix(tname("a"), "b"), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, deeper select and `then`") {
    val code =
      """|def f = if (a) && b
         |    .c then d else e
         |""".stripMargin
    val layout = "def f = if (a && b.c) d else e"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tname("a"), "&&", tselect("b", "c")), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "parenthesized condition: the parentheses at a line end, deeper prefix operator, next statement",
  ) {
    val code =
      """|def f =
         |  if (a)
         |    + b
         |  c
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) +b
         |  c
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tif(tname("a"), Term.ApplyUnary(tname("+"), tname("b"))), tname("c")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator and an indented `match` after the parentheses, `then` on a later line") {
    val code =
      """|def f =
         |  if (a) && b match
         |    case 1 => true
         |    case _ => false
         |  then c
         |  else d
         |""".stripMargin
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |  then c
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: an operator after the parentheses, trailing operator, `then` on the next line") {
    val code =
      """|def f =
         |  if (a) && b ||
         |    c then d
         |  else e
         |""".stripMargin
    val layout = "def f = if (a && b || c) d else e"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator and a colon lambda after the parentheses, `then` on a later line") {
    val code =
      """|def f =
         |  if (a) && xs.exists: x =>
         |    x > 0
         |  then c
         |  else d
         |""".stripMargin
    val layout =
      """|def f = if (a && xs.exists {
         |  x => x > 0
         |}) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(
          tname("a"),
          "&&",
          tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0))))),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses in braces without indentation") {
    val code =
      """|def f = {
         |if (a) b
         |c
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) b
         |  c
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "parenthesized condition: an operator and an interpolation with `if then` after the parentheses",
  ) {
    val code = "def f = if (a) && s\"${if b then c else d}\".isEmpty then e else g"
    val layout = "def f = if (a && s\"${if (b) c else d}\".isEmpty) e else g"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(
          tname("a"),
          "&&",
          tselect(
            Term.Interpolate(
              tname("s"),
              List(Lit.String(""), Lit.String("")),
              List(blk(tif(tname("b"), tname("c"), tname("d")))),
            ),
            "isEmpty",
          ),
        ),
        tname("e"),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a colon lambda after the parentheses, next statement") {
    val code =
      """|def f =
         |  if (a) xs.foreach: x =>
         |    x + 1
         |  c
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) xs.foreach {
         |    x => x + 1
         |  }
         |  c
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(
          tname("a"),
          tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tinfix(tname("x"), "+", lit(1))))),
        ),
        tname("c"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "parenthesized condition: an operator after the parentheses in braces, `then` on the next line",
  ) {
    val code =
      """|def f = {
         |  if (a) && b
         |  then c
         |  d
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tif(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an operator after the parentheses, deeper leading infix, `do` on the next line") {
    val code =
      """|def f =
         |  while (a) && b
         |    || c
         |  do d
         |""".stripMargin
    val layout = "def f = while (a && b || c) d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, `else` on the next line") {
    val code =
      """|def f =
         |  if (a) b
         |  else c
         |""".stripMargin
    val layout = "def f = if (a) b else c"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(tname("a"), tname("b"), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with a term in parentheses after the parentheses") {
    val code = "def f = while (a) (b)"
    val layout = "def f = while (a) b"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.While(tname("a"), tname("b")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with a select after the parentheses, `do` on the same line") {
    val code = "def f = while (a).b do c"
    val layout = "def f = while (a.b) c"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.While(tselect("a", "b"), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an identifier and an argument after the parentheses, `do` on the same line") {
    val code = "def f = while (a) b(c) do d"
    val layout = "def f = while (a b c) d"
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, Term.While(tinfix(tname("a"), "b", tname("c")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses in an argument") {
    val code = "def f = g(if (a) b else c, d)"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tname("g"), tif(tname("a"), tname("b"), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: an operator after the parentheses in an argument") {
    val code = "def f = g(if (a) && b then c else d, e)"
    val layout = "def f = g(if (a && b) c else d, e)"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tname("g"),
        tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")),
        tname("e"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses in a lambda in parentheses") {
    val code = "def f = xs.map(x => if (a) b else c)"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), tfunc(tparam("x"))(tif(tname("a"), tname("b"), tname("c")))),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: an operator after the parentheses in a lambda in braces") {
    val code = "def f = xs.map { x => if (a) && b then c else d }"
    val layout =
      """|def f = xs.map {
         |  x => if (a && b) c else d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses in a lambda in braces") {
    val code = "def f = xs.map { x => if (a) b }"
    val layout =
      """|def f = xs.map {
         |  x => if (a) b
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(tname("a"), tname("b"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses in braces, next statement") {
    val code =
      """|def f = {
         |  if (a) b
         |  c
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("c")))
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: an operator after the parentheses in braces, next statement") {
    val code =
      """|def f = {
         |  if (a) && b then c
         |  d
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tif(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses in a sub-case guard") {
    val code =
      """|def f = d match
         |  case Some(m) if cond if (m) || n match case c: T => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f = d match {
         |  case Some(m) if cond if m || n match {
         |    case c: T => 1
         |  }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("d"),
        Case(
          patextract(tname("Some"), patvar("m")),
          Some(tname("cond")),
          tsubmatch(
            tinfix(tname("m"), "||", tname("n")),
            Case(Pat.Typed(patvar("c"), pname("T")), None, lit(1)),
          ),
        ),
        Case(patwildcard, None, lit(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `match` after the parentheses in a sub-case guard") {
    val code =
      """|def f = d match
         |  case Some(m) if cond if (m) match case c: T => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f = d match {
         |  case Some(m) if cond if m match {
         |    case c: T => 1
         |  }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("d"),
        Case(
          patextract(tname("Some"), patvar("m")),
          Some(tname("cond")),
          tsubmatch(tname("m"), Case(Pat.Typed(patvar("c"), pname("T")), None, lit(1))),
        ),
        Case(patwildcard, None, lit(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses in a sub-case guard, deeper case") {
    val code =
      """|def f = d match
         |  case Some(m) if cond if (m) || n match case c: T => 1
         |    case _ => 2
         |  case _ => 3
         |""".stripMargin
    val layout =
      """|def f = d match {
         |  case Some(m) if cond if m || n match {
         |    case c: T => 1
         |    case _ => 2
         |  }
         |  case _ =>
         |    3
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("d"),
        Case(
          patextract(tname("Some"), patvar("m")),
          Some(tname("cond")),
          tsubmatch(
            tinfix(tname("m"), "||", tname("n")),
            Case(Pat.Typed(patvar("c"), pname("T")), None, lit(1)),
            Case(patwildcard, None, lit(2)),
          ),
        ),
        Case(patwildcard, None, lit(3)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses in a guard") {
    val code =
      """|def f = d match
         |  case Some(m) if (m) || n match case c: T => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f = d match {
         |  case Some(m) if m || n match {
         |    case c: T => 1
         |  }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("d"),
        Case(
          patextract(tname("Some"), patvar("m")),
          None,
          tsubmatch(
            tinfix(tname("m"), "||", tname("n")),
            Case(Pat.Typed(patvar("c"), pname("T")), None, lit(1)),
          ),
        ),
        Case(patwildcard, None, lit(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses in a sub-case guard, cases in braces") {
    val code =
      """|def f = d match {
         |  case Some(m) if cond if (m) || n match case c: T => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val layout =
      """|def f = d match {
         |  case Some(m) if cond if m || n match {
         |    case c: T => 1
         |  }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("d"),
        Case(
          patextract(tname("Some"), patvar("m")),
          Some(tname("cond")),
          tsubmatch(
            tinfix(tname("m"), "||", tname("n")),
            Case(Pat.Typed(patvar("c"), pname("T")), None, lit(1)),
          ),
        ),
        Case(patwildcard, None, lit(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an identifier after the parentheses, `do` on the next line") {
    val code =
      """|def f =
         |  while (a) b
         |  do c
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `do` found
         |  do c
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: `if then` and `match` in a sub-case guard") {
    val code =
      """|def f = d match
         |  case Some(m) if cond if c then d match case c: T => 1
         |  case _ => 2
         |""".stripMargin
    val error =
      """|<input>:2: error: `match` expected
         |  case Some(m) if cond if c then d match case c: T => 1
         |                          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: without quiet syntax, an identifier after the parentheses") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowQuietSyntax(false)
    val code = "def f = if (a) b else d"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(tname("a"), tname("b"), tname("d")))
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: without quiet syntax, an operator after the parentheses, `then` on the same line") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowQuietSyntax(false)
    val code = "def f = if (a) && b then c else d"
    val layout = "def f = if (a) (&& b then) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tname("a"), tpostfix(tinfix(tname("&&"), "b", tname("then")), "c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: without quiet syntax, an identifier after the parentheses in braces, next statement") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowQuietSyntax(false)
    val code =
      """|def f = {
         |  if (a) b
         |  c
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("c")))
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: without quiet syntax, a block after the parentheses") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowQuietSyntax(false)
    val code = "def f = if (a) { b } else d"
    val layout =
      """|def f = if (a) {
         |  b
         |} else d
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(tname("a"), blk(tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: without quiet syntax, `while` with an identifier after the parentheses") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowQuietSyntax(false)
    val code = "def f = while (a) b"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.While(tname("a"), tname("b")))
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, deeper argument in parentheses, `then` on a later line") {
    val code =
      """|def f =
         |  if (a) && xs.exists
         |      (x => x > 0)
         |    then c
         |  else d
         |""".stripMargin
    val layout = "def f = if (a && xs.exists(x => x > 0)) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(
          tname("a"),
          "&&",
          tapply(tselect("xs", "exists"), tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0)))),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, deeper argument in braces, `then` on a later line") {
    val code =
      """|def f =
         |  if (a) && xs.exists
         |      { x => x > 0 }
         |    then c
         |  else d
         |""".stripMargin
    val layout =
      """|def f = if (a && xs.exists {
         |  x => x > 0
         |}) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(
          tname("a"),
          "&&",
          tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0))))),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an operator after the parentheses, deeper argument in parentheses, `do` on a later line") {
    val code =
      """|def f =
         |  while (a) && xs.exists
         |      (x => x > 0)
         |    do c
         |""".stripMargin
    val layout = "def f = while (a && xs.exists(x => x > 0)) c"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.While(
        tinfix(
          tname("a"),
          "&&",
          tapply(tselect("xs", "exists"), tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0)))),
        ),
        tname("c"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "unparenthesized condition: an operator, deeper argument in parentheses, `then` on a later line",
  ) {
    val code =
      """|def f =
         |  if a && xs.exists
         |      (x => x > 0)
         |    then c
         |  else d
         |""".stripMargin
    val layout =
      """|def f = if (a && xs.exists(x => x > 0)) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix("a", "&&", tapply(tselect("xs", "exists"), tfunc(tparam("x"))(tinfix("x", ">", lit(0))))),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator and a colon lambda after the parentheses, `then` at the lambda body") {
    val code =
      """|def f =
         |  if (a) || xs.exists: x =>
         |    x > 0
         |    then c
         |  else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Ident(||) [17..19)
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(exists) [23..29)
         |Colon [29..30)
         |Ident(x) [31..32)
         |RightArrow [33..35)
         |Indentation.Indent [35..35)
         |Ident(x) [40..41)
         |Ident(>) [42..43)
         |Constant.Int(0) [44..45)
         |Indentation.Outdent [45..45)
         |KwThen [50..54)
         |Ident(c) [55..56)
         |KwElse [59..63)
         |Ident(d) [64..65)
         |Indentation.Outdent [65..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if (a || xs.exists {
         |  x => x > 0
         |}) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(
          tname("a"),
          "||",
          tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0))))),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator and a colon lambda with `if then` after the parentheses, `then` at the lambda body") {
    val code =
      """|def f =
         |  if (a) || xs.exists: x =>
         |    if x then true else false
         |    then c
         |  else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Ident(||) [17..19)
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(exists) [23..29)
         |Colon [29..30)
         |Ident(x) [31..32)
         |RightArrow [33..35)
         |Indentation.Indent [35..35)
         |KwIf [40..42)
         |Ident(x) [43..44)
         |KwThen [45..49)
         |KwTrue [50..54)
         |KwElse [55..59)
         |KwFalse [60..65)
         |KwThen [70..74)
         |Ident(c) [75..76)
         |Indentation.Outdent [76..76)
         |KwElse [79..83)
         |Ident(d) [84..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: `outdent` expected but `.` found
         |  if (a) || xs.exists: x =>
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: `while` with an operator and a colon lambda after the parentheses, `do` at the lambda body") {
    val code =
      """|def f =
         |  while (a) || xs.exists: x =>
         |    x > 0
         |    do c
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |LeftParen [16..17)
         |Ident(a) [17..18)
         |RightParen [18..19)
         |Ident(||) [20..22)
         |Ident(xs) [23..25)
         |Dot [25..26)
         |Ident(exists) [26..32)
         |Colon [32..33)
         |Ident(x) [34..35)
         |RightArrow [36..38)
         |Indentation.Indent [38..38)
         |Ident(x) [43..44)
         |Ident(>) [45..46)
         |Constant.Int(0) [47..48)
         |LF [48..49)
         |Indentation.Outdent [48..48)
         |KwDo [53..55)
         |Ident(c) [56..57)
         |Indentation.Outdent [57..57)
         |EOF [58..58)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = while (a || xs.exists {
         |  x => x > 0
         |}) c
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.While(
        tinfix(
          tname("a"),
          "||",
          tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0))))),
        ),
        tname("c"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an operator and a colon lambda with `for do` after the parentheses, `do` at the lambda body") {
    val code =
      """|def f =
         |  while (a) || xs.exists: x =>
         |    for y <- x do println(y)
         |    true
         |    do c
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |LeftParen [16..17)
         |Ident(a) [17..18)
         |RightParen [18..19)
         |Ident(||) [20..22)
         |Ident(xs) [23..25)
         |Dot [25..26)
         |Ident(exists) [26..32)
         |Colon [32..33)
         |Ident(x) [34..35)
         |RightArrow [36..38)
         |Indentation.Indent [38..38)
         |KwFor [43..46)
         |Ident(y) [47..48)
         |LeftArrow [49..51)
         |Ident(x) [52..53)
         |KwDo [54..56)
         |Ident(println) [57..64)
         |LeftParen [64..65)
         |Ident(y) [65..66)
         |RightParen [66..67)
         |LF [67..68)
         |KwTrue [72..76)
         |LF [76..77)
         |KwDo [81..83)
         |Ident(c) [84..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: `outdent` expected but `.` found
         |  while (a) || xs.exists: x =>
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: an operator and a colon lambda with `;` after the parentheses, `then` at the lambda body") {
    val code =
      """|def f =
         |  if (a) || xs.exists: x =>
         |    val y = x; y > 0
         |    then c
         |  else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Ident(||) [17..19)
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(exists) [23..29)
         |Colon [29..30)
         |Ident(x) [31..32)
         |RightArrow [33..35)
         |Indentation.Indent [35..35)
         |KwVal [40..43)
         |Ident(y) [44..45)
         |Equals [46..47)
         |Ident(x) [48..49)
         |Semicolon [49..50)
         |Ident(y) [51..52)
         |Ident(>) [53..54)
         |Constant.Int(0) [55..56)
         |KwThen [61..65)
         |Ident(c) [66..67)
         |Indentation.Outdent [67..67)
         |KwElse [70..74)
         |Ident(d) [75..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: `outdent` expected but `.` found
         |  if (a) || xs.exists: x =>
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: an operator after the parentheses in parentheses, leading infix at the outer column") {
    val code =
      """|val x = (if (a) && b
         |max c then d else e)
         |""".stripMargin
    val layout = "val x = if (a && b max c) d else e"
    val tree = Defn.Val(
      Nil,
      List(patvar("x")),
      None,
      tif(tinfix(tinfix(tname("a"), "&&", tname("b")), "max", tname("c")), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator and an indented `match` after the parentheses, `then` at the `if`") {
    val code =
      """|def f =
         |  if (a) || x match
         |    case y => y
         |  then c
         |  else d
         |""".stripMargin
    val error =
      """|<input>:4: error: `outdent` expected but `then` found
         |  then c
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: an operator and a colon lambda after the parentheses, select and `then` on a later line") {
    val code =
      """|def f =
         |  if (a) || x.map: y =>
         |      y
         |    .isEmpty then c
         |  else d
         |""".stripMargin
    val layout =
      """|def f = if (a || x.map {
         |  y => y
         |}.isEmpty) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(
          tname("a"),
          "||",
          tselect(tapply(tselect("x", "map"), blk(tfunc(tparam("y"))(tname("y")))), "isEmpty"),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, `end if` on the next line") {
    val code =
      """|def f =
         |  if (a) || b
         |  end if
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) ||b
         |
         |  end if
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tif(tname("a"), tpostfix(tname("||"), "b")), Term.EndMarker(tname("if"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, `end if` on the next line") {
    val code =
      """|def f =
         |  if (a) b
         |  end if
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) b
         |  end if
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), Term.EndMarker(tname("if"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator and XML after the parentheses, `then` on the same line") {
    val code = "def f = if (a) || <x/>.isEmpty then c else d"
    val error =
      """|<input>:1: error: `;` expected but `xml start` found
         |def f = if (a) || <x/>.isEmpty then c else d
         |                  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: an assignment and a colon lambda after the parentheses, `then` at the lambda body") {
    val code =
      """|def f =
         |  if (x) = xs.exists: y =>
         |    y > 0
         |    then c
         |  else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(x) [14..15)
         |RightParen [15..16)
         |Equals [17..18)
         |Ident(xs) [19..21)
         |Dot [21..22)
         |Ident(exists) [22..28)
         |Colon [28..29)
         |Ident(y) [30..31)
         |RightArrow [32..34)
         |Indentation.Indent [34..34)
         |Ident(y) [39..40)
         |Ident(>) [41..42)
         |Constant.Int(0) [43..44)
         |Indentation.Outdent [44..44)
         |KwThen [49..53)
         |Ident(c) [54..55)
         |KwElse [58..62)
         |Ident(d) [63..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if (x = xs.exists {
         |  y => y > 0
         |}) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        Term.Assign(
          tname("x"),
          tapply(tselect("xs", "exists"), blk(tfunc(tparam("y"))(tinfix("y", ">", lit(0))))),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    parseAndCheckTree[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an ascription after the parentheses, `then` on the next line") {
    val code =
      """|def f =
         |  if (x): Boolean
         |  then c
         |  else d
         |""".stripMargin
    val layout = "def f = if (x: Boolean) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(Term.Ascribe(tname("x"), pname("Boolean")), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with `for do` after the parentheses") {
    val code = "def f = while (a) for x <- xs do f(x)"
    val layout = "def f = while (a) for (x <- xs) f(x)"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.While(
        tname("a"),
        Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tapply(tname("f"), tname("x"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with `while do` after the parentheses") {
    val code = "def f = while (a) while b do c"
    val layout = "def f = while (a) while (b) c"
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, Term.While(tname("a"), Term.While(tname("b"), tname("c"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, leading infix at the `if`") {
    val code =
      """|def f =
         |  if (a) && b
         |  || c then d
         |  else e
         |""".stripMargin
    val layout = "def f = if (a && b || c) d else e"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an operator after the parentheses, leading infix at the `while`") {
    val code =
      """|def f =
         |  while (a) && b
         |  || c do d
         |""".stripMargin
    val layout = "def f = while (a && b || c) d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "parenthesized condition: an operator after the parentheses, blank line, deeper leading infix",
  ) {
    val code =
      """|def f =
         |  if (a) && b
         |
         |    || c then d
         |  else e
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Ident(&&) [17..19)
         |Ident(b) [20..21)
         |Ident(||) [27..29)
         |Ident(c) [30..31)
         |KwThen [32..36)
         |Ident(d) [37..38)
         |KwElse [41..45)
         |Ident(e) [46..47)
         |Indentation.Outdent [47..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if (a && b || c) d else e
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, blank line, `then`") {
    val code =
      """|def f =
         |  if (a) && b
         |
         |  then c
         |  else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Ident(&&) [17..19)
         |Ident(b) [20..21)
         |KwThen [25..29)
         |Ident(c) [30..31)
         |KwElse [34..38)
         |Ident(d) [39..40)
         |Indentation.Outdent [40..40)
         |EOF [41..41)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout = "def f = if (a && b) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, deeper identifier, `then` on a later line") {
    val code =
      """|def f =
         |  if (a) && b
         |      c
         |    then d
         |  else e
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Ident(&&) [17..19)
         |Ident(b) [20..21)
         |Ident(c) [28..29)
         |KwThen [34..38)
         |Ident(d) [39..40)
         |KwElse [43..47)
         |Ident(e) [48..49)
         |Indentation.Outdent [49..49)
         |EOF [50..50)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if ((a && b) c) d else e
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tpostfix(tinfix(tname("a"), "&&", tname("b")), "c"), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, `;` and next statement") {
    val code =
      """|def f =
         |  if (a) b; c
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) b
         |  c
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an identifier after the parentheses, `;` and next statement") {
    val code =
      """|def f =
         |  while (a) b; c
         |""".stripMargin
    val layout =
      """|def f = {
         |  while (a) b
         |  c
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.While(tname("a"), tname("b")), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, `;` and next statement") {
    val code =
      """|def f =
         |  if (a) && b; c
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) &&b
         |
         |  c
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tpostfix(tname("&&"), "b")), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator and a colon lambda with `if then` after the parentheses, `then` at the `if`") {
    val code =
      """|def f =
         |  if (a) && xs.exists: x =>
         |    if x then true else false
         |  then c
         |  else d
         |""".stripMargin
    val error =
      """|<input>:2: error: `outdent` expected but `.` found
         |  if (a) && xs.exists: x =>
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: an operator and a colon lambda with two statements after the parentheses, `then` at the `if`") {
    val code =
      """|def f =
         |  if (a) && xs.exists: x =>
         |    val y = x
         |    y > 0
         |  then c
         |  else d
         |""".stripMargin
    val error =
      """|<input>:2: error: `outdent` expected but `.` found
         |  if (a) && xs.exists: x =>
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: an operator and a block with `if then` after the parentheses, `then` after the block") {
    val code =
      """|def f =
         |  if (a) && {
         |    if b then c else d
         |  } then e
         |  else g
         |""".stripMargin
    val layout =
      """|def f = if (a && {
         |  if (b) c else d
         |}) e else g
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(tname("a"), "&&", blk(tif(tname("b"), tname("c"), tname("d")))),
        tname("e"),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, single-line comment, `then` on the next line") {
    val code =
      """|def f =
         |  if (a) b
         |  // comment
         |  then c
         |""".stripMargin
    val layout =
      """|def f = if (a b)
         |  // comment
         |  c
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tpostfix(tname("a"), "b"), tnameComments("c")("// comment")()),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an operator after the parentheses, trailing comment, `then` on the next line") {
    val code =
      """|def f =
         |  if (a) && b /* c */
         |  then d
         |""".stripMargin
    val layout =
      """|def f = if (a && b /* c */
         |) d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(tname("a"), "&&", tname("b")).toBuilder.endComment(Seq("/* c */")).result(),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a comment and an operator after the parentheses, `then` on the same line") {
    val code = "def f = if (a) /* c */ && b then d else e"
    val layout = "def f = if (a /* c */ && b) d else e"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tnameComments("a")()("/* c */"), "&&", tname("b")), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a single-line comment after the parentheses, deeper identifier, next statement") {
    val code =
      """|def f =
         |  if (a) // c
         |    b
         |  c
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) // c
         |    b
         |  c
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tnameComments("b")("// c")()), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a comment and an identifier after the parentheses, `else` on the same line") {
    val code = "def f = if (a) /* c */ b else d"
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tif(tname("a"), tnameComments("b")("/* c */")(), tname("d")))
    runTestAssert[Stat](code)(tree)
  }

  test(
    "parenthesized condition: a prefix operator after the parentheses, `else` on the same line",
  ) {
    val code = "def f = if (a) -b else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tname("a"), Term.ApplyUnary(tname("-"), tname("b")), tname("d")),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: a block after the parentheses, `else` on the same line") {
    val code = "def f = if (a) { b } else d"
    val layout =
      """|def f = if (a) {
         |  b
         |} else d
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(tname("a"), blk(tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "parenthesized condition: an ascription and `match` after the parentheses in a sub-case guard",
  ) {
    val code =
      """|def f = d match
         |  case Some(m) if cond if (m): T match case c: T => 1
         |  case _ => 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Ident(d) [8..9)
         |KwMatch [10..15)
         |Indentation.Indent [15..15)
         |KwCase [18..22)
         |Ident(Some) [23..27)
         |LeftParen [27..28)
         |Ident(m) [28..29)
         |RightParen [29..30)
         |KwIf [31..33)
         |Ident(cond) [34..38)
         |KwIf [39..41)
         |LeftParen [42..43)
         |Ident(m) [43..44)
         |RightParen [44..45)
         |Colon [45..46)
         |Ident(T) [47..48)
         |KwMatch [49..54)
         |Indentation.Indent [55..55)
         |KwCase [55..59)
         |Ident(c) [60..61)
         |Colon [61..62)
         |Ident(T) [63..64)
         |RightArrow [65..67)
         |Constant.Int(1) [68..69)
         |Indentation.Outdent [69..69)
         |KwCase [72..76)
         |Underscore [77..78)
         |RightArrow [79..81)
         |Constant.Int(2) [82..83)
         |Indentation.Outdent [83..83)
         |EOF [84..84)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: `match` expected
         |  case Some(m) if cond if (m): T match case c: T => 1
         |                           ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("tokens of `if then` and `match` in a sub-case guard") {
    val code =
      """|def f = d match
         |  case Some(m) if cond if c then d match case c: T => 1
         |  case _ => 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Ident(d) [8..9)
         |KwMatch [10..15)
         |Indentation.Indent [15..15)
         |KwCase [18..22)
         |Ident(Some) [23..27)
         |LeftParen [27..28)
         |Ident(m) [28..29)
         |RightParen [29..30)
         |KwIf [31..33)
         |Ident(cond) [34..38)
         |KwIf [39..41)
         |Ident(c) [42..43)
         |KwThen [44..48)
         |Ident(d) [49..50)
         |KwMatch [51..56)
         |Indentation.Indent [57..57)
         |KwCase [57..61)
         |Ident(c) [62..63)
         |Colon [63..64)
         |Ident(T) [65..66)
         |RightArrow [67..69)
         |Constant.Int(1) [70..71)
         |Indentation.Outdent [71..71)
         |KwCase [74..78)
         |Underscore [79..80)
         |RightArrow [81..83)
         |Constant.Int(2) [84..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
  }

  test("parenthesized condition: without significant indentation, an operator after the parentheses, `then` on the same line") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code = "def f = if (a) && b then c else d"
    val layout = "def f = if (a && b) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "parenthesized condition: without significant indentation, an identifier after the parentheses",
  ) {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code = "def f = if (a) b else d"
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(tname("a"), tname("b"), tname("d")))
    runTestAssert[Stat](code)(tree)
  }

  test("parenthesized condition: without significant indentation, an operator after the parentheses, leading infix before the `if`") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f =
         |  if (a) && b
         ||| c then d
         |  else e
         |""".stripMargin
    val layout = "def f = if (a && b || c) d else e"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: without significant indentation, an operator after the parentheses, deeper leading infix") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f =
         |  if (a) && b
         |    || c then d
         |  else e
         |""".stripMargin
    val layout = "def f = if (a && b || c) d else e"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: without significant indentation, `while` with an operator after the parentheses") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code = "def f = while (a) && b do c"
    val layout = "def f = while (a && b) c"
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: without significant indentation, `while` with an operator after the parentheses, leading infix before the `while`") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f =
         |  while (a) && b
         ||| c do d
         |""".stripMargin
    val layout = "def f = while (a && b || c) d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: without significant indentation, a block after the parentheses") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code = "def f = if (a) { b } else d"
    val layout =
      """|def f = if (a) {
         |  b
         |} else d
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(tname("a"), blk(tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses in an argument of an unparenthesized condition") {
    val code =
      """|object A:
         |  def f = if foo(if (a) b) then c else d
         |""".stripMargin
    val layout = "object A { def f = if (foo(if (a) b)) c else d }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tapply(tname("foo"), tif(tname("a"), tname("b"))), tname("c"), tname("d")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an operator and an argument with `if then` after the parentheses, `then` on the same line") {
    val code =
      """|object A:
         |  def f = if (a) && f(if b then c else d) then e else g
         |""".stripMargin
    val error =
      """|<input>:2: error: `;` expected but `then` found
         |  def f = if (a) && f(if b then c else d) then e else g
         |                                          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("parenthesized condition: an identifier and `else` after the parentheses inside an indented condition") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) b else c
         |    then d
         |    else e
         |""".stripMargin
    val layout = "object A { def f = if (if (a) b else c) d else e }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tname("b"), tname("c")), tname("d"), tname("e")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an operator and `then` after the parentheses inside an indented condition") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b then c else d
         |    then e
         |    else g
         |""".stripMargin
    val layout = "object A { def f = if (if (a && b) c else d) e else g }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")), tname("e"), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: a block after the parentheses inside an indented condition, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) { b }
         |      c
         |    then d
         |    else e
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (if (a) {
         |    b
         |  } c) d else e
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tpostfix(blk(tname("b")), "c")), tname("d"), tname("e")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an identifier after the parentheses inside an indented condition, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      while (a) b
         |      c
         |    then d
         |    else e
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if ({
         |    while (a) b
         |    c
         |  }) d else e
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses inside an indented `while` condition, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if (a) b
         |      c
         |    do d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while ({
         |    if (a) b
         |    c
         |  }) d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(blk(tif(tname("a"), tname("b")), tname("c")), tname("d")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an identifier and `else` after the parentheses inside an indented `while` condition") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if (a) b else c
         |    do d
         |""".stripMargin
    val layout = "object A { def f = while (if (a) b else c) d }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(tif(tname("a"), tname("b"), tname("c")), tname("d")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an identifier after the parentheses inside an indented `while` condition, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) b
         |      c
         |    do d
         |""".stripMargin
    val layout =
      """|object A { def f = while (while (a) b c) d }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(Term.While(tname("a"), tpostfix(tname("b"), "c")), tname("d")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("tokens of an ascription after the parentheses, `then` on the same line") {
    val code =
      """|object A:
         |  def f = if (a): Boolean then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(a) [24..25)
         |RightParen [25..26)
         |Colon [26..27)
         |Ident(Boolean) [28..35)
         |KwThen [36..40)
         |Ident(c) [41..42)
         |KwElse [43..47)
         |Ident(d) [48..49)
         |Indentation.Outdent [49..49)
         |EOF [50..50)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("parenthesized condition: `while` with an ascription after the parentheses, `do` on the same line") {
    val code =
      """|object A:
         |  def f = while (a): Boolean do c
         |""".stripMargin
    val layout = "object A { def f = while (a: Boolean) c }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(Term.Ascribe(tname("a"), pname("Boolean")), tname("c")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("tokens of `while` with an ascription after the parentheses, `do` on the same line") {
    val code =
      """|object A:
         |  def f = while (a): Boolean do c
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwWhile [20..25)
         |LeftParen [26..27)
         |Ident(a) [27..28)
         |RightParen [28..29)
         |Colon [29..30)
         |Ident(Boolean) [31..38)
         |KwDo [39..41)
         |Ident(c) [42..43)
         |Indentation.Outdent [43..43)
         |EOF [44..44)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("parenthesized condition: `while` with an ascription after the parentheses, `do` on the next line") {
    val code =
      """|object A:
         |  def f = while (a): Boolean
         |    do c
         |""".stripMargin
    val layout = "object A { def f = while (a: Boolean) c }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(Term.Ascribe(tname("a"), pname("Boolean")), tname("c")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("tokens of `while` with an ascription after the parentheses, `do` on the next line") {
    val code =
      """|object A:
         |  def f = while (a): Boolean
         |    do c
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwWhile [20..25)
         |LeftParen [26..27)
         |Ident(a) [27..28)
         |RightParen [28..29)
         |Colon [29..30)
         |Ident(Boolean) [31..38)
         |KwDo [43..45)
         |Ident(c) [46..47)
         |Indentation.Outdent [47..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("parenthesized condition: `while` with an ascription after the parentheses, `do` at the line end, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while (a): Boolean do
         |      c
         |    d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    while (a: Boolean) c
         |    d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(Term.Ascribe(tname("a"), pname("Boolean")), tname("c")), tname("d")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("tokens of `while` with an ascription after the parentheses, `do` at the line end, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while (a): Boolean do
         |      c
         |    d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Colon [33..34)
         |Ident(Boolean) [35..42)
         |KwDo [43..45)
         |Indentation.Indent [45..45)
         |Ident(c) [52..53)
         |Indentation.Outdent [53..53)
         |LF [53..54)
         |Ident(d) [58..59)
         |Indentation.Outdent [59..59)
         |Indentation.Outdent [59..59)
         |EOF [60..60)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("parenthesized condition: `while` with an ascription after the parentheses, end of file") {
    val code =
      """|object A:
         |  def f = while (a): Boolean
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  def f = while (a): Boolean
         |                   ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("tokens of `while` with an ascription after the parentheses, end of file") {
    val code =
      """|object A:
         |  def f = while (a): Boolean
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwWhile [20..25)
         |LeftParen [26..27)
         |Ident(a) [27..28)
         |RightParen [28..29)
         |Colon [29..30)
         |Ident(Boolean) [31..38)
         |Indentation.Outdent [38..38)
         |EOF [39..39)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("unparenthesized condition: `new`, `then` and `else` on the same line, next statements") {
    val code =
      """|def f =
         |  if new B(p).ok then c else d
         |  g
         |  h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |KwNew [13..16)
         |Ident(B) [17..18)
         |LeftParen [18..19)
         |Ident(p) [19..20)
         |RightParen [20..21)
         |Dot [21..22)
         |Ident(ok) [22..24)
         |KwThen [25..29)
         |Ident(c) [30..31)
         |KwElse [32..36)
         |Ident(d) [37..38)
         |LF [38..39)
         |Ident(g) [41..42)
         |LF [42..43)
         |Ident(h) [45..46)
         |Indentation.Outdent [46..46)
         |EOF [47..47)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (new B(p).ok) c else d
         |  g
         |  h
         |}
         |""".stripMargin
    val cond = tselect(Term.New(init(pname("B"), List(tname("p")))), "ok")
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tif(cond, tname("c"), tname("d")), tname("g"), tname("h")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `while` with `new`, `do` on the same line, next statements") {
    val code =
      """|def f =
         |  while new B(p).ok do c
         |  g
         |  h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |KwNew [16..19)
         |Ident(B) [20..21)
         |LeftParen [21..22)
         |Ident(p) [22..23)
         |RightParen [23..24)
         |Dot [24..25)
         |Ident(ok) [25..27)
         |KwDo [28..30)
         |Ident(c) [31..32)
         |LF [32..33)
         |Ident(g) [35..36)
         |LF [36..37)
         |Ident(h) [39..40)
         |Indentation.Outdent [40..40)
         |EOF [41..41)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (new B(p).ok) c
         |  g
         |  h
         |}
         |""".stripMargin
    val cond = tselect(Term.New(init(pname("B"), List(tname("p")))), "ok")
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.While(cond, tname("c")), tname("g"), tname("h")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: an operator and `new`, `then` on the same line, no `else`, next statement") {
    val code =
      """|def f =
         |  if a && new B(p).ok then c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(a) [13..14)
         |Ident(&&) [15..17)
         |KwNew [18..21)
         |Ident(B) [22..23)
         |LeftParen [23..24)
         |Ident(p) [24..25)
         |RightParen [25..26)
         |Dot [26..27)
         |Ident(ok) [27..29)
         |KwThen [30..34)
         |Ident(c) [35..36)
         |LF [36..37)
         |Ident(g) [39..40)
         |Indentation.Outdent [40..40)
         |EOF [41..41)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a && new B(p).ok) c
         |  g
         |}
         |""".stripMargin
    val cond = tinfix(tname("a"), "&&", tselect(Term.New(init(pname("B"), List(tname("p")))), "ok"))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(cond, tname("c")), tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a select, an operator and `new` after the parentheses, `then` and `else` on the same line, next statement") {
    val code =
      """|def f =
         |  if (a).isEmpty || new B(p).ok then c else d
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Dot [16..17)
         |Ident(isEmpty) [17..24)
         |Ident(||) [25..27)
         |KwNew [28..31)
         |Ident(B) [32..33)
         |LeftParen [33..34)
         |Ident(p) [34..35)
         |RightParen [35..36)
         |Dot [36..37)
         |Ident(ok) [37..39)
         |KwThen [40..44)
         |Ident(c) [45..46)
         |KwElse [47..51)
         |Ident(d) [52..53)
         |LF [53..54)
         |Ident(g) [56..57)
         |Indentation.Outdent [57..57)
         |EOF [58..58)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a.isEmpty || new B(p).ok) c else d
         |  g
         |}
         |""".stripMargin
    val cond = tinfix(
      tselect(tname("a"), "isEmpty"),
      "||",
      tselect(Term.New(init(pname("B"), List(tname("p")))), "ok"),
    )
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(cond, tname("c"), tname("d")), tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `try` with `catch` in braces, `then` and `else` on the same line, next statement") {
    val code =
      """|def f =
         |  if try a catch { case _ => b } then c else d
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |KwTry [13..16)
         |Ident(a) [17..18)
         |KwCatch [19..24)
         |LeftBrace [25..26)
         |KwCase [27..31)
         |Underscore [32..33)
         |RightArrow [34..36)
         |Ident(b) [37..38)
         |RightBrace [39..40)
         |KwThen [41..45)
         |Ident(c) [46..47)
         |KwElse [48..52)
         |Ident(d) [53..54)
         |LF [54..55)
         |Ident(g) [57..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (try a catch {
         |    case _ => b
         |  }) c else d
         |  g
         |}
         |""".stripMargin
    val cond = Term.Try(tname("a"), List(Case(patwildcard, None, tname("b"))), None)
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(cond, tname("c"), tname("d")), tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` with a parenthesized `for` in a generator, `do` on the same line, next statement") {
    val code =
      """|def f =
         |  for x <- for (y <- ys) yield y do f(x)
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |KwFor [19..22)
         |LeftParen [23..24)
         |Ident(y) [24..25)
         |LeftArrow [26..28)
         |Ident(ys) [29..31)
         |RightParen [31..32)
         |KwYield [33..38)
         |Ident(y) [39..40)
         |KwDo [41..43)
         |Ident(f) [44..45)
         |LeftParen [45..46)
         |Ident(x) [46..47)
         |RightParen [47..48)
         |LF [48..49)
         |Ident(g) [51..52)
         |Indentation.Outdent [52..52)
         |EOF [53..53)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- for (y <- ys) yield y) f(x)
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(Enumerator.Generator(
            patvar("x"),
            Term.ForYield(List(Enumerator.Generator(patvar("y"), tname("ys"))), tname("y")),
          )),
          tapply(tname("f"), tname("x")),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `try` with `catch` without braces, `then` and `else` on the same line, next statement") {
    val code =
      """|def f =
         |  if try a catch case _ => b then c else d
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |KwTry [13..16)
         |Ident(a) [17..18)
         |KwCatch [19..24)
         |Indentation.Indent [25..25)
         |KwCase [25..29)
         |Underscore [30..31)
         |RightArrow [32..34)
         |Ident(b) [35..36)
         |Indentation.Outdent [35..35)
         |KwThen [37..41)
         |Ident(c) [42..43)
         |KwElse [44..48)
         |Ident(d) [49..50)
         |LF [50..51)
         |Ident(g) [53..54)
         |Indentation.Outdent [54..54)
         |EOF [55..55)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (try a catch {
         |    case _ => b
         |  }) c else d
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(
          Term.Try(tname("a"), List(Case(patwildcard, None, tname("b"))), None),
          tname("c"),
          tname("d"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `try` with `catch` without braces, `then` on the same line, no `else`, next statement") {
    val code =
      """|def f =
         |  if try a catch case _ => b then c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |KwTry [13..16)
         |Ident(a) [17..18)
         |KwCatch [19..24)
         |Indentation.Indent [25..25)
         |KwCase [25..29)
         |Underscore [30..31)
         |RightArrow [32..34)
         |Ident(b) [35..36)
         |Indentation.Outdent [35..35)
         |KwThen [37..41)
         |Ident(c) [42..43)
         |LF [43..44)
         |Ident(g) [46..47)
         |Indentation.Outdent [47..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (try a catch {
         |    case _ => b
         |  }) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(Term.Try(tname("a"), List(Case(patwildcard, None, tname("b"))), None), tname("c")),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `while` with `try` and `catch` without braces, `do` on the same line, next statement") {
    val code =
      """|def f =
         |  while try a catch case _ => b do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |KwTry [16..19)
         |Ident(a) [20..21)
         |KwCatch [22..27)
         |Indentation.Indent [28..28)
         |KwCase [28..32)
         |Underscore [33..34)
         |RightArrow [35..37)
         |Ident(b) [38..39)
         |Indentation.Outdent [38..38)
         |KwDo [40..42)
         |Ident(c) [43..44)
         |LF [44..45)
         |Ident(g) [47..48)
         |Indentation.Outdent [48..48)
         |EOF [49..49)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (try a catch {
         |    case _ => b
         |  }) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(Term.Try(tname("a"), List(Case(patwildcard, None, tname("b"))), None), tname("c")),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` guard after a name, next generator on a deeper line, `do` on its own line, next statement") {
    val code =
      """|def f =
         |  for x <- xs if x > 0
         |      y <- ys
         |  do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |Ident(xs) [19..21)
         |KwIf [22..24)
         |Ident(x) [25..26)
         |Ident(>) [27..28)
         |Constant.Int(0) [29..30)
         |LF [30..31)
         |Ident(y) [37..38)
         |LeftArrow [39..41)
         |Ident(ys) [42..44)
         |LF [44..45)
         |KwDo [47..49)
         |Ident(c) [50..51)
         |LF [51..52)
         |Ident(g) [54..55)
         |Indentation.Outdent [55..55)
         |EOF [56..56)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- xs; if x > 0; y <- ys) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator.Generator(patvar("x"), tname("xs")),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` guard after `new`, next generator on a deeper line, `do` on its own line, next statement") {
    val code =
      """|def f =
         |  for x <- new B(a).xs if x > 0
         |      y <- ys
         |  do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |KwNew [19..22)
         |Ident(B) [23..24)
         |LeftParen [24..25)
         |Ident(a) [25..26)
         |RightParen [26..27)
         |Dot [27..28)
         |Ident(xs) [28..30)
         |KwIf [31..33)
         |Ident(x) [34..35)
         |Ident(>) [36..37)
         |Constant.Int(0) [38..39)
         |LF [39..40)
         |Ident(y) [46..47)
         |LeftArrow [48..50)
         |Ident(ys) [51..53)
         |LF [53..54)
         |KwDo [56..58)
         |Ident(c) [59..60)
         |LF [60..61)
         |Ident(g) [63..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- new B(a).xs; if x > 0; y <- ys) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator
              .Generator(patvar("x"), tselect(Term.New(init(pname("B"), List(tname("a")))), "xs")),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` with indented enumerators, a guard after `new`, `do` on its own line, next statement") {
    val code =
      """|def f =
         |  for
         |    x <- new B(a).xs if x > 0
         |    y <- ys
         |  do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Indentation.Indent [13..13)
         |Ident(x) [18..19)
         |LeftArrow [20..22)
         |KwNew [23..26)
         |Ident(B) [27..28)
         |LeftParen [28..29)
         |Ident(a) [29..30)
         |RightParen [30..31)
         |Dot [31..32)
         |Ident(xs) [32..34)
         |KwIf [35..37)
         |Ident(x) [38..39)
         |Ident(>) [40..41)
         |Constant.Int(0) [42..43)
         |LF [43..44)
         |Ident(y) [48..49)
         |LeftArrow [50..52)
         |Ident(ys) [53..55)
         |Indentation.Outdent [55..55)
         |LF [55..56)
         |KwDo [58..60)
         |Ident(c) [61..62)
         |LF [62..63)
         |Ident(g) [65..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- new B(a).xs; if x > 0; y <- ys) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator
              .Generator(patvar("x"), tselect(Term.New(init(pname("B"), List(tname("a")))), "xs")),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` in braces, a guard after `new`, `yield` after the brace, next statement") {
    val code =
      """|def f =
         |  for {
         |    x <- new B(a).xs if x > 0
         |    y <- ys
         |  } yield c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |LeftBrace [14..15)
         |Ident(x) [20..21)
         |LeftArrow [22..24)
         |KwNew [25..28)
         |Ident(B) [29..30)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Dot [33..34)
         |Ident(xs) [34..36)
         |KwIf [37..39)
         |Ident(x) [40..41)
         |Ident(>) [42..43)
         |Constant.Int(0) [44..45)
         |LF [45..46)
         |Ident(y) [50..51)
         |LeftArrow [52..54)
         |Ident(ys) [55..57)
         |LF [57..58)
         |RightBrace [60..61)
         |KwYield [62..67)
         |Ident(c) [68..69)
         |LF [69..70)
         |Ident(g) [72..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- new B(a).xs; if x > 0; y <- ys) yield c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.ForYield(
          List(
            Enumerator
              .Generator(patvar("x"), tselect(Term.New(init(pname("B"), List(tname("a")))), "xs")),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` guard after `try` with `catch` in braces, next generator on a deeper line, `yield` on its own line, next statement") {
    val code =
      """|def f =
         |  for x <- try a catch { case _ => b } if x > 0
         |      y <- ys
         |  yield c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |KwTry [19..22)
         |Ident(a) [23..24)
         |KwCatch [25..30)
         |LeftBrace [31..32)
         |KwCase [33..37)
         |Underscore [38..39)
         |RightArrow [40..42)
         |Ident(b) [43..44)
         |RightBrace [45..46)
         |KwIf [47..49)
         |Ident(x) [50..51)
         |Ident(>) [52..53)
         |Constant.Int(0) [54..55)
         |LF [55..56)
         |Ident(y) [62..63)
         |LeftArrow [64..66)
         |Ident(ys) [67..69)
         |KwYield [72..77)
         |Ident(c) [78..79)
         |LF [79..80)
         |Ident(g) [82..83)
         |Indentation.Outdent [83..83)
         |EOF [84..84)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- try a catch {
         |    case _ => b
         |  }; if x > 0; y <- ys) yield c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.ForYield(
          List(
            Enumerator.Generator(
              patvar("x"),
              Term.Try(tname("a"), List(Case(patwildcard, None, tname("b"))), None),
            ),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` guard with `new`, next generator on a deeper line, `do` on its own line, next statement") {
    val code =
      """|def f =
         |  for x <- xs if new B(x).ok
         |      y <- ys
         |  do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |Ident(xs) [19..21)
         |KwIf [22..24)
         |KwNew [25..28)
         |Ident(B) [29..30)
         |LeftParen [30..31)
         |Ident(x) [31..32)
         |RightParen [32..33)
         |Dot [33..34)
         |Ident(ok) [34..36)
         |LF [36..37)
         |Ident(y) [43..44)
         |LeftArrow [45..47)
         |Ident(ys) [48..50)
         |LF [50..51)
         |KwDo [53..55)
         |Ident(c) [56..57)
         |LF [57..58)
         |Ident(g) [60..61)
         |Indentation.Outdent [61..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- xs; if new B(x).ok; y <- ys) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator.Generator(patvar("x"), tname("xs")),
            Enumerator.Guard(tselect(Term.New(init(pname("B"), List(tname("x")))), "ok")),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` guard after `if then` in a generator, next generator on a deeper line, `do` on its own line, next statement") {
    val code =
      """|def f =
         |  for x <- if a then xs if x > 0
         |      y <- ys
         |  do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |KwIf [19..21)
         |Ident(a) [22..23)
         |KwThen [24..28)
         |Ident(xs) [29..31)
         |KwIf [32..34)
         |Ident(x) [35..36)
         |Ident(>) [37..38)
         |Constant.Int(0) [39..40)
         |LF [40..41)
         |Ident(y) [47..48)
         |LeftArrow [49..51)
         |Ident(ys) [52..54)
         |LF [54..55)
         |KwDo [57..59)
         |Ident(c) [60..61)
         |LF [61..62)
         |Ident(g) [64..65)
         |Indentation.Outdent [65..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- if (a) xs; if x > 0; y <- ys) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator.Generator(patvar("x"), tif(tname("a"), tname("xs"))),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` guard after `try` with an `if` and `catch` in braces, next generator on a deeper line, `do` on its own line, next statement") {
    val code =
      """|def f =
         |  for x <- try if a then xs else ys catch { case _ => zs } if x > 0
         |      y <- ys
         |  do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |KwTry [19..22)
         |KwIf [23..25)
         |Ident(a) [26..27)
         |KwThen [28..32)
         |Ident(xs) [33..35)
         |KwElse [36..40)
         |Ident(ys) [41..43)
         |KwCatch [44..49)
         |LeftBrace [50..51)
         |KwCase [52..56)
         |Underscore [57..58)
         |RightArrow [59..61)
         |Ident(zs) [62..64)
         |RightBrace [65..66)
         |KwIf [67..69)
         |Ident(x) [70..71)
         |Ident(>) [72..73)
         |Constant.Int(0) [74..75)
         |LF [75..76)
         |Ident(y) [82..83)
         |LeftArrow [84..86)
         |Ident(ys) [87..89)
         |LF [89..90)
         |KwDo [92..94)
         |Ident(c) [95..96)
         |LF [96..97)
         |Ident(g) [99..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- try if (a) xs else ys catch {
         |    case _ => zs
         |  }; if x > 0; y <- ys) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator.Generator(
              patvar("x"),
              Term.Try(
                tif(tname("a"), tname("xs"), tname("ys")),
                List(Case(patwildcard, None, tname("zs"))),
                None,
              ),
            ),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` with a nested `if` in a generator, next generator on a deeper line, `do` on its own line, next statement") {
    val code =
      """|def f =
         |  for x <- if a then if b then xs else ys else zs
         |      y <- ys
         |  do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |KwIf [19..21)
         |Ident(a) [22..23)
         |KwThen [24..28)
         |KwIf [29..31)
         |Ident(b) [32..33)
         |KwThen [34..38)
         |Ident(xs) [39..41)
         |KwElse [42..46)
         |Ident(ys) [47..49)
         |KwElse [50..54)
         |Ident(zs) [55..57)
         |LF [57..58)
         |Ident(y) [64..65)
         |LeftArrow [66..68)
         |Ident(ys) [69..71)
         |LF [71..72)
         |KwDo [74..76)
         |Ident(c) [77..78)
         |LF [78..79)
         |Ident(g) [81..82)
         |Indentation.Outdent [82..82)
         |EOF [83..83)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- if (a) if (b) xs else ys else zs; y <- ys) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator.Generator(
              patvar("x"),
              tif(tname("a"), tif(tname("b"), tname("xs"), tname("ys")), tname("zs")),
            ),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` with a guard with `new` and a second guard, next generator on a deeper line, `do` on its own line, next statement") {
    val code =
      """|def f =
         |  for x <- xs if new B(x).ok if x > 0
         |      y <- ys
         |  do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |Ident(xs) [19..21)
         |KwIf [22..24)
         |KwNew [25..28)
         |Ident(B) [29..30)
         |LeftParen [30..31)
         |Ident(x) [31..32)
         |RightParen [32..33)
         |Dot [33..34)
         |Ident(ok) [34..36)
         |KwIf [37..39)
         |Ident(x) [40..41)
         |Ident(>) [42..43)
         |Constant.Int(0) [44..45)
         |LF [45..46)
         |Ident(y) [52..53)
         |LeftArrow [54..56)
         |Ident(ys) [57..59)
         |LF [59..60)
         |KwDo [62..64)
         |Ident(c) [65..66)
         |LF [66..67)
         |Ident(g) [69..70)
         |Indentation.Outdent [70..70)
         |EOF [71..71)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- xs; if new B(x).ok; if x > 0; y <- ys) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator.Generator(patvar("x"), tname("xs")),
            Enumerator.Guard(tselect(Term.New(init(pname("B"), List(tname("x")))), "ok")),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` guard after `new`, a semicolon and the next generator on the same line, `do` on its own line, next statements") {
    val code =
      """|def f =
         |  for x <- new B(a).xs if x > 0; y <- ys
         |  do c
         |  g
         |  h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |KwNew [19..22)
         |Ident(B) [23..24)
         |LeftParen [24..25)
         |Ident(a) [25..26)
         |RightParen [26..27)
         |Dot [27..28)
         |Ident(xs) [28..30)
         |KwIf [31..33)
         |Ident(x) [34..35)
         |Ident(>) [36..37)
         |Constant.Int(0) [38..39)
         |Semicolon [39..40)
         |Ident(y) [41..42)
         |LeftArrow [43..45)
         |Ident(ys) [46..48)
         |LF [48..49)
         |KwDo [51..53)
         |Ident(c) [54..55)
         |LF [55..56)
         |Ident(g) [58..59)
         |LF [59..60)
         |Ident(h) [62..63)
         |Indentation.Outdent [63..63)
         |EOF [64..64)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- new B(a).xs; if x > 0; y <- ys) c
         |  g
         |  h
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator
              .Generator(patvar("x"), tselect(Term.New(init(pname("B"), List(tname("a")))), "xs")),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          ),
          tname("c"),
        ),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: a name on the next line, `then` and `else` on the same line, next statements") {
    val code =
      """|def f =
         |  if
         |    a then c else d
         |  g
         |  h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |Ident(a) [17..18)
         |Indentation.Outdent [17..17)
         |KwThen [19..23)
         |Ident(c) [24..25)
         |KwElse [26..30)
         |Ident(d) [31..32)
         |LF [32..33)
         |Ident(g) [35..36)
         |LF [36..37)
         |Ident(h) [39..40)
         |Indentation.Outdent [40..40)
         |EOF [41..41)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a) c else d
         |  g
         |  h
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tif(tname("a"), tname("c"), tname("d")), tname("g"), tname("h")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `new` on the next line, `then` and `else` on the same line, next statements") {
    val code =
      """|def f =
         |  if
         |    new B(a).ok then c else d
         |  g
         |  h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwNew [17..20)
         |Ident(B) [21..22)
         |LeftParen [22..23)
         |Ident(a) [23..24)
         |RightParen [24..25)
         |Dot [25..26)
         |Ident(ok) [26..28)
         |Indentation.Outdent [26..26)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |LF [42..43)
         |Ident(g) [45..46)
         |LF [46..47)
         |Ident(h) [49..50)
         |Indentation.Outdent [50..50)
         |EOF [51..51)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c else d
         |  g
         |  h
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"), tname("d")),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `new` on the next line, `then` on the same line, no `else`, next statements") {
    val code =
      """|def f =
         |  if
         |    new B(a).ok then c
         |  g
         |  h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwNew [17..20)
         |Ident(B) [21..22)
         |LeftParen [22..23)
         |Ident(a) [23..24)
         |RightParen [24..25)
         |Dot [25..26)
         |Ident(ok) [26..28)
         |Indentation.Outdent [26..26)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |LF [35..36)
         |Ident(g) [38..39)
         |LF [39..40)
         |Ident(h) [42..43)
         |Indentation.Outdent [43..43)
         |EOF [44..44)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c
         |  g
         |  h
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c")),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: in braces, `new` on the next line, `then` and `else` on the same line, next statements") {
    val code =
      """|def f = {
         |  if
         |    new B(a).ok then c else d
         |  g
         |  h
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |LeftBrace [8..9)
         |KwIf [12..14)
         |Indentation.Indent [14..14)
         |KwNew [19..22)
         |Ident(B) [23..24)
         |LeftParen [24..25)
         |Ident(a) [25..26)
         |RightParen [26..27)
         |Dot [27..28)
         |Ident(ok) [28..30)
         |Indentation.Outdent [28..28)
         |KwThen [31..35)
         |Ident(c) [36..37)
         |KwElse [38..42)
         |Ident(d) [43..44)
         |LF [44..45)
         |Ident(g) [47..48)
         |LF [48..49)
         |Ident(h) [51..52)
         |LF [52..53)
         |RightBrace [53..54)
         |EOF [55..55)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c else d
         |  g
         |  h
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"), tname("d")),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `try` with `catch` in braces on the next line, `then` and `else` on the same line, next statements") {
    val code =
      """|def f =
         |  if
         |    try a catch { case _ => b } then c else d
         |  g
         |  h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwTry [17..20)
         |Ident(a) [21..22)
         |KwCatch [23..28)
         |LeftBrace [29..30)
         |KwCase [31..35)
         |Underscore [36..37)
         |RightArrow [38..40)
         |Ident(b) [41..42)
         |RightBrace [43..44)
         |Indentation.Outdent [43..43)
         |KwThen [45..49)
         |Ident(c) [50..51)
         |KwElse [52..56)
         |Ident(d) [57..58)
         |LF [58..59)
         |Ident(g) [61..62)
         |LF [62..63)
         |Ident(h) [65..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (try a catch {
         |    case _ => b
         |  }) c else d
         |  g
         |  h
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(
          Term.Try(tname("a"), List(Case(patwildcard, None, tname("b"))), None),
          tname("c"),
          tname("d"),
        ),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `while` with a name on the next line, `do` on the same line, next statements") {
    val code =
      """|def f =
         |  while
         |    a do c
         |  g
         |  h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |Ident(a) [20..21)
         |Indentation.Outdent [20..20)
         |KwDo [22..24)
         |Ident(c) [25..26)
         |LF [26..27)
         |Ident(g) [29..30)
         |LF [30..31)
         |Ident(h) [33..34)
         |Indentation.Outdent [34..34)
         |EOF [35..35)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a) c
         |  g
         |  h
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.While(tname("a"), tname("c")), tname("g"), tname("h")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `while` with `new` on the next line, `do` on the same line, next statements") {
    val code =
      """|def f =
         |  while
         |    new B(a).ok do c
         |  g
         |  h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |LeftParen [25..26)
         |Ident(a) [26..27)
         |RightParen [27..28)
         |Dot [28..29)
         |Ident(ok) [29..31)
         |Indentation.Outdent [29..29)
         |KwDo [32..34)
         |Ident(c) [35..36)
         |LF [36..37)
         |Ident(g) [39..40)
         |LF [40..41)
         |Ident(h) [43..44)
         |Indentation.Outdent [44..44)
         |EOF [45..45)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (new B(a).ok) c
         |  g
         |  h
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c")),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: an indented `match`, `then` and `else` on the line of the last case, next statements") {
    val code =
      """|def f =
         |  if x match
         |      case 1 => true
         |      case _ => false then c else d
         |  g
         |  h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(x) [13..14)
         |KwMatch [15..20)
         |Indentation.Indent [20..20)
         |KwCase [27..31)
         |Constant.Int(1) [32..33)
         |RightArrow [34..36)
         |KwTrue [37..41)
         |LF [41..42)
         |KwCase [48..52)
         |Underscore [53..54)
         |RightArrow [55..57)
         |KwFalse [58..63)
         |Indentation.Outdent [58..58)
         |KwThen [64..68)
         |Ident(c) [69..70)
         |KwElse [71..75)
         |Ident(d) [76..77)
         |LF [77..78)
         |Ident(g) [80..81)
         |LF [81..82)
         |Ident(h) [84..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (x match {
         |    case 1 => true
         |    case _ => false
         |  }) c else d
         |  g
         |  h
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(
          tmatch(tname("x"), Case(int(1), None, lit(true)), Case(patwildcard, None, lit(false))),
          tname("c"),
          tname("d"),
        ),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "unparenthesized condition: `new` in braces, `then` and `else` on the same line, next statement",
  ) {
    val code =
      """|def f = {
         |  if new B(p).ok then c else d
         |  g
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |LeftBrace [8..9)
         |KwIf [12..14)
         |KwNew [15..18)
         |Ident(B) [19..20)
         |LeftParen [20..21)
         |Ident(p) [21..22)
         |RightParen [22..23)
         |Dot [23..24)
         |Ident(ok) [24..26)
         |KwThen [27..31)
         |Ident(c) [32..33)
         |KwElse [34..38)
         |Ident(d) [39..40)
         |LF [40..41)
         |Ident(g) [43..44)
         |LF [44..45)
         |RightBrace [45..46)
         |EOF [47..47)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (new B(p).ok) c else d
         |  g
         |}
         |""".stripMargin
    val cond = tselect(Term.New(init(pname("B"), List(tname("p")))), "ok")
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(cond, tname("c"), tname("d")), tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `new`, `then` and `else` on the next lines, next statement") {
    val code =
      """|def f =
         |  if new B(p).ok
         |  then c
         |  else d
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |KwNew [13..16)
         |Ident(B) [17..18)
         |LeftParen [18..19)
         |Ident(p) [19..20)
         |RightParen [20..21)
         |Dot [21..22)
         |Ident(ok) [22..24)
         |KwThen [27..31)
         |Ident(c) [32..33)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |LF [42..43)
         |Ident(g) [45..46)
         |Indentation.Outdent [46..46)
         |EOF [47..47)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (new B(p).ok) c else d
         |  g
         |}
         |""".stripMargin
    val cond = tselect(Term.New(init(pname("B"), List(tname("p")))), "ok")
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(cond, tname("c"), tname("d")), tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: an indented `match`, `then` and `else` on the next line, next statement") {
    val code =
      """|def f =
         |  if x match
         |      case 1 => true
         |      case _ => false
         |  then c else d
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(x) [13..14)
         |KwMatch [15..20)
         |Indentation.Indent [20..20)
         |KwCase [27..31)
         |Constant.Int(1) [32..33)
         |RightArrow [34..36)
         |KwTrue [37..41)
         |LF [41..42)
         |KwCase [48..52)
         |Underscore [53..54)
         |RightArrow [55..57)
         |KwFalse [58..63)
         |Indentation.Outdent [63..63)
         |KwThen [66..70)
         |Ident(c) [71..72)
         |KwElse [73..77)
         |Ident(d) [78..79)
         |LF [79..80)
         |Ident(g) [82..83)
         |Indentation.Outdent [83..83)
         |EOF [84..84)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (x match {
         |    case 1 => true
         |    case _ => false
         |  }) c else d
         |  g
         |}
         |""".stripMargin
    val cond = tmatch(tname("x"), Case(lit(1), None, lit(true)), Case(patwildcard, None, lit(false)))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(cond, tname("c"), tname("d")), tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `new`, `then` and `else` at the line end with indented bodies, next statement") {
    val code =
      """|def f =
         |  if new B(p).ok then
         |    c
         |  else
         |    d
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |KwNew [13..16)
         |Ident(B) [17..18)
         |LeftParen [18..19)
         |Ident(p) [19..20)
         |RightParen [20..21)
         |Dot [21..22)
         |Ident(ok) [22..24)
         |KwThen [25..29)
         |Indentation.Indent [29..29)
         |Ident(c) [34..35)
         |Indentation.Outdent [35..35)
         |KwElse [38..42)
         |Indentation.Indent [42..42)
         |Ident(d) [47..48)
         |Indentation.Outdent [48..48)
         |LF [48..49)
         |Ident(g) [51..52)
         |Indentation.Outdent [52..52)
         |EOF [53..53)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (new B(p).ok) c else d
         |  g
         |}
         |""".stripMargin
    val cond = tselect(Term.New(init(pname("B"), List(tname("p")))), "ok")
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(cond, tname("c"), tname("d")), tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: an annotated type ascription after the parentheses, deeper annotation argument, `then` on a later line") {
    val code =
      """|def f =
         |  if (a): B @ann
         |    (c)
         |  then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Colon [16..17)
         |Ident(B) [18..19)
         |At [20..21)
         |Ident(ann) [21..24)
         |LeftParen [29..30)
         |Ident(c) [30..31)
         |RightParen [31..32)
         |KwThen [35..39)
         |Ident(c) [40..41)
         |KwElse [42..46)
         |Ident(d) [47..48)
         |Indentation.Outdent [48..48)
         |EOF [49..49)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout = "def f = if (a: B @ann(c)) c else d"
    val annot = Mod.Annot(init(pname("ann"), List(tname("c"))))
    val cond = Term.Ascribe(tname("a"), Type.Annotate(pname("B"), List(annot)))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tif(cond, tname("c"), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: in an argument, an annotated type ascription after the parentheses, annotation argument and `then` on later lines") {
    val code =
      """|def f = g(if (a): B @ann
         |(c)
         |then c else d)
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Ident(g) [8..9)
         |LeftParen [9..10)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Colon [16..17)
         |Ident(B) [18..19)
         |At [20..21)
         |Ident(ann) [21..24)
         |LeftParen [25..26)
         |Ident(c) [26..27)
         |RightParen [27..28)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |RightParen [42..43)
         |EOF [44..44)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout = "def f = g(if (a: B @ann(c)) c else d)"
    val annot = Mod.Annot(init(pname("ann"), List(tname("c"))))
    val cond = Term.Ascribe(tname("a"), Type.Annotate(pname("B"), List(annot)))
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply(tname("g"), tif(cond, tname("c"), tname("d"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a select after the parentheses, deeper argument in parentheses, `then` on a later line") {
    val code =
      """|def f =
         |  if (a).g
         |    (c)
         |  then c else d
         |""".stripMargin
    val layout =
      """|def f = if (a.g(c)) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tapply(tselect("a", "g"), tname("c")), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: a select, deeper argument in parentheses, `then` on a later line") {
    val code =
      """|def f =
         |  if a.g
         |    (c)
         |  then c else d
         |""".stripMargin
    val layout =
      """|def f = if (a.g(c)) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tapply(tselect("a", "g"), tname("c")), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: an operator and a select of a block with `if then`, deeper argument in parentheses, `then` on a later line, next statement") {
    val code =
      """|def f =
         |  if a && {if b then c else d}.g
         |    (e)
         |  then x else y
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(a) [13..14)
         |Ident(&&) [15..17)
         |LeftBrace [18..19)
         |KwIf [19..21)
         |Ident(b) [22..23)
         |KwThen [24..28)
         |Ident(c) [29..30)
         |KwElse [31..35)
         |Ident(d) [36..37)
         |RightBrace [37..38)
         |Dot [38..39)
         |Ident(g) [39..40)
         |LeftParen [45..46)
         |Ident(e) [46..47)
         |RightParen [47..48)
         |KwThen [51..55)
         |Ident(x) [56..57)
         |KwElse [58..62)
         |Ident(y) [63..64)
         |LF [64..65)
         |Ident(g) [67..68)
         |Indentation.Outdent [68..68)
         |EOF [69..69)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a && {
         |    if (b) c else d
         |  }.g(e)) x else y
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(
          tinfix(
            "a",
            "&&",
            tapply(tselect(blk(tif(tname("b"), tname("c"), tname("d"))), "g"), tname("e")),
          ),
          tname("x"),
          tname("y"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a select and an argument with `if then` after the parentheses, deeper argument in parentheses, `then` on a later line, next statement") {
    val code =
      """|def f =
         |  if (a).g(if b then 1 else 2)
         |    (e)
         |  then x else y
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Dot [16..17)
         |Ident(g) [17..18)
         |LeftParen [18..19)
         |KwIf [19..21)
         |Ident(b) [22..23)
         |KwThen [24..28)
         |Constant.Int(1) [29..30)
         |KwElse [31..35)
         |Constant.Int(2) [36..37)
         |RightParen [37..38)
         |LeftParen [43..44)
         |Ident(e) [44..45)
         |RightParen [45..46)
         |KwThen [49..53)
         |Ident(x) [54..55)
         |KwElse [56..60)
         |Ident(y) [61..62)
         |LF [62..63)
         |Ident(g) [65..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a.g(if (b) 1 else 2)(e)) x else y
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(
          tapply(tapply(tselect("a", "g"), tif(tname("b"), lit(1), lit(2))), tname("e")),
          tname("x"),
          tname("y"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a colon at the line end in braces, deeper `then` and `else`, blank line") {
    val code =
      """|def f = {
         |  if (a):
         |      then c else d
         |
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a {}) c else d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tif(tapply(tname("a"), blk()), tname("c"), tname("d"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `while` with an `if` without `else`, `do` on the same line, next statement") {
    val code =
      """|def f =
         |  while if a then b do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |KwIf [16..18)
         |Ident(a) [19..20)
         |KwThen [21..25)
         |Ident(b) [26..27)
         |KwDo [28..30)
         |Ident(c) [31..32)
         |LF [32..33)
         |Ident(g) [35..36)
         |Indentation.Outdent [36..36)
         |EOF [37..37)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (if (a) b) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.While(tif(tname("a"), tname("b")), tname("c")), tname("g")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: an `if` with `else`, `then` and `else` on the same line, next statement") {
    val code =
      """|def f =
         |  if if a then b else c then d else e
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |KwIf [13..15)
         |Ident(a) [16..17)
         |KwThen [18..22)
         |Ident(b) [23..24)
         |KwElse [25..29)
         |Ident(c) [30..31)
         |KwThen [32..36)
         |Ident(d) [37..38)
         |KwElse [39..43)
         |Ident(e) [44..45)
         |LF [45..46)
         |Ident(g) [48..49)
         |Indentation.Outdent [49..49)
         |EOF [50..50)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (if (a) b else c) d else e
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tif(tif(tname("a"), tname("b"), tname("c")), tname("d"), tname("e")), tname("g")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `for` with `new` in a generator, `do` on the same line, next statement") {
    val code =
      """|def f =
         |  for x <- new B(p).xs do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |KwNew [19..22)
         |Ident(B) [23..24)
         |LeftParen [24..25)
         |Ident(p) [25..26)
         |RightParen [26..27)
         |Dot [27..28)
         |Ident(xs) [28..30)
         |KwDo [31..33)
         |Ident(c) [34..35)
         |LF [35..36)
         |Ident(g) [38..39)
         |Indentation.Outdent [39..39)
         |EOF [40..40)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- new B(p).xs) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator
              .Generator(patvar("x"), tselect(Term.New(init(pname("B"), List(tname("p")))), "xs")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "unparenthesized condition: `new` in the `then` branch, `else` on the same line, next statement",
  ) {
    val code =
      """|def f =
         |  if a then new B(p).ok else d
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(a) [13..14)
         |KwThen [15..19)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |LeftParen [25..26)
         |Ident(p) [26..27)
         |RightParen [27..28)
         |Dot [28..29)
         |Ident(ok) [29..31)
         |KwElse [32..36)
         |Ident(d) [37..38)
         |LF [38..39)
         |Ident(g) [41..42)
         |Indentation.Outdent [42..42)
         |EOF [43..43)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a) new B(p).ok else d
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tname("a"), tselect(Term.New(init(pname("B"), List(tname("p")))), "ok"), tname("d")),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `while` with `try` and `catch` in braces, `do` on the same line, next statement") {
    val code =
      """|def f =
         |  while try a catch { case _ => b } do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |KwTry [16..19)
         |Ident(a) [20..21)
         |KwCatch [22..27)
         |LeftBrace [28..29)
         |KwCase [30..34)
         |Underscore [35..36)
         |RightArrow [37..39)
         |Ident(b) [40..41)
         |RightBrace [42..43)
         |KwDo [44..46)
         |Ident(c) [47..48)
         |LF [48..49)
         |Ident(g) [51..52)
         |Indentation.Outdent [52..52)
         |EOF [53..53)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (try a catch {
         |    case _ => b
         |  }) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(Term.Try(tname("a"), List(Case(patwildcard, None, tname("b"))), None), tname("c")),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `while` with `new`, `do` at the line end, indented body, next statement") {
    val code =
      """|def f =
         |  while new B(p).ok do
         |    c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |KwNew [16..19)
         |Ident(B) [20..21)
         |LeftParen [21..22)
         |Ident(p) [22..23)
         |RightParen [23..24)
         |Dot [24..25)
         |Ident(ok) [25..27)
         |KwDo [28..30)
         |Indentation.Indent [30..30)
         |Ident(c) [35..36)
         |Indentation.Outdent [36..36)
         |LF [36..37)
         |Ident(g) [39..40)
         |Indentation.Outdent [40..40)
         |EOF [41..41)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (new B(p).ok) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(tselect(Term.New(init(pname("B"), List(tname("p")))), "ok"), tname("c")),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `new` with a template body, `then` and `else` on the same line, next statement") {
    val code =
      """|def f =
         |  if new B { def ok = true }.ok then c else d
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |KwNew [13..16)
         |Ident(B) [17..18)
         |LeftBrace [19..20)
         |KwDef [21..24)
         |Ident(ok) [25..27)
         |Equals [28..29)
         |KwTrue [30..34)
         |RightBrace [35..36)
         |Dot [36..37)
         |Ident(ok) [37..39)
         |KwThen [40..44)
         |Ident(c) [45..46)
         |KwElse [47..51)
         |Ident(d) [52..53)
         |LF [53..54)
         |Ident(g) [56..57)
         |Indentation.Outdent [57..57)
         |EOF [58..58)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (new B { def ok = true }.ok) c else d
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(
          tselect(
            Term.NewAnonymous(
              tpl(List(init(pname("B"))), List(Defn.Def(Nil, tname("ok"), Nil, None, lit(true)))),
            ),
            "ok",
          ),
          tname("c"),
          tname("d"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "unparenthesized condition: `for` with `new` in a guard, `do` on the same line, next statement",
  ) {
    val code =
      """|def f =
         |  for x <- xs if new B(x).ok do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwFor [10..13)
         |Ident(x) [14..15)
         |LeftArrow [16..18)
         |Ident(xs) [19..21)
         |KwIf [22..24)
         |KwNew [25..28)
         |Ident(B) [29..30)
         |LeftParen [30..31)
         |Ident(x) [31..32)
         |RightParen [32..33)
         |Dot [33..34)
         |Ident(ok) [34..36)
         |KwDo [37..39)
         |Ident(c) [40..41)
         |LF [41..42)
         |Ident(g) [44..45)
         |Indentation.Outdent [45..45)
         |EOF [46..46)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  for (x <- xs; if new B(x).ok) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.For(
          List(
            Enumerator.Generator(patvar("x"), tname("xs")),
            Enumerator.Guard(tselect(Term.New(init(pname("B"), List(tname("x")))), "ok")),
          ),
          tname("c"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a select, an operator and an indented `match` after the parentheses, `then` and `else` on the next line, next statement") {
    val code =
      """|def f =
         |  if (a).isEmpty || x match
         |      case 1 => true
         |  then c else d
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Dot [16..17)
         |Ident(isEmpty) [17..24)
         |Ident(||) [25..27)
         |Ident(x) [28..29)
         |KwMatch [30..35)
         |Indentation.Indent [35..35)
         |KwCase [42..46)
         |Constant.Int(1) [47..48)
         |RightArrow [49..51)
         |KwTrue [52..56)
         |Indentation.Outdent [56..56)
         |KwThen [59..63)
         |Ident(c) [64..65)
         |KwElse [66..70)
         |Ident(d) [71..72)
         |LF [72..73)
         |Ident(g) [75..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a.isEmpty || x match {
         |    case 1 => true
         |  }) c else d
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(
          tmatch(tinfix(tselect("a", "isEmpty"), "||", tname("x")), Case(lit(1), None, lit(true))),
          tname("c"),
          tname("d"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `while` with an indented `match`, `do` on the next line, next statement") {
    val code =
      """|def f =
         |  while x match
         |      case 1 => true
         |  do c
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Ident(x) [16..17)
         |KwMatch [18..23)
         |Indentation.Indent [23..23)
         |KwCase [30..34)
         |Constant.Int(1) [35..36)
         |RightArrow [37..39)
         |KwTrue [40..44)
         |Indentation.Outdent [44..44)
         |KwDo [47..49)
         |Ident(c) [50..51)
         |LF [51..52)
         |Ident(g) [54..55)
         |Indentation.Outdent [55..55)
         |EOF [56..56)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (x match {
         |    case 1 => true
         |  }) c
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.While(tmatch(tname("x"), Case(lit(1), None, lit(true))), tname("c")), tname("g")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `try` with `finally`, `then` and `else` on the same line, next statement") {
    val code =
      """|def f =
         |  if try a finally b then c else d
         |  g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |KwTry [13..16)
         |Ident(a) [17..18)
         |KwFinally [19..26)
         |Ident(b) [27..28)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |LF [42..43)
         |Ident(g) [45..46)
         |Indentation.Outdent [46..46)
         |EOF [47..47)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (try a finally b) c else d
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tif(Term.Try(tname("a"), Nil, Some(tname("b"))), tname("c"), tname("d")), tname("g")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: `new`, `then` on the same line, no `else`, a block on the next line, next definition") {
    val code =
      """|def f =
         |  if new B(a).ok then c
         |  {c}
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |KwNew [13..16)
         |Ident(B) [17..18)
         |LeftParen [18..19)
         |Ident(a) [19..20)
         |RightParen [20..21)
         |Dot [21..22)
         |Ident(ok) [22..24)
         |KwThen [25..29)
         |Ident(c) [30..31)
         |LF [31..32)
         |LeftBrace [34..35)
         |Ident(c) [35..36)
         |RightBrace [36..37)
         |Indentation.Outdent [37..37)
         |LF [37..38)
         |KwDef [38..41)
         |Ident(g) [42..43)
         |Equals [44..45)
         |Constant.Int(1) [46..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c
         |  {
         |    c
         |  }
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tselect(
              Term.New(Init(pname("B"), Name.Anonymous(), List(Term.ArgClause(List(tname("a")), None)))),
              "ok",
            ),
            tname("c"),
          ),
          blk(tname("c")),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, `;` and an identifier inside an indented condition, `then` and `else` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) b; c then d else e
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(b) [40..41)
         |Semicolon [41..42)
         |Ident(c) [43..44)
         |Indentation.Outdent [43..43)
         |KwThen [45..49)
         |Ident(d) [50..51)
         |KwElse [52..56)
         |Ident(e) [57..58)
         |LF [58..59)
         |Ident(g) [63..64)
         |LF [64..65)
         |Ident(h) [69..70)
         |Indentation.Outdent [70..70)
         |Indentation.Outdent [70..70)
         |EOF [71..71)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      c
         |    }) d else e
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(blk(tif(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an identifier after the parentheses, `;` and an identifier inside an indented `while` condition, `do` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) b; c do d
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(b) [46..47)
         |Semicolon [47..48)
         |Ident(c) [49..50)
         |Indentation.Outdent [49..49)
         |KwDo [51..53)
         |Ident(d) [54..55)
         |LF [55..56)
         |Ident(g) [60..61)
         |LF [61..62)
         |Ident(h) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a) b
         |      c
         |    }) d
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, `;` and an identifier inside an indented condition, `then` on the same line, no `else`, next statements") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) b; c then d
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(b) [40..41)
         |Semicolon [41..42)
         |Ident(c) [43..44)
         |Indentation.Outdent [43..43)
         |KwThen [45..49)
         |Ident(d) [50..51)
         |LF [51..52)
         |Ident(g) [56..57)
         |LF [57..58)
         |Ident(h) [62..63)
         |Indentation.Outdent [63..63)
         |Indentation.Outdent [63..63)
         |EOF [64..64)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      c
         |    }) d
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(blk(tif(tname("a"), tname("b")), tname("c")), tname("d")), tname("g"), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("unparenthesized condition: `for` guard after `try` with `catch case` on the same line, `do` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    for x <- try xs catch case _ => ys if x > 0 do c
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Ident(x) [28..29)
         |LeftArrow [30..32)
         |KwTry [33..36)
         |Ident(xs) [37..39)
         |KwCatch [40..45)
         |Indentation.Indent [46..46)
         |KwCase [46..50)
         |Underscore [51..52)
         |RightArrow [53..55)
         |Ident(ys) [56..58)
         |KwIf [59..61)
         |Ident(x) [62..63)
         |Ident(>) [64..65)
         |Constant.Int(0) [66..67)
         |Indentation.Outdent [66..66)
         |KwDo [68..70)
         |Ident(c) [71..72)
         |LF [72..73)
         |Ident(g) [77..78)
         |LF [78..79)
         |Ident(h) [83..84)
         |Indentation.Outdent [84..84)
         |Indentation.Outdent [84..84)
         |EOF [85..85)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:3: error: `outdent` expected but `if` found
         |    for x <- try xs catch case _ => ys if x > 0 do c
         |                                       ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("parenthesized condition: `for` with `while` and an identifier after the parentheses in a value enumerator, `;` and the next generator, `do` on the same line, next `if` with an indented condition") {
    val code =
      """|object A:
         |  def f =
         |    for x <- xs; _ = while (a) b; y <- ys do c
         |    if
         |      a then d else e
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Ident(x) [28..29)
         |LeftArrow [30..32)
         |Ident(xs) [33..35)
         |Semicolon [35..36)
         |Underscore [37..38)
         |Equals [39..40)
         |KwWhile [41..46)
         |LeftParen [47..48)
         |Ident(a) [48..49)
         |RightParen [49..50)
         |Ident(b) [51..52)
         |Semicolon [52..53)
         |Ident(y) [54..55)
         |LeftArrow [56..58)
         |Ident(ys) [59..61)
         |KwDo [62..64)
         |Ident(c) [65..66)
         |LF [66..67)
         |KwIf [71..73)
         |Indentation.Indent [73..73)
         |Ident(a) [80..81)
         |Indentation.Outdent [80..80)
         |KwThen [82..86)
         |Ident(d) [87..88)
         |KwElse [89..93)
         |Ident(e) [94..95)
         |LF [95..96)
         |Ident(g) [100..101)
         |Indentation.Outdent [101..101)
         |Indentation.Outdent [101..101)
         |EOF [102..102)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; _ = while (a) b; y <- ys) c
         |    if (a) d else e
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.For(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(patwildcard, Term.While(tname("a"), tname("b"))),
              Enumerator.Generator(patvar("y"), tname("ys")),
            ),
            tname("c"),
          ),
          tif(tname("a"), tname("d"), tname("e")),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: a statement, an `if` with a call after the parentheses and an operator, separated by `;` inside an indented `while` condition, `do` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      i += 1; if (i > n) stop(); i < m do step()
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |Ident(i) [36..37)
         |Ident(+=) [38..40)
         |Constant.Int(1) [41..42)
         |Semicolon [42..43)
         |KwIf [44..46)
         |LeftParen [47..48)
         |Ident(i) [48..49)
         |Ident(>) [50..51)
         |Ident(n) [52..53)
         |RightParen [53..54)
         |Ident(stop) [55..59)
         |LeftParen [59..60)
         |RightParen [60..61)
         |Semicolon [61..62)
         |Ident(i) [63..64)
         |Ident(<) [65..66)
         |Ident(m) [67..68)
         |Indentation.Outdent [67..67)
         |KwDo [69..71)
         |Ident(step) [72..76)
         |LeftParen [76..77)
         |RightParen [77..78)
         |LF [78..79)
         |Ident(g) [83..84)
         |LF [84..85)
         |Ident(h) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      i += 1
         |      if (i > n) stop()
         |      i < m
         |    }) step()
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              tinfix(tname("i"), "+=", int(1)),
              tif(tinfix(tname("i"), ">", tname("n")), tapply(tname("stop"))),
              tinfix(tname("i"), "<", tname("m")),
            ),
            tapply(tname("step")),
          ),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `&&` after the parentheses inside an indented condition, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |        || c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [44..45)
         |Ident(||) [53..55)
         |Ident(c) [56..57)
         |KwThen [58..62)
         |Ident(d) [63..64)
         |KwElse [65..69)
         |Ident(e) [70..71)
         |Indentation.Outdent [71..71)
         |KwThen [76..80)
         |Ident(g) [81..82)
         |KwElse [83..87)
         |Ident(h) [88..89)
         |LF [89..90)
         |Ident(k) [94..95)
         |Indentation.Outdent [95..95)
         |Indentation.Outdent [95..95)
         |EOF [96..96)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `&&` after the parentheses inside an indented `while` condition, leading `||` on the next line with `then` and `else`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if (a) && b
         |        || c then d else e
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |LeftParen [39..40)
         |Ident(a) [40..41)
         |RightParen [41..42)
         |Ident(&&) [43..45)
         |Ident(b) [46..47)
         |InfixLF [47..48)
         |Ident(||) [56..58)
         |Ident(c) [59..60)
         |KwThen [61..65)
         |Ident(d) [66..67)
         |KwElse [68..72)
         |Ident(e) [73..74)
         |Indentation.Outdent [74..74)
         |KwDo [79..81)
         |Ident(g) [82..83)
         |LF [83..84)
         |Ident(k) [88..89)
         |Indentation.Outdent [89..89)
         |Indentation.Outdent [89..89)
         |EOF [90..90)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if (a && b || c) d else e) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `while` with `&&` after the parentheses in a `for` value, leading `||` on the next line with `do`, `yield` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    for x <- xs; _ = while (a) && b
         |        || c do d
         |    yield e
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Ident(x) [28..29)
         |LeftArrow [30..32)
         |Ident(xs) [33..35)
         |Semicolon [35..36)
         |Underscore [37..38)
         |Equals [39..40)
         |KwWhile [41..46)
         |LeftParen [47..48)
         |Ident(a) [48..49)
         |RightParen [49..50)
         |Ident(&&) [51..53)
         |Ident(b) [54..55)
         |InfixLF [55..56)
         |Ident(||) [64..66)
         |Ident(c) [67..68)
         |KwDo [69..71)
         |Ident(d) [72..73)
         |KwYield [78..83)
         |Ident(e) [84..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A { def f = for (x <- xs; _ = while (a && b || c) d) yield e }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.ForYield(
          List(
            Enumerator.Generator(patvar("x"), tname("xs")),
            Enumerator.Val(
              patwildcard,
              Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
            ),
          ),
          tname("e"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `while` with `&&` after the parentheses inside an indented `while` condition, leading `||` on the next line with `do`, another statement, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) && b
         |        || c do d
         |      e
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(&&) [46..48)
         |Ident(b) [49..50)
         |InfixLF [50..51)
         |Ident(||) [59..61)
         |Ident(c) [62..63)
         |KwDo [64..66)
         |Ident(d) [67..68)
         |LF [68..69)
         |Ident(e) [75..76)
         |Indentation.Outdent [76..76)
         |KwDo [81..83)
         |Ident(k) [84..85)
         |LF [85..86)
         |Ident(g) [90..91)
         |Indentation.Outdent [91..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a && b || c) d
         |      e
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
              tname("e"),
            ),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `&&` and an identifier after the parentheses, an argument in parentheses on the next line with `then` and `else`, next statements") {
    val code =
      """|object A:
         |  def f =
         |    if (a) && f
         |      (c) then d else e
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Ident(&&) [31..33)
         |Ident(f) [34..35)
         |LeftParen [42..43)
         |Ident(c) [43..44)
         |RightParen [44..45)
         |KwThen [46..50)
         |Ident(d) [51..52)
         |KwElse [53..57)
         |Ident(e) [58..59)
         |LF [59..60)
         |Ident(g) [64..65)
         |LF [65..66)
         |Ident(h) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a && f(c)) d else e
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tinfix(tname("a"), "&&", tapply(tname("f"), tname("c"))), tname("d"), tname("e")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `&&` and an identifier after the parentheses inside an indented condition, an argument in parentheses on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && f
         |        (c) then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(f) [43..44)
         |LeftParen [53..54)
         |Ident(c) [54..55)
         |RightParen [55..56)
         |KwThen [57..61)
         |Ident(d) [62..63)
         |KwElse [64..68)
         |Ident(e) [69..70)
         |Indentation.Outdent [70..70)
         |KwThen [75..79)
         |Ident(g) [80..81)
         |KwElse [82..86)
         |Ident(h) [87..88)
         |LF [88..89)
         |Ident(k) [93..94)
         |Indentation.Outdent [94..94)
         |Indentation.Outdent [94..94)
         |EOF [95..95)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && f(c)) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tinfix(tname("a"), "&&", tapply(tname("f"), tname("c"))), tname("d"), tname("e")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `while` with `&&` and an identifier after the parentheses, an argument in parentheses on the next line with `do`, next statements") {
    val code =
      """|object A:
         |  def f =
         |    while (a) && f
         |      (c) do d
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(&&) [34..36)
         |Ident(f) [37..38)
         |LeftParen [45..46)
         |Ident(c) [46..47)
         |RightParen [47..48)
         |KwDo [49..51)
         |Ident(d) [52..53)
         |LF [53..54)
         |Ident(g) [58..59)
         |LF [59..60)
         |Ident(h) [64..65)
         |Indentation.Outdent [65..65)
         |Indentation.Outdent [65..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (a && f(c)) d
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tinfix(tname("a"), "&&", tapply(tname("f"), tname("c"))), tname("d")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: a colon argument after the parentheses inside an indented condition, a deeper `if` on the next line, outer `then` and `else` on the next line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) f:
         |          if b then c else d
         |      then y else z
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(f) [40..41)
         |Colon [41..42)
         |Indentation.Indent [42..42)
         |KwIf [53..55)
         |Ident(b) [56..57)
         |KwThen [58..62)
         |Ident(c) [63..64)
         |KwElse [65..69)
         |Ident(d) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |KwThen [78..82)
         |Ident(y) [83..84)
         |KwElse [85..89)
         |Ident(z) [90..91)
         |LF [91..92)
         |Ident(g) [96..97)
         |LF [97..98)
         |Ident(h) [102..103)
         |Indentation.Outdent [103..103)
         |Indentation.Outdent [103..103)
         |EOF [104..104)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) f {
         |      if (b) c else d
         |    }) y else z
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tname("a"), tapply(tname("f"), blk(tif(tname("b"), tname("c"), tname("d"))))),
            tname("y"),
            tname("z"),
          ),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `while` with a colon argument after the parentheses inside an indented `while` condition, a deeper `if` on the next line, `do` on the next line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) f:
         |          if b then c else d
         |      do y
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(f) [46..47)
         |Colon [47..48)
         |Indentation.Indent [48..48)
         |KwIf [59..61)
         |Ident(b) [62..63)
         |KwThen [64..68)
         |Ident(c) [69..70)
         |KwElse [71..75)
         |Ident(d) [76..77)
         |Indentation.Outdent [77..77)
         |LF [77..78)
         |Indentation.Outdent [77..77)
         |KwDo [84..86)
         |Ident(y) [87..88)
         |LF [88..89)
         |Ident(g) [93..94)
         |LF [94..95)
         |Ident(h) [99..100)
         |Indentation.Outdent [100..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (while (a) f {
         |      if (b) c else d
         |    }) y
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.While(tname("a"), tapply(tname("f"), blk(tif(tname("b"), tname("c"), tname("d"))))),
            tname("y"),
          ),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an `if` with parentheses in the body, `;` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (x) if (a) b; else c
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(x) [28..29)
         |RightParen [29..30)
         |KwIf [31..33)
         |LeftParen [34..35)
         |Ident(a) [35..36)
         |RightParen [36..37)
         |Ident(b) [38..39)
         |Semicolon [39..40)
         |KwElse [41..45)
         |Ident(c) [46..47)
         |LF [47..48)
         |Ident(g) [52..53)
         |Indentation.Outdent [53..53)
         |Indentation.Outdent [53..53)
         |EOF [54..54)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (x) if (a) b else c
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tname("x"), tif(tname("a"), tname("b"), tname("c"))), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: a `while` with parentheses in the body, `else` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    if (x) while (a) b else c
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(x) [28..29)
         |RightParen [29..30)
         |KwWhile [31..36)
         |LeftParen [37..38)
         |Ident(a) [38..39)
         |RightParen [39..40)
         |Ident(b) [41..42)
         |KwElse [43..47)
         |Ident(c) [48..49)
         |LF [49..50)
         |Ident(g) [54..55)
         |LF [55..56)
         |Ident(h) [60..61)
         |Indentation.Outdent [61..61)
         |Indentation.Outdent [61..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (x) while (a) b else c
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tname("x"), Term.While(tname("a"), tname("b")), tname("c")), tname("g"), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, `;` and an identifier inside a condition in braces, `then` and `else` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    if {if (a) b; c} then d else e
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftBrace [27..28)
         |KwIf [28..30)
         |LeftParen [31..32)
         |Ident(a) [32..33)
         |RightParen [33..34)
         |Ident(b) [35..36)
         |Semicolon [36..37)
         |Ident(c) [38..39)
         |RightBrace [39..40)
         |KwThen [41..45)
         |Ident(d) [46..47)
         |KwElse [48..52)
         |Ident(e) [53..54)
         |LF [54..55)
         |Ident(g) [59..60)
         |LF [60..61)
         |Ident(h) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      c
         |    }) d else e
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(blk(tif(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses in a `for` value, `;` and a generator, `do` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    for x <- xs; _ = if (a) b; y <- ys do c
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Ident(x) [28..29)
         |LeftArrow [30..32)
         |Ident(xs) [33..35)
         |Semicolon [35..36)
         |Underscore [37..38)
         |Equals [39..40)
         |KwIf [41..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(b) [48..49)
         |Semicolon [49..50)
         |Ident(y) [51..52)
         |LeftArrow [53..55)
         |Ident(ys) [56..58)
         |KwDo [59..61)
         |Ident(c) [62..63)
         |LF [63..64)
         |Ident(g) [68..69)
         |LF [69..70)
         |Ident(h) [74..75)
         |Indentation.Outdent [75..75)
         |Indentation.Outdent [75..75)
         |EOF [76..76)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; _ = if (a) b; y <- ys) c
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.For(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(patwildcard, tif(tname("a"), tname("b"))),
              Enumerator.Generator(patvar("y"), tname("ys")),
            ),
            tname("c"),
          ),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: `while` with an identifier after the parentheses, `;` and an identifier inside an indented condition, `then` and `else` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      while (a) b; c then d else e
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwWhile [33..38)
         |LeftParen [39..40)
         |Ident(a) [40..41)
         |RightParen [41..42)
         |Ident(b) [43..44)
         |Semicolon [44..45)
         |Ident(c) [46..47)
         |Indentation.Outdent [46..46)
         |KwThen [48..52)
         |Ident(d) [53..54)
         |KwElse [55..59)
         |Ident(e) [60..61)
         |LF [61..62)
         |Ident(g) [66..67)
         |LF [67..68)
         |Ident(h) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      while (a) b
         |      c
         |    }) d else e
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an identifier and `else` after the parentheses, `;` and an identifier inside an indented `while` condition, `do` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if (a) b else e; c do d
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |LeftParen [39..40)
         |Ident(a) [40..41)
         |RightParen [41..42)
         |Ident(b) [43..44)
         |KwElse [45..49)
         |Ident(e) [50..51)
         |Semicolon [51..52)
         |Ident(c) [53..54)
         |Indentation.Outdent [53..53)
         |KwDo [55..57)
         |Ident(d) [58..59)
         |LF [59..60)
         |Ident(g) [64..65)
         |LF [65..66)
         |Ident(h) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      if (a) b else e
         |      c
         |    }) d
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(blk(tif(tname("a"), tname("b"), tname("e")), tname("c")), tname("d")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: a statement, an identifier after the parentheses, `;` and an identifier inside an indented condition, `then` and `else` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      x
         |      if (a) b; c then d else e
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |Ident(x) [33..34)
         |LF [34..35)
         |KwIf [41..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(b) [48..49)
         |Semicolon [49..50)
         |Ident(c) [51..52)
         |Indentation.Outdent [51..51)
         |KwThen [53..57)
         |Ident(d) [58..59)
         |KwElse [60..64)
         |Ident(e) [65..66)
         |LF [66..67)
         |Ident(g) [71..72)
         |LF [72..73)
         |Ident(h) [77..78)
         |Indentation.Outdent [78..78)
         |Indentation.Outdent [78..78)
         |EOF [79..79)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      x
         |      if (a) b
         |      c
         |    }) d else e
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(blk(tname("x"), tif(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an identifier and `else` after the parentheses in a `for` value, `;` and a generator, `yield` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    for x <- xs; _ = if (a) b else e; y <- ys yield c
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Ident(x) [28..29)
         |LeftArrow [30..32)
         |Ident(xs) [33..35)
         |Semicolon [35..36)
         |Underscore [37..38)
         |Equals [39..40)
         |KwIf [41..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(b) [48..49)
         |KwElse [50..54)
         |Ident(e) [55..56)
         |Semicolon [56..57)
         |Ident(y) [58..59)
         |LeftArrow [60..62)
         |Ident(ys) [63..65)
         |KwYield [66..71)
         |Ident(c) [72..73)
         |LF [73..74)
         |Ident(g) [78..79)
         |LF [79..80)
         |Ident(h) [84..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; _ = if (a) b else e; y <- ys) yield c
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.ForYield(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(patwildcard, tif(tname("a"), tname("b"), tname("e"))),
              Enumerator.Generator(patvar("y"), tname("ys")),
            ),
            tname("c"),
          ),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: an identifier after the parentheses, `;` and an identifier inside an indented condition, `then` and `else` on the next line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) b; c
         |    then d else e
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(b) [40..41)
         |Semicolon [41..42)
         |Ident(c) [43..44)
         |Indentation.Outdent [44..44)
         |KwThen [49..53)
         |Ident(d) [54..55)
         |KwElse [56..60)
         |Ident(e) [61..62)
         |LF [62..63)
         |Ident(g) [67..68)
         |LF [68..69)
         |Ident(h) [73..74)
         |Indentation.Outdent [74..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      c
         |    }) d else e
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(blk(tif(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("parenthesized condition: a select after the parentheses, `;` and an identifier inside an indented `while` condition, `do` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if (a) b.g; c do d
         |    g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |LeftParen [39..40)
         |Ident(a) [40..41)
         |RightParen [41..42)
         |Ident(b) [43..44)
         |Dot [44..45)
         |Ident(g) [45..46)
         |Semicolon [46..47)
         |Ident(c) [48..49)
         |Indentation.Outdent [48..48)
         |KwDo [50..52)
         |Ident(d) [53..54)
         |LF [54..55)
         |Ident(g) [59..60)
         |LF [60..61)
         |Ident(h) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      if (a) b.g
         |      c
         |    }) d
         |    g
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(blk(tif(tname("a"), tselect(tname("b"), "g")), tname("c")), tname("d")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: an `if` with parentheses and `else` inside an `if` inside an indented condition, middle and outer `then` and `else` on the next lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if
         |        if (a) b else c
         |      then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |Indentation.Indent [35..35)
         |KwIf [44..46)
         |LeftParen [47..48)
         |Ident(a) [48..49)
         |RightParen [49..50)
         |Ident(b) [51..52)
         |KwElse [53..57)
         |Ident(c) [58..59)
         |Indentation.Outdent [59..59)
         |KwThen [66..70)
         |Ident(d) [71..72)
         |KwElse [73..77)
         |Ident(e) [78..79)
         |Indentation.Outdent [79..79)
         |KwThen [84..88)
         |Ident(g) [89..90)
         |KwElse [91..95)
         |Ident(h) [96..97)
         |LF [97..98)
         |Ident(k) [102..103)
         |Indentation.Outdent [103..103)
         |Indentation.Outdent [103..103)
         |EOF [104..104)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (if (a) b else c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tif(tname("a"), tname("b"), tname("c")), tname("d"), tname("e")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a `while` with parentheses, `;` and an identifier inside an `if` inside an indented condition, `then` and `else` on the same line, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if
         |        while (a) b; c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |Indentation.Indent [35..35)
         |KwWhile [44..49)
         |LeftParen [50..51)
         |Ident(a) [51..52)
         |RightParen [52..53)
         |Ident(b) [54..55)
         |Semicolon [55..56)
         |Ident(c) [57..58)
         |Indentation.Outdent [57..57)
         |KwThen [59..63)
         |Ident(d) [64..65)
         |KwElse [66..70)
         |Ident(e) [71..72)
         |Indentation.Outdent [72..72)
         |KwThen [77..81)
         |Ident(g) [82..83)
         |KwElse [84..88)
         |Ident(h) [89..90)
         |LF [90..91)
         |Ident(k) [95..96)
         |Indentation.Outdent [96..96)
         |Indentation.Outdent [96..96)
         |EOF [97..97)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if ({
         |      while (a) b
         |      c
         |    }) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: an `if` with parentheses, `;` and an identifier inside an indented `if` condition inside an indented `while` condition, `then` and `else` on the same line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if
         |        if (a) b; c then d else e
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |Indentation.Indent [38..38)
         |KwIf [47..49)
         |LeftParen [50..51)
         |Ident(a) [51..52)
         |RightParen [52..53)
         |Ident(b) [54..55)
         |Semicolon [55..56)
         |Ident(c) [57..58)
         |Indentation.Outdent [57..57)
         |KwThen [59..63)
         |Ident(d) [64..65)
         |KwElse [66..70)
         |Ident(e) [71..72)
         |Indentation.Outdent [72..72)
         |KwDo [77..79)
         |Ident(g) [80..81)
         |LF [81..82)
         |Ident(k) [86..87)
         |Indentation.Outdent [87..87)
         |Indentation.Outdent [87..87)
         |EOF [88..88)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if ({
         |      if (a) b
         |      c
         |    }) d else e) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tif(blk(tif(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses inside an indented `while` condition, `do` on the next line, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      for x <- xs
         |      do d
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwFor [36..39)
         |Ident(x) [40..41)
         |LeftArrow [42..44)
         |Ident(xs) [45..47)
         |LF [47..48)
         |KwDo [54..56)
         |Ident(d) [57..58)
         |Indentation.Outdent [58..58)
         |KwDo [63..65)
         |Ident(k) [66..67)
         |LF [67..68)
         |Ident(g) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with parentheses and `yield`, `;` and an identifier inside an indented condition, `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for (x <- xs) yield d; c then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |LeftParen [37..38)
         |Ident(x) [38..39)
         |LeftArrow [40..42)
         |Ident(xs) [43..45)
         |RightParen [45..46)
         |KwYield [47..52)
         |Ident(d) [53..54)
         |Semicolon [54..55)
         |Ident(c) [56..57)
         |Indentation.Outdent [56..56)
         |KwThen [58..62)
         |Ident(g) [63..64)
         |KwElse [65..69)
         |Ident(h) [70..71)
         |LF [71..72)
         |Ident(k) [76..77)
         |Indentation.Outdent [77..77)
         |Indentation.Outdent [77..77)
         |EOF [78..78)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      for (x <- xs) yield d
         |      c
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(
              Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
              tname("c"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with parentheses and a body, `;` and an identifier inside an indented condition, `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for (x <- xs) d; c then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |LeftParen [37..38)
         |Ident(x) [38..39)
         |LeftArrow [40..42)
         |Ident(xs) [43..45)
         |RightParen [45..46)
         |Ident(d) [47..48)
         |Semicolon [48..49)
         |Ident(c) [50..51)
         |Indentation.Outdent [50..50)
         |KwThen [52..56)
         |Ident(g) [57..58)
         |KwElse [59..63)
         |Ident(h) [64..65)
         |LF [65..66)
         |Ident(k) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      for (x <- xs) d
         |      c
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(
              Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
              tname("c"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with parentheses and `do`, `;` and an identifier inside an indented `while` condition, `do` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      for (x <- xs) do d; c do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwFor [36..39)
         |LeftParen [40..41)
         |Ident(x) [41..42)
         |LeftArrow [43..45)
         |Ident(xs) [46..48)
         |RightParen [48..49)
         |KwDo [50..52)
         |Ident(d) [53..54)
         |Semicolon [54..55)
         |Ident(c) [56..57)
         |Indentation.Outdent [56..56)
         |KwDo [58..60)
         |Ident(k) [61..62)
         |LF [62..63)
         |Ident(g) [67..68)
         |Indentation.Outdent [68..68)
         |Indentation.Outdent [68..68)
         |EOF [69..69)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      for (x <- xs) d
         |      c
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
              tname("c"),
            ),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses with `yield` inside an indented condition, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for x <- xs yield d
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |Ident(x) [37..38)
         |LeftArrow [39..41)
         |Ident(xs) [42..44)
         |KwYield [45..50)
         |Ident(d) [51..52)
         |Indentation.Outdent [52..52)
         |KwThen [57..61)
         |Ident(g) [62..63)
         |KwElse [64..68)
         |Ident(h) [69..70)
         |LF [70..71)
         |Ident(k) [75..76)
         |Indentation.Outdent [76..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (for (x <- xs) yield d) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with indented enumerators inside an indented condition, `yield` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for
         |        x <- xs
         |      yield d
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |Indentation.Indent [36..36)
         |Ident(x) [45..46)
         |LeftArrow [47..49)
         |Ident(xs) [50..52)
         |Indentation.Outdent [52..52)
         |KwYield [59..64)
         |Ident(d) [65..66)
         |Indentation.Outdent [66..66)
         |KwThen [71..75)
         |Ident(g) [76..77)
         |KwElse [78..82)
         |Ident(h) [83..84)
         |LF [84..85)
         |Ident(k) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (for (x <- xs) yield d) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with parentheses and `yield` inside an indented condition, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for (x <- xs) yield d
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |LeftParen [37..38)
         |Ident(x) [38..39)
         |LeftArrow [40..42)
         |Ident(xs) [43..45)
         |RightParen [45..46)
         |KwYield [47..52)
         |Ident(d) [53..54)
         |Indentation.Outdent [54..54)
         |KwThen [59..63)
         |Ident(g) [64..65)
         |KwElse [66..70)
         |Ident(h) [71..72)
         |LF [72..73)
         |Ident(k) [77..78)
         |Indentation.Outdent [78..78)
         |Indentation.Outdent [78..78)
         |EOF [79..79)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (for (x <- xs) yield d) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses with `do` inside an indented `while` condition, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      for x <- xs do d
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwFor [36..39)
         |Ident(x) [40..41)
         |LeftArrow [42..44)
         |Ident(xs) [45..47)
         |KwDo [48..50)
         |Ident(d) [51..52)
         |Indentation.Outdent [52..52)
         |KwDo [57..59)
         |Ident(k) [60..61)
         |LF [61..62)
         |Ident(g) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses inside an indented condition, `yield` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for x <- xs
         |      yield d
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |Ident(x) [37..38)
         |LeftArrow [39..41)
         |Ident(xs) [42..44)
         |KwYield [51..56)
         |Ident(d) [57..58)
         |Indentation.Outdent [58..58)
         |KwThen [63..67)
         |Ident(g) [68..69)
         |KwElse [70..74)
         |Ident(h) [75..76)
         |LF [76..77)
         |Ident(k) [81..82)
         |Indentation.Outdent [82..82)
         |Indentation.Outdent [82..82)
         |EOF [83..83)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (for (x <- xs) yield d) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with indented enumerators inside an indented `while` condition, `do` on the next line, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      for
         |        x <- xs
         |      do d
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwFor [36..39)
         |Indentation.Indent [39..39)
         |Ident(x) [48..49)
         |LeftArrow [50..52)
         |Ident(xs) [53..55)
         |Indentation.Outdent [55..55)
         |LF [55..56)
         |KwDo [62..64)
         |Ident(d) [65..66)
         |Indentation.Outdent [66..66)
         |KwDo [71..73)
         |Ident(k) [74..75)
         |LF [75..76)
         |Ident(g) [80..81)
         |Indentation.Outdent [81..81)
         |Indentation.Outdent [81..81)
         |EOF [82..82)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses, `;` and an identifier inside an indented `while` condition, `do` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) b; c do d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(b) [46..47)
         |Semicolon [47..48)
         |Ident(c) [49..50)
         |Indentation.Outdent [49..49)
         |KwDo [51..53)
         |Ident(d) [54..55)
         |LF [55..56)
         |Ident(g) [60..61)
         |Indentation.Outdent [61..61)
         |Indentation.Outdent [61..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a) b
         |      c
         |    }) d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d")), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses in a value of indented enumerators, leading `||` on the next line with `then` and `else`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      y = if (a) && b
         |        || c then d else e
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |Ident(y) [48..49)
         |Equals [50..51)
         |KwIf [52..54)
         |LeftParen [55..56)
         |Ident(a) [56..57)
         |RightParen [57..58)
         |Ident(&&) [59..61)
         |Ident(b) [62..63)
         |InfixLF [63..64)
         |Ident(||) [72..74)
         |Ident(c) [75..76)
         |KwThen [77..81)
         |Ident(d) [82..83)
         |KwElse [84..88)
         |Ident(e) [89..90)
         |Indentation.Outdent [90..90)
         |LF [90..91)
         |KwDo [95..97)
         |Ident(g) [98..99)
         |LF [99..100)
         |Ident(k) [104..105)
         |Indentation.Outdent [105..105)
         |Indentation.Outdent [105..105)
         |EOF [106..106)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; y = if (a && b || c) d else e) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.For(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(
                patvar("y"),
                tif(
                  tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
                  tname("d"),
                  tname("e"),
                ),
              ),
            ),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and `else` in a value of indented enumerators, `yield` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      y = if (a) b else c
         |    yield y
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |Ident(y) [48..49)
         |Equals [50..51)
         |KwIf [52..54)
         |LeftParen [55..56)
         |Ident(a) [56..57)
         |RightParen [57..58)
         |Ident(b) [59..60)
         |KwElse [61..65)
         |Ident(c) [66..67)
         |Indentation.Outdent [67..67)
         |KwYield [72..77)
         |Ident(y) [78..79)
         |LF [79..80)
         |Ident(k) [84..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; y = if (a) b else c) yield y
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.ForYield(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(patvar("y"), tif(tname("a"), tname("b"), tname("c"))),
            ),
            tname("y"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with parentheses and `yield` in a value of indented enumerators, `yield` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      y = for (z <- zs) yield z
         |    yield y
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |Ident(y) [48..49)
         |Equals [50..51)
         |KwFor [52..55)
         |LeftParen [56..57)
         |Ident(z) [57..58)
         |LeftArrow [59..61)
         |Ident(zs) [62..64)
         |RightParen [64..65)
         |KwYield [66..71)
         |Ident(z) [72..73)
         |Indentation.Outdent [73..73)
         |KwYield [78..83)
         |Ident(y) [84..85)
         |LF [85..86)
         |Ident(k) [90..91)
         |Indentation.Outdent [91..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; y = for (z <- zs) yield z) yield y
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.ForYield(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(
                patvar("y"),
                Term.ForYield(List(Enumerator.Generator(patvar("z"), tname("zs"))), tname("z")),
              ),
            ),
            tname("y"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses with `do` in a value of indented enumerators, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      y = for z <- zs do z
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |Ident(y) [48..49)
         |Equals [50..51)
         |KwFor [52..55)
         |Ident(z) [56..57)
         |LeftArrow [58..60)
         |Ident(zs) [61..63)
         |KwDo [64..66)
         |Ident(z) [67..68)
         |Indentation.Outdent [68..68)
         |LF [68..69)
         |KwDo [73..75)
         |Ident(g) [76..77)
         |LF [77..78)
         |Ident(k) [82..83)
         |Indentation.Outdent [83..83)
         |Indentation.Outdent [83..83)
         |EOF [84..84)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; y = for (z <- zs) z) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.For(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(
                patvar("y"),
                Term.For(List(Enumerator.Generator(patvar("z"), tname("zs"))), tname("z")),
              ),
            ),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses in a guard of indented enumerators, leading `||` on the next line, `yield` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      if (a) && b
         |        || c
         |    yield x
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |KwIf [48..50)
         |LeftParen [51..52)
         |Ident(a) [52..53)
         |RightParen [53..54)
         |Ident(&&) [55..57)
         |Ident(b) [58..59)
         |InfixLF [59..60)
         |Ident(||) [68..70)
         |Ident(c) [71..72)
         |Indentation.Outdent [72..72)
         |KwYield [77..82)
         |Ident(x) [83..84)
         |LF [84..85)
         |Ident(k) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; if a && b || c) yield x
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.ForYield(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Guard(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c"))),
            ),
            tname("x"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses in braces inside an indented condition, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      { if (a) && b
         |        || c then d else e }
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |LeftBrace [33..34)
         |KwIf [35..37)
         |LeftParen [38..39)
         |Ident(a) [39..40)
         |RightParen [40..41)
         |Ident(&&) [42..44)
         |Ident(b) [45..46)
         |InfixLF [46..47)
         |Ident(||) [55..57)
         |Ident(c) [58..59)
         |KwThen [60..64)
         |Ident(d) [65..66)
         |KwElse [67..71)
         |Ident(e) [72..73)
         |RightBrace [74..75)
         |Indentation.Outdent [75..75)
         |KwThen [80..84)
         |Ident(g) [85..86)
         |KwElse [87..91)
         |Ident(h) [92..93)
         |LF [93..94)
         |Ident(k) [98..99)
         |Indentation.Outdent [99..99)
         |Indentation.Outdent [99..99)
         |EOF [100..100)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a && b || c) d else e
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            )),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses in parentheses inside an indented condition, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      (if (a) && b
         |        || c then d else e)
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |LeftParen [33..34)
         |KwIf [34..36)
         |LeftParen [37..38)
         |Ident(a) [38..39)
         |RightParen [39..40)
         |Ident(&&) [41..43)
         |Ident(b) [44..45)
         |InfixLF [45..46)
         |Ident(||) [54..56)
         |Ident(c) [57..58)
         |KwThen [59..63)
         |Ident(d) [64..65)
         |KwElse [66..70)
         |Ident(e) [71..72)
         |RightParen [72..73)
         |Indentation.Outdent [73..73)
         |KwThen [78..82)
         |Ident(g) [83..84)
         |KwElse [85..89)
         |Ident(h) [90..91)
         |LF [91..92)
         |Ident(k) [96..97)
         |Indentation.Outdent [97..97)
         |Indentation.Outdent [97..97)
         |EOF [98..98)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses in braces inside an indented `while` condition, leading `||` on the next line with `do`, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      { while (a) && b
         |        || c do d }
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |LeftBrace [36..37)
         |KwWhile [38..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(&&) [48..50)
         |Ident(b) [51..52)
         |InfixLF [52..53)
         |Ident(||) [61..63)
         |Ident(c) [64..65)
         |KwDo [66..68)
         |Ident(d) [69..70)
         |RightBrace [71..72)
         |Indentation.Outdent [72..72)
         |KwDo [77..79)
         |Ident(k) [80..81)
         |LF [81..82)
         |Ident(g) [86..87)
         |Indentation.Outdent [87..87)
         |Indentation.Outdent [87..87)
         |EOF [88..88)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a && b || c) d
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d"))),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses with `do` in parentheses inside an indented `while` condition, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      (for x <- xs do d)
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |LeftParen [36..37)
         |KwFor [37..40)
         |Ident(x) [41..42)
         |LeftArrow [43..45)
         |Ident(xs) [46..48)
         |KwDo [49..51)
         |Ident(d) [52..53)
         |RightParen [53..54)
         |Indentation.Outdent [54..54)
         |KwDo [59..61)
         |Ident(k) [62..63)
         |LF [63..64)
         |Ident(g) [68..69)
         |Indentation.Outdent [69..69)
         |Indentation.Outdent [69..69)
         |EOF [70..70)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses, `;` and an identifier in braces on separate lines inside an indented condition, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      {
         |        if (a) b; c
         |      }
         |    then d else e
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |LeftBrace [33..34)
         |KwIf [43..45)
         |LeftParen [46..47)
         |Ident(a) [47..48)
         |RightParen [48..49)
         |Ident(b) [50..51)
         |Semicolon [51..52)
         |Ident(c) [53..54)
         |LF [54..55)
         |RightBrace [61..62)
         |Indentation.Outdent [62..62)
         |KwThen [67..71)
         |Ident(d) [72..73)
         |KwElse [74..78)
         |Ident(e) [79..80)
         |LF [80..81)
         |Ident(k) [85..86)
         |Indentation.Outdent [86..86)
         |Indentation.Outdent [86..86)
         |EOF [87..87)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      c
         |    }) d else e
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(blk(tif(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with a colon argument after the parentheses in parentheses inside an indented condition, a deeper `if` on the next line, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      (if (a) f:
         |          if b then c else d
         |      )
         |    then y else z
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |LeftParen [33..34)
         |KwIf [34..36)
         |LeftParen [37..38)
         |Ident(a) [38..39)
         |RightParen [39..40)
         |Ident(f) [41..42)
         |Colon [42..43)
         |Indentation.Indent [43..43)
         |KwIf [54..56)
         |Ident(b) [57..58)
         |KwThen [59..63)
         |Ident(c) [64..65)
         |KwElse [66..70)
         |Ident(d) [71..72)
         |Indentation.Outdent [72..72)
         |RightParen [79..80)
         |Indentation.Outdent [80..80)
         |KwThen [85..89)
         |Ident(y) [90..91)
         |KwElse [92..96)
         |Ident(z) [97..98)
         |LF [98..99)
         |Ident(g) [103..104)
         |Indentation.Outdent [104..104)
         |Indentation.Outdent [104..104)
         |EOF [105..105)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) f {
         |      if (b) c else d
         |    }) y else z
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tname("a"), tapply(tname("f"), blk(tif(tname("b"), tname("c"), tname("d"))))),
            tname("y"),
            tname("z"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |InfixLF [47..48)
         |Ident(&&) [54..56)
         |Ident(w) [57..58)
         |Indentation.Outdent [58..58)
         |KwThen [63..67)
         |Ident(c) [68..69)
         |KwElse [70..74)
         |Ident(d) [75..76)
         |LF [76..77)
         |KwDef [79..82)
         |Ident(g) [83..84)
         |Equals [85..86)
         |Constant.Int(1) [87..88)
         |Indentation.Outdent [88..88)
         |EOF [89..89)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z && w
         |  }) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses, `else`, `;` and an identifier inside an indented condition, `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) b else c; d then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(b) [40..41)
         |KwElse [42..46)
         |Ident(c) [47..48)
         |Semicolon [48..49)
         |Ident(d) [50..51)
         |Indentation.Outdent [50..50)
         |KwThen [52..56)
         |Ident(g) [57..58)
         |KwElse [59..63)
         |Ident(h) [64..65)
         |LF [65..66)
         |Ident(k) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b else c
         |      d
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(blk(tif(tname("a"), tname("b"), tname("c")), tname("d")), tname("g"), tname("h")),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses with `do`, `;` and an identifier inside an indented condition, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for x <- xs do d; e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |Ident(x) [37..38)
         |LeftArrow [39..41)
         |Ident(xs) [42..44)
         |KwDo [45..47)
         |Ident(d) [48..49)
         |Semicolon [49..50)
         |Ident(e) [51..52)
         |Indentation.Outdent [52..52)
         |KwThen [57..61)
         |Ident(g) [62..63)
         |KwElse [64..68)
         |Ident(h) [69..70)
         |LF [70..71)
         |Ident(k) [75..76)
         |Indentation.Outdent [76..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      for (x <- xs) d
         |      e
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(
              Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `then` and `else`, `;` and an identifier inside an indented condition, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if a then b else c; d
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |Ident(a) [36..37)
         |KwThen [38..42)
         |Ident(b) [43..44)
         |KwElse [45..49)
         |Ident(c) [50..51)
         |Semicolon [51..52)
         |Ident(d) [53..54)
         |Indentation.Outdent [54..54)
         |KwThen [59..63)
         |Ident(g) [64..65)
         |KwElse [66..70)
         |Ident(h) [71..72)
         |LF [72..73)
         |Ident(k) [77..78)
         |Indentation.Outdent [78..78)
         |Indentation.Outdent [78..78)
         |EOF [79..79)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b else c
         |      d
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(blk(tif(tname("a"), tname("b"), tname("c")), tname("d")), tname("g"), tname("h")),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses inside an indented `while` condition, `do` on the next line, a blank line, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      for x <- xs
         |      do d
         |
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwFor [36..39)
         |Ident(x) [40..41)
         |LeftArrow [42..44)
         |Ident(xs) [45..47)
         |LF [47..48)
         |KwDo [54..56)
         |Ident(d) [57..58)
         |Indentation.Outdent [59..59)
         |KwDo [64..66)
         |Ident(k) [67..68)
         |LF [68..69)
         |Ident(g) [73..74)
         |Indentation.Outdent [74..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a select after the parentheses, `;` and an identifier inside an indented `while` condition after a comment line, `do` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      // x
         |      if (a) b.g; c do d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [40..40)
         |KwIf [47..49)
         |LeftParen [50..51)
         |Ident(a) [51..52)
         |RightParen [52..53)
         |Ident(b) [54..55)
         |Dot [55..56)
         |Ident(g) [56..57)
         |Semicolon [57..58)
         |Ident(c) [59..60)
         |Indentation.Outdent [59..59)
         |KwDo [61..63)
         |Ident(d) [64..65)
         |LF [65..66)
         |Ident(g) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      // x
         |      if (a) b.g
         |      c
         |    }) d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              tif(tname("a"), tselect(tname("b"), "g")).toBuilder.begComment(Seq("// x")).result(),
              tname("c"),
            ),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses, `;` and an identifier inside an indented condition, a comment between `;` and the identifier, `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) b; /* x */ c then d else e
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(b) [40..41)
         |Semicolon [41..42)
         |Ident(c) [51..52)
         |Indentation.Outdent [51..51)
         |KwThen [53..57)
         |Ident(d) [58..59)
         |KwElse [60..64)
         |Ident(e) [65..66)
         |LF [66..67)
         |Ident(k) [71..72)
         |Indentation.Outdent [72..72)
         |Indentation.Outdent [72..72)
         |EOF [73..73)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      /* x */ c
         |    }) d else e
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(tif(tname("a"), tname("b")), tnameComments("c")("/* x */")()),
            tname("d"),
            tname("e"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses on the line of the outer `if`, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) && b
         |      || c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(&&) [34..36)
         |Ident(b) [37..38)
         |InfixLF [38..39)
         |Ident(||) [45..47)
         |Ident(c) [48..49)
         |KwThen [50..54)
         |Ident(d) [55..56)
         |KwElse [57..61)
         |Ident(e) [62..63)
         |KwThen [68..72)
         |Ident(g) [73..74)
         |KwElse [75..79)
         |Ident(h) [80..81)
         |LF [81..82)
         |Ident(k) [86..87)
         |Indentation.Outdent [87..87)
         |Indentation.Outdent [87..87)
         |EOF [88..88)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and `else` on the line of the outer `if`, outer `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) b else c then d else e
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(b) [34..35)
         |KwElse [36..40)
         |Ident(c) [41..42)
         |KwThen [43..47)
         |Ident(d) [48..49)
         |KwElse [50..54)
         |Ident(e) [55..56)
         |LF [56..57)
         |Ident(k) [61..62)
         |Indentation.Outdent [62..62)
         |Indentation.Outdent [62..62)
         |EOF [63..63)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) b else c) d else e
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tif(tname("a"), tname("b"), tname("c")), tname("d"), tname("e")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses on the line of the outer `while`, `do` on the next line, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while for x <- xs
         |    do d
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwFor [30..33)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |KwDo [46..48)
         |Ident(d) [49..50)
         |KwDo [55..57)
         |Ident(k) [58..59)
         |LF [59..60)
         |Ident(g) [64..65)
         |Indentation.Outdent [65..65)
         |Indentation.Outdent [65..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and `else` on the line of the outer `if`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) b else c
         |    then d else e
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(b) [34..35)
         |KwElse [36..40)
         |Ident(c) [41..42)
         |KwThen [47..51)
         |Ident(d) [52..53)
         |KwElse [54..58)
         |Ident(e) [59..60)
         |LF [60..61)
         |Ident(k) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) b else c) d else e
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tif(tname("a"), tname("b"), tname("c")), tname("d"), tname("e")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` without parentheses with `then` at the line end inside an indented `while` condition, the body and outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if a then
         |        b do c
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |Ident(a) [39..40)
         |KwThen [41..45)
         |Indentation.Indent [45..45)
         |Ident(b) [54..55)
         |Indentation.Outdent [54..54)
         |Indentation.Outdent [54..54)
         |KwDo [56..58)
         |Ident(c) [59..60)
         |LF [60..61)
         |Ident(k) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if (a) b) c
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(tif(tname("a"), tname("b")), tname("c")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` without parentheses with `then` at the line end inside an indented `while` condition, the body on the next line, outer `do` on the line after it at the column of the body, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if a then
         |        b
         |        do c
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |Ident(a) [39..40)
         |KwThen [41..45)
         |Indentation.Indent [45..45)
         |Ident(b) [54..55)
         |LF [55..56)
         |Indentation.Outdent [55..55)
         |Indentation.Outdent [55..55)
         |KwDo [64..66)
         |Ident(c) [67..68)
         |LF [68..69)
         |Ident(k) [73..74)
         |Indentation.Outdent [74..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if (a) b) c
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(tif(tname("a"), tname("b")), tname("c")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` without parentheses with `then` at the line end inside an indented `while` condition, the body, a comment and outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if a then
         |        b /* x */ do c
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |Ident(a) [39..40)
         |KwThen [41..45)
         |Indentation.Indent [45..45)
         |Ident(b) [54..55)
         |Indentation.Outdent [63..63)
         |Indentation.Outdent [63..63)
         |KwDo [64..66)
         |Ident(c) [67..68)
         |LF [68..69)
         |Ident(k) [73..74)
         |Indentation.Outdent [74..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if (a) b /* x */ ) c
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tif(tname("a"), tname("b")).toBuilder.endComment(Seq("/* x */")).result(),
            tname("c"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses and `then` at the line end inside an indented condition, the body with `else` and outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b then
         |        c else d then e else g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |KwThen [45..49)
         |Indentation.Indent [49..49)
         |Ident(c) [58..59)
         |KwElse [60..64)
         |Ident(d) [65..66)
         |Indentation.Outdent [65..65)
         |Indentation.Outdent [65..65)
         |KwThen [67..71)
         |Ident(e) [72..73)
         |KwElse [74..78)
         |Ident(g) [79..80)
         |LF [80..81)
         |Ident(k) [85..86)
         |Indentation.Outdent [86..86)
         |Indentation.Outdent [86..86)
         |EOF [87..87)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b) c else d) e else g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")),
            tname("e"),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` without parentheses with `do` at the line end inside an indented condition, the body and outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      while a do
         |        b then c else d
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwWhile [33..38)
         |Ident(a) [39..40)
         |KwDo [41..43)
         |Indentation.Indent [43..43)
         |Ident(b) [52..53)
         |Indentation.Outdent [52..52)
         |Indentation.Outdent [52..52)
         |KwThen [54..58)
         |Ident(c) [59..60)
         |KwElse [61..65)
         |Ident(d) [66..67)
         |LF [67..68)
         |Ident(k) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (while (a) b) c else d
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(Term.While(tname("a"), tname("b")), tname("c"), tname("d")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and an interpolated string with an `if` in its splice inside an indented condition, `then` and `else` on the same line, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) f(s"${if b then c else d}") then g else h
         |    then k else m
         |    n
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(f) [40..41)
         |LeftParen [41..42)
         |Interpolation.Id(s) [42..43)
         |Interpolation.Start(") [43..44)
         |Interpolation.Part() [44..44)
         |Interpolation.SpliceStart [44..45)
         |LeftBrace [45..46)
         |KwIf [46..48)
         |Ident(b) [49..50)
         |KwThen [51..55)
         |Ident(c) [56..57)
         |KwElse [58..62)
         |Ident(d) [63..64)
         |RightBrace [64..65)
         |Interpolation.SpliceEnd [65..65)
         |Interpolation.Part() [65..65)
         |Interpolation.End(") [65..66)
         |RightParen [66..67)
         |KwThen [68..72)
         |Ident(g) [73..74)
         |KwElse [75..79)
         |Ident(h) [80..81)
         |Indentation.Outdent [81..81)
         |KwThen [86..90)
         |Ident(k) [91..92)
         |KwElse [93..97)
         |Ident(m) [98..99)
         |LF [99..100)
         |Ident(n) [104..105)
         |Indentation.Outdent [105..105)
         |Indentation.Outdent [105..105)
         |EOF [106..106)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a f (s"${if (b) c else d}")) g else h) k else m
         |    n
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(
                tname("a"),
                tname("f"),
                Term.Interpolate(
                  tname("s"),
                  List(lit(""), lit("")),
                  List(blk(tif(tname("b"), tname("c"), tname("d")))),
                ),
              ),
              tname("g"),
              tname("h"),
            ),
            tname("k"),
            tname("m"),
          ),
          tname("n"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `try` with `catch case`, an `if` with parentheses, `;` and an identifier inside an indented condition, outer `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      try a catch case _ => if (b) c; d then e else g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwTry [33..36)
         |Ident(a) [37..38)
         |KwCatch [39..44)
         |Indentation.Indent [45..45)
         |KwCase [45..49)
         |Underscore [50..51)
         |RightArrow [52..54)
         |KwIf [55..57)
         |LeftParen [58..59)
         |Ident(b) [59..60)
         |RightParen [60..61)
         |Ident(c) [62..63)
         |Semicolon [63..64)
         |Ident(d) [65..66)
         |Indentation.Outdent [65..65)
         |Indentation.Outdent [65..65)
         |KwThen [67..71)
         |Ident(e) [72..73)
         |KwElse [74..78)
         |Ident(g) [79..80)
         |LF [80..81)
         |Ident(k) [85..86)
         |Indentation.Outdent [86..86)
         |Indentation.Outdent [86..86)
         |EOF [87..87)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (try a catch {
         |      case _ =>
         |        if (b) c
         |        d
         |    }) e else g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Try(
              tname("a"),
              List(Case(patwildcard, None, blk(tif(tname("b"), tname("c")), tname("d")))),
              None,
            ),
            tname("e"),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `try` with `catch case`, `;` and an identifier inside an indented condition, outer `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      try a catch case _ => c; d then e else g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwTry [33..36)
         |Ident(a) [37..38)
         |KwCatch [39..44)
         |Indentation.Indent [45..45)
         |KwCase [45..49)
         |Underscore [50..51)
         |RightArrow [52..54)
         |Ident(c) [55..56)
         |Semicolon [56..57)
         |Ident(d) [58..59)
         |Indentation.Outdent [58..58)
         |Indentation.Outdent [58..58)
         |KwThen [60..64)
         |Ident(e) [65..66)
         |KwElse [67..71)
         |Ident(g) [72..73)
         |LF [73..74)
         |Ident(k) [78..79)
         |Indentation.Outdent [79..79)
         |Indentation.Outdent [79..79)
         |EOF [80..80)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (try a catch {
         |      case _ =>
         |        c
         |        d
         |    }) e else g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Try(tname("a"), List(Case(patwildcard, None, blk(tname("c"), tname("d")))), None),
            tname("e"),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `==` after the parentheses in an indented `val` body, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      if (a) == b
         |    then 1 else 2
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwIf [38..40)
         |LeftParen [41..42)
         |Ident(a) [42..43)
         |RightParen [43..44)
         |Ident(==) [45..47)
         |Ident(b) [48..49)
         |Indentation.Outdent [49..49)
         |KwThen [54..58)
         |Constant.Int(1) [59..60)
         |KwElse [61..65)
         |Constant.Int(2) [66..67)
         |LF [67..68)
         |Ident(r) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `match` in an indented `then` body, `else` on the line of the last case, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a then
         |      x match
         |        case 1 => b
         |        case _ => c else d
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |KwThen [29..33)
         |Indentation.Indent [33..33)
         |Ident(x) [40..41)
         |KwMatch [42..47)
         |Indentation.Indent [47..47)
         |KwCase [56..60)
         |Constant.Int(1) [61..62)
         |RightArrow [63..65)
         |Ident(b) [66..67)
         |LF [67..68)
         |KwCase [76..80)
         |Underscore [81..82)
         |RightArrow [83..85)
         |Ident(c) [86..87)
         |KwElse [88..92)
         |Ident(d) [93..94)
         |Indentation.Outdent [94..94)
         |Indentation.Outdent [94..94)
         |LF [94..95)
         |Ident(k) [99..100)
         |Indentation.Outdent [100..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `outdent` expected but `else` found
         |        case _ => c else d
         |                    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: two statements in an indented `then` body, `else` on the line of the second one, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a then
         |      b
         |      c else d
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |KwThen [29..33)
         |Indentation.Indent [33..33)
         |Ident(b) [40..41)
         |LF [41..42)
         |Ident(c) [48..49)
         |KwElse [50..54)
         |Ident(d) [55..56)
         |Indentation.Outdent [56..56)
         |LF [56..57)
         |Ident(k) [61..62)
         |Indentation.Outdent [62..62)
         |Indentation.Outdent [62..62)
         |EOF [63..63)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a) {
         |      b
         |      c
         |    } else d
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tname("a"), blk(tname("b"), tname("c")), tname("d")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `new` on the next line, `then` and `else` on the same line, a single-line comment at the column of the condition, next definition") {
    val code =
      """|def f =
         |  if
         |    new B(a).ok then c else d
         |    // comment
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwNew [17..20)
         |Ident(B) [21..22)
         |LeftParen [22..23)
         |Ident(a) [23..24)
         |RightParen [24..25)
         |Dot [25..26)
         |Ident(ok) [26..28)
         |Indentation.Outdent [26..26)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |Indentation.Outdent [57..57)
         |KwDef [58..61)
         |Ident(g) [62..63)
         |Equals [64..65)
         |Constant.Int(1) [66..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c else d
         |  // comment
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"), tname("d"))
            .toBuilder.endComment(Seq("// comment")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `new` on the next line, `then` and `else` on the same line, a blank line, a multi-line comment at the column of the condition, end of file") {
    val code =
      """|def f =
         |  if
         |    new B(a).ok then c else d
         |
         |    /* comment */
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwNew [17..20)
         |Ident(B) [21..22)
         |LeftParen [22..23)
         |Ident(a) [23..24)
         |RightParen [24..25)
         |Dot [25..26)
         |Ident(ok) [26..28)
         |Indentation.Outdent [26..26)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |Indentation.Outdent [61..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c else d
         |
         |  /* comment */
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"), tname("d"))
          .toBuilder.endComment(Seq("/* comment */")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `new` on the next line, `then` and `else` on the same line, a multi-line comment at the column of the `if`, next definition") {
    val code =
      """|def f =
         |  if
         |    new B(a).ok then c else d
         |  /* comment */
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwNew [17..20)
         |Ident(B) [21..22)
         |LeftParen [22..23)
         |Ident(a) [23..24)
         |RightParen [24..25)
         |Dot [25..26)
         |Ident(ok) [26..28)
         |Indentation.Outdent [26..26)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |Indentation.Outdent [58..58)
         |KwDef [59..62)
         |Ident(g) [63..64)
         |Equals [65..66)
         |Constant.Int(1) [67..68)
         |EOF [69..69)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c else d
         |  /* comment */
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"), tname("d"))
            .toBuilder.endComment(Seq("/* comment */")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `new` on the next line, `then` and `else` on the same line, a blank line, a single-line comment at the column of the `if`, end of file") {
    val code =
      """|def f =
         |  if
         |    new B(a).ok then c else d
         |
         |  // comment
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwNew [17..20)
         |Ident(B) [21..22)
         |LeftParen [22..23)
         |Ident(a) [23..24)
         |RightParen [24..25)
         |Dot [25..26)
         |Ident(ok) [26..28)
         |Indentation.Outdent [26..26)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |Indentation.Outdent [56..56)
         |EOF [57..57)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c else d
         |
         |  // comment
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"), tname("d"))
          .toBuilder.endComment(Seq("// comment")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `new` on the next line, `do` on the same line, a single-line comment at the column of the condition, next definition") {
    val code =
      """|def f =
         |  while
         |    new B(a).ok do c
         |    // comment
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |LeftParen [25..26)
         |Ident(a) [26..27)
         |RightParen [27..28)
         |Dot [28..29)
         |Ident(ok) [29..31)
         |Indentation.Outdent [29..29)
         |KwDo [32..34)
         |Ident(c) [35..36)
         |Indentation.Outdent [51..51)
         |KwDef [52..55)
         |Ident(g) [56..57)
         |Equals [58..59)
         |Constant.Int(1) [60..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (new B(a).ok) c
         |  // comment
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"))
            .toBuilder.endComment(Seq("// comment")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `new` on the next line, `do` on the same line, a blank line, a multi-line comment at the column of the condition, end of file") {
    val code =
      """|def f =
         |  while
         |    new B(a).ok do c
         |
         |    /* comment */
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |LeftParen [25..26)
         |Ident(a) [26..27)
         |RightParen [27..28)
         |Dot [28..29)
         |Ident(ok) [29..31)
         |Indentation.Outdent [29..29)
         |KwDo [32..34)
         |Ident(c) [35..36)
         |Indentation.Outdent [55..55)
         |EOF [56..56)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (new B(a).ok) c
         |
         |  /* comment */
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"))
          .toBuilder.endComment(Seq("/* comment */")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `new` on the next line, `do` on the same line, a multi-line comment at the column of the `while`, next definition") {
    val code =
      """|def f =
         |  while
         |    new B(a).ok do c
         |  /* comment */
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |LeftParen [25..26)
         |Ident(a) [26..27)
         |RightParen [27..28)
         |Dot [28..29)
         |Ident(ok) [29..31)
         |Indentation.Outdent [29..29)
         |KwDo [32..34)
         |Ident(c) [35..36)
         |Indentation.Outdent [52..52)
         |KwDef [53..56)
         |Ident(g) [57..58)
         |Equals [59..60)
         |Constant.Int(1) [61..62)
         |EOF [63..63)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (new B(a).ok) c
         |  /* comment */
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"))
            .toBuilder.endComment(Seq("/* comment */")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `new` on the next line, `do` on the same line, a blank line, a single-line comment at the column of the `while`, end of file") {
    val code =
      """|def f =
         |  while
         |    new B(a).ok do c
         |
         |  // comment
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |LeftParen [25..26)
         |Ident(a) [26..27)
         |RightParen [27..28)
         |Dot [28..29)
         |Ident(ok) [29..31)
         |Indentation.Outdent [29..29)
         |KwDo [32..34)
         |Ident(c) [35..36)
         |Indentation.Outdent [50..50)
         |EOF [51..51)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (new B(a).ok) c
         |
         |  // comment
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"))
          .toBuilder.endComment(Seq("// comment")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses on the next line, `then` and `else` on the same line, a single-line comment at the column of the condition, next definition") {
    val code =
      """|def f =
         |  if
         |    (a) && b then c else d
         |    // comment
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |Ident(&&) [21..23)
         |Ident(b) [24..25)
         |Indentation.Outdent [24..24)
         |KwThen [26..30)
         |Ident(c) [31..32)
         |KwElse [33..37)
         |Ident(d) [38..39)
         |Indentation.Outdent [54..54)
         |KwDef [55..58)
         |Ident(g) [59..60)
         |Equals [61..62)
         |Constant.Int(1) [63..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (a && b) c else d
         |  // comment
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")).toBuilder
            .endComment(Seq("// comment")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses on the next line, `then` and `else` on the same line, a blank line, a multi-line comment at the column of the condition, end of file") {
    val code =
      """|def f =
         |  if
         |    (a) && b then c else d
         |
         |    /* comment */
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |Ident(&&) [21..23)
         |Ident(b) [24..25)
         |Indentation.Outdent [24..24)
         |KwThen [26..30)
         |Ident(c) [31..32)
         |KwElse [33..37)
         |Ident(d) [38..39)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (a && b) c else d
         |
         |  /* comment */
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")).toBuilder
          .endComment(Seq("/* comment */")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses on the next line, `then` and `else` on the same line, a multi-line comment at the column of the `if`, next definition") {
    val code =
      """|def f =
         |  if
         |    (a) && b then c else d
         |  /* comment */
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |Ident(&&) [21..23)
         |Ident(b) [24..25)
         |Indentation.Outdent [24..24)
         |KwThen [26..30)
         |Ident(c) [31..32)
         |KwElse [33..37)
         |Ident(d) [38..39)
         |Indentation.Outdent [55..55)
         |KwDef [56..59)
         |Ident(g) [60..61)
         |Equals [62..63)
         |Constant.Int(1) [64..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (a && b) c else d
         |  /* comment */
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")).toBuilder
            .endComment(Seq("/* comment */")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses on the next line, `then` and `else` on the same line, a blank line, a single-line comment at the column of the `if`, end of file") {
    val code =
      """|def f =
         |  if
         |    (a) && b then c else d
         |
         |  // comment
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |Ident(&&) [21..23)
         |Ident(b) [24..25)
         |Indentation.Outdent [24..24)
         |KwThen [26..30)
         |Ident(c) [31..32)
         |KwElse [33..37)
         |Ident(d) [38..39)
         |Indentation.Outdent [53..53)
         |EOF [54..54)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (a && b) c else d
         |
         |  // comment
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")).toBuilder
          .endComment(Seq("// comment")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses on the next line, `do` on the same line, a single-line comment at the column of the condition, next definition") {
    val code =
      """|def f =
         |  while
         |    (a) && b do c
         |    // comment
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |Ident(&&) [24..26)
         |Ident(b) [27..28)
         |Indentation.Outdent [27..27)
         |KwDo [29..31)
         |Ident(c) [32..33)
         |Indentation.Outdent [48..48)
         |KwDef [49..52)
         |Ident(g) [53..54)
         |Equals [55..56)
         |Constant.Int(1) [57..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (a && b) c
         |  // comment
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")).toBuilder
            .endComment(Seq("// comment")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses on the next line, `do` on the same line, a blank line, a multi-line comment at the column of the condition, end of file") {
    val code =
      """|def f =
         |  while
         |    (a) && b do c
         |
         |    /* comment */
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |Ident(&&) [24..26)
         |Ident(b) [27..28)
         |Indentation.Outdent [27..27)
         |KwDo [29..31)
         |Ident(c) [32..33)
         |Indentation.Outdent [52..52)
         |EOF [53..53)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (a && b) c
         |
         |  /* comment */
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")).toBuilder
          .endComment(Seq("/* comment */")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses on the next line, `do` on the same line, a multi-line comment at the column of the `while`, next definition") {
    val code =
      """|def f =
         |  while
         |    (a) && b do c
         |  /* comment */
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |Ident(&&) [24..26)
         |Ident(b) [27..28)
         |Indentation.Outdent [27..27)
         |KwDo [29..31)
         |Ident(c) [32..33)
         |Indentation.Outdent [49..49)
         |KwDef [50..53)
         |Ident(g) [54..55)
         |Equals [56..57)
         |Constant.Int(1) [58..59)
         |EOF [60..60)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (a && b) c
         |  /* comment */
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")).toBuilder
            .endComment(Seq("/* comment */")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses on the next line, `do` on the same line, a blank line, a single-line comment at the column of the `while`, end of file") {
    val code =
      """|def f =
         |  while
         |    (a) && b do c
         |
         |  // comment
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |Ident(&&) [24..26)
         |Ident(b) [27..28)
         |Indentation.Outdent [27..27)
         |KwDo [29..31)
         |Ident(c) [32..33)
         |Indentation.Outdent [47..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (a && b) c
         |
         |  // comment
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")).toBuilder
          .endComment(Seq("// comment")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and a `match` with braces inside an indented condition, outer `then` and `else` on the next line at the column of the inner `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) x match { case _ => true }
         |      then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(x) [40..41)
         |KwMatch [42..47)
         |LeftBrace [48..49)
         |KwCase [50..54)
         |Underscore [55..56)
         |RightArrow [57..59)
         |KwTrue [60..64)
         |RightBrace [65..66)
         |Indentation.Outdent [66..66)
         |KwThen [73..77)
         |Constant.Int(1) [78..79)
         |KwElse [80..84)
         |Constant.Int(2) [85..86)
         |LF [86..87)
         |Ident(g) [91..92)
         |Indentation.Outdent [92..92)
         |Indentation.Outdent [92..92)
         |EOF [93..93)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) x match {
         |      case _ => true
         |    }) 1 else 2
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tname("a"), tmatch(tname("x"), Case(patwildcard, None, bool(true)))),
            int(1),
            int(2),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses, `;` and an identifier inside an indented condition, outer `then` and `else` on the next line at the column of the inner `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) b; c
         |      then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(b) [40..41)
         |Semicolon [41..42)
         |Ident(c) [43..44)
         |Indentation.Outdent [44..44)
         |KwThen [51..55)
         |Constant.Int(1) [56..57)
         |KwElse [58..62)
         |Constant.Int(2) [63..64)
         |LF [64..65)
         |Ident(g) [69..70)
         |Indentation.Outdent [70..70)
         |Indentation.Outdent [70..70)
         |EOF [71..71)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      c
         |    }) 1 else 2
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(blk(tif(tname("a"), tname("b")), tname("c")), int(1), int(2)), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses, `;` and an identifier inside an indented `while` condition, outer `do` on the next line at the column of the inner `while`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) b; c
         |      do d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(b) [46..47)
         |Semicolon [47..48)
         |Ident(c) [49..50)
         |LF [50..51)
         |Indentation.Outdent [50..50)
         |KwDo [57..59)
         |Ident(d) [60..61)
         |LF [61..62)
         |Ident(g) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a) b
         |      c
         |    }) d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d")), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `then` and an identifier on the next line inside an indented condition, outer `then` and `else` on the next line at the column of the inner `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if a then b
         |      c
         |      then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |Ident(a) [36..37)
         |KwThen [38..42)
         |Ident(b) [43..44)
         |LF [44..45)
         |Ident(c) [51..52)
         |Indentation.Outdent [52..52)
         |KwThen [59..63)
         |Constant.Int(1) [64..65)
         |KwElse [66..70)
         |Constant.Int(2) [71..72)
         |LF [72..73)
         |Ident(g) [77..78)
         |Indentation.Outdent [78..78)
         |Indentation.Outdent [78..78)
         |EOF [79..79)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      c
         |    }) 1 else 2
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(blk(tif(tname("a"), tname("b")), tname("c")), int(1), int(2)), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses at the line end, an argument in parentheses with `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (a)
         |      (x) then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Indentation.Indent [30..30)
         |LeftParen [37..38)
         |Ident(x) [38..39)
         |RightParen [39..40)
         |KwThen [41..45)
         |Constant.Int(1) [46..47)
         |KwElse [48..52)
         |Constant.Int(2) [53..54)
         |Indentation.Outdent [54..54)
         |LF [54..55)
         |Ident(g) [59..60)
         |Indentation.Outdent [60..60)
         |Indentation.Outdent [60..60)
         |EOF [61..61)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `then` found
         |      (x) then 1 else 2
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end inside an indented condition, an argument in parentheses with `then` and `else` on the next line, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a)
         |        (x) then 1 else 2
         |    then 3 else 4
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Indentation.Indent [39..39)
         |LeftParen [48..49)
         |Ident(x) [49..50)
         |RightParen [50..51)
         |Indentation.Outdent [50..50)
         |Indentation.Outdent [50..50)
         |KwThen [52..56)
         |Constant.Int(1) [57..58)
         |KwElse [59..63)
         |Constant.Int(2) [64..65)
         |KwThen [70..74)
         |Constant.Int(3) [75..76)
         |KwElse [77..81)
         |Constant.Int(4) [82..83)
         |LF [83..84)
         |Ident(g) [88..89)
         |Indentation.Outdent [89..89)
         |Indentation.Outdent [89..89)
         |EOF [90..90)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `outdent` expected but `then` found
         |    then 3 else 4
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses and an argument with `new` after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) g(new B)
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [34..35)
         |LeftParen [35..36)
         |KwNew [36..39)
         |Ident(B) [40..41)
         |RightParen [41..42)
         |KwThen [47..51)
         |Constant.Int(1) [52..53)
         |KwElse [58..62)
         |Constant.Int(2) [63..64)
         |Indentation.Outdent [64..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) g(new B)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tapply(tname("g"), Term.New(init(pname("B"))))), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses in an `if` with parentheses and `else` in an `if` with parentheses, outer `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if (x) if (c) while (a) b else f
         |    else g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(x) [28..29)
         |RightParen [29..30)
         |KwIf [31..33)
         |LeftParen [34..35)
         |Ident(c) [35..36)
         |RightParen [36..37)
         |KwWhile [38..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(b) [48..49)
         |KwElse [50..54)
         |Ident(f) [55..56)
         |KwElse [61..65)
         |Ident(g) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A { def f = if (x) if (c) while (a) b else f else g }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tname("x"), tif(tname("c"), Term.While(tname("a"), tname("b")), tname("f")), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and an argument with `new` after them on the line of the outer `if`, outer `then` and `else` on the same line") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) g(new B) then 1 else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [34..35)
         |LeftParen [35..36)
         |KwNew [36..39)
         |Ident(B) [40..41)
         |RightParen [41..42)
         |KwThen [43..47)
         |Constant.Int(1) [48..49)
         |KwElse [50..54)
         |Constant.Int(2) [55..56)
         |Indentation.Outdent [56..56)
         |Indentation.Outdent [56..56)
         |EOF [57..57)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) g(new B)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tapply(tname("g"), Term.New(init(pname("B"))))), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses and an argument with `new` after them on the line of the outer `while`, outer `do` on the same line") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) g(new B) do h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(g) [40..41)
         |LeftParen [41..42)
         |KwNew [42..45)
         |Ident(B) [46..47)
         |RightParen [47..48)
         |KwDo [49..51)
         |Ident(h) [52..53)
         |Indentation.Outdent [53..53)
         |Indentation.Outdent [53..53)
         |EOF [54..54)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = while (while (a) g(new B)) h }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(Term.While(tname("a"), tapply(tname("g"), Term.New(init(pname("B"))))), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and an argument with an `if` without parentheses after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) g(if b then c else d)
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [34..35)
         |LeftParen [35..36)
         |KwIf [36..38)
         |Ident(b) [39..40)
         |KwThen [41..45)
         |Ident(c) [46..47)
         |KwElse [48..52)
         |Ident(d) [53..54)
         |RightParen [54..55)
         |KwThen [60..64)
         |Constant.Int(1) [65..66)
         |KwElse [71..75)
         |Constant.Int(2) [76..77)
         |Indentation.Outdent [77..77)
         |Indentation.Outdent [77..77)
         |EOF [78..78)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) g(if (b) c else d)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tif(tname("a"), tapply(tname("g"), tif(tname("b"), tname("c"), tname("d")))),
          int(1),
          int(2),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses, a multi-line comment and an argument with `new` after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) /* c */ g(new B)
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [42..43)
         |LeftParen [43..44)
         |KwNew [44..47)
         |Ident(B) [48..49)
         |RightParen [49..50)
         |KwThen [55..59)
         |Constant.Int(1) [60..61)
         |KwElse [66..70)
         |Constant.Int(2) [71..72)
         |Indentation.Outdent [72..72)
         |Indentation.Outdent [72..72)
         |EOF [73..73)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) /* c */ g(new B)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tif(
            tname("a"),
            tapply(tname("g"), Term.New(init(pname("B")))).toBuilder.begComment(Seq("/* c */"))
              .result(),
          ),
          int(1),
          int(2),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and `new` in parentheses with a selection after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) (new B).ok
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |LeftParen [34..35)
         |KwNew [35..38)
         |Ident(B) [39..40)
         |RightParen [40..41)
         |Dot [41..42)
         |Ident(ok) [42..44)
         |KwThen [49..53)
         |Constant.Int(1) [54..55)
         |KwElse [60..64)
         |Constant.Int(2) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) (new B).ok) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tselect(Term.New(init(pname("B"))), "ok")), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and a `match` with braces after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) x match { case _ => true }
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(x) [34..35)
         |KwMatch [36..41)
         |LeftBrace [42..43)
         |KwCase [44..48)
         |Underscore [49..50)
         |RightArrow [51..53)
         |KwTrue [54..58)
         |RightBrace [59..60)
         |KwThen [65..69)
         |Constant.Int(1) [70..71)
         |KwElse [76..80)
         |Constant.Int(2) [81..82)
         |Indentation.Outdent [82..82)
         |Indentation.Outdent [82..82)
         |EOF [83..83)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (if (a) x match {
         |    case _ => true
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tmatch(tname("x"), Case(patwildcard, None, lit(true)))), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and an argument with `new` after them on the line of the outer `if`, outer `then` indented on the next line, outer `else` on the line after it") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) g(new B)
         |      then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [34..35)
         |LeftParen [35..36)
         |KwNew [36..39)
         |Ident(B) [40..41)
         |RightParen [41..42)
         |KwThen [49..53)
         |Constant.Int(1) [54..55)
         |KwElse [60..64)
         |Constant.Int(2) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) g(new B)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tapply(tname("g"), Term.New(init(pname("B"))))), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and an argument with `throw` after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) g(throw e)
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [34..35)
         |LeftParen [35..36)
         |KwThrow [36..41)
         |Ident(e) [42..43)
         |RightParen [43..44)
         |KwThen [49..53)
         |Constant.Int(1) [54..55)
         |KwElse [60..64)
         |Constant.Int(2) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) g(throw e)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tapply(tname("g"), Term.Throw(tname("e")))), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: an `if` inside an `if` inside an indented condition, `&&` after the parentheses, leading `||` on the next line with `then` and `else`, middle and outer `then` and `else` on the next lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if
         |        if (a) && b
         |          || c then d else e
         |      then f else g
         |    then h else k
         |    m
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |Indentation.Indent [35..35)
         |KwIf [44..46)
         |LeftParen [47..48)
         |Ident(a) [48..49)
         |RightParen [49..50)
         |Ident(&&) [51..53)
         |Ident(b) [54..55)
         |InfixLF [55..56)
         |Ident(||) [66..68)
         |Ident(c) [69..70)
         |KwThen [71..75)
         |Ident(d) [76..77)
         |KwElse [78..82)
         |Ident(e) [83..84)
         |Indentation.Outdent [84..84)
         |KwThen [91..95)
         |Ident(f) [96..97)
         |KwElse [98..102)
         |Ident(g) [103..104)
         |Indentation.Outdent [104..104)
         |KwThen [109..113)
         |Ident(h) [114..115)
         |KwElse [116..120)
         |Ident(k) [121..122)
         |LF [122..123)
         |Ident(m) [127..128)
         |Indentation.Outdent [128..128)
         |Indentation.Outdent [128..128)
         |EOF [129..129)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (if (a && b || c) d else e) f else g) h else k
         |    m
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tif(
                tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
                tname("d"),
                tname("e"),
              ),
              tname("f"),
              tname("g"),
            ),
            tname("h"),
            tname("k"),
          ),
          tname("m"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a `while` inside a `while` inside an indented `while` condition, `&&` after the parentheses, leading `||` on the next line with `do`, middle and outer `do` on the next lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while
         |        while (a) && b
         |          || c do d
         |        e
         |      do f
         |      g
         |    do k
         |    m
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |Indentation.Indent [41..41)
         |KwWhile [50..55)
         |LeftParen [56..57)
         |Ident(a) [57..58)
         |RightParen [58..59)
         |Ident(&&) [60..62)
         |Ident(b) [63..64)
         |InfixLF [64..65)
         |Ident(||) [75..77)
         |Ident(c) [78..79)
         |KwDo [80..82)
         |Ident(d) [83..84)
         |LF [84..85)
         |Ident(e) [93..94)
         |Indentation.Outdent [94..94)
         |KwDo [101..103)
         |Ident(f) [104..105)
         |LF [105..106)
         |Ident(g) [112..113)
         |Indentation.Outdent [113..113)
         |KwDo [118..120)
         |Ident(k) [121..122)
         |LF [122..123)
         |Ident(m) [127..128)
         |Indentation.Outdent [128..128)
         |Indentation.Outdent [128..128)
         |EOF [129..129)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while ({
         |        while (a && b || c) d
         |        e
         |      }) f
         |      g
         |    }) k
         |    m
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              Term.While(
                blk(
                  Term
                    .While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
                  tname("e"),
                ),
                tname("f"),
              ),
              tname("g"),
            ),
            tname("k"),
          ),
          tname("m"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and an identifier after the parentheses inside an indented `while` condition, an argument in parentheses on the next line with `then` and `else`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if (a) && f
         |        (c) then d else e
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |LeftParen [39..40)
         |Ident(a) [40..41)
         |RightParen [41..42)
         |Ident(&&) [43..45)
         |Ident(f) [46..47)
         |LeftParen [56..57)
         |Ident(c) [57..58)
         |RightParen [58..59)
         |KwThen [60..64)
         |Ident(d) [65..66)
         |KwElse [67..71)
         |Ident(e) [72..73)
         |Indentation.Outdent [73..73)
         |KwDo [78..80)
         |Ident(g) [81..82)
         |LF [82..83)
         |Ident(k) [87..88)
         |Indentation.Outdent [88..88)
         |Indentation.Outdent [88..88)
         |EOF [89..89)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if (a && f(c)) d else e) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tif(tinfix(tname("a"), "&&", tapply(tname("f"), tname("c"))), tname("d"), tname("e")),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses in a value of indented enumerators, leading `||` on the next line with `do`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      y = while (a) && b
         |        || c do d
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |Ident(y) [48..49)
         |Equals [50..51)
         |KwWhile [52..57)
         |LeftParen [58..59)
         |Ident(a) [59..60)
         |RightParen [60..61)
         |Ident(&&) [62..64)
         |Ident(b) [65..66)
         |InfixLF [66..67)
         |Ident(||) [75..77)
         |Ident(c) [78..79)
         |KwDo [80..82)
         |Ident(d) [83..84)
         |Indentation.Outdent [84..84)
         |LF [84..85)
         |KwDo [89..91)
         |Ident(g) [92..93)
         |LF [93..94)
         |Ident(k) [98..99)
         |Indentation.Outdent [99..99)
         |Indentation.Outdent [99..99)
         |EOF [100..100)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; y = while (a && b || c) d) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.For(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(
                patvar("y"),
                Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
              ),
            ),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses inside an indented condition, leading `||` on the next line with `then`, `else`, `;` and an identifier, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |        || c then d else e; g
         |    then h else k
         |    m
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [44..45)
         |Ident(||) [53..55)
         |Ident(c) [56..57)
         |KwThen [58..62)
         |Ident(d) [63..64)
         |KwElse [65..69)
         |Ident(e) [70..71)
         |Semicolon [71..72)
         |Ident(g) [73..74)
         |Indentation.Outdent [74..74)
         |KwThen [79..83)
         |Ident(h) [84..85)
         |KwElse [86..90)
         |Ident(k) [91..92)
         |LF [92..93)
         |Ident(m) [97..98)
         |Indentation.Outdent [98..98)
         |Indentation.Outdent [98..98)
         |EOF [99..99)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a && b || c) d else e
         |      g
         |    }) h else k
         |    m
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(
              tif(
                tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
                tname("d"),
                tname("e"),
              ),
              tname("g"),
            ),
            tname("h"),
            tname("k"),
          ),
          tname("m"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses inside an indented `while` condition, leading `||` on the next line with `do`, `;` and an identifier, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) && b
         |        || c do d; e
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(&&) [46..48)
         |Ident(b) [49..50)
         |InfixLF [50..51)
         |Ident(||) [59..61)
         |Ident(c) [62..63)
         |KwDo [64..66)
         |Ident(d) [67..68)
         |Semicolon [68..69)
         |Ident(e) [70..71)
         |Indentation.Outdent [71..71)
         |KwDo [76..78)
         |Ident(k) [79..80)
         |LF [80..81)
         |Ident(g) [85..86)
         |Indentation.Outdent [86..86)
         |Indentation.Outdent [86..86)
         |EOF [87..87)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a && b || c) d
         |      e
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
              tname("e"),
            ),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses inside an indented condition, a blank line, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |
         |        || c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |Ident(||) [54..56)
         |Ident(c) [57..58)
         |KwThen [59..63)
         |Ident(d) [64..65)
         |KwElse [66..70)
         |Ident(e) [71..72)
         |Indentation.Outdent [72..72)
         |KwThen [77..81)
         |Ident(g) [82..83)
         |KwElse [84..88)
         |Ident(h) [89..90)
         |LF [90..91)
         |Ident(k) [95..96)
         |Indentation.Outdent [96..96)
         |Indentation.Outdent [96..96)
         |EOF [97..97)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses inside an indented condition, leading `||` on the next line with `then` and `else`, a blank line, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |        || c then d else e
         |
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [44..45)
         |Ident(||) [53..55)
         |Ident(c) [56..57)
         |KwThen [58..62)
         |Ident(d) [63..64)
         |KwElse [65..69)
         |Ident(e) [70..71)
         |Indentation.Outdent [72..72)
         |KwThen [77..81)
         |Ident(g) [82..83)
         |KwElse [84..88)
         |Ident(h) [89..90)
         |LF [90..91)
         |Ident(k) [95..96)
         |Indentation.Outdent [96..96)
         |Indentation.Outdent [96..96)
         |EOF [97..97)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses inside an indented condition, a comment line, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |        // x
         |        || c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [57..58)
         |Ident(||) [66..68)
         |Ident(c) [69..70)
         |KwThen [71..75)
         |Ident(d) [76..77)
         |KwElse [78..82)
         |Ident(e) [83..84)
         |Indentation.Outdent [84..84)
         |KwThen [89..93)
         |Ident(g) [94..95)
         |KwElse [96..100)
         |Ident(h) [101..102)
         |LF [102..103)
         |Ident(k) [107..108)
         |Indentation.Outdent [108..108)
         |Indentation.Outdent [108..108)
         |EOF [109..109)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b
         |      // x
         |      || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(
                tinfix(tname("a"), "&&", tname("b")),
                Term.Name.newBuilder("||").begComment(detachedComments("// x")).result(),
                tname("c"),
              ),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses inside an indented condition, leading `||` on the next line with `then`, `else` and a comment, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |        || c then d else e // x
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [44..45)
         |Ident(||) [53..55)
         |Ident(c) [56..57)
         |KwThen [58..62)
         |Ident(d) [63..64)
         |KwElse [65..69)
         |Ident(e) [70..71)
         |Indentation.Outdent [76..76)
         |KwThen [81..85)
         |Ident(g) [86..87)
         |KwElse [88..92)
         |Ident(h) [93..94)
         |LF [94..95)
         |Ident(k) [99..100)
         |Indentation.Outdent [100..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e // x
         |    ) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ).toBuilder.endComment(Seq("// x")).result(),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses inside an indented `while` condition, leading `||` on the next line with `do`, another statement, a blank line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) && b
         |        || c do d
         |      e
         |
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(&&) [46..48)
         |Ident(b) [49..50)
         |InfixLF [50..51)
         |Ident(||) [59..61)
         |Ident(c) [62..63)
         |KwDo [64..66)
         |Ident(d) [67..68)
         |LF [68..69)
         |Ident(e) [75..76)
         |Indentation.Outdent [77..77)
         |KwDo [82..84)
         |Ident(k) [85..86)
         |LF [86..87)
         |Ident(g) [91..92)
         |Indentation.Outdent [92..92)
         |Indentation.Outdent [92..92)
         |EOF [93..93)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a && b || c) d
         |      e
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
              tname("e"),
            ),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses on the line of the outer `while`, leading `||` on the next line with `do`, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) && b
         |      || c do d
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [44..45)
         |Ident(||) [51..53)
         |Ident(c) [54..55)
         |KwDo [56..58)
         |Ident(d) [59..60)
         |KwDo [65..67)
         |Ident(k) [68..69)
         |LF [69..70)
         |Ident(g) [74..75)
         |Indentation.Outdent [75..75)
         |Indentation.Outdent [75..75)
         |EOF [76..76)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (while (a && b || c) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: an identifier and an `if` with `&&` after the parentheses inside an indented condition, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      c
         |      if (a) && b
         |        || c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |Ident(c) [33..34)
         |LF [34..35)
         |KwIf [41..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(&&) [48..50)
         |Ident(b) [51..52)
         |InfixLF [52..53)
         |Ident(||) [61..63)
         |Ident(c) [64..65)
         |KwThen [66..70)
         |Ident(d) [71..72)
         |KwElse [73..77)
         |Ident(e) [78..79)
         |Indentation.Outdent [79..79)
         |KwThen [84..88)
         |Ident(g) [89..90)
         |KwElse [91..95)
         |Ident(h) [96..97)
         |LF [97..98)
         |Ident(k) [102..103)
         |Indentation.Outdent [103..103)
         |Indentation.Outdent [103..103)
         |EOF [104..104)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      c
         |      if (a && b || c) d else e
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(
              tname("c"),
              tif(
                tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
                tname("d"),
                tname("e"),
              ),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: an identifier and a `while` with `&&` after the parentheses inside an indented `while` condition, leading `||` on the next line with `do`, an identifier, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      c
         |      while (a) && b
         |        || c do d
         |      e
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |Ident(c) [36..37)
         |LF [37..38)
         |KwWhile [44..49)
         |LeftParen [50..51)
         |Ident(a) [51..52)
         |RightParen [52..53)
         |Ident(&&) [54..56)
         |Ident(b) [57..58)
         |InfixLF [58..59)
         |Ident(||) [67..69)
         |Ident(c) [70..71)
         |KwDo [72..74)
         |Ident(d) [75..76)
         |LF [76..77)
         |Ident(e) [83..84)
         |Indentation.Outdent [84..84)
         |KwDo [89..91)
         |Ident(k) [92..93)
         |LF [93..94)
         |Ident(g) [98..99)
         |Indentation.Outdent [99..99)
         |Indentation.Outdent [99..99)
         |EOF [100..100)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      c
         |      while (a && b || c) d
         |      e
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              tname("c"),
              Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
              tname("e"),
            ),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses and an argument with `new` after them on the line of the outer `while`, outer `do` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) g(new B)
         |    do h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(g) [40..41)
         |LeftParen [41..42)
         |KwNew [42..45)
         |Ident(B) [46..47)
         |RightParen [47..48)
         |KwDo [53..55)
         |Ident(h) [56..57)
         |Indentation.Outdent [57..57)
         |Indentation.Outdent [57..57)
         |EOF [58..58)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = while (while (a) g(new B)) h }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(Term.While(tname("a"), tapply(tname("g"), Term.New(init(pname("B"))))), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses and an argument with `try` and `catch` in braces after them on the line of the outer `while`, outer `do` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) g(try x catch { case _ => y })
         |    do h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(g) [40..41)
         |LeftParen [41..42)
         |KwTry [42..45)
         |Ident(x) [46..47)
         |KwCatch [48..53)
         |LeftBrace [54..55)
         |KwCase [56..60)
         |Underscore [61..62)
         |RightArrow [63..65)
         |Ident(y) [66..67)
         |RightBrace [68..69)
         |RightParen [69..70)
         |KwDo [75..77)
         |Ident(h) [78..79)
         |Indentation.Outdent [79..79)
         |Indentation.Outdent [79..79)
         |EOF [80..80)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = while (while (a) g(try x catch {
         |    case _ => y
         |  })) h
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(
          Term.While(
            tname("a"),
            tapply(tname("g"), Term.Try(tname("x"), List(Case(patwildcard, None, tname("y"))), None)),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses and an assignment with `new` after them on the line of the outer `while`, outer `do` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) x = new B
         |    do h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(x) [40..41)
         |Equals [42..43)
         |KwNew [44..47)
         |Ident(B) [48..49)
         |KwDo [54..56)
         |Ident(h) [57..58)
         |Indentation.Outdent [58..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = while (while (a) x = new B) h }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(
          Term.While(tname("a"), Term.Assign(tname("x"), Term.New(init(pname("B"))))),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses and an argument with `for` and `yield` without parentheses after them on the line of the outer `while`, outer `do` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) g(for x <- xs yield x)
         |    do h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(g) [40..41)
         |LeftParen [41..42)
         |KwFor [42..45)
         |Ident(x) [46..47)
         |LeftArrow [48..50)
         |Ident(xs) [51..53)
         |KwYield [54..59)
         |Ident(x) [60..61)
         |RightParen [61..62)
         |KwDo [67..69)
         |Ident(h) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = while (while (a) g(for (x <- xs) yield x)) h }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(
          Term.While(
            tname("a"),
            tapply(
              tname("g"),
              Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("x")),
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` and a colon argument after the parentheses, the argument on the next line, shallower leading `&&` on the next line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = while (x)(y) && foo:
         |        z
         |      && w
         |    do c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while (x(y) && foo {
         |    z && w
         |  }) c
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          Term.While(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` without parentheses with `&&` and a colon argument, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if x && foo:
         |        z
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (x && foo {
         |    z && w
         |  }) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` without parentheses with `&&` and a colon argument, the argument on the next line, shallower leading `&&` on the next line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = while x && foo:
         |        z
         |      && w
         |    do c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while (x && foo {
         |    z && w
         |  }) c
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          Term.While(
            tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            tname("c"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument outside a condition, the argument on the next line, shallower leading `&&` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = a && foo:
         |        z
         |      && w
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = a && foo {
         |    z && w
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |Indentation.Outdent [48..48)
         |InfixLF [48..49)
         |Ident(&&) [55..57)
         |Ident(w) [58..59)
         |KwThen [64..68)
         |Ident(c) [69..70)
         |KwElse [71..75)
         |Ident(d) [76..77)
         |LF [77..78)
         |KwDef [80..83)
         |Ident(g) [84..85)
         |Equals [86..87)
         |Constant.Int(1) [88..89)
         |Indentation.Outdent [89..89)
         |EOF [90..90)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with an `if` whose `then` body holds a colon argument, the argument on the next line, a blank line, leading `&&` on the next line, `else` and the outer `then` on later lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if a then foo:
         |          z
         |
         |        && w
         |      else d
         |    then c else e
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |KwThen [32..36)
         |Ident(foo) [37..40)
         |Colon [40..41)
         |Indentation.Indent [41..41)
         |Ident(z) [52..53)
         |Indentation.Outdent [54..54)
         |InfixLF [54..55)
         |Ident(&&) [63..65)
         |Ident(w) [66..67)
         |KwElse [74..78)
         |Ident(d) [79..80)
         |KwThen [85..89)
         |Ident(c) [90..91)
         |KwElse [92..96)
         |Ident(e) [97..98)
         |LF [98..99)
         |Ident(g) [103..104)
         |Indentation.Outdent [104..104)
         |Indentation.Outdent [104..104)
         |EOF [105..105)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) foo {
         |      z
         |    } && w else d) c else e
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tname("a"),
              tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
              tname("d"),
            ),
            tname("c"),
            tname("e"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with an `if` whose `then` body holds a colon argument, the argument on the next line, two blank lines, leading `&&` on the next line, `else` and the outer `then` on later lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if a then foo:
         |          z
         |
         |
         |        && w
         |      else d
         |    then c else e
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |KwThen [32..36)
         |Ident(foo) [37..40)
         |Colon [40..41)
         |Indentation.Indent [41..41)
         |Ident(z) [52..53)
         |Indentation.Outdent [55..55)
         |Ident(&&) [64..66)
         |Ident(w) [67..68)
         |KwElse [75..79)
         |Ident(d) [80..81)
         |KwThen [86..90)
         |Ident(c) [91..92)
         |KwElse [93..97)
         |Ident(e) [98..99)
         |LF [99..100)
         |Ident(g) [104..105)
         |Indentation.Outdent [105..105)
         |Indentation.Outdent [105..105)
         |EOF [106..106)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) foo {
         |      z
         |    } && w else d) c else e
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tname("a"),
              tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
              tname("d"),
            ),
            tname("c"),
            tname("e"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with a colon argument in its `then` body, the argument on the next line with a comment, a blank line, leading `&&` on the next line, `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if a then foo:
         |          z // c
         |
         |        && w
         |      else d)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwIf [26..28)
         |Ident(a) [29..30)
         |KwThen [31..35)
         |Ident(foo) [36..39)
         |Colon [39..40)
         |Indentation.Indent [40..40)
         |Ident(z) [51..52)
         |Indentation.Outdent [58..58)
         |Ident(&&) [67..69)
         |Ident(w) [70..71)
         |KwElse [78..82)
         |Ident(d) [83..84)
         |RightParen [84..85)
         |LF [85..86)
         |Ident(h) [90..91)
         |Indentation.Outdent [91..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) foo {
         |      z // c
         |
         |    } && w else d)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tname("g"),
            tif(
              tname("a"),
              tinfix(tapply(tname("foo"), blk(tnameComments("z")()("// c"))), "&&", tname("w")),
              tname("d"),
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`for` within parentheses with `try` and a colon argument after `yield`, the argument on the next line, a blank line, leading `&&` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(for (x <- xs) yield try foo:
         |          z
         |
         |        && w)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwFor [26..29)
         |LeftParen [30..31)
         |Ident(x) [31..32)
         |LeftArrow [33..35)
         |Ident(xs) [36..38)
         |RightParen [38..39)
         |KwYield [40..45)
         |KwTry [46..49)
         |Ident(foo) [50..53)
         |Colon [53..54)
         |Indentation.Indent [54..54)
         |Ident(z) [65..66)
         |Indentation.Outdent [67..67)
         |InfixLF [67..68)
         |Ident(&&) [76..78)
         |Ident(w) [79..80)
         |RightParen [80..81)
         |LF [81..82)
         |Ident(h) [86..87)
         |Indentation.Outdent [87..87)
         |Indentation.Outdent [87..87)
         |EOF [88..88)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(for (x <- xs) yield try foo {
         |      z
         |    } && w)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tname("g"),
            Term.ForYield(
              Term.EnumeratorsBlock(List(Enumerator.Generator(patvar("x"), tname("xs")))),
              Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with a colon argument after `do`, the argument on the next line, a blank line, leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if for (x <- xs) do foo:
         |          z
         |
         |        && w
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwFor [27..30)
         |LeftParen [31..32)
         |Ident(x) [32..33)
         |LeftArrow [34..36)
         |Ident(xs) [37..39)
         |RightParen [39..40)
         |KwDo [41..43)
         |Ident(foo) [44..47)
         |Colon [47..48)
         |Indentation.Indent [48..48)
         |Ident(z) [59..60)
         |Indentation.Outdent [61..61)
         |InfixLF [61..62)
         |Ident(&&) [70..72)
         |Ident(w) [73..74)
         |KwThen [79..83)
         |Ident(c) [84..85)
         |KwElse [86..90)
         |Ident(d) [91..92)
         |LF [92..93)
         |Ident(h) [97..98)
         |Indentation.Outdent [98..98)
         |Indentation.Outdent [98..98)
         |EOF [99..99)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (for (x <- xs) foo {
         |      z
         |    } && w) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.For(
              Term.EnumeratorsBlock(List(Enumerator.Generator(patvar("x"), tname("xs")))),
              tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`for` with braces within parentheses, a colon argument after `yield`, the argument on the next line, a blank line, leading `&&` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(for { x <- xs } yield foo:
         |          z
         |
         |        && w)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwFor [26..29)
         |LeftBrace [30..31)
         |Ident(x) [32..33)
         |LeftArrow [34..36)
         |Ident(xs) [37..39)
         |RightBrace [40..41)
         |KwYield [42..47)
         |Ident(foo) [48..51)
         |Colon [51..52)
         |Indentation.Indent [52..52)
         |Ident(z) [63..64)
         |Indentation.Outdent [65..65)
         |Ident(&&) [74..76)
         |Ident(w) [77..78)
         |RightParen [78..79)
         |LF [79..80)
         |Ident(h) [84..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(for (x <- xs) yield foo {
         |      z
         |    } && w)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tname("g"),
            Term.ForYield(
              Term.EnumeratorsBlock(List(Enumerator.Generator(patvar("x"), tname("xs")))),
              tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("case guard: a colon argument, the argument on the next line, a blank line, leading `&&` and `=>` on the next line, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case y if foo:
         |        z
         |
         |      && w => 1
         |    case _ => 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Ident(y) [37..38)
         |KwIf [39..41)
         |Ident(foo) [42..45)
         |Colon [45..46)
         |Indentation.Indent [46..46)
         |Ident(z) [55..56)
         |Indentation.Outdent [57..57)
         |Ident(&&) [64..66)
         |Ident(w) [67..68)
         |RightArrow [69..71)
         |Constant.Int(1) [72..73)
         |LF [73..74)
         |KwCase [78..82)
         |Underscore [83..84)
         |RightArrow [85..87)
         |Constant.Int(2) [88..89)
         |Indentation.Outdent [89..89)
         |Indentation.Outdent [89..89)
         |EOF [90..90)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case y if foo {
         |      z
         |    } && w => 1
         |    case _ => 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w"))),
            int(1),
          ),
          Case(patwildcard, None, int(2)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("case guard: a colon argument, the argument on the next line, two blank lines, leading `&&` and `=>` on the next line, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case y if foo:
         |        z
         |
         |
         |      && w => 1
         |    case _ => 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Ident(y) [37..38)
         |KwIf [39..41)
         |Ident(foo) [42..45)
         |Colon [45..46)
         |Indentation.Indent [46..46)
         |Ident(z) [55..56)
         |Indentation.Outdent [58..58)
         |Ident(&&) [65..67)
         |Ident(w) [68..69)
         |RightArrow [70..72)
         |Constant.Int(1) [73..74)
         |LF [74..75)
         |KwCase [79..83)
         |Underscore [84..85)
         |RightArrow [86..88)
         |Constant.Int(2) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case y if foo {
         |      z
         |    } && w => 1
         |    case _ => 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w"))),
            int(1),
          ),
          Case(patwildcard, None, int(2)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("case guard: a colon argument, the argument on the next line with a comment, a blank line, leading `&&` and `=>` on the next line, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case y if foo:
         |        z // c
         |
         |      && w => 1
         |    case _ => 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Ident(y) [37..38)
         |KwIf [39..41)
         |Ident(foo) [42..45)
         |Colon [45..46)
         |Indentation.Indent [46..46)
         |Ident(z) [55..56)
         |Indentation.Outdent [62..62)
         |Ident(&&) [69..71)
         |Ident(w) [72..73)
         |RightArrow [74..76)
         |Constant.Int(1) [77..78)
         |LF [78..79)
         |KwCase [83..87)
         |Underscore [88..89)
         |RightArrow [90..92)
         |Constant.Int(2) [93..94)
         |Indentation.Outdent [94..94)
         |Indentation.Outdent [94..94)
         |EOF [95..95)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case y if foo {
         |      z // c
         |
         |    } && w => 1
         |    case _ => 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tinfix(tapply(tname("foo"), blk(tnameComments("z")()("// c"))), "&&", tname("w"))),
            int(1),
          ),
          Case(patwildcard, None, int(2)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a colon lambda, its body on the next line, a blank line, leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists: x =>
         |        x > 0
         |
         |      && w
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(xs) [27..29)
         |Dot [29..30)
         |Ident(exists) [30..36)
         |Colon [36..37)
         |Ident(x) [38..39)
         |RightArrow [40..42)
         |Indentation.Indent [42..42)
         |Ident(x) [51..52)
         |Ident(>) [53..54)
         |Constant.Int(0) [55..56)
         |Indentation.Outdent [57..57)
         |InfixLF [57..58)
         |Ident(&&) [64..66)
         |Ident(w) [67..68)
         |KwThen [73..77)
         |Ident(c) [78..79)
         |KwElse [80..84)
         |Ident(d) [85..86)
         |LF [86..87)
         |Ident(h) [91..92)
         |Indentation.Outdent [92..92)
         |Indentation.Outdent [92..92)
         |EOF [93..93)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (xs.exists {
         |      x => x > 0
         |    } && w) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tapply(
                tselect(tname("xs"), "exists"),
                blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", int(0)))),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("a colon argument within parentheses after type arguments, the argument on the next line, a blank line, leading `&&` and `)` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g[Int](foo:
         |        z
         |
         |      && w)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftBracket [25..26)
         |Ident(Int) [26..29)
         |RightBracket [29..30)
         |LeftParen [30..31)
         |Ident(foo) [31..34)
         |Colon [34..35)
         |Indentation.Indent [35..35)
         |Ident(z) [44..45)
         |Indentation.Outdent [46..46)
         |Ident(&&) [53..55)
         |Ident(w) [56..57)
         |RightParen [57..58)
         |LF [58..59)
         |Ident(h) [63..64)
         |Indentation.Outdent [64..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g[Int](foo {
         |      z
         |    } && w)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tapplytype(tname("g"), pname("Int")),
            tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with a colon argument in its `else` body, the argument on the next line, a blank line, leading `&&` and `)` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if a then b else foo:
         |          z
         |
         |        && w)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwIf [26..28)
         |Ident(a) [29..30)
         |KwThen [31..35)
         |Ident(b) [36..37)
         |KwElse [38..42)
         |Ident(foo) [43..46)
         |Colon [46..47)
         |Indentation.Indent [47..47)
         |Ident(z) [58..59)
         |Indentation.Outdent [60..60)
         |Ident(&&) [69..71)
         |Ident(w) [72..73)
         |RightParen [73..74)
         |LF [74..75)
         |Ident(h) [79..80)
         |Indentation.Outdent [80..80)
         |Indentation.Outdent [80..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) b else foo {
         |      z
         |    } && w)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tname("g"),
            tif(
              tname("a"),
              tname("b"),
              tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and nested colon arguments, the arguments on the next lines, a comment line, a blank line, leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |        bar:
         |          z
         |        // c
         |
         |      && w
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(bar) [45..48)
         |Colon [48..49)
         |Indentation.Indent [49..49)
         |Ident(z) [60..61)
         |Indentation.Outdent [61..61)
         |Indentation.Outdent [75..75)
         |Ident(&&) [82..84)
         |Ident(w) [85..86)
         |KwThen [91..95)
         |Ident(c) [96..97)
         |KwElse [98..102)
         |Ident(d) [103..104)
         |LF [104..105)
         |Ident(h) [109..110)
         |Indentation.Outdent [110..110)
         |Indentation.Outdent [110..110)
         |EOF [111..111)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a && foo {
         |      bar {
         |        z
         |      }
         |      // c
         |
         |    } && w) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tinfix(
                tname("a"),
                "&&",
                tapply(
                  tname("foo"),
                  blk(tapply(tname("bar"), blk(tname("z"))).toBuilder.endComment(Seq("// c")).result()),
                ),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with the condition in parentheses, a deeper `then` body in parentheses on the next line, `else` on that line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if (a)
         |        (x) else y)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwIf [26..28)
         |LeftParen [29..30)
         |Ident(a) [30..31)
         |RightParen [31..32)
         |Indentation.Indent [32..32)
         |LeftParen [41..42)
         |Ident(x) [42..43)
         |RightParen [43..44)
         |KwElse [45..49)
         |Ident(y) [50..51)
         |Indentation.Outdent [50..50)
         |RightParen [51..52)
         |LF [52..53)
         |Ident(h) [57..58)
         |Indentation.Outdent [58..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) x else y)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tapply(tname("g"), tif(tname("a"), tname("x"), tname("y"))), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with the condition in parentheses, a deeper `then` body in parentheses on the next line, `else` on that line, the outer `then` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if (a)
         |         (x) else y
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |LeftParen [43..44)
         |Ident(x) [44..45)
         |RightParen [45..46)
         |KwElse [47..51)
         |Ident(y) [52..53)
         |Indentation.Outdent [53..53)
         |KwThen [58..62)
         |Ident(c) [63..64)
         |KwElse [65..69)
         |Ident(d) [70..71)
         |LF [71..72)
         |Ident(h) [76..77)
         |Indentation.Outdent [77..77)
         |Indentation.Outdent [77..77)
         |EOF [78..78)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) x else y) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tif(tname("a"), tname("x"), tname("y")), tname("c"), tname("d")), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with the condition in parentheses, a deeper `then` body on the next line, `else` on that line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if (a)
         |        x else y)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwIf [26..28)
         |LeftParen [29..30)
         |Ident(a) [30..31)
         |RightParen [31..32)
         |Indentation.Indent [32..32)
         |Ident(x) [41..42)
         |KwElse [43..47)
         |Ident(y) [48..49)
         |Indentation.Outdent [48..48)
         |RightParen [49..50)
         |LF [50..51)
         |Ident(h) [55..56)
         |Indentation.Outdent [56..56)
         |Indentation.Outdent [56..56)
         |EOF [57..57)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) x else y)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tapply(tname("g"), tif(tname("a"), tname("x"), tname("y"))), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with the condition in parentheses, a deeper `then` body on the next line, `else` on that line, the outer `then` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if (a)
         |         x else y
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |Ident(x) [43..44)
         |KwElse [45..49)
         |Ident(y) [50..51)
         |Indentation.Outdent [51..51)
         |KwThen [56..60)
         |Ident(c) [61..62)
         |KwElse [63..67)
         |Ident(d) [68..69)
         |LF [69..70)
         |Ident(h) [74..75)
         |Indentation.Outdent [75..75)
         |Indentation.Outdent [75..75)
         |EOF [76..76)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) x else y) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tif(tname("a"), tname("x"), tname("y")), tname("c"), tname("d")), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with the condition in parentheses, the `then` body on the next line at the column of the `if`, `else` on that line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if (a)
         |      x else y)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwIf [26..28)
         |LeftParen [29..30)
         |Ident(a) [30..31)
         |RightParen [31..32)
         |Indentation.Indent [32..32)
         |Ident(x) [39..40)
         |KwElse [41..45)
         |Ident(y) [46..47)
         |Indentation.Outdent [46..46)
         |RightParen [47..48)
         |LF [48..49)
         |Ident(h) [53..54)
         |Indentation.Outdent [54..54)
         |Indentation.Outdent [54..54)
         |EOF [55..55)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) x else y)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tapply(tname("g"), tif(tname("a"), tname("x"), tname("y"))), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with an inline `then` body, a deeper leading `+` on the next line, `else` on that line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if a then b
         |        + c else y)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwIf [26..28)
         |Ident(a) [29..30)
         |KwThen [31..35)
         |Ident(b) [36..37)
         |InfixLF [37..38)
         |Ident(+) [46..47)
         |Ident(c) [48..49)
         |KwElse [50..54)
         |Ident(y) [55..56)
         |RightParen [56..57)
         |LF [57..58)
         |Ident(h) [62..63)
         |Indentation.Outdent [63..63)
         |Indentation.Outdent [63..63)
         |EOF [64..64)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) b + c else y)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(tname("g"), tif(tname("a"), tinfix(tname("b"), "+", tname("c")), tname("y"))),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` with a colon argument in its `then` body, the argument on the next line, a blank line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a then foo:
         |        z
         |
         |    w
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |KwThen [29..33)
         |Ident(foo) [34..37)
         |Colon [37..38)
         |Indentation.Indent [38..38)
         |Ident(z) [47..48)
         |Indentation.Outdent [49..49)
         |LFLF [49..50)
         |Ident(w) [54..55)
         |Indentation.Outdent [55..55)
         |Indentation.Outdent [55..55)
         |EOF [56..56)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a) foo {
         |      z
         |    }
         |    w
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tname("a"), tapply(tname("foo"), blk(tname("z")))), tname("w")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` with `try` and a colon argument in its `then` body, the argument on the next line, a blank line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a then try foo:
         |        z
         |
         |    w
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |KwThen [29..33)
         |KwTry [34..37)
         |Ident(foo) [38..41)
         |Colon [41..42)
         |Indentation.Indent [42..42)
         |Ident(z) [51..52)
         |Indentation.Outdent [53..53)
         |LFLF [53..54)
         |Ident(w) [58..59)
         |Indentation.Outdent [59..59)
         |Indentation.Outdent [59..59)
         |EOF [60..60)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a) try foo {
         |      z
         |    }
         |    w
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tname("a"), Term.Try(tapply(tname("foo"), blk(tname("z"))), None, None)), tname("w")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("class parents: a colon argument within parentheses, the argument on the next line, `)` and `derives` on the next line") {
    val code =
      """|object A:
         |  class C extends B(foo:
         |      z
         |    ) derives D
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwClass [12..17)
         |Ident(C) [18..19)
         |KwExtends [20..27)
         |Ident(B) [28..29)
         |LeftParen [29..30)
         |Ident(foo) [30..33)
         |Colon [33..34)
         |Indentation.Indent [34..34)
         |Ident(z) [41..42)
         |Indentation.Outdent [42..42)
         |RightParen [47..48)
         |Ident(derives) [49..56)
         |Ident(D) [57..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  class C extends B(foo {
         |    z
         |  }) derives D
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Class(
        Nil,
        pname("C"),
        Nil,
        ctor,
        Template(
          None,
          List(init(pname("B"), Term.ArgClause(List(tapply(tname("foo"), blk(tname("z"))))))),
          Template.Body(None, Nil),
          List(pname("D")),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a colon argument, the argument on the next line, a blank line, `match` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if foo:
         |        z
         |
         |      match { case _ => true }
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(foo) [27..30)
         |Colon [30..31)
         |Indentation.Indent [31..31)
         |Ident(z) [40..41)
         |Indentation.Outdent [42..42)
         |KwMatch [49..54)
         |LeftBrace [55..56)
         |KwCase [57..61)
         |Underscore [62..63)
         |RightArrow [64..66)
         |KwTrue [67..71)
         |RightBrace [72..73)
         |KwThen [78..82)
         |Ident(c) [83..84)
         |KwElse [85..89)
         |Ident(d) [90..91)
         |LF [91..92)
         |Ident(h) [96..97)
         |Indentation.Outdent [97..97)
         |Indentation.Outdent [97..97)
         |EOF [98..98)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (foo {
         |      z
         |    } match {
         |      case _ => true
         |    }) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tmatch(tapply(tname("foo"), blk(tname("z"))), Case(patwildcard, None, bool(true))),
            tname("c"),
            tname("d"),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with a colon argument, the argument on the next line, a blank line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while foo:
         |        z
         |
         |    do b
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Ident(foo) [30..33)
         |Colon [33..34)
         |Indentation.Indent [34..34)
         |Ident(z) [43..44)
         |Indentation.Outdent [45..45)
         |KwDo [50..52)
         |Ident(b) [53..54)
         |LF [54..55)
         |Ident(h) [59..60)
         |Indentation.Outdent [60..60)
         |Indentation.Outdent [60..60)
         |EOF [61..61)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (foo {
         |      z
         |    }) b
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(tapply(tname("foo"), blk(tname("z"))), tname("b")), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a colon argument, the argument on the next line, a blank line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if foo:
         |        z
         |
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(foo) [27..30)
         |Colon [30..31)
         |Indentation.Indent [31..31)
         |Ident(z) [40..41)
         |Indentation.Outdent [42..42)
         |KwThen [47..51)
         |Ident(c) [52..53)
         |KwElse [54..58)
         |Ident(d) [59..60)
         |LF [60..61)
         |Ident(h) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (foo {
         |      z
         |    }) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tapply(tname("foo"), blk(tname("z"))), tname("c"), tname("d")), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("a colon argument, the argument on the next line, a blank line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    foo:
         |      z
         |
         |    w
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(foo) [24..27)
         |Colon [27..28)
         |Indentation.Indent [28..28)
         |Ident(z) [35..36)
         |Indentation.Outdent [37..37)
         |LFLF [37..38)
         |Ident(w) [42..43)
         |Indentation.Outdent [43..43)
         |Indentation.Outdent [43..43)
         |EOF [44..44)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    foo {
         |      z
         |    }
         |    w
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(Nil, tname("f"), Nil, None, blk(tapply(tname("foo"), blk(tname("z"))), tname("w")))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("a colon argument, the argument on the next line with a comment, next statement") {
    val code =
      """|object A:
         |  def f =
         |    foo:
         |      z // c
         |    w
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(foo) [24..27)
         |Colon [27..28)
         |Indentation.Indent [28..28)
         |Ident(z) [35..36)
         |Indentation.Outdent [41..41)
         |Ident(w) [46..47)
         |Indentation.Outdent [47..47)
         |Indentation.Outdent [47..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    foo {
         |      z // c
         |    }
         |    w
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tapply(tname("foo"), blk(tnameComments("z")()("// c"))), tname("w")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`try` with a colon argument, the argument on the next line, a blank line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    try foo:
         |      z
         |
         |    w
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwTry [24..27)
         |Ident(foo) [28..31)
         |Colon [31..32)
         |Indentation.Indent [32..32)
         |Ident(z) [39..40)
         |Indentation.Outdent [41..41)
         |LFLF [41..42)
         |Ident(w) [46..47)
         |Indentation.Outdent [47..47)
         |Indentation.Outdent [47..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    try foo {
         |      z
         |    }
         |    w
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.Try(tapply(tname("foo"), blk(tname("z"))), None, None), tname("w")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a colon argument after `finally`, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if try a finally foo:
         |        z
         |
         |    && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |Ident(a) [31..32)
         |KwFinally [33..40)
         |Ident(foo) [41..44)
         |Colon [44..45)
         |Indentation.Indent [45..45)
         |Ident(z) [54..55)
         |Indentation.Outdent [56..56)
         |InfixLF [56..57)
         |Ident(&&) [61..63)
         |Ident(w) [64..65)
         |KwThen [70..74)
         |Ident(c) [75..76)
         |KwElse [77..81)
         |Ident(d) [82..83)
         |Indentation.Outdent [83..83)
         |Indentation.Outdent [83..83)
         |EOF [84..84)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (try a finally foo {
         |    z
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          Term.Try(
            tname("a"),
            None,
            Some(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w"))),
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and nested colon arguments, the arguments on the next lines, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |          bar:
         |            z
         |
         |    && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(bar) [47..50)
         |Colon [50..51)
         |Indentation.Indent [51..51)
         |Ident(z) [64..65)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |InfixLF [66..67)
         |Ident(&&) [71..73)
         |Ident(w) [74..75)
         |KwThen [80..84)
         |Ident(c) [85..86)
         |KwElse [87..91)
         |Ident(d) [92..93)
         |Indentation.Outdent [93..93)
         |Indentation.Outdent [93..93)
         |EOF [94..94)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (a && foo {
         |    bar {
         |      z
         |    }
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tinfix(
            tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tapply(tname("bar"), blk(tname("z")))))),
            "&&",
            tname("w"),
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and nested colon arguments, the arguments on the next lines, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |          bar:
         |            z
         |
         |      && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(bar) [47..50)
         |Colon [50..51)
         |Indentation.Indent [51..51)
         |Ident(z) [64..65)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |InfixLF [66..67)
         |Ident(&&) [73..75)
         |Ident(w) [76..77)
         |KwThen [82..86)
         |Ident(c) [87..88)
         |KwElse [89..93)
         |Ident(d) [94..95)
         |Indentation.Outdent [95..95)
         |Indentation.Outdent [95..95)
         |EOF [96..96)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (a && foo {
         |    bar {
         |      z
         |    }
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tinfix(
            tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tapply(tname("bar"), blk(tname("z")))))),
            "&&",
            tname("w"),
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and three nested colon arguments, the arguments on the next lines, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |          bar:
         |            baz:
         |              z
         |
         |    && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(bar) [47..50)
         |Colon [50..51)
         |Indentation.Indent [51..51)
         |Ident(baz) [64..67)
         |Colon [67..68)
         |Indentation.Indent [68..68)
         |Ident(z) [83..84)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |InfixLF [85..86)
         |Ident(&&) [90..92)
         |Ident(w) [93..94)
         |KwThen [99..103)
         |Ident(c) [104..105)
         |KwElse [106..110)
         |Ident(d) [111..112)
         |Indentation.Outdent [112..112)
         |LF [112..113)
         |KwDef [115..118)
         |Ident(g) [119..120)
         |Equals [121..122)
         |Constant.Int(1) [123..124)
         |Indentation.Outdent [124..124)
         |EOF [125..125)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (a && foo {
         |    bar {
         |      baz {
         |        z
         |      }
         |    }
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(
                tname("a"),
                "&&",
                tapply(tname("foo"), blk(tapply(tname("bar"), blk(tapply(tname("baz"), blk(tname("z"))))))),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with two `try` and a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if try try foo:
         |        z
         |
         |    && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |KwTry [31..34)
         |Ident(foo) [35..38)
         |Colon [38..39)
         |Indentation.Indent [39..39)
         |Ident(z) [48..49)
         |Indentation.Outdent [50..50)
         |InfixLF [50..51)
         |Ident(&&) [55..57)
         |Ident(w) [58..59)
         |KwThen [64..68)
         |Ident(c) [69..70)
         |KwElse [71..75)
         |Ident(d) [76..77)
         |Indentation.Outdent [77..77)
         |Indentation.Outdent [77..77)
         |EOF [78..78)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (try try foo {
         |    z
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          Term.Try(
            Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            None,
            None,
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("lambda within parentheses with `if`, `&&` and a colon argument, the argument on the next line, a blank line, leading `&&`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f = g(x => if a && foo:
         |        z
         |
         |      && w then 1 else 2)
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(g) [20..21)
         |LeftParen [21..22)
         |Ident(x) [22..23)
         |RightArrow [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |Ident(&&) [32..34)
         |Ident(foo) [35..38)
         |Colon [38..39)
         |Indentation.Indent [39..39)
         |Ident(z) [48..49)
         |Indentation.Outdent [50..50)
         |InfixLF [50..51)
         |Ident(&&) [57..59)
         |Ident(w) [60..61)
         |KwThen [62..66)
         |Constant.Int(1) [67..68)
         |KwElse [69..73)
         |Constant.Int(2) [74..75)
         |RightParen [75..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = g(x => if (a && foo {
         |    z
         |  } && w) 1 else 2)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tname("g"),
          tfunc(tparam("x"))(tif(
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            int(1),
            int(2),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the condition in parentheses, the argument on the next line, a blank line, leading `||` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if (a) && foo:
         |        z
         |
         |    || w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Ident(&&) [31..33)
         |Ident(foo) [34..37)
         |Colon [37..38)
         |Indentation.Indent [38..38)
         |Ident(z) [47..48)
         |Indentation.Outdent [49..49)
         |InfixLF [49..50)
         |Ident(||) [54..56)
         |Ident(w) [57..58)
         |KwThen [63..67)
         |Ident(c) [68..69)
         |KwElse [70..74)
         |Ident(d) [75..76)
         |Indentation.Outdent [76..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (a && foo {
         |    z
         |  } || w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "||", tname("w")),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `while`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while foo:
         |        z
         |
         |    && w
         |    do b
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Ident(foo) [30..33)
         |Colon [33..34)
         |Indentation.Indent [34..34)
         |Ident(z) [43..44)
         |Indentation.Outdent [45..45)
         |InfixLF [45..46)
         |Ident(&&) [50..52)
         |Ident(w) [53..54)
         |KwDo [59..61)
         |Ident(b) [62..63)
         |LF [63..64)
         |Ident(g) [68..69)
         |Indentation.Outdent [69..69)
         |Indentation.Outdent [69..69)
         |EOF [70..70)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (foo {
         |      z
         |    } && w) b
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), tname("b")),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("condition in parentheses with `try` and a colon argument, the argument on the next line, a blank line, leading `&&`, `)` and both bodies at the column of the `if`") {
    val code =
      """|object A:
         |  def f =
         |    if (try foo:
         |        z
         |
         |    && w) c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |KwTry [28..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(z) [45..46)
         |Indentation.Outdent [47..47)
         |InfixLF [47..48)
         |Ident(&&) [52..54)
         |Ident(w) [55..56)
         |RightParen [56..57)
         |Ident(c) [58..59)
         |KwElse [60..64)
         |Ident(d) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (try foo {
         |    z
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` in a case body with `&&` and a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case 1 =>
         |      if a && foo:
         |          z
         |
         |      && w
         |      then c else d
         |    case _ => e
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Constant.Int(1) [37..38)
         |RightArrow [39..41)
         |Indentation.Indent [41..41)
         |KwIf [48..50)
         |Ident(a) [51..52)
         |Ident(&&) [53..55)
         |Ident(foo) [56..59)
         |Colon [59..60)
         |Indentation.Indent [60..60)
         |Ident(z) [71..72)
         |Indentation.Outdent [73..73)
         |InfixLF [73..74)
         |Ident(&&) [80..82)
         |Ident(w) [83..84)
         |KwThen [91..95)
         |Ident(c) [96..97)
         |KwElse [98..102)
         |Ident(d) [103..104)
         |Indentation.Outdent [104..104)
         |LF [104..105)
         |KwCase [109..113)
         |Underscore [114..115)
         |RightArrow [116..118)
         |Ident(e) [119..120)
         |Indentation.Outdent [120..120)
         |Indentation.Outdent [120..120)
         |EOF [121..121)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      if (a && foo {
         |        z
         |      } && w) c else d
         |    case _ =>
         |      e
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tmatch(
          tname("x"),
          Case(
            int(1),
            None,
            tif(
              tinfix(
                tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))),
                "&&",
                tname("w"),
              ),
              tname("c"),
              tname("d"),
            ),
          ),
          Case(patwildcard, None, tname("e")),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` in a case body within braces with `&&` and a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f = xs.map {
         |    case 1 =>
         |      if a && foo:
         |          z
         |
         |      && w
         |      then c else d
         |  }
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(map) [23..26)
         |LeftBrace [27..28)
         |KwCase [33..37)
         |Constant.Int(1) [38..39)
         |RightArrow [40..42)
         |KwIf [49..51)
         |Ident(a) [52..53)
         |Ident(&&) [54..56)
         |Ident(foo) [57..60)
         |Colon [60..61)
         |Indentation.Indent [61..61)
         |Ident(z) [72..73)
         |Indentation.Outdent [74..74)
         |InfixLF [74..75)
         |Ident(&&) [81..83)
         |Ident(w) [84..85)
         |KwThen [92..96)
         |Ident(c) [97..98)
         |KwElse [99..103)
         |Ident(d) [104..105)
         |LF [105..106)
         |RightBrace [108..109)
         |Indentation.Outdent [109..109)
         |EOF [110..110)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map {
         |    case 1 =>
         |      if (a && foo {
         |        z
         |      } && w) c else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect(tname("xs"), "map"),
          Term.PartialFunction(List(Case(
            int(1),
            None,
            tif(
              tinfix(
                tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))),
                "&&",
                tname("w"),
              ),
              tname("c"),
              tname("d"),
            ),
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("case guard in parentheses: a colon argument, the argument on the next line, a blank line, leading `&&`, `)` and `=>` on the next line, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case y if (foo:
         |        z
         |
         |      && w) => 1
         |    case _ => 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Ident(y) [37..38)
         |KwIf [39..41)
         |LeftParen [42..43)
         |Ident(foo) [43..46)
         |Colon [46..47)
         |Indentation.Indent [47..47)
         |Ident(z) [56..57)
         |Indentation.Outdent [58..58)
         |Ident(&&) [65..67)
         |Ident(w) [68..69)
         |RightParen [69..70)
         |RightArrow [71..73)
         |Constant.Int(1) [74..75)
         |LF [75..76)
         |KwCase [80..84)
         |Underscore [85..86)
         |RightArrow [87..89)
         |Constant.Int(2) [90..91)
         |Indentation.Outdent [91..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case y if foo {
         |      z
         |    } && w => 1
         |    case _ => 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w"))),
            int(1),
          ),
          Case(patwildcard, None, int(2)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument, the argument on the next line, a blank line, a comment line at the column of the argument, leading `&&` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |        z
         |
         |        // c
         |    && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(z) [45..46)
         |Indentation.Outdent [60..60)
         |InfixLF [60..61)
         |Ident(&&) [65..67)
         |Ident(w) [68..69)
         |KwThen [74..78)
         |Ident(c) [79..80)
         |KwElse [81..85)
         |Ident(d) [86..87)
         |Indentation.Outdent [87..87)
         |Indentation.Outdent [87..87)
         |EOF [88..88)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (a && foo {
         |    z
         |
         |    // c
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tinfix(
            tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tnameComments("z")()("// c")))),
            "&&",
            tname("w"),
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`try` with a colon argument after `finally`, the argument on the next line, a blank line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    try a finally foo:
         |        z
         |
         |    b
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwTry [24..27)
         |Ident(a) [28..29)
         |KwFinally [30..37)
         |Ident(foo) [38..41)
         |Colon [41..42)
         |Indentation.Indent [42..42)
         |Ident(z) [51..52)
         |Indentation.Outdent [53..53)
         |LFLF [53..54)
         |Ident(b) [58..59)
         |Indentation.Outdent [59..59)
         |Indentation.Outdent [59..59)
         |EOF [60..60)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    try a finally foo {
         |      z
         |    }
         |    b
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.Try(tname("a"), None, Some(tapply(tname("foo"), blk(tname("z"))))), tname("b")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`match` with a colon argument in a case body on the line of the `case`, the argument on the next line, a blank line, next case, next definition") {
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => foo:
         |        z
         |
         |    case 2 => b
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Constant.Int(1) [37..38)
         |RightArrow [39..41)
         |Ident(foo) [42..45)
         |Colon [45..46)
         |Indentation.Indent [46..46)
         |Ident(z) [55..56)
         |Indentation.Outdent [57..57)
         |LFLF [57..58)
         |KwCase [62..66)
         |Constant.Int(2) [67..68)
         |RightArrow [69..71)
         |Ident(b) [72..73)
         |Indentation.Outdent [73..73)
         |LF [73..74)
         |KwDef [76..79)
         |Ident(g) [80..81)
         |Equals [82..83)
         |Constant.Int(1) [84..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      foo {
         |        z
         |      }
         |    case 2 =>
         |      b
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tmatch(
            tname("x"),
            Case(int(1), None, tapply(tname("foo"), blk(tname("z")))),
            Case(int(2), None, tname("b")),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `match` with a colon argument in a case body, the argument on the next line, a blank line, next case, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if x match
         |        case 1 => foo:
         |            z
         |
         |        case _ => false
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(x) [27..28)
         |KwMatch [29..34)
         |Indentation.Indent [34..34)
         |KwCase [43..47)
         |Constant.Int(1) [48..49)
         |RightArrow [50..52)
         |Ident(foo) [53..56)
         |Colon [56..57)
         |Indentation.Indent [57..57)
         |Ident(z) [70..71)
         |Indentation.Outdent [72..72)
         |LFLF [72..73)
         |KwCase [81..85)
         |Underscore [86..87)
         |RightArrow [88..90)
         |KwFalse [91..96)
         |Indentation.Outdent [96..96)
         |KwThen [101..105)
         |Ident(c) [106..107)
         |KwElse [108..112)
         |Ident(d) [113..114)
         |Indentation.Outdent [114..114)
         |Indentation.Outdent [114..114)
         |EOF [115..115)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x match {
         |    case 1 =>
         |      foo {
         |        z
         |      }
         |    case _ =>
         |      false
         |  }) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tmatch(
            tname("x"),
            Case(int(1), None, tapply(tname("foo"), blk(tname("z")))),
            Case(patwildcard, None, bool(false)),
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`for` with a guard that holds a colon argument, the argument on the next line, a blank line, next generator, `do` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    for x <- xs
         |        if foo:
         |          z
         |
         |        y <- ys
         |    do g(x, y)
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Ident(x) [28..29)
         |LeftArrow [30..32)
         |Ident(xs) [33..35)
         |LF [35..36)
         |KwIf [44..46)
         |Ident(foo) [47..50)
         |Colon [50..51)
         |Indentation.Indent [51..51)
         |Ident(z) [62..63)
         |Indentation.Outdent [64..64)
         |LFLF [64..65)
         |Ident(y) [73..74)
         |LeftArrow [75..77)
         |Ident(ys) [78..80)
         |LF [80..81)
         |KwDo [85..87)
         |Ident(g) [88..89)
         |LeftParen [89..90)
         |Ident(x) [90..91)
         |Comma [91..92)
         |Ident(y) [93..94)
         |RightParen [94..95)
         |Indentation.Outdent [95..95)
         |Indentation.Outdent [95..95)
         |EOF [96..96)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = for (x <- xs; if foo {
         |    z
         |  }; y <- ys) g(x, y)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.For(
          Term.EnumeratorsBlock(List(
            Enumerator.Generator(patvar("x"), tname("xs")),
            Enumerator.Guard(tapply(tname("foo"), blk(tname("z")))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          )),
          tapply(tname("g"), tname("x"), tname("y")),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`for` with braces and a generator that holds a colon argument, the argument on the next line, a blank line, next generator") {
    val code =
      """|object A:
         |  def f =
         |    for {
         |      x <- foo:
         |        z
         |
         |      y <- ys
         |    } yield x
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |LeftBrace [28..29)
         |Ident(x) [36..37)
         |LeftArrow [38..40)
         |Ident(foo) [41..44)
         |Colon [44..45)
         |Indentation.Indent [45..45)
         |Ident(z) [54..55)
         |Indentation.Outdent [56..56)
         |LFLF [56..57)
         |Ident(y) [63..64)
         |LeftArrow [65..67)
         |Ident(ys) [68..70)
         |LF [70..71)
         |RightBrace [75..76)
         |KwYield [77..82)
         |Ident(x) [83..84)
         |Indentation.Outdent [84..84)
         |Indentation.Outdent [84..84)
         |EOF [85..85)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = for (x <- foo {
         |    z
         |  }; y <- ys) yield x
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.ForYield(
          Term.EnumeratorsBlock(List(
            Enumerator.Generator(patvar("x"), tapply(tname("foo"), blk(tname("z")))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          )),
          tname("x"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("class body: nested colon arguments, the arguments on the next lines, a blank line, next definition") {
    val code =
      """|class A:
         |  def f = foo:
         |      bar:
         |        z
         |
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwClass [0..5)
         |Ident(A) [6..7)
         |Colon [7..8)
         |Indentation.Indent [8..8)
         |KwDef [11..14)
         |Ident(f) [15..16)
         |Equals [17..18)
         |Ident(foo) [19..22)
         |Colon [22..23)
         |Indentation.Indent [23..23)
         |Ident(bar) [30..33)
         |Colon [33..34)
         |Indentation.Indent [34..34)
         |Ident(z) [43..44)
         |Indentation.Outdent [45..45)
         |Indentation.Outdent [45..45)
         |LFLF [45..46)
         |KwDef [48..51)
         |Ident(g) [52..53)
         |Equals [54..55)
         |Constant.Int(1) [56..57)
         |Indentation.Outdent [57..57)
         |EOF [58..58)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|class A {
         |  def f = foo {
         |    bar {
         |      z
         |    }
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      ctor,
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tname("foo"), blk(tapply(tname("bar"), blk(tname("z"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`new` with a template body: a colon argument, the argument on the next line, a blank line, next definition in the body, next definition") {
    val code =
      """|object A:
         |  val a = new B:
         |    def f = foo:
         |        z
         |
         |    def g = 1
         |  def h = 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwVal [12..15)
         |Ident(a) [16..17)
         |Equals [18..19)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |Colon [25..26)
         |Indentation.Indent [26..26)
         |KwDef [31..34)
         |Ident(f) [35..36)
         |Equals [37..38)
         |Ident(foo) [39..42)
         |Colon [42..43)
         |Indentation.Indent [43..43)
         |Ident(z) [52..53)
         |Indentation.Outdent [54..54)
         |LFLF [54..55)
         |KwDef [59..62)
         |Ident(g) [63..64)
         |Equals [65..66)
         |Constant.Int(1) [67..68)
         |Indentation.Outdent [68..68)
         |LF [68..69)
         |KwDef [71..74)
         |Ident(h) [75..76)
         |Equals [77..78)
         |Constant.Int(2) [79..80)
         |Indentation.Outdent [80..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  val a = new B {
         |    def f = foo {
         |      z
         |    }
         |    def g = 1
         |  }
         |  def h = 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(
          Nil,
          List(patvar("a")),
          None,
          Term.NewAnonymous(tpl(
            List(init(pname("B"))),
            List(
              Defn.Def(Nil, tname("f"), Nil, None, tapply(tname("foo"), blk(tname("z")))),
              Defn.Def(Nil, tname("g"), Nil, None, int(1)),
            ),
          )),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, int(2)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` with `try` and a colon argument in its `then` body, the argument on the next line, a blank line, `finally` on the next line, `else` on the line after") {
    val code =
      """|object A:
         |  def f =
         |    if c then try foo:
         |        z
         |
         |    finally b
         |    else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(c) [27..28)
         |KwThen [29..33)
         |KwTry [34..37)
         |Ident(foo) [38..41)
         |Colon [41..42)
         |Indentation.Indent [42..42)
         |Ident(z) [51..52)
         |Indentation.Outdent [53..53)
         |KwFinally [58..65)
         |Ident(b) [66..67)
         |KwElse [72..76)
         |Ident(d) [77..78)
         |Indentation.Outdent [78..78)
         |Indentation.Outdent [78..78)
         |EOF [79..79)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (c) try foo {
         |    z
         |  } finally b else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tname("c"),
          Term.Try(tapply(tname("foo"), blk(tname("z"))), None, Some(tname("b"))),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("default parameter value: a colon argument, the argument on the next line, a blank line, next parameter after a leading comma") {
    val code =
      """|object A:
         |  def f(x: Int = foo:
         |      z
         |
         |    , y: Int) = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |LeftParen [17..18)
         |Ident(x) [18..19)
         |Colon [19..20)
         |Ident(Int) [21..24)
         |Equals [25..26)
         |Ident(foo) [27..30)
         |Colon [30..31)
         |Ident(z) [38..39)
         |Comma [45..46)
         |Ident(y) [47..48)
         |Colon [48..49)
         |Ident(Int) [50..53)
         |RightParen [53..54)
         |Equals [55..56)
         |Constant.Int(1) [57..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A { def f(x: Int = foo: z, y: Int) = 1 }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        List(pcg(tpc(
          Term.Param(Nil, tname("x"), Some(pname("Int")), Some(Term.Ascribe(tname("foo"), pname("z")))),
          Term.Param(Nil, tname("y"), Some(pname("Int")), None),
        ))),
        None,
        int(1),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a colon argument, the argument on the next line, a blank line, `catch` on the next line, `then` and `else` on the line after") {
    val code =
      """|object A:
         |  def f =
         |    if try foo:
         |        z
         |
         |    catch case _ => false
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |Ident(foo) [31..34)
         |Colon [34..35)
         |Indentation.Indent [35..35)
         |Ident(z) [44..45)
         |Indentation.Outdent [46..46)
         |KwCatch [51..56)
         |Indentation.Indent [57..57)
         |KwCase [57..61)
         |Underscore [62..63)
         |RightArrow [64..66)
         |KwFalse [67..72)
         |Indentation.Outdent [72..72)
         |KwThen [77..81)
         |Ident(c) [82..83)
         |KwElse [84..88)
         |Ident(d) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (try foo {
         |    z
         |  } catch {
         |    case _ => false
         |  }) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          Term.Try(
            tapply(tname("foo"), blk(tname("z"))),
            Some(Term.CasesBlock(List(Case(patwildcard, None, bool(false))))),
            None,
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`try` within parentheses with a colon argument, the argument on the next line, a blank line, `catch` and `)` on the next line") {
    val code =
      """|object A:
         |  def f = g(try foo:
         |      z
         |
         |    catch case _ => 1)
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(g) [20..21)
         |LeftParen [21..22)
         |KwTry [22..25)
         |Ident(foo) [26..29)
         |Colon [29..30)
         |Indentation.Indent [30..30)
         |Ident(z) [37..38)
         |Indentation.Outdent [39..39)
         |KwCatch [44..49)
         |Indentation.Indent [50..50)
         |KwCase [50..54)
         |Underscore [55..56)
         |RightArrow [57..59)
         |Constant.Int(1) [60..61)
         |Indentation.Outdent [60..60)
         |RightParen [61..62)
         |Indentation.Outdent [62..62)
         |EOF [63..63)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = g(try foo {
         |    z
         |  } catch {
         |    case _ => 1
         |  })
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tname("g"),
          Term.Try(
            tapply(tname("foo"), blk(tname("z"))),
            Some(Term.CasesBlock(List(Case(patwildcard, None, int(1))))),
            None,
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("a colon argument within parentheses after type arguments, the argument on the next line, a blank line, next argument after a leading comma") {
    val code =
      """|object A:
         |  def f = g[Int](foo:
         |      z
         |
         |    , 2)
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(g) [20..21)
         |LeftBracket [21..22)
         |Ident(Int) [22..25)
         |RightBracket [25..26)
         |LeftParen [26..27)
         |Ident(foo) [27..30)
         |Colon [30..31)
         |Indentation.Indent [31..31)
         |Ident(z) [38..39)
         |Indentation.Outdent [40..40)
         |Comma [45..46)
         |Constant.Int(2) [47..48)
         |RightParen [48..49)
         |Indentation.Outdent [49..49)
         |EOF [50..50)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = g[Int](foo {
         |    z
         |  }, 2)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tapplytype(tname("g"), pname("Int")), tapply(tname("foo"), blk(tname("z"))), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument, the argument on the next line, a blank line, `then` and `else` at the column of the `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |        z
         |
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(z) [45..46)
         |Indentation.Outdent [47..47)
         |KwThen [52..56)
         |Ident(c) [57..58)
         |KwElse [59..63)
         |Ident(d) [64..65)
         |LF [65..66)
         |Ident(g) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a && foo {
         |      z
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), tname("c"), tname("d")),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`given` with a body: a colon argument, the argument on the next line, a blank line, next definition") {
    val code =
      """|object A:
         |  given B with
         |    def f = foo:
         |        z
         |
         |    def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwGiven [12..17)
         |Ident(B) [18..19)
         |KwWith [20..24)
         |Indentation.Indent [24..24)
         |KwDef [29..32)
         |Ident(f) [33..34)
         |Equals [35..36)
         |Ident(foo) [37..40)
         |Colon [40..41)
         |Indentation.Indent [41..41)
         |Ident(z) [50..51)
         |Indentation.Outdent [52..52)
         |LFLF [52..53)
         |KwDef [57..60)
         |Ident(g) [61..62)
         |Equals [63..64)
         |Constant.Int(1) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  given B with {
         |    def f = foo {
         |      z
         |    }
         |    def g = 1
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Given(
        Nil,
        anon,
        Nil,
        tpl(
          List(init(pname("B"))),
          List(
            Defn.Def(Nil, tname("f"), Nil, None, tapply(tname("foo"), blk(tname("z")))),
            Defn.Def(Nil, tname("g"), Nil, None, int(1)),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `try` and a colon argument, the argument on the next line, a blank line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while try foo:
         |        z
         |
         |    do b
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwTry [30..33)
         |Ident(foo) [34..37)
         |Colon [37..38)
         |Indentation.Indent [38..38)
         |Ident(z) [47..48)
         |Indentation.Outdent [49..49)
         |KwDo [54..56)
         |Ident(b) [57..58)
         |LF [58..59)
         |Ident(g) [63..64)
         |Indentation.Outdent [64..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (try foo {
         |      z
         |    }) b
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(Term.Try(tapply(tname("foo"), blk(tname("z"))), None, None), tname("b")),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` and a colon argument, the argument on the next line, a blank line, shallower leading `&&` on the next line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while x && foo:
         |        z
         |
         |      && w
         |    do c
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Ident(x) [30..31)
         |Ident(&&) [32..34)
         |Ident(foo) [35..38)
         |Colon [38..39)
         |Indentation.Indent [39..39)
         |Ident(z) [48..49)
         |Indentation.Outdent [50..50)
         |InfixLF [50..51)
         |Ident(&&) [57..59)
         |Ident(w) [60..61)
         |KwDo [66..68)
         |Ident(c) [69..70)
         |LF [70..71)
         |Ident(g) [75..76)
         |Indentation.Outdent [76..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (x && foo {
         |      z
         |    } && w) c
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tinfix(tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            tname("c"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (x)(y) && foo:
         |        z
         |
         |    && w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(x) [28..29)
         |RightParen [29..30)
         |LeftParen [30..31)
         |Ident(y) [31..32)
         |RightParen [32..33)
         |Ident(&&) [34..36)
         |Ident(foo) [37..40)
         |Colon [40..41)
         |Indentation.Indent [41..41)
         |Ident(z) [50..51)
         |Indentation.Outdent [52..52)
         |InfixLF [52..53)
         |Ident(&&) [57..59)
         |Ident(w) [60..61)
         |KwThen [66..70)
         |Ident(c) [71..72)
         |KwElse [73..77)
         |Ident(d) [78..79)
         |LF [79..80)
         |Ident(g) [84..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (x(y) && foo {
         |      z
         |    } && w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon lambda after the parentheses, its body on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f = if (x)(y) && xs.exists: x =>
         |        z
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(xs) [33..35)
         |Dot [35..36)
         |Ident(exists) [36..42)
         |Colon [42..43)
         |Ident(x) [44..45)
         |RightArrow [46..48)
         |Indentation.Indent [48..48)
         |Ident(z) [57..58)
         |Indentation.Outdent [59..59)
         |InfixLF [59..60)
         |Ident(&&) [66..68)
         |Ident(w) [69..70)
         |KwThen [75..79)
         |Ident(c) [80..81)
         |KwElse [82..86)
         |Ident(d) [87..88)
         |LF [88..89)
         |KwDef [91..94)
         |Ident(g) [95..96)
         |Equals [97..98)
         |Constant.Int(1) [99..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && xs.exists {
         |    x => z
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(
                tapply(tname("x"), tname("y")),
                "&&",
                tapply(tselect(tname("xs"), "exists"), blk(tfunc(tparam("x"))(tname("z")))),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and nested colon arguments after the parentheses, the arguments on the next lines, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        bar:
         |            z
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(bar) [46..49)
         |Colon [49..50)
         |Indentation.Indent [50..50)
         |Ident(z) [63..64)
         |Indentation.Outdent [65..65)
         |Indentation.Outdent [65..65)
         |InfixLF [65..66)
         |Ident(&&) [72..74)
         |Ident(w) [75..76)
         |KwThen [81..85)
         |Ident(c) [86..87)
         |KwElse [88..92)
         |Ident(d) [93..94)
         |LF [94..95)
         |KwDef [97..100)
         |Ident(g) [101..102)
         |Equals [103..104)
         |Constant.Int(1) [105..106)
         |Indentation.Outdent [106..106)
         |EOF [107..107)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    bar {
         |      z
         |    }
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(
                tapply(tname("x"), tname("y")),
                "&&",
                tapply(tname("foo"), blk(tapply(tname("bar"), blk(tname("z"))))),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, a comment line, shallower leading `&&` on the next line, `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |
         |      // c
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |Indentation.Outdent [48..48)
         |InfixLF [59..60)
         |Ident(&&) [66..68)
         |Ident(w) [69..70)
         |KwThen [75..79)
         |Ident(c) [80..81)
         |KwElse [82..86)
         |Ident(d) [87..88)
         |LF [88..89)
         |KwDef [91..94)
         |Ident(g) [95..96)
         |Equals [97..98)
         |Constant.Int(1) [99..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z
         |  }
         |    // c
         |    && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              Term.Name.newBuilder("&&").begComment(detachedComments("// c")).result(),
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon lambda within parentheses, its body on the next line, a blank line, leading `+` and `)` on the next line, next definition") {
    val code =
      """|object A:
         |  def f = g(xs.map: x =>
         |        x
         |
         |      + b)
         |  def h = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(g) [20..21)
         |LeftParen [21..22)
         |Ident(xs) [22..24)
         |Dot [24..25)
         |Ident(map) [25..28)
         |Colon [28..29)
         |Ident(x) [30..31)
         |RightArrow [32..34)
         |Indentation.Indent [34..34)
         |Ident(x) [43..44)
         |Indentation.Outdent [45..45)
         |Ident(+) [52..53)
         |Ident(b) [54..55)
         |RightParen [55..56)
         |LF [56..57)
         |KwDef [59..62)
         |Ident(h) [63..64)
         |Equals [65..66)
         |Constant.Int(1) [67..68)
         |Indentation.Outdent [68..68)
         |EOF [69..69)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = g(xs.map {
         |    x => x
         |  } + b)
         |  def h = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(
            tname("g"),
            tinfix(
              tapply(tselect(tname("xs"), "map"), blk(tfunc(tparam("x"))(tname("x")))),
              "+",
              tname("b"),
            ),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` on the line after `if`, `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, shallower leading `&&` on the next line, both `then` and `else` on the next lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (x)(y) && foo:
         |          z
         |
         |        && w
         |      then c else d
         |    then e else h
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(x) [37..38)
         |RightParen [38..39)
         |LeftParen [39..40)
         |Ident(y) [40..41)
         |RightParen [41..42)
         |Ident(&&) [43..45)
         |Ident(foo) [46..49)
         |Colon [49..50)
         |Indentation.Indent [50..50)
         |Ident(z) [61..62)
         |Indentation.Outdent [63..63)
         |InfixLF [63..64)
         |Ident(&&) [72..74)
         |Ident(w) [75..76)
         |KwThen [83..87)
         |Ident(c) [88..89)
         |KwElse [90..94)
         |Ident(d) [95..96)
         |Indentation.Outdent [96..96)
         |KwThen [101..105)
         |Ident(e) [106..107)
         |KwElse [108..112)
         |Ident(h) [113..114)
         |LF [114..115)
         |Ident(g) [119..120)
         |Indentation.Outdent [120..120)
         |Indentation.Outdent [120..120)
         |EOF [121..121)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (x(y) && foo {
         |      z
         |    } && w) c else d) e else h
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(
                tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
                "&&",
                tname("w"),
              ),
              tname("c"),
              tname("d"),
            ),
            tname("e"),
            tname("h"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, a shallower expression in parentheses on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (x)(y) && foo:
         |        z
         |
         |      (w)
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(x) [28..29)
         |RightParen [29..30)
         |LeftParen [30..31)
         |Ident(y) [31..32)
         |RightParen [32..33)
         |Ident(&&) [34..36)
         |Ident(foo) [37..40)
         |Colon [40..41)
         |Indentation.Indent [41..41)
         |Ident(z) [50..51)
         |Indentation.Outdent [52..52)
         |LeftParen [59..60)
         |Ident(w) [60..61)
         |RightParen [61..62)
         |KwThen [67..71)
         |Ident(c) [72..73)
         |KwElse [74..78)
         |Ident(d) [79..80)
         |LF [80..81)
         |Ident(g) [85..86)
         |Indentation.Outdent [86..86)
         |Indentation.Outdent [86..86)
         |EOF [87..87)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (x(y) && foo {
         |      z
         |    }(w)) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tapply(tname("foo"), blk(tname("z"))), tname("w")),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a colon lambda, its body on the next line, a blank line, shallower leading `==` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists: x =>
         |        x
         |
         |      == w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(xs) [27..29)
         |Dot [29..30)
         |Ident(exists) [30..36)
         |Colon [36..37)
         |Ident(x) [38..39)
         |RightArrow [40..42)
         |Indentation.Indent [42..42)
         |Ident(x) [51..52)
         |Indentation.Outdent [53..53)
         |InfixLF [53..54)
         |Ident(==) [60..62)
         |Ident(w) [63..64)
         |KwThen [69..73)
         |Ident(c) [74..75)
         |KwElse [76..80)
         |Ident(d) [81..82)
         |LF [82..83)
         |Ident(g) [87..88)
         |Indentation.Outdent [88..88)
         |Indentation.Outdent [88..88)
         |EOF [89..89)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (xs.exists {
         |      x => x
         |    } == w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tapply(tselect(tname("xs"), "exists"), blk(tfunc(tparam("x"))(tname("x")))),
              "==",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: after a statement with a leading `+` line, `&&` and a colon argument, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    if x && foo:
         |        z
         |
         |      && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(v) [28..29)
         |Equals [30..31)
         |Ident(a) [32..33)
         |InfixLF [33..34)
         |Ident(+) [40..41)
         |Ident(b) [42..43)
         |LF [43..44)
         |KwIf [48..50)
         |Ident(x) [51..52)
         |Ident(&&) [53..55)
         |Ident(foo) [56..59)
         |Colon [59..60)
         |Indentation.Indent [60..60)
         |Ident(z) [69..70)
         |Indentation.Outdent [71..71)
         |InfixLF [71..72)
         |Ident(&&) [78..80)
         |Ident(w) [81..82)
         |KwThen [87..91)
         |Ident(c) [92..93)
         |KwElse [94..98)
         |Ident(d) [99..100)
         |Indentation.Outdent [100..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    val v = a + b
         |    if (x && foo {
         |      z
         |    } && w) c else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
          tif(
            tinfix(tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            tname("c"),
            tname("d"),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`try` within parentheses with a colon argument, the argument on the next line, a blank line, leading `+` and `)` on the next line") {
    val code =
      """|object A:
         |  def f = g(try foo:
         |      z
         |
         |    + w)
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(g) [20..21)
         |LeftParen [21..22)
         |KwTry [22..25)
         |Ident(foo) [26..29)
         |Colon [29..30)
         |Indentation.Indent [30..30)
         |Ident(z) [37..38)
         |Indentation.Outdent [39..39)
         |InfixLF [39..40)
         |Ident(+) [44..45)
         |Ident(w) [46..47)
         |RightParen [47..48)
         |Indentation.Outdent [48..48)
         |EOF [49..49)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = g(try foo {
         |    z
         |  } + w)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tname("g"),
          Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "+", tname("w")), None, None),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("lambda within parentheses with `try` and a colon argument, the argument on the next line, a blank line, leading `+` and `)` on the next line") {
    val code =
      """|object A:
         |  def f = xs.map(x => try foo:
         |      z
         |
         |    + w)
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(map) [23..26)
         |LeftParen [26..27)
         |Ident(x) [27..28)
         |RightArrow [29..31)
         |KwTry [32..35)
         |Ident(foo) [36..39)
         |Colon [39..40)
         |Indentation.Indent [40..40)
         |Ident(z) [47..48)
         |Indentation.Outdent [49..49)
         |InfixLF [49..50)
         |Ident(+) [54..55)
         |Ident(w) [56..57)
         |RightParen [57..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map(x => try foo {
         |    z
         |  } + w)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect(tname("xs"), "map"),
          tfunc(
            tparam("x"),
          )(Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "+", tname("w")), None, None)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: top-level `def` with `if`, `&&` and a colon argument, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line") {
    val code =
      """|def f = if a && foo:
         |    z
         |
         |  && w
         |  then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |KwIf [8..10)
         |Ident(a) [11..12)
         |Ident(&&) [13..15)
         |Ident(foo) [16..19)
         |Colon [19..20)
         |Indentation.Indent [20..20)
         |Ident(z) [25..26)
         |Indentation.Outdent [27..27)
         |InfixLF [27..28)
         |Ident(&&) [30..32)
         |Ident(w) [33..34)
         |KwThen [37..41)
         |Ident(c) [42..43)
         |KwElse [44..48)
         |Ident(d) [49..50)
         |EOF [51..51)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = if (a && foo {
         |  z
         |} && w) c else d
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
        tname("c"),
        tname("d"),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: top-level `def` with `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line") {
    val code =
      """|def f = if (x)(y) && foo:
         |    z
         |
         |  && w
         |  then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |KwIf [8..10)
         |LeftParen [11..12)
         |Ident(x) [12..13)
         |RightParen [13..14)
         |LeftParen [14..15)
         |Ident(y) [15..16)
         |RightParen [16..17)
         |Ident(&&) [18..20)
         |Ident(foo) [21..24)
         |Colon [24..25)
         |Indentation.Indent [25..25)
         |Ident(z) [30..31)
         |Indentation.Outdent [32..32)
         |InfixLF [32..33)
         |Ident(&&) [35..37)
         |Ident(w) [38..39)
         |KwThen [42..46)
         |Ident(c) [47..48)
         |KwElse [49..53)
         |Ident(d) [54..55)
         |EOF [56..56)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = if (x(y) && foo {
         |  z
         |} && w) c else d
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(
          tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
          "&&",
          tname("w"),
        ),
        tname("c"),
        tname("d"),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: top-level `val` with `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next definition") {
    val code =
      """|val r = if (x)(y) && foo:
         |    z
         |
         |  && w
         |  then c else d
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwVal [0..3)
         |Ident(r) [4..5)
         |Equals [6..7)
         |KwIf [8..10)
         |LeftParen [11..12)
         |Ident(x) [12..13)
         |RightParen [13..14)
         |LeftParen [14..15)
         |Ident(y) [15..16)
         |RightParen [16..17)
         |Ident(&&) [18..20)
         |Ident(foo) [21..24)
         |Colon [24..25)
         |Indentation.Indent [25..25)
         |Ident(z) [30..31)
         |Indentation.Outdent [32..32)
         |InfixLF [32..33)
         |Ident(&&) [35..37)
         |Ident(w) [38..39)
         |KwThen [42..46)
         |Ident(c) [47..48)
         |KwElse [49..53)
         |Ident(d) [54..55)
         |LF [55..56)
         |KwDef [56..59)
         |Ident(g) [60..61)
         |Equals [62..63)
         |Constant.Int(1) [64..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|val r = if (x(y) && foo {
         |  z
         |} && w) c else d
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Val(
        Nil,
        List(patvar("r")),
        None,
        tif(
          tinfix(
            tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
            "&&",
            tname("w"),
          ),
          tname("c"),
          tname("d"),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if try foo:
         |        z
         |
         |    && w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |Ident(foo) [31..34)
         |Colon [34..35)
         |Indentation.Indent [35..35)
         |Ident(z) [44..45)
         |Indentation.Outdent [46..46)
         |InfixLF [46..47)
         |Ident(&&) [51..53)
         |Ident(w) [54..55)
         |KwThen [60..64)
         |Ident(c) [65..66)
         |KwElse [67..71)
         |Ident(d) [72..73)
         |LF [73..74)
         |Ident(g) [78..79)
         |Indentation.Outdent [79..79)
         |Indentation.Outdent [79..79)
         |EOF [80..80)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (try foo {
         |      z
         |    } && w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a colon argument, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f = if try foo:
         |        z
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |KwTry [23..26)
         |Ident(foo) [27..30)
         |Colon [30..31)
         |Indentation.Indent [31..31)
         |Ident(z) [40..41)
         |Indentation.Outdent [42..42)
         |InfixLF [42..43)
         |Ident(&&) [49..51)
         |Ident(w) [52..53)
         |KwThen [58..62)
         |Ident(c) [63..64)
         |KwElse [65..69)
         |Ident(d) [70..71)
         |LF [71..72)
         |KwDef [74..77)
         |Ident(g) [78..79)
         |Equals [80..81)
         |Constant.Int(1) [82..83)
         |Indentation.Outdent [83..83)
         |EOF [84..84)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (try foo {
         |    z
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `try` and a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `while`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while try foo:
         |        z
         |
         |    && w
         |    do c
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwTry [30..33)
         |Ident(foo) [34..37)
         |Colon [37..38)
         |Indentation.Indent [38..38)
         |Ident(z) [47..48)
         |Indentation.Outdent [49..49)
         |InfixLF [49..50)
         |Ident(&&) [54..56)
         |Ident(w) [57..58)
         |KwDo [63..65)
         |Ident(c) [66..67)
         |LF [67..68)
         |Ident(g) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (try foo {
         |      z
         |    } && w) c
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            tname("c"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a comment line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |        // c
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z
         |      // c
         |      && w
         |  }) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tnameComments("z")()("// c"), "&&", tname("w")))),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    parseAndCheckTree[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses in braces, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line at the column of the `if`, next statement") {
    val code =
      """|object A {
         |  def f = {
         |    if (x)(y) && foo:
         |        z
         |      && w
         |    then c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    if (x(y) && foo {
         |      z && w
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses in an indented block, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line at the column of the `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (x)(y) && foo:
         |        z
         |      && w
         |    then c else d
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    if (x(y) && foo {
         |      z && w
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses after a statement in an indented block, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line at the column of the `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    e
         |    if (x)(y) && foo:
         |        z
         |      && w
         |    then c else d
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    e
         |    if (x(y) && foo {
         |      z && w
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tname("e"),
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` and a colon argument after the parentheses in an indented block, the argument on the next line, shallower leading `&&` on the next line, `do` on the next line at the column of the `while`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while (x)(y) && foo:
         |        z
         |      && w
         |    do c
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    while (x(y) && foo {
         |      z && w
         |    }) c
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses at the line end, an argument in parentheses with `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while (a)
         |      (x) do g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |LeftParen [40..41)
         |Ident(x) [41..42)
         |RightParen [42..43)
         |KwDo [44..46)
         |Ident(g) [47..48)
         |Indentation.Outdent [48..48)
         |LF [48..49)
         |Ident(h) [53..54)
         |Indentation.Outdent [54..54)
         |Indentation.Outdent [54..54)
         |EOF [55..55)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `do` found
         |      (x) do g
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end, an argument in braces with `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (a)
         |      { x } then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Indentation.Indent [30..30)
         |LeftBrace [37..38)
         |Ident(x) [39..40)
         |RightBrace [41..42)
         |KwThen [43..47)
         |Constant.Int(1) [48..49)
         |KwElse [50..54)
         |Constant.Int(2) [55..56)
         |Indentation.Outdent [56..56)
         |LF [56..57)
         |Ident(g) [61..62)
         |Indentation.Outdent [62..62)
         |Indentation.Outdent [62..62)
         |EOF [63..63)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `then` found
         |      { x } then 1 else 2
         |            ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `while` with parentheses at the line end, an argument in braces with `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while (a)
         |      { x } do g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |LeftBrace [40..41)
         |Ident(x) [42..43)
         |RightBrace [44..45)
         |KwDo [46..48)
         |Ident(g) [49..50)
         |Indentation.Outdent [50..50)
         |LF [50..51)
         |Ident(h) [55..56)
         |Indentation.Outdent [56..56)
         |Indentation.Outdent [56..56)
         |EOF [57..57)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `do` found
         |      { x } do g
         |            ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end, an argument in parentheses on the next line, `then` and `else` on the line after it, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (a)
         |      (x)
         |    then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Indentation.Indent [30..30)
         |LeftParen [37..38)
         |Ident(x) [38..39)
         |RightParen [39..40)
         |Indentation.Outdent [40..40)
         |KwThen [45..49)
         |Constant.Int(1) [50..51)
         |KwElse [52..56)
         |Constant.Int(2) [57..58)
         |LF [58..59)
         |Ident(g) [63..64)
         |Indentation.Outdent [64..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `while` with parentheses at the line end, an argument in parentheses on the next line, `do` on the line after it, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while (a)
         |      (x)
         |    do g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |LeftParen [40..41)
         |Ident(x) [41..42)
         |RightParen [42..43)
         |Indentation.Outdent [43..43)
         |LF [43..44)
         |KwDo [48..50)
         |Ident(g) [51..52)
         |LF [52..53)
         |Ident(h) [57..58)
         |Indentation.Outdent [58..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `while` expected but `identifier` found
         |    h
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end, a comment line, an argument in parentheses with `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (a)
         |      // c
         |      (x) then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Indentation.Indent [41..41)
         |LeftParen [48..49)
         |Ident(x) [49..50)
         |RightParen [50..51)
         |KwThen [52..56)
         |Constant.Int(1) [57..58)
         |KwElse [59..63)
         |Constant.Int(2) [64..65)
         |Indentation.Outdent [65..65)
         |LF [65..66)
         |Ident(g) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |      (x) then 1 else 2
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end in braces, an argument in parentheses with `then` and `else` on the next line, next statement") {
    val code =
      """|object A {
         |  def f = {
         |    if (a)
         |      (x) then 1 else 2
         |    g
         |  }
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |LeftBrace [9..10)
         |KwDef [13..16)
         |Ident(f) [17..18)
         |Equals [19..20)
         |LeftBrace [21..22)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |LeftParen [40..41)
         |Ident(x) [41..42)
         |RightParen [42..43)
         |KwThen [44..48)
         |Constant.Int(1) [49..50)
         |KwElse [51..55)
         |Constant.Int(2) [56..57)
         |Indentation.Outdent [57..57)
         |LF [57..58)
         |Ident(g) [62..63)
         |LF [63..64)
         |RightBrace [66..67)
         |LF [67..68)
         |RightBrace [68..69)
         |EOF [70..70)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `then` found
         |      (x) then 1 else 2
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end after a statement in an indented block, an argument in parentheses with `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    e
         |    if (a)
         |      (x) then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(e) [24..25)
         |LF [25..26)
         |KwIf [30..32)
         |LeftParen [33..34)
         |Ident(a) [34..35)
         |RightParen [35..36)
         |Indentation.Indent [36..36)
         |LeftParen [43..44)
         |Ident(x) [44..45)
         |RightParen [45..46)
         |KwThen [47..51)
         |Constant.Int(1) [52..53)
         |KwElse [54..58)
         |Constant.Int(2) [59..60)
         |Indentation.Outdent [60..60)
         |LF [60..61)
         |Ident(g) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |      (x) then 1 else 2
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end on the line of the `def`, an argument in parentheses with `then` and `else` on the next line at a column before the `if`, next definition") {
    val code =
      """|object A:
         |  def f = if (a)
         |    (x) then 1 else 2
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(a) [24..25)
         |RightParen [25..26)
         |Indentation.Indent [26..26)
         |LeftParen [31..32)
         |Ident(x) [32..33)
         |RightParen [33..34)
         |KwThen [35..39)
         |Constant.Int(1) [40..41)
         |KwElse [42..46)
         |Constant.Int(2) [47..48)
         |Indentation.Outdent [48..48)
         |LF [48..49)
         |KwDef [51..54)
         |Ident(g) [55..56)
         |Equals [57..58)
         |Constant.Int(1) [59..60)
         |Indentation.Outdent [60..60)
         |EOF [61..61)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:3: error: `outdent` expected but `then` found
         |    (x) then 1 else 2
         |        ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `while` with `==` after the parentheses in an indented `val` body, `do` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      while (a) == b
         |    do g
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwWhile [38..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(==) [48..50)
         |Ident(b) [51..52)
         |Indentation.Outdent [52..52)
         |LF [52..53)
         |KwDo [57..59)
         |Ident(g) [60..61)
         |LF [61..62)
         |Ident(r) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `while` expected but `identifier` found
         |    r
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` without parentheses with `==` in an indented `val` body, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      if a == b
         |    then 1 else 2
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwIf [38..40)
         |Ident(a) [41..42)
         |Ident(==) [43..45)
         |Ident(b) [46..47)
         |Indentation.Outdent [47..47)
         |KwThen [52..56)
         |Constant.Int(1) [57..58)
         |KwElse [59..63)
         |Constant.Int(2) [64..65)
         |LF [65..66)
         |Ident(r) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `then` expected but `outdent` found
         |      if a == b
         |               ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `while` without parentheses with `==` in an indented `val` body, `do` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      while a == b
         |    do g
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwWhile [38..43)
         |Ident(a) [44..45)
         |Ident(==) [46..48)
         |Ident(b) [49..50)
         |Indentation.Outdent [50..50)
         |LF [50..51)
         |KwDo [55..57)
         |Ident(g) [58..59)
         |LF [59..60)
         |Ident(r) [64..65)
         |Indentation.Outdent [65..65)
         |Indentation.Outdent [65..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `do` expected but `outdent` found
         |      while a == b
         |                  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with `==` after the parentheses in an indented `val` body, a blank line, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      if (a) == b
         |
         |    then 1 else 2
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwIf [38..40)
         |LeftParen [41..42)
         |Ident(a) [42..43)
         |RightParen [43..44)
         |Ident(==) [45..47)
         |Ident(b) [48..49)
         |Indentation.Outdent [50..50)
         |KwThen [55..59)
         |Constant.Int(1) [60..61)
         |KwElse [62..66)
         |Constant.Int(2) [67..68)
         |LF [68..69)
         |Ident(r) [73..74)
         |Indentation.Outdent [74..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with `==` after the parentheses in an indented `val` body, a comment line, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      if (a) == b
         |      // c
         |    then 1 else 2
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwIf [38..40)
         |LeftParen [41..42)
         |Ident(a) [42..43)
         |RightParen [43..44)
         |Ident(==) [45..47)
         |Ident(b) [48..49)
         |Indentation.Outdent [60..60)
         |KwThen [65..69)
         |Constant.Int(1) [70..71)
         |KwElse [72..76)
         |Constant.Int(2) [77..78)
         |LF [78..79)
         |Ident(r) [83..84)
         |Indentation.Outdent [84..84)
         |Indentation.Outdent [84..84)
         |EOF [85..85)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with `==` after the parentheses in an indented `val` body in braces, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A {
         |  def f = {
         |    val r =
         |      if (a) == b
         |    then 1 else 2
         |    r
         |  }
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |LeftBrace [9..10)
         |KwDef [13..16)
         |Ident(f) [17..18)
         |Equals [19..20)
         |LeftBrace [21..22)
         |KwVal [27..30)
         |Ident(r) [31..32)
         |Equals [33..34)
         |Indentation.Indent [34..34)
         |KwIf [41..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(==) [48..50)
         |Ident(b) [51..52)
         |Indentation.Outdent [52..52)
         |KwThen [57..61)
         |Constant.Int(1) [62..63)
         |KwElse [64..68)
         |Constant.Int(2) [69..70)
         |LF [70..71)
         |Ident(r) [75..76)
         |LF [76..77)
         |RightBrace [79..80)
         |LF [80..81)
         |RightBrace [81..82)
         |EOF [83..83)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `}` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: a statement and an `if` with `==` after the parentheses in an indented `val` body, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      e
         |      if (a) == b
         |    then 1 else 2
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |Ident(e) [38..39)
         |LF [39..40)
         |KwIf [46..48)
         |LeftParen [49..50)
         |Ident(a) [50..51)
         |RightParen [51..52)
         |Ident(==) [53..55)
         |Ident(b) [56..57)
         |Indentation.Outdent [57..57)
         |KwThen [62..66)
         |Constant.Int(1) [67..68)
         |KwElse [69..73)
         |Constant.Int(2) [74..75)
         |LF [75..76)
         |Ident(r) [80..81)
         |Indentation.Outdent [81..81)
         |Indentation.Outdent [81..81)
         |EOF [82..82)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with `==` after the parentheses in an indented `def` body, `then` and `else` on the next line at the column of the `def`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    def h =
         |      if (a) == b
         |    then 1 else 2
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwDef [24..27)
         |Ident(h) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwIf [38..40)
         |LeftParen [41..42)
         |Ident(a) [42..43)
         |RightParen [43..44)
         |Ident(==) [45..47)
         |Ident(b) [48..49)
         |Indentation.Outdent [49..49)
         |KwThen [54..58)
         |Constant.Int(1) [59..60)
         |KwElse [61..65)
         |Constant.Int(2) [66..67)
         |LF [67..68)
         |Ident(h) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `for` with a guard with `==` after the parentheses in an indented `val` body, `yield` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      for x <- xs if (a) == b
         |    yield x
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwFor [38..41)
         |Ident(x) [42..43)
         |LeftArrow [44..46)
         |Ident(xs) [47..49)
         |KwIf [50..52)
         |LeftParen [53..54)
         |Ident(a) [54..55)
         |RightParen [55..56)
         |Ident(==) [57..59)
         |Ident(b) [60..61)
         |Indentation.Outdent [61..61)
         |KwYield [66..71)
         |Ident(x) [72..73)
         |LF [73..74)
         |Ident(r) [78..79)
         |Indentation.Outdent [79..79)
         |Indentation.Outdent [79..79)
         |EOF [80..80)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: illegal start of simple expression
         |      for x <- xs if (a) == b
         |                             ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("leading infix after a colon argument: a continued statement, the argument in a `then` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    if c then a && foo:
         |        z
         |      && w
         |    else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val v = a + b
         |    if (c) a && foo {
         |      z
         |    } && w else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
          Term.If(
            tname("c"),
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            tname("d"),
            Nil,
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued statement, the argument in a `try` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    try a && foo:
         |        z
         |      && w
         |    finally d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val v = a + b
         |    try a && foo {
         |      z
         |    } && w finally d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
          Term.Try(
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            Nil,
            Some(tname("d")),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after an indented condition: a continued statement, a condition of two statements, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    if
         |        val q = 1
         |        q
         |      && w
         |    then c else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val v = a + b
         |    if ({
         |      val q = 1
         |      q
         |    } && w) c else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
          Term.If(
            tinfix(blk(Defn.Val(Nil, List(patvar("q")), None, int(1)), tname("q")), "&&", tname("w")),
            tname("c"),
            tname("d"),
            Nil,
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after an indented `then` body: a continued statement, a body of two statements, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    if c then
         |        val q = 1
         |        q
         |      && w
         |    else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val v = a + b
         |    if (c) {
         |      val q = 1
         |      q
         |    } && w else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
          Term.If(
            tname("c"),
            tinfix(blk(Defn.Val(Nil, List(patvar("q")), None, int(1)), tname("q")), "&&", tname("w")),
            tname("d"),
            Nil,
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued `val`, the argument in a `then` body of the next `def`, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  val e = 1
         |    + q
         |  def h = if c then a && foo:
         |      z
         |    && w
         |  else d
         |""".stripMargin
    val layout =
      """|object A {
         |  val e = 1 + q
         |  def h = if (c) a && foo {
         |    z
         |  } && w else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(Nil, List(patvar("e")), None, tinfix(int(1), "+", tname("q"))),
        Defn.Def(
          Nil,
          tname("h"),
          Nil,
          None,
          Term.If(
            tname("c"),
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            tname("d"),
            Nil,
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued `catch` case on the line of the `try`, the argument in a `then` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val r = try a catch case _ => b
         |      + c
         |    if c then a && foo:
         |        z
         |      && w
         |    else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val r = try a catch {
         |      case _ =>
         |        b + c
         |    }
         |    if (c) a && foo {
         |      z
         |    } && w else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Defn.Val(
            Nil,
            List(patvar("r")),
            None,
            Term.Try(
              tname("a"),
              List(Case(Pat.Wildcard(), None, tinfix(tname("b"), "+", tname("c")))),
              None,
            ),
          ),
          tif(
            tname("c"),
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            tname("d"),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued `catch` case on the line of the `try`, the argument in a `try` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val r = try a catch case _ => b
         |      + c
         |    try a && foo:
         |        z
         |      && w
         |    finally d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val r = try a catch {
         |      case _ =>
         |        b + c
         |    }
         |    try a && foo {
         |      z
         |    } && w finally d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Defn.Val(
            Nil,
            List(patvar("r")),
            None,
            Term.Try(
              tname("a"),
              List(Case(Pat.Wildcard(), None, tinfix(tname("b"), "+", tname("c")))),
              None,
            ),
          ),
          Term.Try(
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            Nil,
            Some(tname("d")),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued statement in a `case` body in braces, the argument in a `then` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f = xs.map {
         |    case 1 =>
         |      val v = a
         |        + b
         |      if c then a && foo:
         |          z
         |        && w
         |      else d
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    case 1 =>
         |      val v = a + b
         |      if (c) a && foo {
         |        z
         |      } && w else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          Term.PartialFunction(List(Case(
            int(1),
            None,
            blk(
              Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
              tif(
                tname("c"),
                tinfix(
                  tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))),
                  "&&",
                  tname("w"),
                ),
                tname("d"),
              ),
            ),
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued statement in a `case` body in braces, the argument in a `try` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f = xs.map {
         |    case 1 =>
         |      val v = a
         |        + b
         |      try a && foo:
         |          z
         |        && w
         |      finally d
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    case 1 =>
         |      val v = a + b
         |      try a && foo {
         |        z
         |      } && w finally d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          Term.PartialFunction(List(Case(
            int(1),
            None,
            blk(
              Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
              Term.Try(
                tinfix(
                  tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))),
                  "&&",
                  tname("w"),
                ),
                Nil,
                Some(tname("d")),
              ),
            ),
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a `case` after a `case` on the line of the brace, the argument in a condition in its body, `&&` at the column of the cases") {
    val code =
      """|object A:
         |  def f = xs.map { case 0 => a
         |    case 1 => if x && foo:
         |        z
         |    && w
         |    then c else d
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    case 0 => a
         |    case 1 =>
         |      if (x && foo {
         |        z
         |      } && w) c else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect(tname("xs"), "map"),
          Term.PartialFunction(List(
            Case(int(0), None, tname("a")),
            Case(
              int(1),
              None,
              tif(
                tinfix(
                  tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tname("z")))),
                  "&&",
                  tname("w"),
                ),
                tname("c"),
                tname("d"),
              ),
            ),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a `catch` case after a case on the line of the brace, the argument in a condition in its body, `&&` at the column of the cases") {
    val code =
      """|object A:
         |  def f = try a catch { case _: E => b
         |    case _ => if x && foo:
         |        z
         |    && w
         |    then c else d
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case _: E =>
         |      b
         |    case _ =>
         |      if (x && foo {
         |        z
         |      } && w) c else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.Try(
          tname("a"),
          List(
            Case(Pat.Typed(Pat.Wildcard(), pname("E")), None, tname("b")),
            Case(
              Pat.Wildcard(),
              None,
              tif(
                tinfix(
                  tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tname("z")))),
                  "&&",
                  tname("w"),
                ),
                tname("c"),
                tname("d"),
              ),
            ),
          ),
          None,
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` on a deeper statement line, the condition on the next line at the column of the `if`, a continuation line, a colon argument, leading `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    a
         |      if
         |      x
         |        + y && foo:
         |            z
         |        && w
         |      then c else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    a
         |    if (x + y && foo {
         |      z
         |    } && w) c else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tname("a"),
          tif(
            tinfix(
              tinfix(tinfix(tname("x"), "+", tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a generator after one on the line of the brace of a `for`, the argument in a condition, `&&` at the column of the generator") {
    val code =
      """|object A:
         |  def f =
         |    for { a <- as
         |          y <- if x && foo:
         |              z
         |          && w
         |          then ys else zs
         |    } yield y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (a <- as; y <- if (x && foo {
         |    z && w
         |  }) ys else zs) yield y
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.ForYield(
          List(
            Enumerator.Generator(patvar("a"), tname("as")),
            Enumerator.Generator(
              patvar("y"),
              tif(
                tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
                tname("ys"),
                tname("zs"),
              ),
            ),
          ),
          tname("y"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with a deeper next line, the argument in a condition, `&&` at the column of that line") {
    val code =
      """|object A:
         |  self =>
         |    def g = 1
         |  def f = if c && foo:
         |        z
         |    && w
         |  then 1 else 2
         |""".stripMargin
    val layout =
      """|object A { self =>
         |  def g = 1
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with a deeper next line, a continuation line between the columns of the self type and that line, the argument in a condition in parentheses, `&&` deeper than that line") {
    val code =
      """|object A:
         |  self =>
         |      def g = 1
         |  def f = 1 +
         |   (if c && foo:
         |        z
         |       && w
         |   then 1 else 2)
         |""".stripMargin
    val layout =
      """|object A { self =>
         |  def g = 1
         |  def f = 1 + (if (c && foo {
         |    z && w
         |  }) 1 else 2)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tinfix(
            int(1),
            "+",
            tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            ),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with a deeper next line, a nested class, the argument in a condition in its body, `&&` between the columns of the class body and that line") {
    val code =
      """|object A:
         |  self =>
         |        def g = 1
         |  class B:
         |    def f = if c && foo:
         |          z
         |       && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|object A { self =>
         |  def g = 1
         |  class B {
         |    def f = if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Class(
          Nil,
          pname("B"),
          Nil,
          ctor,
          tpl(Defn.Def(
            Nil,
            tname("f"),
            Nil,
            None,
            tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            ),
          )),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with code after it on its line, the argument in a condition, `&&` deeper than the self type") {
    val code =
      """|object A:
         |  self => def g = 1
         |  def f = if c && foo:
         |        z
         |    && w
         |  then 1 else 2
         |""".stripMargin
    val layout =
      """|object A { self =>
         |  def g = 1
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with a next line at its column, the argument in a condition, `&&` deeper than the self type") {
    val code =
      """|object A:
         |  self =>
         |  def g = 1
         |  def f = if c && foo:
         |        z
         |    && w
         |  then 1 else 2
         |""".stripMargin
    val layout =
      """|object A { self =>
         |  def g = 1
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with a deeper next line in a `given` body in braces after `with`, the argument in a condition, `&&` deeper than that line") {
    val code =
      """|given A with {
         |  self =>
         |    def g = 1
         |  def f = if c && foo:
         |        z
         |      && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|given A with { self =>
         |  def g = 1
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Given(
      Nil,
      anon,
      Nil,
      tpl(
        List(init(pname("A"))),
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda in braces with a parameter on the line after the brace and a deeper next line, the argument in a condition on a shallower line, `&&` at the column of that line") {
    val code =
      """|object O:
         |  def h = xs.foreach {
         |    x =>
         |      g(x)
         |    if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |  }
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = xs.foreach {
         |    x => g(x)
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        tapply(
          tselect("xs", "foreach"),
          blk(
            tfunc(tparam("x"))(tapply(tname("g"), tname("x"))),
            tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            ),
          ),
        ),
      )),
    )))
    parseAndCheckTree[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `x: Int =>` on the line after a brace and a deeper next line, the argument in a condition on a shallower line, `&&` at the column of that line") {
    val code =
      """|object O:
         |  def h = xs.foreach {
         |    x: Int =>
         |      g(x)
         |    if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |  }
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = xs.foreach {
         |    x {
         |      Int => g(x)
         |    }
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        tapply(
          tselect("xs", "foreach"),
          blk(
            tapply(tname("x"), blk(tfunc(tparam("Int"))(tapply(tname("g"), tname("x"))))),
            tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            ),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda in braces with a parameter on the line after the brace, the argument in a condition in its body on the next line, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h = xs.foreach {
         |    x =>
         |      if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |  }
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = xs.foreach {
         |    x => if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        tapply(
          tselect("xs", "foreach"),
          blk(tfunc(tparam("x"))(tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon block on the first line of a body, the argument in a condition in that block, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h =
         |    boundary:
         |      if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = boundary {
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        tapply(
          tname("boundary"),
          blk(tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon block on the first line of an object body, the argument in a condition in a later definition, `&&` deeper than the definition") {
    val code =
      """|object O:
         |  locally:
         |    g
         |  def f = if c && foo:
         |        z
         |    && w
         |  then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  locally {
         |    g
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("locally"), blk(tname("g"))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon lambda on the first line of a body and a deeper next line, the argument in a condition on a shallower line, `&&` between the columns of the condition and that line") {
    val code =
      """|object O:
         |  def h =
         |    run: x =>
         |        g(x)
         |    if c && foo:
         |          z
         |       && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = {
         |    run {
         |      x => g(x)
         |    }
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        blk(
          tapply(tname("run"), blk(tfunc(tparam("x"))(tapply(tname("g"), tname("x"))))),
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon lambda with parameters in parentheses on the first line of a body and a deeper next line, the argument in a condition on a shallower line, `&&` between the columns of the condition and that line") {
    val code =
      """|object O:
         |  def h =
         |    run: (x, y) =>
         |        g(x)
         |    if c && foo:
         |          z
         |       && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = {
         |    run {
         |      (x, y) => g(x)
         |    }
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        blk(
          tapply(tname("run"), blk(tfunc(tparam("x"), tparam("y"))(tapply(tname("g"), tname("x"))))),
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon lambda on the first line of a body and a deeper next line, the argument in a condition in a colon block on a shallower line, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h =
         |    run: x =>
         |        g(x)
         |    locally:
         |      if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = {
         |    run {
         |      x => g(x)
         |    }
         |    locally {
         |      if (c && foo {
         |        z && w
         |      }) 1 else 2
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        blk(
          tapply(tname("run"), blk(tfunc(tparam("x"))(tapply(tname("g"), tname("x"))))),
          tapply(
            tname("locally"),
            blk(tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            )),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a context colon lambda on the first line of a body and a deeper next line, the argument in a condition on a shallower line, `&&` between the columns of the condition and that line") {
    val code =
      """|object O:
         |  def h =
         |    run: x ?=>
         |        g(x)
         |    if c && foo:
         |          z
         |       && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = {
         |    run {
         |      x ?=> g(x)
         |    }
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        blk(
          tapply(
            tname("run"),
            blk(Term.ContextFunction(List(tparam("x")), tapply(tname("g"), tname("x")))),
          ),
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon block and a blank line on the first lines of a body, the argument in a condition in that block, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h =
         |    boundary:
         |
         |      if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = boundary {
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        tapply(
          tname("boundary"),
          blk(tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of a class body in braces, a lambda on a deeper next line, the argument in a condition, `&&` between the columns of the name and the lambda") {
    val code =
      """|class A {
         |  self:
         |    B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|class A {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      ctor,
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: an ascription on the first line of a body, the argument in a condition on the next line, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h =
         |    x: Int
         |    if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = {
         |    x: Int
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        blk(
          Term.Ascribe(tname("x"), pname("Int")),
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon block with the colon on the next line, the argument in a condition in that block, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h =
         |    boundary
         |      :
         |      if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |""".stripMargin
    val error =
      """|<input>:4: error: expected fewer-braces method body
         |      :
         |      ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces, a type and an arrow at the column of the name, the argument in a condition, `&&` between the columns of the name and the next line") {
    val code =
      """|object O {
         |  self:
         |  B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces, a form feed, a type and an arrow on the next line deeper than the name, the argument in a condition, `&&` between the columns of the name and the next line") {
    val code =
      s"""|object O {
          |  self:
          |\f    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name, a colon, a form feed, a type and an arrow on the first line of an object body in braces, the argument in a condition, `&&` between the columns of the name and the next line") {
    val code =
      s"""|object O {
          |  self:\f  B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name, a colon, a carriage return, a type deeper than the name and an arrow on the first line of an object body in braces, the argument in a condition, `&&` between the columns of the name and the next line") {
    val code =
      s"""|object O {
          |  self:\r    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces, a blank line, a type and an arrow at column 0, the argument in a condition, `&&` between the columns of the name and the next line") {
    val code =
      """|object O {
         |  self:
         |
         |B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `\n\n` found
         |  self:
         |       ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces indented with tabs, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |\t\tself:
          |\t\t\t\tB =>
          |\t\t\t\t\t\tdef g = 1
          |\t\tdef f = if c && foo:
          |\t\t\t\t\t\t\t\tz
          |\t\t\t\t\t&& w
          |\t\tthen 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name, a colon, a carriage return and a line feed at the end of the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |  self:\r
          |    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces, a type and an arrow on the next line indented as the name and a tab, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |  self:
          |  \tB =>
          |  \t\t\tdef g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test {
    "leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces indented with a space and a tab, a type and an arrow on the next line indented with a space and two tabs, the argument in a condition, `&&` between the columns of the type and the line after it"
  } {
    val code =
      s"""|object O {
          | \tself:
          | \t\tB =>
          | \t\t\t\tdef g = 1
          | \tdef f = if c && foo:
          | \t\t\t\t\t\t\tz
          | \t\t\t\t&& w
          | \tthen 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test {
    "leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces indented with a space and a tab, a type and an arrow on the next line indented with a space, a tab and a space, the argument in a condition, `&&` between the columns of the type and the line after it"
  } {
    val code =
      s"""|object O {
          | \tself:
          | \t B =>
          | \t   def g = 1
          | \tdef f = if c && foo:
          | \t\t\t\t\t\t\tz
          | \t\t\t\t&& w
          | \tthen 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a form feed, a name and a colon on the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |\f  self:
          |    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a carriage return, a name and a colon on the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |\r  self:
          |    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: two spaces, a carriage return, a name and a colon on the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |  \r  self:
          |    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: two spaces, a form feed, a name and a colon on the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |  \f  self:
          |    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a comment, a name and a colon on the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      """|object O {
         |  /* c */ self:
         |    B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|object O {
         |  /* c */ self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1))))))
          .toBuilder.begComment(Seq("/* c */")).result(),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a multi-line comment after the brace of an object body, a name and a colon after it on its last line, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      """|object O { /* c
         |  */ self:
         |    B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|object O {
         |  /* c
         |  */ self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1))))))
          .toBuilder.begComment(Seq(
            """|/* c
               |  */""".stripMargin,
          )).result(),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test {
    "leading infix after a colon argument: a name on the first line of an object body in braces, a colon on a deeper next line, a type and an arrow on the line after it, between the columns of the name and the colon, the argument in a condition, `&&` between the columns of the type and the line after it"
  } {
    val code =
      """|object O {
         |  self
         |      :
         |    B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end, `try` and `catch` on a deeper next line, its case on a shallower line, the argument in a condition in the case, `&&` at the column of the `try`") {
    val code =
      """|def h: Int =
         |  throw
         |      try g catch
         |    case _ => if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|def h: Int = throw try g catch {
         |  case _ =>
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      Term.Throw(Term.Try(
        tname("g"),
        List(Case(
          Pat.Wildcard(),
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        )),
        None,
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end, `try` and an argument list on a deeper next line, the list closing with `catch` on a shallower line, its case at that column, the argument in a condition in the case, `&&` at the column of the `try`") {
    val code =
      """|def h: Int =
         |  throw
         |      try g(a,
         |    b) catch
         |    case _ => if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|def h: Int = throw try g(a, b) catch {
         |  case _ =>
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      Term.Throw(Term.Try(
        tapply(tname("g"), tname("a"), tname("b")),
        List(Case(
          Pat.Wildcard(),
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        )),
        None,
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end, `while` and a condition on a deeper next line, the condition continued with `do` on a shallower line, the argument in a condition at that column, `&&` at the column of the `while`") {
    val code =
      """|def h: Unit =
         |  throw
         |      while a &&
         |    b do
         |    if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|def h: Unit = throw while (a && b) if (c && foo {
         |  z && w
         |}) 1 else 2
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Unit")),
      Term.Throw(Term.While(
        tinfix(tname("a"), "&&", tname("b")),
        tif(
          tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          int(1),
          int(2),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end in braces, an argument list on a deeper next line, the list closing with `match` and a case in braces on a shallower line, the argument in a condition in the case, `&&` between the columns of that line and the list") {
    val code =
      """|def h: Int = {
         |  throw
         |      g(a,
         |    b) match { case _ => if c && foo:
         |          z
         |     && w
         |    then 1 else 2
         |    }
         |}
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  throw g(a, b) match {
         |    case _ =>
         |      if (c && foo {
         |        z && w
         |      }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Term.Throw(Term.Match(
        tapply(tname("g"), tname("a"), tname("b")),
        List(Case(
          Pat.Wildcard(),
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        )),
      ))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end in braces, `if` and a condition on a deeper next line, `then` and the argument in a condition on a shallower line, `&&` between the columns of `then` and the `if`") {
    val code =
      """|def h: Int = {
         |  throw
         |      if a
         |  then if c && foo:
         |         z
         |    && w
         |       then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  throw if (a) if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Term.Throw(tif(
        tname("a"),
        tif(
          tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          int(1),
          int(2),
        ),
      ))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test {
    "leading infix after a colon argument: a name and a colon at the end of the first line of a class body in braces, a multi-line comment on the next line that ends on the line of a type and an arrow, the argument in a condition, `&&` between the columns of the type and the line after it, with LF and with CR LF"
  } {
    val code =
      """|class O {
         |  self:
         |   /*
         |*/ B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |      && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|class O { self:
         |  /*
         |*/ B =>
         |  def g = 1
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val layoutCRLF =
      """|class O {
         |  self {
         |    /*
         |*/ B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("O"),
      Nil,
      EmptyCtor(),
      tpl(
        self(tname("self"), pname("B").toBuilder.begComment(Seq("/*\n*/")).result()),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    val parsed = source(code)
    assertStruct(parsed)(tree.structure)
    assertNoDiff(parsed.reprint, layout)
    assertNoDiff(source(code.replace("\n", "\r\n")).reprint.replace("\r\n", "\n"), layoutCRLF)
  }

  test("leading infix after a colon argument: a self type on the line of a trait's brace, the argument in a condition in a method body, `&&` between the columns of the condition and the argument") {
    val code =
      """|trait T { self =>
         |  def f =
         |    if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|trait T { self =>
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Trait(
      Nil,
      pname("T"),
      Nil,
      EmptyCtor(),
      tpl(
        self(tname("self")),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda in braces with a parameter on the line of the brace, the argument in a condition in its body, `&&` between the columns of the condition and the argument") {
    val code =
      """|def h = xs.foreach { x =>
         |    val f = if c && foo:
         |        z
         |     && w
         |     then 1 else 2
         |    f
         |  }
         |""".stripMargin
    val layout =
      """|def h = xs.foreach { x =>
         |  val f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |  f
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam("x"))(blk(
          Defn.Val(
            Nil,
            List(patvar("f")),
            None,
            tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            ),
          ),
          tname("f"),
        ))),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a line that starts with `||` and the argument in a condition in parentheses, `&&` between the columns of that line and the argument") {
    val code =
      """|def h: Int =
         |  val v =
         |      a
         |    || (if c && foo:
         |              z
         |       && w
         |    then 1 else 2)
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  val v = a || (if (c && foo {
         |    z && w
         |  }) 1 else 2)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Defn.Val(
        Nil,
        List(patvar("v")),
        None,
        tinfix(
          tname("a"),
          "||",
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end, `try` and an argument list on a deeper next line, the list closing with `catch` on a line shallower than the `throw`, the argument in a condition in its case, `&&` between the columns of the case and the `try`") {
    val code =
      """|def h: Int =
         |  throw
         |      try g(a,
         |  b) catch
         |    case _ => if c && foo:
         |                    z
         |     && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|def h: Int = throw try g(a, b) catch {
         |  case _ =>
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      Term.Throw(Term.Try(
        tapply(tname("g"), tname("a"), tname("b")),
        List(Case(
          Pat.Wildcard(),
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        )),
        None,
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end in braces, `if` and a condition on a deeper next line, the condition continued with `.b` and `then` on a shallower line, the argument in a condition after `then`, `&&` between the columns of that line and the `if`") {
    val code =
      """|def h: Int = {
         |  throw
         |      if a
         |    .b then if c && foo:
         |                  z
         |     && w
         |    then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  throw if (a.b) if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Term.Throw(Term.If(
        tselect("a", "b"),
        tif(
          tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          int(1),
          int(2),
        ),
        Lit.Unit(),
      ))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end in braces, `for` and an enumerator on a deeper next line, a second enumerator and `do` on a shallower line, the argument in a condition after `do`, `&&` between the columns of that line and the `for`") {
    val code =
      """|def h: Int = {
         |  throw
         |      for x <- xs
         |    y <- ys do if c && foo:
         |                     z
         |     && w
         |    then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  throw for (x <- xs; y <- ys) if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Term.Throw(Term.For(
        List(
          Enumerator.Generator(patvar("x"), tname("xs")),
          Enumerator.Generator(patvar("y"), tname("ys")),
        ),
        tif(
          tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          int(1),
          int(2),
        ),
      ))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end in braces, `for` and an enumerator on a deeper next line, a guard on a shallower line, `yield` and the argument in a condition at that column, `&&` between the columns of that line and the `for`") {
    val code =
      """|def h: Int = {
         |  throw
         |      for x <- xs
         |    if p
         |    yield if c && foo:
         |                z
         |     && w
         |    then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  throw for (x <- xs; if p) yield if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Term.Throw(Term.ForYield(
        List(Enumerator.Generator(patvar("x"), tname("xs")), Enumerator.Guard(tname("p"))),
        tif(
          tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          int(1),
          int(2),
        ),
      ))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda parameter at the end of the condition's line, its body on a deeper next line, `&&` four columns deeper than the `if`") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists: x =>
         |          x > 0
         |        && ys.isEmpty
         |    then a else b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.exists {
         |    x => x > 0 && ys.isEmpty
         |  }) a else b
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("x"))(
              tinfix(tinfix(tname("x"), ">", int(0)), "&&", tselect("ys", "isEmpty")),
            )),
          ),
          tname("a"),
          tname("b"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda parameter at the end of the condition's line, its body on a deeper next line, `&&` two columns deeper than the `if`") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists: x =>
         |          x > 0
         |      && ys.isEmpty
         |    then a else b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.exists {
         |    x => x > 0 && ys.isEmpty
         |  }) a else b
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("x"))(
              tinfix(tinfix(tname("x"), ">", int(0)), "&&", tselect("ys", "isEmpty")),
            )),
          ),
          tname("a"),
          tname("b"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a placeholder lambda on a deeper next line, `&&` four columns deeper than the `if`") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists:
         |          _ > 0
         |        && ys.isEmpty
         |    then a else b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.exists {
         |    _ > 0 && ys.isEmpty
         |  }) a else b
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tapply(
            tselect("xs", "exists"),
            blk(Term.AnonymousFunction(
              tinfix(tinfix(Term.Placeholder(), ">", int(0)), "&&", tselect("ys", "isEmpty")),
            )),
          ),
          tname("a"),
          tname("b"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a `for` with its first generator on its line, a second generator on the next line, the argument in a condition after its arrow, `&&` deeper than that line") {
    val code =
      """|object A:
         |  def f =
         |    for x <- xs
         |        y <- if c && foo:
         |            z
         |          && w
         |        then a else b
         |    yield y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- xs; y <- if (c && foo {
         |    z
         |  } && w) a else b) yield y
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.ForYield(
          List(
            Enumerator.Generator(patvar("x"), tname("xs")),
            Enumerator.Generator(
              patvar("y"),
              tif(
                tinfix(
                  tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tname("z")))),
                  "&&",
                  tname("w"),
                ),
                tname("a"),
                tname("b"),
              ),
            ),
          ),
          tname("y"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end, `if` and two `} else if` in braces on a deeper next line, the argument in the last condition, `&&` deeper than the `if`") {
    val code =
      """|def h: Int =
         |  throw
         |      if (a) {
         |        x
         |      } else if (b) {
         |        y
         |      } else if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |""".stripMargin
    val layout =
      """|def h: Int = throw if (a) {
         |  x
         |} else if (b) {
         |  y
         |} else if (c && foo {
         |  z && w
         |}) 1 else 2
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      Term.Throw(tif(
        tname("a"),
        blk(tname("x")),
        tif(
          tname("b"),
          blk(tname("y")),
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, shallower leading `&&` on the next line, a blank line, leading `||` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |      && w
         |
         |      || v
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z && w
         |  } || v) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(
                tapply(tname("x"), tname("y")),
                "&&",
                tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
              ),
              "||",
              tname("v"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line with a comment, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z // c
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |Indentation.Outdent [53..53)
         |Ident(&&) [60..62)
         |Ident(w) [63..64)
         |KwThen [69..73)
         |Ident(c) [74..75)
         |KwElse [76..80)
         |Ident(d) [81..82)
         |LF [82..83)
         |KwDef [85..88)
         |Ident(g) [89..90)
         |Equals [91..92)
         |Constant.Int(1) [93..94)
         |Indentation.Outdent [94..94)
         |EOF [95..95)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z // c
         |
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(
                tapply(tname("x"), tname("y")),
                "&&",
                tapply(tname("foo"), blk(tnameComments("z")()("// c"))),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, two blank lines, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |Indentation.Outdent [49..49)
         |Ident(&&) [56..58)
         |Ident(w) [59..60)
         |KwThen [65..69)
         |Ident(c) [70..71)
         |KwElse [72..76)
         |Ident(d) [77..78)
         |LF [78..79)
         |KwDef [81..84)
         |Ident(g) [85..86)
         |Equals [87..88)
         |Constant.Int(1) [89..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, a comment line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |
         |      // c
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |Indentation.Outdent [48..48)
         |Ident(&&) [67..69)
         |Ident(w) [70..71)
         |KwThen [76..80)
         |Ident(c) [81..82)
         |KwElse [83..87)
         |Ident(d) [88..89)
         |LF [89..90)
         |KwDef [92..95)
         |Ident(g) [96..97)
         |Equals [98..99)
         |Constant.Int(1) [100..101)
         |Indentation.Outdent [101..101)
         |EOF [102..102)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z
         |  }
         |    // c
         |
         |    && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              Term.Name.newBuilder("&&").begComment(detachedComments("// c")).result(),
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a colon argument, the argument on the next line, two blank lines, leading `&&` at the column of the `if`, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if try foo:
         |        z
         |
         |
         |    && w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |Ident(foo) [31..34)
         |Colon [34..35)
         |Indentation.Indent [35..35)
         |Ident(z) [44..45)
         |Indentation.Outdent [47..47)
         |Ident(&&) [52..54)
         |Ident(w) [55..56)
         |KwThen [61..65)
         |Ident(c) [66..67)
         |KwElse [68..72)
         |Ident(d) [73..74)
         |LF [74..75)
         |Ident(g) [79..80)
         |Indentation.Outdent [80..80)
         |Indentation.Outdent [80..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (try foo {
         |      z
         |    } && w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with `try` and a colon argument in its `then` body, the argument on the next line, a blank line, leading `&&` on the next line, `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if a then try foo:
         |          z
         |
         |        && w
         |      else d)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwIf [26..28)
         |Ident(a) [29..30)
         |KwThen [31..35)
         |KwTry [36..39)
         |Ident(foo) [40..43)
         |Colon [43..44)
         |Indentation.Indent [44..44)
         |Ident(z) [55..56)
         |Indentation.Outdent [57..57)
         |InfixLF [57..58)
         |Ident(&&) [66..68)
         |Ident(w) [69..70)
         |KwElse [77..81)
         |Ident(d) [82..83)
         |RightParen [83..84)
         |LF [84..85)
         |Ident(h) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) try foo {
         |      z
         |    } && w else d)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tname("g"),
            tif(
              tname("a"),
              Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
              tname("d"),
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with an `if` whose `then` body holds `try` and a colon argument, the argument on the next line, a blank line, leading `&&` on the next line, `else` and the outer `then` on later lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if a then try foo:
         |          z
         |
         |        && w
         |      else d
         |    then c else e
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |KwThen [32..36)
         |KwTry [37..40)
         |Ident(foo) [41..44)
         |Colon [44..45)
         |Indentation.Indent [45..45)
         |Ident(z) [56..57)
         |Indentation.Outdent [58..58)
         |InfixLF [58..59)
         |Ident(&&) [67..69)
         |Ident(w) [70..71)
         |KwElse [78..82)
         |Ident(d) [83..84)
         |KwThen [89..93)
         |Ident(c) [94..95)
         |KwElse [96..100)
         |Ident(e) [101..102)
         |LF [102..103)
         |Ident(g) [107..108)
         |Indentation.Outdent [108..108)
         |Indentation.Outdent [108..108)
         |EOF [109..109)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) try foo {
         |      z
         |    } && w else d) c else e
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tname("a"),
              Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
              tname("d"),
            ),
            tname("c"),
            tname("e"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with an `if` whose `then` body holds `try`, a deeper line in parentheses, `else` and the outer `then` on later lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if a then try b
         |          (c)
         |      else d
         |    then e else k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |KwThen [32..36)
         |KwTry [37..40)
         |Ident(b) [41..42)
         |LeftParen [53..54)
         |Ident(c) [54..55)
         |RightParen [55..56)
         |KwElse [63..67)
         |Ident(d) [68..69)
         |KwThen [74..78)
         |Ident(e) [79..80)
         |KwElse [81..85)
         |Ident(k) [86..87)
         |LF [87..88)
         |Ident(g) [92..93)
         |Indentation.Outdent [93..93)
         |Indentation.Outdent [93..93)
         |EOF [94..94)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) try b(c) else d) e else k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tname("a"), Term.Try(tapply(tname("b"), tname("c")), None, None), tname("d")),
            tname("e"),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with `try` in its `then` body, a deeper line in parentheses, `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if a then try b
         |          (c)
         |      else d)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwIf [26..28)
         |Ident(a) [29..30)
         |KwThen [31..35)
         |KwTry [36..39)
         |Ident(b) [40..41)
         |LeftParen [52..53)
         |Ident(c) [53..54)
         |RightParen [54..55)
         |KwElse [62..66)
         |Ident(d) [67..68)
         |RightParen [68..69)
         |LF [69..70)
         |Ident(h) [74..75)
         |Indentation.Outdent [75..75)
         |Indentation.Outdent [75..75)
         |EOF [76..76)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) try b(c) else d)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tname("g"),
            tif(tname("a"), Term.Try(tapply(tname("b"), tname("c")), None, None), tname("d")),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a `catch` case on its line, a colon argument in the case body, the argument on the next line, a blank line, leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if try a catch case _: E => foo:
         |            z
         |
         |      && w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |Ident(a) [31..32)
         |KwCatch [33..38)
         |Indentation.Indent [39..39)
         |KwCase [39..43)
         |Underscore [44..45)
         |Colon [45..46)
         |Ident(E) [47..48)
         |RightArrow [49..51)
         |Ident(foo) [52..55)
         |Colon [55..56)
         |Indentation.Indent [56..56)
         |Ident(z) [69..70)
         |Indentation.Outdent [71..71)
         |LFLF [71..72)
         |Ident(&&) [78..80)
         |Ident(w) [81..82)
         |Indentation.Outdent [82..82)
         |KwThen [87..91)
         |Ident(c) [92..93)
         |KwElse [94..98)
         |Ident(d) [99..100)
         |LF [100..101)
         |Ident(g) [105..106)
         |Indentation.Outdent [106..106)
         |Indentation.Outdent [106..106)
         |EOF [107..107)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (try a catch {
         |      case _: E =>
         |        foo {
         |          z
         |        }
         |        &&w
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Try(
              tname("a"),
              List(Case(
                Pat.Typed(Pat.Wildcard(), pname("E")),
                None,
                blk(tapply(tname("foo"), blk(tname("z"))), tpostfix(tname("&&"), "w")),
              )),
              None,
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`try` within parentheses with a `catch` case on its line, a colon argument in the case body, the argument on the next line, a blank line, leading `&&` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(try a catch case _: E => foo:
         |          z
         |
         |        && w)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwTry [26..29)
         |Ident(a) [30..31)
         |KwCatch [32..37)
         |Indentation.Indent [38..38)
         |KwCase [38..42)
         |Underscore [43..44)
         |Colon [44..45)
         |Ident(E) [46..47)
         |RightArrow [48..50)
         |Ident(foo) [51..54)
         |Colon [54..55)
         |Indentation.Indent [55..55)
         |Ident(z) [66..67)
         |Indentation.Outdent [68..68)
         |LFLF [68..69)
         |Ident(&&) [77..79)
         |Ident(w) [80..81)
         |Indentation.Outdent [80..80)
         |RightParen [81..82)
         |LF [82..83)
         |Ident(h) [87..88)
         |Indentation.Outdent [88..88)
         |Indentation.Outdent [88..88)
         |EOF [89..89)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(try a catch {
         |      case _: E =>
         |        foo {
         |          z
         |        }
         |        &&w
         |    })
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tname("g"),
            Term.Try(
              tname("a"),
              List(Case(
                Pat.Typed(Pat.Wildcard(), pname("E")),
                None,
                blk(tapply(tname("foo"), blk(tname("z"))), tpostfix(tname("&&"), "w")),
              )),
              None,
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("case guard: `if` at the end of a line, a colon argument on the next line, the argument on the line after, an expression at the column of the colon argument, `=>` on the next line") {
    val code =
      """|object A:
         |  def f = x match
         |    case y if
         |        foo:
         |          z
         |        w
         |      => c
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Ident(y) [37..38)
         |KwIf [39..41)
         |Ident(foo) [50..53)
         |Colon [53..54)
         |Indentation.Indent [54..54)
         |Ident(z) [65..66)
         |Indentation.Outdent [66..66)
         |Ident(w) [75..76)
         |RightArrow [83..85)
         |Ident(c) [86..87)
         |Indentation.Outdent [87..87)
         |LF [87..88)
         |KwDef [90..93)
         |Ident(g) [94..95)
         |Equals [96..97)
         |Constant.Int(1) [98..99)
         |Indentation.Outdent [99..99)
         |EOF [100..100)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case y if foo {
         |      z
         |    } w => c
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tmatch(
            tname("x"),
            Case(patvar("y"), Some(tpostfix(tapply(tname("foo"), blk(tname("z"))), "w")), tname("c")),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument, the argument on the next line ending in `;`, `+` and an expression between the columns of the `if` and the argument, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |        z;
         |      + w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(z) [45..46)
         |Semicolon [46..47)
         |InfixLF [47..48)
         |Ident(+) [54..55)
         |Ident(w) [56..57)
         |Indentation.Outdent [57..57)
         |KwThen [62..66)
         |Ident(c) [67..68)
         |KwElse [69..73)
         |Ident(d) [74..75)
         |LF [75..76)
         |Ident(g) [80..81)
         |Indentation.Outdent [81..81)
         |Indentation.Outdent [81..81)
         |EOF [82..82)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a && foo {
         |      z
         |      +w
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z"), unary("+", tname("w"))))),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `match` with cases on the next lines, leading `&&` between the columns of the `if` and the cases, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if x match
         |        case 1 => true
         |        case _ => false
         |      && w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(x) [27..28)
         |KwMatch [29..34)
         |Indentation.Indent [34..34)
         |KwCase [43..47)
         |Constant.Int(1) [48..49)
         |RightArrow [50..52)
         |KwTrue [53..57)
         |LF [57..58)
         |KwCase [66..70)
         |Underscore [71..72)
         |RightArrow [73..75)
         |KwFalse [76..81)
         |Indentation.Outdent [81..81)
         |InfixLF [81..82)
         |Ident(&&) [88..90)
         |Ident(w) [91..92)
         |KwThen [97..101)
         |Ident(c) [102..103)
         |KwElse [104..108)
         |Ident(d) [109..110)
         |LF [110..111)
         |Ident(g) [115..116)
         |Indentation.Outdent [116..116)
         |Indentation.Outdent [116..116)
         |EOF [117..117)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ((x match {
         |      case 1 => true
         |      case _ => false
         |    }) && w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tmatch(tname("x"), Case(int(1), None, bool(true)), Case(patwildcard, None, bool(false))),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `new` with a template body after a colon, the body on the next line, leading `==` between the columns of the `if` and the body, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if new B:
         |         def z = 1
         |       == w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwNew [27..30)
         |Ident(B) [31..32)
         |Colon [32..33)
         |Indentation.Indent [33..33)
         |KwDef [43..46)
         |Ident(z) [47..48)
         |Equals [49..50)
         |Constant.Int(1) [51..52)
         |Indentation.Outdent [52..52)
         |InfixLF [52..53)
         |Ident(==) [60..62)
         |Ident(w) [63..64)
         |KwThen [69..73)
         |Ident(c) [74..75)
         |KwElse [76..80)
         |Ident(d) [81..82)
         |LF [82..83)
         |Ident(g) [87..88)
         |Indentation.Outdent [88..88)
         |Indentation.Outdent [88..88)
         |EOF [89..89)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (new B { def z = 1 } == w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              Term.NewAnonymous(
                tpl(List(init(pname("B"))), List(Defn.Def(Nil, tname("z"), Nil, None, int(1)))),
              ),
              "==",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`then` and `do`: `if` without parentheses at a line end, `then` on the next line, in braces, next statement") {
    val code =
      """|def f = {
         |  if a
         |  then b
         |  d
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |LeftBrace [8..9)
         |KwIf [12..14)
         |Ident(a) [15..16)
         |KwThen [19..23)
         |Ident(b) [24..25)
         |LF [25..26)
         |Ident(d) [28..29)
         |LF [29..30)
         |RightBrace [30..31)
         |EOF [32..32)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `if` without parentheses at a line end, `then` on the next line, in an indented block, next statement") {
    val code =
      """|def f =
         |  if a
         |  then b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(a) [13..14)
         |KwThen [17..21)
         |Ident(b) [22..23)
         |LF [23..24)
         |Ident(d) [26..27)
         |Indentation.Outdent [27..27)
         |EOF [28..28)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `if` without parentheses at a line end, `then` on the next line, after a statement in an indented block, next statement") {
    val code =
      """|def f =
         |  g
         |  if a
         |  then b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(g) [10..11)
         |LF [11..12)
         |KwIf [14..16)
         |Ident(a) [17..18)
         |KwThen [21..25)
         |Ident(b) [26..27)
         |LF [27..28)
         |Ident(d) [30..31)
         |Indentation.Outdent [31..31)
         |EOF [32..32)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  g
         |  if (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tname("g"), tif(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `if` with parentheses at a line end, `then` on the next line, in braces, next statement") {
    val code =
      """|def f = {
         |  if (a)
         |  then b
         |  d
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |LeftBrace [8..9)
         |KwIf [12..14)
         |LeftParen [15..16)
         |Ident(a) [16..17)
         |RightParen [17..18)
         |KwThen [21..25)
         |Ident(b) [26..27)
         |LF [27..28)
         |Ident(d) [30..31)
         |LF [31..32)
         |RightBrace [32..33)
         |EOF [34..34)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `if` with parentheses at a line end, `then` on the next line, in an indented block, next statement") {
    val code =
      """|def f =
         |  if (a)
         |  then b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |KwThen [19..23)
         |Ident(b) [24..25)
         |LF [25..26)
         |Ident(d) [28..29)
         |Indentation.Outdent [29..29)
         |EOF [30..30)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `if` with parentheses at a line end, `then` on the next line, after a statement in an indented block, next statement") {
    val code =
      """|def f =
         |  g
         |  if (a)
         |  then b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(g) [10..11)
         |LF [11..12)
         |KwIf [14..16)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |KwThen [23..27)
         |Ident(b) [28..29)
         |LF [29..30)
         |Ident(d) [32..33)
         |Indentation.Outdent [33..33)
         |EOF [34..34)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  g
         |  if (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tname("g"), tif(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `while` with parentheses at a line end, `do` on the next line, in braces, next statement") {
    val code =
      """|def f = {
         |  while (a)
         |  do b
         |  d
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |LeftBrace [8..9)
         |KwWhile [12..17)
         |LeftParen [18..19)
         |Ident(a) [19..20)
         |RightParen [20..21)
         |LF [21..22)
         |KwDo [24..26)
         |Ident(b) [27..28)
         |LF [28..29)
         |Ident(d) [31..32)
         |LF [32..33)
         |RightBrace [33..34)
         |EOF [35..35)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.While(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `while` with parentheses at a line end, `do` on the next line, in an indented block, next statement") {
    val code =
      """|def f =
         |  while (a)
         |  do b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |LeftParen [16..17)
         |Ident(a) [17..18)
         |RightParen [18..19)
         |LF [19..20)
         |KwDo [22..24)
         |Ident(b) [25..26)
         |LF [26..27)
         |Ident(d) [29..30)
         |Indentation.Outdent [30..30)
         |EOF [31..31)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.While(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `while` with parentheses at a line end, `do` on the next line, after a statement in an indented block, next statement") {
    val code =
      """|def f =
         |  g
         |  while (a)
         |  do b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(g) [10..11)
         |LF [11..12)
         |KwWhile [14..19)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |LF [23..24)
         |KwDo [26..28)
         |Ident(b) [29..30)
         |LF [30..31)
         |Ident(d) [33..34)
         |Indentation.Outdent [34..34)
         |EOF [35..35)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  g
         |  while (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tname("g"), Term.While(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `then` after the parentheses of `if`, in braces, next statement") {
    val code =
      """|def f = {
         |  if (a) then b
         |  d
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |LeftBrace [8..9)
         |KwIf [12..14)
         |LeftParen [15..16)
         |Ident(a) [16..17)
         |RightParen [17..18)
         |KwThen [19..23)
         |Ident(b) [24..25)
         |LF [25..26)
         |Ident(d) [28..29)
         |LF [29..30)
         |RightBrace [30..31)
         |EOF [32..32)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `then` after the parentheses of `if`, in an indented block, next statement") {
    val code =
      """|def f =
         |  if (a) then b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |KwThen [17..21)
         |Ident(b) [22..23)
         |LF [23..24)
         |Ident(d) [26..27)
         |Indentation.Outdent [27..27)
         |EOF [28..28)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tif(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `then` after the parentheses of `if`, after a statement in an indented block, next statement") {
    val code =
      """|def f =
         |  g
         |  if (a) then b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(g) [10..11)
         |LF [11..12)
         |KwIf [14..16)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |KwThen [21..25)
         |Ident(b) [26..27)
         |LF [27..28)
         |Ident(d) [30..31)
         |Indentation.Outdent [31..31)
         |EOF [32..32)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  g
         |  if (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tname("g"), tif(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `do` after the parentheses of `while`, in braces, next statement") {
    val code =
      """|def f = {
         |  while (a) do b
         |  d
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |LeftBrace [8..9)
         |KwWhile [12..17)
         |LeftParen [18..19)
         |Ident(a) [19..20)
         |RightParen [20..21)
         |KwDo [22..24)
         |Ident(b) [25..26)
         |LF [26..27)
         |Ident(d) [29..30)
         |LF [30..31)
         |RightBrace [31..32)
         |EOF [33..33)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.While(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "`then` and `do`: `do` after the parentheses of `while`, in an indented block, next statement",
  ) {
    val code =
      """|def f =
         |  while (a) do b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |LeftParen [16..17)
         |Ident(a) [17..18)
         |RightParen [18..19)
         |KwDo [20..22)
         |Ident(b) [23..24)
         |LF [24..25)
         |Ident(d) [27..28)
         |Indentation.Outdent [28..28)
         |EOF [29..29)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.While(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `do` after the parentheses of `while`, after a statement in an indented block, next statement") {
    val code =
      """|def f =
         |  g
         |  while (a) do b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(g) [10..11)
         |LF [11..12)
         |KwWhile [14..19)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |KwDo [24..26)
         |Ident(b) [27..28)
         |LF [28..29)
         |Ident(d) [31..32)
         |Indentation.Outdent [32..32)
         |EOF [33..33)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  g
         |  while (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tname("g"), Term.While(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: an operator after the parentheses of `if`, `then` on the next line, in an indented block, next statement") {
    val code =
      """|def f =
         |  if (a) && b
         |  then c
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Ident(&&) [17..19)
         |Ident(b) [20..21)
         |KwThen [24..28)
         |Ident(c) [29..30)
         |LF [30..31)
         |Ident(d) [33..34)
         |Indentation.Outdent [34..34)
         |EOF [35..35)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tif(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: an operator after the parentheses of `if`, `then` on the next line, after a statement in an indented block, next statement") {
    val code =
      """|def f =
         |  g
         |  if (a) && b
         |  then c
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(g) [10..11)
         |LF [11..12)
         |KwIf [14..16)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |Ident(&&) [21..23)
         |Ident(b) [24..25)
         |KwThen [28..32)
         |Ident(c) [33..34)
         |LF [34..35)
         |Ident(d) [37..38)
         |Indentation.Outdent [38..38)
         |EOF [39..39)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  g
         |  if (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tname("g"), tif(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: an operator after the parentheses of `if`, `then` on the same line, in an indented block, next statement") {
    val code =
      """|def f =
         |  if (a) && b then c
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |LeftParen [13..14)
         |Ident(a) [14..15)
         |RightParen [15..16)
         |Ident(&&) [17..19)
         |Ident(b) [20..21)
         |KwThen [22..26)
         |Ident(c) [27..28)
         |LF [28..29)
         |Ident(d) [31..32)
         |Indentation.Outdent [32..32)
         |EOF [33..33)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tif(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: an operator after the parentheses of `if`, `then` on the same line, after a statement in an indented block, next statement") {
    val code =
      """|def f =
         |  g
         |  if (a) && b then c
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(g) [10..11)
         |LF [11..12)
         |KwIf [14..16)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |Ident(&&) [21..23)
         |Ident(b) [24..25)
         |KwThen [26..30)
         |Ident(c) [31..32)
         |LF [32..33)
         |Ident(d) [35..36)
         |Indentation.Outdent [36..36)
         |EOF [37..37)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  g
         |  if (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tname("g"), tif(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: an operator after the parentheses of `while`, `do` on the next line, in braces, next statement") {
    val code =
      """|def f = {
         |  while (a) && b
         |  do c
         |  d
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |LeftBrace [8..9)
         |KwWhile [12..17)
         |LeftParen [18..19)
         |Ident(a) [19..20)
         |RightParen [20..21)
         |Ident(&&) [22..24)
         |Ident(b) [25..26)
         |KwDo [29..31)
         |Ident(c) [32..33)
         |LF [33..34)
         |Ident(d) [36..37)
         |LF [37..38)
         |RightBrace [38..39)
         |EOF [40..40)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: an operator after the parentheses of `while`, `do` on the next line, in an indented block, next statement") {
    val code =
      """|def f =
         |  while (a) && b
         |  do c
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |LeftParen [16..17)
         |Ident(a) [17..18)
         |RightParen [18..19)
         |Ident(&&) [20..22)
         |Ident(b) [23..24)
         |KwDo [27..29)
         |Ident(c) [30..31)
         |LF [31..32)
         |Ident(d) [34..35)
         |Indentation.Outdent [35..35)
         |EOF [36..36)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: an operator after the parentheses of `while`, `do` on the next line, after a statement in an indented block, next statement") {
    val code =
      """|def f =
         |  g
         |  while (a) && b
         |  do c
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(g) [10..11)
         |LF [11..12)
         |KwWhile [14..19)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |Ident(&&) [24..26)
         |Ident(b) [27..28)
         |KwDo [31..33)
         |Ident(c) [34..35)
         |LF [35..36)
         |Ident(d) [38..39)
         |Indentation.Outdent [39..39)
         |EOF [40..40)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  g
         |  while (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tname("g"), Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: an operator after the parentheses of `while`, `do` on the same line, in braces, next statement") {
    val code =
      """|def f = {
         |  while (a) && b do c
         |  d
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |LeftBrace [8..9)
         |KwWhile [12..17)
         |LeftParen [18..19)
         |Ident(a) [19..20)
         |RightParen [20..21)
         |Ident(&&) [22..24)
         |Ident(b) [25..26)
         |KwDo [27..29)
         |Ident(c) [30..31)
         |LF [31..32)
         |Ident(d) [34..35)
         |LF [35..36)
         |RightBrace [36..37)
         |EOF [38..38)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: an operator after the parentheses of `while`, `do` on the same line, in an indented block, next statement") {
    val code =
      """|def f =
         |  while (a) && b do c
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |LeftParen [16..17)
         |Ident(a) [17..18)
         |RightParen [18..19)
         |Ident(&&) [20..22)
         |Ident(b) [23..24)
         |KwDo [25..27)
         |Ident(c) [28..29)
         |LF [29..30)
         |Ident(d) [32..33)
         |Indentation.Outdent [33..33)
         |EOF [34..34)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: an operator after the parentheses of `while`, `do` on the same line, after a statement in an indented block, next statement") {
    val code =
      """|def f =
         |  g
         |  while (a) && b do c
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(g) [10..11)
         |LF [11..12)
         |KwWhile [14..19)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |Ident(&&) [24..26)
         |Ident(b) [27..28)
         |KwDo [29..31)
         |Ident(c) [32..33)
         |LF [33..34)
         |Ident(d) [36..37)
         |Indentation.Outdent [37..37)
         |EOF [38..38)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  g
         |  while (a && b) c
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tname("g"), Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `while` without parentheses at a line end, `do` on the next line, in braces, next statement") {
    val code =
      """|def f = {
         |  while a
         |  do b
         |  d
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |LeftBrace [8..9)
         |KwWhile [12..17)
         |Ident(a) [18..19)
         |KwDo [22..24)
         |Ident(b) [25..26)
         |LF [26..27)
         |Ident(d) [29..30)
         |LF [30..31)
         |RightBrace [31..32)
         |EOF [33..33)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.While(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `while` without parentheses at a line end, `do` on the next line, in an indented block, next statement") {
    val code =
      """|def f =
         |  while a
         |  do b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Ident(a) [16..17)
         |KwDo [20..22)
         |Ident(b) [23..24)
         |LF [24..25)
         |Ident(d) [27..28)
         |Indentation.Outdent [28..28)
         |EOF [29..29)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.While(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("`then` and `do`: `while` without parentheses at a line end, `do` on the next line, after a statement in an indented block, next statement") {
    val code =
      """|def f =
         |  g
         |  while a
         |  do b
         |  d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(g) [10..11)
         |LF [11..12)
         |KwWhile [14..19)
         |Ident(a) [20..21)
         |KwDo [24..26)
         |Ident(b) [27..28)
         |LF [28..29)
         |Ident(d) [31..32)
         |Indentation.Outdent [32..32)
         |EOF [33..33)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  g
         |  while (a) b
         |  d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tname("g"), Term.While(tname("a"), tname("b")), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

}
