package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.internal.parsers.{LazyTokenIterator, ScannerTokens}
import scala.meta.tests.TestHelpers
import scala.meta.tokenizers.TokenizerOptions

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
    runTestError[Stat](code, "`then` expected but `identifier` found")
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
    val layout =
      """|def f = if (a && b match {
         |  case _ => true
         |}) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tmatch(tinfix(tname("a"), "&&", tname("b")), Case(patwildcard, None, lit(true))),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "parenthesized condition: an operator and `new` after the parentheses, `then` on the same line",
  ) {
    val code = "def f = if (a) && new B().c then c else d"
    val layout = "def f = if (a && new B().c) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(tname("a"), "&&", tselect(Term.New(init(pname("B"), List())), "c")),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
    val layout =
      """|def f = if (a && b match {
         |  case 1 => true
         |  case _ => false
         |}) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tmatch(
          tinfix(tname("a"), "&&", tname("b")),
          Case(lit(1), None, lit(true)),
          Case(patwildcard, None, lit(false)),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
      """|<input>:3: error: `while` expected but `outdent` found
         |  do c
         |      ^""".stripMargin
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
    val error =
      """|<input>:3: error: `then` expected but `(` found
         |      (x => x > 0)
         |      ^""".stripMargin
    runTestError[Stat](code, error)
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
         |LF [45..46)
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
         |LF [65..66)
         |Indentation.Outdent [65..65)
         |KwThen [70..74)
         |Ident(c) [75..76)
         |KwElse [79..83)
         |Ident(d) [84..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if (a || xs.exists {
         |  x => if (x) true else false
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
          tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("x"))(tif(tname("x"), lit(true), lit(false)))),
          ),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |Indentation.Outdent [76..76)
         |KwDo [81..83)
         |Ident(c) [84..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = while (a || xs.exists { x =>
         |  for (y <- x) println(y)
         |  true
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
          tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("x"))(blk(
              Term.For(
                List(Enumerator.Generator(patvar("y"), tname("x"))),
                tapply(tname("println"), tname("y")),
              ),
              lit(true),
            ))),
          ),
        ),
        tname("c"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |LF [56..57)
         |Indentation.Outdent [56..56)
         |KwThen [61..65)
         |Ident(c) [66..67)
         |KwElse [70..74)
         |Ident(d) [75..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if (a || xs.exists { x =>
         |  val y = x
         |  y > 0
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
          tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("x"))(
              blk(Defn.Val(Nil, List(patvar("y")), None, tname("x")), tinfix(tname("y"), ">", lit(0))),
            )),
          ),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
    val layout =
      """|def f = if (a || x match {
         |  case y => y
         |}) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tmatch(tinfix(tname("a"), "||", tname("x")), Case(patvar("y"), None, tname("y"))),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
    val layout = "def f = if (a || <x/>.isEmpty) c else d"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(tname("a"), "||", tselect(Term.Xml(List(Lit.String("<x/>")), Nil), "isEmpty")),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |LF [44..45)
         |KwThen [49..53)
         |Ident(c) [54..55)
         |Indentation.Outdent [55..55)
         |KwElse [58..62)
         |Ident(d) [63..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  if (x) = xs.exists: y =>
         |         ^""".stripMargin
    runTestError[Stat](code, error)
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
         |LFLF [21..23)
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
    val error =
      """|<input>:4: error: `outdent` expected but `then` found
         |    || c then d
         |         ^""".stripMargin
    runTestError[Stat](code, error)
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
         |LFLF [21..23)
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
         |LF [21..22)
         |Ident(c) [28..29)
         |LF [29..30)
         |KwThen [34..38)
         |Ident(d) [39..40)
         |KwElse [43..47)
         |Ident(e) [48..49)
         |Indentation.Outdent [49..49)
         |EOF [50..50)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:4: error: illegal start of statement
         |    then d
         |    ^""".stripMargin
    runTestError[Stat](code, error)
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
    val layout =
      """|def f = if (a && xs.exists {
         |  x => if (x) true else false
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
          tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("x"))(tif(tname("x"), lit(true), lit(false)))),
          ),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
    val layout =
      """|def f = if (a && xs.exists { x =>
         |  val y = x
         |  y > 0
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
          tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("x"))(
              blk(Defn.Val(Nil, List(patvar("y")), None, tname("x")), tinfix(tname("y"), ">", lit(0))),
            )),
          ),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |LF [69..70)
         |KwCase [72..76)
         |Underscore [77..78)
         |RightArrow [79..81)
         |Constant.Int(2) [82..83)
         |Indentation.Outdent [83..83)
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
         |LF [71..72)
         |KwCase [74..78)
         |Underscore [79..80)
         |RightArrow [81..83)
         |Constant.Int(2) [84..85)
         |Indentation.Outdent [85..85)
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
    val layout = "object A { def f = if (a && f(if (b) c else d)) e else g }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tinfix(tname("a"), "&&", tapply(tname("f"), tif(tname("b"), tname("c"), tname("d")))),
          tname("e"),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
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
         |  def f = if ({
         |    if (a) {
         |      b
         |    }
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
        tif(blk(tif(tname("a"), blk(tname("b"))), tname("c")), tname("d"), tname("e")),
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
      """|object A {
         |  def f = while ({
         |    while (a) b
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
        Term.While(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d")),
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
         |LF [38..39)
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
         |KwThen [37..41)
         |Ident(c) [42..43)
         |KwElse [44..48)
         |Ident(d) [49..50)
         |Indentation.Outdent [50..50)
         |LF [50..51)
         |Ident(g) [53..54)
         |Indentation.Outdent [54..54)
         |EOF [55..55)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: `outdent` expected but `then` found
         |  if try a catch case _ => b then c else d
         |                             ^""".stripMargin
    runTestError[Stat](code, error)
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
         |KwThen [37..41)
         |Ident(c) [42..43)
         |Indentation.Outdent [43..43)
         |LF [43..44)
         |Ident(g) [46..47)
         |Indentation.Outdent [47..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: `outdent` expected but `then` found
         |  if try a catch case _ => b then c
         |                             ^""".stripMargin
    runTestError[Stat](code, error)
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
         |KwDo [40..42)
         |Ident(c) [43..44)
         |Indentation.Outdent [44..44)
         |LF [44..45)
         |Ident(g) [47..48)
         |Indentation.Outdent [48..48)
         |EOF [49..49)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: `outdent` expected but `do` found
         |  while try a catch case _ => b do c
         |                                ^""".stripMargin
    runTestError[Stat](code, error)
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
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |Indentation.Outdent [42..42)
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
         |KwThen [29..33)
         |Ident(c) [34..35)
         |Indentation.Outdent [35..35)
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
         |KwThen [31..35)
         |Ident(c) [36..37)
         |KwElse [38..42)
         |Ident(d) [43..44)
         |Indentation.Outdent [44..44)
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
         |KwThen [45..49)
         |Ident(c) [50..51)
         |KwElse [52..56)
         |Ident(d) [57..58)
         |Indentation.Outdent [58..58)
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
         |KwDo [32..34)
         |Ident(c) [35..36)
         |Indentation.Outdent [36..36)
         |LF [36..37)
         |Ident(g) [39..40)
         |LF [40..41)
         |Ident(h) [43..44)
         |Indentation.Outdent [44..44)
         |EOF [45..45)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:3: error: `outdent` expected but `do` found
         |    new B(a).ok do c
         |                ^""".stripMargin
    runTestError[Stat](code, error)
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
         |KwThen [64..68)
         |Ident(c) [69..70)
         |KwElse [71..75)
         |Ident(d) [76..77)
         |Indentation.Outdent [77..77)
         |LF [77..78)
         |Ident(g) [80..81)
         |LF [81..82)
         |Ident(h) [84..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:4: error: `outdent` expected but `then` found
         |      case _ => false then c else d
         |                      ^""".stripMargin
    runTestError[Stat](code, error)
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
         |LF [24..25)
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
         |LF [63..64)
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
         |LF [24..25)
         |LeftParen [29..30)
         |Ident(c) [30..31)
         |RightParen [31..32)
         |LF [32..33)
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
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  if (a).g
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("unparenthesized condition: a select, deeper argument in parentheses, `then` on a later line") {
    val code =
      """|def f =
         |  if a.g
         |    (c)
         |  then c else d
         |""".stripMargin
    val error =
      """|<input>:3: error: `then` expected but `(` found
         |    (c)
         |    ^""".stripMargin
    runTestError[Stat](code, error)
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
         |LF [40..41)
         |LeftParen [45..46)
         |Ident(e) [46..47)
         |RightParen [47..48)
         |LF [48..49)
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
    val error =
      """|<input>:3: error: `then` expected but `(` found
         |    (e)
         |    ^""".stripMargin
    runTestError[Stat](code, error)
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
         |LF [38..39)
         |LeftParen [43..44)
         |Ident(e) [44..45)
         |RightParen [45..46)
         |LF [46..47)
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
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  if (a).g(if b then 1 else 2)
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("parenthesized condition: a colon at the line end in braces, deeper `then` and `else`, blank line") {
    val code =
      """|def f = {
         |  if (a):
         |      then c else d
         |
         |}
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  if (a):
         |        ^""".stripMargin
    runTestError[Stat](code, error)
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
         |LF [56..57)
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
      """|def f = if (new B(a).ok) c {
         |  c
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"),
          tapply(tname("c"), blk(tname("c"))),
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
         |KwThen [45..49)
         |Ident(d) [50..51)
         |KwElse [52..56)
         |Ident(e) [57..58)
         |Indentation.Outdent [58..58)
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
         |KwDo [51..53)
         |Ident(d) [54..55)
         |Indentation.Outdent [55..55)
         |LF [55..56)
         |Ident(g) [60..61)
         |LF [61..62)
         |Ident(h) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `do` found
         |      while (a) b; c do d
         |                     ^""".stripMargin
    runTestError[Source](code, error)
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
         |KwThen [45..49)
         |Ident(d) [50..51)
         |Indentation.Outdent [51..51)
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
         |KwDo [68..70)
         |Ident(c) [71..72)
         |LF [72..73)
         |Ident(g) [77..78)
         |LF [78..79)
         |Ident(h) [83..84)
         |Indentation.Outdent [84..84)
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
         |Ident(a) [80..81)
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
         |KwDo [69..71)
         |Ident(step) [72..76)
         |LeftParen [76..77)
         |RightParen [77..78)
         |Indentation.Outdent [78..78)
         |LF [78..79)
         |Ident(g) [83..84)
         |LF [84..85)
         |Ident(h) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `do` found
         |      i += 1; if (i > n) stop(); i < m do step()
         |                                       ^""".stripMargin
    runTestError[Source](code, error)
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
         |Indentation.Outdent [56..56)
         |KwThen [58..62)
         |Ident(d) [63..64)
         |KwElse [65..69)
         |Ident(e) [70..71)
         |LF [71..72)
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
    val error =
      """|<input>:6: error: illegal start of statement
         |    then g else h
         |    ^""".stripMargin
    runTestError[Source](code, error)
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
         |LF [74..75)
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
         |Indentation.Outdent [62..62)
         |KwDo [64..66)
         |Ident(d) [67..68)
         |LF [68..69)
         |Ident(e) [75..76)
         |LF [76..77)
         |KwDo [81..83)
         |Ident(k) [84..85)
         |LF [85..86)
         |Ident(g) [90..91)
         |Indentation.Outdent [91..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:8: error: `while` expected but `identifier` found
         |    g
         |    ^""".stripMargin
    runTestError[Source](code, error)
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
         |LF [35..36)
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
         |LF [44..45)
         |LeftParen [53..54)
         |Ident(c) [54..55)
         |RightParen [55..56)
         |Indentation.Outdent [55..55)
         |KwThen [57..61)
         |Ident(d) [62..63)
         |KwElse [64..68)
         |Ident(e) [69..70)
         |LF [70..71)
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
    val error =
      """|<input>:6: error: illegal start of statement
         |    then g else h
         |    ^""".stripMargin
    runTestError[Source](code, error)
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
         |LF [38..39)
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
         |LF [71..72)
         |KwThen [78..82)
         |Ident(y) [83..84)
         |KwElse [85..89)
         |Ident(z) [90..91)
         |Indentation.Outdent [91..91)
         |LF [91..92)
         |Ident(g) [96..97)
         |LF [97..98)
         |Ident(h) [102..103)
         |Indentation.Outdent [103..103)
         |Indentation.Outdent [103..103)
         |EOF [104..104)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:7: error: `then` expected but `identifier` found
         |    g
         |    ^""".stripMargin
    runTestError[Source](code, error)
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
         |KwDo [84..86)
         |Ident(y) [87..88)
         |Indentation.Outdent [88..88)
         |LF [88..89)
         |Ident(g) [93..94)
         |LF [94..95)
         |Ident(h) [99..100)
         |Indentation.Outdent [100..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:7: error: `do` expected but `identifier` found
         |    g
         |    ^""".stripMargin
    runTestError[Source](code, error)
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
         |KwThen [48..52)
         |Ident(d) [53..54)
         |KwElse [55..59)
         |Ident(e) [60..61)
         |Indentation.Outdent [61..61)
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
         |KwThen [53..57)
         |Ident(d) [58..59)
         |KwElse [60..64)
         |Ident(e) [65..66)
         |Indentation.Outdent [66..66)
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
         |LF [44..45)
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
         |KwDo [50..52)
         |Ident(d) [53..54)
         |Indentation.Outdent [54..54)
         |LF [54..55)
         |Ident(g) [59..60)
         |LF [60..61)
         |Ident(h) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `do` found
         |      if (a) b.g; c do d
         |                    ^""".stripMargin
    runTestError[Source](code, error)
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
         |LF [16..17)
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
         |LF [14..15)
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
         |LF [18..19)
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
         |LF [18..19)
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
         |LF [16..17)
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
         |LF [20..21)
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
         |LF [21..22)
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
         |LF [25..26)
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
         |LF [26..27)
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
         |LF [24..25)
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
         |LF [28..29)
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
         |LF [19..20)
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
         |LF [17..18)
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
         |LF [21..22)
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
