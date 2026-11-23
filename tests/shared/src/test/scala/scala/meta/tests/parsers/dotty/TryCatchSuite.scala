package scala.meta.tests.parsers.dotty

import scala.meta._

class TryCatchSuite extends BaseDottySuite {

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
         |Indentation.Outdent [61..61)
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
         |InfixLF [68..69)
         |Ident(&&) [77..79)
         |Ident(w) [80..81)
         |Indentation.Outdent [81..81)
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
         |        } && w
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
                tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
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

  test("indented try body, catch on the body line") {
    val code =
      """|def f =
         |  try
         |    x catch case _ => 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwTry [10..13)
         |Indentation.Indent [13..13)
         |Ident(x) [18..19)
         |Indentation.Outdent [19..19)
         |KwCatch [20..25)
         |Indentation.Indent [26..26)
         |KwCase [26..30)
         |Underscore [31..32)
         |RightArrow [33..35)
         |Constant.Int(1) [36..37)
         |Indentation.Outdent [37..37)
         |Indentation.Outdent [37..37)
         |EOF [38..38)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = try x catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.Try(tname("x"), List(Case(Pat.Wildcard(), None, lit(1))), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented try body, finally on the body line") {
    val code =
      """|def f =
         |  try
         |    x finally g
         |""".stripMargin
    val layout = "def f = try x finally g"
    val tree = Defn.Def(Nil, "f", Nil, None, Term.Try(tname("x"), Nil, Some(tname("g"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented try body, catch on the next line") {
    val code =
      """|def f =
         |  try
         |    x
         |  catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try x catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.Try(tname("x"), List(Case(Pat.Wildcard(), None, lit(1))), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("catch case on the try line, finally on the case body line") {
    val code =
      """|def f =
         |  try a catch case _ =>
         |    b finally g
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case _ => b
         |} finally g
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(tname("a"), List(Case(Pat.Wildcard(), None, tname("b"))), Some(tname("g"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scala 3.0: indented try body, catch on the body line") {
    implicit val dialect: Dialect = dialects.Scala30
    val code =
      """|def f =
         |  try
         |    x catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try x catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.Try(tname("x"), List(Case(Pat.Wildcard(), None, lit(1))), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented try body, a comment line and a comment before finally at the body indent") {
    val code =
      """|def f =
         |  try
         |    a
         |    // note
         |    /* more */ finally b
         |""".stripMargin
    val layout =
      """|def f = try {
         |  a
         |  // note
         |  /* more */
         |} finally b
         |""".stripMargin
    val b = tname("b")
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(blk(tnameComments("a")()("// note", "/* more */")), Nil, Some(b)),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented try body, a comment right before finally at the body indent") {
    val code =
      """|def f =
         |  try
         |    a
         |    /* note */finally b
         |""".stripMargin
    val layout =
      """|def f = try {
         |  a
         |  /* note */
         |} finally b
         |""".stripMargin
    val b = tname("b")
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.Try(blk(tnameComments("a")()("/* note */")), Nil, Some(b)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented try body, a comment before catch at the body indent, cases on the next line") {
    val code =
      """|def f =
         |  try
         |    a
         |    /* note */ catch
         |      case _ => b
         |""".stripMargin
    val layout =
      """|def f = try {
         |  a
         |  /* note */
         |} catch {
         |  case _ => b
         |}
         |""".stripMargin
    val cases = Term.CasesBlock(List(Case(Pat.Wildcard(), None, tname("b"))))
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.Try(blk(tnameComments("a")()("/* note */")), Some(cases), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented try body, a comment right before catch at the body indent") {
    val code =
      """|def f =
         |  try
         |    a
         |    /* note */catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try {
         |  a
         |  /* note */
         |} catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val cases = List(Case(Pat.Wildcard(), None, lit(1)))
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.Try(blk(tnameComments("a")()("/* note */")), cases, None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented catch cases, a comment before finally at the case indent") {
    val code =
      """|def f =
         |  try a catch
         |    case _ => b
         |    /* note */ finally c
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case _ =>
         |    b
         |  /* note */
         |} finally c
         |""".stripMargin
    val c = tname("c")
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(
        tname("a"),
        List(Case.newBuilder(Pat.Wildcard(), None, tname("b")).endComment(Seq("/* note */")).result()),
        Some(c),
      ),
    )
    // parsing the reprint attaches the comment to `b`, not to the case
    parseAndCheckTree[Stat](code, layout)(tree)
  }

  test("indented try body, a comment before finally at the body indent") {
    val code =
      """|def f =
         |  try
         |    a
         |    /* note */ finally b
         |""".stripMargin
    val layout =
      """|def f = try {
         |  a
         |  /* note */
         |} finally b
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(blk(tnameComments("a")()("/* note */")), Nil, Some(tname("b"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented try body, a comment before catch at the body indent") {
    val code =
      """|def f =
         |  try
         |    x
         |    /* note */ catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try {
         |  x
         |  /* note */
         |} catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val cases = List(Case(Pat.Wildcard(), None, lit(1)))
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.Try(blk(tnameComments("x")()("/* note */")), cases, None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented try body, a block comment and catch on the body line") {
    val code =
      """|def f =
         |  try
         |    x /* note */ catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try x /* note */ catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(tnameComments("x")()("/* note */"), List(Case(Pat.Wildcard(), None, lit(1))), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("inline try body, an argument on a deeper line, catch on the next line") {
    val code =
      """|def f =
         |  try foo
         |    (x)
         |  catch case _ => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `outdent` expected but `catch` found
         |  catch case _ => 1
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("indented try body, an if without else and catch on the body line") {
    val code =
      """|def f =
         |  try
         |    if (bar) baz catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try if (bar) baz catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val cond = Term.If(tname("bar"), tname("baz"), lit(), Nil)
    runTestAssert[Stat](code, layout)(
      Defn.Def(Nil, "f", Nil, None, Term.Try(cond, List(Case(Pat.Wildcard(), None, lit(1))), None)),
    )
    val ifTree = parseStat(code).collect { case t: Term.If => t }.head
    assertEquals(ifTree.elsep.pos.start, ifTree.thenp.pos.end)
  }

  test("indented try body, an if without else and finally on the body line") {
    val code =
      """|def f =
         |  try
         |    if (bar) baz finally g
         |""".stripMargin
    val layout = "def f = try if (bar) baz finally g"
    val cond = Term.If(tname("bar"), tname("baz"), lit(), Nil)
    runTestAssert[Stat](code, layout)(
      Defn.Def(Nil, "f", Nil, None, Term.Try(cond, Nil, Some(tname("g")))),
    )
    val ifTree = parseStat(code).collect { case t: Term.If => t }.head
    assertEquals(ifTree.elsep.pos.start, ifTree.thenp.pos.end)
  }

  test("same-line catch case with a try body, a select on the next line") {
    val code =
      """|def g =
         |  try a catch case e => try b
         |  .h
         |""".stripMargin
    val layout =
      """|def g = try a catch {
         |  case e =>
         |    try b.h
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.Try(tselect("b", "h"), Nil, None))
    val tree = Defn.Def(Nil, "g", Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case with a then body, a select on the next line") {
    val code =
      """|def g =
         |  try a catch case e => if p then b
         |  .h
         |""".stripMargin
    val layout =
      """|def g = try a catch {
         |  case e =>
         |    if (p) b.h
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.If(tname("p"), tselect("b", "h"), lit(), Nil))
    val tree = Defn.Def(Nil, "g", Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case with an if and else body, a select on the next line") {
    val code =
      """|def g =
         |  try a catch case e => if p then b else c
         |  .h
         |""".stripMargin
    val layout =
      """|def g = try a catch {
         |  case e =>
         |    if (p) b else c.h
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tif(tname("p"), tname("b"), tselect("c", "h")))
    val tree = Defn.Def(Nil, tname("g"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case with a parenthesized if body, a select on the next line") {
    val code =
      """|def g =
         |  try a catch case e => if (p) b
         |  .h
         |""".stripMargin
    val layout =
      """|def g = try a catch {
         |  case e =>
         |    if (p) b.h
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.If(tname("p"), tselect("b", "h"), lit(), Nil))
    val tree = Defn.Def(Nil, "g", Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case with a while body, a select on the next line") {
    val code =
      """|def g =
         |  try a catch case e => while (p) b
         |  .h
         |""".stripMargin
    val layout =
      """|def g = try a catch {
         |  case e =>
         |    while (p) b.h
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.While(tname("p"), tselect("b", "h")))
    val tree = Defn.Def(Nil, tname("g"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("then with try and catch, else on the next line") {
    val code =
      """|if c then try a catch { case _ => b }
         |else d
         |""".stripMargin
    val layout =
      """|if (c) try a catch {
         |  case _ => b
         |} else d
         |""".stripMargin
    val body = Term.Try(tname("a"), List(Case(Pat.Wildcard(), None, tname("b"))), None)
    runTestAssert[Stat](code, layout)(Term.If(tname("c"), body, tname("d"), Nil))
  }

  test("lambda argument with try and catch, a comma on the try line") {
    val code =
      """|foo(x =>
         |  try a catch { case _ => b }, c)
         |""".stripMargin
    val layout =
      """|foo(x => try a catch {
         |  case _ => b
         |}, c)
         |""".stripMargin
    val body = Term.Try(tname("a"), List(Case(Pat.Wildcard(), None, tname("b"))), None)
    runTestAssert[Stat](code, layout)(tapply(tname("foo"), tfunc(tparam("x"))(body), tname("c")))
  }

  test("lambda argument with try and catch, a comma at the try line end") {
    val code =
      """|foo(
         |  x =>
         |    try a catch { case _ => b },
         |  c,
         |)
         |""".stripMargin
    val layout =
      """|foo(x => try a catch {
         |  case _ => b
         |}, c)
         |""".stripMargin
    val body = Term.Try(tname("a"), List(Case(Pat.Wildcard(), None, tname("b"))), None)
    runTestAssert[Stat](code, layout)(tapply(tname("foo"), tfunc(tparam("x"))(body), tname("c")))
  }

  test("lambda argument with try, a comma on the try line") {
    val code =
      """|foo(x =>
         |  try a, c)
         |""".stripMargin
    val layout = "foo(x => try a, c)"
    val body = Term.Try(tname("a"), Nil, None)
    runTestAssert[Stat](code, layout)(tapply(tname("foo"), tfunc(tparam("x"))(body), tname("c")))
  }

  test("script: try and catch, a parenthesized line and a statement") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowToplevelTerms(true)
    val code =
      """|try a catch { case _ => b }
         |(c)
         |d
         |""".stripMargin
    val layout =
      """|try a catch {
         |  case _ => b
         |}
         |c
         |d
         |""".stripMargin
    val stat = Term.Try(tname("a"), List(Case(Pat.Wildcard(), None, tname("b"))), None)
    runTestAssert[Source](code, layout)(Source(List(stat, tname("c"), tname("d"))))
  }

  test("lambda argument with try and a catch expression, a comma on the try line") {
    val code =
      """|foo(x =>
         |  try a catch h, c)
         |""".stripMargin
    val layout = "foo(x => try a catch h, c)"
    val body = Term.TryWithHandler(tname("a"), tname("h"), None)
    runTestAssert[Stat](code, layout)(tapply(tname("foo"), tfunc(tparam("x"))(body), tname("c")))
  }

  test("generator with try and catch, do on the next line") {
    val code =
      """|for x <- try a catch { case _ => Nil }
         |do f(x)
         |""".stripMargin
    val layout =
      """|for (x <- try a catch {
         |  case _ => Nil
         |}) f(x)
         |""".stripMargin
    val rhs = Term.Try(tname("a"), List(Case(Pat.Wildcard(), None, tname("Nil"))), None)
    val tree = Term.For(List(Enumerator.Generator(patvar("x"), rhs)), tapply("f", "x"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented then with try and catch, else on the next line") {
    val code =
      """|if c then
         |  try a catch { case _ => b }
         |else d
         |""".stripMargin
    val layout =
      """|if (c) try a catch {
         |  case _ => b
         |} else d
         |""".stripMargin
    val body = Term.Try(tname("a"), List(Case(Pat.Wildcard(), None, tname("b"))), None)
    runTestAssert[Stat](code, layout)(Term.If(tname("c"), body, tname("d"), Nil))
  }

  test("case clause with a try and a case on the catch line, handler arrow at the line end, cases on the next lines") {
    val code =
      """|def f = x match
         |  case 1 => try a catch case e =>
         |      case X => 1
         |  case 2 => c
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e =>
         |        {
         |          case X => 1
         |        }
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val handler = Term.PartialFunction(List(Case(tname("X"), None, int(1))))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(1), None, Term.Try(tname("a"), List(Case(patvar("e"), None, handler)), None)),
        Case(int(2), None, tname("c")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case clause with a try and a case on the catch line, semicolon after the handler, next case") {
    val code =
      """|def f = x match
         |  case 1 => try a catch case e => b;
         |  case 2 => c
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e => b
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(1), None, Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None)),
        Case(int(2), None, tname("c")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case clause with a try and a case on the catch line, semicolon after the handler, next case, match with braces") {
    val code =
      """|def f = x match {
         |  case 1 => try a catch case e => b;
         |  case 2 => c
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e => b
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(1), None, Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None)),
        Case(int(2), None, tname("c")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case clause with a try and a case on the catch line, match in the handler, empty last case, next case") {
    val code =
      """|def f = x match
         |  case 1 => try a catch case e => e match
         |      case X => g()
         |      case _ =>
         |  case 2 => c
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e =>
         |        e match {
         |          case X =>
         |            g()
         |          case _ =>
         |        }
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val handler =
      tmatch(tname("e"), Case(tname("X"), None, tapply(tname("g"))), Case(Pat.Wildcard(), None, blk()))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(1), None, Term.Try(tname("a"), List(Case(patvar("e"), None, handler)), None)),
        Case(int(2), None, tname("c")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case clause with a try and a case on the catch line, try with catch at the line end in the handler, next case") {
    val code =
      """|def f = x match
         |  case 1 => try a catch case e => try b catch
         |      case X => g()
         |  case 2 => c
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e =>
         |        try b catch {
         |          case X =>
         |            g()
         |        }
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val handler = Term.Try(tname("b"), List(Case(tname("X"), None, tapply(tname("g")))), None)
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(1), None, Term.Try(tname("a"), List(Case(patvar("e"), None, handler)), None)),
        Case(int(2), None, tname("c")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case clause with a try and a case on the catch line, try with catch at the line end in the handler, next case, match with braces") {
    val code =
      """|def f = x match {
         |  case 1 => try a catch case e => try b catch
         |      case X => g()
         |  case 2 => c
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e =>
         |        try b catch {
         |          case X =>
         |            g()
         |        }
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val handler = Term.Try(tname("b"), List(Case(tname("X"), None, tapply(tname("g")))), None)
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(1), None, Term.Try(tname("a"), List(Case(patvar("e"), None, handler)), None)),
        Case(int(2), None, tname("c")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case clause with a try and a case on the catch line, inline then with a match in the handler, next case") {
    val code =
      """|def f = x match
         |  case 1 => try a catch case e => if p then e match
         |      case X => 1
         |  case 2 => c
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e =>
         |        if (p) e match {
         |          case X => 1
         |        }
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val handler = Term
      .If(tname("p"), tmatch(tname("e"), Case(tname("X"), None, int(1))), Lit.Unit(), Nil)
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(1), None, Term.Try(tname("a"), List(Case(patvar("e"), None, handler)), None)),
        Case(int(2), None, tname("c")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in an indented def body, two deeper lines, a line at the try's indent") {
    val code =
      """|def f =
         |  try a catch case e => b
         |    c
         |    d
         |  e
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e => b
         |  }
         |  c
         |  d
         |  e
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tname("c"), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case on the def line, a deeper leading infix") {
    val code =
      """|def f = try a catch case e => b
         |  + 1
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    b + 1
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("b"), "+", lit(1)))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in an indented def body, a deeper comment line, a deeper line") {
    val code =
      """|def f =
         |  try a catch case e => b
         |    // c
         |    c
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e => b
         |  }
         |  // c
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tnameComments("c")("// c")()),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case, a trailing symbolic operator, a deeper operand, a line at the try's indent") {
    val code =
      """|def f =
         |  try a catch case e => b +
         |    c
         |  d
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e =>
         |      b + c
         |  }
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("b"), "+", tname("c")))
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.Try(tname("a"), List(handler), None), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case with a symbolic infix body, a deeper line, a line at the try's indent") {
    val code =
      """|def f =
         |  try a catch case e => a + b
         |    c
         |  d
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e =>
         |      a + b
         |  }
         |  c
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("a"), "+", tname("b")))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case, a trailing alphanumeric operator, a deeper operand") {
    val code =
      """|def f =
         |  try a catch case e => b max
         |    c
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    b max c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("b"), "max", tname("c")))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case with a prefix operator body, a deeper line, a line at the try's indent") {
    val code =
      """|def f =
         |  try a catch case e => -b
         |    c
         |  d
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e =>
         |      -b
         |  }
         |  c
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.ApplyUnary(tname("-"), tname("b")))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case, a deeper argument in parentheses") {
    val code =
      """|def f =
         |  try a catch case e => b
         |    (x)
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    b(x)
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tapply(tname("b"), tname("x")))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case with an alphanumeric infix body of names, a deeper line, a line at the try's indent") {
    val code =
      """|def f =
         |  try a catch case e => xs map g
         |    c
         |  d
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e =>
         |      xs map g
         |  }
         |  c
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("xs"), "map", tname("g")))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case, a deeper argument in braces") {
    val code =
      """|def f =
         |  try a catch case e => b
         |    { x }
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    b {
         |      x
         |    }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tapply(tname("b"), blk(tname("x"))))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case on the def line in an object, a semicolon and a statement") {
    val code =
      """|object A:
         |  def f = try a catch case e => b; c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e => b
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val defn = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(Defn.Object(Nil, tname("A"), tpl(defn, tname("c"))))
  }

  test("indented def body, a deeper argument in parentheses") {
    val code =
      """|def f =
         |  b
         |    (x)
         |""".stripMargin
    val layout = "def f = b(x)"
    runTestAssert[Stat](code, layout)(
      Defn.Def(Nil, tname("f"), Nil, None, tapply(tname("b"), tname("x"))),
    )
  }

  test("same-line catch case in braces, a semicolon and a statement") {
    val code =
      """|def f = { try a catch case e => b; c }
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e => b
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.Try(tname("a"), List(handler), None), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case, the arrow at the line end, a deeper case") {
    val code =
      """|def f =
         |  try a catch case e =>
         |    case x => x
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    {
         |      case x => x
         |    }
         |}
         |""".stripMargin
    val body = Term.PartialFunction(List(Case(patvar("x"), None, tname("x"))))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, body)), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "same-line catch case on the def line in an object, a deeper leading select, a deeper line",
  ) {
    val code =
      """|object A:
         |  def f = try a catch case e => b
         |      .foo
         |      c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      b.foo
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tselect("b", "foo"))
    val defn = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(Defn.Object(Nil, tname("A"), tpl(defn, tname("c"))))
  }

  test("same-line catch case in an indented def body, finally at the try's indent") {
    val code =
      """|def f =
         |  try a catch case e => b
         |  finally c
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e => b
         |} finally c
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), Some(tname("c"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in a block in braces, a deeper line, a line at the try's indent") {
    val code =
      """|def f = {
         |  try a catch case e => b
         |    c
         |  d
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e => b
         |  }
         |  c
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case on the def line, a second case aligned with the first") {
    val code =
      """|def f = try a catch case e: E => 1
         |                    case g => 2
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e: E => 1
         |  case g => 2
         |}
         |""".stripMargin
    val cases =
      List(Case(Pat.Typed(patvar("e"), pname("E")), None, int(1)), Case(patvar("g"), None, int(2)))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), cases, None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in a match case body, a deeper line, the next case") {
    val code =
      """|def f = x match
         |  case 1 => try a catch case e => b
         |    c
         |  case 2 => d
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e => b
         |    }
         |    c
         |  case 2 =>
         |    d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(1), None, blk(Term.Try(tname("a"), List(handler), None), tname("c"))),
        Case(int(2), None, tname("d")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case, a second case on the same line") {
    val code =
      """|def f = try a catch case e => 1 case g => 2
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e => 1
         |  case g => 2
         |}
         |""".stripMargin
    val cases = List(Case(patvar("e"), None, int(1)), Case(patvar("g"), None, int(2)))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), cases, None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in a then branch, a deeper line, a line at the if's indent") {
    val code =
      """|def f =
         |  if p then try a catch case e => b
         |    c
         |  d
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (p) try a catch {
         |    case e => b
         |  }
         |  c
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = tif(tname("p"), Term.Try(tname("a"), List(handler), None))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(body, tname("c"), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in parentheses, a semicolon and a statement") {
    val code =
      """|def f = g(try a catch case e => b; c)
         |""".stripMargin
    val layout =
      """|def f = g(try a catch {
         |  case e =>
         |    b
         |    c
         |})
         |""".stripMargin
    val handler = Case(patvar("e"), None, blk(tname("b"), tname("c")))
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply(tname("g"), Term.Try(tname("a"), List(handler), None)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in an else branch, a deeper line, a line at the if's indent") {
    val code =
      """|def f =
         |  if p then a else try b catch case e => c
         |    d
         |  g
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (p) a else try b catch {
         |    case e => c
         |  }
         |  d
         |  g
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("c"))
    val body = tif(tname("p"), tname("a"), Term.Try(tname("b"), List(handler), None))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(body, tname("d"), tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in a for generator, the next generator") {
    val code =
      """|def f =
         |  for
         |    x <- try xs catch case e => ys
         |    y <- zs
         |  yield y
         |""".stripMargin
    val layout =
      """|def f = for (x <- try xs catch {
         |  case e => ys
         |}; y <- zs) yield y
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("ys"))
    val enums = List(
      Enumerator.Generator(patvar("x"), Term.Try(tname("xs"), List(handler), None)),
      Enumerator.Generator(patvar("y"), tname("zs")),
    )
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.ForYield(enums, tname("y")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in a while body, a deeper line, a line at the while's indent") {
    val code =
      """|def f =
         |  while p do try a catch case e => b
         |    c
         |  d
         |""".stripMargin
    val layout =
      """|def f = {
         |  while (p) try a catch {
         |    case e => b
         |  }
         |  c
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = Term.While(tname("p"), Term.Try(tname("a"), List(handler), None))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(body, tname("c"), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case, the arrow at the line end, a deeper case") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f =
         |  try a catch case e =>
         |    case x => x
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    {
         |      case x => x
         |    }
         |}
         |""".stripMargin
    val body = Term.PartialFunction(List(Case(patvar("x"), None, tname("x"))))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, body)), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in a colon argument, a deeper line, a line at the argument's indent") {
    val code =
      """|def f =
         |  foo:
         |    try a catch case e => b
         |      c
         |  d
         |""".stripMargin
    val layout =
      """|def f = {
         |  foo {
         |    try a catch {
         |      case e => b
         |    }
         |    c
         |  }
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = tapply(tname("foo"), blk(Term.Try(tname("a"), List(handler), None), tname("c")))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(body, tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "without significant indentation: same-line catch case on the def line, a deeper leading select",
  ) {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = try a catch case e => b
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    b.foo
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tselect("b", "foo"))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in an assignment, a deeper line, a line at the assignment's indent") {
    val code =
      """|def f =
         |  y = try a catch case e => b
         |    c
         |  d
         |""".stripMargin
    val layout =
      """|def f = {
         |  y = try a catch {
         |    case e => b
         |  }
         |  c
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = Term.Assign(tname("y"), Term.Try(tname("a"), List(handler), None))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(body, tname("c"), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "without significant indentation: same-line catch case on the def line, a deeper leading infix",
  ) {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = try a catch case e => b
         |  + 1
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    b + 1
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("b"), "+", lit(1)))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in a nested handler, a deeper line, a line at the try's indent") {
    val code =
      """|def f =
         |  try a catch case e => try b catch case g => c
         |    d
         |  h
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e =>
         |      try b catch {
         |        case g => c
         |      }
         |  }
         |  d
         |  h
         |}
         |""".stripMargin
    val inner = Term.Try(tname("b"), List(Case(patvar("g"), None, tname("c"))), None)
    val handler = Case(patvar("e"), None, inner)
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tname("d"), tname("h")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "without significant indentation: same-line catch case in braces, a semicolon and a statement",
  ) {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = { try a catch case e => b; c }
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e => b
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.Try(tname("a"), List(handler), None), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case on the def line in an object, a deeper line, the next definition") {
    val code =
      """|object A:
         |  def f = try a catch case e => b
         |    c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e => b
         |  }
         |  c
         |  def g = 1
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val defn = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    val tree = Defn
      .Object(Nil, tname("A"), tpl(defn, tname("c"), Defn.Def(Nil, tname("g"), Nil, None, int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case on the def line in an object, a semicolon and a statement") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|object A {
         |  def f = try a catch case e => b; c
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e => b
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val defn = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(Defn.Object(Nil, tname("A"), tpl(defn, tname("c"))))
  }

  test(
    "same-line catch case at the top level of a script, a deeper line, a line at the try's indent",
  ) {
    implicit val dialect: Dialect = dialects.Scala3.withAllowToplevelTerms(true)
    val code =
      """|try a catch case e => b
         |  c
         |d
         |""".stripMargin
    val layout =
      """|try a catch {
         |  case e => b
         |}
         |c
         |d
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Source(List(Term.Try(tname("a"), List(handler), None), tname("c"), tname("d")))
    runTestAssert[Source](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case then on the def line, else on the same line") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = if c then try a catch case e => b else d
         |""".stripMargin
    val layout =
      """|def f = if (c) try a catch {
         |  case e => b
         |} else d
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = tif(tname("c"), Term.Try(tname("a"), List(handler), None), tname("d"))
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, body))
  }

  test("Scala32: same-line catch case in an indented def body, two deeper lines, a line at the try's indent") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f =
         |  try a catch case e => b
         |    c
         |    d
         |  e
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e => b
         |  }
         |  c
         |  d
         |  e
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tname("c"), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case after then in braces, a deeper else on the next line") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = {
         |  if c then try a catch case e => b
         |    else d
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (c) try a catch {
         |    case e => b
         |  } else d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = tif(tname("c"), Term.Try(tname("a"), List(handler), None), tname("d"))
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, blk(body)))
  }

  test("Scala32: same-line catch case on the def line in an object, a semicolon and a statement") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def f = try a catch case e => b; c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e => b
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val defn = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(Defn.Object(Nil, tname("A"), tpl(defn, tname("c"))))
  }

  test(
    "without significant indentation: same-line catch case in a match case in braces, the next case",
  ) {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = x match {
         |  case 1 => try a catch case _: Exception => b
         |  case 2 => c
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case _: Exception => b
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val handler = Case(Pat.Typed(Pat.Wildcard(), pname("Exception")), None, tname("b"))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(1), None, Term.Try(tname("a"), List(handler), None)),
        Case(int(2), None, tname("c")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: relaxed lambda: case expression in catch case body") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f = try a catch case e => case x => x
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    {
         |      case x => x
         |    }
         |}
         |""".stripMargin
    val cases =
      List(Case(patvar("e"), None, Term.PartialFunction(List(Case(patvar("x"), None, tname("x"))))))
    runTestAssert[Stat](code, layout)(
      Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), cases, None)),
    )
  }

  test(
    "without significant indentation: same-line catch case in braces, finally at the try's indent",
  ) {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = {
         |  try a catch case e => b
         |  finally c
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e => b
         |  } finally c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.Try(tname("a"), List(handler), Some(tname("c")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case in an indented def body, two deeper lines, a line at the try's indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f =
         |  try a catch case e => b
         |    c
         |    d
         |  e
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e => b
         |  }
         |  c
         |  d
         |  e
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tname("c"), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in a nested handler on the def line in an object, a semicolon and a statement") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|object A {
         |  def f = try a catch case e => try b catch case g => c; d
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      try b catch {
         |        case g => c
         |      }
         |  }
         |  d
         |}
         |""".stripMargin
    val inner = Term.Try(tname("b"), List(Case(patvar("g"), None, tname("c"))), None)
    val handler = Case(patvar("e"), None, inner)
    val defn = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(Defn.Object(Nil, tname("A"), tpl(defn, tname("d"))))
  }

  test(
    "Scala3Future: same-line catch case on the def line in an object, a semicolon and a statement",
  ) {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = try a catch case e => b; c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e => b
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val defn = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(Defn.Object(Nil, tname("A"), tpl(defn, tname("c"))))
  }

  test("without significant indentation: same-line catch case in braces, a deeper case") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = {
         |  try a catch case e => b
         |    case g => c
         |}
         |""".stripMargin
    val error =
      """|<input>:3: error: `}` expected but `case` found
         |    case g => c
         |    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala3Future: relaxed lambda: case expression in catch case body") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = try a catch case e => case x => x
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    {
         |      case x => x
         |    }
         |}
         |""".stripMargin
    val cases =
      List(Case(patvar("e"), None, Term.PartialFunction(List(Case(patvar("x"), None, tname("x"))))))
    runTestAssert[Stat](code, layout)(
      Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), cases, None)),
    )
  }

  test("without significant indentation: same-line catch case in parentheses, a semicolon and a statement") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = g(try a catch case e => b; c)
         |""".stripMargin
    val error =
      """|<input>:1: error: `)` expected but `;` found
         |def f = g(try a catch case e => b; c)
         |                                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("without significant indentation: same-line catch case in braces, a deeper line, a line at the try's indent") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = {
         |  try a catch case e => b
         |    c
         |  d
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e => b
         |  }
         |  c
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in braces, a trailing symbolic operator, a deeper operand, a line at the try's indent") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = {
         |  try a catch case e => b +
         |    c
         |  d
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e =>
         |      b + c
         |  }
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("b"), "+", tname("c")))
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.Try(tname("a"), List(handler), None), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in braces, the arrow at the line end, a deeper case") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = {
         |  try a catch case e =>
         |    case x => x
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e =>
         |      {
         |        case x => x
         |      }
         |  }
         |}
         |""".stripMargin
    val handler =
      Case(patvar("e"), None, Term.PartialFunction(List(Case(patvar("x"), None, tname("x")))))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(Term.Try(tname("a"), List(handler), None)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: relaxed lambda: case expression in catch case body") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = try a catch case e => case x => x
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    {
         |      case x => x
         |    }
         |}
         |""".stripMargin
    val handler =
      Case(patvar("e"), None, Term.PartialFunction(List(Case(patvar("x"), None, tname("x")))))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in braces, a deeper argument in parentheses") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = {
         |  try a catch case e => b
         |    (x)
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e => b
         |  }
         |  x
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(Term.Try(tname("a"), List(handler), None), tname("x")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case on the def line in an object, a deeper leading select, a deeper line") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|object A {
         |  def f = try a catch case e => b
         |      .foo
         |      c
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      b.foo
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tselect("b", "foo"))
    val defn = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(Defn.Object(Nil, tname("A"), tpl(defn, tname("c"))))
  }

  test(
    "without significant indentation: same-line catch case in braces, a deeper argument in braces",
  ) {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = {
         |  try a catch case e => b
         |    { x }
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e =>
         |      b {
         |        x
         |      }
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tapply(tname("b"), blk(tname("x"))))
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(Term.Try(tname("a"), List(handler), None)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case on the def line in an object, a deeper line, the next definition") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|object A {
         |  def f = try a catch case e => b
         |    c
         |  def g = 1
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e => b
         |  }
         |  c
         |  def g = 1
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val defn = Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))
    val tree = Defn
      .Object(Nil, tname("A"), tpl(defn, tname("c"), Defn.Def(Nil, tname("g"), Nil, None, int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case with a try body, try at the line end, a deeper body, a line at the try's indent") {
    val code =
      """|def g =
         |  try a catch case e => try
         |      b
         |  h()
         |""".stripMargin
    val layout =
      """|def g = {
         |  try a catch {
         |    case e =>
         |      try b
         |  }
         |  h()
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.Try(tname("b"), Nil, None))
    val tree = Defn.Def(
      Nil,
      tname("g"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tapply(tname("h"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in braces with a then body, else on the next line, a statement") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def g = {
         |  try a catch case e => if p then b
         |  else c
         |  h()
         |}
         |""".stripMargin
    val layout =
      """|def g = {
         |  try a catch {
         |    case e =>
         |      if (p) b else c
         |  }
         |  h()
         |}
         |""".stripMargin
    val body = tif(tname("p"), tname("b"), tname("c"))
    val handler = Case(patvar("e"), None, body)
    val tree = Defn.Def(
      Nil,
      tname("g"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tapply(tname("h"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case with a try body, finally at the line end, a deeper body, a line at the try's indent") {
    val code =
      """|def g =
         |  try a catch case e => try b finally
         |      c
         |  h()
         |""".stripMargin
    val layout =
      """|def g = {
         |  try a catch {
         |    case e =>
         |      try b finally c
         |  }
         |  h()
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.Try(tname("b"), Nil, Some(tname("c"))))
    val tree = Defn.Def(
      Nil,
      tname("g"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tapply(tname("h"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in braces with a try body, finally on the next line, a statement") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def g = {
         |  try a catch case e => try b
         |  finally c
         |  h()
         |}
         |""".stripMargin
    val layout =
      """|def g = {
         |  try a catch {
         |    case e =>
         |      try b finally c
         |  }
         |  h()
         |}
         |""".stripMargin
    val body = Term.Try(tname("b"), Nil, Some(tname("c")))
    val handler = Case(patvar("e"), None, body)
    val tree = Defn.Def(
      Nil,
      tname("g"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tapply(tname("h"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in an argument list, a comma at the line end") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
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
    val body = Term.Try(lit(1), List(Case(Pat.Wildcard(), None, lit(2))), None)
    val tree = Defn.Val(Nil, List(patvar("foo")), None, tapply(tname("test"), body, lit("bar")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in braces with a try body, try at the line end, a deeper body, a statement") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def g = {
         |  try a catch case e => try
         |      b
         |  h()
         |}
         |""".stripMargin
    val layout =
      """|def g = {
         |  try a catch {
         |    case e =>
         |      try b
         |  }
         |  h()
         |}
         |""".stripMargin
    val body = Term.Try(tname("b"), Nil, None)
    val handler = Case(patvar("e"), None, body)
    val tree = Defn.Def(
      Nil,
      tname("g"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tapply(tname("h"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in a while body in braces, a deeper line, a statement") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def f = {
         |  while p do try a catch case e => b
         |    c
         |  d
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  while (p) try a catch {
         |    case e => b
         |  }
         |  c
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = Term.While(tname("p"), Term.Try(tname("a"), List(handler), None))
    runTestAssert[Stat](code, layout)(
      Defn.Def(Nil, tname("f"), Nil, None, blk(body, tname("c"), tname("d"))),
    )
  }

  test("without significant indentation: same-line catch case in braces with a try body, finally at the line end, a deeper body, a statement") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def g = {
         |  try a catch case e => try b finally
         |      c
         |  h()
         |}
         |""".stripMargin
    val layout =
      """|def g = {
         |  try a catch {
         |    case e =>
         |      try b finally c
         |  }
         |  h()
         |}
         |""".stripMargin
    val body = Term.Try(tname("b"), Nil, Some(tname("c")))
    val handler = Case(patvar("e"), None, body)
    val tree = Defn.Def(
      Nil,
      tname("g"),
      Nil,
      None,
      blk(Term.Try(tname("a"), List(handler), None), tapply(tname("h"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in an argument list, a comma at the line end, a deeper argument") {
    val code =
      """|object A:
         |  def g = f(try a catch case e => b,
         |    y)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def g = f(try a catch {
         |    case e => b
         |  }, y)
         |  def h = 1
         |}
         |""".stripMargin
    val arg = Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None)
    val g = Defn.Def(Nil, tname("g"), Nil, None, tapply(tname("f"), arg, tname("y")))
    val tree = Defn.Object(Nil, tname("A"), tpl(g, Defn.Def(Nil, tname("h"), Nil, None, int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in an argument list in a def body, a comma at the line end, a deeper argument") {
    val code =
      """|def g =
         |  f(try a catch case e => b,
         |    y)
         |""".stripMargin
    val layout =
      """|def g = f(try a catch {
         |  case e => b
         |}, y)
         |""".stripMargin
    val arg = Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None)
    val tree = Defn.Def(Nil, tname("g"), Nil, None, tapply(tname("f"), arg, tname("y")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case, return at the line end, a deeper value, a statement") {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => return
         |      x
         |    y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        return x
         |    }
         |    y
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.Return(tname("x")))
    val body = blk(Term.Try(tname("a"), List(handler), None), tname("y"))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "same-line catch case with an if body, return at the line end, a deeper value, a statement",
  ) {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => if p then return
         |      x
         |    y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        if (p) return x
         |    }
         |    y
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.If(tname("p"), Term.Return(tname("x")), lit(), Nil))
    val body = blk(Term.Try(tname("a"), List(handler), None), tname("y"))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case, match on a deeper line") {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => b
         |      match
         |        case 1 => c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      b match {
         |        case 1 => c
         |      }
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tmatch(tname("b"), Case(lit(1), None, tname("c"))))
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case, match at the try's indent") {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => b
         |    match
         |      case 1 => c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      b match {
         |        case 1 => c
         |      }
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tmatch(tname("b"), Case(lit(1), None, tname("c"))))
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case, a trailing operator, its operand at the try's indent") {
    val code =
      """|object A:
         |  def g =
         |    try a catch case e => b +
         |    c
         |""".stripMargin
    val layout =
      """|object A {
         |  def g = try a catch {
         |    case e =>
         |      b + c
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("b"), "+", tname("c")))
    val body = Term.Try(tname("a"), List(handler), None)
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("g"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in for parentheses, a semicolon and an enumerator") {
    val code =
      """|object A:
         |  def f = for (x <- try a catch case e => b; y <- c) yield (x, y)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- try a catch {
         |    case e => b
         |  }; y <- c) yield (x, y)
         |}
         |""".stripMargin
    val x = Enumerator
      .Generator(patvar("x"), Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None))
    val body = Term.ForYield(
      List(x, Enumerator.Generator(patvar("y"), tname("c"))),
      Term.Tuple(List(tname("x"), tname("y"))),
    )
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("match case on the match line in an argument list, a comma and an argument") {
    val code =
      """|object A:
         |  def g = f(x match case 1 => a, b)
         |""".stripMargin
    val error =
      """|<input>:2: error: `outdent` expected but `,` found
         |  def g = f(x match case 1 => a, b)
         |                               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("same-line catch case at the top level of a source, a semicolon and a term") {
    val code = "def f = try a catch case e => b; c"
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    b
         |    c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, blk(tname("b"), tname("c")))
    val tree =
      Source(List(Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))))
    runTestAssert[Source](code, layout)(tree)
  }

  test("same-line catch case in a match case, a semicolon and a second try with a same-line catch, the next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => try a catch case e => b; try c catch case g => d
         |    case 2 => h
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      try a catch {
         |        case e => b
         |      }
         |      try c catch {
         |        case g => d
         |      }
         |    case 2 =>
         |      h
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val try2 = Term.Try(tname("c"), List(Case(patvar("g"), None, tname("d"))), None)
    val case1 = Case(lit(1), None, blk(Term.Try(tname("a"), List(handler), None), try2))
    val body = tmatch(tname("x"), case1, Case(lit(2), None, tname("h")))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in a match case, a semicolon and a colon lambda with a try with a same-line catch, the next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => try a catch case e => b; xs.map: y => try c catch case g => d
         |    case 2 => h
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      try a catch {
         |        case e => b
         |      }
         |      xs.map {
         |        y => try c catch {
         |          case g => d
         |        }
         |      }
         |    case 2 =>
         |      h
         |  }
         |}
         |""".stripMargin
    val cases = List(Case(patvar("g"), None, tname("d")))
    val lambda = tfunc(tparam("y"))(Term.Try(tname("c"), cases, None))
    val handler = Case(patvar("e"), None, tname("b"))
    val case1 = Case(
      lit(1),
      None,
      blk(Term.Try(tname("a"), List(handler), None), tapply(tselect("xs", "map"), blk(lambda))),
    )
    val body = tmatch(tname("x"), case1, Case(lit(2), None, tname("h")))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in a match case in braces, a semicolon and a second try with a same-line catch, the next case") {
    val code =
      """|object A:
         |  def f = x match {
         |    case 1 => try a catch case e => b; try c catch case g => d
         |    case 2 => h
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      try a catch {
         |        case e => b
         |      }
         |      try c catch {
         |        case g => d
         |      }
         |    case 2 =>
         |      h
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val try2 = Term.Try(tname("c"), List(Case(patvar("g"), None, tname("d"))), None)
    val case1 = Case(lit(1), None, blk(Term.Try(tname("a"), List(handler), None), try2))
    val body = tmatch(tname("x"), case1, Case(lit(2), None, tname("h")))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case after then, a semicolon and a term, else on the same line") {
    val code =
      """|object A:
         |  def f = if p then try a catch case e => b; c else d
         |""".stripMargin
    val error =
      """|<input>:2: error: `;` expected but `else` found
         |  def f = if p then try a catch case e => b; c else d
         |                                               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala 2: same-line catch case without braces, a deeper line, a statement") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|object A {
         |  def f = {
         |    try a catch case e => b
         |      c
         |    d
         |  }
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e => b
         |    }
         |    c
         |    d
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = blk(Term.Try(tname("a"), List(handler), None), tname("c"), tname("d"))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala 2: same-line catch case without braces in braces, a semicolon and a term") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|def g = { try a catch case e => b; c }
         |""".stripMargin
    val layout =
      """|def g = {
         |  try a catch {
         |    case e => b
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = blk(Term.Try(tname("a"), List(handler), None), tname("c"))
    val tree = Defn.Def(Nil, tname("g"), Nil, None, body)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case in an enum body, an enum case on the next line") {
    val code =
      """|enum Color:
         |  def safe = try compute() catch case e: Exception => 0
         |  case Red, Green
         |""".stripMargin
    val layout =
      """|enum Color {
         |  def safe = try compute() catch {
         |    case e: Exception => 0
         |  }
         |  case Red, Green
         |}
         |""".stripMargin
    val handler = Case(Pat.Typed(patvar("e"), pname("Exception")), None, lit(0))
    val body = Term.Try(tapply(tname("compute")), List(handler), None)
    val tree = Defn.Enum(
      Nil,
      pname("Color"),
      Nil,
      EmptyCtor(),
      tpl(
        Defn.Def(Nil, tname("safe"), Nil, None, body),
        Defn.RepeatedEnumCase(Nil, List(tname("Red"), tname("Green"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in an enum body in braces, two enum cases") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|enum Color {
         |  def safe = try compute() catch case e: Exception => 0
         |  case Red
         |  case Green
         |}
         |""".stripMargin
    val layout =
      """|enum Color {
         |  def safe = try compute() catch {
         |    case e: Exception => 0
         |  }
         |  case Red
         |  case Green
         |}
         |""".stripMargin
    val handler = Case(Pat.Typed(patvar("e"), pname("Exception")), None, lit(0))
    val body = Term.Try(tapply(tname("compute")), List(handler), None)
    val tree = Defn.Enum(
      Nil,
      pname("Color"),
      Nil,
      EmptyCtor(),
      tpl(
        Defn.Def(Nil, tname("safe"), Nil, None, body),
        Defn.EnumCase(Nil, tname("Red"), Nil, EmptyCtor(), Nil),
        Defn.EnumCase(Nil, tname("Green"), Nil, EmptyCtor(), Nil),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in an argument list, a deeper argument clause") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|object A {
         |  def h = f(try a catch case e => g
         |    (x))
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def h = f(try a catch {
         |    case e =>
         |      g(x)
         |  })
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tapply(tname("g"), tname("x")))
    val body = tapply(tname("f"), Term.Try(tname("a"), List(handler), None))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("h"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case, match on a deeper line") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => b
         |      match
         |        case 1 => c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      b match {
         |        case 1 => c
         |      }
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tmatch(tname("b"), Case(lit(1), None, tname("c"))))
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case, return at the line end, a deeper value, a statement") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => return
         |      x
         |    y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        return x
         |    }
         |    y
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.Return(tname("x")))
    val body = blk(Term.Try(tname("a"), List(handler), None), tname("y"))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case, match at the try's indent") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => b
         |    match
         |      case 1 => c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      b match {
         |        case 1 => c
         |      }
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tmatch(tname("b"), Case(lit(1), None, tname("c"))))
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case with an if body, return at the line end, a deeper value, a statement") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => if p then return
         |      x
         |    y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        if (p) return x
         |    }
         |    y
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.If(tname("p"), Term.Return(tname("x")), lit(), Nil))
    val body = blk(Term.Try(tname("a"), List(handler), None), tname("y"))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case, a trailing operator, its operand at the try's indent") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def g =
         |    try a catch case e => b +
         |    c
         |""".stripMargin
    val layout =
      """|object A {
         |  def g = try a catch {
         |    case e =>
         |      b + c
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("b"), "+", tname("c")))
    val body = Term.Try(tname("a"), List(handler), None)
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("g"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case, match on a deeper line") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => b
         |      match
         |        case 1 => c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      b match {
         |        case 1 => c
         |      }
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tmatch(tname("b"), Case(lit(1), None, tname("c"))))
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case, return at the line end, a deeper value, a statement") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => return
         |      x
         |    y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        return x
         |    }
         |    y
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.Return(tname("x")))
    val body = blk(Term.Try(tname("a"), List(handler), None), tname("y"))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case, match at the try's indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => b
         |    match
         |      case 1 => c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      b match {
         |        case 1 => c
         |      }
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tmatch(tname("b"), Case(lit(1), None, tname("c"))))
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case with an if body, return at the line end, a deeper value, a statement") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => if p then return
         |      x
         |    y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        if (p) return x
         |    }
         |    y
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, Term.If(tname("p"), Term.Return(tname("x")), lit(), Nil))
    val body = blk(Term.Try(tname("a"), List(handler), None), tname("y"))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case, a trailing operator, its operand at the try's indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def g =
         |    try a catch case e => b +
         |    c
         |""".stripMargin
    val layout =
      """|object A {
         |  def g = try a catch {
         |    case e =>
         |      b + c
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("b"), "+", tname("c")))
    val body = Term.Try(tname("a"), List(handler), None)
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("g"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case in for parentheses, a semicolon and an enumerator") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def f = for (x <- try a catch case e => b; y <- c) yield (x, y)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- try a catch {
         |    case e => b
         |  }; y <- c) yield (x, y)
         |}
         |""".stripMargin
    val x = Enumerator
      .Generator(patvar("x"), Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None))
    val body = Term.ForYield(
      List(x, Enumerator.Generator(patvar("y"), tname("c"))),
      Term.Tuple(List(tname("x"), tname("y"))),
    )
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case in an argument list, a comma at the line end, a deeper argument") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def g = f(try a catch case e => b,
         |    y)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def g = f(try a catch {
         |    case e => b
         |  }, y)
         |  def h = 1
         |}
         |""".stripMargin
    val arg = Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None)
    val g = Defn.Def(Nil, tname("g"), Nil, None, tapply(tname("f"), arg, tname("y")))
    val tree = Defn.Object(Nil, tname("A"), tpl(g, Defn.Def(Nil, tname("h"), Nil, None, int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case in an argument list in a def body, a comma at the line end, a deeper argument") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def g =
         |  f(try a catch case e => b,
         |    y)
         |""".stripMargin
    val layout =
      """|def g = f(try a catch {
         |  case e => b
         |}, y)
         |""".stripMargin
    val arg = Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None)
    val tree = Defn.Def(Nil, tname("g"), Nil, None, tapply(tname("f"), arg, tname("y")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case in for parentheses, a semicolon and an enumerator") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = for (x <- try a catch case e => b; y <- c) yield (x, y)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- try a catch {
         |    case e => b
         |  }; y <- c) yield (x, y)
         |}
         |""".stripMargin
    val x = Enumerator
      .Generator(patvar("x"), Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None))
    val body = Term.ForYield(
      List(x, Enumerator.Generator(patvar("y"), tname("c"))),
      Term.Tuple(List(tname("x"), tname("y"))),
    )
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "Scala32: same-line catch case in an argument list, a comma at the line end, a deeper argument",
  ) {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def g = f(try a catch case e => b,
         |    y)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def g = f(try a catch {
         |    case e => b
         |  }, y)
         |  def h = 1
         |}
         |""".stripMargin
    val arg = Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None)
    val g = Defn.Def(Nil, tname("g"), Nil, None, tapply(tname("f"), arg, tname("y")))
    val tree = Defn.Object(Nil, tname("A"), tpl(g, Defn.Def(Nil, tname("h"), Nil, None, int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case in an argument list in a def body, a comma at the line end, a deeper argument") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def g =
         |  f(try a catch case e => b,
         |    y)
         |""".stripMargin
    val layout =
      """|def g = f(try a catch {
         |  case e => b
         |}, y)
         |""".stripMargin
    val arg = Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None)
    val tree = Defn.Def(Nil, tname("g"), Nil, None, tapply(tname("f"), arg, tname("y")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case in a match case, a semicolon and a second try with a same-line catch, the next case") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => try a catch case e => b; try c catch case g => d
         |    case 2 => h
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      try a catch {
         |        case e => b
         |      }
         |      try c catch {
         |        case g => d
         |      }
         |    case 2 =>
         |      h
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val try2 = Term.Try(tname("c"), List(Case(patvar("g"), None, tname("d"))), None)
    val case1 = Case(lit(1), None, blk(Term.Try(tname("a"), List(handler), None), try2))
    val body = tmatch(tname("x"), case1, Case(lit(2), None, tname("h")))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "Scala3Future: same-line catch case after then, a semicolon and a term, else on the same line",
  ) {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = if p then try a catch case e => b; c else d
         |""".stripMargin
    val error =
      """|<input>:2: error: `;` expected but `else` found
         |  def f = if p then try a catch case e => b; c else d
         |                                               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala32: same-line catch case in a match case, a semicolon and a colon lambda with a try with a same-line catch, the next case") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => try a catch case e => b; xs.map: y => try c catch case g => d
         |    case 2 => h
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    case 1 => try a catch case e => b; xs.map: y => try c catch case g => d
         |                                                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala3Future: same-line catch case in a match case in braces, a semicolon and a second try with a same-line catch, the next case") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = x match {
         |    case 1 => try a catch case e => b; try c catch case g => d
         |    case 2 => h
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      try a catch {
         |        case e => b
         |      }
         |      try c catch {
         |        case g => d
         |      }
         |    case 2 =>
         |      h
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val try2 = Term.Try(tname("c"), List(Case(patvar("g"), None, tname("d"))), None)
    val case1 = Case(lit(1), None, blk(Term.Try(tname("a"), List(handler), None), try2))
    val body = tmatch(tname("x"), case1, Case(lit(2), None, tname("h")))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case in a match case in braces, a semicolon and a second try with a same-line catch, the next case") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def f = x match {
         |    case 1 => try a catch case e => b; try c catch case g => d
         |    case 2 => h
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      try a catch {
         |        case e => b
         |      }
         |      try c catch {
         |        case g => d
         |      }
         |    case 2 =>
         |      h
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val try2 = Term.Try(tname("c"), List(Case(patvar("g"), None, tname("d"))), None)
    val case1 = Case(lit(1), None, blk(Term.Try(tname("a"), List(handler), None), try2))
    val body = tmatch(tname("x"), case1, Case(lit(2), None, tname("h")))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case in a match case, a semicolon and a colon lambda with a try with a same-line catch, the next case") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => try a catch case e => b; xs.map: y => try c catch case g => d
         |    case 2 => h
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      try a catch {
         |        case e => b
         |      }
         |      xs.map {
         |        y => try c catch {
         |          case g => d
         |        }
         |      }
         |    case 2 =>
         |      h
         |  }
         |}
         |""".stripMargin
    val cases = List(Case(patvar("g"), None, tname("d")))
    val lambda = tfunc(tparam("y"))(Term.Try(tname("c"), cases, None))
    val handler = Case(patvar("e"), None, tname("b"))
    val case1 = Case(
      lit(1),
      None,
      blk(Term.Try(tname("a"), List(handler), None), tapply(tselect("xs", "map"), blk(lambda))),
    )
    val body = tmatch(tname("x"), case1, Case(lit(2), None, tname("h")))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: same-line catch case after then, a semicolon and a term, else on the same line") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|object A:
         |  def f = if p then try a catch case e => b; c else d
         |""".stripMargin
    val error =
      """|<input>:2: error: `;` expected but `else` found
         |  def f = if p then try a catch case e => b; c else d
         |                                               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala3Future: same-line catch case in a match case, a semicolon and a second try with a same-line catch, the next case") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => try a catch case e => b; try c catch case g => d
         |    case 2 => h
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      try a catch {
         |        case e => b
         |      }
         |      try c catch {
         |        case g => d
         |      }
         |    case 2 =>
         |      h
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val try2 = Term.Try(tname("c"), List(Case(patvar("g"), None, tname("d"))), None)
    val case1 = Case(lit(1), None, blk(Term.Try(tname("a"), List(handler), None), try2))
    val body = tmatch(tname("x"), case1, Case(lit(2), None, tname("h")))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in an enum body in braces, an enum case on the next line") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|enum Color {
         |  def safe = try compute() catch case e: Exception => 0
         |  case Red, Green
         |}
         |""".stripMargin
    val layout =
      """|enum Color {
         |  def safe = try compute() catch {
         |    case e: Exception => 0
         |  }
         |  case Red, Green
         |}
         |""".stripMargin
    val handler = Case(Pat.Typed(patvar("e"), pname("Exception")), None, lit(0))
    val body = Term.Try(tapply(tname("compute")), List(handler), None)
    val tree = Defn.Enum(
      Nil,
      pname("Color"),
      Nil,
      EmptyCtor(),
      tpl(
        Defn.Def(Nil, tname("safe"), Nil, None, body),
        Defn.RepeatedEnumCase(Nil, List(tname("Red"), tname("Green"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case after else, a semicolon and a term") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = if p then a else try b catch case e => c; d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (p) a else try b catch {
         |    case e => c
         |  }
         |  d
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("c"))
    val body = Term.If(tname("p"), tname("a"), Term.Try(tname("b"), List(handler), None), Nil)
    val tree = Defn
      .Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in for parentheses, a semicolon and an enumerator") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|object A {
         |  def f = for (x <- try a catch case e => b; y <- c) yield (x, y)
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- try a catch {
         |    case e => b
         |  }; y <- c) yield (x, y)
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val enums = List(
      Enumerator.Generator(patvar("x"), Term.Try(tname("a"), List(handler), None)),
      Enumerator.Generator(patvar("y"), tname("c")),
    )
    val body = Term.ForYield(enums, Term.Tuple(List(tname("x"), tname("y"))))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case after then in a handler, a semicolon and a term, else on the same line") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = try a catch case e => if p then try b catch case g => c; d else h
         |""".stripMargin
    val error =
      """|<input>:2: error: `;` expected but `else` found
         |  def f = try a catch case e => if p then try b catch case g => c; d else h
         |                                                                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("without significant indentation: same-line catch case after a condition in parentheses, a semicolon and a term") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|object A {
         |  def f = if (p) try a catch case e => b; c
         |  def g = 1
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (p) try a catch {
         |    case e => b
         |  }
         |  c
         |  def g = 1
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = Term.If(tname("p"), Term.Try(tname("a"), List(handler), None), Lit.Unit(), Nil)
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(Nil, tname("f"), Nil, None, body),
        tname("c"),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case after then in a handler, a semicolon and a term") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = try a catch case e => if p then try b catch case g => c; d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      if (p) try b catch {
         |        case g => c
         |      }
         |  }
         |  d
         |}
         |""".stripMargin
    val inner = Term.Try(tname("b"), List(Case(patvar("g"), None, tname("c"))), None)
    val handler = Case(patvar("e"), None, Term.If(tname("p"), inner, lit(), Nil))
    val body = Term.Try(tname("a"), List(handler), None)
    val tree = Defn
      .Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body), tname("d")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in braces, a trailing operator, its operand at the try's indent") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|object A {
         |  def g = {
         |    try a catch case e => b +
         |    c
         |  }
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def g = {
         |    try a catch {
         |      case e =>
         |        b + c
         |    }
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tinfix(tname("b"), "+", tname("c")))
    val body = blk(Term.Try(tname("a"), List(handler), None))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("g"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case in a match case, a semicolon and the next case on the same line") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => try a catch case e => b; case 2 => c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      try a catch {
         |        case e => b
         |      }
         |    case 2 =>
         |      c
         |  }
         |}
         |""".stripMargin
    val cases = List(Case(patvar("e"), None, tname("b")))
    val body = tmatch(
      tname("x"),
      Case(lit(1), None, Term.Try(tname("a"), cases, None)),
      Case(lit(2), None, tname("c")),
    )
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in a try body, a semicolon and a term") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|object A {
         |  def f = try try a catch case e => b; c
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try try a catch {
         |    case e => b
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = Term.Try(Term.Try(tname("a"), List(handler), None), Nil, None)
    val tree = Defn
      .Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case after then, a deeper line, else at the if's indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f =
         |    if p then try a catch case e => b
         |      c
         |    else d
         |""".stripMargin
    val error =
      """|<input>:5: error: `outdent` expected but `else` found
         |    else d
         |    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("without significant indentation: same-line catch case in an argument list, a comma at the line end, a deeper argument") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|object A {
         |  def g = f(try a catch case e => b,
         |    y)
         |  def h = 1
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def g = f(try a catch {
         |    case e => b
         |  }, y)
         |  def h = 1
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val g = Defn.Def(
      Nil,
      tname("g"),
      Nil,
      None,
      tapply(tname("f"), Term.Try(tname("a"), List(handler), None), tname("y")),
    )
    val tree = Defn.Object(Nil, tname("A"), tpl(g, Defn.Def(Nil, tname("h"), Nil, None, lit(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case after then in a match case, a semicolon and the next case on the same line") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => if p then try a catch case e => b; case 2 => c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      if (p) try a catch {
         |        case e => b
         |      }
         |    case 2 =>
         |      c
         |  }
         |}
         |""".stripMargin
    val cases = List(Case(patvar("e"), None, tname("b")))
    val case1 = Case(lit(1), None, Term.If(tname("p"), Term.Try(tname("a"), cases, None), lit(), Nil))
    val body = tmatch(tname("x"), case1, Case(lit(2), None, tname("c")))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("without significant indentation: same-line catch case in an argument list in a def body, a comma at the line end, a deeper argument") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def g =
         |  f(try a catch case e => b,
         |    y)
         |""".stripMargin
    val layout =
      """|def g = f(try a catch {
         |  case e => b
         |}, y)
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = tapply(tname("f"), Term.Try(tname("a"), List(handler), None), tname("y"))
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("g"), Nil, None, body))
  }

  test(
    "Scala3Future: same-line catch case after then, a semicolon and a term, else on the next line",
  ) {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f =
         |    if p then try a catch case e => b; c
         |    else d
         |""".stripMargin
    val error =
      """|<input>:4: error: `outdent` expected but `else` found
         |    else d
         |    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("same-line catch case at the top level of a source, a semicolon and a definition") {
    val code = "def f = try a catch case e => b; def g = 1"
    val layout =
      """|def f = try a catch {
         |  case e => b
         |}
         |def g = 1
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val tree = Source(List(
      Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None)),
      Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case after then, a semicolon and a term, a statement on the next line") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f =
         |    if p then try a catch case e => b; c
         |    d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    if (p) try a catch {
         |      case e => b
         |    }
         |    c
         |    d
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val stat = Term.If(tname("p"), Term.Try(tname("a"), List(handler), None), lit(), Nil)
    val body = blk(stat, tname("c"), tname("d"))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case at the top level of a source, a deeper line") {
    val code =
      """|def f = try a catch case e => b
         |  c
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    b
         |    c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, blk(tname("b"), tname("c")))
    val tree =
      Source(List(Defn.Def(Nil, tname("f"), Nil, None, Term.Try(tname("a"), List(handler), None))))
    runTestAssert[Source](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case after then, a semicolon and a term, the next definition") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = if p then try a catch case e => b; c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (p) try a catch {
         |    case e => b
         |  }
         |  c
         |  def g = 1
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = Term.If(tname("p"), Term.Try(tname("a"), List(handler), None), lit(), Nil)
    val f = Defn.Def(Nil, tname("f"), Nil, None, body)
    val tree = Defn
      .Object(Nil, tname("A"), tpl(f, tname("c"), Defn.Def(Nil, tname("g"), Nil, None, lit(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala 2: same-line catch case without braces in a lambda in braces, a deeper line") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|object A {
         |  def f = xs.map { x => try a catch case e => b
         |    c }
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map { x =>
         |    try a catch {
         |      case e => b
         |    }
         |    c
         |  }
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val lambda = tfunc(tparam("x"))(blk(Term.Try(tname("a"), List(handler), None), tname("c")))
    val body = tapply(tselect("xs", "map"), blk(lambda))
    val tree = Defn.Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case in a try body, a semicolon and a term, catch on the same line") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = try try a catch case e => b; c catch case g => d
         |""".stripMargin
    val error =
      """|<input>:2: error: `;` expected but `catch` found
         |  def f = try try a catch case e => b; c catch case g => d
         |                                         ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala3Future: same-line catch case in a try body, a semicolon and a term") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = try try a catch case e => b; c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try try a catch {
         |    case e => b
         |  }
         |  c
         |}
         |""".stripMargin
    val inner = Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None)
    val body = Term.Try(inner, Nil, None)
    val tree = Defn
      .Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: same-line catch case after do, a semicolon and a term") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object A:
         |  def f = while p do try a catch case e => b; c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while (p) try a catch {
         |    case e => b
         |  }
         |  c
         |}
         |""".stripMargin
    val handler = Case(patvar("e"), None, tname("b"))
    val body = Term.While(tname("p"), Term.Try(tname("a"), List(handler), None))
    val tree = Defn
      .Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, body), tname("c")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case as a statement, a semicolon and a term") {
    val code = "try a catch case e => b; c"
    val error =
      """|<input>:1: error: `end of file` expected but `identifier` found
         |try a catch case e => b; c
         |                         ^""".stripMargin
    runTestError[Stat](code, error)(stat(_), implicitly[munit.Location])
  }

  test("same-line catch case as a term, a semicolon and a term") {
    val code = "try a catch case e => b; c"
    val error =
      """|<input>:1: error: `end of file` expected but `;` found
         |try a catch case e => b; c
         |                       ^""".stripMargin
    runTestError[Term](code, error)(term(_), implicitly[munit.Location])
  }

}
