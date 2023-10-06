package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class InfixSuite extends BaseDottySuite {

  test("simple-modifier") {
    runTestAssert[Stat](
      """|infix def a(param: Int) = param
         |""".stripMargin
    )(
      Defn.Def(
        List(Mod.Infix()),
        tname("a"),
        Nil,
        List(List(Term.Param(Nil, tname("param"), Some(pname("Int")), None))),
        None,
        tname("param")
      )
    )
  }
  test("infix-type-complex") {
    runTestAssert[Stat](
      """|class A:
         |  infix type or[X, Y]
         |  infix def x(a : Int): String or Int = 1
         |""".stripMargin,
      assertLayout = Some(
        """|class A {
           |  infix type or[X, Y]
           |  infix def x(a: Int): String or Int = 1
           |}
           |""".stripMargin
      )
    )(
      Defn.Class(
        Nil,
        pname("A"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Decl.Type(
              List(Mod.Infix()),
              pname("or"),
              List(
                Type.Param(Nil, pname("X"), Nil, Type.Bounds(None, None), Nil, Nil),
                Type.Param(Nil, pname("Y"), Nil, Type.Bounds(None, None), Nil, Nil)
              ),
              Type.Bounds(None, None)
            ),
            Defn.Def(
              List(Mod.Infix()),
              tname("x"),
              Nil,
              List(List(Term.Param(Nil, tname("a"), Some(pname("Int")), None))),
              Some(Type.ApplyInfix(pname("String"), pname("or"), pname("Int"))),
              int(1)
            )
          )
        )
      )
    )

  }

  test("infix-class") {
    runTestAssert[Stat]("infix class A[B, C]")(
      Defn.Class(
        List(Mod.Infix()),
        pname("A"),
        List(
          pparam("B"),
          pparam("C")
        ),
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )
  }

  test("infix-trait") {
    runTestAssert[Stat]("infix trait A[B, C]")(
      Defn.Trait(
        List(Mod.Infix()),
        pname("A"),
        List(
          pparam("B"),
          pparam("C")
        ),
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )
  }

  test("infix-identifier") {
    runTestAssert[Stat]("infix def infix(infix: infix): infix = new infix {}")(
      Defn.Def(
        List(Mod.Infix()),
        tname("infix"),
        Nil,
        List(List(Term.Param(Nil, tname("infix"), Some(pname("infix")), None))),
        Some(pname("infix")),
        Term.NewAnonymous(
          Template(
            Nil,
            List(Init(pname("infix"), Name(""), emptyArgClause)),
            Self(Name(""), None),
            Nil
          )
        )
      )
    )
  }

  test("extension-method") {
    runTestAssert[Stat]("extension (i: Int) infix def zero(other: Int): Int = 0")(
      Defn.ExtensionGroup(
        Nil,
        List(List(Term.Param(Nil, tname("i"), Some(pname("Int")), None))),
        Defn.Def(
          List(Mod.Infix()),
          tname("zero"),
          Nil,
          List(List(Term.Param(Nil, tname("other"), Some(pname("Int")), None))),
          Some(pname("Int")),
          int(0)
        )
      )
    )
  }

  test("issue-2880 1") {
    runTestAssert[Stat](
      """|Flow {
         |    b.add()
         |
         |    input_< ~> filtering
         |      ~> removeItems.in0
         |    ~> removeItems
         |}
         |""".stripMargin,
      Some(
        """|Flow {
           |  b.add()
           |  input_< ~> filtering ~> removeItems.in0 ~> removeItems
           |}
           |""".stripMargin
      )
    )(
      Term.Apply(
        tname("Flow"),
        Term.Block(
          List(
            Term.Apply(Term.Select(tname("b"), tname("add")), Nil),
            Term.ApplyInfix(
              Term.ApplyInfix(
                Term.ApplyInfix(tname("input_<"), tname("~>"), Nil, List(tname("filtering"))),
                tname("~>"),
                Nil,
                List(Term.Select(tname("removeItems"), tname("in0")))
              ),
              tname("~>"),
              Nil,
              List(tname("removeItems"))
            )
          )
        ) :: Nil
      )
    )
  }

  test("issue-2880 2") {
    runTestAssert[Stat](
      """|Flow {
         |  def foo =
         |    b.add()
         |
         |    input_< ~> filtering
         |     ~> removeItems1
         |    ~> removeItems2
         |}
         |""".stripMargin,
      Some(
        """|Flow {
           |  def foo = {
           |    b.add()
           |    input_< ~> filtering ~> removeItems1 ~> removeItems2
           |  }
           |}
           |""".stripMargin
      )
    )(
      Term.Apply(
        tname("Flow"),
        Term.Block(
          Defn.Def(
            Nil,
            tname("foo"),
            None,
            None,
            Term.Block(
              List(
                Term.Apply(Term.Select(tname("b"), tname("add")), Nil),
                Term.ApplyInfix(
                  Term.ApplyInfix(
                    Term.ApplyInfix(tname("input_<"), tname("~>"), Nil, List(tname("filtering"))),
                    tname("~>"),
                    Nil,
                    List(tname("removeItems1"))
                  ),
                  tname("~>"),
                  Nil,
                  List(tname("removeItems2"))
                )
              )
            )
          ) :: Nil
        ) :: Nil
      )
    )
  }

  test("scala3 infix syntax 1") {
    runTestAssert[Stat](
      """|val str = "hello"
         |  ++ " world"
         |  ++ "!"
         |""".stripMargin,
      Some(
        """|val str = "hello" ++ " world" ++ "!"
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("str"))),
        None,
        Term.ApplyInfix(
          Term.ApplyInfix(Lit.String("hello"), tname("++"), Nil, List(Lit.String(" world"))),
          tname("++"),
          Nil,
          List(Lit.String("!"))
        )
      )
    )
  }

  test("scala3 infix syntax 2") {
    runTestAssert[Stat](
      """|def condition =
         |  x > 0
         |  ||
         |  xs.exists(_ > 0)
         |  || xs.isEmpty
         |""".stripMargin,
      Some(
        """|def condition = x > 0 || xs.exists(_ > 0) || xs.isEmpty
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        tname("condition"),
        None,
        None,
        Term.ApplyInfix(
          Term.ApplyInfix(
            Term.ApplyInfix(
              tname("x"),
              tname(">"),
              Nil,
              List(int(0))
            ),
            tname("||"),
            Nil,
            List(
              Term.Apply(
                Term.Select(tname("xs"), tname("exists")),
                List(
                  Term.AnonymousFunction(
                    Term.ApplyInfix(
                      Term.Placeholder(),
                      tname(">"),
                      Nil,
                      List(int(0))
                    )
                  )
                )
              )
            )
          ),
          tname("||"),
          Nil,
          List(Term.Select(tname("xs"), tname("isEmpty")))
        )
      )
    )
  }

  test("scala3 infix syntax 3.1") {
    runTestAssert[Stat](
      """|{
         |  freezing
         |    | boiling
         |}
         |""".stripMargin,
      Some(
        """|{
           |  freezing | boiling
           |}
           |""".stripMargin
      )
    )(
      Term.Block(List(Term.ApplyInfix(tname("freezing"), tname("|"), Nil, List(tname("boiling")))))
    )
  }

  test("scala3 infix syntax 3.2") {
    runTestAssert[Stat](
      """|{
         |  freezing
         |
         |    | boiling
         |}
         |""".stripMargin,
      Some(
        """|{
           |  freezing
           |  |.boiling
           |}
           |""".stripMargin
      )
    )(
      Term.Block(List(tname("freezing"), Term.Select(tname("|"), tname("boiling"))))
    )
  }

  test("scala3 infix syntax 3.3") {
    runTestAssert[Stat](
      """|{
         |  freezing
         |    |
         |    boiling
         |}
         |""".stripMargin,
      Some(
        """|{
           |  freezing | boiling
           |}
           |""".stripMargin
      )
    )(
      Term.Block(List(Term.ApplyInfix(tname("freezing"), tname("|"), Nil, List(tname("boiling")))))
    )
  }

  test("scala3 infix syntax 3.4") {
    runTestAssert[Stat](
      """|{
         |  freezing
         |  |
         |  boiling
         |}
         |""".stripMargin,
      Some(
        """|{
           |  freezing | boiling
           |}
           |""".stripMargin
      )
    )(
      Term.Block(List(Term.ApplyInfix(tname("freezing"), tname("|"), Nil, List(tname("boiling")))))
    )
  }

  test("scala3 infix syntax 3.5") {
    runTestError[Stat](
      """|{
         |  freezing
         |    |
         |  boiling
         |}
         |""".stripMargin,
      """|error: Invalid indented leading infix operator found
         |    |
         |    ^""".stripMargin
    )
  }

  test("scala3 infix syntax 4") {
    runTestAssert[Stat](
      """|{
         |  freezing
         |    !boiling
         |}
         |""".stripMargin,
      Some(
        """|{
           |  freezing
           |  !boiling
           |}
           |""".stripMargin
      )
    )(
      Term.Block(List(tname("freezing"), Term.ApplyUnary(tname("!"), tname("boiling"))))
    )
  }

  test("scala3 infix syntax 5.1") {
    runTestAssert[Stat](
      """|{
         |  println("hello")
         |  ???
         |  ??? match {
         |    case 0 => 1
         |  }
         |}
         |""".stripMargin,
      Some(
        """|{
           |  println("hello")
           |  ???
           |  ??? match {
           |    case 0 => 1
           |  }
           |}
           |""".stripMargin
      )
    )(
      Term.Block(
        List(
          Term.Apply(tname("println"), List(str("hello"))),
          tname("???"),
          Term.Match(
            tname("???"),
            List(Case(int(0), None, int(1))),
            Nil
          )
        )
      )
    )
  }

  test("scala3 infix syntax 5.2") {
    runTestError[Stat](
      """|{
         |  println("hello")
         |    ???
         |    ??? match {
         |      case 0 => 1
         |    }
         |}
         |""".stripMargin,
      """|error: Invalid indented leading infix operator found
         |    ???
         |    ^""".stripMargin
    )
  }

  test("scala3 infix syntax 6") {
    runTestAssert[Stat](
      """|{
         |  def toBeContinued(altToken: Token): Boolean =
         |    inline def canContinue =
         |      !in.canStartStatTokens.contains(in.token)  // not statement, so take as continued expr
         |    || followedByToken(altToken)                 // scan ahead to see whether we find a `then` or `do`
         |
         |    !in.isNewLine       // a newline token means the expression is finished
         |    && !migrateTo3      // old syntax
         |    && canContinue
         |  end toBeContinued
         |}
         |""".stripMargin,
      Some(
        """|{
           |  def toBeContinued(altToken: Token): Boolean = {
           |    inline def canContinue = !in.canStartStatTokens.contains(in.token) || followedByToken(altToken)
           |    !in.isNewLine && !migrateTo3 && canContinue
           |  }
           |  end toBeContinued
           |}
           |""".stripMargin
      )
    )(
      Term.Block(
        List(
          Defn.Def(
            Nil,
            tname("toBeContinued"),
            Nil,
            List(List(tparam("altToken", "Token"))),
            Some(pname("Boolean")),
            Term.Block(
              List(
                Defn.Def(
                  List(Mod.Inline()),
                  tname("canContinue"),
                  None,
                  None,
                  Term.ApplyInfix(
                    Term.ApplyUnary(
                      tname("!"),
                      Term.Apply(
                        Term.Select(
                          Term.Select(tname("in"), tname("canStartStatTokens")),
                          tname("contains")
                        ),
                        List(Term.Select(tname("in"), tname("token")))
                      )
                    ),
                    tname("||"),
                    Nil,
                    List(Term.Apply(tname("followedByToken"), List(tname("altToken"))))
                  )
                ),
                Term.ApplyInfix(
                  Term.ApplyInfix(
                    Term.ApplyUnary(tname("!"), Term.Select(tname("in"), tname("isNewLine"))),
                    tname("&&"),
                    Nil,
                    List(Term.ApplyUnary(tname("!"), tname("migrateTo3")))
                  ),
                  tname("&&"),
                  Nil,
                  List(tname("canContinue"))
                )
              )
            )
          ),
          Term.EndMarker(tname("toBeContinued"))
        )
      )
    )
  }

  test("#3051 scala3 leading infix syntax") {
    runTestAssert[Stat](
      """|val httpRoutes2 = (MetricsApp() ++ HomeApp() ++ GreetingApp())
         |      @@ Middleware.cors(corsConfig)
         |      @@ Middleware.metrics(MetricsApp.pathLabelMapper)
         |      @@ Middleware.debug
         |""".stripMargin,
      Some(
        """|val httpRoutes2 = (MetricsApp() ++ HomeApp() ++ GreetingApp()) @@ Middleware.cors(corsConfig) @@ Middleware.metrics(MetricsApp.pathLabelMapper) @@ Middleware.debug
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("httpRoutes2"))),
        None,
        Term.ApplyInfix(
          Term.ApplyInfix(
            Term.ApplyInfix(
              Term.ApplyInfix(
                Term.ApplyInfix(
                  Term.Apply(tname("MetricsApp"), Nil),
                  tname("++"),
                  Nil,
                  List(Term.Apply(tname("HomeApp"), Nil))
                ),
                tname("++"),
                Nil,
                List(Term.Apply(tname("GreetingApp"), Nil))
              ),
              tname("@@"),
              Nil,
              Term.Apply(
                Term.Select(tname("Middleware"), tname("cors")),
                List(tname("corsConfig"))
              ) :: Nil
            ),
            tname("@@"),
            Nil,
            Term.Apply(
              Term.Select(tname("Middleware"), tname("metrics")),
              List(Term.Select(tname("MetricsApp"), tname("pathLabelMapper")))
            ) :: Nil
          ),
          tname("@@"),
          Nil,
          List(Term.Select(tname("Middleware"), tname("debug")))
        )
      )
    )
  }

}
