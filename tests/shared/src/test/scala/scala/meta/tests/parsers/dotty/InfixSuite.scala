package scala.meta.tests.parsers.dotty

import scala.meta._

class InfixSuite extends BaseDottySuite {

  test("simple-modifier") {
    runTestAssert[Stat](
      """|infix def a(param: Int) = param
         |""".stripMargin
    )(Defn.Def(
      List(Mod.Infix()),
      tname("a"),
      Nil,
      List(List(tparam("param", "Int"))),
      None,
      tname("param")
    ))
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
    )(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(
        Decl.Type(List(Mod.Infix()), pname("or"), List(pparam("X"), pparam("Y")), noBounds),
        Defn.Def(
          List(Mod.Infix()),
          tname("x"),
          Nil,
          List(List(tparam("a", "Int"))),
          Some(Type.ApplyInfix(pname("String"), pname("or"), pname("Int"))),
          int(1)
        )
      )
    ))

  }

  test("infix-class") {
    runTestAssert[Stat]("infix class A[B, C]")(
      Defn.Class(List(Mod.Infix()), pname("A"), List(pparam("B"), pparam("C")), ctor, tplNoBody())
    )
  }

  test("infix-trait") {
    runTestAssert[Stat]("infix trait A[B, C]")(
      Defn.Trait(List(Mod.Infix()), pname("A"), List(pparam("B"), pparam("C")), ctor, tplNoBody())
    )
  }

  test("infix-identifier") {
    runTestAssert[Stat]("infix def infix(infix: infix): infix = new infix {}")(Defn.Def(
      List(Mod.Infix()),
      tname("infix"),
      Nil,
      List(List(tparam("infix", "infix"))),
      Some(pname("infix")),
      Term.NewAnonymous(tpl(List(Init(pname("infix"), anon, emptyArgClause)), Nil))
    ))
  }

  test("extension-method") {
    runTestAssert[Stat]("extension (i: Int) infix def zero(other: Int): Int = 0")(
      Defn.ExtensionGroup(
        Nil,
        List(List(tparam("i", "Int"))),
        Defn.Def(
          List(Mod.Infix()),
          tname("zero"),
          Nil,
          List(List(tparam("other", "Int"))),
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
    )(Term.Apply(
      tname("Flow"),
      Term.Block(List(
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
      )) :: Nil
    ))
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
    )(Term.Apply(
      tname("Flow"),
      Term.Block(
        Defn.Def(
          Nil,
          tname("foo"),
          None,
          None,
          Term.Block(List(
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
          ))
        ) :: Nil
      ) :: Nil
    ))
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
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("str"))),
      None,
      Term.ApplyInfix(
        Term.ApplyInfix(str("hello"), tname("++"), Nil, List(str(" world"))),
        tname("++"),
        Nil,
        List(str("!"))
      )
    ))
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
    )(Defn.Def(
      Nil,
      tname("condition"),
      None,
      None,
      Term.ApplyInfix(
        Term.ApplyInfix(
          Term.ApplyInfix(tname("x"), tname(">"), Nil, List(int(0))),
          tname("||"),
          Nil,
          List(Term.Apply(
            Term.Select(tname("xs"), tname("exists")),
            List(Term.AnonymousFunction(
              Term.ApplyInfix(Term.Placeholder(), tname(">"), Nil, List(int(0)))
            ))
          ))
        ),
        tname("||"),
        Nil,
        List(Term.Select(tname("xs"), tname("isEmpty")))
      )
    ))
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
    )(Term.Block(List(Term.ApplyInfix(tname("freezing"), tname("|"), Nil, List(tname("boiling"))))))
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
    )(Term.Block(List(tname("freezing"), Term.Select(tname("|"), tname("boiling")))))
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
    )(Term.Block(List(Term.ApplyInfix(tname("freezing"), tname("|"), Nil, List(tname("boiling"))))))
  }

  test("scala3 infix syntax 3.4") {
    val code = """|{
                  |  freezing
                  |  |
                  |  boiling
                  |}
                  |""".stripMargin
    val layout = """|{
                    |  freezing | boiling
                    |}
                    |""".stripMargin
    val tree = blk(Term.ApplyInfix(tname("freezing"), tname("|"), Nil, List(tname("boiling"))))
    runTestAssert[Stat](code, Some(layout))(tree)
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

  test("scala3 infix syntax 3.6 comment") {
    val code = """|{
                  |  freezing /*
                  |    c1 */ // c2
                  |  |
                  |  boiling
                  |}
                  |""".stripMargin
    val layout = """|{
                    |  freezing | boiling
                    |}
                    |""".stripMargin
    val tree = blk(Term.ApplyInfix(tname("freezing"), tname("|"), Nil, List(tname("boiling"))))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scala3 infix syntax 3.7 comment") {
    val code = """|{
                  |  freezing
                  |  // c1
                  |  /*
                  |    c2
                  |  */ |
                  |  boiling
                  |}
                  |""".stripMargin
    val layout = """|{
                    |  freezing | boiling
                    |}
                    |""".stripMargin
    val tree = blk(Term.ApplyInfix(tname("freezing"), tname("|"), Nil, List(tname("boiling"))))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scala3 infix syntax 3.8 comment") {
    val code = """|{
                  |  freezing
                  |  // c1
                  |  /*
                  |    c2
                  |  */
                  |  |
                  |  boiling
                  |}
                  |""".stripMargin
    val layout = """|{
                    |  freezing | boiling
                    |}
                    |""".stripMargin
    val tree = blk(Term.ApplyInfix(tname("freezing"), tname("|"), Nil, List(tname("boiling"))))
    runTestAssert[Stat](code, Some(layout))(tree)
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
    )(Term.Block(List(tname("freezing"), Term.ApplyUnary(tname("!"), tname("boiling")))))
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
    )(Term.Block(List(
      Term.Apply(tname("println"), List(str("hello"))),
      tname("???"),
      Term.Match(tname("???"), List(Case(int(0), None, int(1))), Nil)
    )))
  }

  test("scala3 infix syntax 5.2") {
    val code = """|{
                  |  println("hello")
                  |    ???
                  |    ??? match {
                  |      case 0 => 1
                  |    }
                  |}
                  |""".stripMargin
    val layout = """|{
                    |  println("hello")
                    |  ???
                    |  ??? match {
                    |    case 0 => 1
                    |  }
                    |}
                    |""".stripMargin
    val tree = Term.Block(List(
      Term.Apply(tname("println"), List(str("hello"))),
      tname("???"),
      Term.Match(tname("???"), List(Case(int(0), None, int(1))), Nil)
    ))
    runTestAssert[Stat](code, layout)(tree)
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
    )(Term.Block(List(
      Defn.Def(
        Nil,
        tname("toBeContinued"),
        Nil,
        List(List(tparam("altToken", "Token"))),
        Some(pname("Boolean")),
        Term.Block(List(
          Defn.Def(
            List(Mod.Inline()),
            tname("canContinue"),
            None,
            None,
            Term.ApplyInfix(
              Term.ApplyUnary(
                tname("!"),
                Term.Apply(
                  Term
                    .Select(Term.Select(tname("in"), tname("canStartStatTokens")), tname("contains")),
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
        ))
      ),
      Term.EndMarker(tname("toBeContinued"))
    )))
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
    )(Defn.Val(
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
            Term
              .Apply(Term.Select(tname("Middleware"), tname("cors")), List(tname("corsConfig"))) ::
              Nil
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
    ))
  }

  test("case: break before `|`") {
    val code = """|case 'a'
                  |   | 'A' =>
                  |""".stripMargin
    val layout = "case 'a' | 'A' =>"
    val tree = Case(Pat.Alternative(Lit.Char('a'), Lit.Char('A')), None, Term.Block(Nil))
    runTestAssert[Case](code, Some(layout))(tree)
  }

  test("scalafmt #3720 different leading infix indentations") {
    val code = """
                 |object a:
                 |  def mtd =
                 |    abc(
                 |        arg1
                 |    )
                 |    ++
                 |    abc(arg2)
                 |      ++
                 |        abc(arg3)
                 |  ++
                 |    abc(arg4)
                 |""".stripMargin
    val layout = "object a { def mtd = abc(arg1) ++ abc(arg2) ++ abc(arg3) ++ abc(arg4) }"
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Defn.Def(
        Nil,
        tname("mtd"),
        Nil,
        None,
        Term.ApplyInfix(
          Term.ApplyInfix(
            Term.ApplyInfix(
              Term.Apply(tname("abc"), List(tname("arg1"))),
              tname("++"),
              Nil,
              List(Term.Apply(tname("abc"), List(tname("arg2"))))
            ),
            tname("++"),
            Nil,
            List(Term.Apply(tname("abc"), List(tname("arg3"))))
          ),
          tname("++"),
          Nil,
          List(Term.Apply(tname("abc"), List(tname("arg4"))))
        )
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3825 1") {
    val code = """|object a:
                  |  foo
                  |    .map: i =>
                  |      i + 1
                  |    *> bar
                  |""".stripMargin
    val layout = """|object a {
                    |  foo.map {
                    |    i => i + 1
                    |  } *> bar
                    |}
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Term.ApplyInfix(
        Term.Apply(
          Term.Select(tname("foo"), tname("map")),
          blk(
            Term
              .Function(List(tparam("i")), Term.ApplyInfix(tname("i"), tname("+"), Nil, List(int(1))))
          ) :: Nil
        ),
        tname("*>"),
        Nil,
        List(tname("bar"))
      ))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3825 2") {
    val code = """|object a:
                  |  object b:
                  |    foo
                  |      .map: i =>
                  |        i + 1
                  |          + 2
                  |        + 3
                  |      *> bar
                  |    baz
                  |  qux
                  |""".stripMargin
    val layout = """|object a {
                    |  object b {
                    |    foo.map {
                    |      i => i + 1 + 2 + 3
                    |    } *> bar
                    |    baz
                    |  }
                    |  qux
                    |}
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(
        Defn.Object(
          Nil,
          tname("b"),
          tpl(
            Term.ApplyInfix(
              Term.Apply(
                Term.Select(tname("foo"), tname("map")),
                blk(Term.Function(
                  List(tparam("i")),
                  Term.ApplyInfix(
                    Term.ApplyInfix(
                      Term.ApplyInfix(tname("i"), tname("+"), Nil, List(int(1))),
                      tname("+"),
                      Nil,
                      List(int(2))
                    ),
                    tname("+"),
                    Nil,
                    List(int(3))
                  )
                )) :: Nil
              ),
              tname("*>"),
              Nil,
              List(tname("bar"))
            ),
            tname("baz")
          )
        ),
        tname("qux")
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3948 1") {
    val code = """|object Hello {
                  |  buffer
                  |    += new Object()
                  |    += new Object
                  |}
                  |""".stripMargin
    val tokenized = """|BOF [0..0)
                       |KwObject [0..6)
                       |Ident(Hello) [7..12)
                       |LeftBrace [13..14)
                       |Ident(buffer) [17..23)
                       |InfixLF [23..24)
                       |Ident(+=) [28..30)
                       |KwNew [31..34)
                       |Ident(Object) [35..41)
                       |LeftParen [41..42)
                       |RightParen [42..43)
                       |InfixLF [43..44)
                       |Ident(+=) [48..50)
                       |KwNew [51..54)
                       |Ident(Object) [55..61)
                       |LF [61..62)
                       |RightBrace [62..63)
                       |EOF [64..64)
                       |""".stripMargin
    assertTokenizedAsStructureLines(code, tokenized)
    val layout = "object Hello { buffer += (new Object()) += (new Object) }"
    val tree = Defn.Object(
      Nil,
      tname("Hello"),
      tpl(Term.ApplyInfix(
        Term
          .ApplyInfix(tname("buffer"), tname("+="), Nil, List(Term.New(init("Object", List(Nil))))),
        tname("+="),
        Nil,
        List(Term.New(init("Object")))
      ))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3948 2") {
    val code = """|object Hello {
                  |  buffer
                  |    += new Object() with Foo {
                  |      def toString() = "foo"
                  |    }
                  |    += new Object
                  |}
                  |""".stripMargin
    val tokenized = """|BOF [0..0)
                       |KwObject [0..6)
                       |Ident(Hello) [7..12)
                       |LeftBrace [13..14)
                       |Ident(buffer) [17..23)
                       |InfixLF [23..24)
                       |Ident(+=) [28..30)
                       |KwNew [31..34)
                       |Ident(Object) [35..41)
                       |LeftParen [41..42)
                       |RightParen [42..43)
                       |KwWith [44..48)
                       |Ident(Foo) [49..52)
                       |LeftBrace [53..54)
                       |KwDef [61..64)
                       |Ident(toString) [65..73)
                       |LeftParen [73..74)
                       |RightParen [74..75)
                       |Equals [76..77)
                       |Constant.String(foo) [78..83)
                       |LF [83..84)
                       |RightBrace [88..89)
                       |InfixLF [89..90)
                       |Ident(+=) [94..96)
                       |KwNew [97..100)
                       |Ident(Object) [101..107)
                       |LF [107..108)
                       |RightBrace [108..109)
                       |EOF [110..110)
                       |""".stripMargin
    assertTokenizedAsStructureLines(code, tokenized)
    val layout =
      """object Hello { buffer += (new Object() with Foo { def toString() = "foo" }) += (new Object) }"""
    val tree = Defn.Object(
      Nil,
      tname("Hello"),
      tpl(Term.ApplyInfix(
        Term.ApplyInfix(
          tname("buffer"),
          tname("+="),
          Nil,
          List(Term.NewAnonymous(tpl(
            List(init("Object", List(Nil)), init("Foo")),
            List(Defn.Def(Nil, tname("toString"), Nil, List(Nil), None, lit("foo")))
          )))
        ),
        tname("+="),
        Nil,
        List(Term.New(init("Object")))
      ))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
