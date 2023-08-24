package scala.meta.tests.parsers.dotty

import scala.meta._

class FewerBracesSuite extends BaseDottySuite {

  test("simple") {
    runTestAssert[Stat](
      """|val firstLine = files.get(fileName).fold:
         |    val fileNames = files.values
         |    filenames
         |
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.get(fileName).fold {
           |  val fileNames = files.values
           |  filenames
           |}
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("firstLine"))),
        None,
        Term.Apply(
          Term.Select(
            Term.Apply(
              Term.Select(Term.Name("files"), Term.Name("get")),
              List(Term.Name("fileName"))
            ),
            Term.Name("fold")
          ),
          List(
            Term.Block(
              List(
                Defn.Val(
                  Nil,
                  List(Pat.Var(Term.Name("fileNames"))),
                  None,
                  Term.Select(Term.Name("files"), Term.Name("values"))
                ),
                Term.Name("filenames")
              )
            )
          )
        )
      )
    )
  }
  test("simple-comment") {
    runTestAssert[Stat](
      """|val firstLine = map: /*
         |    line
         |    line */
         |   indentedCode
         |
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = map(indentedCode)
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("firstLine"))),
        None,
        Term.Apply(Term.Name("map"), List(Term.Name("indentedCode")))
      )
    )
  }

  test("simple-same-line") {
    runTestAssert[Stat](
      """|val firstLine = files.map: a =>
         |    a
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.map(a => a)
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("firstLine"))),
        None,
        Term.Apply(
          Term.Select(Term.Name("files"), Term.Name("map")),
          List(Term.Function(List(Term.Param(Nil, Term.Name("a"), None, None)), Term.Name("a")))
        )
      )
    )
  }

  test("advanced-same-line") {
    runTestAssert[Stat](
      """|val firstLine = files.map: (a, b) =>
         |    a
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.map((a, b) => a)
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("firstLine"))),
        None,
        Term.Apply(
          Term.Select(Term.Name("files"), Term.Name("map")),
          List(
            Term.Function(
              List(
                Term.Param(Nil, Term.Name("a"), None, None),
                Term.Param(Nil, Term.Name("b"), None, None)
              ),
              Term.Name("a")
            )
          )
        )
      )
    )
  }

  test("advanced-same-line-case") {
    runTestAssert[Stat](
      """|val firstLine = files.map: 
         |  case (a, b) =>
         |    a
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.map({
           |  case (a, b) => a
           |})
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("firstLine"))),
        None,
        Term.Apply(
          Term.Select(Term.Name("files"), Term.Name("map")),
          List(
            Term.PartialFunction(
              List(
                Case(
                  Pat.Tuple(List(Pat.Var(Term.Name("a")), Pat.Var(Term.Name("b")))),
                  None,
                  Term.Name("a")
                )
              )
            )
          )
        )
      )
    )
  }

  test("multi-case") {
    runTestAssert[Stat](
      """|def main =
         |  val firstLine = files.map: 
         |    case A(a) =>
         |      a         
         |    case B(b) =>
         |      b
         |  def hello = ???
         |""".stripMargin,
      assertLayout = Some(
        """|def main = {
           |  val firstLine = files.map({
           |    case A(a) => a
           |    case B(b) => b
           |  })
           |  def hello = ???
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("main"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Defn.Val(
              Nil,
              List(Pat.Var(Term.Name("firstLine"))),
              None,
              Term.Apply(
                Term.Select(Term.Name("files"), Term.Name("map")),
                List(
                  Term.PartialFunction(
                    List(
                      Case(
                        Pat.Extract(Term.Name("A"), List(Pat.Var(Term.Name("a")))),
                        None,
                        Term.Name("a")
                      ),
                      Case(
                        Pat.Extract(Term.Name("B"), List(Pat.Var(Term.Name("b")))),
                        None,
                        Term.Name("b")
                      )
                    )
                  )
                )
              )
            ),
            Defn.Def(Nil, Term.Name("hello"), Nil, Nil, None, Term.Name("???"))
          )
        )
      )
    )
  }

  test("multiple") {
    runTestAssert[Stat](
      """|def O =
         |  val firstLine = files.fold:
         |    123
         |  val secondLine = files.fold:
         |    123
         |
         |""".stripMargin,
      assertLayout = Some(
        """|def O = {
           |  val firstLine = files.fold(123)
           |  val secondLine = files.fold(123)
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("O"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Defn.Val(
              Nil,
              List(Pat.Var(Term.Name("firstLine"))),
              None,
              Term.Apply(Term.Select(Term.Name("files"), Term.Name("fold")), List(Lit.Int(123)))
            ),
            Defn.Val(
              Nil,
              List(Pat.Var(Term.Name("secondLine"))),
              None,
              Term.Apply(Term.Select(Term.Name("files"), Term.Name("fold")), List(Lit.Int(123)))
            )
          )
        )
      )
    )
  }

  test("infix") {
    runTestAssert[Stat](
      """|def O =
         |  credentials `++`:
         |    val file = Path.userHome / ".credentials"
         |    file
         |""".stripMargin,
      assertLayout = Some(
        """|def O = credentials ++ {
           |  val file = Path.userHome / ".credentials"
           |  file
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        tname("O"),
        Nil,
        None,
        Term.ApplyInfix(
          tname("credentials"),
          tname("++"),
          Nil,
          Term.Block(
            List(
              Defn.Val(
                Nil,
                List(Pat.Var(tname("file"))),
                None,
                Term.ApplyInfix(
                  Term.Select(tname("Path"), tname("userHome")),
                  tname("/"),
                  Nil,
                  List(str(".credentials"))
                )
              ),
              tname("file")
            )
          ) :: Nil
        )
      )
    )
  }

  test("multiple-apply") {
    runTestAssert[Stat](
      """|def O =
         |  val firstLine = files.fold:
         |    123
         |  .apply: 
         |   (a,b) =>
         |      a
         |
         |""".stripMargin,
      assertLayout = Some(
        """|def O = {
           |  val firstLine = files.fold(123).apply((a, b) => a)
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("O"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Defn.Val(
              Nil,
              List(Pat.Var(Term.Name("firstLine"))),
              None,
              Term.Apply(
                Term.Select(
                  Term
                    .Apply(Term.Select(Term.Name("files"), Term.Name("fold")), List(Lit.Int(123))),
                  Term.Name("apply")
                ),
                List(
                  Term.Function(
                    List(
                      Term.Param(Nil, Term.Name("a"), None, None),
                      Term.Param(Nil, Term.Name("b"), None, None)
                    ),
                    Term.Name("a")
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("nested") {
    runTestAssert[Stat](
      """|def O =
         |  List((1, (List(""), 3))).map: (a: (Int, (List[String], Int))) =>
         |    a._1 + 1
         |""".stripMargin,
      assertLayout = Some(
        """|def O = List((1, (List(""), 3))).map((a: (Int, (List[String], Int))) => a._1 + 1)
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("O"),
        Nil,
        Nil,
        None,
        Term.Apply(
          Term.Select(
            Term.Apply(
              Term.Name("List"),
              List(
                Term.Tuple(
                  List(
                    Lit.Int(1),
                    Term.Tuple(
                      List(Term.Apply(Term.Name("List"), List(Lit.String(""))), Lit.Int(3))
                    )
                  )
                )
              )
            ),
            Term.Name("map")
          ),
          List(
            Term.Function(
              List(
                Term.Param(
                  Nil,
                  Term.Name("a"),
                  Some(
                    Type.Tuple(
                      List(
                        Type.Name("Int"),
                        Type.Tuple(
                          List(
                            Type.Apply(Type.Name("List"), List(Type.Name("String"))),
                            Type.Name("Int")
                          )
                        )
                      )
                    )
                  ),
                  None
                )
              ),
              Term.ApplyInfix(
                Term.Select(Term.Name("a"), Term.Name("_1")),
                Term.Name("+"),
                Nil,
                List(Lit.Int(1))
              )
            )
          )
        )
      )
    )
  }

  test("chain") {
    runTestAssert[Stat](
      """|val a: Int = xs
         |    .map: x =>
         |      x * x
         |    .filter: (y: Int) =>
         |      y > 0
         |    (0)
         |""".stripMargin,
      assertLayout = Some(
        """|val a: Int = xs.map(x => x * x).filter((y: Int) => y > 0)(0)
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("a"))),
        Some(Type.Name("Int")),
        Term.Apply(
          Term.Apply(
            Term.Select(
              Term.Apply(
                Term.Select(Term.Name("xs"), Term.Name("map")),
                List(
                  Term.Function(
                    List(Term.Param(Nil, Term.Name("x"), None, None)),
                    Term.ApplyInfix(Term.Name("x"), Term.Name("*"), Nil, List(Term.Name("x")))
                  )
                )
              ),
              Term.Name("filter")
            ),
            List(
              Term.Function(
                List(Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)),
                Term.ApplyInfix(Term.Name("y"), Term.Name(">"), Nil, List(Lit.Int(0)))
              )
            )
          ),
          List(Lit.Int(0))
        )
      )
    )
  }

  test("no-self-type") {
    runTestAssert[Stat](
      """|class C:
         |  f:
         |    22
         |""".stripMargin,
      assertLayout = Some(
        """|class C { f(22) }
           |""".stripMargin
      )
    )(
      Defn.Class(
        Nil,
        Type.Name("C"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Term.Apply(Term.Name("f"), List(Lit.Int(22)))),
          Nil
        )
      )
    )
  }

  test("no-indent") {
    runTestAssert[Stat](
      """| xs.map:
         |  x =>
         |  x
         | .filter:
         |  x =>
         |  x
         |""".stripMargin,
      assertLayout = Some(
        """|xs.map(x => x).filter(x => x)
           |""".stripMargin
      )
    )(
      Term.Apply(
        Term.Select(
          Term.Apply(
            Term.Select(Term.Name("xs"), Term.Name("map")),
            List(Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x")))
          ),
          Term.Name("filter")
        ),
        List(Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x")))
      )
    )
  }

  test("if-braceless") {
    runTestAssert[Stat](
      """|if
         |    arr.isEmpty
         |    || locally:
         |      val first = arr(0)
         |      first != 1
         |  then println("invalid arr")
         |""".stripMargin,
      assertLayout = Some(
        """|if (arr.isEmpty || locally {
           |  val first = arr(0)
           |  first != 1
           |}) println("invalid arr")
           |""".stripMargin
      )
    )(
      Term.If(
        Term.ApplyInfix(
          Term.Select(Term.Name("arr"), Term.Name("isEmpty")),
          Term.Name("||"),
          Nil,
          List(
            Term.Apply(
              Term.Name("locally"),
              List(
                Term.Block(
                  List(
                    Defn.Val(
                      Nil,
                      List(Pat.Var(Term.Name("first"))),
                      None,
                      Term.Apply(Term.Name("arr"), List(Lit.Int(0)))
                    ),
                    Term.ApplyInfix(Term.Name("first"), Term.Name("!="), Nil, List(Lit.Int(1)))
                  )
                )
              )
            )
          )
        ),
        Term.Apply(Term.Name("println"), List(Lit.String("invalid arr"))),
        Lit.Unit(),
        Nil
      )
    )
  }
  // Cannot chain any braceless lambdas without `.`
  test("chain-infix") {
    runTestAssert[Stat](
      """|def test24 =
         |  x < y or
         |    x > y
         |  `or`:
         |    x == y
         |""".stripMargin,
      assertLayout = Some(
        """|def test24 = {
           |  x < y or x > y
           |  or(x == y)
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("test24"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Term.ApplyInfix(
              Term.ApplyInfix(Term.Name("x"), Term.Name("<"), Nil, List(Term.Name("y"))),
              Term.Name("or"),
              Nil,
              List(Term.ApplyInfix(Term.Name("x"), Term.Name(">"), Nil, List(Term.Name("y"))))
            ),
            Term.Apply(
              Term.Name("or"),
              List(Term.ApplyInfix(Term.Name("x"), Term.Name("=="), Nil, List(Term.Name("y"))))
            )
          )
        )
      )
    )
  }

  // Infix chains don't allow to continue with a .
  test("chain-infix-error") {
    runTestError[Stat](
      """|val a: Int = xs map: x =>
         |      x * x
         |    .filter: (y: Int) =>
         |      y > 0
         |    (0)
         |""".stripMargin,
      "error: ; expected but . found"
    )
  }

  test("#3164 empty argument 1") {
    runTestError[Stat](
      """|object a:
         |  test("foo"):
         |""".stripMargin,
      """|<input>:2: error: expected fewer-braces method body
         |  test("foo"):
         |             ^""".stripMargin
    )
  }

  test("#3164 empty argument 2") {
    runTestError[Stat](
      """|object a:
         |  foo :
         |  bar
         |""".stripMargin,
      """|<input>:2: error: expected fewer-braces method body
         |  foo :
         |      ^""".stripMargin
    )
  }

  test("#3164 empty argument 3") {
    runTestAssert[Stat](
      """|object a:
         |  foo :
         |    bar
         |""".stripMargin,
      Some("object a { foo(bar) }")
    )(
      Defn.Object(
        Nil,
        tname("a"),
        Template(Nil, Nil, slf, List(Term.Apply(tname("foo"), List(tname("bar")))), Nil)
      )
    )
  }

  test("#3164 empty argument 4") {
    runTestAssert[Stat](
      """|object a:
         |  foo
         |  : bar
         |""".stripMargin,
      Some("object a { foo: bar }")
    )(
      Defn.Object(
        Nil,
        tname("a"),
        Template(Nil, Nil, slf, List(Term.Ascribe(tname("foo"), pname("bar"))), Nil)
      )
    )
  }

  test("#3296") {
    val code =
      """object MyApp:
        |
        |  def test(str: String)(block: => Boolean): Unit =
        |    println(str + " : " + block)
        |
        |  test("First test"):
        |    case class Foo(x: Int, y: String)
        |    1 == 1
        |
        |  test("Second test"):
        |    1 == 1
        |""".stripMargin
    val layout =
      """|object MyApp {
         |  def test(str: String)(block: => Boolean): Unit = println(str + " : " + block)
         |  test("First test") {
         |    case class Foo(x: Int, y: String)
         |    1 == 1
         |  }
         |  test("Second test")(1 == 1)
         |}
         |""".stripMargin
    assertNoDiff(parseStat(code, dialect).reprint, layout)
  }

  test("#3319 lambda complex lhs") {
    val code =
      """object MyApp:
        |  List(1,2,3) foreach: x =>
        |    println(x)
        |""".stripMargin
    val layout = "object MyApp { List(1, 2, 3) foreach (x => println(x)) }"
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(
        Term.ApplyInfix(
          Term.Apply(tname("List"), List(int(1), int(2), int(3))),
          tname("foreach"),
          Nil,
          Term.Function(
            List(tparam("x", None)),
            Term.Apply(tname("println"), List(tname("x")))
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 lambda simple lhs") {
    val code =
      """object MyApp:
        |  ids foreach: x =>
        |    println(x)
        |""".stripMargin
    val layout = "object MyApp { ids foreach (x => println(x)) }"
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(
        Term.ApplyInfix(
          tname("ids"),
          tname("foreach"),
          Nil,
          Term.Function(
            List(tparam("x", None)),
            Term.Apply(tname("println"), List(tname("x")))
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 lambda 'chained'") {
    val code =
      """object MyApp:
        |  ids map: x =>
        |      foo(x)
        |    map: x =>
        |      bar(x)
        |""".stripMargin
    val layout =
      """|object MyApp {
         |  ids map (x => foo(x))
         |  map(x => bar(x))
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(
        List(
          Term.ApplyInfix(
            tname("ids"),
            tname("map"),
            Nil,
            Term.Function(
              List(tparam("x", None)),
              Term.Apply(tname("foo"), List(tname("x")))
            ) :: Nil
          ),
          Term.Apply(
            tname("map"),
            Term.Function(
              List(tparam("x", None)),
              Term.Apply(tname("bar"), List(tname("x")))
            ) :: Nil
          )
        )
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 lambda block") {
    val code =
      """object MyApp:
        |  ids map: x =>
        |      foo(x)
        |      bar(x)
        |""".stripMargin
    val layout =
      """|object MyApp {
         |  ids map (x => {
         |    foo(x)
         |    bar(x)
         |  })
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(
        Term.ApplyInfix(
          tname("ids"),
          tname("map"),
          Nil,
          Term.Function(
            List(tparam("x", None)),
            Term.Block(
              List(
                Term.Apply(tname("foo"), List(tname("x"))),
                Term.Apply(tname("bar"), List(tname("x")))
              )
            )
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 function complex lhs") {
    val code =
      """object MyApp:
        |  List(1,2,3) foreach:
        |    println
        |""".stripMargin
    val layout = "object MyApp { List(1, 2, 3) foreach println }"
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(
        Term.ApplyInfix(
          Term.Apply(tname("List"), List(int(1), int(2), int(3))),
          tname("foreach"),
          Nil,
          List(tname("println"))
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 function simple lhs") {
    val code =
      """object MyApp:
        |  ids foreach:
        |    println
        |""".stripMargin
    val layout = "object MyApp { ids foreach println }"
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(
        Term.ApplyInfix(
          tname("ids"),
          tname("foreach"),
          Nil,
          List(tname("println"))
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 function 'chained'") {
    val code =
      """object MyApp:
        |  ids map:
        |      foo
        |    map:
        |      bar
        |""".stripMargin
    val layout =
      """|object MyApp {
         |  ids map foo
         |  map(bar)
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(
        List(
          Term.ApplyInfix(tname("ids"), tname("map"), Nil, List(tname("foo"))),
          Term.Apply(tname("map"), List(tname("bar")))
        )
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

}
