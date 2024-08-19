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
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("firstLine"))),
      None,
      Term.Apply(
        Term.Select(
          Term.Apply(Term.Select(tname("files"), tname("get")), List(tname("fileName"))),
          tname("fold")
        ),
        List(Term.Block(List(
          Defn.Val(
            Nil,
            List(Pat.Var(tname("fileNames"))),
            None,
            Term.Select(tname("files"), tname("values"))
          ),
          tname("filenames")
        )))
      )
    ))
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
        """|val firstLine = map {
           |  indentedCode
           |}
           |""".stripMargin
      )
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("firstLine"))),
      None,
      Term.Apply(tname("map"), Term.Block(List(tname("indentedCode"))) :: Nil)
    ))
  }

  test("simple-same-line") {
    runTestAssert[Stat](
      """|val firstLine = files.map: a =>
         |    a
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.map {
           |  a => a
           |}
           |""".stripMargin
      )
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("firstLine"))),
      None,
      Term.Apply(
        Term.Select(tname("files"), tname("map")),
        Term.Block(List(Term.Function(List(tparam("a")), tname("a")))) :: Nil
      )
    ))
  }

  test("advanced-same-line") {
    runTestAssert[Stat](
      """|val firstLine = files.map: (a, b) =>
         |    a
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.map {
           |  (a, b) => a
           |}
           |""".stripMargin
      )
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("firstLine"))),
      None,
      Term.Apply(
        Term.Select(tname("files"), tname("map")),
        Term.Block(List(Term.Function(List(tparam("a"), tparam("b")), tname("a")))) :: Nil
      )
    ))
  }

  test("advanced-same-line-case") {
    runTestAssert[Stat](
      """|val firstLine = files.map: 
         |  case (a, b) =>
         |    a
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.map {
           |  case (a, b) => a
           |}
           |""".stripMargin
      )
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("firstLine"))),
      None,
      Term.Apply(
        Term.Select(tname("files"), tname("map")),
        List(Term.PartialFunction(List(
          Case(Pat.Tuple(List(Pat.Var(tname("a")), Pat.Var(tname("b")))), None, tname("a"))
        )))
      )
    ))
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
           |  val firstLine = files.map {
           |    case A(a) => a
           |    case B(b) => b
           |  }
           |  def hello = ???
           |}
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("main"),
      Nil,
      Nil,
      None,
      Term.Block(List(
        Defn.Val(
          Nil,
          List(Pat.Var(tname("firstLine"))),
          None,
          Term.Apply(
            Term.Select(tname("files"), tname("map")),
            List(Term.PartialFunction(List(
              Case(Pat.Extract(tname("A"), List(Pat.Var(tname("a")))), None, tname("a")),
              Case(Pat.Extract(tname("B"), List(Pat.Var(tname("b")))), None, tname("b"))
            )))
          )
        ),
        Defn.Def(Nil, tname("hello"), Nil, Nil, None, tname("???"))
      ))
    ))
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
           |  val firstLine = files.fold {
           |    123
           |  }
           |  val secondLine = files.fold {
           |    123
           |  }
           |}
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("O"),
      Nil,
      Nil,
      None,
      Term.Block(List(
        Defn.Val(
          Nil,
          List(Pat.Var(tname("firstLine"))),
          None,
          Term.Apply(Term.Select(tname("files"), tname("fold")), Term.Block(List(int(123))) :: Nil)
        ),
        Defn.Val(
          Nil,
          List(Pat.Var(tname("secondLine"))),
          None,
          Term.Apply(Term.Select(tname("files"), tname("fold")), Term.Block(List(int(123))) :: Nil)
        )
      ))
    ))
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
    )(Defn.Def(
      Nil,
      tname("O"),
      Nil,
      None,
      Term.ApplyInfix(
        tname("credentials"),
        tname("++"),
        Nil,
        Term.Block(List(
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
        )) :: Nil
      )
    ))
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
           |  val firstLine = files.fold {
           |    123
           |  }.apply {
           |    (a, b) => a
           |  }
           |}
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("O"),
      Nil,
      Nil,
      None,
      Term.Block(
        Defn.Val(
          Nil,
          List(Pat.Var(tname("firstLine"))),
          None,
          Term.Apply(
            Term.Select(
              Term
                .Apply(Term.Select(tname("files"), tname("fold")), Term.Block(List(int(123))) :: Nil),
              tname("apply")
            ),
            Term.Block(Term.Function(List(tparam("a"), tparam("b")), tname("a")) :: Nil) :: Nil
          )
        ) :: Nil
      )
    ))
  }

  test("nested") {
    runTestAssert[Stat](
      """|def O =
         |  List((1, (List(""), 3))).map: (a: (Int, (List[String], Int))) =>
         |    a._1 + 1
         |""".stripMargin,
      assertLayout = Some(
        """|def O = List((1, (List(""), 3))).map {
           |  (a: (Int, (List[String], Int))) => a._1 + 1
           |}
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("O"),
      Nil,
      Nil,
      None,
      Term.Apply(
        Term.Select(
          Term.Apply(
            tname("List"),
            Term.Tuple(
              List(int(1), Term.Tuple(List(Term.Apply(tname("List"), List(str(""))), int(3))))
            ) :: Nil
          ),
          tname("map")
        ),
        Term.Block(
          Term.Function(
            tparam(
              Nil,
              "a",
              Type.Tuple(List(
                pname("Int"),
                Type.Tuple(List(Type.Apply(pname("List"), List(pname("String"))), pname("Int")))
              ))
            ) :: Nil,
            Term.ApplyInfix(Term.Select(tname("a"), tname("_1")), tname("+"), Nil, List(int(1)))
          ) :: Nil
        ) :: Nil
      )
    ))
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
        """|val a: Int = xs.map {
           |  x => x * x
           |}.filter {
           |  (y: Int) => y > 0
           |}(0)
           |""".stripMargin
      )
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("a"))),
      Some(pname("Int")),
      Term.Apply(
        Term.Apply(
          Term.Select(
            Term.Apply(
              Term.Select(tname("xs"), tname("map")),
              Term.Block(
                Term.Function(
                  List(tparam("x")),
                  Term.ApplyInfix(tname("x"), tname("*"), Nil, List(tname("x")))
                ) :: Nil
              ) :: Nil
            ),
            tname("filter")
          ),
          Term.Block(
            Term.Function(
              List(tparam("y", "Int")),
              Term.ApplyInfix(tname("y"), tname(">"), Nil, List(int(0)))
            ) :: Nil
          ) :: Nil
        ),
        List(int(0))
      )
    ))
  }

  test("no-self-type") {
    runTestAssert[Stat](
      """|class C:
         |  f:
         |    22
         |""".stripMargin,
      assertLayout = Some(
        """|class C {
           |  f {
           |    22
           |  }
           |}
           |""".stripMargin
      )
    )(Defn.Class(
      Nil,
      pname("C"),
      Nil,
      EmptyCtor(),
      tpl(Term.Apply(tname("f"), Term.Block(List(int(22))) :: Nil))
    ))
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
        """|xs.map {
           |  x => x
           |}.filter {
           |  x => x
           |}
           |""".stripMargin
      )
    )(Term.Apply(
      Term.Select(
        Term.Apply(
          Term.Select(tname("xs"), tname("map")),
          Term.Block(List(Term.Function(List(tparam("x")), tname("x")))) :: Nil
        ),
        tname("filter")
      ),
      Term.Block(List(Term.Function(List(tparam("x")), tname("x")))) :: Nil
    ))
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
    )(Term.If(
      Term.ApplyInfix(
        Term.Select(tname("arr"), tname("isEmpty")),
        tname("||"),
        Nil,
        List(Term.Apply(
          tname("locally"),
          List(Term.Block(List(
            Defn
              .Val(Nil, List(Pat.Var(tname("first"))), None, Term.Apply(tname("arr"), List(int(0)))),
            Term.ApplyInfix(tname("first"), tname("!="), Nil, List(int(1)))
          )))
        ))
      ),
      Term.Apply(tname("println"), List(str("invalid arr"))),
      Lit.Unit(),
      Nil
    ))
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
           |  or {
           |    x == y
           |  }
           |}
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("test24"),
      Nil,
      Nil,
      None,
      Term.Block(List(
        Term.ApplyInfix(
          Term.ApplyInfix(tname("x"), tname("<"), Nil, List(tname("y"))),
          tname("or"),
          Nil,
          List(Term.ApplyInfix(tname("x"), tname(">"), Nil, List(tname("y"))))
        ),
        Term.Apply(
          tname("or"),
          Term.Block(List(Term.ApplyInfix(tname("x"), tname("=="), Nil, List(tname("y"))))) :: Nil
        )
      ))
    ))
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
      """|<input>:3: error: illegal start of definition `.`
         |    .filter: (y: Int) =>
         |    ^""".stripMargin
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
      Some(
        """|object a {
           |  foo {
           |    bar
           |  }
           |}""".stripMargin
      )
    )(
      Defn
        .Object(Nil, tname("a"), tpl(Term.Apply(tname("foo"), Term.Block(List(tname("bar"))) :: Nil)))
    )
  }

  test("#3164 empty argument 4") {
    runTestAssert[Stat](
      """|object a:
         |  foo
         |  : bar
         |""".stripMargin,
      Some("object a { foo: bar }")
    )(Defn.Object(Nil, tname("a"), tpl(Term.Ascribe(tname("foo"), pname("bar")))))
  }

  test("#3296") {
    val code = """object MyApp:
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
    val layout = """|object MyApp {
                    |  def test(str: String)(block: => Boolean): Unit = println(str + " : " + block)
                    |  test("First test") {
                    |    case class Foo(x: Int, y: String)
                    |    1 == 1
                    |  }
                    |  test("Second test") {
                    |    1 == 1
                    |  }
                    |}
                    |""".stripMargin
    assertNoDiff(parseStat(code).reprint, layout)
  }

  test("#3319 lambda complex lhs") {
    val code = """object MyApp:
                 |  List(1,2,3) foreach: x =>
                 |    println(x)
                 |""".stripMargin
    val layout = """|object MyApp {
                    |  List(1, 2, 3) foreach {
                    |    x => println(x)
                    |  }
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(Term.ApplyInfix(
        Term.Apply(tname("List"), List(int(1), int(2), int(3))),
        tname("foreach"),
        Nil,
        Term.Block(
          Term.Function(List(tparam("x", None)), Term.Apply(tname("println"), List(tname("x")))) ::
            Nil
        ) :: Nil
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 lambda simple lhs") {
    val code = """object MyApp:
                 |  ids foreach: x =>
                 |    println(x)
                 |""".stripMargin
    val layout = """|object MyApp {
                    |  ids foreach {
                    |    x => println(x)
                    |  }
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(Term.ApplyInfix(
        tname("ids"),
        tname("foreach"),
        Nil,
        Term.Block(
          Term.Function(List(tparam("x", None)), Term.Apply(tname("println"), List(tname("x")))) ::
            Nil
        ) :: Nil
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 lambda 'chained'") {
    val code = """object MyApp:
                 |  ids map: x =>
                 |      foo(x)
                 |    map: x =>
                 |      bar(x)
                 |""".stripMargin
    val layout = """|object MyApp {
                    |  ids map {
                    |    x => foo(x)
                    |  }
                    |  map {
                    |    x => bar(x)
                    |  }
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(
        Term.ApplyInfix(
          tname("ids"),
          tname("map"),
          Nil,
          Term.Block(
            Term.Function(List(tparam("x", None)), Term.Apply(tname("foo"), List(tname("x")))) ::
              Nil
          ) :: Nil
        ),
        Term.Apply(
          tname("map"),
          Term.Block(
            Term.Function(List(tparam("x", None)), Term.Apply(tname("bar"), List(tname("x")))) ::
              Nil
          ) :: Nil
        )
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 lambda block") {
    val code = """object MyApp:
                 |  ids map: x =>
                 |      foo(x)
                 |      bar(x)
                 |""".stripMargin
    val layout = """|object MyApp {
                    |  ids map { x =>
                    |    foo(x)
                    |    bar(x)
                    |  }
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(Term.ApplyInfix(
        tname("ids"),
        tname("map"),
        Nil,
        Term.Block(
          Term.Function(
            List(tparam("x", None)),
            Term.Block(List(
              Term.Apply(tname("foo"), List(tname("x"))),
              Term.Apply(tname("bar"), List(tname("x")))
            ))
          ) :: Nil
        ) :: Nil
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 function complex lhs") {
    val code = """object MyApp:
                 |  List(1,2,3) foreach:
                 |    println
                 |""".stripMargin
    val layout = """|object MyApp {
                    |  List(1, 2, 3) foreach {
                    |    println
                    |  }
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(Term.ApplyInfix(
        Term.Apply(tname("List"), List(int(1), int(2), int(3))),
        tname("foreach"),
        Nil,
        Term.Block(List(tname("println"))) :: Nil
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 function simple lhs") {
    val code = """object MyApp:
                 |  ids foreach:
                 |    println
                 |""".stripMargin
    val layout = """|object MyApp {
                    |  ids foreach {
                    |    println
                    |  }
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(
        Term
          .ApplyInfix(tname("ids"), tname("foreach"), Nil, Term.Block(List(tname("println"))) :: Nil)
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 function 'chained'") {
    val code = """object MyApp:
                 |  ids map:
                 |      foo
                 |    map:
                 |      bar
                 |""".stripMargin
    val layout = """|object MyApp {
                    |  ids map {
                    |    foo
                    |  }
                    |  map {
                    |    bar
                    |  }
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(
        Term.ApplyInfix(tname("ids"), tname("map"), Nil, Term.Block(List(tname("foo"))) :: Nil),
        Term.Apply(tname("map"), Term.Block(List(tname("bar"))) :: Nil)
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3390 within infix, within indented block") {
    val code = """object MyApp:
                 |  a >>> b.map:
                 |    foo
                 |    bar(
                 |      baz,
                 |      qux
                 |    )
                 |  >>> c
                 |""".stripMargin
    val layout = """|object MyApp {
                    |  a >>> b.map {
                    |    foo
                    |    bar(baz, qux)
                    |  } >>> c
                    |}
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(Term.ApplyInfix(
        Term.ApplyInfix(
          tname("a"),
          tname(">>>"),
          Nil,
          Term.Apply(
            Term.Select(tname("b"), tname("map")),
            Term.Block(
              List(tname("foo"), Term.Apply(tname("bar"), List(tname("baz"), tname("qux"))))
            ) :: Nil
          ) :: Nil
        ),
        tname(">>>"),
        Nil,
        List(tname("c"))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3390 within infix, at top level") {
    val code = """a >>> b.map:
                 |  foo
                 |  bar(
                 |    baz,
                 |    qux
                 |  )
                 |>>> c
                 |""".stripMargin
    val layout = """|a >>> b.map {
                    |  foo
                    |  bar(baz, qux)
                    |} >>> c
                    |""".stripMargin
    val tree = Term.ApplyInfix(
      Term.ApplyInfix(
        tname("a"),
        tname(">>>"),
        Nil,
        Term.Apply(
          Term.Select(tname("b"), tname("map")),
          Term
            .Block(
              List(tname("foo"), Term.Apply(tname("bar"), List(tname("baz"), tname("qux"))))
            ) :: Nil
        ) :: Nil
      ),
      tname(">>>"),
      Nil,
      List(tname("c"))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 1 simple, space after op, top-level, not indented") {
    val code = """
                 |baz:
                 |  arg
                 |++ qux
                 |""".stripMargin
    val layout = """|baz {
                    |  arg
                    |} ++ qux""".stripMargin
    val tree = Term.ApplyInfix(
      Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil),
      tname("++"),
      Nil,
      List(tname("qux"))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 1 simple, space after op, top-level, indented") {
    val code = """
                 |  baz:
                 |    arg
                 |  ++ qux
                 |""".stripMargin
    val layout = """|baz {
                    |  arg
                    |} ++ qux""".stripMargin
    val tree = Term.ApplyInfix(
      Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil),
      tname("++"),
      Nil,
      List(tname("qux"))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 1 simple, space after op, object in braces") {
    val code = """
                 |object a {
                 |  baz:
                 |    arg
                 |  ++ qux
                 |}
                 |""".stripMargin
    val layout = """|object a {
                    |  baz {
                    |    arg
                    |  } ++ qux
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Term.ApplyInfix(
        Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil),
        tname("++"),
        Nil,
        List(tname("qux"))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 1 simple, space after op, object indented") {
    val code = """
                 |object a:
                 |  baz:
                 |    arg
                 |  ++ qux
                 |""".stripMargin
    val layout = """|object a {
                    |  baz {
                    |    arg
                    |  } ++ qux
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Term.ApplyInfix(
        Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil),
        tname("++"),
        Nil,
        List(tname("qux"))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 2 complex, break after op, top-level, not indented") {
    val code = """
                 |foo.bar:
                 |  arg
                 |++
                 |  baz:
                 |    arg
                 |  ++
                 |  qux
                 |""".stripMargin
    val layout = """|foo.bar {
                    |  arg
                    |} ++ baz {
                    |  arg
                    |} ++ qux""".stripMargin
    val tree = Term.ApplyInfix(
      Term.ApplyInfix(
        Term.Apply(Term.Select(tname("foo"), tname("bar")), Term.Block(List(tname("arg"))) :: Nil),
        tname("++"),
        Nil,
        List(Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil))
      ),
      tname("++"),
      Nil,
      List(tname("qux"))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 2 complex, break after op, top-level, indented") {
    val code = """
                 |  foo.bar:
                 |    arg
                 |  ++
                 |  baz:
                 |    arg
                 |  ++
                 |  qux
                 |""".stripMargin
    val layout = """|foo.bar {
                    |  arg
                    |} ++ baz {
                    |  arg
                    |} ++ qux""".stripMargin
    val tree = Term.ApplyInfix(
      Term.ApplyInfix(
        Term.Apply(Term.Select(tname("foo"), tname("bar")), Term.Block(List(tname("arg"))) :: Nil),
        tname("++"),
        Nil,
        List(Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil))
      ),
      tname("++"),
      Nil,
      List(tname("qux"))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 2 complex, break after op, objeect in braces") {
    val code = """
                 |object a {
                 |  foo.bar:
                 |    arg
                 |  ++
                 |  baz:
                 |    arg
                 |  ++
                 |  qux
                 |}
                 |""".stripMargin
    val layout = """|object a {
                    |  foo.bar {
                    |    arg
                    |  } ++ baz {
                    |    arg
                    |  } ++ qux
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Term.ApplyInfix(
        Term.ApplyInfix(
          Term.Apply(Term.Select(tname("foo"), tname("bar")), Term.Block(List(tname("arg"))) :: Nil),
          tname("++"),
          Nil,
          List(Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil))
        ),
        tname("++"),
        Nil,
        List(tname("qux"))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 2 complex, break after op, objeect indented") {
    val code = """
                 |object a:
                 |  foo.bar:
                 |    arg
                 |  ++
                 |  baz:
                 |    arg
                 |  ++
                 |  qux
                 |""".stripMargin
    val layout = """|object a {
                    |  foo.bar {
                    |    arg
                    |  } ++ baz {
                    |    arg
                    |  } ++ qux
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Term.ApplyInfix(
        Term.ApplyInfix(
          Term.Apply(Term.Select(tname("foo"), tname("bar")), Term.Block(List(tname("arg"))) :: Nil),
          tname("++"),
          Nil,
          List(Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil))
        ),
        tname("++"),
        Nil,
        List(tname("qux"))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 3 complex, space after op, top-level, not indented") {
    val code = """
                 |foo.bar:
                 |  arg
                 |++
                 |  baz:
                 |    arg
                 |  ++ qux
                 |""".stripMargin
    val layout = """|foo.bar {
                    |  arg
                    |} ++ baz {
                    |  arg
                    |} ++ qux""".stripMargin
    val tree = Term.ApplyInfix(
      Term.ApplyInfix(
        Term.Apply(Term.Select(tname("foo"), tname("bar")), Term.Block(List(tname("arg"))) :: Nil),
        tname("++"),
        Nil,
        List(Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil))
      ),
      tname("++"),
      Nil,
      List(tname("qux"))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 3 complex, space after op, top-level, indented") {
    val code = """
                 |  foo.bar:
                 |    arg
                 |  ++
                 |    baz:
                 |      arg
                 |    ++ qux
                 |""".stripMargin
    val layout = """|foo.bar {
                    |  arg
                    |} ++ baz {
                    |  arg
                    |} ++ qux""".stripMargin
    val tree = Term.ApplyInfix(
      Term.ApplyInfix(
        Term.Apply(Term.Select(tname("foo"), tname("bar")), Term.Block(List(tname("arg"))) :: Nil),
        tname("++"),
        Nil,
        List(Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil))
      ),
      tname("++"),
      Nil,
      List(tname("qux"))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 3 complex, space after op, object in braces") {
    val code = """
                 |object a {
                 |  foo.bar:
                 |    arg
                 |  ++
                 |    baz:
                 |      arg
                 |    ++ qux
                 |}
                 |""".stripMargin
    val layout = """|object a {
                    |  foo.bar {
                    |    arg
                    |  } ++ baz {
                    |    arg
                    |  } ++ qux
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Term.ApplyInfix(
        Term.ApplyInfix(
          Term.Apply(Term.Select(tname("foo"), tname("bar")), Term.Block(List(tname("arg"))) :: Nil),
          tname("++"),
          Nil,
          List(Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil))
        ),
        tname("++"),
        Nil,
        List(tname("qux"))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 3 complex, space after op, object indented") {
    val code = """
                 |object a:
                 |  foo.bar:
                 |    arg
                 |  ++
                 |    baz:
                 |      arg
                 |    ++ qux
                 |""".stripMargin
    val layout = """|object a {
                    |  foo.bar {
                    |    arg
                    |  } ++ baz {
                    |    arg
                    |  } ++ qux
                    |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(Term.ApplyInfix(
        Term.ApplyInfix(
          Term.Apply(Term.Select(tname("foo"), tname("bar")), Term.Block(List(tname("arg"))) :: Nil),
          tname("++"),
          Nil,
          List(Term.Apply(tname("baz"), Term.Block(List(tname("arg"))) :: Nil))
        ),
        tname("++"),
        Nil,
        List(tname("qux"))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 4 complex, partial outdents, unindented def") {
    val code = """
                 |def mtd =
                 |    abc:
                 |        arg1
                 |      ++
                 |        abc:
                 |            arg2
                 |          ++
                 |            abc:
                 |                arg3
                 |""".stripMargin
    val layout = """|def mtd = abc {
                    |  arg1 ++ abc {
                    |    arg2 ++ abc {
                    |      arg3
                    |    }
                    |  }
                    |}""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      Term.Apply(
        tname("abc"),
        Term.Block(
          Term.ApplyInfix(
            tname("arg1"),
            tname("++"),
            Nil,
            Term.Apply(
              tname("abc"),
              Term.Block(
                Term.ApplyInfix(
                  tname("arg2"),
                  tname("++"),
                  Nil,
                  List(Term.Apply(tname("abc"), Term.Block(List(tname("arg3"))) :: Nil))
                ) :: Nil
              ) :: Nil
            ) :: Nil
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 4 complex, partial outdents, indented def") {
    val code = """
                 |  def mtd =
                 |    abc:
                 |        arg1
                 |      ++
                 |        abc:
                 |            arg2
                 |          ++
                 |            abc:
                 |                arg3
                 |""".stripMargin
    val layout = """|def mtd = abc {
                    |  arg1 ++ abc {
                    |    arg2 ++ abc {
                    |      arg3
                    |    }
                    |  }
                    |}""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      Term.Apply(
        tname("abc"),
        Term.Block(
          Term.ApplyInfix(
            tname("arg1"),
            tname("++"),
            Nil,
            Term.Apply(
              tname("abc"),
              Term.Block(
                Term.ApplyInfix(
                  tname("arg2"),
                  tname("++"),
                  Nil,
                  List(Term.Apply(tname("abc"), Term.Block(List(tname("arg3"))) :: Nil))
                ) :: Nil
              ) :: Nil
            ) :: Nil
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 4 complex, partial outdents, indented def in braces") {
    val code = """
                 |  def mtd = {
                 |    abc:
                 |        arg1
                 |      ++
                 |        abc:
                 |            arg2
                 |          ++
                 |            abc:
                 |                arg3
                 |}
                 |""".stripMargin
    val layout = """
                   |def mtd = {
                   |  abc {
                   |    arg1 ++ abc {
                   |      arg2 ++ abc {
                   |        arg3
                   |      }
                   |    }
                   |  }
                   |}
                   |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      Term.Block(
        Term.Apply(
          tname("abc"),
          Term.Block(
            Term.ApplyInfix(
              tname("arg1"),
              tname("++"),
              Nil,
              Term.Apply(
                tname("abc"),
                Term.Block(
                  Term.ApplyInfix(
                    tname("arg2"),
                    tname("++"),
                    Nil,
                    List(Term.Apply(tname("abc"), Term.Block(List(tname("arg3"))) :: Nil))
                  ) :: Nil
                ) :: Nil
              ) :: Nil
            ) :: Nil
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 5 complex, full/partial outdents, unindented def") {
    val code = """
                 |def mtd =
                 |    abc:
                 |        arg1
                 |      ++
                 |        abc:
                 |            arg2
                 |        ++
                 |            abc:
                 |                arg3
                 |""".stripMargin
    val layout = """|def mtd = abc {
                    |  arg1 ++ abc {
                    |    arg2
                    |  } ++ abc {
                    |    arg3
                    |  }
                    |}""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      Term.Apply(
        tname("abc"),
        Term.Block(
          Term.ApplyInfix(
            Term.ApplyInfix(
              tname("arg1"),
              tname("++"),
              Nil,
              List(Term.Apply(tname("abc"), Term.Block(List(tname("arg2"))) :: Nil))
            ),
            tname("++"),
            Nil,
            List(Term.Apply(tname("abc"), Term.Block(List(tname("arg3"))) :: Nil))
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 5 complex, full/partial outdents, indented def") {
    val code = """
                 |  def mtd =
                 |    abc:
                 |        arg1
                 |      ++
                 |        abc:
                 |            arg2
                 |        ++
                 |            abc:
                 |                arg3
                 |""".stripMargin
    val layout = """|def mtd = abc {
                    |  arg1 ++ abc {
                    |    arg2
                    |  } ++ abc {
                    |    arg3
                    |  }
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      Term.Apply(
        tname("abc"),
        Term.Block(
          Term.ApplyInfix(
            Term.ApplyInfix(
              tname("arg1"),
              tname("++"),
              Nil,
              List(Term.Apply(tname("abc"), Term.Block(List(tname("arg2"))) :: Nil))
            ),
            tname("++"),
            Nil,
            List(Term.Apply(tname("abc"), Term.Block(List(tname("arg3"))) :: Nil))
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 5 complex, full/partial outdents, indented def in braces") {
    val code = """
                 |  def mtd = {
                 |    abc:
                 |        arg1
                 |      ++
                 |        abc:
                 |            arg2
                 |        ++
                 |            abc:
                 |                arg3
                 |  }
                 |""".stripMargin
    val layout = """
                   |def mtd = {
                   |  abc {
                   |    arg1 ++ abc {
                   |      arg2
                   |    } ++ abc {
                   |      arg3
                   |    }
                   |  }
                   |}
                   |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      Term.Block(
        Term.Apply(
          tname("abc"),
          Term.Block(
            Term.ApplyInfix(
              Term.ApplyInfix(
                tname("arg1"),
                tname("++"),
                Nil,
                List(Term.Apply(tname("abc"), Term.Block(List(tname("arg2"))) :: Nil))
              ),
              tname("++"),
              Nil,
              List(Term.Apply(tname("abc"), Term.Block(List(tname("arg3"))) :: Nil))
            ) :: Nil
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 6 complex, with indented comments") {
    val code = """
                 |  def mtd =
                 |    abc:
                 |        arg1
                 |    ++ /* c1 */ // c2
                 |        abc:
                 |            arg2
                 |        ++ /*
                 |        c3
                 |        */ abc:
                 |                arg3
                 |""".stripMargin
    val layout = """|def mtd = abc {
                    |  arg1
                    |} ++ abc {
                    |  arg2
                    |} ++ abc {
                    |  arg3
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      Term.ApplyInfix(
        Term.ApplyInfix(
          Term.Apply(tname("abc"), Term.Block(List(tname("arg1"))) :: Nil),
          tname("++"),
          Nil,
          List(Term.Apply(tname("abc"), Term.Block(List(tname("arg2"))) :: Nil))
        ),
        tname("++"),
        Nil,
        List(Term.Apply(tname("abc"), Term.Block(List(tname("arg3"))) :: Nil))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 6 complex, with outindented comments") {
    val code = """
                 |  def mtd =
                 |    abc:
                 |        arg1
                 |    ++ /*
                 |   c2
                 |    */ abc:
                 |        arg2
                 |""".stripMargin
    val layout = """|def mtd = abc {
                    |  arg1
                    |} ++ abc {
                    |  arg2
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      Term.ApplyInfix(
        Term.Apply(tname("abc"), Term.Block(List(tname("arg1"))) :: Nil),
        tname("++"),
        Nil,
        List(Term.Apply(tname("abc"), Term.Block(List(tname("arg2"))) :: Nil))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 6 complex, with newline after multiline comment") {
    val code = """
                 |  def mtd =
                 |    abc:
                 |        arg1
                 |    ++ /*
                 |       c2
                 |    */
                 |    abc:
                 |        arg2
                 |""".stripMargin
    val layout = """|def mtd = abc {
                    |  arg1
                    |} ++ abc {
                    |  arg2
                    |}""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      Term.ApplyInfix(
        Term.Apply(tname("abc"), Term.Block(List(tname("arg1"))) :: Nil),
        tname("++"),
        Nil,
        List(Term.Apply(tname("abc"), Term.Block(List(tname("arg2"))) :: Nil))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3763 implicit after space") {
    val code = """
                 |def a =
                 |   foo: implicit bar =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  implicit bar => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Implicit()), "bar")), Some(Mod.Implicit())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 implicit after space, with type") {
    val code = """
                 |def a =
                 |   foo: implicit bar: Int =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  implicit bar: Int => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term
              .ParamClause(List(tparam(List(Mod.Implicit()), "bar", "Int")), Some(Mod.Implicit())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 implicit after newline") {
    val code = """
                 |def a = foo:
                 |   implicit bar =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  implicit bar => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Implicit()), "bar")), Some(Mod.Implicit())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 implicit after newline, with type") {
    val code = """
                 |def a = foo:
                 |   implicit bar: Int =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  implicit bar: Int => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term
              .ParamClause(List(tparam(List(Mod.Implicit()), "bar", "Int")), Some(Mod.Implicit())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 implicit in braces, no fewer") {
    val code = "def a = foo { implicit bar => baz }"
    val layout = """def a = foo {
                   |  implicit bar => baz
                   |}
                   |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Implicit()), "bar")), Some(Mod.Implicit())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 implicit in braces, with type, no fewer") {
    val code = "def a = foo { implicit bar: Int => baz }"
    val layout = """def a = foo {
                   |  implicit bar: Int => baz
                   |}
                   |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term
              .ParamClause(List(tparam(List(Mod.Implicit()), "bar", "Int")), Some(Mod.Implicit())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after space") {
    val code = """
                 |def a =
                 |   foo: using bar =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using bar) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Using()), "bar")), Some(Mod.Using())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after space, with type") {
    val code = """
                 |def a =
                 |   foo: using bar: Int =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using bar: Int) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Using()), "bar", "Int")), Some(Mod.Using())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after newline") {
    val code = """
                 |def a = foo:
                 |   using bar =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using bar) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Using()), "bar")), Some(Mod.Using())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after newline, no indent") {
    val code = """
                 |def a =
                 |   foo:
                 |   using bar =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using bar) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(Term.Function(List(tparam(List(Mod.Using()), "bar")), tname("baz")) :: Nil) ::
          Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after newline, with type") {
    val code = """
                 |def a = foo:
                 |   using bar: Int =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using bar: Int) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Using()), "bar", "Int")), Some(Mod.Using())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after newline, with type, no indent") {
    val code = """
                 |def a =
                 |   foo:
                 |   using bar: Int =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using bar: Int) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(List(tparam(List(Mod.Using()), "bar", "Int")), tname("baz")) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using, arg in parens (no fewer braces)") {
    val code = """
                 |def a =
                 |   foo(using bar => baz)
                 |""".stripMargin
    val layout = "def a = foo(using bar => baz)"
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.ArgClause(Term.Function(List(tparam("bar")), tname("baz")) :: Nil, Some(Mod.Using()))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using, arg in braces (no fewer braces)") {
    val code = """
                 |def a =
                 |   foo { using bar => baz }
                 |""".stripMargin
    val layout = """def a = foo {
                   |  (using bar) => baz
                   |}
                   |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Using()), "bar")), Some(Mod.Using())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using, with type, arg in parens (no fewer braces)") {
    val code = """
                 |def a =
                 |   foo(using bar: Int => baz)
                 |""".stripMargin
    val layout = "def a = foo(using bar: Int => baz)"
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.ArgClause(
          Term.Ascribe(tname("bar"), Type.Function(List(pname("Int")), pname("baz"))) :: Nil,
          Some(Mod.Using())
        )
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using, with type, arg in braces (no fewer braces)") {
    val code = """
                 |def a =
                 |   foo { using bar: Int => baz }
                 |""".stripMargin
    val layout = """def a = foo {
                   |  (using bar: Int) => baz
                   |}
                   |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Using()), "bar", "Int")), Some(Mod.Using())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using in parens") {
    val code = """
                 |def a =
                 |   foo: (using bar) =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using bar) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Using()), "bar")), Some(Mod.Using())),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using in parens, with type") {
    val code = """
                 |def a =
                 |   foo: (using bar: Int => String) =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using bar: Int => String) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(
              tparam(
                List(Mod.Using()),
                "bar",
                Type.Function(List(pname("Int")), pname("String"))
              ) :: Nil,
              Some(Mod.Using())
            ),
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using id in parens, with type") {
    val code = """
                 |def a =
                 |   foo: (using: Int => String) =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using: Int => String) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            tparam(Nil, "using", Type.Function(List(pname("Int")), pname("String"))) :: Nil,
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 multiple using in parens") {
    val code = """
                 |def a =
                 |   foo: (using bar: Int, baz: String) =>
                 |      qux
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using bar: Int, baz: String) => qux
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            List(tparam(List(Mod.Using()), "bar", "Int"), tparam("baz", "String")),
            tname("qux")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 multiple using in parens, second is id") {
    val code = """
                 |def a =
                 |   foo: (using bar: Int, using: String) =>
                 |      qux
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (using bar: Int, using: String) => qux
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(
              List(tparam(List(Mod.Using()), "bar", "Int"), tparam("using", "String")),
              Some(Mod.Using())
            ),
            tname("qux")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after space") {
    val code = """
                 |def a =
                 |   foo: erased bar =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  erased bar => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(Term.Function(List(tparam(List(Mod.Erased()), "bar")), tname("baz")) :: Nil) ::
          Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after space, with type") {
    val code = """
                 |def a =
                 |   foo: erased bar: Int =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (erased bar: Int) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(List(tparam(List(Mod.Erased()), "bar", "Int")), tname("baz")) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after newline") {
    val code = """
                 |def a = foo:
                 |   erased bar =>
                 |      baz
                 |""".stripMargin
    val layout = """def a = foo {
                   |  erased bar => baz
                   |}
                   |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(Term.Function(List(tparam(List(Mod.Erased()), "bar")), tname("baz")) :: Nil) ::
          Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after newline, no indent") {
    val code = """
                 |def a =
                 |   foo:
                 |   erased bar =>
                 |      baz
                 |""".stripMargin
    val layout = """def a = foo {
                   |  erased bar => baz
                   |}
                   |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(Term.Function(List(tparam(List(Mod.Erased()), "bar")), tname("baz")) :: Nil) ::
          Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after newline, with type") {
    val code = """
                 |def a = foo:
                 |   erased bar: Int =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (erased bar: Int) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(List(tparam(List(Mod.Erased()), "bar", "Int")), tname("baz")) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after newline, with type, no indent") {
    val code = """
                 |def a =
                 |   foo:
                 |   erased bar: Int =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (erased bar: Int) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(List(tparam(List(Mod.Erased()), "bar", "Int")), tname("baz")) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased, arg in parens (no fewer braces)") {
    val code = """
                 |def a =
                 |   foo(erased bar => baz)
                 |""".stripMargin
    val layout = "def a = foo(erased bar => baz)"
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Function(List(tparam(List(Mod.Erased()), "bar")), tname("baz")) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased, arg in braces (no fewer braces)") {
    val code = """
                 |def a =
                 |   foo { erased bar => baz }
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  erased bar => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(Term.Function(List(tparam(List(Mod.Erased()), "bar")), tname("baz")) :: Nil) ::
          Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased in parens") {
    val code = """
                 |def a =
                 |   foo: (erased bar) =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  erased bar => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(Term.Function(List(tparam(List(Mod.Erased()), "bar")), tname("baz")) :: Nil) ::
          Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased in parens, with type") {
    val code = """
                 |def a =
                 |   foo: (erased bar: Int => String) =>
                 |      baz
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (erased bar: Int => String) => baz
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            tparam(List(Mod.Erased()), "bar", Type.Function(List(pname("Int")), pname("String"))) ::
              Nil,
            tname("baz")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 multiple erased in parens") {
    val code = """
                 |def a =
                 |   foo: (erased bar: Int, erased baz: String) =>
                 |      qux
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  (erased bar: Int, erased baz: String) => qux
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            List(
              tparam(List(Mod.Erased()), "bar", "Int"),
              tparam(List(Mod.Erased()), "baz", "String")
            ),
            tname("qux")
          ) :: Nil
        ) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 no params with space") {
    val code = """
                 |def a =
                 |   foo: () =>
                 |      qux
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  () => qux
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(tname("foo"), Term.Block(List(Term.Function(Nil, tname("qux")))) :: Nil)
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 no params with newline") {
    val code = """
                 |def a =
                 |   foo:
                 |     () =>
                 |       qux
                 |""".stripMargin
    val layout = """|def a = foo {
                    |  () => qux
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      Term.Apply(tname("foo"), Term.Block(List(Term.Function(Nil, tname("qux")))) :: Nil)
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3785") {
    val code = """|val foo = bar(): loader =>
                  |  baz:
                  |    loader: _ => 
                  |      qux
                  |""".stripMargin
    val layout = """|val foo = bar() {
                    |  loader => baz {
                    |    loader {
                    |      _ => qux
                    |    }
                    |  }
                    |}
                    |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(Pat.Var(tname("foo"))),
      None,
      Term.Apply(
        Term.Apply(tname("bar"), Nil),
        blk(Term.Function(
          List(tparam("loader")),
          Term.Apply(
            tname("baz"),
            blk(
              Term.Apply(tname("loader"), blk(Term.Function(List(tparam("_")), tname("qux"))) :: Nil)
            ) :: Nil
          )
        )) :: Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
