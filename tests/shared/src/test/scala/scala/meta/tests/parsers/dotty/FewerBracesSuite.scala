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
      List(patvar("firstLine")),
      None,
      tapply(
        tselect(tapply(tselect("files", "get"), tname("fileName")), "fold"),
        blk(
          Defn.Val(Nil, List(patvar("fileNames")), None, tselect("files", "values")),
          tname("filenames")
        )
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
      """|val firstLine = map {
         |  /*
         |    line
         |    line */
         |  indentedCode
         |}
         |""".stripMargin
    )(Defn.Val(
      Nil,
      List(patvar("firstLine")),
      None,
      tapply(tname("map"), blk(tnameComments("indentedCode")("/*\n    line\n    line */")()))
    ))
  }

  test("simple-same-line")(
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
      List(patvar("firstLine")),
      None,
      tapply(tselect("files", "map"), blk(tfunc(tparam("a"))(tname("a"))))
    ))
  )

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
      List(patvar("firstLine")),
      None,
      tapply(tselect("files", "map"), blk(tfunc(tparam("a"), tparam("b"))(tname("a"))))
    ))
  }

  test("advanced-same-line-poly") {
    val code =
      """|val firstLine = files.map: [a, b] =>
         |    a
         |""".stripMargin
    val layout =
      """|val firstLine = files.map {
         |  [a, b] => a
         |}
         |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(patvar("firstLine")),
      None,
      tapply(tselect("files", "map"), blk(tpolyfunc(pparam("a"), pparam("b"))(tname("a"))))
    )
    runTestAssert[Stat](code, layout)(tree)
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
      List(patvar("firstLine")),
      None,
      tapply(
        tselect("files", "map"),
        Term.PartialFunction(List(Case(Pat.Tuple(List(patvar("a"), patvar("b"))), None, tname("a"))))
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
      blk(
        Defn.Val(
          Nil,
          List(patvar("firstLine")),
          None,
          tapply(
            tselect("files", "map"),
            Term.PartialFunction(List(
              Case(Pat.Extract(tname("A"), List(patvar("a"))), None, tname("a")),
              Case(Pat.Extract(tname("B"), List(patvar("b"))), None, tname("b"))
            ))
          )
        ),
        Defn.Def(Nil, tname("hello"), Nil, Nil, None, tname("???"))
      )
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
      blk(
        Defn.Val(Nil, List(patvar("firstLine")), None, tapply(tselect("files", "fold"), blk(int(123)))),
        Defn.Val(Nil, List(patvar("secondLine")), None, tapply(tselect("files", "fold"), blk(int(123))))
      )
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
      tinfix(
        tname("credentials"),
        "++",
        blk(
          Defn.Val(
            Nil,
            List(patvar("file")),
            None,
            tinfix(tselect("Path", "userHome"), "/", str(".credentials"))
          ),
          tname("file")
        )
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
      blk(Defn.Val(
        Nil,
        List(patvar("firstLine")),
        None,
        tapply(
          tselect(tapply(tselect("files", "fold"), blk(int(123))), "apply"),
          blk(tfunc(tparam("a"), tparam("b"))(tname("a")))
        )
      ))
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
      tapply(
        tselect(
          tapply(
            tname("List"),
            Term.Tuple(List(int(1), Term.Tuple(List(tapply(tname("List"), str("")), int(3)))))
          ),
          "map"
        ),
        blk(
          tfunc(tparam(
            Nil,
            "a",
            Type.Tuple(List(pname("Int"), Type.Tuple(List(papply("List", "String"), pname("Int")))))
          ))(tinfix(tselect("a", "_1"), "+", int(1)))
        )
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
      List(patvar("a")),
      Some(pname("Int")),
      tapply(
        tapply(
          tselect(
            tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix(tname("x"), "*", tname("x"))))),
            "filter"
          ),
          blk(tfunc(tparam("y", "Int"))(tinfix(tname("y"), ">", int(0))))
        ),
        int(0)
      )
    ))
  }

  test("no-self-type")(
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
    )(Defn.Class(Nil, pname("C"), Nil, EmptyCtor(), tpl(tapply(tname("f"), blk(int(22))))))
  )

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
    )(tapply(
      tselect(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), "filter"),
      blk(tfunc(tparam("x"))(tname("x")))
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
      tinfix(
        tselect("arr", "isEmpty"),
        "||",
        tapply(
          tname("locally"),
          blk(
            Defn.Val(Nil, List(patvar("first")), None, tapply(tname("arr"), int(0))),
            tinfix(tname("first"), "!=", int(1))
          )
        )
      ),
      tapply(tname("println"), str("invalid arr")),
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
      blk(
        tinfix(tinfix(tname("x"), "<", tname("y")), "or", tinfix(tname("x"), ">", tname("y"))),
        tapply(tname("or"), blk(tinfix(tname("x"), "==", tname("y"))))
      )
    ))
  }

  // Infix chains don't allow to continue with a .
  test("chain-infix-error")(runTestError[Stat](
    """|val a: Int = xs map: x =>
       |      x * x
       |    .filter: (y: Int) =>
       |      y > 0
       |    (0)
       |""".stripMargin,
    """|<input>:3: error: illegal start of definition `.`
       |    .filter: (y: Int) =>
       |    ^""".stripMargin
  ))

  test("#3164 empty argument 1")(runTestError[Stat](
    """|object a:
       |  test("foo"):
       |""".stripMargin,
    """|<input>:2: error: expected fewer-braces method body
       |  test("foo"):
       |             ^""".stripMargin
  ))

  test("#3164 empty argument 2")(runTestError[Stat](
    """|object a:
       |  foo :
       |  bar
       |""".stripMargin,
    """|<input>:2: error: expected fewer-braces method body
       |  foo :
       |      ^""".stripMargin
  ))

  test("#3164 empty argument 3")(
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
    )(Defn.Object(Nil, tname("a"), tpl(tapply(tname("foo"), blk(tname("bar"))))))
  )

  test("#3164 empty argument 4")(
    runTestAssert[Stat](
      """|object a:
         |  foo
         |  : bar
         |""".stripMargin,
      Some("object a { foo: bar }")
    )(Defn.Object(Nil, tname("a"), tpl(Term.Ascribe(tname("foo"), pname("bar")))))
  )

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
         |  test("Second test") {
         |    1 == 1
         |  }
         |}
         |""".stripMargin
    assertNoDiff(parseStat(code).reprint, layout)
  }

  test("#3319 lambda complex lhs") {
    val code =
      """object MyApp:
        |  List(1,2,3) foreach: x =>
        |    println(x)
        |""".stripMargin
    val layout =
      """|object MyApp {
         |  List(1, 2, 3) foreach {
         |    x => println(x)
         |  }
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(tinfix(
        tapply(tname("List"), int(1), int(2), int(3)),
        "foreach",
        blk(tfunc(tparam("x", None))(tapply(tname("println"), tname("x"))))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 lambda simple lhs") {
    val code =
      """object MyApp:
        |  ids foreach: x =>
        |    println(x)
        |""".stripMargin
    val layout =
      """|object MyApp {
         |  ids foreach {
         |    x => println(x)
         |  }
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(tinfix(
        tname("ids"),
        "foreach",
        blk(tfunc(tparam("x", None))(tapply(tname("println"), tname("x"))))
      ))
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
        tinfix(tname("ids"), "map", blk(tfunc(tparam("x", None))(tapply(tname("foo"), tname("x"))))),
        tapply(tname("map"), blk(tfunc(tparam("x", None))(tapply(tname("bar"), tname("x")))))
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
         |  ids map { x =>
         |    foo(x)
         |    bar(x)
         |  }
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(tinfix(
        tname("ids"),
        "map",
        blk(tfunc(tparam("x", None))(
          blk(tapply(tname("foo"), tname("x")), tapply(tname("bar"), tname("x")))
        ))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 function complex lhs") {
    val code =
      """object MyApp:
        |  List(1,2,3) foreach:
        |    println
        |""".stripMargin
    val layout =
      """|object MyApp {
         |  List(1, 2, 3) foreach {
         |    println
         |  }
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(tinfix(tapply(tname("List"), int(1), int(2), int(3)), "foreach", blk(tname("println"))))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3319 function simple lhs") {
    val code =
      """object MyApp:
        |  ids foreach:
        |    println
        |""".stripMargin
    val layout =
      """|object MyApp {
         |  ids foreach {
         |    println
         |  }
         |}""".stripMargin
    val tree = Defn
      .Object(Nil, tname("MyApp"), tpl(tinfix(tname("ids"), "foreach", blk(tname("println")))))
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
      tpl(tinfix(tname("ids"), "map", blk(tname("foo"))), tapply(tname("map"), blk(tname("bar"))))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3390 within infix, within indented block") {
    val code =
      """object MyApp:
        |  a >>> b.map:
        |    foo
        |    bar(
        |      baz,
        |      qux
        |    )
        |  >>> c
        |""".stripMargin
    val layout =
      """|object MyApp {
         |  a >>> b.map {
         |    foo
         |    bar(baz, qux)
         |  } >>> c
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("MyApp"),
      tpl(tinfix(
        tinfix(
          tname("a"),
          ">>>",
          tapply(
            tselect("b", "map"),
            blk(tname("foo"), tapply(tname("bar"), tname("baz"), tname("qux")))
          )
        ),
        ">>>",
        tname("c")
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3390 within infix, at top level") {
    val code =
      """a >>> b.map:
        |  foo
        |  bar(
        |    baz,
        |    qux
        |  )
        |>>> c
        |""".stripMargin
    val layout =
      """|a >>> b.map {
         |  foo
         |  bar(baz, qux)
         |} >>> c
         |""".stripMargin
    val tree = tinfix(
      tinfix(
        tname("a"),
        ">>>",
        tapply(tselect("b", "map"), blk(tname("foo"), tapply(tname("bar"), tname("baz"), tname("qux"))))
      ),
      ">>>",
      tname("c")
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 1 simple, space after op, top-level, not indented") {
    val code =
      """
        |baz:
        |  arg
        |++ qux
        |""".stripMargin
    val layout =
      """|baz {
         |  arg
         |} ++ qux""".stripMargin
    val tree = tinfix(tapply(tname("baz"), blk(tname("arg"))), "++", tname("qux"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 1 simple, space after op, top-level, indented") {
    val code =
      """
        |  baz:
        |    arg
        |  ++ qux
        |""".stripMargin
    val layout =
      """|baz {
         |  arg
         |} ++ qux""".stripMargin
    val tree = tinfix(tapply(tname("baz"), blk(tname("arg"))), "++", tname("qux"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 1 simple, space after op, object in braces") {
    val code =
      """
        |object a {
        |  baz:
        |    arg
        |  ++ qux
        |}
        |""".stripMargin
    val layout =
      """|object a {
         |  baz {
         |    arg
         |  } ++ qux
         |}""".stripMargin
    val tree = Defn
      .Object(Nil, tname("a"), tpl(tinfix(tapply(tname("baz"), blk(tname("arg"))), "++", tname("qux"))))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 1 simple, space after op, object indented") {
    val code =
      """
        |object a:
        |  baz:
        |    arg
        |  ++ qux
        |""".stripMargin
    val layout =
      """|object a {
         |  baz {
         |    arg
         |  } ++ qux
         |}""".stripMargin
    val tree = Defn
      .Object(Nil, tname("a"), tpl(tinfix(tapply(tname("baz"), blk(tname("arg"))), "++", tname("qux"))))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 2 complex, break after op, top-level, not indented") {
    val code =
      """
        |foo.bar:
        |  arg
        |++
        |  baz:
        |    arg
        |  ++
        |  qux
        |""".stripMargin
    val layout =
      """|foo.bar {
         |  arg
         |} ++ baz {
         |  arg
         |} ++ qux""".stripMargin
    val tree = tinfix(
      tinfix(
        tapply(tselect("foo", "bar"), blk(tname("arg"))),
        "++",
        tapply(tname("baz"), blk(tname("arg")))
      ),
      "++",
      tname("qux")
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 2 complex, break after op, top-level, indented") {
    val code =
      """
        |  foo.bar:
        |    arg
        |  ++
        |  baz:
        |    arg
        |  ++
        |  qux
        |""".stripMargin
    val layout =
      """|foo.bar {
         |  arg
         |} ++ baz {
         |  arg
         |} ++ qux""".stripMargin
    val tree = tinfix(
      tinfix(
        tapply(tselect("foo", "bar"), blk(tname("arg"))),
        "++",
        tapply(tname("baz"), blk(tname("arg")))
      ),
      "++",
      tname("qux")
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 2 complex, break after op, objeect in braces") {
    val code =
      """
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
    val layout =
      """|object a {
         |  foo.bar {
         |    arg
         |  } ++ baz {
         |    arg
         |  } ++ qux
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(tinfix(
        tinfix(
          tapply(tselect("foo", "bar"), blk(tname("arg"))),
          "++",
          tapply(tname("baz"), blk(tname("arg")))
        ),
        "++",
        tname("qux")
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 2 complex, break after op, objeect indented") {
    val code =
      """
        |object a:
        |  foo.bar:
        |    arg
        |  ++
        |  baz:
        |    arg
        |  ++
        |  qux
        |""".stripMargin
    val layout =
      """|object a {
         |  foo.bar {
         |    arg
         |  } ++ baz {
         |    arg
         |  } ++ qux
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(tinfix(
        tinfix(
          tapply(tselect("foo", "bar"), blk(tname("arg"))),
          "++",
          tapply(tname("baz"), blk(tname("arg")))
        ),
        "++",
        tname("qux")
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 3 complex, space after op, top-level, not indented") {
    val code =
      """
        |foo.bar:
        |  arg
        |++
        |  baz:
        |    arg
        |  ++ qux
        |""".stripMargin
    val layout =
      """|foo.bar {
         |  arg
         |} ++ baz {
         |  arg
         |} ++ qux""".stripMargin
    val tree = tinfix(
      tinfix(
        tapply(tselect("foo", "bar"), blk(tname("arg"))),
        "++",
        tapply(tname("baz"), blk(tname("arg")))
      ),
      "++",
      tname("qux")
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 3 complex, space after op, top-level, indented") {
    val code =
      """
        |  foo.bar:
        |    arg
        |  ++
        |    baz:
        |      arg
        |    ++ qux
        |""".stripMargin
    val layout =
      """|foo.bar {
         |  arg
         |} ++ baz {
         |  arg
         |} ++ qux""".stripMargin
    val tree = tinfix(
      tinfix(
        tapply(tselect("foo", "bar"), blk(tname("arg"))),
        "++",
        tapply(tname("baz"), blk(tname("arg")))
      ),
      "++",
      tname("qux")
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 3 complex, space after op, object in braces") {
    val code =
      """
        |object a {
        |  foo.bar:
        |    arg
        |  ++
        |    baz:
        |      arg
        |    ++ qux
        |}
        |""".stripMargin
    val layout =
      """|object a {
         |  foo.bar {
         |    arg
         |  } ++ baz {
         |    arg
         |  } ++ qux
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(tinfix(
        tinfix(
          tapply(tselect("foo", "bar"), blk(tname("arg"))),
          "++",
          tapply(tname("baz"), blk(tname("arg")))
        ),
        "++",
        tname("qux")
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 3 complex, space after op, object indented") {
    val code =
      """
        |object a:
        |  foo.bar:
        |    arg
        |  ++
        |    baz:
        |      arg
        |    ++ qux
        |""".stripMargin
    val layout =
      """|object a {
         |  foo.bar {
         |    arg
         |  } ++ baz {
         |    arg
         |  } ++ qux
         |}""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("a"),
      tpl(tinfix(
        tinfix(
          tapply(tselect("foo", "bar"), blk(tname("arg"))),
          "++",
          tapply(tname("baz"), blk(tname("arg")))
        ),
        "++",
        tname("qux")
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 4 complex, partial outdents, unindented def") {
    val code =
      """
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
    val layout =
      """|def mtd = abc {
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
      tapply(
        tname("abc"),
        blk(tinfix(
          tname("arg1"),
          "++",
          tapply(tname("abc"), blk(tinfix(tname("arg2"), "++", tapply(tname("abc"), blk(tname("arg3"))))))
        ))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 4 complex, partial outdents, indented def") {
    val code =
      """
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
    val layout =
      """|def mtd = abc {
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
      tapply(
        tname("abc"),
        blk(tinfix(
          tname("arg1"),
          "++",
          tapply(tname("abc"), blk(tinfix(tname("arg2"), "++", tapply(tname("abc"), blk(tname("arg3"))))))
        ))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 4 complex, partial outdents, indented def in braces") {
    val code =
      """
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
    val layout =
      """
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
      blk(tapply(
        tname("abc"),
        blk(tinfix(
          tname("arg1"),
          "++",
          tapply(tname("abc"), blk(tinfix(tname("arg2"), "++", tapply(tname("abc"), blk(tname("arg3"))))))
        ))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 5 complex, full/partial outdents, unindented def") {
    val code =
      """
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
    val layout =
      """|def mtd = abc {
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
      tapply(
        tname("abc"),
        blk(tinfix(
          tinfix(tname("arg1"), "++", tapply(tname("abc"), blk(tname("arg2")))),
          "++",
          tapply(tname("abc"), blk(tname("arg3")))
        ))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 5 complex, full/partial outdents, indented def") {
    val code =
      """
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
    val layout =
      """|def mtd = abc {
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
      tapply(
        tname("abc"),
        blk(tinfix(
          tinfix(tname("arg1"), "++", tapply(tname("abc"), blk(tname("arg2")))),
          "++",
          tapply(tname("abc"), blk(tname("arg3")))
        ))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 5 complex, full/partial outdents, indented def in braces") {
    val code =
      """
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
    val layout =
      """
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
      blk(tapply(
        tname("abc"),
        blk(tinfix(
          tinfix(tname("arg1"), "++", tapply(tname("abc"), blk(tname("arg2")))),
          "++",
          tapply(tname("abc"), blk(tname("arg3")))
        ))
      ))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 6 complex, with indented comments") {
    val code =
      """
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
    val layout =
      """|def mtd = abc {
         |  arg1
         |} ++ /* c1 */ // c2
         |  abc {
         |    arg2
         |  } ++ /*
         |        c3
         |        */
         |  abc {
         |    arg3
         |  }
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      tinfix(
        tinfix(
          tapply(tname("abc"), blk(tname("arg1"))),
          tnameComments("++")()("/* c1 */", "// c2"),
          tapply(tname("abc"), blk(tname("arg2")))
        ),
        tnameComments("++")()("/*\n        c3\n        */"),
        tapply(tname("abc"), blk(tname("arg3")))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 6 complex, with outindented comments") {
    val code =
      """
        |  def mtd =
        |    abc:
        |        arg1
        |    ++ /*
        |   c2
        |    */ abc:
        |        arg2
        |""".stripMargin
    val layout =
      """|def mtd = abc {
         |  arg1
         |} ++ /*
         |   c2
         |    */
         |  abc {
         |    arg2
         |  }
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      tinfix(
        tapply(tname("abc"), blk(tname("arg1"))),
        tnameComments("++")()("/*\n   c2\n    */"),
        tapply(tname("abc"), blk(tname("arg2")))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3720 6 complex, with newline after multiline comment") {
    val code =
      """
        |  def mtd =
        |    abc:
        |        arg1
        |    ++ /*
        |       c2
        |    */
        |    abc:
        |        arg2
        |""".stripMargin
    val layout =
      """|def mtd = abc {
         |  arg1
         |} ++ /*
         |       c2
         |    */
         |  abc {
         |    arg2
         |  }
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      tinfix(
        tapply(tname("abc"), blk(tname("arg1"))),
        tnameComments("++")()("/*\n       c2\n    */"),
        tapply(tname("abc"), blk(tname("arg2")))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("scalafmt #3763 implicit after space") {
    val code =
      """
        |def a =
        |   foo: implicit bar =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  implicit bar => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 implicit after space, with type") {
    val code =
      """
        |def a =
        |   foo: implicit bar: Int =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  implicit bar: Int => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar", "Int"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 implicit after newline") {
    val code =
      """
        |def a = foo:
        |   implicit bar =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  implicit bar => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 implicit after newline, with type") {
    val code =
      """
        |def a = foo:
        |   implicit bar: Int =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  implicit bar: Int => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar", "Int"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 implicit in braces, no fewer") {
    val code = "def a = foo { implicit bar => baz }"
    val layout =
      """def a = foo {
        |  implicit bar => baz
        |}
        |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 implicit in braces, with type, no fewer") {
    val code = "def a = foo { implicit bar: Int => baz }"
    val layout =
      """def a = foo {
        |  implicit bar: Int => baz
        |}
        |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar", "Int"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after space") {
    val code =
      """
        |def a =
        |   foo: using bar =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after space, with type") {
    val code =
      """
        |def a =
        |   foo: using bar: Int =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar: Int) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after newline") {
    val code =
      """
        |def a = foo:
        |   using bar =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after newline, no indent") {
    val code =
      """
        |def a =
        |   foo:
        |   using bar =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after newline, with type") {
    val code =
      """
        |def a = foo:
        |   using bar: Int =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar: Int) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using after newline, with type, no indent") {
    val code =
      """
        |def a =
        |   foo:
        |   using bar: Int =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar: Int) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using, arg in parens (no fewer braces)") {
    val code =
      """
        |def a =
        |   foo(using bar => baz)
        |""".stripMargin
    val layout = "def a = foo(using bar => baz)"
    val tree = Defn
      .Def(Nil, tname("a"), Nil, None, tapplyUsing(tname("foo"), tfunc(tparam("bar"))(tname("baz"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using, arg in braces (no fewer braces)") {
    val code =
      """
        |def a =
        |   foo { using bar => baz }
        |""".stripMargin
    val layout =
      """def a = foo {
        |  (using bar) => baz
        |}
        |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using, with type, arg in parens (no fewer braces)") {
    val code =
      """
        |def a =
        |   foo(using bar: Int => baz)
        |""".stripMargin
    val layout = "def a = foo(using bar: Int => baz)"
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapplyUsing(tname("foo"), Term.Ascribe(tname("bar"), pfunc(pname("Int"))(pname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using, with type, arg in braces (no fewer braces and no parens)") {
    val code =
      """
        |def a =
        |   foo { using bar: Int => baz }
        |""".stripMargin
    val error =
      """|<input>:3: error: `}` expected but `=>` found
         |   foo { using bar: Int => baz }
         |                        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("scalafmt #3763 using, with type, arg in braces (no fewer braces)") {
    val code =
      """|def a =
         |   foo { (using bar: Int) => baz }
         |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar: Int) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using in parens") {
    val code =
      """
        |def a =
        |   foo: (using bar) =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using in parens, with type") {
    val code =
      """
        |def a =
        |   foo: (using bar: Int => String) =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar: Int => String) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(
        tname("foo"),
        blk(
          tfunc(tparam(List(Mod.Using()), "bar", pfunc(pname("Int"))(pname("String"))))(tname("baz"))
        )
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using id in parens, with type") {
    val code =
      """
        |def a =
        |   foo: (using: Int => String) =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using: Int => String) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(
        tname("foo"),
        blk(tfunc(tparam(Nil, "using", pfunc(pname("Int"))(pname("String"))))(tname("baz")))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 multiple using in parens") {
    val code =
      """
        |def a =
        |   foo: (using bar: Int, baz: String) =>
        |      qux
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar: Int, baz: String) => qux
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(
        tname("foo"),
        blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"), tparam("baz", "String"))(tname("qux")))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 multiple using in parens, second is id") {
    val code =
      """
        |def a =
        |   foo: (using bar: Int, using: String) =>
        |      qux
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (using bar: Int, using: String) => qux
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(
        tname("foo"),
        blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"), tparam("using", "String"))(tname("qux")))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after space") {
    val code =
      """
        |def a =
        |   foo: erased bar =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  erased bar => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after space, with type") {
    val code =
      """
        |def a =
        |   foo: erased bar: Int =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (erased bar: Int) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar", "Int"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after newline") {
    val code =
      """
        |def a = foo:
        |   erased bar =>
        |      baz
        |""".stripMargin
    val layout =
      """def a = foo {
        |  erased bar => baz
        |}
        |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after newline, no indent") {
    val code =
      """
        |def a =
        |   foo:
        |   erased bar =>
        |      baz
        |""".stripMargin
    val layout =
      """def a = foo {
        |  erased bar => baz
        |}
        |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after newline, with type") {
    val code =
      """
        |def a = foo:
        |   erased bar: Int =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (erased bar: Int) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar", "Int"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased after newline, with type, no indent") {
    val code =
      """
        |def a =
        |   foo:
        |   erased bar: Int =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (erased bar: Int) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar", "Int"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased, arg in parens (no fewer braces)") {
    val code =
      """
        |def a =
        |   foo(erased bar => baz)
        |""".stripMargin
    val layout = "def a = foo(erased bar => baz)"
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased, arg in braces (no fewer braces)") {
    val code =
      """
        |def a =
        |   foo { erased bar => baz }
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  erased bar => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased in parens") {
    val code =
      """
        |def a =
        |   foo: (erased bar) =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  erased bar => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 erased in parens, with type") {
    val code =
      """
        |def a =
        |   foo: (erased bar: Int => String) =>
        |      baz
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (erased bar: Int => String) => baz
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(
        tname("foo"),
        blk(
          tfunc(tparam(List(Mod.Erased()), "bar", pfunc(pname("Int"))(pname("String"))))(tname("baz"))
        )
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 multiple erased in parens") {
    val code =
      """
        |def a =
        |   foo: (erased bar: Int, erased baz: String) =>
        |      qux
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  (erased bar: Int, erased baz: String) => qux
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(
        tname("foo"),
        blk(
          tfunc(tparam(List(Mod.Erased()), "bar", "Int"), tparam(List(Mod.Erased()), "baz", "String"))(
            tname("qux")
          )
        )
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 no params with space") {
    val code =
      """
        |def a =
        |   foo: () =>
        |      qux
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  () => qux
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("a"), Nil, None, tapply(tname("foo"), blk(tfunc()(tname("qux")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 no params with newline") {
    val code =
      """
        |def a =
        |   foo:
        |     () =>
        |       qux
        |""".stripMargin
    val layout =
      """|def a = foo {
         |  () => qux
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("a"), Nil, None, tapply(tname("foo"), blk(tfunc()(tname("qux")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3785") {
    val code =
      """|val foo = bar(): loader =>
         |  baz:
         |    loader: _ => 
         |      qux
         |""".stripMargin
    val layout =
      """|val foo = bar() {
         |  loader => baz {
         |    loader {
         |      _ => qux
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(patvar("foo")),
      None,
      tapply(
        tapply(tname("bar")),
        blk(tfunc(tparam("loader"))(
          tapply(tname("baz"), blk(tapply(tname("loader"), blk(tfunc(tparam("_"))(tname("qux"))))))
        ))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4042") {
    val code =
      """|arg
         |  .fn1:
         |    _ => "FOO1"
         |
         |  .fn2:
         |    _ => "FOO2"
         |""".stripMargin
    val layout =
      """|arg.fn1 {
         |  _ => "FOO1"
         |}.fn2 {
         |  _ => "FOO2"
         |}
         |""".stripMargin
    val tree = tapply(
      tselect(tapply(tselect("arg", "fn1"), blk(tfunc(tparam("_"))(lit("FOO1")))), "fn2"),
      blk(tfunc(tparam("_"))(lit("FOO2")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4152 not fewer braces: class") {
    val code =
      """|class A(b:
         |        Int)
         |""".stripMargin
    val layout = "class A(b: Int)"
    val tree = Defn.Class(Nil, pname("A"), Nil, ctorp(tparam("b", "Int")), tplNoBody())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4152 not fewer braces: extension 1") {
    val code =
      """|extension (b:
         |        Int)
         |  def foo = b
         |""".stripMargin
    val layout = "extension (b: Int) def foo = b"
    val tree = Defn
      .ExtensionGroup(Nil, List(List(tparam("b", "Int"))), Defn.Def(Nil, "foo", Nil, None, "b"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4152 not fewer braces: extension with type params") {
    val code =
      """|extension [B:
         |        Seq](b:
         |        Int)
         |  def foo = b
         |""".stripMargin
    val layout = "extension [B: Seq](b: Int) def foo = b"
    val tree = Defn.ExtensionGroup(
      List(pparam("B", bounds(cb = List("Seq")))),
      List(List(tparam("b", "Int"))),
      Defn.Def(Nil, "foo", Nil, None, "b")
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4152 not fewer braces: given") {
    val code =
      """|given (b:
         |        Int) = b
         |""".stripMargin
    val layout = "given (b: Int) = b"
    val tree = Defn.GivenAlias(Nil, "", Nil, Type.Tuple(List(Type.TypedParam("b", "Int", Nil))), "b")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4152 not fewer braces: def") {
    val code =
      """|def A(b:
         |        Int) = ???
         |""".stripMargin
    val layout = "def A(b: Int) = ???"
    val tree = Defn.Def(Nil, "A", Nil, List(List(tparam("b", "Int"))), None, "???")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4152 not fewer braces: type") {
    val code =
      """|def A[B:
         |        Seq] = ???
         |""".stripMargin
    val layout = "def A[B: Seq] = ???"
    val tree = Defn.Def(Nil, "A", List(pparam("B", bounds(cb = List("Seq")))), Nil, None, "???")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("sfmt #5046 fewer braces: 'starts with' but not 'is' partial function") {
    val code =
      """|val a = List("").map:
         |  case x =>
         |  x
         |  .trim
         |""".stripMargin
    val layout =
      """|val a = List("").map {
         |  case x =>
         |    x.trim
         |}
         |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(patvar("a")),
      None,
      tapply(
        tselect(tapply("List", str("")), "map"),
        Term.PartialFunction(List(Case(patvar("x"), None, tselect("x", "trim"))))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda") {
    val code =
      """|xs.map: x => x + 1
         |""".stripMargin
    val layout =
      """|xs.map {
         |  x => x + 1
         |}
         |""".stripMargin
    val tree = tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix("x", "+", lit(1)))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: erased") {
    val code =
      """|xs.map: erased x => x + 1
         |""".stripMargin
    val layout =
      """|xs.map {
         |  erased x => x + 1
         |}
         |""".stripMargin
    val tree =
      tapply(tselect("xs", "map"), blk(tfunc(tparam(List(Mod.Erased()), "x"))(tinfix("x", "+", lit(1)))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: using") {
    val code =
      """|xs.map: using x => x + 1
         |""".stripMargin
    val layout =
      """|xs.map {
         |  (using x) => x + 1
         |}
         |""".stripMargin
    val tree =
      tapply(tselect("xs", "map"), blk(tfunc(tparam(List(Mod.Using()), "x"))(tinfix("x", "+", lit(1)))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: implicit") {
    val code =
      """|xs.map: implicit x => x + 1
         |""".stripMargin
    val layout =
      """|xs.map {
         |  implicit x => x + 1
         |}
         |""".stripMargin
    val tree = tapply(
      tselect("xs", "map"),
      blk(tfunc(tparam(List(Mod.Implicit()), "x"))(tinfix("x", "+", lit(1))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: implicit w/ type") {
    val code =
      """|xs.map: (implicit x: Int) => x + 1
         |""".stripMargin
    val layout =
      """|xs.map {
         |  implicit x: Int => x + 1
         |}
         |""".stripMargin
    val tree = tapply(
      tselect("xs", "map"),
      blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(tinfix("x", "+", lit(1))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: nested") {
    val code =
      """|fooX: x => fooY: y => fooZ: z =>
         |  x + y + z
         |""".stripMargin
    val layout =
      """|fooX {
         |  x => fooY {
         |    y => fooZ {
         |      z => x + y + z
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = tapply(
      "fooX",
      blk(tfunc(tparam("x"))(tapply(
        "fooY",
        blk(tfunc(tparam("y"))(
          tapply("fooZ", blk(tfunc(tparam("z"))(tinfix(tinfix("x", "+", "y"), "+", "z"))))
        ))
      )))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 nested lambda: indented 1") {
    val code =
      """|fooX:
         |  x => fooY: y => fooZ: z =>
         |    x + y + z
         |""".stripMargin
    val layout =
      """|fooX {
         |  x => fooY {
         |    y => fooZ {
         |      z => x + y + z
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = tapply(
      "fooX",
      blk(tfunc(tparam("x"))(tapply(
        "fooY",
        blk(tfunc(tparam("y"))(
          tapply("fooZ", blk(tfunc(tparam("z"))(tinfix(tinfix("x", "+", "y"), "+", "z"))))
        ))
      )))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 nested lambda: indented 2") {
    val code =
      """|fooX:
         |  x => fooY:
         |    y => fooZ: z =>
         |      x + y + z
         |""".stripMargin
    val layout =
      """|fooX {
         |  x => fooY {
         |    y => fooZ {
         |      z => x + y + z
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = tapply(
      "fooX",
      blk(tfunc(tparam("x"))(tapply(
        "fooY",
        blk(tfunc(tparam("y"))(
          tapply("fooZ", blk(tfunc(tparam("z"))(tinfix(tinfix("x", "+", "y"), "+", "z"))))
        ))
      )))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 nested lambda: indented 3") {
    val code =
      """|fooX:
         |  x => fooY:
         |    y => fooZ:
         |      z =>
         |        x + y + z
         |""".stripMargin
    val layout =
      """|fooX {
         |  x => fooY {
         |    y => fooZ {
         |      z => x + y + z
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = tapply(
      "fooX",
      blk(tfunc(tparam("x"))(tapply(
        "fooY",
        blk(tfunc(tparam("y"))(
          tapply("fooZ", blk(tfunc(tparam("z"))(tinfix(tinfix("x", "+", "y"), "+", "z"))))
        ))
      )))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 nested fewer-braces single-line lambda") {
    // this looks like a simple lambda in scala2, but scala3 requires parens around typed param
    // https://docs.scala-lang.org/scala3/guides/migration/incompat-syntactic.html#parentheses-around-lambda-parameter
    val code =
      """|foo {
         |  x: X => x + 1
         |}
         |""".stripMargin
    val layout =
      """|foo {
         |  x {
         |    X => x + 1
         |  }
         |}
         |""".stripMargin
    val tree = tapply("foo", blk(tapply("x", blk(tfunc(tparam("X"))(tinfix("x", "+", lit(1)))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4497") {
    val layout =
      """|foo {
         |  bar
         |} match {
         |  case bar =>
         |}
         |""".stripMargin
    val tree = tmatch(tapply("foo", blk("bar")), Case(patvar("bar"), None, blk()))

    val codeWithBlank =
      """|foo:
         |    bar
         |
         |match
         |    case bar =>
         |""".stripMargin
    val codeWithoutBlank =
      """|foo:
         |    bar
         |match
         |    case bar =>
         |""".stripMargin

    runTestAssert[Stat](codeWithBlank, layout)(tree)
    runTestAssert[Stat](codeWithoutBlank, layout)(tree)
  }

  test("#4497 within braces") {
    val layout =
      """|{
         |  foo {
         |    bar
         |  }
         |} match {
         |  case bar =>
         |}
         |""".stripMargin
    val tree = tmatch(blk(tapply("foo", blk("bar"))), Case(patvar("bar"), None, blk()))

    val codeWithBlankBefore =
      """|{
         |  foo:
         |    bar
         |
         |}
         |match
         |    case bar =>
         |""".stripMargin
    val codeWithBlankAfter =
      """|{
         |  foo:
         |    bar
         |
         |}
         |
         |match
         |    case bar =>
         |""".stripMargin
    val codeWithoutBlank =
      """|{
         |  foo:
         |    bar
         |}
         |match
         |    case bar =>
         |""".stripMargin

    runTestAssert[Stat](codeWithBlankBefore, layout)(tree)
    runTestAssert[Stat](codeWithBlankAfter, layout)(tree)
    runTestAssert[Stat](codeWithoutBlank, layout)(tree)
  }

  test("match and infix after outdent") {
    val code =
      """|{
         |  foo:
         |    bar
         |  match
         |    case baz =>
         |  + 1
         |}
         |""".stripMargin
    val layout =
      """|{
         |  (foo {
         |    bar
         |  } match {
         |    case baz =>
         |  }) + 1
         |}
         |""".stripMargin
    val tree =
      blk(tinfix(tmatch(tapply("foo", blk("bar")), Case(patvar("baz"), None, blk())), "+", lit(1)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("sfmt #5217 with param name and break") {
    val code =
      """|fooB(
         |  b =
         |    fooC:
         |      2
         |    + 1
         |)
         |""".stripMargin
    val layout =
      """|fooB(b = fooC {
         |  2
         |} + 1)
         |""".stripMargin
    val tree = tapply("fooB", Term.Assign("b", tinfix(tapply("fooC", blk(lit(2))), "+", lit(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("sfmt #5217 with param name, no break, parens, no indent") {
    val code =
      """|fooB(
         |  b = fooC(2)
         |  + 1
         |)
         |""".stripMargin
    val layout = "fooB(b = fooC(2) + 1)"
    val tree = tapply("fooB", Term.Assign("b", tinfix(tapply("fooC", lit(2)), "+", lit(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("sfmt #5217 with param name, no break, no indent") {
    val code =
      """|fooB(
         |  b = fooC:
         |      2
         |  + 1
         |)
         |""".stripMargin
    val layout =
      """|fooB(b = fooC {
         |  2
         |} + 1)
         |""".stripMargin
    val tree = tapply("fooB", Term.Assign("b", tinfix(tapply("fooC", blk(lit(2))), "+", lit(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("sfmt #5217 with param name, no break, parens and indent") {
    val code =
      """|fooB(
         |  b = fooC(2)
         |    + 1
         |)
         |""".stripMargin
    val layout = "fooB(b = fooC(2) + 1)"
    val tree = tapply("fooB", Term.Assign("b", tinfix(tapply("fooC", lit(2)), "+", lit(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("sfmt #5217 with param name, no break and indent") {
    val code =
      """|fooB(
         |  b = fooC:
         |      2
         |    + 1
         |)
         |""".stripMargin
    val layout =
      """|fooB(b = fooC {
         |  2 + 1
         |})
         |""".stripMargin
    val tree = tapply("fooB", Term.Assign("b", tapply("fooC", blk(tinfix(lit(2), "+", lit(1))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("sfmt #5217 without param name") {
    val code =
      """|fooB(
         |    fooC:
         |      2
         |    + 1
         |)
         |""".stripMargin
    val layout =
      """|fooB(fooC {
         |  2
         |} + 1)
         |""".stripMargin
    val tree = tapply("fooB", tinfix(tapply("fooC", blk(lit(2))), "+", lit(1)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("sfmt #5217 without param name, no indent, parens") {
    val code =
      """|fooB(
         |    fooC(2)
         |    + 1
         |)
         |""".stripMargin
    val layout = "fooB(fooC(2) + 1)"
    val tree = tapply("fooB", tinfix(tapply("fooC", lit(2)), "+", lit(1)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("sfmt #5217 without param name, indent, parens") {
    val code =
      """|fooB(
         |    fooC(2)
         |      + 1
         |)
         |""".stripMargin
    val layout = "fooB(fooC(2) + 1)"
    val tree = tapply("fooB", tinfix(tapply("fooC", lit(2)), "+", lit(1)))
    runTestAssert[Stat](code, layout)(tree)
  }

}
