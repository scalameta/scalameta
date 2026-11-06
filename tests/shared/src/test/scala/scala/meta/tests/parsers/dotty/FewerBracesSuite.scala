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
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("firstLine")),
      None,
      tapply(
        tselect(tapply(tselect("files", "get"), tname("fileName")), "fold"),
        blk(
          Defn.Val(Nil, List(patvar("fileNames")), None, tselect("files", "values")),
          tname("filenames"),
        ),
      ),
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
         |""".stripMargin,
    )(Defn.Val(
      Nil,
      List(patvar("firstLine")),
      None,
      tapply(tname("map"), blk(tnameComments("indentedCode")("/*\n    line\n    line */")())),
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
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("firstLine")),
      None,
      tapply(tselect("files", "map"), blk(tfunc(tparam("a"))(tname("a")))),
    )),
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
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("firstLine")),
      None,
      tapply(tselect("files", "map"), blk(tfunc(tparam("a"), tparam("b"))(tname("a")))),
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
      tapply(tselect("files", "map"), blk(tpolyfunc(pparam("a"), pparam("b"))(tname("a")))),
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
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("firstLine")),
      None,
      tapply(
        tselect("files", "map"),
        Term.PartialFunction(List(Case(Pat.Tuple(List(patvar("a"), patvar("b"))), None, tname("a")))),
      ),
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
           |""".stripMargin,
      ),
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
              Case(Pat.Extract(tname("B"), List(patvar("b"))), None, tname("b")),
            )),
          ),
        ),
        Defn.Def(Nil, tname("hello"), Nil, Nil, None, tname("???")),
      ),
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
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("O"),
      Nil,
      Nil,
      None,
      blk(
        Defn.Val(Nil, List(patvar("firstLine")), None, tapply(tselect("files", "fold"), blk(int(123)))),
        Defn.Val(Nil, List(patvar("secondLine")), None, tapply(tselect("files", "fold"), blk(int(123)))),
      ),
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
           |""".stripMargin,
      ),
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
            tinfix(tselect("Path", "userHome"), "/", str(".credentials")),
          ),
          tname("file"),
        ),
      ),
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
           |""".stripMargin,
      ),
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
          blk(tfunc(tparam("a"), tparam("b"))(tname("a"))),
        ),
      )),
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
           |""".stripMargin,
      ),
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
            Term.Tuple(List(int(1), Term.Tuple(List(tapply(tname("List"), str("")), int(3))))),
          ),
          "map",
        ),
        blk(
          tfunc(tparam(
            Nil,
            "a",
            Type.Tuple(List(pname("Int"), Type.Tuple(List(papply("List", "String"), pname("Int"))))),
          ))(tinfix(tselect("a", "_1"), "+", int(1))),
        ),
      ),
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
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("a")),
      Some(pname("Int")),
      tapply(
        tapply(
          tselect(
            tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix(tname("x"), "*", tname("x"))))),
            "filter",
          ),
          blk(tfunc(tparam("y", "Int"))(tinfix(tname("y"), ">", int(0)))),
        ),
        int(0),
      ),
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
           |""".stripMargin,
      ),
    )(Defn.Class(Nil, pname("C"), Nil, EmptyCtor(), tpl(tapply(tname("f"), blk(int(22)))))),
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
           |""".stripMargin,
      ),
    )(tapply(
      tselect(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), "filter"),
      blk(tfunc(tparam("x"))(tname("x"))),
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
           |""".stripMargin,
      ),
    )(Term.If(
      tinfix(
        tselect("arr", "isEmpty"),
        "||",
        tapply(
          tname("locally"),
          blk(
            Defn.Val(Nil, List(patvar("first")), None, tapply(tname("arr"), int(0))),
            tinfix(tname("first"), "!=", int(1)),
          ),
        ),
      ),
      tapply(tname("println"), str("invalid arr")),
      Lit.Unit(),
      Nil,
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
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("test24"),
      Nil,
      Nil,
      None,
      blk(
        tinfix(tinfix(tname("x"), "<", tname("y")), "or", tinfix(tname("x"), ">", tname("y"))),
        tapply(tname("or"), blk(tinfix(tname("x"), "==", tname("y")))),
      ),
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
       |    ^""".stripMargin,
  ))

  test("#3164 empty argument 1")(runTestError[Stat](
    """|object a:
       |  test("foo"):
       |""".stripMargin,
    """|<input>:2: error: expected fewer-braces method body
       |  test("foo"):
       |             ^""".stripMargin,
  ))

  test("#3164 empty argument 2")(runTestError[Stat](
    """|object a:
       |  foo :
       |  bar
       |""".stripMargin,
    """|<input>:2: error: expected fewer-braces method body
       |  foo :
       |      ^""".stripMargin,
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
           |}""".stripMargin,
      ),
    )(Defn.Object(Nil, tname("a"), tpl(tapply(tname("foo"), blk(tname("bar")))))),
  )

  test("#3164 empty argument 4")(
    runTestAssert[Stat](
      """|object a:
         |  foo
         |  : bar
         |""".stripMargin,
      Some("object a { foo: bar }"),
    )(Defn.Object(Nil, tname("a"), tpl(Term.Ascribe(tname("foo"), pname("bar"))))),
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
        blk(tfunc(tparam("x", None))(tapply(tname("println"), tname("x")))),
      )),
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
        blk(tfunc(tparam("x", None))(tapply(tname("println"), tname("x")))),
      )),
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
        tapply(tname("map"), blk(tfunc(tparam("x", None))(tapply(tname("bar"), tname("x"))))),
      ),
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
          blk(tapply(tname("foo"), tname("x")), tapply(tname("bar"), tname("x"))),
        )),
      )),
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
      tpl(tinfix(tapply(tname("List"), int(1), int(2), int(3)), "foreach", blk(tname("println")))),
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
      tpl(tinfix(tname("ids"), "map", blk(tname("foo"))), tapply(tname("map"), blk(tname("bar")))),
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
            blk(tname("foo"), tapply(tname("bar"), tname("baz"), tname("qux"))),
          ),
        ),
        ">>>",
        tname("c"),
      )),
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
        tapply(tselect("b", "map"), blk(tname("foo"), tapply(tname("bar"), tname("baz"), tname("qux")))),
      ),
      ">>>",
      tname("c"),
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
        tapply(tname("baz"), blk(tname("arg"))),
      ),
      "++",
      tname("qux"),
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
        tapply(tname("baz"), blk(tname("arg"))),
      ),
      "++",
      tname("qux"),
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
          tapply(tname("baz"), blk(tname("arg"))),
        ),
        "++",
        tname("qux"),
      )),
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
          tapply(tname("baz"), blk(tname("arg"))),
        ),
        "++",
        tname("qux"),
      )),
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
        tapply(tname("baz"), blk(tname("arg"))),
      ),
      "++",
      tname("qux"),
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
        tapply(tname("baz"), blk(tname("arg"))),
      ),
      "++",
      tname("qux"),
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
          tapply(tname("baz"), blk(tname("arg"))),
        ),
        "++",
        tname("qux"),
      )),
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
          tapply(tname("baz"), blk(tname("arg"))),
        ),
        "++",
        tname("qux"),
      )),
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
          tapply(tname("abc"), blk(tinfix(tname("arg2"), "++", tapply(tname("abc"), blk(tname("arg3")))))),
        )),
      ),
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
          tapply(tname("abc"), blk(tinfix(tname("arg2"), "++", tapply(tname("abc"), blk(tname("arg3")))))),
        )),
      ),
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
          tapply(tname("abc"), blk(tinfix(tname("arg2"), "++", tapply(tname("abc"), blk(tname("arg3")))))),
        )),
      )),
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
          tapply(tname("abc"), blk(tname("arg3"))),
        )),
      ),
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
          tapply(tname("abc"), blk(tname("arg3"))),
        )),
      ),
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
          tapply(tname("abc"), blk(tname("arg3"))),
        )),
      )),
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
         |        */ abc {
         |  arg3
         |}
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
          tapply(tname("abc"), blk(tname("arg2"))),
        ),
        tnameComments("++")()("/*\n        c3\n        */"),
        tapply(tname("abc"), blk(tname("arg3"))),
      ),
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
         |    */ abc {
         |  arg2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("mtd"),
      Nil,
      None,
      tinfix(
        tapply(tname("abc"), blk(tname("arg1"))),
        tnameComments("++")()("/*\n   c2\n    */"),
        tapply(tname("abc"), blk(tname("arg2"))),
      ),
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
        tapply(tname("abc"), blk(tname("arg2"))),
      ),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar", "Int"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar", "Int"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Implicit()), "bar", "Int"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar"))(tname("baz")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalafmt #3763 using, with type, arg in parens (no fewer braces)") {
    val code =
      """
        |def a =
        |   foo(using bar: Int => baz)
        |""".stripMargin
    val layout = "def a = foo(using bar: (Int => baz))"
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapplyUsing(tname("foo"), Term.Ascribe(tname("bar"), pfunc(pname("Int"))(pname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Using()), "bar"))(tname("baz")))),
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
          tfunc(tparam(List(Mod.Using()), "bar", pfunc(pname("Int"))(pname("String"))))(tname("baz")),
        ),
      ),
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
        blk(tfunc(tparam(Nil, "using", pfunc(pname("Int"))(pname("String"))))(tname("baz"))),
      ),
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
        blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"), tparam("baz", "String"))(tname("qux"))),
      ),
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
        blk(tfunc(tparam(List(Mod.Using()), "bar", "Int"), tparam("using", "String"))(tname("qux"))),
      ),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar", "Int"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar", "Int"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar", "Int"))(tname("baz")))),
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
      tapply(tname("foo"), tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz"))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz")))),
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
      tapply(tname("foo"), blk(tfunc(tparam(List(Mod.Erased()), "bar"))(tname("baz")))),
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
          tfunc(tparam(List(Mod.Erased()), "bar", pfunc(pname("Int"))(pname("String"))))(tname("baz")),
        ),
      ),
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
            tname("qux"),
          ),
        ),
      ),
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
          tapply(tname("baz"), blk(tapply(tname("loader"), blk(tfunc(tparam("_"))(tname("qux")))))),
        )),
      ),
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
      blk(tfunc(tparam("_"))(lit("FOO2"))),
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
      Defn.Def(Nil, "foo", Nil, None, "b"),
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
        Term.PartialFunction(List(Case(patvar("x"), None, tselect("x", "trim")))),
      ),
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
      blk(tfunc(tparam(List(Mod.Implicit()), "x"))(tinfix("x", "+", lit(1)))),
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
      blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(tinfix("x", "+", lit(1)))),
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
          tapply("fooZ", blk(tfunc(tparam("z"))(tinfix(tinfix("x", "+", "y"), "+", "z")))),
        )),
      ))),
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
          tapply("fooZ", blk(tfunc(tparam("z"))(tinfix(tinfix("x", "+", "y"), "+", "z")))),
        )),
      ))),
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
          tapply("fooZ", blk(tfunc(tparam("z"))(tinfix(tinfix("x", "+", "y"), "+", "z")))),
        )),
      ))),
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
          tapply("fooZ", blk(tfunc(tparam("z"))(tinfix(tinfix("x", "+", "y"), "+", "z")))),
        )),
      ))),
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

  test("#4518 no indents in match, and fb lambda: simple param") {
    val code =
      """|a match
         |case n
         |if foo: b =>
         |  true
         |=>
         |  2
         |""".stripMargin
    val layout =
      """|a match {
         |  case n if foo {
         |    b => true
         |  } => 2
         |}
         |""".stripMargin
    val tree =
      tmatch("a", Case(patvar("n"), Some(tapply("foo", blk(tfunc(tparam("b"))(lit(true))))), lit(2)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4518 no indents in match, and fb lambda: case param") {
    val code =
      """|a match
         |case n
         |if foo:
         |  case b =>
         |    true
         |=>
         |  2
         |""".stripMargin
    val layout =
      """|a match {
         |  case n if foo {
         |    case b => true
         |  } => 2
         |}
         |""".stripMargin
    val tree = tmatch(
      "a",
      Case(
        patvar("n"),
        Some(tapply("foo", Term.PartialFunction(List(Case(patvar("b"), None, lit(true)))))),
        lit(2),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4518 no indents in match, and fb lambda: simple param, one-liner") {
    val code =
      """|a match
         |case n
         |if foo: b => true
         |=>
         |  2
         |""".stripMargin
    val layout =
      """|a match {
         |  case n if foo {
         |    b => true
         |  } => 2
         |}
         |""".stripMargin
    val tree =
      tmatch("a", Case(patvar("n"), Some(tapply("foo", blk(tfunc(tparam("b"))(lit(true))))), lit(2)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4518 no indents in match, and fb lambda: case param, one-liner") {
    val code =
      """|a match
         |case n
         |if foo: case b => true
         |=>
         |  2
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `:` found
         |if foo: case b => true
         |      ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("#4525") {
    val code =
      """|def test =
         |  a.flatMap: b =>
         |    c
         |      .map: d =>
         |        1
         |
         |    if true then
         |      1
         |      2
         |    else 3
         |""".stripMargin
    val layout =
      """|def test = a.flatMap { b =>
         |  c.map {
         |    d => 1
         |  }
         |  if (true) {
         |    1
         |    2
         |  } else 3
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "test",
      Nil,
      None,
      tapply(
        tselect("a", "flatMap"),
        blk(tfunc(tparam("b"))(blk(
          tapply(tselect("c", "map"), blk(tfunc(tparam("d"))(lit(1)))),
          Term.If(lit(true), blk(lit(1), lit(2)), lit(3), Nil),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4531") {
    val code =
      """|new String(
         |  foo:
         |    bar()
         |)
         |""".stripMargin
    val layout =
      """|new String(foo {
         |  bar()
         |})
         |""".stripMargin
    val tree = Term.New(init("String", List(tapply("foo", blk(tapply("bar"))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("fewer braces: this") {
    val code =
      """|this:
         |  bar
         |""".stripMargin
    val layout =
      """|this {
         |  bar
         |}
         |""".stripMargin
    val tree = tapply(Term.This(anon), blk("bar"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("partial function with a placeholder guard") {
    val code =
      """|xs.collect:
         |  case x if _ => x
         |""".stripMargin
    val layout =
      """|xs.collect {
         |  case x if _ => x
         |}
         |""".stripMargin
    val tree = Term.AnonymousFunction(tapply(
      tselect("xs", "collect"),
      Term.PartialFunction(List(Case(patvar("x"), Some(Term.Placeholder()), tname("x")))),
    ))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("tokens of an if with parentheses and an ascription in a single-line lambda, then on the next line") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a): Boolean
         |      then c else d
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
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(map) [23..26)
         |Colon [26..27)
         |Ident(x) [28..29)
         |RightArrow [30..32)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Colon [39..40)
         |Ident(Boolean) [41..48)
         |KwThen [55..59)
         |Ident(c) [60..61)
         |KwElse [62..66)
         |Ident(d) [67..68)
         |LF [68..69)
         |KwDef [71..74)
         |Ident(g) [75..76)
         |Equals [77..78)
         |Constant.Int(1) [79..80)
         |Indentation.Outdent [80..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens of an if with parentheses and an ascription in a single-line lambda, deeper leading infix") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a): Boolean
         |      && b
         |      then c else d
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
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(map) [23..26)
         |Colon [26..27)
         |Ident(x) [28..29)
         |RightArrow [30..32)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Colon [39..40)
         |Ident(Boolean) [41..48)
         |InfixLF [48..49)
         |Ident(&&) [55..57)
         |Ident(b) [58..59)
         |KwThen [66..70)
         |Ident(c) [71..72)
         |KwElse [73..77)
         |Ident(d) [78..79)
         |LF [79..80)
         |KwDef [82..85)
         |Ident(g) [86..87)
         |Equals [88..89)
         |Constant.Int(1) [90..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens of a while with parentheses and an ascription in a single-line lambda, do on the next line") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a): Boolean
         |      do c
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
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(foreach) [23..30)
         |Colon [30..31)
         |Ident(x) [32..33)
         |RightArrow [34..36)
         |KwWhile [37..42)
         |LeftParen [43..44)
         |Ident(a) [44..45)
         |RightParen [45..46)
         |Colon [46..47)
         |Ident(Boolean) [48..55)
         |KwDo [62..64)
         |Ident(c) [65..66)
         |LF [66..67)
         |KwDef [69..72)
         |Ident(g) [73..74)
         |Equals [75..76)
         |Constant.Int(1) [77..78)
         |Indentation.Outdent [78..78)
         |EOF [79..79)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens of a while with parentheses and an ascription in a single-line lambda, deeper leading infix") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a): Boolean
         |      && b
         |      do c
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
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(foreach) [23..30)
         |Colon [30..31)
         |Ident(x) [32..33)
         |RightArrow [34..36)
         |KwWhile [37..42)
         |LeftParen [43..44)
         |Ident(a) [44..45)
         |RightParen [45..46)
         |Colon [46..47)
         |Ident(Boolean) [48..55)
         |InfixLF [55..56)
         |Ident(&&) [62..64)
         |Ident(b) [65..66)
         |KwDo [73..75)
         |Ident(c) [76..77)
         |LF [77..78)
         |KwDef [80..83)
         |Ident(g) [84..85)
         |Equals [86..87)
         |Constant.Int(1) [88..89)
         |Indentation.Outdent [89..89)
         |EOF [90..90)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and an annotation ascription with an if argument, then on the next line, then on the next line") {
    val code =
      """|object O:
         |  def f =
         |    if
         |      xs.exists: x => if (a): @ann(if b then 1 else 2)
         |        then c else d
         |    then e else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = if (xs.exists {
         |    x => if (a: @ann(if (b) 1 else 2)) c else d
         |  }) e else g
         |  def h = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tapply(
              tselect("xs", "exists"),
              blk(tfunc(tparam("x"))(tif(
                Term.Annotate(
                  tname("a"),
                  List(Mod.Annot(Init(
                    pname("ann"),
                    Name.Anonymous(),
                    List(Term.ArgClause(List(tif(tname("b"), lit(1), lit(2))), None)),
                  ))),
                ),
                tname("c"),
                tname("d"),
              ))),
            ),
            tname("e"),
            tname("g"),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and a colon argument with an indented if, then on the next line, then on the next line") {
    val code =
      """|object O:
         |  def f =
         |    if
         |      xs.exists: x => if (f):
         |          if b then true else false
         |        then c else d
         |    then e else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = if (xs.exists {
         |    x => if (f {
         |      if (b) true else false
         |    }) c else d
         |  }) e else g
         |  def h = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tapply(
              tselect("xs", "exists"),
              blk(tfunc(tparam("x"))(tif(
                tapply(tname("f"), blk(tif(tname("b"), Lit.Boolean(true), Lit.Boolean(false)))),
                tname("c"),
                tname("d"),
              ))),
            ),
            tname("e"),
            tname("g"),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented while condition, while with parentheses and an annotation ascription with a while argument, do on the next line, do on the next line") {
    val code =
      """|object O:
         |  def f =
         |    while
         |      xs.exists: x => while (a): @ann(while b do g)
         |        do h(x)
         |    do k
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = while (xs.exists {
         |    x => while (a: @ann(while (b) g)) h(x)
         |  }) k
         |  def h = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          Term.While(
            tapply(
              tselect("xs", "exists"),
              blk(tfunc(tparam("x"))(Term.While(
                Term.Annotate(
                  tname("a"),
                  List(Mod.Annot(Init(
                    pname("ann"),
                    Name.Anonymous(),
                    List(Term.ArgClause(List(Term.While(tname("b"), tname("g"))), None)),
                  ))),
                ),
                tapply(tname("h"), tname("x")),
              ))),
            ),
            tname("k"),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon lambda in an if condition, `if` with parentheses, `;` and an identifier in the body on the next line, `then` and `else` on the same line, next statements") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists: x =>
         |        if (a) b; c then d else e
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
         |Ident(xs) [27..29)
         |Dot [29..30)
         |Ident(exists) [30..36)
         |Colon [36..37)
         |Ident(x) [38..39)
         |RightArrow [40..42)
         |Indentation.Indent [42..42)
         |KwIf [51..53)
         |LeftParen [54..55)
         |Ident(a) [55..56)
         |RightParen [56..57)
         |Ident(b) [58..59)
         |Semicolon [59..60)
         |Ident(c) [61..62)
         |Indentation.Outdent [61..61)
         |KwThen [63..67)
         |Ident(d) [68..69)
         |KwElse [70..74)
         |Ident(e) [75..76)
         |LF [76..77)
         |Ident(g) [81..82)
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
         |    if (xs.exists { x =>
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
          tif(
            tapply(
              tselect(tname("xs"), "exists"),
              blk(tfunc(tparam("x"))(blk(tif(tname("a"), tname("b")), tname("c")))),
            ),
            tname("d"),
            tname("e"),
          ),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon lambda in an indented if condition, `match` with an `if` with `&&` after the parentheses in the first case, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the line of the last case, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.forall: x =>
         |        x match
         |          case 1 => if (a) && b
         |            || c then true else false
         |          case _ => false then d else e
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
         |Ident(xs) [33..35)
         |Dot [35..36)
         |Ident(forall) [36..42)
         |Colon [42..43)
         |Ident(x) [44..45)
         |RightArrow [46..48)
         |Indentation.Indent [48..48)
         |Ident(x) [57..58)
         |KwMatch [59..64)
         |Indentation.Indent [64..64)
         |KwCase [75..79)
         |Constant.Int(1) [80..81)
         |RightArrow [82..84)
         |KwIf [85..87)
         |LeftParen [88..89)
         |Ident(a) [89..90)
         |RightParen [90..91)
         |Ident(&&) [92..94)
         |Ident(b) [95..96)
         |InfixLF [96..97)
         |Ident(||) [109..111)
         |Ident(c) [112..113)
         |Indentation.Outdent [112..112)
         |Indentation.Outdent [112..112)
         |Indentation.Outdent [112..112)
         |KwThen [114..118)
         |KwTrue [119..123)
         |KwElse [124..128)
         |KwFalse [129..134)
         |LF [134..135)
         |KwCase [145..149)
         |Underscore [150..151)
         |RightArrow [152..154)
         |KwFalse [155..160)
         |KwThen [161..165)
         |Ident(d) [166..167)
         |KwElse [168..172)
         |Ident(e) [173..174)
         |LF [174..175)
         |Ident(k) [179..180)
         |Indentation.Outdent [180..180)
         |Indentation.Outdent [180..180)
         |EOF [181..181)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:8: error: `outdent` expected but `case` found
         |          case _ => false then d else e
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("colon lambda, `if` with `&&` after the parentheses in the body, `then` and `else` on the next line at the column of the lambda, next definition") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x =>
         |      if (x) && b
         |    then 1 else 2
         |  def k = 1
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
         |Ident(xs) [24..26)
         |Dot [26..27)
         |Ident(map) [27..30)
         |Colon [30..31)
         |Ident(x) [32..33)
         |RightArrow [34..36)
         |Indentation.Indent [36..36)
         |KwIf [43..45)
         |LeftParen [46..47)
         |Ident(x) [47..48)
         |RightParen [48..49)
         |Ident(&&) [50..52)
         |Ident(b) [53..54)
         |Indentation.Outdent [54..54)
         |KwThen [59..63)
         |Constant.Int(1) [64..65)
         |KwElse [66..70)
         |Constant.Int(2) [71..72)
         |Indentation.Outdent [72..72)
         |LF [72..73)
         |KwDef [75..78)
         |Ident(k) [79..80)
         |Equals [81..82)
         |Constant.Int(1) [83..84)
         |Indentation.Outdent [84..84)
         |EOF [85..85)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("colon lambda, `if` without parentheses with `&&` in the body, `then` and `else` on the next line at the column of the lambda, next definition") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x =>
         |      if x && b
         |    then 1 else 2
         |  def k = 1
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
         |Ident(xs) [24..26)
         |Dot [26..27)
         |Ident(map) [27..30)
         |Colon [30..31)
         |Ident(x) [32..33)
         |RightArrow [34..36)
         |Indentation.Indent [36..36)
         |KwIf [43..45)
         |Ident(x) [46..47)
         |Ident(&&) [48..50)
         |Ident(b) [51..52)
         |Indentation.Outdent [52..52)
         |KwThen [57..61)
         |Constant.Int(1) [62..63)
         |KwElse [64..68)
         |Constant.Int(2) [69..70)
         |Indentation.Outdent [70..70)
         |LF [70..71)
         |KwDef [73..76)
         |Ident(k) [77..78)
         |Equals [79..80)
         |Constant.Int(1) [81..82)
         |Indentation.Outdent [82..82)
         |EOF [83..83)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    xs.map: x =>
         |              ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("colon lambda in an indented if condition, `if` with `&&` after the parentheses in the body, `then` and `else` on the next line at the column of the lambda, outer `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.exists: x =>
         |        if (x) && b
         |      then true else false
         |    then 1 else 2
         |  def k = 1
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
         |Ident(xs) [33..35)
         |Dot [35..36)
         |Ident(exists) [36..42)
         |Colon [42..43)
         |Ident(x) [44..45)
         |RightArrow [46..48)
         |Indentation.Indent [48..48)
         |KwIf [57..59)
         |LeftParen [60..61)
         |Ident(x) [61..62)
         |RightParen [62..63)
         |Ident(&&) [64..66)
         |Ident(b) [67..68)
         |Indentation.Outdent [68..68)
         |Indentation.Outdent [68..68)
         |KwThen [75..79)
         |KwTrue [80..84)
         |KwElse [85..89)
         |KwFalse [90..95)
         |KwThen [100..104)
         |Constant.Int(1) [105..106)
         |KwElse [107..111)
         |Constant.Int(2) [112..113)
         |Indentation.Outdent [113..113)
         |LF [113..114)
         |KwDef [116..119)
         |Ident(k) [120..121)
         |Equals [122..123)
         |Constant.Int(1) [124..125)
         |Indentation.Outdent [125..125)
         |EOF [126..126)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:7: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

}
