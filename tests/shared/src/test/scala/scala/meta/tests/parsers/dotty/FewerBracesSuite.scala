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
    val layout =
      """|def a = foo {
         |  using bar {
         |    Int => baz
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("a"),
      Nil,
      None,
      tapply(tname("foo"), blk(tinfix(tname("using"), "bar", blk(tfunc(tparam("Int"))(tname("baz")))))),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |Indentation.Indent [28..28)
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
         |Indentation.Outdent [68..68)
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
         |Indentation.Indent [28..28)
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
         |Indentation.Outdent [79..79)
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
         |Indentation.Indent [32..32)
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
         |Indentation.Outdent [66..66)
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
         |Indentation.Indent [32..32)
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
         |Indentation.Outdent [77..77)
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
         |Indentation.Outdent [62..62)
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
         |KwThen [114..118)
         |KwTrue [119..123)
         |KwElse [124..128)
         |KwFalse [129..134)
         |LF [134..135)
         |KwCase [145..149)
         |Underscore [150..151)
         |RightArrow [152..154)
         |KwFalse [155..160)
         |Indentation.Outdent [160..160)
         |Indentation.Outdent [160..160)
         |Indentation.Outdent [160..160)
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
    val layout =
      """|object A {
         |  def f = {
         |    if (xs.forall {
         |      x => x match {
         |        case 1 =>
         |          if (a && b || c) true else false
         |        case _ =>
         |          false
         |      }
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
            tapply(
              tselect(tname("xs"), "forall"),
              blk(tfunc(tparam("x"))(tmatch(
                tname("x"),
                Case(
                  int(1),
                  None,
                  tif(
                    tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
                    lit(true),
                    lit(false),
                  ),
                ),
                Case(patwildcard, None, lit(false)),
              ))),
            ),
            tname("d"),
            tname("e"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
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

  test("parens: select colon after new prefix, arrow on the same line") {
    val code =
      """|def f = g(new A(1).foo: x => x)
         |""".stripMargin
    val layout =
      """|def f = g(new A(1).foo: (x => x))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("g", Term.Ascribe(tselect(Term.New(init("A", List(lit(1)))), "foo"), pfunc("x")("x"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: select colon after an infix new operand, arrow on the same line") {
    val code =
      """|def f = g(new A + xs.map: x => x)
         |""".stripMargin
    val layout =
      """|def f = g(new A + xs.map: (x => x))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        "g",
        Term.Ascribe(tinfix(Term.New(init("A")), "+", tselect("xs", "map")), pfunc("x")("x")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("new prefix: select, brace on the next line, next statement") {
    val code =
      """|def f =
         |  val a = new A(1).foo
         |  { x => x }
         |  a
         |""".stripMargin
    val layout =
      """|def f = {
         |  val a = new A(1).foo
         |  {
         |    x => x
         |  }
         |  a
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Defn.Val(Nil, List(patvar("a")), None, tselect(Term.New(init("A", List(lit(1)))), "foo")),
        blk(tfunc(tparam("x"))(tname("x"))),
        tname("a"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("new prefix: select in braces, brace on the next line") {
    val code =
      """|def f = {
         |  val a = new A(1).foo
         |  { x => x }
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  val a = new A(1).foo
         |  {
         |    x => x
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Defn.Val(Nil, List(patvar("a")), None, tselect(Term.New(init("A", List(lit(1)))), "foo")),
        blk(tfunc(tparam("x"))(tname("x"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("new prefix: select, brace argument with a typed extractor pattern") {
    val code =
      """|def f = new A(1).foo { case Some(x): B => 1 }
         |""".stripMargin
    val layout =
      """|def f = new A(1).foo {
         |  case Some(x): B => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect(Term.New(init("A", List(lit(1)))), "foo"),
        Term.PartialFunction(List(
          Case(Pat.Typed(Pat.Extract(tname("Some"), List(patvar("x"))), pname("B")), None, lit(1)),
        )),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("new prefix: select colon, case on the next line with a typed extractor pattern") {
    val code =
      """|def f = new A(1).foo:
         |  case Some(x): B => 1
         |""".stripMargin
    val layout =
      """|def f = new A(1).foo {
         |  case Some(x): B => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect(Term.New(init("A", List(lit(1)))), "foo"),
        Term.PartialFunction(List(
          Case(Pat.Typed(Pat.Extract(tname("Some"), List(patvar("x"))), pname("B")), None, lit(1)),
        )),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: placeholder colon, function type arrow at end of line") {
    val code =
      """|def f = xs.map(_: A =>
         |  B)
         |""".stripMargin
    val layout =
      """|def f = xs.map(_: (A => B))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.AnonymousFunction(
        tapply(tselect("xs", "map"), Term.Ascribe(Term.Placeholder(), pfunc("A")("B"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: literal colon, function type arrow at end of line") {
    val code =
      """|def f = g(1: A =>
         |  C)
         |""".stripMargin
    val layout =
      """|def f = g(1: (A => C))
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tapply("g", Term.Ascribe(lit(1), pfunc("A")("C"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix operand colon: new prefix, lambda arrow at end of line") {
    val code =
      """|def f = 1 + new A: x =>
         |    x
         |""".stripMargin
    val layout =
      """|def f = 1 + (new A) {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tinfix(lit(1), "+", tapply(Term.New(init("A")), blk(tfunc(tparam("x"))(tname("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix operand colon: parenthesized operand, lambda arrow at end of line") {
    val code =
      """|def f = a + (b): x =>
         |    x
         |""".stripMargin
    val layout =
      """|def f = a + b {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tinfix("a", "+", tapply("b", blk(tfunc(tparam("x"))(tname("x"))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix operand colon: parenthesized operand, block on the next line") {
    val code =
      """|def f = a + (b):
         |    c
         |""".stripMargin
    val layout =
      """|def f = a + b {
         |  c
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tinfix("a", "+", tapply("b", blk(tname("c")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix operand colon: parenthesized operand in a for guard, lambda arrow at end of line") {
    val code =
      """|def f =
         |  for
         |    z <- zs
         |    if a op (b): x =>
         |      x
         |  yield z
         |""".stripMargin
    val layout =
      """|def f = for (z <- zs; if a op b {
         |  x => x
         |}) yield z
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.ForYield(
        List(
          Enumerator.Generator(patvar("z"), tname("zs")),
          Enumerator.Guard(tinfix("a", "op", tapply("b", blk(tfunc(tparam("x"))(tname("x")))))),
        ),
        tname("z"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("lambda parameter: function type, arrow at end of line") {
    val code =
      """|def f = (g: A =>
         |  B) => g
         |""".stripMargin
    val layout =
      """|def f = (g: A => B) => g
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tfunc(tparam("g", pfunc("A")("B")))(tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("lambda parameter: function type, lambda arrow on the next line") {
    val code =
      """|def f = (g: A =>
         |  B)
         |  => g
         |""".stripMargin
    val layout =
      """|def f = (g: A => B) => g
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tfunc(tparam("g", pfunc("A")("B")))(tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("lambda parameter: function type, arrow at end of line, in an argument") {
    val code =
      """|def f = xs.map((g: A =>
         |  B) => g)
         |""".stripMargin
    val layout =
      """|def f = xs.map((g: A => B) => g)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), tfunc(tparam("g", pfunc("A")("B")))(tname("g"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: case guard, select colon, lambda arrow at end of line") {
    val code =
      """|def f = x match
         |  case y if (ys.exists: z =>
         |      z.ok) => 1
         |  case _ => 0
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if (ys.exists: (z => z.ok)) => 1
         |  case _ => 0
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(Term.Ascribe(tselect("ys", "exists"), pfunc("z")(pselect("z", "ok")))),
          int(1),
        ),
        Case(patwildcard, None, int(0)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: case guard, infix operand, typed name, function type arrow at end of line") {
    val code =
      """|def f = x match
         |  case y if a && (g: A =>
         |    B) => g => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && (g: (A => B)) =>
         |    g => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix("a", "&&", Term.Ascribe("g", pfunc("A")("B")))),
          tfunc(tparam("g"))(int(1)),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("block colon, literal on the next line") {
    val code =
      """|def f = { a }:
         |  1
         |""".stripMargin
    val layout =
      """|def f = {
         |  a
         |}: 1
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Ascribe(blk(tname("a")), int(1)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: block colon, literal on the next line") {
    val code =
      """|def f = g({ a }:
         |  1
         |)
         |""".stripMargin
    val layout =
      """|def f = g({
         |  a
         |}: 1)
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tapply("g", Term.Ascribe(blk(tname("a")), int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: block colon, lambda on the next line") {
    val code =
      """|def f = g({ a }:
         |  y => y
         |)
         |""".stripMargin
    val layout =
      """|def f = g({
         |  a
         |}: (y => y))
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply("g", Term.Ascribe(blk(tname("a")), pfunc("y")("y"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("brace argument colon, literal on the next line") {
    val code =
      """|def f = xs.foo { a }:
         |  1
         |""".stripMargin
    val layout =
      """|def f = xs.foo {
         |  a
         |}: 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(tapply(tselect("xs", "foo"), blk(tname("a"))), int(1)),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: brace argument colon, literal on the next line") {
    val code =
      """|def f = g(xs.foo { a }:
         |  1
         |)
         |""".stripMargin
    val layout =
      """|def f = g(xs.foo {
         |  a
         |}: 1)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("g", Term.Ascribe(tapply(tselect("xs", "foo"), blk(tname("a"))), int(1))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: brace argument colon, lambda on the next line") {
    val code =
      """|def f = g(xs.foo { a }:
         |  y => y
         |)
         |""".stripMargin
    val layout =
      """|def f = g(xs.foo {
         |  a
         |}: (y => y))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("g", Term.Ascribe(tapply(tselect("xs", "foo"), blk(tname("a"))), pfunc("y")("y"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("match colon, literal on the next line") {
    val code =
      """|def f = x match { case _ => g }:
         |  1
         |""".stripMargin
    val layout =
      """|def f = (x match {
         |  case _ => g
         |}): 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(tmatch(tname("x"), Case(patwildcard, None, tname("g"))), int(1)),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: match colon, literal on the next line") {
    val code =
      """|def f = g(x match { case _ => g }:
         |  1
         |)
         |""".stripMargin
    val layout =
      """|def f = g((x match {
         |  case _ => g
         |}): 1)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("g", Term.Ascribe(tmatch(tname("x"), Case(patwildcard, None, tname("g"))), int(1))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: match colon, lambda on the next line") {
    val code =
      """|def f = g(x match { case _ => g }:
         |  y => y
         |)
         |""".stripMargin
    val layout =
      """|def f = g((x match {
         |  case _ => g
         |}): (y => y))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        "g",
        Term.Ascribe(tmatch(tname("x"), Case(patwildcard, None, tname("g"))), pfunc("y")("y")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("new anonymous colon, literal on the next line") {
    val code =
      """|def f = new A {}:
         |  1
         |""".stripMargin
    val layout =
      """|def f = new A {}: 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(Term.NewAnonymous(tpl(List(init("A")), Nil)), int(1)),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: new anonymous colon, literal on the next line") {
    val code =
      """|def f = g(new A {}:
         |  1
         |)
         |""".stripMargin
    val layout =
      """|def f = g(new A {}: 1)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("g", Term.Ascribe(Term.NewAnonymous(tpl(List(init("A")), Nil)), int(1))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: new anonymous colon, lambda on the next line") {
    val code =
      """|def f = g(new A {}:
         |  y => y
         |)
         |""".stripMargin
    val layout =
      """|def f = g(new A {}: (y => y))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("g", Term.Ascribe(Term.NewAnonymous(tpl(List(init("A")), Nil)), pfunc("y")("y"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix operand colon: block, type on the next line") {
    val code =
      """|def f = xs ++ { a }:
         |  Int
         |""".stripMargin
    val layout =
      """|def f = xs ++ {
         |  a
         |}: Int
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, Term.Ascribe(tinfix("xs", "++", blk(tname("a"))), pname("Int")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix operand colon: block, type on the next line, in an argument") {
    val code =
      """|def f = h(xs ++ { a }:
         |  Int)
         |""".stripMargin
    val layout =
      """|def f = h(xs ++ {
         |  a
         |}: Int)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("h", Term.Ascribe(tinfix("xs", "++", blk(tname("a"))), pname("Int"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix operand colon: block, lambda on the next line, in an argument") {
    val code =
      """|def f = h(xs ++ { a }:
         |  y => y)
         |""".stripMargin
    val layout =
      """|def f = h(xs ++ {
         |  a
         |}: (y => y))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("h", Term.Ascribe(tinfix("xs", "++", blk(tname("a"))), pfunc("y")("y"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix operand colon: brace argument, type on the next line") {
    val code =
      """|def f = xs ++ ys.foo { a }:
         |  Int
         |""".stripMargin
    val layout =
      """|def f = xs ++ ys.foo {
         |  a
         |}: Int
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(tinfix("xs", "++", tapply(tselect("ys", "foo"), blk(tname("a")))), pname("Int")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("block colon, type on the next line at the same indent") {
    val code =
      """|def f =
         |  { a }:
         |  Int
         |""".stripMargin
    val layout =
      """|def f = {
         |  a
         |}: Int
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Ascribe(blk(tname("a")), pname("Int")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("select colon, literal on the next line") {
    val code =
      """|def f = xs.map:
         |  1
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  1
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("apply colon, literal on the next line") {
    val code =
      """|def f = g(1):
         |  1
         |""".stripMargin
    val layout =
      """|def f = g(1) {
         |  1
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tapply(tapply("g", int(1)), blk(int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("type apply colon, literal on the next line") {
    val code =
      """|def f = g[Int]:
         |  1
         |""".stripMargin
    val layout =
      """|def f = g[Int] {
         |  1
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tapply(tapplytype("g", pname("Int")), blk(int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: select colon, lambda on the next line") {
    val code =
      """|def f = g(xs.map:
         |  y => y
         |)
         |""".stripMargin
    val layout =
      """|def f = g(xs.map {
         |  y => y
         |})
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("g", tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tname("y"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: apply colon, lambda on the next line") {
    val code =
      """|def f = h(g(1):
         |  y => y
         |)
         |""".stripMargin
    val layout =
      """|def f = h(g(1) {
         |  y => y
         |})
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("h", tapply(tapply("g", int(1)), blk(tfunc(tparam("y"))(tname("y"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: type apply colon, lambda on the next line") {
    val code =
      """|def f = h(g[Int]:
         |  y => y
         |)
         |""".stripMargin
    val layout =
      """|def f = h(g[Int] {
         |  y => y
         |})
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("h", tapply(tapplytype("g", pname("Int")), blk(tfunc(tparam("y"))(tname("y"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parens: this colon, lambda on the next line") {
    val code =
      """|def f = g(this:
         |  y => y
         |)
         |""".stripMargin
    val layout =
      """|def f = g(this {
         |  y => y
         |})
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("g", tapply(Term.This(anon), blk(tfunc(tparam("y"))(tname("y"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("refinement: type alias, type apply colon, declaration on the next line") {
    val code =
      """|type T = A[Int]:
         |  def f: Int
         |""".stripMargin
    val layout =
      """|type T = A[Int] {
         |  def f: Int
         |}
         |""".stripMargin
    val tree = Defn.Type(
      Nil,
      pname("T"),
      Nil,
      Type.Refine(Some(papply("A", "Int")), List(Decl.Def(Nil, tname("f"), Nil, pname("Int")))),
      noBounds,
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("refinement: type argument, name colon, declaration on the next line") {
    val code =
      """|type T = F[A:
         |  def g: Int
         |]
         |""".stripMargin
    val layout =
      """|type T = F[A {
         |  def g: Int
         |}]
         |""".stripMargin
    val tree = Defn.Type(
      Nil,
      pname("T"),
      Nil,
      papply("F", Type.Refine(Some(pname("A")), List(Decl.Def(Nil, tname("g"), Nil, pname("Int"))))),
      noBounds,
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("refinement: parameter type, name colon, declaration on the next line") {
    val code =
      """|def f(x: A:
         |  def g: Int
         |) = 1
         |""".stripMargin
    val layout =
      """|def f(x: A {
         |  def g: Int
         |}) = 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      List(List(
        tparam("x", Type.Refine(Some(pname("A")), List(Decl.Def(Nil, tname("g"), Nil, pname("Int"))))),
      )),
      None,
      int(1),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda, `if` with `&&` and a colon argument after the parentheses in the body, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line at the column of the `if`, next definition") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x =>
         |      if (x)(y) && foo:
         |          z
         |        && w
         |      then c else d
         |  def k = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (x(y) && foo {
         |      z && w
         |    }) c else d
         |  }
         |  def k = 1
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
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(
              tinfix(
                tapply(tname("x"), tname("y")),
                "&&",
                tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
              ),
              tname("c"),
              tname("d"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("k"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon lambda, `if` with parentheses at the line end in the body, an argument in parentheses with `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x =>
         |      if (a)
         |        (x) then 1 else 2
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
         |Ident(a) [47..48)
         |RightParen [48..49)
         |Indentation.Indent [49..49)
         |LeftParen [58..59)
         |Ident(x) [59..60)
         |RightParen [60..61)
         |KwThen [62..66)
         |Constant.Int(1) [67..68)
         |KwElse [69..73)
         |Constant.Int(2) [74..75)
         |Indentation.Outdent [75..75)
         |Indentation.Outdent [75..75)
         |Indentation.Outdent [75..75)
         |LF [75..76)
         |KwDef [78..81)
         |Ident(k) [82..83)
         |Equals [84..85)
         |Constant.Int(1) [86..87)
         |Indentation.Outdent [87..87)
         |EOF [88..88)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    xs.map: x =>
         |              ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("colon lambda, `while` with `&&` after the parentheses in the body, `do` on the next line at the column of the lambda, next definition") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x =>
         |      while (x) && b
         |    do g
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
         |KwWhile [43..48)
         |LeftParen [49..50)
         |Ident(x) [50..51)
         |RightParen [51..52)
         |Ident(&&) [53..55)
         |Ident(b) [56..57)
         |Indentation.Outdent [57..57)
         |LF [57..58)
         |KwDo [62..64)
         |Ident(g) [65..66)
         |Indentation.Outdent [66..66)
         |LF [66..67)
         |KwDef [69..72)
         |Ident(k) [73..74)
         |Equals [75..76)
         |Constant.Int(1) [77..78)
         |Indentation.Outdent [78..78)
         |EOF [79..79)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `while` expected but `outdent` found
         |    do g
         |        ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("colon lambda: a continued statement, the lambda in a `then` body, `++` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    if c then xs.map: x =>
         |        x
         |      ++ ys
         |    else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val v = a + b
         |    if (c) xs.map {
         |      x => x
         |    } ++ ys else d
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
            tinfix(
              tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
              "++",
              tname("ys"),
            ),
            tname("d"),
            Nil,
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon lambda: a continued statement in the body, a colon argument in a `then` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f = xs.map: x =>
         |    e
         |      + q
         |    if c then a && foo:
         |        z
         |      && w
         |    else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map { x =>
         |    e + q
         |    if (c) a && foo {
         |      z
         |    } && w else d
         |  }
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
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(blk(
              tinfix(tname("e"), "+", tname("q")),
              Term.If(
                tname("c"),
                tinfix(
                  tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))),
                  "&&",
                  tname("w"),
                ),
                tname("d"),
                Nil,
              ),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued `val`, the argument in the next `def`, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  val v = a
         |    + b
         |  def f = a && foo:
         |      z
         |    && w
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  val v = a + b
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
        Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
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

  test("leading infix after a colon argument: an indented first line, a continued `val`, the argument in a shallower `def`, `&&` at the column of the continuation") {
    val code =
      """|    val v = a
         |      + b
         |  def f = a && foo:
         |        z
         |      && w
         |  def g = 1
         |""".stripMargin
    val layout =
      """|val v = a + b
         |def f = a && foo {
         |  z && w
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: an indented first line, the argument in it, `&&` at the column of the first line") {
    val code =
      """|  def f = a && foo:
         |      z
         |  && w
         |""".stripMargin
    val layout =
      """|def f = a && foo {
         |  z && w
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda in braces, a continued statement, the argument in the next statement, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f = xs.map { x =>
         |      val v = a
         |        + b
         |      a && foo:
         |          z
         |        && w
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map { x =>
         |    val v = a + b
         |    a && foo {
         |      z && w
         |    }
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
          blk(tfunc(tparam("x"))(blk(
            Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
            tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda in braces, a continued statement, the argument in a `then` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f = xs.map { x =>
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
         |  def f = xs.map { x =>
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
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(blk(
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
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued `catch` case on the line of the `try`, the argument in the next statement, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val r = try a catch case _ => b
         |      + c
         |    a && foo:
         |        z
         |      && w
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val r = try a catch {
         |      case _ =>
         |        b + c
         |    }
         |    a && foo {
         |      z && w
         |    }
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
          tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon lambda with a parameter on the next line and a deeper next line, a shallower next statement") {
    val code =
      """|object O:
         |  def h = xs.foreach:
         |    x =>
         |      g(x)
         |    k(x)
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = xs.foreach {
         |    x => g(x)
         |    k(x)
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
          blk(tfunc(tparam("x"))(tapply(tname("g"), tname("x"))), tapply(tname("k"), tname("x"))),
        ),
      )),
    )))
    parseAndCheckTree[Source](code, layout)(tree)
  }

  test("colon lambda in then, else on the body line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    x else 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(c) [13..14)
         |KwThen [15..19)
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(map) [23..26)
         |Colon [26..27)
         |Ident(x) [28..29)
         |RightArrow [30..32)
         |Indentation.Indent [32..32)
         |Ident(x) [37..38)
         |Indentation.Outdent [38..38)
         |KwElse [39..43)
         |Constant.Int(1) [44..45)
         |Indentation.Outdent [45..45)
         |EOF [46..46)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, else on the next line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    x
         |  else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon argument in then, else on the body line") {
    val code =
      """|def f =
         |  if c then xs.foo:
         |    x else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.foo {
         |  x
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(tname("c"), tapply(tselect("xs", "foo"), blk(tname("x"))), lit(1), Nil),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("nested colon lambdas in then, else on the inner body line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    ys.map: y =>
         |      y else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => ys.map {
         |    y => y
         |  }
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(tapply(tselect("ys", "map"), blk(tfunc(tparam("y"))(tname("y")))))),
        ),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in an indented then body, else on the body line") {
    val code =
      """|def f =
         |  if c then
         |    xs.map: x =>
         |      x else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, else if on the body line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    x else if d then 2 else 3
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |} else if (d) 2 else 3
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        Term.If(tname("d"), lit(2), lit(3), Nil),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda after a parenthesized condition, else on the body line") {
    val code =
      """|def f =
         |  if (c) xs.map: x =>
         |    x else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in try, catch on the body line") {
    val code =
      """|def f =
         |  try xs.map: x =>
         |    x catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try xs.map {
         |  x => x
         |} catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        List(Case(Pat.Wildcard(), None, lit(1))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in try, finally on the body line") {
    val code =
      """|def f =
         |  try xs.map: x =>
         |    x finally g
         |""".stripMargin
    val layout =
      """|def f = try xs.map {
         |  x => x
         |} finally g
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term
        .Try(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), Nil, Some(tname("g"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in a condition, then on the body line") {
    val code =
      """|def f =
         |  if xs.exists: x =>
         |    x then 1 else 2
         |""".stripMargin
    val layout =
      """|def f = if (xs.exists {
         |  x => x
         |}) 1 else 2
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(1),
        lit(2),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in a while condition, do on the body line") {
    val code =
      """|def f =
         |  while xs.exists: x =>
         |    x do ()
         |""".stripMargin
    val layout =
      """|def f = while (xs.exists {
         |  x => x
         |}) ()
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.While(tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tname("x")))), lit()),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in a generator, yield on the body line") {
    val code =
      """|def f =
         |  for y <- xs.map: x =>
         |    x yield y
         |""".stripMargin
    val layout =
      """|def f = for (y <- xs.map {
         |  x => x
         |}) yield y
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.ForYield(
        List(
          Enumerator
            .Generator(patvar("y"), tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x"))))),
        ),
        tname("y"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in parens, closing parenthesis on the body line") {
    val code =
      """|def f =
         |  g(xs.map: x =>
         |    x)
         |""".stripMargin
    val layout =
      """|def f = g(xs.map {
         |  x => x
         |})
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      tapply("g", tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in braces, closing brace on the body line") {
    val code =
      """|def f =
         |  { xs.map: x =>
         |    x }
         |""".stripMargin
    val layout =
      """|def f = {
         |  xs.map {
         |    x => x
         |  }
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, "f", Nil, None, blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x"))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, if with else on the body line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    if d then e else f
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => if (d) e else f
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(Term.If(tname("d"), tname("e"), tname("f"), Nil))),
        ),
        lit(),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in try, try with catch on the body line") {
    val code =
      """|def f =
         |  try xs.map: x =>
         |    try a catch case _ => b
         |""".stripMargin
    val layout =
      """|def f = try xs.map {
         |  x => try a catch {
         |    case _ => b
         |  }
         |}
         |""".stripMargin
    val inner = Term.Try(tname("a"), List(Case(Pat.Wildcard(), None, tname("b"))), None)
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(inner))), Nil, None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon argument in an indented then body, else on the body line") {
    val code =
      """|def f =
         |  if c then
         |    xs.foo:
         |      x else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.foo {
         |  x
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(tname("c"), tapply(tselect("xs", "foo"), blk(tname("x"))), lit(1), Nil),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in an indented try body, catch on the body line") {
    val code =
      """|def f =
         |  try
         |    xs.map: x =>
         |      x catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try xs.map {
         |  x => x
         |} catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        List(Case(Pat.Wildcard(), None, lit(1))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon argument in an indented try body, catch on the body line") {
    val code =
      """|def f =
         |  try
         |    xs.foo:
         |      x catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try xs.foo {
         |  x
         |} catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(
        tapply(tselect("xs", "foo"), blk(tname("x"))),
        List(Case(Pat.Wildcard(), None, lit(1))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("nested colon lambdas in try, catch on the inner body line") {
    val code =
      """|def f =
         |  try xs.map: x =>
         |    ys.map: y =>
         |      y catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try xs.map {
         |  x => ys.map {
         |    y => y
         |  }
         |} catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(tapply(tselect("ys", "map"), blk(tfunc(tparam("y"))(tname("y")))))),
        ),
        List(Case(Pat.Wildcard(), None, lit(1))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in a generator, do on the body line") {
    val code =
      """|def f =
         |  for y <- xs.map: x =>
         |    x do g(y)
         |""".stripMargin
    val layout =
      """|def f = for (y <- xs.map {
         |  x => x
         |}) g(y)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.For(
        List(
          Enumerator
            .Generator(patvar("y"), tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x"))))),
        ),
        tapply("g", tname("y")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon argument with cases in then, else on the case line") {
    val code =
      """|def f =
         |  if c then xs.collect:
         |    case x => x else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.collect {
         |  case x => x
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "collect"), Term.PartialFunction(List(Case(patvar("x"), None, tname("x"))))),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in a nested if, else on the body line, outer else on the next line") {
    val code =
      """|def f =
         |  if a then if c then xs.map: x =>
         |    x else 1
         |  else 2
         |""".stripMargin
    val layout =
      """|def f = if (a) if (c) xs.map {
         |  x => x
         |} else 1 else 2
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("a"),
        Term.If(
          tname("c"),
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
          lit(1),
          Nil,
        ),
        lit(2),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then inside a colon lambda, else on the body line, then a statement") {
    val code =
      """|def f = ys.foreach: y =>
         |  if c then xs.map: x =>
         |    x else 1
         |  g(y)
         |""".stripMargin
    val layout =
      """|def f = ys.foreach { y =>
         |  if (c) xs.map {
         |    x => x
         |  } else 1
         |  g(y)
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      tapply(
        tselect("ys", "foreach"),
        blk(tfunc(tparam("y"))(blk(
          Term.If(
            tname("c"),
            tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
            lit(1),
            Nil,
          ),
          tapply(tname("g"), tname("y")),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in an else if branch, else on the body line") {
    val code =
      """|def f =
         |  if c then 1 else if d then xs.map: x =>
         |    x else 2
         |""".stripMargin
    val layout =
      """|def f = if (c) 1 else if (d) xs.map {
         |  x => x
         |} else 2
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        lit(1),
        Term.If(
          tname("d"),
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
          lit(2),
          Nil,
        ),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in parens, comma on the body line") {
    val code =
      """|def f =
         |  g(xs.map: x =>
         |    x, 2)
         |""".stripMargin
    val layout =
      """|def f = g(xs.map {
         |  x => x
         |}, 2)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      tapply("g", tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), lit(2)),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in a nested if, else on the body line") {
    val code =
      """|def f =
         |  if a then if c then xs.map: x =>
         |    x else 1
         |""".stripMargin
    val layout =
      """|def f = if (a) if (c) xs.map {
         |  x => x
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("a"),
        Term.If(
          tname("c"),
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
          lit(1),
          Nil,
        ),
        lit(),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in an indented try body in then, catch and else on the body line") {
    val code =
      """|def f =
         |  if c then
         |    try
         |      xs.map: x =>
         |        x catch case _ => 1 else 2
         |""".stripMargin
    val layout =
      """|def f = if (c) try xs.map {
         |  x => x
         |} catch {
         |  case _ => 1
         |} else 2
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        Term.Try(
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
          List(Case(Pat.Wildcard(), None, lit(1))),
          None,
        ),
        lit(2),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, a block comment and else on the body line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    x /* c */ else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x /* c */
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(tname("x")).toBuilder.endComment(Seq("/* c */")).result()),
        ),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, a select and else on the next line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |      x
         |    .sum else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |}.sum else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tselect(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), "sum"),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in try, catch with braces on the body line") {
    val code =
      """|def f =
         |  try xs.map: x =>
         |    x catch { case _ => 1 }
         |""".stripMargin
    val layout =
      """|def f = try xs.map {
         |  x => x
         |} catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        List(Case(Pat.Wildcard(), None, lit(1))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then of a val, else on the body line") {
    val code =
      """|val y = if c then xs.map: x =>
         |  x else 1
         |""".stripMargin
    val layout =
      """|val y = if (c) xs.map {
         |  x => x
         |} else 1
         |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(patvar("y")),
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then in parens, else on the body line") {
    val code =
      """|def f =
         |  g(if c then xs.map: x =>
         |    x else 1)
         |""".stripMargin
    val layout =
      """|def f = g(if (c) xs.map {
         |  x => x
         |} else 1)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      tapply(
        tname("g"),
        Term.If(
          tname("c"),
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
          lit(1),
          Nil,
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, if and else on their own lines, outer else on the inner else line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    if d then e
         |    else g else h
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => if (d) e else g
         |} else h
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(Term.If(tname("d"), tname("e"), tname("g"), Nil))),
        ),
        tname("h"),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, try and catch on their own lines, finally and else on the catch line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    try a
         |    catch case _ => b finally g else h
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => try a catch {
         |    case _ => b
         |  } finally g
         |} else h
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(
            Term.Try(tname("a"), List(Case(Pat.Wildcard(), None, tname("b"))), Some(tname("g"))),
          )),
        ),
        tname("h"),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda after a parenthesized condition, a call and else on the body line") {
    val code =
      """|def f =
         |  if (c) xs.foreach: x =>
         |    println(x) else println(0)
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.foreach {
         |  x => println(x)
         |} else println(0)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tapply(tname("println"), tname("x"))))),
        tapply(tname("println"), lit(0)),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, if with an infix continuation and else on the next line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    if (bar) baz &&
         |      qux else quux
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(c) [13..14)
         |KwThen [15..19)
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(map) [23..26)
         |Colon [26..27)
         |Ident(x) [28..29)
         |RightArrow [30..32)
         |Indentation.Indent [32..32)
         |KwIf [37..39)
         |LeftParen [40..41)
         |Ident(bar) [41..44)
         |RightParen [44..45)
         |Ident(baz) [46..49)
         |Ident(&&) [50..52)
         |LF [52..53)
         |Ident(qux) [59..62)
         |KwElse [63..67)
         |Ident(quux) [68..72)
         |Indentation.Outdent [72..72)
         |Indentation.Outdent [72..72)
         |EOF [73..73)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if (c) xs.map {
         |  x => if (bar) baz && qux else quux
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(
            Term.If(tname("bar"), tinfix(tname("baz"), "&&", tname("qux")), tname("quux"), Nil),
          )),
        ),
        lit(),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in an infix continuation in nested ifs, else on the body line, outer else next") {
    val code =
      """|def f =
         |  if (foo)
         |    if (bar) baz &&
         |      xs.map: x =>
         |        x else quux
         |  else xyzzy
         |""".stripMargin
    val layout =
      """|def f = if (foo) if (bar) baz && xs.map {
         |  x => x
         |} else quux else xyzzy
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("foo"),
        Term.If(
          tname("bar"),
          tinfix(tname("baz"), "&&", tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x"))))),
          tname("quux"),
          Nil,
        ),
        tname("xyzzy"),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("no quiet syntax: colon lambda after a parenthesized condition, else on the body line") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowQuietSyntax(false)
    val code =
      """|def f =
         |  if (c) xs.map: x =>
         |    x else 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `else` found
         |    x else 1
         |      ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("no quiet syntax: colon lambda in try, catch with braces on the body line") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowQuietSyntax(false)
    val code =
      """|def f =
         |  try xs.map: x =>
         |    x catch { case _ => 1 }
         |""".stripMargin
    val layout =
      """|def f = try xs.map {
         |  x => x
         |} catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        List(Case(Pat.Wildcard(), None, lit(1))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, else at the body indent") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    x
         |    else 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(c) [13..14)
         |KwThen [15..19)
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(map) [23..26)
         |Colon [26..27)
         |Ident(x) [28..29)
         |RightArrow [30..32)
         |Indentation.Indent [32..32)
         |Ident(x) [37..38)
         |Indentation.Outdent [38..38)
         |KwElse [43..47)
         |Constant.Int(1) [48..49)
         |Indentation.Outdent [49..49)
         |EOF [50..50)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, a comment line, else at the body indent") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    x
         |    // note
         |    else 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(c) [13..14)
         |KwThen [15..19)
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(map) [23..26)
         |Colon [26..27)
         |Ident(x) [28..29)
         |RightArrow [30..32)
         |Indentation.Indent [32..32)
         |Ident(x) [37..38)
         |Indentation.Outdent [38..38)
         |KwElse [55..59)
         |Constant.Int(1) [60..61)
         |Indentation.Outdent [61..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |}
         |// note
         |else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(1).toBuilder.begComment(Seq("// note")).result(),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, a block comment and else at the body indent") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    x
         |    /* note */ else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |}
         |/* note */ else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(1).toBuilder.begComment(Seq("/* note */")).result(),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in then, a trailing comment, else on the next line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    x // note
         |  else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x // note
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(
          tselect("xs", "map"),
          blk(
            Term.Function.newBuilder(List(tparam("x")), tname("x")).endComment(Seq("// note"))
              .result(),
          ),
        ),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in an indented try body, catch at the body indent") {
    val code =
      """|def f =
         |  try
         |    xs.map: x =>
         |      x
         |      catch case _ => 1
         |""".stripMargin
    val layout =
      """|def f = try xs.map {
         |  x => x
         |} catch {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.Try(
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        List(Case(Pat.Wildcard(), None, lit(1))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("nested colon lambdas in then, a comment line in the inner body, else at the outer body") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    ys.map: y =>
         |      y
         |      // c
         |    else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => ys.map {
         |    y => {
         |      y
         |      // c
         |    }
         |  }
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(
            tapply(tselect("ys", "map"), blk(tfunc(tparam("y"))(blk(tnameComments("y")()("// c"))))),
          )),
        ),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
    val fn = parseStat(code).collect { case t: Term.Function => t }.head
    assertEquals(
      fn.pos.text,
      """|x =>
         |    ys.map: y =>
         |      y
         |      // c""".stripMargin,
    )
  }

  test("colon lambda in then, a comment line in a case, else at the lambda body") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    x match
         |      case 1 => y
         |      // c
         |    else 1
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x match {
         |    case 1 =>
         |      y
         |      // c
         |  }
         |} else 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(tmatch(tname("x"), Case(lit(1), None, tnameComments("y")()("// c"))))),
        ),
        lit(1),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
    val fn = parseStat(code).collect { case t: Term.Function => t }.head
    assertEquals(
      fn.pos.text,
      """|x =>
         |    x match
         |      case 1 => y
         |      // c""".stripMargin,
    )
  }

  test("colon lambda in then, if with a prefix operator and a space, an infix continuation and else on the next line") {
    val code =
      """|def f =
         |  if c then xs.map: x =>
         |    if (bar) ! baz &&
         |      qux else quux
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => if (bar) !baz && qux else quux
         |}
         |""".stripMargin
    val body = Term.If(
      tname("bar"),
      tinfix(Term.ApplyUnary(tname("!"), tname("baz")), "&&", tname("qux")),
      tname("quux"),
      Nil,
    )
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(tname("c"), tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body))), lit(), Nil),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon before a lambda in parens after an argument") {
    val code = "foo(a, xs.map: x => x + 1)"
    val layout = "foo(a, xs.map: (x => x + 1))"
    val mapArg = Term.Ascribe(tselect("xs", "map"), pfunc("x")(pinfix("x", "+", lit(1))))
    runTestAssert[Stat](code, layout)(tapply(tname("foo"), tname("a"), mapArg))
  }

  test("colon before a lambda in parens after a try with catch") {
    val code = "foo(try a catch { case _ => b }, xs.map: x => x + 1)"
    val layout =
      """|foo(try a catch {
         |  case _ => b
         |}, xs.map: (x => x + 1))
         |""".stripMargin
    val arg = Term.Try(tname("a"), List(Case(Pat.Wildcard(), None, tname("b"))), None)
    val mapArg = Term.Ascribe(tselect("xs", "map"), pfunc("x")(pinfix("x", "+", lit(1))))
    runTestAssert[Stat](code, layout)(tapply(tname("foo"), arg, mapArg))
  }

  test("case clause with a try and a case on the catch line, colon argument of cases in the handler") {
    val code =
      """|def f = x match
         |  case 1 => try a catch case e => xs.map:
         |      case X => 1
         |  case 2 => c
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e =>
         |        xs.map {
         |          case X => 1
         |        }
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val handler =
      tapply(tselect("xs", "map"), Term.PartialFunction(List(Case(tname("X"), None, int(1)))))
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

  test("case clause with a try and a case on the catch line, colon argument of cases in the handler, empty last case, next case") {
    val code =
      """|def f = x match
         |  case 1 => try a catch case e => xs.foreach:
         |      case X => g()
         |      case _ =>
         |  case 2 => c
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e =>
         |        xs.foreach {
         |          case X =>
         |            g()
         |          case _ =>
         |        }
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val handler = tapply(
      tselect("xs", "foreach"),
      Term.PartialFunction(
        List(Case(tname("X"), None, tapply(tname("g"))), Case(Pat.Wildcard(), None, blk())),
      ),
    )
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

  test("case clause with a try and a case on the catch line, colon argument of cases in the handler, empty last case, next case, match with braces") {
    val code =
      """|def f = x match {
         |  case 1 => try a catch case e => xs.foreach:
         |      case X => g()
         |      case _ =>
         |  case 2 => c
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try a catch {
         |      case e =>
         |        xs.foreach {
         |          case X =>
         |            g()
         |          case _ =>
         |        }
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val handler = tapply(
      tselect("xs", "foreach"),
      Term.PartialFunction(
        List(Case(tname("X"), None, tapply(tname("g"))), Case(Pat.Wildcard(), None, blk())),
      ),
    )
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

  test("case guard, colon at line end, case clause with a try and a case on the catch line, match in the handler, empty last case, next case") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b => try g(b) catch case e => e match
         |        case X => true
         |        case _ =>
         |    case c => false
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    case b =>
         |      try g(b) catch {
         |        case e =>
         |          e match {
         |            case X => true
         |            case _ =>
         |          }
         |      }
         |    case c => false
         |  } => 1
         |}
         |""".stripMargin
    val handler =
      tmatch(tname("e"), Case(tname("X"), None, bool(true)), Case(Pat.Wildcard(), None, blk()))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            Term.PartialFunction(List(
              Case(
                patvar("b"),
                None,
                Term
                  .Try(tapply(tname("g"), tname("b")), List(Case(patvar("e"), None, handler)), None),
              ),
              Case(patvar("c"), None, bool(false)),
            )),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, case clause with a try and a case on the catch line, try with catch at the line end in the handler, next case") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b => try g(b) catch case e => try h(e) catch
         |        case X => true
         |    case c => false
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    case b =>
         |      try g(b) catch {
         |        case e =>
         |          try h(e) catch {
         |            case X => true
         |          }
         |      }
         |    case c => false
         |  } => 1
         |}
         |""".stripMargin
    val handler = Term
      .Try(tapply(tname("h"), tname("e")), List(Case(tname("X"), None, bool(true))), None)
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            Term.PartialFunction(List(
              Case(
                patvar("b"),
                None,
                Term
                  .Try(tapply(tname("g"), tname("b")), List(Case(patvar("e"), None, handler)), None),
              ),
              Case(patvar("c"), None, bool(false)),
            )),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda and case arrow on the guard line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z => z => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `outdent` found
         |  case y if xs.exists: z => z => 1
         |                                  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    z => z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    y
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.contains {
         |    y
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(patvar("y"), Some(tapply(tselect("xs", "contains"), blk(tname("y")))), int(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, two lines, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    val w = y
         |    w
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.contains {
         |    val w = y
         |    w
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "contains"),
            blk(Defn.Val(Nil, List(patvar("w")), None, tname("y")), tname("w")),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, case clause, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b => true
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    case b => true
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            Term.PartialFunction(List(Case(patvar("b"), None, bool(true)))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, val of a lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    val g = (a: Int) => a
         |    g(z)
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists { z =>
         |    val g = (a: Int) => a
         |    g(z)
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(blk(
              Defn.Val(Nil, List(patvar("g")), None, tfunc(tparam("a", pname("Int")))(tname("a"))),
              tapply(tname("g"), tname("z")),
            ))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, lambda in braces, case arrow after the braces") {
    val code =
      """|def f = x match
         |  case y if xs.exists { z => z } => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, case arrow between the body's and the case's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |      z
         |    => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, case arrow after an infix line at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |  && b => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } && b => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
            "&&",
            tname("b"),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, case arrow after an infix line at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    y
         |  && b => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.contains {
         |    y
         |  } && b => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(tapply(tselect("xs", "contains"), blk(tname("y"))), "&&", tname("b"))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, curried colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z => w =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("for guard, colon lambda, yield at a smaller indent") {
    val code =
      """|def f = for
         |  x <- xs
         |  if ys.exists: z =>
         |    z
         |yield x
         |""".stripMargin
    val layout =
      """|def f = for (x <- xs; if ys.exists {
         |  z => z
         |}) yield x
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.ForYield(
        Term.EnumeratorsBlock(List(
          Enumerator.Generator(patvar("x"), tname("xs")),
          Enumerator.Guard(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
        )),
        tname("x"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, case clause with its body on the next line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b =>
         |      b => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      b => 1
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, nested colon at line end, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    ys.exists:
         |      w => w
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    ys.exists {
         |      w => w
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tapply(tselect("ys", "exists"), blk(tfunc(tparam("w"))(tname("w"))))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, leading infix line, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if a
         |    && xs.exists: z =>
         |      z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("if condition, colon lambda, then at a smaller indent") {
    val code =
      """|def f =
         |  if xs.exists: z =>
         |    z
         |  then 1 else 2
         |""".stripMargin
    val layout =
      """|def f = if (xs.exists {
         |  z => z
         |}) 1 else 2
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.If(
        tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
        int(1),
        int(2),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, try with a case on the catch line, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    try z > 0
         |    catch case _: Exception => false
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => try z > 0 catch {
         |      case _: Exception => false
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val body = Term.Try(
      tinfix(tname("z"), tname(">"), int(0)),
      List(Case(Pat.Typed(patwildcard, Type.Name("Exception")), None, bool(false))),
      None,
    )
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(body)))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, comment at the end of the body, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z // c
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z // c
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(tname("z")).toBuilder.endComment(Seq("// c")).result()),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, lambda, comment line left of the lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |      z =>
         |        z > 1
         |     // c
         |    => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z > 1
         |    // c
         |  } => 1
         |}
         |""".stripMargin
    val body = tinfix(tname("z"), ">", int(1))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(body).toBuilder.endComment(Seq("// c")).result()),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, lambda, case arrow after a block comment whose last line is deeper") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |      z =>
         |        z > 1
         |    /* c
         |     d */ => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z > 1
         |    /* c
         |     d */
         |  } => 1
         |}
         |""".stripMargin
    val lambda = tfunc(tparam("z"))(tinfix(tname("z"), ">", int(1)))
    val arg = blk(lambda.toBuilder.endComment(Seq("/* c\n     d */")).result())
    val guard = tapply(tselect("xs", "exists"), arg)
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tmatch(tname("x"), Case(patvar("y"), Some(guard), int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, lambda, case arrow after a block comment whose last line is shallower") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |      z =>
         |        z > 1
         |       /* c
         |     d */ => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z > 1
         |    /* c
         |     d */
         |  } => 1
         |}
         |""".stripMargin
    val lambda = tfunc(tparam("z"))(tinfix(tname("z"), ">", int(1)))
    val arg = blk(lambda.toBuilder.endComment(Seq("/* c\n     d */")).result())
    val guard = tapply(tselect("xs", "exists"), arg)
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tmatch(tname("x"), Case(patvar("y"), Some(guard), int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, lambda, case arrow after a block comment on its line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |      z =>
         |        z > 1
         |     /* c */ => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z > 1
         |    /* c */
         |  } => 1
         |}
         |""".stripMargin
    val lambda = tfunc(tparam("z"))(tinfix(tname("z"), ">", int(1)))
    val arg = blk(lambda.toBuilder.endComment(Seq("/* c */")).result())
    val guard = tapply(tselect("xs", "exists"), arg)
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tmatch(tname("x"), Case(patvar("y"), Some(guard), int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, lambda, case arrow after a block comment with a deeper middle line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |      z =>
         |        z > 1
         |     /* c
         |       e
         |     d */ => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z > 1
         |    /* c
         |       e
         |     d */
         |  } => 1
         |}
         |""".stripMargin
    val lambda = tfunc(tparam("z"))(tinfix(tname("z"), ">", int(1)))
    val arg = blk(lambda.toBuilder.endComment(Seq("/* c\n       e\n     d */")).result())
    val guard = tapply(tselect("xs", "exists"), arg)
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tmatch(tname("x"), Case(patvar("y"), Some(guard), int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, lambda, case arrow after a block comment with a line break right after its opener") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |      z =>
         |        z > 1
         |       /*
         |     d */ => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z > 1
         |    /*
         |     d */
         |  } => 1
         |}
         |""".stripMargin
    val lambda = tfunc(tparam("z"))(tinfix(tname("z"), ">", int(1)))
    val arg = blk(lambda.toBuilder.endComment(Seq("/*\n     d */")).result())
    val guard = tapply(tselect("xs", "exists"), arg)
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tmatch(tname("x"), Case(patvar("y"), Some(guard), int(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, match, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z match
         |      case 1 => true
         |      case _ => false
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z match {
         |      case 1 => true
         |      case _ => false
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(
              tmatch(tname("z"), Case(int(1), None, bool(true)), Case(patwildcard, None, bool(false))),
            )),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, nested colon at line end, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z.ys.exists:
         |      w => w
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z.ys.exists {
         |      w => w
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(
              tapply(tselect(tselect("z", "ys"), "exists"), blk(tfunc(tparam("w"))(tname("w")))),
            )),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, nested colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z.ys.exists: w =>
         |      w
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z.ys.exists {
         |      w => w
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(
              tapply(tselect(tselect("z", "ys"), "exists"), blk(tfunc(tparam("w"))(tname("w")))),
            )),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, val with a function type, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    val g: Int => Int = identity
         |    g(z)
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists { z =>
         |    val g: Int => Int = identity
         |    g(z)
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(blk(
              Defn.Val(
                Nil,
                List(patvar("g")),
                Some(pfunc(pname("Int"))(pname("Int"))),
                tname("identity"),
              ),
              tapply(tname("g"), tname("z")),
            ))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match with braces, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match {
         |  case y if xs.exists: z =>
         |    z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in partial function, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = xs.collect {
         |  case y if ys.exists: z =>
         |    z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = xs.collect {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "collect"),
        Term.PartialFunction(List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in catch, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = try g catch
         |  case y if ys.exists: z =>
         |    z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = try g catch {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tname("g"),
        List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        )),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in colon argument of cases, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = xs.collect:
         |  case y if ys.exists: z =>
         |    z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = xs.collect {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "collect"),
        Term.PartialFunction(List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in the second case, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case 0 => 0
         |  case y if xs.exists: z =>
         |    z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 0 => 0
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(0), None, int(0)),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in the second case of a match with braces, colon at line end, case arrow at a smaller indent") {
    val code =
      """|def f = x match {
         |  case 0 => 0
         |  case y if xs.contains:
         |    y
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 0 => 0
         |  case y if xs.contains {
         |    y
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(0), None, int(0)),
        Case(patvar("y"), Some(tapply(tselect("xs", "contains"), blk(tname("y")))), int(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match in a case body, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y =>
         |    y match
         |      case w if ws.exists: z =>
         |        z
         |      => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y =>
         |    y match {
         |      case w if ws.exists {
         |        z => z
         |      } => 1
         |    }
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
        tname("x"),
        Case(
          patvar("y"),
          None,
          tmatch(
            tname("y"),
            Case(
              patvar("w"),
              Some(tapply(tselect("ws", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
              int(1),
            ),
          ),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the catch line, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = try g catch case y if ys.exists: z =>
         |  z
         |=> 1
         |""".stripMargin
    val layout =
      """|def f = try g catch {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tname("g"),
        List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        )),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y
         |    if xs.exists: z =>
         |      z
         |    => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match with braces, colon lambda, body is a lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match {
         |  case y if xs.exists: z =>
         |    z => w
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z => w
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tfunc(tparam("z"))(tname("w")))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "case guard in a match with braces, colon at line end, lambda, case arrow at a smaller indent",
  ) {
    val code =
      """|def f = x match {
         |  case y if xs.contains:
         |    y => w
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.contains {
         |    y => w
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "contains"), blk(tfunc(tparam("y"))(tname("w"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end, colon lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match
         |  case y if
         |      xs.exists: z =>
         |        z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end, colon lambda, case arrow at the guard's indent") {
    val code =
      """|def f = x match
         |  case y if
         |      xs.exists: z =>
         |        z
         |      => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end, lambda in parentheses, case arrow on the guard's line") {
    val code =
      """|def f = x match
         |  case y if
         |      xs.exists(z => z) => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists(z => z) => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), tfunc(tparam("z"))(tname("z")))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, trailing infix operator, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if a &&
         |      xs.exists: z =>
         |        z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, case arrow after a select line at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.map: z =>
         |    z + 1
         |  .contains(3) => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.map {
         |    z => z + 1
         |  }.contains(3) => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect(
              tapply(tselect("xs", "map"), blk(tfunc(tparam("z"))(tinfix(tname("z"), "+", int(1))))),
              "contains",
            ),
            int(3),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, case arrow after a select line at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.filter:
         |    _ > 0
         |  .nonEmpty => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.filter {
         |    _ > 0
         |  }.nonEmpty => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tselect(
            tapply(
              tselect("xs", "filter"),
              blk(Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", int(0)))),
            ),
            "nonEmpty",
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, if-then-else, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    if z > 0 then z < 9 else false
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => if (z > 0) z < 9 else false
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(Term.If(
              tinfix(tname("z"), ">", int(0)),
              tinfix(tname("z"), "<", int(9)),
              bool(false),
              Nil,
            ))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the brace line, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = xs.collect { case y if ys.exists: z =>
         |    z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = xs.collect {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "collect"),
        Term.PartialFunction(List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match before a statement, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f =
         |  x match
         |    case y if xs.exists: z =>
         |      z
         |    => 1
         |  g
         |""".stripMargin
    val layout =
      """|def f = {
         |  x match {
         |    case y if xs.exists {
         |      z => z
         |    } => 1
         |  }
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
            int(1),
          ),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon at line end, nested colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    ys.exists: w =>
         |      w
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    ys.exists {
         |      w => w
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tapply(tselect("ys", "exists"), blk(tfunc(tparam("w"))(tname("w"))))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end, colon at line end, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if
         |      p:
         |        Boolean
         |    => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if p {
         |    Boolean
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(tname("x"), Case(patvar("y"), Some(tapply(tname("p"), blk(tname("Boolean")))), int(1))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end, colon at line end, arrow on the argument's line, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if
         |      p:
         |        Boolean => 1
         |    => 2
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if p {
         |    Boolean => 1
         |  } => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(patvar("y"), Some(tapply(tname("p"), blk(tfunc(tparam("Boolean"))(int(1))))), int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end, colon at line end, lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if
         |      xs.exists:
         |        z => z > 0
         |    => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z > 0
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", int(0)))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after a typed pattern, colon at line end, case arrow at a smaller indent") {
    val code =
      """|def f(x: Any) = x match
         |  case s: String if g:
         |    s.length > 4
         |  => s
         |  case x => x.toString
         |""".stripMargin
    val layout =
      """|def f(x: Any) = x match {
         |  case s: String if g {
         |    s.length > 4
         |  } =>
         |    s
         |  case x =>
         |    x.toString
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Any"))))),
      None,
      tmatch(
        tname("x"),
        Case(
          Pat.Typed(patvar("s"), pname("String")),
          Some(tapply(tname("g"), blk(tinfix(tselect("s", "length"), ">", int(4))))),
          tname("s"),
        ),
        Case(patvar("x"), None, tselect("x", "toString")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after a typed pattern, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case s: String if xs.exists: z =>
         |    z == s
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case s: String if xs.exists {
         |    z => z == s
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          Pat.Typed(patvar("s"), pname("String")),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(tinfix(tname("z"), "==", tname("s")))),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after a typed pattern, no indents in match, colon at line end, case arrow at a smaller indent") {
    val code =
      """|def f(x: Any) =
         |  x match
         |  case s: String if g:
         |    s.length > 4
         |  => s
         |  case x => x.toString
         |""".stripMargin
    val layout =
      """|def f(x: Any) = x match {
         |  case s: String if g {
         |    s.length > 4
         |  } =>
         |    s
         |  case x =>
         |    x.toString
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Any"))))),
      None,
      tmatch(
        tname("x"),
        Case(
          Pat.Typed(patvar("s"), pname("String")),
          Some(tapply(tname("g"), blk(tinfix(tselect("s", "length"), ">", int(4))))),
          tname("s"),
        ),
        Case(patvar("x"), None, tselect("x", "toString")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, no indents in a nested match, colon lambda, case arrow on a line of its own") {
    val code =
      """|def test =
         |  list.map:
         |    case i: Int =>
         |      n match
         |      case j: Int
         |      if list.exists: k =>
         |        list.exists(_ == j && j == k)
         |      =>
         |        1
         |      case _ => 2
         |    case _ => 3
         |""".stripMargin
    val layout =
      """|def test = list.map {
         |  case i: Int =>
         |    n match {
         |      case j: Int if list.exists {
         |        k => list.exists(_ == j && j == k)
         |      } => 1
         |      case _ => 2
         |    }
         |  case _ => 3
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("test"),
      Nil,
      None,
      tapply(
        tselect("list", "map"),
        Term.PartialFunction(List(
          Case(
            Pat.Typed(patvar("i"), pname("Int")),
            None,
            tmatch(
              tname("n"),
              Case(
                Pat.Typed(patvar("j"), pname("Int")),
                Some(tapply(
                  tselect("list", "exists"),
                  blk(tfunc(tparam("k"))(tapply(
                    tselect("list", "exists"),
                    Term.AnonymousFunction(tinfix(
                      tinfix(Term.Placeholder(), "==", tname("j")),
                      "&&",
                      tinfix(tname("j"), "==", tname("k")),
                    )),
                  ))),
                )),
                int(1),
              ),
              Case(patwildcard, None, int(2)),
            ),
          ),
          Case(patwildcard, None, int(3)),
        )),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match with an end marker, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f =
         |  x match
         |    case y if xs.exists: z =>
         |      z
         |    => 1
         |  end match
         |""".stripMargin
    val layout =
      """|def f = {
         |  x match {
         |    case y if xs.exists {
         |      z => z
         |    } => 1
         |  }
         |  end match
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
            int(1),
          ),
        ),
        Term.EndMarker(tname("match")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, leading-dot line, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs
         |      .exists: z =>
         |        z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in catch with braces, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = try g catch {
         |  case y if ys.exists: z =>
         |    z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = try g catch {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tname("g"),
        List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        )),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in catch with finally, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = try g catch
         |  case y if ys.exists: z =>
         |    z
         |  => 1
         |finally h
         |""".stripMargin
    val layout =
      """|def f = try g catch {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |} finally h
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tname("g"),
        List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        )),
        Some(tname("h")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, inner guard colon lambda, inner case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z match
         |      case w if ws.exists: v =>
         |        v
         |      => true
         |      case _ => false
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z match {
         |      case w if ws.exists {
         |        v => v
         |      } => true
         |      case _ => false
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(tmatch(
              tname("z"),
              Case(
                patvar("w"),
                Some(tapply(tselect("ws", "exists"), blk(tfunc(tparam("v"))(tname("v"))))),
                bool(true),
              ),
              Case(patwildcard, None, bool(false)),
            ))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case clause with an inline then in a try, catch on the next line, next case") {
    val code =
      """|def f = x match
         |  case 1 => try if p then a
         |    catch case e => b
         |  case 2 => c
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    try if (p) a catch {
         |      case e => b
         |    }
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val body = Term.Try(
      Term.If(tname("p"), tname("a"), Lit.Unit(), Nil),
      List(Case(patvar("e"), None, tname("b"))),
      None,
    )
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(tname("x"), Case(int(1), None, body), Case(int(2), None, tname("c"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case clause with a try in an inline then, else on the next line, next case") {
    val code =
      """|def f = x match
         |  case 1 => if p then try a
         |    else b
         |  case 2 => c
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    if (p) try a else b
         |  case 2 =>
         |    c
         |}
         |""".stripMargin
    val body = Term.If(tname("p"), Term.Try(tname("a"), Nil, None), tname("b"), Nil)
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(tname("x"), Case(int(1), None, body), Case(int(2), None, tname("c"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case on the catch line, try with an inline then in the handler, finally on the next line") {
    val code =
      """|def g =
         |  try a catch case e => try if p then b
         |  finally c
         |""".stripMargin
    val layout =
      """|def g = try a catch {
         |  case e =>
         |    try if (p) b finally c
         |}
         |""".stripMargin
    val handler = Term.Try(Term.If(tname("p"), tname("b"), Lit.Unit(), Nil), Nil, Some(tname("c")))
    val tree = Defn.Def(
      Nil,
      tname("g"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, handler)), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case on the catch line, inline then with a try in the handler, else on the next line") {
    val code =
      """|def g =
         |  try a catch case e => if p then try b
         |  else c
         |""".stripMargin
    val layout =
      """|def g = try a catch {
         |  case e =>
         |    if (p) try b else c
         |}
         |""".stripMargin
    val handler = Term.If(tname("p"), Term.Try(tname("b"), Nil, None), tname("c"), Nil)
    val tree = Defn.Def(
      Nil,
      tname("g"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, handler)), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "case clause with a try in a match in an inline then, comment line, else at the case's indent",
  ) {
    val code =
      """|def f =
         |  if q then x match
         |  case 1 => try a
         |  // c
         |  else b
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Ident(q) [13..14)
         |KwThen [15..19)
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [30..34)
         |Constant.Int(1) [35..36)
         |RightArrow [37..39)
         |KwTry [40..43)
         |Ident(a) [44..45)
         |Indentation.Outdent [52..52)
         |KwElse [55..59)
         |Ident(b) [60..61)
         |Indentation.Outdent [61..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
  }

  test("case guard, colon lambda, comment line first, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    // c
         |    z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z =>
         |      // c
         |      z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(tname("z").toBuilder.begComment(detachedComments("// c")).result())),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, comment line at the body's indent before the case arrow") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |    // c
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => {
         |      z
         |      // c
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(blk(
              tname("z").toBuilder.endComment(detachedComments("// c")).result(),
            ))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, comment line at the case's indent before the case arrow") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |  // c
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |    // c
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(tname("z")).toBuilder.endComment(detachedComments("// c")).result()),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end in a match with braces, colon lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match {
         |  case y if
         |      xs.exists: z =>
         |        z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end in catch, colon lambda, case arrow at the case's indent") {
    val code =
      """|def f = try g catch
         |  case y if
         |      xs.exists: z =>
         |        z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = try g catch {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tname("g"),
        List(Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        )),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "case guard after if at line end, infix operand, colon lambda, case arrow at the case's indent",
  ) {
    val code =
      """|def f = x match
         |  case y if
         |      a && xs.exists: z =>
         |        z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end, colon lambda with two parameters, case arrow at the case's indent") {
    val code =
      """|def f = x match
         |  case y if
         |      m.exists: (k, v) =>
         |        v
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if m.exists {
         |    (k, v) => v
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("m", "exists"), blk(tfunc(tparam("k"), tparam("v"))(tname("v"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end in the second case, colon lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match
         |  case 0 => 0
         |  case y if
         |      xs.exists: z =>
         |        z
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 0 => 0
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(int(0), None, int(0)),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match in a case body, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y =>
         |    y match
         |      case w if ws.exists: z =>
         |        z => 1
         |  case _ => 2
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `outdent` found
         |        z => 1
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after an extractor pattern, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case Some(s) if xs.exists: z =>
         |    z == s
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case Some(s) if xs.exists {
         |    z => z == s
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patextract(tname("Some"), patvar("s")),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(tinfix(tname("z"), "==", tname("s")))),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, leading infix line, colon at line end, lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match
         |  case y if a
         |    && xs.exists:
         |      z => z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, trailing infix operator, colon at line end, lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match
         |  case y if a &&
         |      xs.exists:
         |        z => z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, leading-dot line, colon at line end, lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match
         |  case y if xs
         |      .exists:
         |        z => z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern, colon at line end, lambda, case arrow at the guard's indent") {
    val code =
      """|def f = x match
         |  case y
         |    if xs.exists:
         |      z => z
         |    => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern, colon at line end, lambda, blank line, infix line at the guard's indent") {
    val code =
      """|def f = x match
         |  case y
         |    if xs.exists:
         |      z => z > 0
         |
         |    && b => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z > 0
         |  } && b => 1
         |}
         |""".stripMargin
    val arg =
      tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tinfix(tname("z"), tname(">"), int(0)))))
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(tname("x"), Case(patvar("y"), Some(tinfix(arg, tname("&&"), tname("b"))), int(1))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, no indents in match, colon at line end, lambda, case arrow on a line of its own") {
    val code =
      """|a match
         |case n
         |if foo:
         |  b => true
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
    val tree = tmatch(
      tname("a"),
      Case(patvar("n"), Some(tapply(tname("foo"), blk(tfunc(tparam("b"))(bool(true))))), int(2)),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, leading infix line, colon at line end, case arrow at the case's indent") {
    val code =
      """|def f = x match
         |  case y if a
         |    && p:
         |      Boolean
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && p {
         |    Boolean
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(tname("a"), "&&", tapply(tname("p"), blk(tname("Boolean"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end in partial function, colon lambda, case arrow at the case's indent") {
    val code =
      """|def f = xs.collect {
         |  case y if
         |      ys.exists: z =>
         |        z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = xs.collect {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "collect"),
        Term.PartialFunction(List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end in a match with braces, infix operand, colon lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match {
         |  case y if
         |      a && xs.exists: z =>
         |        z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end in a match with braces, colon lambda, case arrow at the guard's indent") {
    val code =
      """|def f = x match {
         |  case y if
         |      xs.exists: z =>
         |        z
         |      => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match with braces, leading infix line, colon at line end, lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match {
         |  case y if a
         |    && xs.exists:
         |      z => z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern in a match with braces, colon at line end, lambda, case arrow at the guard's indent") {
    val code =
      """|def f = x match {
         |  case y
         |    if xs.exists:
         |      z => z
         |    => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern, colon at line end, lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match
         |  case y
         |    if xs.exists:
         |      z => z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end in colon argument of cases, colon lambda, case arrow at the case's indent") {
    val code =
      """|def f = xs.collect:
         |  case y if
         |      ys.exists: z =>
         |        z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = xs.collect {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "collect"),
        Term.PartialFunction(List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end in catch with braces, colon lambda, case arrow at the case's indent") {
    val code =
      """|def f = try g catch {
         |  case y if
         |      xs.exists: z =>
         |        z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = try g catch {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tname("g"),
        List(Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        )),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end on the brace line, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = xs.collect { case y if
         |      ys.exists: z =>
         |        z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = xs.collect {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "collect"),
        Term.PartialFunction(List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match with braces, trailing infix operator, colon at line end, lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match {
         |  case y if a &&
         |      xs.exists:
         |        z => z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match with braces, leading-dot line, colon at line end, lambda, case arrow at the case's indent") {
    val code =
      """|def f = x match {
         |  case y if xs
         |      .exists:
         |        z => z
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern in colon argument of cases, colon at line end, lambda, case arrow at the case's indent") {
    val code =
      """|def f = xs.collect:
         |  case y
         |    if ys.exists:
         |      z => z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = xs.collect {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "collect"),
        Term.PartialFunction(List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in colon argument of cases, colon at line end, lambda, case arrow at the case's indent") {
    val code =
      """|def f = xs.collect:
         |  case y if ys.exists:
         |    z => z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = xs.collect {
         |  case y if ys.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "collect"),
        Term.PartialFunction(List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in colon argument of cases, colon lambda, body is a lambda, case arrow at the case's indent") {
    val code =
      """|def f = xs.collect:
         |  case y if ys.exists: z =>
         |    z => w
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = xs.collect {
         |  case y if ys.exists {
         |    z => z => w
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "collect"),
        Term.PartialFunction(List(Case(
          patvar("y"),
          Some(tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tfunc(tparam("z"))(tname("w")))))),
          int(1),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "case guard, colon lambda, argument in braces over three lines, case arrow at a smaller indent",
  ) {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    ys.exists { w =>
         |      w == z
         |    }
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => ys.exists {
         |      w => w == z
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(tapply(
              tselect("ys", "exists"),
              blk(tfunc(tparam("w"))(tinfix(tname("w"), "==", tname("z")))),
            ))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match in parentheses, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = g(x match
         |  case y if xs.exists: z =>
         |    z
         |  => 1
         |)
         |""".stripMargin
    val layout =
      """|def f = g(x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |})
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tname("g"),
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
            int(1),
          ),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, infix operand, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if a && xs.map: x =>
         |    x => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    x => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, two lines, case arrow on the last line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    val w = z
         |    w => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    w => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, name, colon at line end, case arrow on the argument's line") {
    val code =
      """|def f = x match
         |  case y if p:
         |    Boolean => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    Boolean => 1
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, lambda, case arrow on the lambda's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    z => z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => z => 1
         |               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, apply, colon at line end, case arrow on the argument's line") {
    val code =
      """|def f = x match
         |  case y if f(a):
         |    b => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    b => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, infix operand, colon at line end, lambda, case arrow on the lambda's line") {
    val code =
      """|def f = x match
         |  case y if a && xs.exists:
         |    z => z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => z => 1
         |               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in a match with braces, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match {
         |  case y if xs.exists: z =>
         |    z => 1
         |}
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `}` found
         |}
         |^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard in a match with braces, colon at line end, lambda, case arrow on the lambda's line",
  ) {
    val code =
      """|def f = x match {
         |  case y if xs.exists:
         |    z => z => 1
         |}
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `}` found
         |}
         |^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in partial function, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = xs.collect {
         |  case y if ys.exists: z =>
         |    z => 1
         |}
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `}` found
         |}
         |^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in catch, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = try g catch
         |  case y if ys.exists: z =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in colon argument of cases, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = xs.collect:
         |  case y if ys.exists: z =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow at the end of the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z =>
         |    1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    1
         |     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, lambda, case arrow at the end of the lambda's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    z => z =>
         |      1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      1
         |       ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow on the body's line, another case") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z => 1
         |  case _ => 2
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `case` found
         |  case _ => 2
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda with a typed parameter, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: (z: Int) =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case clause, case arrow on the clause's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b => b => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    case b => b => 1
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case clause with a try and a catch in braces, case arrow on the clause's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b => try g(b) catch { case _: Exception => false } => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    case b => try g(b) catch { case _: Exception => false } => 1
         |                                                            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case clause with a try and a catch in braces, case arrow on its own line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b => try g(b) catch { case _: Exception => false }
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `outdent` expected but `=>` found
         |    => 1
         |    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, val of a lambda, case arrow on the last line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    val g = (a: Int) => a
         |    g(z) => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, val of a lambda, case arrow on the last line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    val g = (a: Int) => a
         |    g => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    g => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, lambda in parentheses, case arrow on the argument's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    ys.map(a => a).contains => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    ys.map(a => a).contains => 1
         |                            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, lambda in parentheses, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    ys.map(a => a).contains(z) => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow on the body's line, another body line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z => 1
         |    2
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    2
         |     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case arrow on the argument's line, another body line") {
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    y => 1
         |    2
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    2
         |     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda with a using parameter, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: (using z: Int) =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda with a placeholder parameter, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: _ =>
         |    true => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: _ =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard on the catch line, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = try g catch case y if ys.exists: z =>
         |  z => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `outdent` found
         |  z => 1
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case arrow on its own line at the argument's indent") {
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    y
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    => 1
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, backquoted name, colon at line end, case arrow on the argument's line") {
    val code =
      """|def f = x match
         |  case y if `p`:
         |    Boolean => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    Boolean => 1
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, val with an indented lambda, case arrow on the last line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    val g =
         |      (a: Int) => a
         |    g => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `outdent` found
         |    g => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, type apply, colon at line end, case arrow on the argument's line") {
    val code =
      """|def f = x match
         |  case y if p[Int]:
         |    Boolean => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    Boolean => 1
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, leading infix line, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if a
         |    && xs.exists: z =>
         |      z => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      z => 1
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard on the line after the pattern, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y
         |    if xs.exists: z =>
         |      z => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      z => 1
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow on its own line at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    => 1
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, try with a case on the catch line, case arrow on its own line at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    try z > 0
         |    catch case _: Exception => false
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, try with a case on the line after catch, case arrow on its own line at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    try z > 0
         |    catch
         |      case _: Exception => false
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, try with a catch in braces, case arrow after the closing brace") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    try z > 0
         |    catch { case _: Exception => false } => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, try with a case on the catch line, case arrow on its own line at the argument's indent") {
    val code =
      """|def f = x match
         |  case y if p:
         |    try q
         |    catch case _: Exception => false
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `outdent` expected but `=>` found
         |    => 1
         |    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow on its own line deeper than the body") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |      => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow and a comment at the end of the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z => // c
         |    1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    1
         |     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, lambda in braces, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    ys.map { a => a }.contains(z) => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda with two parameters, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if m.exists: (k, v) =>
         |    v => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    v => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, prefix operator, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if !xs.exists: z =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in the second case of a match with braces, colon at line end, lambda, case arrow on the lambda's line") {
    val code =
      """|def f = x match {
         |  case 0 => 0
         |  case y if xs.exists:
         |    z => z => 1
         |}
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `}` found
         |}
         |^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in the second case, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case 0 => 0
         |  case y if xs.exists: z =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in catch with braces, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = try g catch {
         |  case y if ys.exists: z =>
         |    z => 1
         |}
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `}` found
         |}
         |^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, no indents in match, colon lambda, case arrow on the body's line") {
    val code =
      """|a match
         |case n
         |if foo: b =>
         |  b => 2
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |  b => 2
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, no indents in match, colon at line end, case arrow on the argument's line") {
    val code =
      """|a match
         |case n
         |if foo:
         |  b => 2
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |  b => 2
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, two lines, case arrow on the last line") {
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    val w = y
         |    w => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    w => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, val of a match, case arrow on the last line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    val b = z match
         |      case 1 => true
         |      case _ => false
         |    b => 1
         |""".stripMargin
    val error =
      """|<input>:6: error: `=>` expected but `outdent` found
         |    b => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, infix line, case arrow on the infix line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |    && b => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, block comment before the case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z /* c */ => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z /* c */ => 1
         |                  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard, colon at line end, infix line with a colon lambda, case arrow on the body's line",
  ) {
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    y
         |  && ys.exists: z =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, apply, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if f(a): z =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, val with a context function type, case arrow on the last line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    val g: Int ?=> Int = 1
         |    g => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    g => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, match, case arrow on its own line at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z match
         |      case 1 => true
         |      case _ => false
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard, colon lambda, case arrow on the body's line, val of a lambda in the case body",
  ) {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z => 1
         |    val h = (a: Int) => a
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    val h = (a: Int) => a
         |                         ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, two lambda lines") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    z => z
         |    w => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    w => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after if at line end, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if
         |      xs.exists: z =>
         |        z => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |        z => 1
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case clause with a try and a case on the catch line, case arrow on its own line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b => try g(b) catch case _: Exception => false
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `outdent` expected but `=>` found
         |    => 1
         |    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, trailing infix operator, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if a &&
         |      xs.exists: z =>
         |        z => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |        z => 1
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, if-then-else, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    if z > 0 then z < 9 else false => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, if-then-else, case arrow on the argument's line") {
    val code =
      """|def f = x match
         |  case y if p:
         |    if a then b else c => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    if a then b else c => 1
         |                           ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, if-then-else over three lines, case arrow on the else line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    if z > 0 then
         |      z < 9
         |    else false => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard on the brace line, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = xs.collect { case y if ys.exists: z =>
         |    z => 1
         |}
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `}` found
         |}
         |^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in catch with finally, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = try g catch
         |  case y if ys.exists: z =>
         |    z => 1
         |finally h
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in a match before a statement, colon lambda, case arrow on the body's line") {
    val code =
      """|def f =
         |  x match
         |    case y if xs.exists: z =>
         |      z => 1
         |  g
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      z => 1
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, argument in parentheses over three lines, case arrow after the closing parenthesis") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    g(
         |      z
         |    ) => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, nested colon lambda, case arrow on the nested body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z.ys.exists: w =>
         |      w => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      w => 1
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after if at line end, colon at line end, case arrow on the argument's line") {
    val code =
      """|def f = x match
         |  case y if
         |      p:
         |        Boolean => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |        Boolean => 1
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after a typed pattern, colon at line end, case arrow on the argument's line") {
    val code =
      """|def f(x: Any) = x match
         |  case s: String if g:
         |    s.length > 4 => s
         |  case x => x.toString
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    s.length > 4 => s
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after a typed pattern, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case s: String if xs.exists: z =>
         |    z == s => 1
         |  case _ => 2
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case s: String if xs.exists: z =>
         |                             ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after a typed pattern, no indents in match, colon at line end, case arrow on the argument's line") {
    val code =
      """|def f(x: Any) =
         |  x match
         |  case s: String if g:
         |    s.length > 4 => s
         |  case x => x.toString
         |""".stripMargin
    val error =
      """|<input>:4: error: `outdent` expected but `=>` found
         |    s.length > 4 => s
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after an extractor pattern, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case Some(s) if xs.exists: z =>
         |    z == s => 1
         |  case _ => 2
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case Some(s) if xs.exists: z =>
         |                           ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, no indents in a nested match, colon lambda, case arrow on the body's line") {
    val code =
      """|def test =
         |  list.map:
         |    case i: Int =>
         |      n match
         |      case j: Int
         |      if list.exists: k =>
         |        list.exists(_ == j && j == k) =>
         |        1
         |      case _ => 2
         |    case _ => 3
         |""".stripMargin
    val error =
      """|<input>:6: error: `=>` expected but `:` found
         |      if list.exists: k =>
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in a match with an end marker, colon lambda, case arrow on the body's line") {
    val code =
      """|def f =
         |  x match
         |    case y if xs.exists: z =>
         |      z => 1
         |  end match
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      z => 1
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, leading-dot line, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs
         |      .exists: z =>
         |        z => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |        z => 1
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow at the end of the body's line, deeper case body") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z =>
         |      1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      1
         |       ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard, colon at line end, case arrow at the end of the argument's line, deeper case body",
  ) {
    val code =
      """|def f(x: Any) = x match
         |  case s: String if g:
         |    s.length > 4 =>
         |      s
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    s.length > 4 =>
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, comment on the guard line, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z => // c
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, comment on the guard line, case arrow on the argument's line") {
    val code =
      """|def f(x: Any) = x match
         |  case s: String if g: // c
         |    s.length > 4 => s
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    s.length > 4 => s
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard, colon lambda, inner guard colon lambda, inner case arrow on the inner body's line",
  ) {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z match
         |      case w if ws.exists: v =>
         |        v => true
         |      case _ => false
         |  => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case clause with a guard colon lambda, inner case arrow on the inner body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b if bs.exists: c =>
         |      c => true
         |    case _ => false
         |  => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `case` found
         |    case _ => false
         |    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard, colon lambda, inner guard colon lambda, outer case arrow at the body's indent",
  ) {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z match
         |      case w if ws.exists: v =>
         |        v => true
         |      case _ => false
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, last case clause with a guard colon lambda, inner case arrow on the inner body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b if bs.exists: c =>
         |      c => true
         |  => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      c => true
         |               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, last case clause with a guard colon lambda, inner case arrow on the inner body's line, outer case arrow at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b if bs.exists: c =>
         |      c => true
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `outdent` found
         |    => 1
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case clause, then a case clause with a guard colon lambda, inner case arrow on the inner body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case 0 => false
         |    case b if bs.exists: c =>
         |      c => true
         |  => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `outdent` found
         |      c => true
         |               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, last case clause with a guard colon at line end, inner case arrow on the inner body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b if bs.contains:
         |      b => true
         |  => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      b => true
         |               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, last case clause with a guard colon lambda, try and catch lines, inner case arrow on its own line, outer case arrow at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b if b.ws.exists: w =>
         |      try w > 0
         |      catch case _: Exception => false
         |      => true
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `:` found
         |    case b if b.ws.exists: w =>
         |                         ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, last case clause with a guard colon lambda, inner case arrow on the inner body's line before a try, outer case arrow at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b if b.ws.exists: w =>
         |      w > 0 => try g(b) catch case _: Exception => false
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `:` found
         |    case b if b.ws.exists: w =>
         |                         ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, inner guard colon lambda in the last case, inner case arrow on the inner body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z match
         |      case w if ws.exists: v =>
         |        v => true
         |  => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, inner guard colon lambda in the last case, inner case arrow on the inner body's line, outer case arrow at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z match
         |      case w if ws.exists: v =>
         |        v => true
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in a match with braces, colon lambda, inner guard colon lambda in the last case, inner case arrow on the inner body's line") {
    val code =
      """|def f = x match {
         |  case y if xs.exists: z =>
         |    z match
         |      case w if ws.exists: v =>
         |        v => true
         |  => 1
         |}
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, last case clause whose body matches, with a guard colon lambda in its last case, inner case arrow on the inner body's line, outer case arrow at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b => b match
         |      case c if cs.exists: d =>
         |        d => true
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `outdent` found
         |        d => true
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, last case clause whose body on the next line matches, with a guard colon lambda in its last case, inner case arrow on the inner body's line, outer case arrow at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b =>
         |      b match
         |        case c if cs.exists: d =>
         |          d => true
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:6: error: `=>` expected but `outdent` found
         |          d => true
         |                   ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in a match with braces, colon at line end, last case clause whose body matches, with a guard colon lambda in its last case, inner case arrow on the inner body's line, outer case arrow at the body's indent") {
    val code =
      """|def f = x match {
         |  case y if xs.exists:
         |    case b => b match
         |      case c if cs.exists: d =>
         |        d => true
         |    => 1
         |}
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `outdent` found
         |        d => true
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, try with a case on the catch line, inline then with a match in the handler, case arrow on its own line at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    try g(z) catch case e => if p then e match
         |      case X => true
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, try with a case on the catch line, try with catch at the line end in the handler, case arrow on its own line at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    try g(z) catch case e => try h(z) catch
         |      case X => true
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, try with a case on the catch line, try without catch in the handler, case arrow on its own line at the body's indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    try g(z) catch case e => try h(z)
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case clause with a try and a case on the catch line, inline then with a match in the handler, case arrow on its own line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b => try g(b) catch case e => if p then e match
         |        case X => true
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `outdent` expected but `=>` found
         |    => 1
         |    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case clause with a try and a case on the catch line, try with catch at the line end in the handler, case arrow on its own line") {
    val code =
      """|def f = x match
         |  case y if xs.exists:
         |    case b => try g(b) catch case e => try h(b) catch
         |        case X => true
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `outdent` expected but `=>` found
         |    => 1
         |    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, comment line first, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    // c
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, comment line first, case arrow on the argument's line") {
    val code =
      """|def f = x match
         |  case y if p:
         |    // c
         |    Boolean => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    Boolean => 1
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, leading infix line, colon at line end, case arrow on the argument's line") {
    val code =
      """|def f = x match
         |  case y if a
         |    && p:
         |      Boolean => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      Boolean => 1
         |                  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, argument in braces over three lines, case arrow after the closing brace") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    ys.exists { w =>
         |      w == z
         |    } => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in a match in parentheses, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = g(x match
         |  case y if xs.exists: z =>
         |    z => 1
         |)
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case arrow at the end of the argument's line, case body at the same indent") {
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    y =>
         |    1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    1
         |     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after an alternative pattern, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case A | B if xs.exists: z =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow at the end of the body's line, shallower case body") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |      z =>
         |    1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `integer constant` found
         |    1
         |    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after a binder pattern, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case s @ Some(_) if xs.exists: z =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard in a match with braces, colon lambda, case arrow on the body's line, another case",
  ) {
    val code =
      """|def f = x match {
         |  case y if xs.exists: z =>
         |    z => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `case` found
         |  case _ => 2
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after a pattern over two lines, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case Some(
         |         y) if xs.exists: z =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after an alternative pattern over two lines, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case A
         |     | B if xs.exists: z =>
         |         z => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |         z => 1
         |               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard, colon lambda, case arrow on the body's line, deeper select line in the case body",
  ) {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z => g
         |      .h
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      .h
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow on the body's line, deeper infix line in the case body") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z => a +
         |      b
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      b
         |       ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard, colon lambda, case arrow on the body's line, deeper argument line in the case body",
  ) {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z => g(
         |      1)
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      1)
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow on the body's line, colon lambda in the case body") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z => ys.foreach: w =>
         |      w
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      w
         |       ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case arrow on the argument's line, deeper select line in the case body") {
    val code =
      """|def f(x: Any) = x match
         |  case s: String if g:
         |    s.length > 4 => s
         |      .trim
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    s.length > 4 => s
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in a match with braces, colon lambda, case arrow on the body's line, deeper select line in the case body") {
    val code =
      """|def f = x match {
         |  case y if xs.exists: z =>
         |    z => g
         |      .h
         |  case _ => 2
         |}
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `case` found
         |  case _ => 2
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in a sub-case, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case Some(v) if v match
         |    case w if ws.exists: z =>
         |      z => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |      z => 1
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after a case with a sub-match, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case Some(v) if v match
         |    case 1 => 1
         |  case y if xs.exists: z =>
         |    z => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `outdent` found
         |    z => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, if with parentheses, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    if (z > 0) z < 9 else false => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, blank line, case arrow on the last line") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    val w = z
         |
         |    w => 1
         |""".stripMargin
    val error =
      """|<input>:5: error: `=>` expected but `outdent` found
         |    w => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, function type in brackets, case arrow on the body's line") {
    val code =
      """|def f = x match
         |  case y if fs.exists: f =>
         |    f.isInstanceOf[Int => Int] => 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if fs.exists: f =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, case arrow at the end of the body's line, empty case body, another case") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z > y =>
         |  case _ => g()
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, case arrow at the end of the argument's line, empty case body, another case") {
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    y =>
         |  case _ => g()
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `case` found
         |  case _ => g()
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in catch with braces, colon lambda, case arrow at the end of the body's line, empty case body") {
    val code =
      """|def f = try g() catch {
         |  case e if ignored.exists: c =>
         |    c == e =>
         |}
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case e if ignored.exists: c =>
         |                          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard, colon lambda, case arrow at the end of the body's line, empty case body at the end",
  ) {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z > y =>
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in a match in a colon lambda, colon lambda, case arrow on the body's line") {
    val code =
      """|def f = xs.foreach: x =>
         |  x match
         |    case y if ys.exists: z =>
         |      z > y => g(y)
         |    case _ => g(0)
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `if` found
         |    case y if ys.exists: z =>
         |           ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in the last case of a match in a colon lambda, colon lambda, case arrow on the body's line, select line after the lambda") {
    val code =
      """|def f = xs.map: x =>
         |    x match
         |      case 0 => 0
         |      case y if ys.exists: z =>
         |        z > y => 1
         |  .sum
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `if` found
         |      case y if ys.exists: z =>
         |             ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in the last case of a match in a colon lambda, colon at line end, case arrow on the argument's line, select line after the lambda") {
    val code =
      """|def f = xs.map: x =>
         |    x match
         |      case 0 => 0
         |      case y if ys.exists:
         |        _ > y => 1
         |  .sum
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `if` found
         |      case y if ys.exists:
         |             ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard in the last case of a match in a colon argument, colon lambda, case arrow on the body's line, select line after the argument") {
    val code =
      """|def f = Option:
         |    x match
         |      case 0 => 0
         |      case y if ys.exists: z =>
         |        z > y => 1
         |  .get
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `:` found
         |      case y if ys.exists: z =>
         |                         ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "case guard after a case with a colon lambda body, colon lambda, case arrow on the body's line",
  ) {
    val code =
      """|def f = x match
         |  case 0 => xs.foreach: z =>
         |      g(z)
         |  case y if ys.exists: z =>
         |    z > y => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `:` found
         |  case y if ys.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon at line end, placeholder function, case arrow on the argument's line") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if xs.exists:
         |    _ > y => 1
         |  case _ => 2
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    _ > y => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon lambda, placeholder functions in the body, case arrow on the body's line") {
    val code =
      """|def f(x: Int, xs: List[List[Int]]) = x match
         |  case y if xs.exists: z =>
         |    z.exists(_ > y) && z.forall(_ < 9) => 1
         |  case _ => 2
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if xs.exists: z =>
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard, colon context lambda, case arrow on the body's line") {
    val code =
      """|def f(x: Int, g: (Int ?=> Boolean) => Boolean) = x match
         |  case y if g: (z: Int) ?=>
         |    z > y => 1
         |  case _ => 2
         |""".stripMargin
    val error =
      """|<input>:2: error: `=>` expected but `:` found
         |  case y if g: (z: Int) ?=>
         |             ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala3Future: case guard, infix operand, colon lambda, case arrow on the body's line") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if a && xs.map: x =>
         |    x => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    x => 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala3Future: case guard, colon lambda, case arrow at the end of the body's line") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z =>
         |    1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    1
         |     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala3Future: case guard, colon lambda, case arrow on its own line at the body's indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    => 1
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala3Future: case guard, name, colon at line end, case arrow on the argument's line") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if p:
         |    Boolean => 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `=>` expected but `outdent` found
         |    Boolean => 1
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala3Future: case guard, colon at line end, case arrow at the end of the argument's line, deeper case body") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f(x: Any) = x match
         |  case s: String if g:
         |    s.length > 4 =>
         |      s
         |""".stripMargin
    val error =
      """|<input>:3: error: `outdent` expected but `=>` found
         |    s.length > 4 =>
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("Scala3Future: case guard, colon at line end, case arrow on its own line at the argument's indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    y
         |    => 1
         |""".stripMargin
    val error =
      """|<input>:4: error: `=>` expected but `outdent` found
         |    => 1
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after an alternative pattern over two lines, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case A
         |     | B if xs.exists: z =>
         |         z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case A | B if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          Pat.Alternative(tname("A"), tname("B")),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after a pattern over two lines, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case Some(
         |         y) if xs.exists: z =>
         |    z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case Some(y) if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patextract(tname("Some"), patvar("y")),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "case guard, leading infix lines at two indents, colon lambda, case arrow at a smaller indent",
  ) {
    val code =
      """|def f = x match
         |  case y if a
         |        && b
         |    && xs.exists: z =>
         |      z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && b && xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tinfix(tname("a"), "&&", tname("b")),
            "&&",
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after a binder pattern, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case s @ Some(_) if xs.exists: z =>
         |    z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case s @ Some(_) if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          Pat.Bind(patvar("s"), patextract(tname("Some"), patwildcard)),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, sub-match after the guard") {
    val code =
      """|def f = x match
         |  case Some(v) if xs.exists: z =>
         |      z == v
         |    if v match
         |      case 1 => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case Some(v) if xs.exists {
         |    z => z == v
         |  } if v match {
         |    case 1 => 1
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patextract(tname("Some"), patvar("v")),
          Some(tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("z"))(tinfix(tname("z"), "==", tname("v")))),
          )),
          tsubmatch(tname("v"), Case(int(1), None, int(1))),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, blank line, case arrow at a smaller indent") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, lambda in braces over three lines, case arrow after the closing brace") {
    val code =
      """|def f = x match {
         |  case y if xs.exists { z =>
         |    z
         |  } => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end, colon lambda, case arrow between the case's and the guard's indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if
         |      xs.exists: z =>
         |        z > y
         |    => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.exists {
         |    z => z > y
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y"))))),
          ),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern at the case's indent, colon lambda, case arrow at the case's indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y
         |  if xs.exists: z =>
         |    z > y
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.exists {
         |    z => z > y
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y"))))),
          ),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern at the case's indent, colon lambda, nested colon lambda, case arrow at the case's indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y
         |  if xs.forall: w =>
         |    xs.exists: z =>
         |      z > w
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.forall {
         |    w => xs.exists {
         |      z => z > w
         |    }
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "forall"),
            blk(tfunc(tparam("w"))(tapply(
              tselect("xs", "exists"),
              blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("w")))),
            ))),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern, colon lambda, case arrow at the guard's indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y
         |    if xs.exists: z =>
         |      z > y
         |    => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.exists {
         |    z => z > y
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y"))))),
          ),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern, colon lambda, nested colon lambda, case arrow at the guard's indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y
         |    if xs.forall: w =>
         |      xs.exists: z =>
         |        z > w
         |    => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.forall {
         |    w => xs.exists {
         |      z => z > w
         |    }
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "forall"),
            blk(tfunc(tparam("w"))(tapply(
              tselect("xs", "exists"),
              blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("w")))),
            ))),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, deeper nested colon lambda, case arrow at a smaller indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if xs.forall: w =>
         |      xs.exists: z =>
         |        z > w
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.forall {
         |    w => xs.exists {
         |      z => z > w
         |    }
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "forall"),
            blk(tfunc(tparam("w"))(tapply(
              tselect("xs", "exists"),
              blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("w")))),
            ))),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "case guard, colon lambda, deeper val of a lambda, case arrow at a smaller indent, another case",
  ) {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if xs.forall: w =>
         |      val g = (z: Int) => z > w
         |      xs.exists(g)
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.forall { w =>
         |    val g = (z: Int) => z > w
         |    xs.exists(g)
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "forall"),
            blk(tfunc(tparam("w"))(blk(
              Defn.Val(
                Nil,
                List(patvar("g")),
                None,
                tfunc(tparam("z", pname("Int")))(tinfix(tname("z"), ">", tname("w"))),
              ),
              tapply(tselect("xs", "exists"), tname("g")),
            ))),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, leading infix line, colon lambda, val with a function type, case arrow at the case's indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match
         |  case y if a
         |    && xs.forall: w =>
         |      val p: Int => Boolean = _ > w
         |      xs.exists(p)
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a && xs.forall { w =>
         |    val p: Int => Boolean = _ > w
         |    xs.exists(p)
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(
        tparam("x", pname("Int")),
        tparam("xs", papply(pname("List"), pname("Int"))),
        tparam("a", pname("Boolean")),
      ))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(
              tselect("xs", "forall"),
              blk(tfunc(tparam("w"))(blk(
                Defn.Val(
                  Nil,
                  List(patvar("p")),
                  Some(pfunc(pname("Int"))(pname("Boolean"))),
                  Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", tname("w"))),
                ),
                tapply(tselect("xs", "exists"), tname("p")),
              ))),
            ),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, leading infix line, colon lambda, nested colon lambda, case arrow at the case's indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match
         |  case y if a
         |    && xs.forall: w =>
         |      xs.exists: z =>
         |        z > w
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a && xs.forall {
         |    w => xs.exists {
         |      z => z > w
         |    }
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(
        tparam("x", pname("Int")),
        tparam("xs", papply(pname("List"), pname("Int"))),
        tparam("a", pname("Boolean")),
      ))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(
              tselect("xs", "forall"),
              blk(tfunc(tparam("w"))(tapply(
                tselect("xs", "exists"),
                blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("w")))),
              ))),
            ),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, leading infix line, colon lambda, val of a lambda, case arrow at the case's indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match
         |  case y if a
         |    && xs.forall: w =>
         |      val g = (z: Int) => z > w
         |      xs.exists(g)
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a && xs.forall { w =>
         |    val g = (z: Int) => z > w
         |    xs.exists(g)
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(
        tparam("x", pname("Int")),
        tparam("xs", papply(pname("List"), pname("Int"))),
        tparam("a", pname("Boolean")),
      ))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(
              tselect("xs", "forall"),
              blk(tfunc(tparam("w"))(blk(
                Defn.Val(
                  Nil,
                  List(patvar("g")),
                  None,
                  tfunc(tparam("z", pname("Int")))(tinfix(tname("z"), ">", tname("w"))),
                ),
                tapply(tselect("xs", "exists"), tname("g")),
              ))),
            ),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, leading infix line, colon lambda, lambda in parentheses, case arrow at the case's indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match
         |  case y if a
         |    && xs.forall: w =>
         |      xs.exists(z => z > w)
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a && xs.forall {
         |    w => xs.exists(z => z > w)
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(
        tparam("x", pname("Int")),
        tparam("xs", papply(pname("List"), pname("Int"))),
        tparam("a", pname("Boolean")),
      ))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(
              tselect("xs", "forall"),
              blk(tfunc(tparam("w"))(
                tapply(tselect("xs", "exists"), tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("w")))),
              )),
            ),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern, colon at line end, placeholder function, case arrow on a line of its own, colon lambda in the case body") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y
         |    if xs.exists:
         |      _ > y
         |    =>
         |      xs.count: z =>
         |        z > y
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.exists {
         |    _ > y
         |  } =>
         |    xs.count {
         |      z => z > y
         |    }
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            blk(Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", tname("y")))),
          )),
          tapply(tselect("xs", "count"), blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y"))))),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end without a colon, case arrow on a line of its own, val of a lambda in the case body") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if
         |      xs.exists(_ > y)
         |  =>
         |    val g = (z: Int) => z > y
         |    xs.count(g)
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.exists(_ > y) =>
         |    val g = (z: Int) => z > y
         |    xs.count(g)
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", tname("y"))),
          )),
          blk(
            Defn.Val(
              Nil,
              List(patvar("g")),
              None,
              tfunc(tparam("z", pname("Int")))(tinfix(tname("z"), ">", tname("y"))),
            ),
            tapply(tselect("xs", "count"), tname("g")),
          ),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, deeper body, case arrow at a smaller indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if xs.exists: z =>
         |      z > y
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.exists {
         |    z => z > y
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y"))))),
          ),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, case arrow at a smaller indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if xs.exists: z =>
         |    z > y
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.exists {
         |    z => z > y
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y"))))),
          ),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end without a colon, leading infix line left of the guard, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, a: Boolean, b: Boolean) = x match
         |  case y if
         |      a
         |    && b
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, a: Boolean, b: Boolean) = x match {
         |  case y if a && b => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(
        tpc(tparam("x", pname("Int")), tparam("a", pname("Boolean")), tparam("b", pname("Boolean"))),
      )),
      None,
      tmatch(
        tname("x"),
        Case(patvar("y"), Some(tinfix(tname("a"), "&&", tname("b"))), int(1)),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end without a colon, trailing infix operator, next line left of the guard, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, a: Boolean, b: Boolean) = x match
         |  case y if
         |      a &&
         |    b
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, a: Boolean, b: Boolean) = x match {
         |  case y if a && b => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(
        tpc(tparam("x", pname("Int")), tparam("a", pname("Boolean")), tparam("b", pname("Boolean"))),
      )),
      None,
      tmatch(
        tname("x"),
        Case(patvar("y"), Some(tinfix(tname("a"), "&&", tname("b"))), int(1)),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end without a colon, val with a function type, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if
         |      val p: Int => Boolean = _ > y
         |      xs.exists(p)
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val error =
      """|<input>:3: error: illegal start of simple expression
         |      val p: Int => Boolean = _ > y
         |      ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after if at line end without a colon, val of a lambda, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if
         |      val g = (z: Int) => z > y
         |      xs.exists(g)
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val error =
      """|<input>:3: error: illegal start of simple expression
         |      val g = (z: Int) => z > y
         |      ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("case guard after if at line end without a colon, match, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if
         |      y match
         |        case 0 => true
         |        case _ => false
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if (y match {
         |    case 0 => true
         |    case _ => false
         |  }) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tmatch(tname("y"), Case(int(0), None, bool(true)), Case(patwildcard, None, bool(false)))),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard after if at line end without a colon, lambda in parentheses, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if
         |      xs.exists(z => z > y)
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.exists(z => z > y) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y"))))),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard with a match at line end, lambda in parentheses in an inner case, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if y match
         |    case 0 => xs.exists(z => z > y)
         |    case _ => false
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if (y match {
         |    case 0 =>
         |      xs.exists(z => z > y)
         |    case _ =>
         |      false
         |  }) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tmatch(
            tname("y"),
            Case(
              int(0),
              None,
              tapply(tselect("xs", "exists"), tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y")))),
            ),
            Case(patwildcard, None, bool(false)),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard with a match at line end, val with a function type in an inner case, case arrow left of the inner cases") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if y match
         |      case 0 =>
         |        val p: Int => Boolean = _ > y
         |        xs.exists(p)
         |      case _ => false
         |    => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if (y match {
         |    case 0 =>
         |      val p: Int => Boolean = _ > y
         |      xs.exists(p)
         |    case _ =>
         |      false
         |  }) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tmatch(
            tname("y"),
            Case(
              int(0),
              None,
              blk(
                Defn.Val(
                  Nil,
                  List(patvar("p")),
                  Some(pfunc(pname("Int"))(pname("Boolean"))),
                  Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", tname("y"))),
                ),
                tapply(tselect("xs", "exists"), tname("p")),
              ),
            ),
            Case(patwildcard, None, bool(false)),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard with a match at line end, colon lambda in an inner case, case arrow left of the inner cases") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if y match
         |      case 0 => xs.exists: z =>
         |        z > y
         |      case _ => false
         |    => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if (y match {
         |    case 0 =>
         |      xs.exists {
         |        z => z > y
         |      }
         |    case _ =>
         |      false
         |  }) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tmatch(
            tname("y"),
            Case(
              int(0),
              None,
              tapply(
                tselect("xs", "exists"),
                blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y")))),
              ),
            ),
            Case(patwildcard, None, bool(false)),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard with a match at line end, lambda in parentheses in an inner case, case arrow left of the inner cases") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if y match
         |      case 0 => xs.exists(z => z > y)
         |      case _ => false
         |    => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if (y match {
         |    case 0 =>
         |      xs.exists(z => z > y)
         |    case _ =>
         |      false
         |  }) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tmatch(
            tname("y"),
            Case(
              int(0),
              None,
              tapply(tselect("xs", "exists"), tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y")))),
            ),
            Case(patwildcard, None, bool(false)),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, leading infix line, colon lambda, val with a context function type, case arrow at the case's indent, another case") {
    val code =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match
         |  case y if a
         |    && xs.forall: w =>
         |      val p: Int ?=> Boolean = summon[Int] > w
         |      xs.exists(z => p(using z))
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a && xs.forall { w =>
         |    val p: Int ?=> Boolean = summon[Int] > w
         |    xs.exists(z => p(using z))
         |  } => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(
        tparam("x", pname("Int")),
        tparam("xs", papply(pname("List"), pname("Int"))),
        tparam("a", pname("Boolean")),
      ))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(
              tselect("xs", "forall"),
              blk(tfunc(tparam("w"))(blk(
                Defn.Val(
                  Nil,
                  List(patvar("p")),
                  Some(pctxfunc(pname("Int"))(pname("Boolean"))),
                  tinfix(tapplytype(tname("summon"), pname("Int")), ">", tname("w")),
                ),
                tapply(tselect("xs", "exists"), tfunc(tparam("z"))(tapplyUsing(tname("p"), tname("z")))),
              ))),
            ),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard, colon lambda, case arrow on a line of its own at a smaller indent, empty case body, another case") {
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z > y
         |  =>
         |  case _ => g()
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z > y
         |  } =>
         |  case _ =>
         |    g()
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y"))))),
          ),
          blk(),
        ),
        Case(patwildcard, None, tapply(tname("g"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "case guard in catch with braces, colon lambda, case arrow at a smaller indent, empty case body",
  ) {
    val code =
      """|def f = try g() catch {
         |  case e if ignored.exists: c =>
         |    c == e
         |  =>
         |}
         |""".stripMargin
    val layout =
      """|def f = try g() catch {
         |  case e if ignored.exists {
         |    c => c == e
         |  } =>
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tapply(tname("g")),
        List(Case(
          patvar("e"),
          Some(tapply(
            tselect("ignored", "exists"),
            blk(tfunc(tparam("c"))(tinfix(tname("c"), "==", tname("e")))),
          )),
          blk(),
        )),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match in a colon lambda, colon lambda, case arrow at a smaller indent") {
    val code =
      """|def f = xs.foreach: x =>
         |  x match
         |    case y if ys.exists: z =>
         |      z > y
         |    => g(y)
         |    case _ => g(0)
         |""".stripMargin
    val layout =
      """|def f = xs.foreach {
         |  x => x match {
         |    case y if ys.exists {
         |      z => z > y
         |    } =>
         |      g(y)
         |    case _ =>
         |      g(0)
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam("x"))(tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tapply(
              tselect("ys", "exists"),
              blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y")))),
            )),
            tapply(tname("g"), tname("y")),
          ),
          Case(patwildcard, None, tapply(tname("g"), int(0))),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "case guard after a case with a colon lambda body, colon lambda, case arrow at a smaller indent",
  ) {
    val code =
      """|def f = x match
         |  case 0 => xs.foreach: z =>
         |      g(z)
         |  case y if ys.exists: z =>
         |    z > y
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 0 =>
         |    xs.foreach {
         |      z => g(z)
         |    }
         |  case y if ys.exists {
         |    z => z > y
         |  } =>
         |    1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          int(0),
          None,
          tapply(tselect("xs", "foreach"), blk(tfunc(tparam("z"))(tapply(tname("g"), tname("z"))))),
        ),
        Case(
          patvar("y"),
          Some(
            tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y"))))),
          ),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match with braces, leading infix line, colon at line end, val with a function type, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a
         |    && xs.exists:
         |      val p: Int => Boolean = _ > y
         |      p(y)
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a && xs.exists {
         |    val p: Int => Boolean = _ > y
         |    p(y)
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(
        tparam("x", pname("Int")),
        tparam("xs", papply(pname("List"), pname("Int"))),
        tparam("a", pname("Boolean")),
      ))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(
              tselect("xs", "exists"),
              blk(
                Defn.Val(
                  Nil,
                  List(patvar("p")),
                  Some(pfunc(pname("Int"))(pname("Boolean"))),
                  Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", tname("y"))),
                ),
                tapply(tname("p"), tname("y")),
              ),
            ),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match with braces, leading infix line, colon lambda, val with a function type, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a
         |    && xs.forall: w =>
         |      val p: Int => Boolean = _ > w
         |      xs.exists(p)
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a && xs.forall { w =>
         |    val p: Int => Boolean = _ > w
         |    xs.exists(p)
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(
        tparam("x", pname("Int")),
        tparam("xs", papply(pname("List"), pname("Int"))),
        tparam("a", pname("Boolean")),
      ))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(
              tselect("xs", "forall"),
              blk(tfunc(tparam("w"))(blk(
                Defn.Val(
                  Nil,
                  List(patvar("p")),
                  Some(pfunc(pname("Int"))(pname("Boolean"))),
                  Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", tname("w"))),
                ),
                tapply(tselect("xs", "exists"), tname("p")),
              ))),
            ),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard in a match with braces, leading infix line, colon lambda, nested colon lambda, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a
         |    && xs.forall: w =>
         |      xs.exists: z =>
         |        z > w
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int], a: Boolean) = x match {
         |  case y if a && xs.forall {
         |    w => xs.exists {
         |      z => z > w
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(
        tparam("x", pname("Int")),
        tparam("xs", papply(pname("List"), pname("Int"))),
        tparam("a", pname("Boolean")),
      ))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tname("a"),
            "&&",
            tapply(
              tselect("xs", "forall"),
              blk(tfunc(tparam("w"))(tapply(
                tselect("xs", "exists"),
                blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("w")))),
              ))),
            ),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case guard on the line after the pattern at the case's indent in a match with braces, colon lambda, nested colon lambda, case arrow at the case's indent") {
    val code =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y
         |  if xs.forall: w =>
         |    xs.exists: z =>
         |      z > w
         |  => 1
         |}
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.forall {
         |    w => xs.exists {
         |      z => z > w
         |    }
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "forall"),
            blk(tfunc(tparam("w"))(tapply(
              tselect("xs", "exists"),
              blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("w")))),
            ))),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: case guard, colon lambda, case arrow at a smaller indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: case guard, colon lambda, case arrow after an infix line at a smaller indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if xs.exists: z =>
         |    z
         |  && b => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } && b => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z")))),
            "&&",
            tname("b"),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: case guard, colon lambda, case arrow after a select line at a smaller indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if xs.map: z =>
         |    z + 1
         |  .contains(3) => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.map {
         |    z => z + 1
         |  }.contains(3) => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect(
              tapply(tselect("xs", "map"), blk(tfunc(tparam("z"))(tinfix(tname("z"), "+", int(1))))),
              "contains",
            ),
            int(3),
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: case guard, colon at line end, case arrow at a smaller indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    y
         |  => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.contains {
         |    y
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(patvar("y"), Some(tapply(tselect("xs", "contains"), blk(tname("y")))), int(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: case guard, colon at line end, case arrow after an infix line at a smaller indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if xs.contains:
         |    y
         |  && b => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.contains {
         |    y
         |  } && b => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(tapply(tselect("xs", "contains"), blk(tname("y"))), "&&", tname("b"))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala3Future: case guard, colon at line end, case arrow after a select line at a smaller indent") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|def f = x match
         |  case y if xs.filter:
         |    _ > 0
         |  .nonEmpty => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.filter {
         |    _ > 0
         |  }.nonEmpty => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tselect(
            tapply(
              tselect("xs", "filter"),
              blk(Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", int(0)))),
            ),
            "nonEmpty",
          )),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard, lambda in braces, case arrow after the braces") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f = x match
         |  case y if xs.exists { z => z } => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: if condition, colon lambda, then at a smaller indent") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f =
         |  if xs.exists: z =>
         |    z
         |  then 1 else 2
         |""".stripMargin
    val layout = "def f = if (xs.exists: (z => z)) 1 else 2"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.If(
        Term.Ascribe(tselect("xs", "exists"), pfunc(pname("z"))(pname("z"))),
        int(1),
        int(2),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: parens: case guard, infix operand, typed name, function type arrow at end of line") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f = x match
         |  case y if a && (g: A =>
         |    B) => g => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if a && (g: (A => B)) =>
         |    g => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tinfix(tname("a"), "&&", Term.Ascribe(tname("g"), pfunc(pname("A"))(pname("B"))))),
          tfunc(tparam("g"))(int(1)),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: parens: case guard, select colon, lambda arrow at end of line") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f = x match
         |  case y if (ys.exists: z =>
         |      z.ok) => 1
         |  case _ => 0
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if (ys.exists: (z => z.ok)) => 1
         |  case _ => 0
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(Term.Ascribe(tselect("ys", "exists"), pfunc(pname("z"))(pselect("z", "ok")))),
          int(1),
        ),
        Case(patwildcard, None, int(0)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard after if at line end, lambda in parentheses, case arrow on the guard's line") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f = x match
         |  case y if
         |      xs.exists(z => z) => 1
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists(z => z) => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), tfunc(tparam("z"))(tname("z")))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard, lambda in braces over three lines, case arrow after the closing brace") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f = x match {
         |  case y if xs.exists { z =>
         |    z
         |  } => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard after if at line end without a colon, case arrow on a line of its own, val of a lambda in the case body") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if
         |      xs.exists(_ > y)
         |  =>
         |    val g = (z: Int) => z > y
         |    xs.count(g)
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.exists(_ > y) =>
         |    val g = (z: Int) => z > y
         |    xs.count(g)
         |  case _ =>
         |    2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(
            tselect("xs", "exists"),
            Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", tname("y"))),
          )),
          blk(
            Defn.Val(
              Nil,
              List(patvar("g")),
              None,
              tfunc(tparam("z", pname("Int")))(tinfix(tname("z"), ">", tname("y"))),
            ),
            tapply(tselect("xs", "count"), tname("g")),
          ),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard after if at line end without a colon, leading infix line left of the guard, case arrow at the case's indent") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f(x: Int, a: Boolean, b: Boolean) = x match
         |  case y if
         |      a
         |    && b
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, a: Boolean, b: Boolean) = x match {
         |  case y if a && b => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(
        tpc(tparam("x", pname("Int")), tparam("a", pname("Boolean")), tparam("b", pname("Boolean"))),
      )),
      None,
      tmatch(
        tname("x"),
        Case(patvar("y"), Some(tinfix(tname("a"), "&&", tname("b"))), int(1)),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard after if at line end without a colon, trailing infix operator, next line left of the guard, case arrow at the case's indent") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f(x: Int, a: Boolean, b: Boolean) = x match
         |  case y if
         |      a &&
         |    b
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, a: Boolean, b: Boolean) = x match {
         |  case y if a && b => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(
        tpc(tparam("x", pname("Int")), tparam("a", pname("Boolean")), tparam("b", pname("Boolean"))),
      )),
      None,
      tmatch(
        tname("x"),
        Case(patvar("y"), Some(tinfix(tname("a"), "&&", tname("b"))), int(1)),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard after if at line end without a colon, match, case arrow at the case's indent") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if
         |      y match
         |        case 0 => true
         |        case _ => false
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if (y match {
         |    case 0 => true
         |    case _ => false
         |  }) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tmatch(tname("y"), Case(int(0), None, bool(true)), Case(patwildcard, None, bool(false)))),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard after if at line end without a colon, lambda in parentheses, case arrow at the case's indent") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if
         |      xs.exists(z => z > y)
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if xs.exists(z => z > y) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y"))))),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard with a match at line end, lambda in parentheses in an inner case, case arrow at the case's indent") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if y match
         |    case 0 => xs.exists(z => z > y)
         |    case _ => false
         |  => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if (y match {
         |    case 0 =>
         |      xs.exists(z => z > y)
         |    case _ =>
         |      false
         |  }) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tmatch(
            tname("y"),
            Case(
              int(0),
              None,
              tapply(tselect("xs", "exists"), tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y")))),
            ),
            Case(patwildcard, None, bool(false)),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard with a match at line end, val with a function type in an inner case, case arrow left of the inner cases") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if y match
         |      case 0 =>
         |        val p: Int => Boolean = _ > y
         |        xs.exists(p)
         |      case _ => false
         |    => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if (y match {
         |    case 0 =>
         |      val p: Int => Boolean = _ > y
         |      xs.exists(p)
         |    case _ =>
         |      false
         |  }) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tmatch(
            tname("y"),
            Case(
              int(0),
              None,
              blk(
                Defn.Val(
                  Nil,
                  List(patvar("p")),
                  Some(pfunc(pname("Int"))(pname("Boolean"))),
                  Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", tname("y"))),
                ),
                tapply(tselect("xs", "exists"), tname("p")),
              ),
            ),
            Case(patwildcard, None, bool(false)),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala32: case guard with a match at line end, lambda in parentheses in an inner case, case arrow left of the inner cases") {
    implicit val dialect: Dialect = dialects.Scala32
    val code =
      """|def f(x: Int, xs: List[Int]) = x match
         |  case y if y match
         |      case 0 => xs.exists(z => z > y)
         |      case _ => false
         |    => 1
         |  case _ => 2
         |""".stripMargin
    val layout =
      """|def f(x: Int, xs: List[Int]) = x match {
         |  case y if (y match {
         |    case 0 =>
         |      xs.exists(z => z > y)
         |    case _ =>
         |      false
         |  }) => 1
         |  case _ => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(pcg(tpc(tparam("x", pname("Int")), tparam("xs", papply(pname("List"), pname("Int")))))),
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tmatch(
            tname("y"),
            Case(
              int(0),
              None,
              tapply(tselect("xs", "exists"), tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y")))),
            ),
            Case(patwildcard, None, bool(false)),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Scala213: case guard, lambda in braces over three lines, case arrow after the closing brace") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|def f = x match {
         |  case y if xs.exists { z =>
         |    z
         |  } => 1
         |}
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs.exists {
         |    z => z
         |  } => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        tname("x"),
        Case(
          patvar("y"),
          Some(tapply(tselect("xs", "exists"), blk(tfunc(tparam("z"))(tname("z"))))),
          int(1),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
