package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.parsers.Parse

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

  test("SIP-75 single-line lambda, leading select on the next line") {
    val code =
      """|xs.map: x => x + 1
         |.filter: y => y > 0
         |""".stripMargin
    val layout =
      """|xs.map {
         |  x => x + 1.filter {
         |    y => y > 0
         |  }
         |}
         |""".stripMargin
    val tree = tapply(
      tselect("xs", "map"),
      blk(tfunc(tparam("x"))(tinfix(
        "x",
        "+",
        tapply(tselect(lit(1), "filter"), blk(tfunc(tparam("y"))(tinfix("y", ">", lit(0))))),
      ))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: body continues inside parens") {
    val code =
      """|xs.map: x => foo(
         |  bar(x)
         |    .baz
         |)
         |""".stripMargin
    val layout =
      """|xs.map {
         |  x => foo(bar(x).baz)
         |}
         |""".stripMargin
    val tree = tapply(
      tselect("xs", "map"),
      blk(tfunc(tparam("x"))(tapply("foo", tselect(tapply("bar", "x"), "baz")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case on the same line") {
    val code =
      """|val used = score.filter: case (_, (r, w)) => r > 0 || w > 0
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |val used = score.filter: case (_, (r, w)) => r > 0 || w > 0
         |                         ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case with guard") {
    val code =
      """|xs.collect: case (a, b) if b > 2 => a
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |xs.collect: case (a, b) if b > 2 => a
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case, leading select on the next line") {
    val code =
      """|val one = list
         |  .collect: case x => (x, x + 1)
         |  .toMap
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  .collect: case x => (x, x + 1)
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: indented case body") {
    val code =
      """|val three = list
         |  .collect: case x =>
         |    val y = x + 1
         |    (x, y)
         |  .toMap
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  .collect: case x =>
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case expression") {
    val code =
      """|val partial = case (a, b) if b > 2 => a
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |val partial = case (a, b) if b > 2 => a
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case expression in parens") {
    val code =
      """|xs.collect(case (a, b) if b > 2 => a)
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |xs.collect(case (a, b) if b > 2 => a)
         |           ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case expression indented in parens") {
    val code =
      """|xs.collect(
         |  case (a, b) =>
         |    a
         |)
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  case (a, b) =>
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 curried multi-line lambda") {
    val code =
      """|fun: (x: Int) => y =>
         |  x + y
         |""".stripMargin
    val layout =
      """|fun {
         |  (x: Int) => y => x + y
         |}
         |""".stripMargin
    val tree = tapply("fun", blk(tfunc(tparam("x", "Int"))(tfunc(tparam("y"))(tinfix("x", "+", "y")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line polymorphic lambda") {
    val code =
      """|xs.map: [T] => (x: T) => List(x)
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `[` found
         |xs.map: [T] => (x: T) => List(x)
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: def rhs, name param") {
    val code =
      """|def f = g: x => x + 1
         |""".stripMargin
    val layout =
      """|def f = g {
         |  x => x + 1
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply("g", blk(tfunc(tparam("x"))(tinfix("x", "+", lit(1))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in braces, closed on the same line") {
    val code =
      """|def f = { g: x => x + 1 }
         |""".stripMargin
    val layout =
      """|def f = {
         |  g {
         |    x => x + 1
         |  }
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tapply("g", blk(tfunc(tparam("x"))(tinfix("x", "+", lit(1)))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: braces arg, typed param name, closed on the same line") {
    val code =
      """|def f = foo { x: X => x + 1 }
         |""".stripMargin
    val layout =
      """|def f = foo {
         |  x {
         |    X => x + 1
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("foo", blk(tapply("x", blk(tfunc(tparam("X"))(tinfix("x", "+", lit(1))))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: indented def rhs") {
    val code =
      """|def f =
         |  g: x => x + 1
         |""".stripMargin
    val layout =
      """|def f = g {
         |  x => x + 1
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply("g", blk(tfunc(tparam("x"))(tinfix("x", "+", lit(1))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression, val rhs with type") {
    val code =
      """|val p: PartialFunction[(Int, Int), Int] = case (a, b) => a
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |val p: PartialFunction[(Int, Int), Int] = case (a, b) => a
         |                                          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case expression in parens, no guard") {
    val code =
      """|def f = xs.collect(case (a, b) => a)
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = xs.collect(case (a, b) => a)
         |                   ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 curried multi-line lambda: name params") {
    val code =
      """|def f = xs.map: x => y =>
         |  x + y
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => y => x + y
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tfunc(tparam("y"))(tinfix("x", "+", "y"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 curried single-line lambda: name params") {
    val code =
      """|def f = xs.map: x => y => x + y
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => y => x + y
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tfunc(tparam("y"))(tinfix("x", "+", "y"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: ascription of a function type") {
    val code =
      """|def f = (g: Int => Int)
         |""".stripMargin
    val layout =
      """|def f = g: (Int => Int)
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Ascribe(tname("g"), pfunc("Int")("Int")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: colon and lambda") {
    val code =
      """|def f = (g: x => x + 1)
         |""".stripMargin
    val layout =
      """|def f = g: (x => x + 1)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(tname("g"), pfunc("x")(pinfix("x", "+", Lit.Int(1)))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: placeholder, no arrow") {
    val code =
      """|def f = xs.map: _ + 1
         |""".stripMargin
    val layout =
      """|def f = xs.map: _ + 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(tselect("xs", "map"), pinfix(pwildcard, "+", Lit.Int(1))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: name, no arrow") {
    val code =
      """|def f = xs.map: plus1
         |""".stripMargin
    val layout =
      """|def f = xs.map: plus1
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Ascribe(tselect("xs", "map"), "plus1"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: indented leading select on the next line") {
    val code =
      """|def f = xs.map: x => x
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x.foo
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tselect("x", "foo")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: indented leading infix on the next line") {
    val code =
      """|def f = xs.map: x => x
         |  + 1
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x + 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix("x", "+", lit(1))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: next statement in the block") {
    val code =
      """|def f =
         |  xs.map: x => x
         |  g
         |""".stripMargin
    val layout =
      """|def f = {
         |  xs.map {
         |    x => x
         |  }
         |  g
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), tname("g")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: body args span lines") {
    val code =
      """|def f = xs.map: x => foo(x,
         |  1)
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => foo(x, 1)
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tapply("foo", "x", lit(1))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: nested on the same line, after infix") {
    val code =
      """|def f = xs.map: x => a + ys.map: y => y
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => a + ys.map {
         |    y => y
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(
          tinfix("a", "+", tapply(tselect("ys", "map"), blk(tfunc(tparam("y"))(tname("y"))))),
        )),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: semicolon after body") {
    val code =
      """|def f = xs.foreach: x => println(x); println(2)
         |""".stripMargin
    val layout =
      """|def f = xs.foreach {
         |  x => println(x)
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tapply("println", "x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in braces in parens, closed on the same line") {
    val code =
      """|def f = foo(bar { xs.map: x => x }, 1)
         |""".stripMargin
    val layout =
      """|def f = foo(bar {
         |  xs.map {
         |    x => x
         |  }
         |}, 1)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        "foo",
        tapply("bar", blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))))),
        lit(1),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: colon and lambda on its own line") {
    val code =
      """|def f = foo(
         |  xs.map: x => x + 1,
         |  2)
         |""".stripMargin
    val layout =
      """|def f = foo(xs.map: (x => x + 1), 2)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        "foo",
        Term.Ascribe(tselect("xs", "map"), pfunc("x")(pinfix("x", "+", Lit.Int(1)))),
        lit(2),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: case body, next case") {
    val code =
      """|def f = x match
         |  case 1 => xs.map: x => x
         |  case 2 => 3
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    xs.map {
         |      x => x
         |    }
         |  case 2 =>
         |    3
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        "x",
        Case(lit(1), None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x"))))),
        Case(lit(2), None, lit(3)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case, body indented on the next line") {
    val code =
      """|def f = xs.map: case x =>
         |  x + 1
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.map: case x =>
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: context function") {
    val code =
      """|def f = xs.map: x ?=> x
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x ?=> x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tctxfunc(tparam("x"))(tname("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: two params in parens") {
    val code =
      """|def f = xs.map: (x, y) => x + y
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  (x, y) => x + y
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"), tparam("y"))(tinfix("x", "+", "y")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: self type in braces") {
    val code =
      """|class C { self: B => def f = 1 }
         |""".stripMargin
    val layout =
      """|class C { self: B => def f = 1 }
         |""".stripMargin
    val tree = Defn.Class(
      Nil,
      pname("C"),
      Nil,
      ctor,
      tpl(self("self", "B"), Defn.Def(Nil, tname("f"), Nil, None, lit(1))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: val rhs, type name param") {
    val code =
      """|val g = f: Int => Int
         |""".stripMargin
    val layout =
      """|val g = f {
         |  Int => Int
         |}
         |""".stripMargin
    val tree = Defn
      .Val(Nil, List(patvar("g")), None, tapply("f", blk(tfunc(tparam("Int"))(tname("Int")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: interpolated body") {
    val code =
      """|def f = xs.map: x => s"a${x}b"
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => s"a${x}b"
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(
          Term.Interpolate(tname("s"), List(str("a"), str("b")), List(blk(tname("x")))),
        )),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for generator, yield on the next line") {
    val code =
      """|def f = for x <- xs.map: y => y
         |yield x
         |""".stripMargin
    val layout =
      """|def f = for (x <- xs.map {
         |  y => y
         |}) yield x
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.ForYield(
        List(
          Enumerator
            .Generator(patvar("x"), tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tname("y"))))),
        ),
        tname("x"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: named arg with colon and lambda") {
    val code =
      """|def f = foo(a = xs.map: y => y)
         |""".stripMargin
    val layout =
      """|def f = foo(a = xs.map: (y => y))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("foo", Term.Assign(tname("a"), Term.Ascribe(tselect("xs", "map"), pfunc("y")("y")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: end marker on the next line") {
    val code =
      """|object A:
         |  def f = xs.map: x => x
         |  end f
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x
         |  }
         |  end f
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        ),
        Term.EndMarker(tname("f")),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: comments after the body") {
    val code =
      """|object A:
         |  def f = xs.map: x => x // c
         |  // d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x // c
         |  }
         |  // d
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
          tapply(
            tselect("xs", "map"),
            blk(
              Term.Function.newBuilder(List(tparam("x")), tname("x")).endComment(Seq("// c")).result(),
            ),
          ),
        ),
        Defn.Def.newBuilder(Nil, tname("g"), Nil, None, lit(1)).begComment(Seq("// d")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in try, catch on the next line") {
    val code =
      """|def f = try xs.map: x => x
         |  catch case e => 1
         |""".stripMargin
    val layout =
      """|def f = try xs.map {
         |  x => x
         |} catch {
         |  case e => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        List(Case(patvar("e"), None, lit(1))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: braces body spans lines") {
    val code =
      """|def f = xs.map: x => { a
         |  b }
         |""".stripMargin
    val layout =
      """|def f = xs.map { x =>
         |  a
         |  b
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(blk(tname("a"), tname("b"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new prefix") {
    val code =
      """|def f = new A: x => x
         |""".stripMargin
    val layout =
      """|def f = new A: (x => x)
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Ascribe(Term.New(init("A")), pfunc("x")("x")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: this prefix") {
    val code =
      """|def f = this: x => x
         |""".stripMargin
    val layout =
      """|def f = this {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply(Term.This(anon), blk(tfunc(tparam("x"))(tname("x")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case in indented def rhs") {
    val code =
      """|def f =
         |  case (a, b) => a
         |""".stripMargin
    val layout =
      """|def f = {
         |  case (a, b) => a
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.PartialFunction(List(Case(Pat.Tuple(List(patvar("a"), patvar("b"))), None, tname("a")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case in braces") {
    val code =
      """|def f = { case (a, b) => a }
         |""".stripMargin
    val layout =
      """|def f = {
         |  case (a, b) => a
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.PartialFunction(List(Case(Pat.Tuple(List(patvar("a"), patvar("b"))), None, tname("a")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression in nested parens") {
    val code =
      """|def f = foo((case x => x), 1)
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = foo((case x => x), 1)
         |             ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 parens in braces: colon and lambda") {
    val code =
      """|def f = foo { (g: x => x) }
         |""".stripMargin
    val layout =
      """|def f = foo {
         |  g: (x => x)
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply("foo", blk(Term.Ascribe(tname("g"), pfunc("x")("x")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: colon, lambda arrow at end of line") {
    val code =
      """|def f = foo((g: x =>
         |  x))
         |""".stripMargin
    val layout =
      """|def f = foo(g {
         |  x => x
         |})
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply("foo", tapply("g", blk(tfunc(tparam("x"))(tname("x"))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: select colon, lambda arrow at end of line") {
    val code =
      """|def f = foo(xs.map: x =>
         |  x)
         |""".stripMargin
    val layout =
      """|def f = foo(xs.map {
         |  x => x
         |})
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("foo", tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: two defs in a row") {
    val code =
      """|object A:
         |  def f = xs.map: x => y
         |  def g = xs.map: (x, y) => x
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => y
         |  }
         |  def g = xs.map {
         |    (x, y) => x
         |  }
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("y")))),
        ),
        Defn.Def(
          Nil,
          tname("g"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"), tparam("y"))(tname("x")))),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case with guard, second case on the next line") {
    val code =
      """|def f = xs.map: case x if x > 1 => x
         |  case y => y
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.map: case x if x > 1 => x
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: case lambda in the body") {
    val code =
      """|def f = xs.map: x => xs.map: case y => y
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.map: x => xs.map: case y => y
         |                             ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: nested on the same line, after infix, selects") {
    val code =
      """|def f = a.b: x => x + c.d: y => y
         |""".stripMargin
    val layout =
      """|def f = a.b {
         |  x => x + c.d {
         |    y => y
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("a", "b"),
        blk(tfunc(tparam("x"))(
          tinfix("x", "+", tapply(tselect("c", "d"), blk(tfunc(tparam("y"))(tname("y"))))),
        )),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 curried single-line lambda: same name twice") {
    val code =
      """|def f = xs.map: x => x => x
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tfunc(tparam("x"))(tname("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: catch case on the same line") {
    val code =
      """|def f = try a catch case e => 1
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e => 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, lit(1))), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: def rhs, type name param") {
    val code =
      """|def f = g: Int => Int
         |""".stripMargin
    val layout =
      """|def f = g {
         |  Int => Int
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, tapply("g", blk(tfunc(tparam("Int"))(tname("Int")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: typed param in braces, closed on the same line") {
    val code =
      """|def f = { g: (x: Int) => x }
         |""".stripMargin
    val layout =
      """|def f = {
         |  g {
         |    (x: Int) => x
         |  }
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tapply("g", blk(tfunc(tparam("x", "Int"))(tname("x"))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: braces arg spans lines, leading select") {
    val code =
      """|def f = xs.map: x => foo {
         |  a
         |} + 1
         |  .bar
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => foo {
         |    a
         |  } + 1.bar
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(tinfix(tapply("foo", blk(tname("a"))), "+", tselect(lit(1), "bar")))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in then, else on the next line") {
    val code =
      """|def f = if c then xs.map: x => x
         |  else 2
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |} else 2
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(2),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in then, else on the same line") {
    val code =
      """|def f = if c then xs.map: x => x else 2
         |""".stripMargin
    val layout =
      """|def f = if (c) xs.map {
         |  x => x
         |} else 2
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        lit(2),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression, first of two args") {
    val code =
      """|def f = foo(case x => x, 1)
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = foo(case x => x, 1)
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case, indented block body") {
    val code =
      """|def f = xs.map: case x =>
         |  a
         |  b
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.map: case x =>
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case expression with guard, next statement") {
    val code =
      """|object A:
         |  val p: PF = case x if g => y
         |  val q = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val p: PF = case x if g => y
         |              ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: in fewer braces in parens") {
    val code =
      """|def f = foo(
         |  bar:
         |    xs.map: x => x
         |)
         |""".stripMargin
    val layout =
      """|def f = foo(bar {
         |  xs.map {
         |    x => x
         |  }
         |})
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("foo", tapply("bar", blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x"))))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in braces in parens, closed on the next line") {
    val code =
      """|def f = foo({ xs.map: x => x
         |})
         |""".stripMargin
    val layout =
      """|def f = foo {
         |  xs.map {
         |    x => x
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("foo", blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in interpolation") {
    val code =
      """|def f = s"${xs.map: x => x}"
         |""".stripMargin
    val layout =
      """|def f = s"${xs.map {
         |  x => x
         |}}"
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Interpolate(
        tname("s"),
        List(str(""), str("")),
        List(blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: indented def rhs, next def") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => x
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 multi-line polymorphic lambda") {
    val code =
      """|def f = xs.map: [T] => (x: T) =>
         |  x
         |""".stripMargin
    val layout =
      """|def f = xs.map: ([T] => (x: T) => x)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(
        tselect("xs", "map"),
        ppolyfunc(pparam("T"))(pfunc(Type.TypedParam("x", "T", Nil))("x")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: apply prefix, two params") {
    val code =
      """|def f = xs.foldLeft(0): (a, b) => a + b
         |""".stripMargin
    val layout =
      """|def f = xs.foldLeft(0) {
         |  (a, b) => a + b
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tapply(tselect("xs", "foldLeft"), lit(0)),
        blk(tfunc(tparam("a"), tparam("b"))(tinfix("a", "+", "b"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression in parens in case body") {
    val code =
      """|def f = x match
         |  case 1 => foo(case y => y)
         |  case 2 => 3
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  case 1 => foo(case y => y)
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case expression, indented block body") {
    val code =
      """|object A:
         |  val p = case y =>
         |    a
         |    b
         |  val q = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val p = case y =>
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: indented statement on the next line") {
    val code =
      """|def f =
         |  xs.foreach: x => println(x)
         |    println(2)
         |""".stripMargin
    val layout =
      """|def f = {
         |  xs.foreach {
         |    x => println(x)
         |  }
         |  println(2)
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tapply("println", "x")))),
        tapply("println", lit(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: first template statement") {
    val code =
      """|object A:
         |  g: x => x + 1
         |""".stripMargin
    val layout =
      """|object A { g: x => x + 1 }
         |""".stripMargin
    val tree = Source(List(Defn.Object(Nil, tname("A"), tpl(self("g", "x"), tinfix("x", "+", lit(1))))))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: later template statement") {
    val code =
      """|class C:
         |  def f = 1
         |  g: x => x
         |""".stripMargin
    val layout =
      """|class C {
         |  def f = 1
         |  g {
         |    x => x
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("C"),
      Nil,
      ctor,
      tpl(Defn.Def(Nil, tname("f"), Nil, None, lit(1)), tapply("g", blk(tfunc(tparam("x"))(tname("x"))))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match in braces in body") {
    val code =
      """|def f = xs.map: x => x match { case 1 => 2 }
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x match {
         |    case 1 => 2
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tmatch("x", Case(lit(1), None, lit(2)))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression in val in case body") {
    val code =
      """|def f = x match
         |  case 1 => val p = case y => y; p
         |  case 2 => 3
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  case 1 => val p = case y => y; p
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case lambda in catch case body") {
    val code =
      """|object A:
         |  def f = try a catch case e => xs.map: case y => y
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = try a catch case e => xs.map: case y => y
         |                                        ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 curried multi-line lambda: second param on the next line") {
    val code =
      """|def f = xs.map: x =>
         |  y => z
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => y => z
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tfunc(tparam("y"))(tname("z"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 curried single-line lambda: typed params") {
    val code =
      """|def f = xs.map: (x: Int) => (y: Int) => x + y
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  (x: Int) => (y: Int) => x + y
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x", "Int"))(tfunc(tparam("y", "Int"))(tinfix("x", "+", "y")))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: no newline at the end of input") {
    val code = "def f = xs.map: x => x"
    val layout =
      """|def f = xs.map {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: infix prefix") {
    val code =
      """|def f = xs map: x => x
         |""".stripMargin
    val layout =
      """|def f = xs map: (x => x)
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, Term.Ascribe(tpostfix("xs", "map"), pfunc("x")("x")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case, indented leading select on the next line") {
    val code =
      """|def f = xs.map: case x => x
         |  .foo
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.map: case x => x
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: empty case body, next case on the same line") {
    val code =
      """|def f = x match
         |  case 1 => case 2 => 3
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |  case 2 => 3
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch("x", Case(lit(1), None, blk()), Case(lit(2), None, lit(3))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in catch case body") {
    val code =
      """|def f = try a catch case e => xs.map: x => x
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    xs.map {
         |      x => x
         |    }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tname("a"),
        List(Case(patvar("e"), None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case body with single-line lambda") {
    val code =
      """|def f = xs.map: case x => xs.map: y => y
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.map: case x => xs.map: y => y
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: infix rhs") {
    val code =
      """|def f = a + xs.map: x => x
         |""".stripMargin
    val layout =
      """|def f = a + xs.map {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tinfix("a", "+", tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: indented def rhs, indented leading select") {
    val code =
      """|def f =
         |  xs.map: x => x
         |    .foo
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x.foo
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tselect("x", "foo")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: placeholder param") {
    val code =
      """|def f = xs.map: _ => 1
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  _ => 1
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("_"))(lit(1)))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case, indented leading infix on the next line") {
    val code =
      """|def f = xs.map: case (a, b) => a
         |  + 1
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.map: case (a, b) => a
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case in braces arg and in parens") {
    val code =
      """|object A:
         |  def f = foo { case x => x }
         |  def g = (case x => x)
         |""".stripMargin
    val error =
      """|<input>:3: error: illegal start of simple expression
         |  def g = (case x => x)
         |           ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: case arrow at end of line, def on the next line") {
    val code =
      """|object A:
         |  def f = xs.map: case x =>
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.map: case x =>
         |                  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: in yield") {
    val code =
      """|def f = for
         |  x <- xs
         |yield xs.map: y => y
         |""".stripMargin
    val layout =
      """|def f = for (x <- xs) yield xs.map {
         |  y => y
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.ForYield(
        List(Enumerator.Generator(patvar("x"), tname("xs"))),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tname("y")))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: apply prefix") {
    val code =
      """|def f = a.b(c): x => x
         |""".stripMargin
    val layout =
      """|def f = a.b(c) {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tapply(tselect("a", "b"), "c"), blk(tfunc(tparam("x"))(tname("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: type apply prefix") {
    val code =
      """|def f = a.b[T]: x => x
         |""".stripMargin
    val layout =
      """|def f = a.b[T] {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tapplytype(tselect("a", "b"), "T"), blk(tfunc(tparam("x"))(tname("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression, indented leading select") {
    val code =
      """|val p = case x => x
         |  .foo
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |val p = case x => x
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case in indented case body") {
    val code =
      """|def f = x match
         |  case 1 =>
         |    case 2 => 3
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    {
         |      case 2 => 3
         |    }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch("x", Case(lit(1), None, Term.PartialFunction(List(Case(lit(2), None, lit(3)))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression in catch case body") {
    val code =
      """|def f = try a catch case e => case x => x
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |  case x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tname("a"),
        List(Case(patvar("e"), None, blk()), Case(patvar("x"), None, tname("x"))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: empty case body, next case on the next line") {
    val code =
      """|def f = x match
         |  case 1 =>
         |  case 2 => 3
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |  case 2 => 3
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch("x", Case(lit(1), None, blk()), Case(lit(2), None, lit(3))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression in parens, leading select") {
    val code =
      """|def f = foo(case x => x
         |  .bar)
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = foo(case x => x
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case in lambda block body") {
    val code =
      """|def f = xs.map: x =>
         |  case y => y
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => {
         |    case y => y
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(Term.PartialFunction(List(Case(patvar("y"), None, tname("y")))))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: empty case body in braces, next case on the same line") {
    val code =
      """|def f = x match { case 1 => case 2 => 3 }
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |  case 2 => 3
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch("x", Case(lit(1), None, blk()), Case(lit(2), None, lit(3))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case in fewer braces block") {
    val code =
      """|def f = xs.map:
         |  case y => y
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  case y => y
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), Term.PartialFunction(List(Case(patvar("y"), None, tname("y"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: two cases in indented val rhs") {
    val code =
      """|val p =
         |  case y => y
         |  case z => z
         |""".stripMargin
    val layout =
      """|val p = {
         |  case y => y
         |  case z => z
         |}
         |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(patvar("p")),
      None,
      Term
        .PartialFunction(List(Case(patvar("y"), None, tname("y")), Case(patvar("z"), None, tname("z")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: nested match in case body") {
    val code =
      """|def f = x match
         |  case 1 => y match
         |    case 2 => 3
         |  case 4 => 5
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    y match {
         |      case 2 => 3
         |    }
         |  case 4 =>
         |    5
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        "x",
        Case(lit(1), None, tmatch("y", Case(lit(2), None, lit(3)))),
        Case(lit(4), None, lit(5)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression after an arg") {
    val code =
      """|object A:
         |  def f = { case 1 => 2 } andThen { g }
         |  def h = foo(x, case y => y)
         |""".stripMargin
    val error =
      """|<input>:3: error: illegal start of simple expression
         |  def h = foo(x, case y => y)
         |                 ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: case expression as second argument") {
    val code = "def h = foo(x, case y => y)"
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def h = foo(x, case y => y)
         |               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: end marker of the object on the next line") {
    val code =
      """|object A:
         |  def f = xs.map: x => x
         |end A
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x
         |  }
         |}
         |end A
         |""".stripMargin
    val tree = Source(List(
      Defn.Object(
        Nil,
        tname("A"),
        tpl(Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        )),
      ),
      Term.EndMarker(tname("A")),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: indented def rhs, leading select at the same indent") {
    val code =
      """|def f =
         |  xs.map: x => x
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x.foo
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tselect("x", "foo")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: two typed params") {
    val code =
      """|def f = xs.map: (x: Int, y: Int) => x
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  (x: Int, y: Int) => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x", "Int"), tparam("y", "Int"))(tname("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case, indented body, next def") {
    val code =
      """|object A:
         |  def f = xs.map: case x =>
         |    a
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.map: case x =>
         |                  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: case, indented body, leading select") {
    val code =
      """|object A:
         |  def f = xs.map: case x =>
         |      a
         |    .foo
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.map: case x =>
         |                  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: case, next def") {
    val code =
      """|object A:
         |  def f = xs.map: case x => x
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.map: case x => x
         |                  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: tuple body spans lines, leading select") {
    val code =
      """|def f = xs.map: x => (x,
         |  1) + 2
         |  .bar
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => (x, 1) + 2.bar
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(
          tfunc(tparam("x"))(tinfix(Term.Tuple(List(tname("x"), lit(1))), "+", tselect(lit(2), "bar"))),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: semicolon and def after body") {
    val code =
      """|object A:
         |  def f = xs.map: x => x; def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression with guard, second case on the next line") {
    val code =
      """|object A:
         |  val p = case x if x > 1 => x
         |    case y => y
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val p = case x if x > 1 => x
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: in match braces in parens, closed on the same line") {
    val code =
      """|def f = foo(a match { case 1 => xs.map: x => x })
         |""".stripMargin
    val layout =
      """|def f = foo(a match {
         |  case 1 =>
         |    xs.map {
         |      x => x
         |    }
         |})
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        "foo",
        tmatch("a", Case(lit(1), None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in match braces in parens, closed on the next line") {
    val code =
      """|def f = foo(a match { case 1 => xs.map: x => x
         |})
         |""".stripMargin
    val layout =
      """|def f = foo(a match {
         |  case 1 =>
         |    xs.map {
         |      x => x
         |    }
         |})
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        "foo",
        tmatch("a", Case(lit(1), None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: indented leading infix operator") {
    val code =
      """|def f = xs.map: x => x
         |  ++ ys
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x ++ ys
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix("x", "++", "ys")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: blank line after the body") {
    val code =
      """|object A:
         |  def f = xs.map: x => x
         |
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case lambda in case body, next case") {
    val code =
      """|def f = x match
         |  case 1 => xs.map: case y => y
         |  case 2 => 3
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  case 1 => xs.map: case y => y
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern") {
    val code =
      """|def f = ys.collect: case z: Int => z
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = ys.collect: case z: Int => z
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: interpolation prefix") {
    val code =
      """|def f = s"$x".map: c => c
         |""".stripMargin
    val layout =
      """|def f = s"$x".map {
         |  c => c
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect(Term.Interpolate(tname("s"), List(str(""), str("")), List(tname("x"))), "map"),
        blk(tfunc(tparam("c"))(tname("c"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: try and catch case in the body") {
    val code =
      """|def f = xs.map: x => try f(x) catch case _ => 0
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => try f(x) catch {
         |    case _ => 0
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(
          tfunc(tparam("x"))(Term.Try(tapply("f", "x"), List(Case(patwildcard, None, lit(0))), None)),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 scala213: single-line lambda") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|def f = g: x => x + 1
         |""".stripMargin
    val layout =
      """|def f = g: (x => x + 1)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(tname("g"), pfunc("x")(pinfix("x", "+", Lit.Int(1)))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 scala213: case expression in parens") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|def f = xs.collect(case x => x)
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = xs.collect(case x => x)
         |                   ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 scala213: case expression in val rhs") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|val p = case x => x
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |val p = case x => x
         |        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 scala213: case after colon") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|def f = xs.collect: case x => x
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.collect: case x => x
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 scala213: ascription of a function type in parens") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|def f = (g: Int => Int)
         |""".stripMargin
    val layout =
      """|def f = g: (Int => Int)
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Ascribe(tname("g"), pfunc("Int")("Int")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: implicit closure in braces, typed param, one line") {
    val code =
      """|def f = xs.foreach { implicit x: Int => foo(x) }
         |""".stripMargin
    val layout =
      """|def f = xs.foreach {
         |  implicit x: Int => foo(x)
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(tapply("foo", "x"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case after colon in parens") {
    val code =
      """|def f = foo(xs.map: case x => x)
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = foo(xs.map: case x => x)
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case expression after colon, next line") {
    val code =
      """|def f = xs.map:
         |  case x => x
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  case x =>
         |    x.foo
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        Term.PartialFunction(List(Case(patvar("x"), None, tselect("x", "foo")))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("implicit closure in braces, typed param, select on next line") {
    val code =
      """|def t = xs.foreach { implicit x: Int => foo(x)
         |  .bar }
         |""".stripMargin
    val layout =
      """|def t = xs.foreach {
         |  implicit x: Int => foo(x).bar
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(tselect(tapply("foo", "x"), "bar"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("implicit closure in braces, typed param, infix on next line") {
    val code =
      """|def t = xs.foreach { implicit x: Int => x
         |  + 1 }
         |""".stripMargin
    val layout =
      """|def t = xs.foreach {
         |  implicit x: Int => x + 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(tinfix("x", "+", lit(1)))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("implicit closure in braces, typed param, statement on next line") {
    val code =
      """|def t = xs.foreach { implicit x: Int => println(x)
         |  println(2) }
         |""".stripMargin
    val layout =
      """|def t = xs.foreach { implicit x: Int =>
         |  println(x)
         |  println(2)
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(blk(
          tapply("println", "x"),
          tapply("println", lit(2)),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("implicit closure after colon, typed param, select on next line") {
    val code =
      """|def t =
         |  xs.foreach:
         |    implicit x: Int => foo(x)
         |      .bar
         |""".stripMargin
    val layout =
      """|def t = xs.foreach {
         |  implicit x: Int => foo(x).bar
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(tselect(tapply("foo", "x"), "bar"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("implicit closure after colon, typed param, statement on next line") {
    val code =
      """|def t =
         |  xs.foreach:
         |    implicit x: Int => foo(x)
         |    bar
         |""".stripMargin
    val layout =
      """|def t = xs.foreach { implicit x: Int =>
         |  foo(x)
         |  bar
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(blk(tapply("foo", "x"), tname("bar")))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("implicit closure in braces, typed param, select on next line: scala213") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|def t = xs.foreach { implicit x: Int => foo(x)
         |  .bar }
         |""".stripMargin
    val layout =
      """|def t = xs.foreach {
         |  implicit x: Int => foo(x).bar
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(tselect(tapply("foo", "x"), "bar"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("implicit closure in braces, typed param, infix on next line: scala213") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|def t = xs.foreach { implicit x: Int => x
         |  + 1 }
         |""".stripMargin
    val layout =
      """|def t = xs.foreach { implicit x: Int =>
         |  x
         |  1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(blk(tname("x"), lit(1)))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda, select on next line") {
    val code =
      """|xs.map: x => x
         |.foo
         |""".stripMargin
    val layout =
      """|xs.map {
         |  x => x.foo
         |}
         |""".stripMargin
    val tree = tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tselect("x", "foo"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda, infix on next line") {
    val code =
      """|xs.map: x => x
         |+ 1
         |""".stripMargin
    val layout =
      """|xs.map {
         |  x => x + 1
         |}
         |""".stripMargin
    val tree = tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix("x", "+", lit(1)))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda in def, select on next line: scala213") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|def f = xs.map: x => x
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = xs.map: (x => x.foo)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(tselect("xs", "map"), pfunc("x")(Type.Select(Term.Name("x"), Type.Name("foo")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda, then end marker") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => x
         |  end f
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x
         |  }
         |  end f
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        ),
        Term.EndMarker(tname("f")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda in then, else on next line") {
    val code =
      """|def t =
         |  if c then xs.map: x => x
         |  else ys
         |""".stripMargin
    val layout =
      """|def t = if (c) xs.map {
         |  x => x
         |} else ys
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      Term.If(
        tname("c"),
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        tname("ys"),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda in try, catch on next line") {
    val code =
      """|def u =
         |  try xs.map: x => x
         |  catch case _ => Nil
         |""".stripMargin
    val layout =
      """|def u = try xs.map {
         |  x => x
         |} catch {
         |  case _ => Nil
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("u"),
      Nil,
      None,
      Term.Try(
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        List(Case(patwildcard, None, tname("Nil"))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda, trailing comment and blank line") {
    val code =
      """|def t =
         |  xs.map: x => x // c
         |  .filter: y => y
         |
         |  val z = 1
         |  z
         |""".stripMargin
    val layout =
      """|def t = {
         |  xs.map {
         |    x => x // c
         |    .filter {
         |      y => y
         |    }
         |  }
         |  val z = 1
         |  z
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      blk(
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(
            tapply(tselect(tnameComments("x")()("// c"), "filter"), blk(tfunc(tparam("y"))(tname("y")))),
          )),
        ),
        Defn.Val(Nil, List(patvar("z")), None, lit(1)),
        tname("z"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda, comment line before select") {
    val code =
      """|def t =
         |  xs.map: x => x
         |  // c
         |  .foo
         |""".stripMargin
    val layout =
      """|def t = xs.map {
         |  x => x
         |  // c
         |  .foo
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tselect("x", tnameComments("foo")("// c")())))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 two single-line lambdas on a line, select on next line") {
    val code =
      """|def t =
         |  xs.map: x => ys.map: y => x + y
         |  .sum
         |""".stripMargin
    val layout =
      """|def t = xs.map {
         |  x => ys.map {
         |    y => x + y.sum
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(
          tapply(tselect("ys", "map"), blk(tfunc(tparam("y"))(tinfix("x", "+", tselect("y", "sum"))))),
        )),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda, if-then and else on next line") {
    val code =
      """|def t =
         |  xs.map: x => if x > 0 then 1
         |  else 2
         |""".stripMargin
    val layout =
      """|def t = xs.map {
         |  x => if (x > 0) 1 else 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(Term.If(tinfix("x", ">", lit(0)), lit(1), lit(2), Nil))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda, then multi-line lambda on the same line") {
    val code =
      """|def t =
         |  xs.map: x => x.foo: y =>
         |    y + 1
         |  .bar
         |""".stripMargin
    val layout =
      """|def t = xs.map {
         |  x => x.foo {
         |    y => y + 1
         |  }.bar
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(tselect(
          tapply(tselect("x", "foo"), blk(tfunc(tparam("y"))(tinfix("y", "+", lit(1))))),
          "bar",
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda, try-catch in body, select on next line") {
    val code =
      """|def t =
         |  xs.map: x => try f(x) catch case _ => 0
         |  .size
         |""".stripMargin
    val layout =
      """|def t = xs.map {
         |  x => try f(x) catch {
         |    case _ => 0
         |  }
         |}.size
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tselect(
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(
            Term.Try(tapply("f", "x"), List(Case(patwildcard, None, lit(0))), None),
          )),
        ),
        "size",
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 case expression: typed pattern, infix on next line") {
    val code =
      """|def t =
         |  val h: PartialFunction[Any, Int] = case x: Int => x
         |    + 1
         |  h
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val h: PartialFunction[Any, Int] = case x: Int => x
         |                                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 case expression: typed pattern, select on next line") {
    val code =
      """|def t =
         |  val h: PartialFunction[Any, String] = case x: Int => x
         |    .toString
         |  h
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val h: PartialFunction[Any, String] = case x: Int => x
         |                                        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 case expression: typed pattern, statement on next line") {
    val code =
      """|def t =
         |  val h: PartialFunction[Any, Int] = case x: Int => x
         |  val g = 1
         |  g
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val h: PartialFunction[Any, Int] = case x: Int => x
         |                                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 case expression: untyped pattern, infix on next line") {
    val code =
      """|def t =
         |  val h: PartialFunction[Any, Int] = case x => x
         |    + 1
         |  h
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val h: PartialFunction[Any, Int] = case x => x
         |                                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 case expression: untyped pattern, select on next line") {
    val code =
      """|def t =
         |  val h: PartialFunction[Any, String] = case x => x
         |    .toString
         |  h
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val h: PartialFunction[Any, String] = case x => x
         |                                        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 case expression in template: typed pattern, infix on next line") {
    val code =
      """|object A:
         |  val h: PartialFunction[Any, Int] = case x: Int => x
         |    + 1
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val h: PartialFunction[Any, Int] = case x: Int => x
         |                                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 case expression in template: typed pattern, select on next line") {
    val code =
      """|object A:
         |  val h: PartialFunction[Any, String] = case x: Int => x
         |    .toString
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val h: PartialFunction[Any, String] = case x: Int => x
         |                                        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 case expression in template: typed pattern, statement on next line") {
    val code =
      """|object A:
         |  val h: PartialFunction[Any, Int] = case x: Int => x
         |  val k = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val h: PartialFunction[Any, Int] = case x: Int => x
         |                                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 case expression in template: untyped pattern, infix on next line") {
    val code =
      """|object A:
         |  val h: PartialFunction[Any, Int] = case x => x
         |    + 1
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val h: PartialFunction[Any, Int] = case x => x
         |                                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda in parens, over several lines") {
    val code =
      """|def t = foo(
         |  xs.map: x => x + 1
         |)
         |""".stripMargin
    val layout =
      """|def t = foo(xs.map: (x => x + 1))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply("foo", Term.Ascribe(tselect("xs", "map"), pfunc("x")(pinfix("x", "+", Lit.Int(1))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda in parens, over several lines: scala213") {
    implicit val dialect: Dialect = dialects.Scala213
    val code =
      """|def t = foo(
         |  xs.map: x => x + 1
         |)
         |""".stripMargin
    val layout =
      """|def t = foo(xs.map: (x => x + 1))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      tapply("foo", Term.Ascribe(tselect("xs", "map"), pfunc("x")(pinfix("x", "+", Lit.Int(1))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: scala213") {
    implicit val dialect: Dialect = dialects.Scala213
    val code = "xs.map: x => x + 1"
    val error =
      """|<input>:1: error: `;` expected but `=>` found
         |xs.map: x => x + 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda as first template statement") {
    val code =
      """|class C:
         |  g: x => x + 1
         |""".stripMargin
    val layout =
      """|class C { g: x => x + 1 }
         |""".stripMargin
    val tree = Defn.Class(Nil, pname("C"), Nil, ctor, tpl(self("g", "x"), tinfix("x", "+", lit(1))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case in a match case body, next case") {
    val code =
      """|def t =
         |  x match
         |    case y => ys.collect: case z: Int => z
         |    case _ => Nil
         |""".stripMargin
    val error =
      """|<input>:3: error: `identifier` expected but `case` found
         |    case y => ys.collect: case z: Int => z
         |                          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case, second case on next line") {
    val code =
      """|def t =
         |  xs.collect: case x => x
         |  case y => y
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  xs.collect: case x => x
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 colon: typed param name in braces, body on the next line") {
    val code =
      """|def f = foo { x: Int =>
         |  x + 1
         |}
         |""".stripMargin
    val layout =
      """|def f = foo {
         |  x {
         |    Int => x + 1
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("foo", blk(tapply("x", blk(tfunc(tparam("Int"))(tinfix("x", "+", lit(1))))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: self type in given braces") {
    val code =
      """|object A:
         |  given Foo with { self: Bar => def f = 1 }
         |""".stripMargin
    val layout =
      """|object A { given Foo with { self: Bar => def f = 1 } }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Given(
        Nil,
        Name.Anonymous(),
        Nil,
        Template(
          None,
          List(init(pname("Foo"))),
          Template.Body(
            Some(Self(Term.Name("self"), Some(Type.Name("Bar")))),
            List(Defn.Def(Nil, Term.Name("f"), Nil, None, Lit.Int(1))),
          ),
          Nil,
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: braces arg prefix") {
    val code =
      """|def f = foo { a }: x => x
         |""".stripMargin
    val layout =
      """|def f = foo {
         |  a
         |} {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tapply("foo", blk(tname("a"))), blk(tfunc(tparam("x"))(tname("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression with typed pattern, val rhs") {
    val code =
      """|val p: PF = case x: Int => x
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |val p: PF = case x: Int => x
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern, def rhs") {
    val code =
      """|def f = xs.map: case x: Int => x
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.map: case x: Int => x
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 implicit closure: typed param in braces arg") {
    val code =
      """|def f = xs.map { implicit x: Int => x }
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  implicit x: Int => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam(List(Mod.Implicit()), "x", "Int"))(tname("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: given with a context function type") {
    val code =
      """|object A:
         |  given f: Ord[Int] => Ord[List[Int]] = ???
         |""".stripMargin
    val layout =
      """|object A { given f: (Ord[Int]) => Ord[List[Int]] = ??? }
         |""".stripMargin
    val tree = Source {
      List {
        Defn.Object(
          Nil,
          tname("A"),
          tpl(Defn.GivenAlias(
            Nil,
            Term.Name("f"),
            List(Member.ParamClauseGroup(
              Type.ParamClause(Nil),
              List(Term.ParamClause(List(Term.Param(
                Nil,
                Name.Anonymous(),
                Some(Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("Int"))))),
                None,
              )))),
            )),
            Type.Apply(
              Type.Name("Ord"),
              Type.ArgClause(List(Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("Int")))))),
            ),
            Term.Name("???"),
          )),
        )
      }
    }
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, leading select") {
    val code =
      """|def f = xs.map: x => try a catch case e => b
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => try a catch {
         |    case e =>
         |      b.foo
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(
          Term.Try(tname("a"), List(Case(patvar("e"), None, tselect("b", "foo"))), None),
        )),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: indented comment line before select") {
    val code =
      """|def f = xs.map: x => x
         |  // comment
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x
         |  // comment
         |  .foo
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(tselect("x", tnameComments("foo")("// comment")()))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for generator in parens") {
    val code =
      """|def f = for (x <- xs.map: y => y) yield x
         |""".stripMargin
    val layout =
      """|def f = for (x <- xs.map: (y => y)) yield x
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.ForYield(
        List(Enumerator.Generator(patvar("x"), Term.Ascribe(tselect("xs", "map"), pfunc("y")("y")))),
        tname("x"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case after new prefix") {
    val code =
      """|def f = new A: case x => x
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = new A: case x => x
         |               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: param list spans lines") {
    val code =
      """|def f = xs.map: (x: Int,
         |  y: Int) => x
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  (x: Int, y: Int) => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x", "Int"), tparam("y", "Int"))(tname("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: typed val rhs") {
    val code =
      """|val x: Int = foo: y => y
         |""".stripMargin
    val layout =
      """|val x: Int = foo {
         |  y => y
         |}
         |""".stripMargin
    val tree = Defn
      .Val(Nil, List(patvar("x")), Some("Int"), tapply("foo", blk(tfunc(tparam("y"))(tname("y")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match in braces, leading select") {
    val code =
      """|def f = xs.map: x => x match { case 1 => 2 }
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x match {
         |    case 1 => 2
         |  }
         |}.foo
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tselect(
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tmatch("x", Case(lit(1), None, lit(2)))))),
        "foo",
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression in parens, lambda in case body") {
    val code =
      """|def f = foo(case x => xs.map: y => y)
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = foo(case x => xs.map: y => y)
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 colon: this self type as first template statement") {
    val code =
      """|object A:
         |  this: B => def f = 1
         |""".stripMargin
    val layout =
      """|object A { this: B => def f = 1 }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(self(Name.This(), "B"), Defn.Def(Nil, tname("f"), Nil, None, lit(1))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: multi-line comment after the arrow") {
    val code =
      """|def f = xs.map: x => /* a
         |  b */ x
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => /* a
         |  b */ x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tnameComments("x")("/* a\n  b */")()))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case, indented body and two leading selects") {
    val code =
      """|def f = xs.map: case x =>
         |    a
         |    .foo
         |  .bar
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.map: case x =>
         |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 parens: colon and lambda in val rhs") {
    val code =
      """|val f = (xs.map: x => x)
         |""".stripMargin
    val layout =
      """|val f = xs.map: (x => x)
         |""".stripMargin
    val tree = Defn
      .Val(Nil, List(patvar("f")), None, Term.Ascribe(tselect("xs", "map"), pfunc("x")("x")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: placeholder self type as first template statement") {
    val code =
      """|object A:
         |  _: B => def f = 1
         |""".stripMargin
    val layout =
      """|object A { _: B => def f = 1 }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(self(Name.Placeholder(), "B"), Defn.Def(Nil, tname("f"), Nil, None, lit(1))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: line comment after the arrow") {
    val code =
      """|def f = xs.map: x => // c
         |  x + 1
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => // c
         |    x + 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(
          Term.ApplyInfix.newBuilder(tname("x"), tname("+"), Nil, List(lit(1)))
            .begComment(Seq("// c")).result(),
        )),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, leading select") {
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
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, tselect("b", "foo"))), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case arrow at end of line, body at the select indent") {
    val code =
      """|def f =
         |  foo
         |    .map: case x =>
         |    a
         |""".stripMargin
    val error =
      """|<input>:3: error: `identifier` expected but `case` found
         |    .map: case x =>
         |          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 colon: literal prefix in def rhs, lambda on the next line") {
    val code =
      """|def f = !1:
         |  x => x
         |""".stripMargin
    val layout =
      """|def f = !1 {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, unary("!", tapply(lit(1), blk(tfunc(tparam("x"))(tname("x"))))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: name prefix and context function type") {
    val code =
      """|val x = y: Int ?=> Boolean
         |""".stripMargin
    val layout =
      """|val x = y {
         |  Int ?=> Boolean
         |}
         |""".stripMargin
    val tree = Defn
      .Val(Nil, List(patvar("x")), None, tapply("y", blk(tctxfunc(tparam("Int"))(tname("Boolean")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: typed pattern in a match case") {
    val code =
      """|def f = x match
         |  case 1 => a
         |  case y: Int => b
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 => a
         |  case y: Int => b
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        "x",
        Case(lit(1), None, tname("a")),
        Case(Pat.Typed(patvar("y"), "Int"), None, tname("b")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 curried single-line lambda: two spaces after the first arrow") {
    val code =
      """|def f = xs.map: x =>  y => x + y
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => y => x + y
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tfunc(tparam("y"))(tinfix("x", "+", "y"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression in parens, indented block body") {
    val code =
      """|def f = xs.collect(
         |  case (a, b) =>
         |    println(b)
         |    a
         |)
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  case (a, b) =>
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 catch case on the same line, semicolon after the body") {
    val code =
      """|def f = try a catch case e => b; c
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    b
         |    c
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, blk(tname("b"), tname("c")))), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, indented line after the body") {
    val code =
      """|def f =
         |  try a catch case e => b
         |    c
         |  d
         |""".stripMargin
    val layout =
      """|def f = {
         |  try a catch {
         |    case e =>
         |      b
         |      c
         |  }
         |  d
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.Try(tname("a"), List(Case(patvar("e"), None, blk(tname("b"), tname("c")))), None),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: term parameter entry point, function type") {
    implicit val parser: String => Term.Param = parseRule(_, Parse.parseTermParam)
    runTestAssert[Term.Param]("x: A => B")(tparam("x", pfunc(pname("A"))(pname("B"))))
  }

  test("SIP-75 colon: template entry point, self type") {
    implicit val parser: String => Template = parseRule(_, Parse.parseTemplate)
    val code = "A { self: B => def f = 1 }"
    val tree = tpl(List(init("A")), self("self", "B"), Defn.Def(Nil, tname("f"), Nil, None, lit(1)))
    parseAndCheckTree[Template](code, "extends A { self: B => def f = 1 }")(tree)
  }

  test("SIP-75 colon: term entry point, single-line lambda") {
    implicit val parser: String => Term = term(_)
    val layout =
      """|g {
         |  x => x + 1
         |}""".stripMargin
    runTestAssert[Term]("g: x => x + 1", layout)(
      tapply(tname("g"), blk(tfunc(tparam("x"))(tinfix("x", "+", lit(1))))),
    )
  }

  test("SIP-75 single-line lambda: new body, indented leading select") {
    val code =
      """|def f = xs.map: x => new A
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => new A.foo
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.New(init(Type.Select("A", "foo")))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new body, leading select at the def indent") {
    val code =
      """|object A:
         |  def f = xs.map: x => new A
         |  .foo
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => new A.foo
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.New(init(Type.Select("A", "foo")))))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match case body, indented leading select") {
    val code =
      """|def f = xs.map: x => x match case 1 => a
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x match {
         |    case 1 =>
         |      a.foo
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(tmatch("x", Case(lit(1), None, tselect("a", "foo"))))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 match case on the same line, indented leading select") {
    val code =
      """|def f = x match case 1 => a
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    a.foo
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tmatch("x", Case(lit(1), None, tselect("a", "foo"))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, indented leading infix") {
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
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, tinfix("b", "+", lit(1)))), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, two indented lines, then an outdented one") {
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
         |    case e =>
         |      b
         |      c
         |      d
         |  }
         |  e
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.Try(
          tname("a"),
          List(Case(patvar("e"), None, blk(tname("b"), tname("c"), tname("d")))),
          None,
        ),
        tname("e"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, indented comment line before a statement") {
    val code =
      """|def f =
         |  try a catch case e => b
         |    // c
         |    c
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e =>
         |    b
         |    // c
         |    c
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tname("a"),
        List(Case(patvar("e"), None, blk(tname("b"), tnameComments("c")("// c")()))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, typed pattern, next case aligned with it") {
    val code =
      """|def f = try a catch case e: E => 1
         |                    case f => 2
         |""".stripMargin
    val layout =
      """|def f = try a catch {
         |  case e: E => 1
         |  case f => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(
        tname("a"),
        List(Case(Pat.Typed(patvar("e"), pname("E")), None, lit(1)), Case(patvar("f"), None, lit(2))),
        None,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, finally on the next line") {
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
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), Some(tname("c"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, indented line in parens") {
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
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, tapply("b", "x"))), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, indented line in braces") {
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
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, tapply("b", blk(tname("x"))))), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 indented def rhs, deeper line in parens") {
    val code =
      """|def f =
         |  b
         |    (x)
         |""".stripMargin
    val layout = "def f = b(x)"
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, tapply("b", "x")))
  }

  test("SIP-75 relaxed lambda: typed pattern after then") {
    val code =
      """|def f = if c then case x: Int => x else g
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = if c then case x: Int => x else g
         |                  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after else") {
    val code =
      """|def f = if c then g else case x: Int => x
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = if c then g else case x: Int => x
         |                         ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after do") {
    val code =
      """|def f = while c do case x: Int => x
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = while c do case x: Int => x
         |                   ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after yield") {
    val code =
      """|def f = for x <- xs yield case y: Int => y
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = for x <- xs yield case y: Int => y
         |                          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after return") {
    val code =
      """|def f: Any = return case x: Int => x
         |""".stripMargin
    val error =
      """|<input>:1: error: `;` expected but `case` found
         |def f: Any = return case x: Int => x
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after throw") {
    val code =
      """|def f = throw case x: Int => x
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = throw case x: Int => x
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after try") {
    val code =
      """|def f = try case x: Int => x finally g
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = try case x: Int => x finally g
         |            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after finally") {
    val code =
      """|def f = try g finally case x: Int => x
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = try g finally case x: Int => x
         |                      ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after a lambda arrow") {
    val code =
      """|val f = (x: Int) => case y: Int => y
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |val f = (x: Int) => case y: Int => y
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after a context lambda arrow") {
    val code =
      """|val f = (x: Int) ?=> case y: Int => y
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |val f = (x: Int) ?=> case y: Int => y
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after a generator arrow") {
    val code =
      """|def f = for x <- case y: Int => y do ()
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = for x <- case y: Int => y do ()
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after a lambda arrow in parens") {
    val code =
      """|def f = xs.map(x => case y: Int => y)
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |def f = xs.map(x => case y: Int => y)
         |                    ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after a colon lambda arrow") {
    val code =
      """|def f = xs.map: x => case y: Int => y
         |""".stripMargin
    val error =
      """|<input>:1: error: `identifier` expected but `case` found
         |def f = xs.map: x => case y: Int => y
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after a match case arrow on the same line") {
    val code =
      """|def f = a match case 1 => case x: Int => x
         |""".stripMargin
    val layout =
      """|def f = a match {
         |  case 1 =>
         |  case x: Int => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        "a",
        Case(lit(1), None, blk()),
        Case(Pat.Typed(patvar("x"), pname("Int")), None, tname("x")),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: using lambda param") {
    val code =
      """|def f = xs.map: using x => x
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  (using x) => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(Term.Function(tpc(Mod.Using(), tparam(List(Mod.Using()), "x")), tname("x"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: erased lambda param") {
    val code =
      """|def f = xs.map: erased x => x
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  erased x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam(List(Mod.Erased()), "x"))(tname("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: implicit lambda param") {
    val code =
      """|def f = xs.map: implicit x => x
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  implicit x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(Term.Function(tpc(Mod.Implicit(), tparam(List(Mod.Implicit()), "x")), tname("x"))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: guard with colon and lambda, case arrow on the same line") {
    val code =
      """|def f = a match
         |  case a if foo: b => true => 2
         |""".stripMargin
    val layout =
      """|def f = a match {
         |  case a if foo {
         |    b => true
         |  } => 2
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch("a", Case(patvar("a"), Some(tapply("foo", blk(tfunc(tparam("b"))(lit(true))))), lit(2))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, nested try, semicolon after the body") {
    val code =
      """|object A:
         |  def f = try a catch case e => try b catch case g => c; d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      try b catch {
         |        case g =>
         |          c
         |          d
         |      }
         |  }
         |}
         |""".stripMargin
    val inner = Term
      .Try(tname("b"), List(Case(patvar("g"), None, blk(tname("c"), tname("d")))), None)
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.Try(tname("a"), List(Case(patvar("e"), None, inner)), None),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: context bound with a function type, indented trait") {
    val code =
      """|trait X:
         |  type T: C => D
         |""".stripMargin
    val layout = "trait X { type T: C => D }"
    val tree = Defn.Trait(
      Nil,
      pname("X"),
      Nil,
      ctor,
      tpl(Decl.Type(Nil, pname("T"), Nil, bounds(cb = List(pfunc(pname("C"))(pname("D")))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: context bound with a function type, trait in braces") {
    val code = "trait X { type T: C => D }"
    val tree = Defn.Trait(
      Nil,
      pname("X"),
      Nil,
      ctor,
      tpl(Decl.Type(Nil, pname("T"), Nil, bounds(cb = List(pfunc(pname("C"))(pname("D")))))),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("SIP-75 colon: context bound with a function type, refinement in a source") {
    val code =
      """|object A:
         |  type R = A { type T: C => D }
         |""".stripMargin
    val layout =
      """|object A {
         |  type R = A {
         |    type T: C => D
         |  }
         |}
         |""".stripMargin
    val refine = Type.Refine(
      Some(pname("A")),
      stats(Decl.Type(Nil, pname("T"), Nil, bounds(cb = List(pfunc(pname("C"))(pname("D")))))),
    )
    val tree = Source(List(Defn.Object(Nil, tname("A"), tpl(Defn.Type(Nil, pname("R"), Nil, refine)))))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 colon: template entry point, context bound with a function type") {
    implicit val parser: String => Template = parseRule(_, Parse.parseTemplate)
    val code = "A { type T: C => D }"
    val tree = tpl(
      List(init("A")),
      List(Decl.Type(Nil, pname("T"), Nil, bounds(cb = List(pfunc(pname("C"))(pname("D")))))),
    )
    parseAndCheckTree[Template](code, "extends A { type T: C => D }")(tree)
  }

  test("SIP-75 colon: type entry point, context bound with a function type") {
    val code = "A { type T: C => D }"
    val layout =
      """|A {
         |  type T: C => D
         |}""".stripMargin
    val tree = Type.Refine(
      Some(pname("A")),
      stats(Decl.Type(Nil, pname("T"), Nil, bounds(cb = List(pfunc(pname("C"))(pname("D")))))),
    )
    runTestAssert[Type](code, layout)(tree)
  }

  test("SIP-75 colon: context bound with a polymorphic function type") {
    val code =
      """|trait X:
         |  type T: [X] => X
         |""".stripMargin
    val error =
      """|<input>:2: error: polymorphic function types must have a value parameter
         |  type T: [X] => X
         |              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: case generator in for parens") {
    val code =
      """|def f = for (case (a, b) <- xs) yield a
         |""".stripMargin
    val layout = "def f = for ( case (a, b) <- xs) yield a"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.ForYield(
        List(Enumerator.CaseGenerator(Pat.Tuple(List(patvar("a"), patvar("b"))), tname("xs"))),
        tname("a"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: typed case generator and guard in for parens") {
    val code =
      """|def f = for (case x: Int <- xs if x > 0) yield x
         |""".stripMargin
    val layout = "def f = for ( case x: Int <- xs; if x > 0) yield x"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.ForYield(
        List(
          Enumerator.CaseGenerator(Pat.Typed(patvar("x"), pname("Int")), tname("xs")),
          Enumerator.Guard(tinfix("x", ">", lit(0))),
        ),
        tname("x"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: lambda after a using infix operator") {
    val code =
      """|def f = a using b: x => x
         |""".stripMargin
    val layout =
      """|def f = a using b {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tinfix("a", "using", tapply("b", blk(tfunc(tparam("x"))(tname("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: return in a match case, next case in braces") {
    val code =
      """|def f = x match { case 1 => return case 2 => 3 }
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    return
         |  case 2 =>
         |    3
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch("x", Case(lit(1), None, Term.Return(lit())), Case(lit(2), None, lit(3))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: return in a match case, next case indented") {
    val code =
      """|def f: Int =
         |  x match
         |    case 1 => return case 2 => 3
         |""".stripMargin
    val layout =
      """|def f: Int = x match {
         |  case 1 =>
         |    return
         |  case 2 =>
         |    3
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      Some(pname("Int")),
      tmatch("x", Case(lit(1), None, Term.Return(lit())), Case(lit(2), None, lit(3))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: typed case after a lambda arrow and an indent") {
    val code =
      """|val f = (x: Int) =>
         |  case y: Int => y
         |  case _ => 0
         |""".stripMargin
    val layout =
      """|val f = (x: Int) => {
         |  case y: Int => y
         |  case _ => 0
         |}
         |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(patvar("f")),
      None,
      tfunc(tparam("x", "Int"))(Term.PartialFunction(List(
        Case(Pat.Typed(patvar("y"), pname("Int")), None, tname("y")),
        Case(Pat.Wildcard(), None, lit(0)),
      ))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: typed case after a colon lambda arrow and an indent") {
    val code =
      """|def g = xs.map: x =>
         |  case y: Int => y
         |""".stripMargin
    val layout =
      """|def g = xs.map {
         |  x => {
         |    case y: Int => y
         |  }
         |}
         |""".stripMargin
    val pf = Term.PartialFunction(List(Case(Pat.Typed(patvar("y"), pname("Int")), None, tname("y"))))
    val tree = Defn
      .Def(Nil, tname("g"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(pf))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: guarded case after a lambda arrow, colon lambda on the next line") {
    val code =
      """|object A:
         |  val f = (x: Int) => case y if y > 0 => y
         |  def g = xs.map: z => z
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of simple expression
         |  val f = (x: Int) => case y if y > 0 => y
         |                      ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: two colon lambdas on one line, indented body") {
    val code =
      """|def f = fooX: x => fooY: y =>
         |  body
         |""".stripMargin
    val layout =
      """|def f = fooX {
         |  x => fooY {
         |    y => body
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("fooX", blk(tfunc(tparam("x"))(tapply("fooY", blk(tfunc(tparam("y"))(tname("body"))))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, trailing infix operator") {
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
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.Try(tname("a"), List(Case(patvar("e"), None, tinfix("b", "+", "c"))), None),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: trailing infix operator, operand on the next line") {
    val code =
      """|def f = xs.map: x => x +
         |  y
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => x + y
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix("x", "+", "y")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, trailing alphanumeric operator") {
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
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Try(tname("a"), List(Case(patvar("e"), None, tinfix("b", "max", "c"))), None),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: enum in braces, typed self type before a case") {
    val code =
      """|object A:
         |  enum E { self: A => case X }
         |""".stripMargin
    val layout = "object A { enum E { self: A => case X } }"
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Enum(
        Nil,
        pname("E"),
        Nil,
        ctor,
        tpl(self("self", pname("A")), Defn.EnumCase(Nil, tname("X"), Nil, ctor, Nil)),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: enum in braces, self alias before a case") {
    val code =
      """|object A:
         |  enum E { self => case X }
         |""".stripMargin
    val layout = "object A { enum E { self => case X } }"
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Enum(
        Nil,
        pname("E"),
        Nil,
        ctor,
        tpl(self("self"), Defn.EnumCase(Nil, tname("X"), Nil, ctor, Nil)),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: context bound with a function type after an upper bound") {
    val code =
      """|trait X:
         |  type T <: U : C => D
         |""".stripMargin
    val layout = "trait X { type T <: U: C => D }"
    val tree = Defn.Trait(
      Nil,
      pname("X"),
      Nil,
      ctor,
      tpl(
        Decl
          .Type(Nil, pname("T"), Nil, bounds(hi = pname("U"), cb = List(pfunc(pname("C"))(pname("D"))))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: context bound with a function type after a type parameter") {
    val code =
      """|trait X:
         |  type T[Y]: C => D
         |""".stripMargin
    val layout = "trait X { type T[Y]: C => D }"
    val tree = Defn.Trait(
      Nil,
      pname("X"),
      Nil,
      ctor,
      tpl(Decl.Type(Nil, pname("T"), List(pparam("Y")), bounds(cb = List(pfunc(pname("C"))(pname("D")))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: context bound with a function type, type outside a template") {
    val code = "type T: C => D"
    val tree = Decl.Type(Nil, pname("T"), Nil, bounds(cb = List(pfunc(pname("C"))(pname("D")))))
    runTestAssert[Stat](code)(tree)
  }

  test("SIP-75 colon: abstract type and semicolon before a given with an indented body") {
    val code =
      """|trait X:
         |  type T; given Ord[Int]:
         |    def compare(a: Int, b: Int) = 0
         |""".stripMargin
    val layout =
      """|trait X {
         |  type T
         |  given Ord[Int] { def compare(a: Int, b: Int) = 0 }
         |}
         |""".stripMargin
    val tree = Defn.Trait(
      Nil,
      pname("X"),
      Nil,
      ctor,
      tpl(
        Decl.Type(Nil, pname("T"), Nil, noBounds),
        Defn.Given(
          Nil,
          anon,
          Nil,
          tpl(
            List(init(papply("Ord", "Int"))),
            List(Defn.Def(
              Nil,
              tname("compare"),
              Nil,
              List(List(tparam("a", "Int"), tparam("b", "Int"))),
              None,
              lit(0),
            )),
          ),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: abstract val and semicolon before a def") {
    val code =
      """|object A:
         |  val x: Int; def f = xs.map: y => y
         |""".stripMargin
    val layout =
      """|object A {
         |  val x: Int
         |  def f = xs.map {
         |    y => y
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Decl.Val(Nil, List(patvar("x")), pname("Int")),
        Defn.Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))("y")))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, alphanumeric infix body, deeper line, outdented line") {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => a max b
         |      c
         |    d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        a max b
         |        c
         |    }
         |    d
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.Try(
            tname("a"),
            List(Case(patvar("e"), None, blk(tinfix("a", "max", "b"), tname("c")))),
            None,
          ),
          tname("d"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, symbolic infix body, deeper line, outdented line") {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => a + b
         |      c
         |    d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        a + b
         |        c
         |    }
         |    d
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.Try(
            tname("a"),
            List(Case(patvar("e"), None, blk(tinfix("a", "+", "b"), tname("c")))),
            None,
          ),
          tname("d"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, prefix operator body, deeper line, outdented line") {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => -b
         |      c
         |    d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        -b
         |        c
         |    }
         |    d
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.Try(
            tname("a"),
            List(Case(patvar("e"), None, blk(Term.ApplyUnary(tname("-"), tname("b")), tname("c")))),
            None,
          ),
          tname("d"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, second case on the same line") {
    val code =
      """|object A:
         |  def f = try a catch case e => 1 case f => 2
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e => 1
         |    case f => 2
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.Try(
          tname("a"),
          List(Case(patvar("e"), None, lit(1)), Case(patvar("f"), None, lit(2))),
          None,
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line in parens, semicolon after the body") {
    val code =
      """|object A:
         |  def f = g(try a catch case e => b; c)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = g(try a catch {
         |    case e =>
         |      b
         |      c
         |  })
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply("g", Term.Try(tname("a"), List(Case(patvar("e"), None, blk("b", "c"))), None)),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in try, catch on the same line") {
    val code =
      """|object A:
         |  def f = try xs.map: x => x catch case e => 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try xs.map {
         |    x => x
         |  } catch {
         |    case e => 1
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.Try(
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))("x"))),
          List(Case(patvar("e"), None, lit(1))),
          None,
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in try, finally on the same line") {
    val code =
      """|object A:
         |  def f = try xs.map: x => x finally g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try xs.map {
         |    x => x
         |  } finally g
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.Try(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))("x"))), Nil, Some(tname("g"))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new body, deeper with on the next line") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => new A
         |      with B
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => new A with B
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(Term.NewAnonymous(tplNoBody(init("A"), init("B"))))),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: try body, catch on the next line at the statement indent") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => try a
         |    catch case e => b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => try a catch {
         |      case e => b
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(Term.Try(tname("a"), List(Case(patvar("e"), None, "b")), None))),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: for generator, function type ascription in parens") {
    val code =
      """|object A:
         |  def f = for x: (A => B) <- fs yield x
         |""".stripMargin
    val layout = "object A { def f = for (x: (A => B) <- fs) yield x }"
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.ForYield(
          List(Enumerator.Generator(Pat.Typed(patvar("x"), pfunc(pname("A"))(pname("B"))), "fs")),
          tname("x"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: for generator, function type ascription") {
    val code =
      """|object A:
         |  def g = for x: A => B <- fs yield x
         |""".stripMargin
    val error =
      """|<input>:2: error: `<-` expected but `=>` found
         |  def g = for x: A => B <- fs yield x
         |                   ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 colon: for generator in braces, function type ascription") {
    val code =
      """|object A:
         |  def f = for { x: A => B <- fs } yield x
         |""".stripMargin
    val error =
      """|<input>:2: error: `<-` expected but `=>` found
         |  def f = for { x: A => B <- fs } yield x
         |                     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: braces arg, typed param name without parens") {
    val code =
      """|object A:
         |  def f = xs.map { x: Int => x }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x {
         |      Int => x
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tselect("xs", "map"), blk(tapply("x", blk(tfunc(tparam("Int"))("x"))))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: typed pattern after an alphanumeric infix operator") {
    val code =
      """|object A:
         |  def f = a orElse case x: Int => x
         |""".stripMargin
    val error =
      """|<input>:2: error: `;` expected but `case` found
         |  def f = a orElse case x: Int => x
         |                   ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: in an if condition, then on the same line") {
    val code =
      """|object A:
         |  def f = if xs.exists: x => x then 1 else 2
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.exists {
         |    x => x
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))("x"))), lit(1), lit(2)),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: deeper argument in parens on the next line") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => f
         |      (y)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => f(y)
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tapply("f", "y")))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: indented enum, typed self type before a case") {
    val code =
      """|object A:
         |  enum E:
         |    self: A =>
         |    case X
         |""".stripMargin
    val layout = "object A { enum E { self: A => case X } }"
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Enum(
        Nil,
        pname("E"),
        Nil,
        ctor,
        tpl(self("self", pname("A")), Defn.EnumCase(Nil, tname("X"), Nil, ctor, Nil)),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, deeper case on the next line") {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => b
         |      case g => c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e => b
         |    case g => c
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.Try(
          tname("a"),
          List(Case(patvar("e"), None, tname("b")), Case(patvar("g"), None, tname("c"))),
          None,
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if body, else on the next line at the statement indent") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => if x then a
         |    else b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (x) a else b
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif("x", "a", "b")))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line in braces, semicolon after the body") {
    val code =
      """|object A:
         |  def f = { try a catch case e => b; c }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        b
         |        c
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.Try(tname("a"), List(Case(patvar("e"), None, blk("b", "c"))), None)),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line, alphanumeric infix with name operands, deeper line, outdented line") {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => xs map f
         |      c
         |    d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        xs map f
         |        c
         |    }
         |    d
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.Try(
            tname("a"),
            List(Case(patvar("e"), None, blk(tinfix("xs", "map", "f"), tname("c")))),
            None,
          ),
          tname("d"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: assignment body, deeper line") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => a = 1
         |      b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => a = 1
         |    }
         |    b
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.Assign("a", lit(1))))),
          tname("b"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line after then, else on the same line") {
    val code =
      """|object A:
         |  def f = if c then try a catch case e => b else d
         |""".stripMargin
    val error =
      """|<input>:2: error: `outdent` expected but `else` found
         |  def f = if c then try a catch case e => b else d
         |                                            ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after a lambda arrow in braces") {
    val code =
      """|object A:
         |  def f = xs.map { x => case y: Int => y }
         |""".stripMargin
    val error =
      """|<input>:2: error: `}` expected but `case` found
         |  def f = xs.map { x => case y: Int => y }
         |                        ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: typed pattern after a lambda arrow in a class in braces") {
    val code =
      """|class C { val f = (x: Int) => case y: Int => y }
         |""".stripMargin
    val error =
      """|<input>:1: error: illegal start of simple expression
         |class C { val f = (x: Int) => case y: Int => y }
         |                              ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 catch case on the same line, deeper leading select and infix, outdented line") {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e => b
         |      .foo + c
         |    d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    try a catch {
         |      case e =>
         |        b.foo + c
         |    }
         |    d
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.Try(
            tname("a"),
            List(Case(patvar("e"), None, tinfix(tselect("b", "foo"), "+", "c"))),
            None,
          ),
          tname("d"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an if condition, call body, then on the same line") {
    val code =
      """|object A:
         |  def f = if xs.exists: x => g(x) then 1 else 2
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.exists {
         |    x => g(x)
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tapply("g", "x")))),
          lit(1),
          lit(2),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: if condition in parens, lambda with a call body") {
    val code =
      """|object A:
         |  def f = if (xs.exists: x => g(x)) 1 else 2
         |""".stripMargin
    val error =
      """|<input>:2: error: `)` expected but `(` found
         |  def f = if (xs.exists: x => g(x)) 1 else 2
         |                               ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: in a while condition, call body, do on the same line") {
    val code =
      """|object A:
         |  def f = while xs.exists: x => g(x) do ()
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while (xs.exists {
         |    x => g(x)
         |  }) ()
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tapply("g", "x")))), lit()),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: while condition in parens, lambda with a call body") {
    val code =
      """|object A:
         |  def f = while (xs.exists: x => g(x)) ()
         |""".stripMargin
    val error =
      """|<input>:2: error: `)` expected but `(` found
         |  def f = while (xs.exists: x => g(x)) ()
         |                                  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: for generator, call body, yield on the same line") {
    val code =
      """|object A:
         |  def f = for x <- xs.map: y => g(y) yield x
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- xs.map {
         |    y => g(y)
         |  }) yield x
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.ForYield(
          List(Enumerator.Generator(
            patvar("x"),
            tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tapply("g", "y")))),
          )),
          tname("x"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: for generator in parens, lambda with a call body") {
    val code =
      """|object A:
         |  def f = for (x <- xs.map: y => g(y)) yield x
         |""".stripMargin
    val error =
      """|<input>:2: error: `)` expected but `<-` found
         |  def f = for (x <- xs.map: y => g(y)) yield x
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: for generator in braces, call body, closed on the same line") {
    val code =
      """|object A:
         |  def f = for { x <- xs.map: y => g(y) } yield x
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- xs.map {
         |    y => g(y)
         |  }) yield x
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.ForYield(
          List(Enumerator.Generator(
            patvar("x"),
            tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tapply("g", "y")))),
          )),
          tname("x"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in then in parens, call body, else on the same line") {
    val code =
      """|object A:
         |  def f = foo(if c then xs.map: x => g(x) else y)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = foo(if (c) xs.map {
         |    x => g(x)
         |  } else y)
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          "foo",
          tif("c", tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tapply("g", "x")))), "y"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in then after a condition in parens, in parens, call body") {
    val code =
      """|object A:
         |  def f = foo(if (c) xs.map: x => g(x) else y)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = foo(if (c) xs.map {
         |    x => g(x)
         |  } else y)
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          "foo",
          tif("c", tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tapply("g", "x")))), "y"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in try in parens, call body, finally on the same line") {
    val code =
      """|object A:
         |  def f = foo(try xs.map: x => g(x) finally h)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = foo(try xs.map {
         |    x => g(x)
         |  } finally h)
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          "foo",
          Term.Try(
            tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tapply("g", "x")))),
            Nil,
            Some(tname("h")),
          ),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in then, if body, else on the next line at the statement indent") {
    val code =
      """|object A:
         |  def f =
         |    if c then xs.map: x => if x then a
         |    else b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (c) xs.map {
         |    x => if (x) a else b
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif("c", tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif("x", "a", "b"))))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in try, try body, catch on the next line at the statement indent") {
    val code =
      """|object A:
         |  def f =
         |    try xs.map: x => try a
         |    catch case e => b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try xs.map {
         |    x => try a catch {
         |      case e => b
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.Try(
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.Try(tname("a"), List(Case(patvar("e"), None, "b")), None))),
          ),
          Nil,
          None,
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 catch case on the same line after then, deeper else on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if c then try a catch case e => b
         |      else d
         |""".stripMargin
    val error =
      """|<input>:4: error: `outdent` expected but `else` found
         |      else d
         |      ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: catch case in the body, deeper case on the next line") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => try a catch case e => b
         |      case g => c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => try a catch {
         |      case e => b
         |      case g => c
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(Term.Try(
            tname("a"),
            List(Case(patvar("e"), None, tname("b")), Case(patvar("g"), None, tname("c"))),
            None,
          ))),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, trailing infix operator, deeper operand") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => try a catch case e => b +
         |      c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => try a catch {
         |      case e =>
         |        b + c
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(
            Term.Try(tname("a"), List(Case(patvar("e"), None, tinfix("b", "+", "c"))), None),
          )),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "SIP-75 single-line lambda: catch case in the body, deeper argument in parens on the next line",
  ) {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => try a catch case e => b
         |      (y)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => try a catch {
         |      case e =>
         |        b(y)
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(
            Term.Try(tname("a"), List(Case(patvar("e"), None, tapply("b", "y"))), None),
          )),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: typed lambda arrow in a match case, next case on the same line") {
    val code =
      """|object A:
         |  def f = x match { case 1 => (y: Int) => case 2 => 3 }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      (y: Int) => {}
         |    case 2 =>
         |      3
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tmatch("x", Case(lit(1), None, tfunc(tparam("y", "Int"))(blk())), Case(lit(2), None, lit(3))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: catch case arrow at the end of the line, deeper case") {
    val code =
      """|object A:
         |  def f =
         |    try a catch case e =>
         |      case x => x
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      {
         |        case x => x
         |      }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.Try(
          tname("a"),
          List(Case(patvar("e"), None, Term.PartialFunction(List(Case(patvar("x"), None, "x"))))),
          None,
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: ascription body with a compound type") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => x: A with B
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x: A with B
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(Term.Ascribe("x", Type.With(pname("A"), pname("B"))))),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for generator in braces, closing brace on the next line") {
    val code =
      """|object A:
         |  def f = for { x <- xs.map: y => y
         |  } yield x
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- xs.map {
         |    y => y
         |  }) yield x
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.ForYield(
          List(
            Enumerator
              .Generator(patvar("x"), tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))("y")))),
          ),
          tname("x"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new body, with on the next line at the statement indent") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => new A
         |    with B
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => new A with B
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(Term.NewAnonymous(tplNoBody(init("A"), init("B"))))),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match at the line end, deeper case, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => x match
         |      case 1 => 2
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => x match {
         |        case 1 => 2
         |      }
         |    }
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tmatch("x", Case(lit(1), None, lit(2)))))),
          tname("g"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: call body, deeper braces, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => g(x)
         |      { y }
         |    h
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => g(x) {
         |        y
         |      }
         |    }
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tapply(tapply("g", "x"), blk("y"))))),
          tname("h"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in then, if body, deeper else, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if c then xs.map: x => if x then a
         |      else b
         |    h
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    if (c) xs.map {
         |      x => if (x) a else b
         |    }
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif("c", tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif("x", "a", "b"))))),
          tname("h"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: then at the line end, deeper then, else at the statement indent") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => if x then
         |      a
         |    else
         |      b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (x) a else b
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif("x", "a", "b")))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: try at the line end, deeper body, catch at the statement indent") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => try
         |      a
         |    catch case e => b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => try a catch {
         |      case e => b
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(Term.Try(tname("a"), List(Case(patvar("e"), None, "b")), None))),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: colon at the line end, deeper argument, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => foo:
         |      y
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => foo {
         |        y
         |      }
         |    }
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tapply(tname("foo"), blk("y"))))),
          tname("g"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new with a colon at the line end, deeper member, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => new A:
         |      def g = 1
         |    h
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => new A { def g = 1 }
         |    }
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.NewAnonymous(
              tpl(List(init("A")), List(Defn.Def(Nil, tname("g"), Nil, None, lit(1)))),
            ))),
          ),
          tname("h"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in a case in braces, next case on the same line") {
    val code =
      """|object A:
         |  def f = x match { case 1 => xs.map: y => y case 2 => 3 }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      xs.map {
         |        y => y
         |      }
         |    case 2 =>
         |      3
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tmatch(
          "x",
          Case(lit(1), None, tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))("y")))),
          Case(lit(2), None, lit(3)),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match at the line end, deeper case, leading select") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => x match
         |      case 1 => 2
         |    .foo
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x match {
         |      case 1 => 2
         |    }
         |  }.foo
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tselect(
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tmatch("x", Case(lit(1), None, lit(2)))))),
          "foo",
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: yield at the line end, deeper body, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => for y <- ys yield
         |      y
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => for (y <- ys) yield y
         |    }
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term.ForYield(List(Enumerator.Generator(patvar("y"), "ys")), tname("y")),
            )),
          ),
          tname("g"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match at the line end, case at the statement indent") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => x match
         |    case 1 => 2
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x match {
         |      case 1 => 2
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tmatch("x", Case(lit(1), None, lit(2)))))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: else at the line end, deeper else body, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => if x then a else
         |      b
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => if (x) a else b
         |    }
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif("x", "a", "b")))), tname("g")),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new body, deeper braces, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => new A
         |      { def g = 1 }
         |    h
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => new A { def g = 1 }
         |    }
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.NewAnonymous(
              tpl(List(init("A")), List(Defn.Def(Nil, tname("g"), Nil, None, lit(1)))),
            ))),
          ),
          tname("h"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, in a match case, next case") {
    val code =
      """|object A:
         |  def f =
         |    x match
         |      case 1 => xs.map: y => try a catch case e => b
         |      case 2 => 3
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      xs.map {
         |        y => try a catch {
         |          case e => b
         |          case 2 => 3
         |        }
         |      }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tmatch(
          "x",
          Case(
            lit(1),
            None,
            tapply(
              tselect("xs", "map"),
              blk(tfunc(tparam("y"))(Term.Try(
                tname("a"),
                List(Case(patvar("e"), None, "b"), Case(lit(2), None, lit(3))),
                None,
              ))),
            ),
          ),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match at the statement indent, deeper case") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => x
         |    match
         |      case 1 => 2
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x match {
         |      case 1 => 2
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tmatch("x", Case(lit(1), None, lit(2)))))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if body, deeper leading select") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => if x then a else b
         |      .foo
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (x) a else b.foo
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif("x", "a", tselect("b", "foo"))))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: return at the line end, top level, deeper line") {
    val code =
      """|def f = xs.map: x => return
         |  y
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => return y
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.Return("y")))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: return at the line end, deeper line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => return
         |      y
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => return y
         |    }
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.Return("y")))), tname("g")),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: infix body, deeper leading select") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => a + b
         |      .foo
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => a + b.foo
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix("a", "+", tselect("b", "foo"))))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match in braces, deeper leading select") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => x match { case 1 => 2 }
         |      .foo
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x match {
         |      case 1 => 2
         |    }
         |  }.foo
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tselect(
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tmatch("x", Case(lit(1), None, lit(2)))))),
          "foo",
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case expression after colon, next line, in an object") {
    val code =
      """|object A:
         |  def f = xs.map:
         |    case x => x
         |    .foo
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    case x =>
         |      x.foo
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          Term.PartialFunction(List(Case(patvar("x"), None, tselect("x", "foo")))),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, deeper line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => try a catch case e => b
         |      c
         |    d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => try a catch {
         |        case e =>
         |          b
         |          c
         |      }
         |    }
         |    d
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term.Try(tname("a"), List(Case(patvar("e"), None, blk("b", "c"))), None),
            )),
          ),
          tname("d"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, semicolon, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => try a catch case e => b; c
         |    d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => try a catch {
         |        case e =>
         |          b
         |          c
         |      }
         |    }
         |    d
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term.Try(tname("a"), List(Case(patvar("e"), None, blk("b", "c"))), None),
            )),
          ),
          tname("d"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for body with a semicolon, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => for y <- ys; z <- zs yield z
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => for (y <- ys; z <- zs) yield z
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Object(
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
            blk(tfunc(tparam("x"))(Term.ForYield(
              List(Enumerator.Generator(patvar("y"), "ys"), Enumerator.Generator(patvar("z"), "zs")),
              tname("z"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: for with a semicolon, no lambda, next definition") {
    val code =
      """|object A:
         |  def f = for y <- ys; z <- zs yield z
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (y <- ys; z <- zs) yield z
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          Term.ForYield(
            List(Enumerator.Generator(patvar("y"), "ys"), Enumerator.Generator(patvar("z"), "zs")),
            tname("z"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case after a lambda arrow on the same line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => case y => y
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.map: x => case y => y
         |                       ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 relaxed lambda: lambda arrow at the line end in an object, deeper case") {
    val code =
      """|object A:
         |  xs.foreach: x =>
         |    case 1 => 2
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  xs.foreach {
         |    x => {
         |      case 1 => 2
         |    }
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(
        tapply(
          tselect("xs", "foreach"),
          blk(tfunc(tparam("x"))(Term.PartialFunction(List(Case(lit(1), None, lit(2)))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while body with a semicolon, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while x > 0; x < 9 do g(x)
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `while` found
         |  def f = xs.foreach: x => while x > 0; x < 9 do g(x)
         |                           ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 single-line lambda: if body with a semicolon, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if a; b then c else d
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `if` found
         |  def f = xs.map: x => if a; b then c else d
         |                       ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "SIP-75 single-line lambda: if with `new` in the condition, then and else, next definition",
  ) {
    val code =
      """|object A:
         |  def f = xs.map: x => if new B(x).ok then c else d
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
         |KwNew [36..39)
         |Ident(B) [40..41)
         |LeftParen [41..42)
         |Ident(x) [42..43)
         |RightParen [43..44)
         |Dot [44..45)
         |Ident(ok) [45..47)
         |KwThen [48..52)
         |Ident(c) [53..54)
         |KwElse [55..59)
         |Ident(d) [60..61)
         |LF [61..62)
         |KwDef [64..67)
         |Ident(g) [68..69)
         |Equals [70..71)
         |Constant.Int(1) [72..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (new B(x).ok) c else d
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val cond = tselect(Term.New(init(pname("B"), List(tname("x")))), "ok")
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for with `new` in a generator and yield, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => for y <- new B(x).ys yield y
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
         |KwFor [33..36)
         |Ident(y) [37..38)
         |LeftArrow [39..41)
         |KwNew [42..45)
         |Ident(B) [46..47)
         |LeftParen [47..48)
         |Ident(x) [48..49)
         |RightParen [49..50)
         |Dot [50..51)
         |Ident(ys) [51..53)
         |KwYield [54..59)
         |Ident(y) [60..61)
         |LF [61..62)
         |KwDef [64..67)
         |Ident(g) [68..69)
         |Equals [70..71)
         |Constant.Int(1) [72..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => for (y <- new B(x).ys) yield y
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.ForYield(
              List(
                Enumerator
                  .Generator(patvar("y"), tselect(Term.New(init(pname("B"), List(tname("x")))), "ys")),
              ),
              tname("y"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with an if without else in the condition, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => while if a then b do c
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
         |KwWhile [33..38)
         |KwIf [39..41)
         |Ident(a) [42..43)
         |KwThen [44..48)
         |Ident(b) [49..50)
         |KwDo [51..53)
         |Ident(c) [54..55)
         |LF [55..56)
         |KwDef [58..61)
         |Ident(g) [62..63)
         |Equals [64..65)
         |Constant.Int(1) [66..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => while (if (a) b) c
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.While(tif(tname("a"), tname("b")), tname("c")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test(
    "SIP-75 single-line lambda: if with `new` in the condition, then without else, next definition",
  ) {
    val code =
      """|object A:
         |  def f = xs.map: x => if new B(x).ok then c
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
         |KwNew [36..39)
         |Ident(B) [40..41)
         |LeftParen [41..42)
         |Ident(x) [42..43)
         |RightParen [43..44)
         |Dot [44..45)
         |Ident(ok) [45..47)
         |KwThen [48..52)
         |Ident(c) [53..54)
         |LF [54..55)
         |KwDef [57..60)
         |Ident(g) [61..62)
         |Equals [63..64)
         |Constant.Int(1) [65..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (new B(x).ok) c
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              tif(tselect(Term.New(init(pname("B"), List(tname("x")))), "ok"), tname("c")),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with `new` in the condition, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => while new B(x).ok do c
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
         |KwWhile [33..38)
         |KwNew [39..42)
         |Ident(B) [43..44)
         |LeftParen [44..45)
         |Ident(x) [45..46)
         |RightParen [46..47)
         |Dot [47..48)
         |Ident(ok) [48..50)
         |KwDo [51..53)
         |Ident(c) [54..55)
         |LF [55..56)
         |KwDef [58..61)
         |Ident(g) [62..63)
         |Equals [64..65)
         |Constant.Int(1) [66..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => while (new B(x).ok) c
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term.While(tselect(Term.New(init(pname("B"), List(tname("x")))), "ok"), tname("c")),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 colon: polymorphic function type after a colon") {
    val code =
      """|object A:
         |  val g = f: [T] => T => T
         |""".stripMargin
    val layout = "object A { val g = f: ([T] => T => T) }"
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Val(
        Nil,
        List(patvar("g")),
        None,
        Term.Ascribe("f", ppolyfunc(pparam("T"))(pfunc(pname("T"))(pname("T")))),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in a class, deeper leading select, next definition") {
    val code =
      """|class C:
         |  xs.foreach: x => g(x)
         |    .foo
         |  def h = 1
         |""".stripMargin
    val layout =
      """|class C {
         |  xs.foreach {
         |    x => g(x).foo
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val tree = Defn.Class(
      Nil,
      pname("C"),
      Nil,
      EmptyCtor(),
      tpl(
        tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tselect(tapply("g", "x"), "foo")))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: lambda arrow at the line end in an object, deeper typed case") {
    val code =
      """|object A:
         |  xs.foreach: x =>
         |    case y: Int => y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  xs.foreach {
         |    x => {
         |      case y: Int => y
         |    }
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(
        tapply(
          tselect("xs", "foreach"),
          blk(tfunc(tparam("x"))(Term.PartialFunction(List(
            Case(Pat.Typed(patvar("y"), pname("Int")), None, "y"),
          )))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: lambda arrow at the line end in a class in braces, deeper typed case") {
    val code =
      """|class C {
         |  xs.foreach: x =>
         |    case y: Int => y
         |  def g = 1
         |}
         |""".stripMargin
    val layout =
      """|class C {
         |  xs.foreach {
         |    x => {
         |      case y: Int => y
         |    }
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Class(
      Nil,
      pname("C"),
      Nil,
      EmptyCtor(),
      tpl(
        tapply(
          tselect("xs", "foreach"),
          blk(tfunc(tparam("x"))(Term.PartialFunction(List(
            Case(Pat.Typed(patvar("y"), pname("Int")), None, "y"),
          )))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for body with a semicolon, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.map: x => for y <- ys; z <- zs yield z
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.map {
         |      x => for (y <- ys; z <- zs) yield z
         |    }
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.ForYield(
              List(Enumerator.Generator(patvar("y"), "ys"), Enumerator.Generator(patvar("z"), "zs")),
              tname("z"),
            ))),
          ),
          tname("g"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: context bound with a function type") {
    val code =
      """|object A:
         |  def f[T: C => D] = 1
         |""".stripMargin
    val layout = "object A { def f[T: C => D] = 1 }"
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        List(pparam("T", bounds(cb = List(pfunc(pname("C"))(pname("D")))))),
        Nil,
        None,
        lit(1),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for body with a semicolon and a guard, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => for y <- ys; if y > 0 do g(y)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => for (y <- ys; if y > 0) g(y)
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.For(
              List(Enumerator.Generator(patvar("y"), "ys"), Enumerator.Guard(tinfix("y", ">", lit(0)))),
              tapply("g", "y"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: term entry point, for body with a semicolon") {
    implicit val parser: String => Term = term(_)
    val layout =
      """|xs.map {
         |  x => for (y <- ys; z <- zs) yield z
         |}""".stripMargin
    val tree = tapply(
      tselect("xs", "map"),
      blk(tfunc(tparam("x"))(Term.ForYield(
        List(Enumerator.Generator(patvar("y"), "ys"), Enumerator.Generator(patvar("z"), "zs")),
        tname("z"),
      ))),
    )
    runTestAssert[Term]("xs.map: x => for y <- ys; z <- zs yield z", layout)(tree)
  }

  test("SIP-75 single-line lambda: term entry point, for body with a semicolon and a guard") {
    implicit val parser: String => Term = term(_)
    val layout =
      """|xs.foreach {
         |  x => for (y <- ys; if y > 0) g(y)
         |}""".stripMargin
    val tree = tapply(
      tselect("xs", "foreach"),
      blk(tfunc(tparam("x"))(Term.For(
        List(Enumerator.Generator(patvar("y"), "ys"), Enumerator.Guard(tinfix("y", ">", lit(0)))),
        tapply("g", "y"),
      ))),
    )
    runTestAssert[Term]("xs.foreach: x => for y <- ys; if y > 0 do g(y)", layout)(tree)
  }

  test("SIP-75 single-line lambda: top level, for body with a semicolon, next definition") {
    val code =
      """|def f = xs.map: x => for y <- ys; z <- zs yield z
         |def g = 1
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => for (y <- ys; z <- zs) yield z
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(Term.ForYield(
            List(Enumerator.Generator(patvar("y"), "ys"), Enumerator.Generator(patvar("z"), "zs")),
            tname("z"),
          ))),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for body with a semicolon and a guard, next statement") {
    val code =
      """|object A:
         |  def f =
         |    xs.foreach: x => for y <- ys; if y > 0 do g(y)
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.foreach {
         |      x => for (y <- ys; if y > 0) g(y)
         |    }
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.For(
              List(Enumerator.Generator(patvar("y"), "ys"), Enumerator.Guard(tinfix("y", ">", lit(0)))),
              tapply("g", "y"),
            ))),
          ),
          tname("g"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: lambda arrow at the line end in an indented class, deeper typed case") {
    val code =
      """|class C:
         |  xs.foreach: x =>
         |    case y: Int => y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|class C {
         |  xs.foreach {
         |    x => {
         |      case y: Int => y
         |    }
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("C"),
      Nil,
      EmptyCtor(),
      tpl(
        tapply(
          tselect("xs", "foreach"),
          blk(tfunc(tparam("x"))(Term.PartialFunction(List(
            Case(Pat.Typed(patvar("y"), pname("Int")), None, "y"),
          )))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: lambda arrow at the line end in a trait, deeper typed case") {
    val code =
      """|trait T:
         |  xs.foreach: x =>
         |    case y: Int => y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|trait T {
         |  xs.foreach {
         |    x => {
         |      case y: Int => y
         |    }
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Trait(
      Nil,
      pname("T"),
      Nil,
      EmptyCtor(),
      tpl(
        tapply(
          tselect("xs", "foreach"),
          blk(tfunc(tparam("x"))(Term.PartialFunction(List(
            Case(Pat.Typed(patvar("y"), pname("Int")), None, "y"),
          )))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: lambda arrow at the line end in a block, deeper typed case") {
    val code =
      """|def f =
         |  xs.foreach: x =>
         |    case y: Int => y
         |  g
         |""".stripMargin
    val layout =
      """|def f = {
         |  xs.foreach {
         |    x => {
         |      case y: Int => y
         |    }
         |  }
         |  g
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tapply(
          tselect("xs", "foreach"),
          blk(tfunc(tparam("x"))(Term.PartialFunction(List(
            Case(Pat.Typed(patvar("y"), pname("Int")), None, "y"),
          )))),
        ),
        tname("g"),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: term entry point, lambda arrow at the line end, deeper typed case") {
    implicit val parser: String => Term = term(_)
    val code =
      """|xs.foreach: x =>
         |  case y: Int => y""".stripMargin
    val layout =
      """|xs.foreach {
         |  x => {
         |    case y: Int => y
         |  }
         |}""".stripMargin
    val tree = tapply(
      tselect("xs", "foreach"),
      blk(tfunc(tparam("x"))(Term.PartialFunction(List(
        Case(Pat.Typed(patvar("y"), pname("Int")), None, "y"),
      )))),
    )
    runTestAssert[Term](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: template entry point in braces, lambda arrow at the line end, deeper typed case") {
    implicit val parser: String => Template = parseRule(_, Parse.parseTemplate)
    val code =
      """|A {
         |  xs.foreach: x =>
         |    case y: Int => y
         |  def g = 1
         |}""".stripMargin
    val layout =
      """|extends A {
         |  xs.foreach {
         |    x => {
         |      case y: Int => y
         |    }
         |  }
         |  def g = 1
         |}""".stripMargin
    val tree = tpl(
      List(init("A")),
      List(
        tapply(
          tselect("xs", "foreach"),
          blk(tfunc(tparam("x"))(Term.PartialFunction(List(
            Case(Pat.Typed(patvar("y"), pname("Int")), None, "y"),
          )))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    parseAndCheckTree[Template](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: template entry point with a colon, lambda arrow at the line end, deeper typed case") {
    implicit val parser: String => Template = parseRule(_, Parse.parseTemplate)
    val code =
      """|A:
         |  xs.foreach: x =>
         |    case y: Int => y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|extends A {
         |  xs.foreach {
         |    x => {
         |      case y: Int => y
         |    }
         |  }
         |  def g = 1
         |}""".stripMargin
    val tree = tpl(
      List(init("A")),
      List(
        tapply(
          tselect("xs", "foreach"),
          blk(tfunc(tparam("x"))(Term.PartialFunction(List(
            Case(Pat.Typed(patvar("y"), pname("Int")), None, "y"),
          )))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    parseAndCheckTree[Template](code, layout)(tree)
  }

  test("SIP-75 colon: type parameter entry point, context bound with a function type") {
    implicit val parser: String => Type.Param = parseRule(_, Parse.parseTypeParam)
    val tree = pparam("T", bounds(cb = List(pfunc(pname("C"))(pname("D")))))
    runTestAssert[Type.Param]("T: C => D")(tree)
  }

  test(
    "SIP-75 single-line lambda: if with parentheses at the line end, deeper body, next definition",
  ) {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a)
         |      b
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a) b
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(tname("a"), tname("b"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses at the line end, deeper body, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a)
         |      g(x)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => while (a) g(x)
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(tname("a"), tapply("g", "x")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test(
    "SIP-75 single-line lambda: if condition at the line end, deeper leading infix, next definition",
  ) {
    val code =
      """|object A:
         |  def f = xs.map: x => if a
         |      && b then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b) c else d
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tinfix("a", "&&", "b"), tname("c"), tname("d")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for body with a value enumerator and an if, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => for y <- ys; z = if y > 0 then 1 else 2 yield z
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => for (y <- ys; z = if (y > 0) 1 else 2) yield z
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.ForYield(
              List(
                Enumerator.Generator(patvar("y"), "ys"),
                Enumerator.Val(patvar("z"), tif(tinfix("y", ">", lit(0)), lit(1), lit(2))),
              ),
              tname("z"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for body with an if in a generator, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => for y <- if x then ys else zs yield y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => for (y <- if (x) ys else zs) yield y
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.ForYield(
              List(Enumerator.Generator(patvar("y"), tif(tname("x"), tname("ys"), tname("zs")))),
              tname("y"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for body with an if with parentheses in a generator, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => for y <- if (x) ys else zs yield y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => for (y <- if (x) ys else zs) yield y
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.ForYield(
              List(Enumerator.Generator(patvar("y"), tif(tname("x"), tname("ys"), tname("zs")))),
              tname("y"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while condition at the line end, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while a
         |      && b do g(x)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => while (a && b) g(x)
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(tinfix("a", "&&", "b"), tapply("g", "x")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: then body on the same line, deeper else, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if a then b
         |      else c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a) b else c
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(tname("a"), tname("b"), tname("c"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: first statement of an indented class, arrow body on the same line, next definition") {
    val code =
      """|class C:
         |  foo: x => g(x)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|class C { foo: x =>
         |  g(x)
         |  def h = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("C"),
      Nil,
      ctor,
      tpl(self("foo", pname("x")), tapply("g", "x"), Defn.Def(Nil, tname("h"), Nil, None, lit(1))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: first statement of an indented class, arrow at the line end, deeper typed case") {
    val code =
      """|class C:
         |  foo: x =>
         |    case y: Int => y
         |  def h = 1
         |""".stripMargin
    val error =
      """|<input>:3: error: `;` expected but `:` found
         |    case y: Int => y
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: first statement of an indented class, typed self, deeper definition") {
    val code =
      """|class C:
         |  self: A =>
         |    def f = 1
         |""".stripMargin
    val layout = "class C { self: A => def f = 1 }"
    val tree = Source(List(Defn.Class(
      Nil,
      pname("C"),
      Nil,
      ctor,
      tpl(self("self", pname("A")), Defn.Def(Nil, tname("f"), Nil, None, lit(1))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: arrow after a leading semicolon in an enum in braces") {
    val code =
      """|enum E {
         |  ; self =>
         |  case X
         |}
         |""".stripMargin
    val error =
      """|<input>:2: error: `;` expected but `=>` found
         |  ; self =>
         |         ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: first statement of a class in braces, arrow body on the same line") {
    val code = "class C { foo: x => g(x) }"
    val tree = Source(
      List(Defn.Class(Nil, pname("C"), Nil, ctor, tpl(self("foo", pname("x")), tapply("g", "x")))),
    )
    runTestAssert[Source](code)(tree)
  }

  test("SIP-75 relaxed lambda: arrow after an annotation in an enum in braces") {
    val code =
      """|enum E {
         |  @deprecated self =>
         |  case X
         |}
         |""".stripMargin
    val error =
      """|<input>:2: error: expected start of definition
         |  @deprecated self =>
         |              ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: typed self after a comment in an indented enum") {
    val code =
      """|enum E:
         |  /* c */ self: Any =>
         |  case X
         |  case Y
         |""".stripMargin
    val layout =
      """|enum E {
         |  /* c */ self: Any =>
         |  case X
         |  case Y
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Enum(
      Nil,
      pname("E"),
      Nil,
      ctor,
      tpl(
        Self.newBuilder(tname("self"), Some(pname("Any"))).begComment(Seq("/* c */")).result(),
        Defn.EnumCase(Nil, tname("X"), Nil, ctor, Nil),
        Defn.EnumCase(Nil, tname("Y"), Nil, ctor, Nil),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: case class on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => x
         |  case class B(a: Int)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x
         |  }
         |  case class B(a: Int)
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))("x")))),
        Defn.Class(List(Mod.Case()), pname("B"), Nil, ctorp(tparam("a", "Int")), tplNoBody()),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: case class on the next line, end of file") {
    val code =
      """|object A:
         |  def f = xs.map: x => x
         |  case class B(a: Int)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x
         |  }
         |  case class B(a: Int)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))("x")))),
        Defn.Class(List(Mod.Case()), pname("B"), Nil, ctorp(tparam("a", "Int")), tplNoBody()),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an enum, enum case on the next line") {
    val code =
      """|enum E:
         |  case X
         |  def f = xs.map: x => x
         |  case Y
         |""".stripMargin
    val layout =
      """|enum E {
         |  case X
         |  def f = xs.map {
         |    x => x
         |  }
         |  case Y
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Enum(
      Nil,
      pname("E"),
      Nil,
      ctor,
      tpl(
        Defn.EnumCase(Nil, tname("X"), Nil, ctor, Nil),
        Defn.Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))("x")))),
        Defn.EnumCase(Nil, tname("Y"), Nil, ctor, Nil),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in a block, case object on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val y = xs.map: x => x
         |    case object B
         |    y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val y = xs.map {
         |      x => x
         |    }
         |    case object B
         |    y
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
            List(patvar("y")),
            None,
            tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))("x"))),
          ),
          Defn.Object(List(Mod.Case()), tname("B"), tplNoBody()),
          tname("y"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: postfix operator at the line end, next definition") {
    val code =
      """|import scala.language.postfixOps
         |object A:
         |  def f = xs.map: x => x.toList sorted
         |  def g = 1
         |""".stripMargin
    val layout =
      """|import scala.language.postfixOps
         |object A {
         |  def f = xs.map {
         |    x => x.toList sorted
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(
      Import(List(Importer(tselect("scala", "language"), List(Importee.Name(Name("postfixOps")))))),
      Defn.Object(
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
              blk(tfunc(tparam("x"))(tpostfix(tselect("x", "toList"), "sorted"))),
            ),
          ),
          Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
        ),
      ),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in a block, postfix operator at the line end, next statement") {
    val code =
      """|import scala.language.postfixOps
         |object A:
         |  def f =
         |    xs.map: x => x seconds
         |    g
         |""".stripMargin
    val layout =
      """|import scala.language.postfixOps
         |object A {
         |  def f = xs.map {
         |    x => x seconds g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(
      Import(List(Importer(tselect("scala", "language"), List(Importee.Name(Name("postfixOps")))))),
      Defn.Object(
        Nil,
        tname("A"),
        tpl(Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tinfix(tname("x"), "seconds", tname("g")))),
          ),
        )),
      ),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && b
         |    && c then d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b && c) d
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term
                .If(tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")), tname("d"), lit()),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and `then`, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) then b else c
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
         |KwThen [40..44)
         |Ident(b) [45..46)
         |KwElse [47..51)
         |Ident(c) [52..53)
         |LF [53..54)
         |KwDef [56..59)
         |Ident(g) [60..61)
         |Equals [62..63)
         |Constant.Int(1) [64..65)
         |Indentation.Outdent [65..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a) b else c
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(tname("a"), tname("b"), tname("c"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and `then`, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) then b else c
         |    + 1
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
         |KwThen [40..44)
         |Ident(b) [45..46)
         |KwElse [47..51)
         |Ident(c) [52..53)
         |InfixLF [53..54)
         |Ident(+) [58..59)
         |Constant.Int(1) [60..61)
         |LF [61..62)
         |KwDef [64..67)
         |Ident(g) [68..69)
         |Equals [70..71)
         |Constant.Int(1) [72..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a) b else c + 1
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tname("a"), tname("b"), tinfix(tname("c"), "+", lit(1))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and `then`, deeper leading select, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) then b else c
         |    .foo
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
         |KwThen [40..44)
         |Ident(b) [45..46)
         |KwElse [47..51)
         |Ident(c) [52..53)
         |Dot [58..59)
         |Ident(foo) [59..62)
         |LF [62..63)
         |KwDef [65..68)
         |Ident(g) [69..70)
         |Equals [71..72)
         |Constant.Int(1) [73..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a) b else c.foo
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tname("a"), tname("b"), tselect(tname("c"), "foo")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and `do`, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a) do b
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
         |KwDo [47..49)
         |Ident(b) [50..51)
         |LF [51..52)
         |KwDef [54..57)
         |Ident(g) [58..59)
         |Equals [60..61)
         |Constant.Int(1) [62..63)
         |Indentation.Outdent [63..63)
         |EOF [64..64)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => while (a) b
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
          tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(Term.While(tname("a"), tname("b"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and `do`, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a) do b
         |    + 1
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
         |KwDo [47..49)
         |Ident(b) [50..51)
         |InfixLF [51..52)
         |Ident(+) [56..57)
         |Constant.Int(1) [58..59)
         |LF [59..60)
         |KwDef [62..65)
         |Ident(g) [66..67)
         |Equals [68..69)
         |Constant.Int(1) [70..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => while (a) b + 1
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(tname("a"), tinfix(tname("b"), "+", lit(1))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and `do`, deeper leading select, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a) do b
         |    .foo
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
         |KwDo [47..49)
         |Ident(b) [50..51)
         |Dot [56..57)
         |Ident(foo) [57..60)
         |LF [60..61)
         |KwDef [63..66)
         |Ident(g) [67..68)
         |Equals [69..70)
         |Constant.Int(1) [71..72)
         |Indentation.Outdent [72..72)
         |EOF [73..73)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => while (a) b.foo
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(tname("a"), tselect(tname("b"), "foo")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and an infix, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a) && b
         |    && c do g(x)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => while (a && b && c) g(x)
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")),
              tapply("g", "x"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: nested on the same line in a match case, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => xs.map: y => ys.map: z => z
         |    case 2 => 3
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      xs.map {
         |        y => ys.map {
         |          z => z
         |        }
         |      }
         |    case 2 =>
         |      3
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
          "x",
          Case(
            lit(1),
            None,
            tapply(
              tselect("xs", "map"),
              blk(tfunc(tparam("y"))(tapply(tselect("ys", "map"), blk(tfunc(tparam("z"))("z"))))),
            ),
          ),
          Case(lit(2), None, lit(3)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, top level, deeper line") {
    val code =
      """|def f = xs.map: x => try a catch case e => b
         |  c
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => try a catch {
         |    case e =>
         |      b
         |      c
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(
          tfunc(tparam("x"))(Term.Try(tname("a"), List(Case(patvar("e"), None, blk("b", "c"))), None)),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, top level, semicolon") {
    val code = "def f = xs.map: x => try a catch case e => b; c"
    val layout =
      """|def f = xs.map {
         |  x => try a catch {
         |    case e =>
         |      b
         |      c
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(
          tfunc(tparam("x"))(Term.Try(tname("a"), List(Case(patvar("e"), None, blk("b", "c"))), None)),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, package in braces, deeper line") {
    val code =
      """|package p {
         |  def f = xs.map: x => try a catch case e => b
         |    c
         |}
         |""".stripMargin
    val layout =
      """|package p
         |def f = xs.map {
         |  x => try a catch {
         |    case e =>
         |      b
         |      c
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Pkg(
      tname("p"),
      List(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(
            Term.Try(tname("a"), List(Case(patvar("e"), None, blk("b", "c"))), None),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: nested on the same line, catch case in the body, top level, deeper line") {
    val code =
      """|def f = xs.map: x => ys.map: y => try a catch case e => b
         |  c
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => ys.map {
         |    y => try a catch {
         |      case e =>
         |        b
         |        c
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("xs", "map"),
        blk(tfunc(tparam("x"))(tapply(
          tselect("ys", "map"),
          blk(tfunc(tparam("y"))(
            Term.Try(tname("a"), List(Case(patvar("e"), None, blk("b", "c"))), None),
          )),
        ))),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for generator in braces, guard on the same line") {
    val code =
      """|object A:
         |  def f = for {
         |    x <- xs.map: y => y if x > 0
         |  } yield x
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- xs.map {
         |    y => y
         |  }; if x > 0) yield x
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
            Enumerator
              .Generator(patvar("x"), tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tname("y"))))),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
          ),
          tname("x"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for generator in indented enumerators, guard on the same line") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs.map: y => y if x > 0
         |    yield x
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- xs.map {
         |    y => y
         |  }; if x > 0) yield x
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
            Enumerator
              .Generator(patvar("x"), tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tname("y"))))),
            Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
          ),
          tname("x"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a body, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) b
         |    + 1
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a) b + 1
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tname("a"), tinfix(tname("b"), "+", lit(1))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a body in braces, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) { b }
         |    + 1
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a) {
         |      b
         |    } + 1
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tname("a"), tinfix(blk(tname("b")), "+", lit(1))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a body in parentheses, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) (b)
         |    + 1
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a) b + 1
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tname("a"), tinfix(tname("b"), "+", lit(1))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and a body, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a) b
         |    + 1
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => while (a) b + 1
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(tname("a"), tinfix(tname("b"), "+", lit(1))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a body, deeper leading select, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) b
         |    .foo
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a) b.foo
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(tname("a"), tselect("b", "foo"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in the body of an if with parentheses, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = if (a) xs.map: x => x
         |    + 1
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (a) xs.map {
         |    x => x + 1
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
          tif(
            tname("a"),
            tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix(tname("x"), "+", lit(1))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in the body of an if with parentheses, if with parentheses and an infix, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = if (z) xs.map: x => if (a) && b
         |    && c then d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (z) xs.map {
         |    x => if (a && b && c) d
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
          tif(
            tname("z"),
            tapply(
              tselect("xs", "map"),
              blk(tfunc(tparam("x"))(
                tif(tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")), tname("d")),
              )),
            ),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in the body of an if with parentheses, if with parentheses and an infix, deeper then, next definition") {
    val code =
      """|object A:
         |  def f = if (z) xs.map: x => if (a) && b
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (z) xs.map {
         |    x => if (a && b) c else d
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
          tif(
            tname("z"),
            tapply(
              tselect("xs", "map"),
              blk(tfunc(tparam("x"))(tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")))),
            ),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in the body of a while with parentheses, if with parentheses and an infix, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = while (z) xs.foreach: x => if (a) && b
         |    && c then g(x)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while (z) xs.foreach {
         |    x => if (a && b && c) g(x)
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
          Term.While(
            tname("z"),
            tapply(
              tselect("xs", "foreach"),
              blk(tfunc(tparam("x"))(tif(
                tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")),
                tapply(tname("g"), tname("x")),
              ))),
            ),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented if condition, then on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.exists: x => x > 0
         |    then a
         |    else b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.exists {
         |    x => x > 0
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
          tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0))))),
          tname("a"),
          tname("b"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented while condition, do on the next line") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      xs.exists: x => x > 0
         |    do g()
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while (xs.exists {
         |    x => x > 0
         |  }) g()
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
          tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0))))),
          tapply(tname("g")),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon lambda in an if condition, body on the next line, `then` on the same line, next statements") {
    val code =
      """|def f =
         |  if xs.exists: x =>
         |      x.ok then c else d
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
         |Ident(xs) [13..15)
         |Dot [15..16)
         |Ident(exists) [16..22)
         |Colon [22..23)
         |Ident(x) [24..25)
         |RightArrow [26..28)
         |Indentation.Indent [28..28)
         |Ident(x) [35..36)
         |Dot [36..37)
         |Ident(ok) [37..39)
         |Indentation.Outdent [37..37)
         |KwThen [40..44)
         |Ident(c) [45..46)
         |KwElse [47..51)
         |Ident(d) [52..53)
         |LF [53..54)
         |Ident(g) [56..57)
         |LF [57..58)
         |Ident(h) [60..61)
         |Indentation.Outdent [61..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (xs.exists {
         |    x => x.ok
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
          tapply(tselect(tname("xs"), "exists"), blk(tfunc(tparam("x"))(tselect(tname("x"), "ok")))),
          tname("c"),
          tname("d"),
        ),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in an if condition, body on the next line, `then` on the same line, no `else`, next statements") {
    val code =
      """|def f =
         |  if xs.exists: x =>
         |      x > 0 then c
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
         |Ident(xs) [13..15)
         |Dot [15..16)
         |Ident(exists) [16..22)
         |Colon [22..23)
         |Ident(x) [24..25)
         |RightArrow [26..28)
         |Indentation.Indent [28..28)
         |Ident(x) [35..36)
         |Ident(>) [37..38)
         |Constant.Int(0) [39..40)
         |Indentation.Outdent [39..39)
         |KwThen [41..45)
         |Ident(c) [46..47)
         |LF [47..48)
         |Ident(g) [50..51)
         |LF [51..52)
         |Ident(h) [54..55)
         |Indentation.Outdent [55..55)
         |EOF [56..56)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (xs.exists {
         |    x => x > 0
         |  }) c
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
          tapply(
            tselect(tname("xs"), "exists"),
            blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0)))),
          ),
          tname("c"),
        ),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon lambda in an if condition, body on the next line with `new`, `then` on the same line, next statements") {
    val code =
      """|def f =
         |  if xs.exists: x =>
         |      new B(x).ok then c else d
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
         |Ident(xs) [13..15)
         |Dot [15..16)
         |Ident(exists) [16..22)
         |Colon [22..23)
         |Ident(x) [24..25)
         |RightArrow [26..28)
         |Indentation.Indent [28..28)
         |KwNew [35..38)
         |Ident(B) [39..40)
         |LeftParen [40..41)
         |Ident(x) [41..42)
         |RightParen [42..43)
         |Dot [43..44)
         |Ident(ok) [44..46)
         |KwThen [47..51)
         |Ident(c) [52..53)
         |KwElse [54..58)
         |Ident(d) [59..60)
         |Indentation.Outdent [60..60)
         |LF [60..61)
         |Ident(g) [63..64)
         |LF [64..65)
         |Ident(h) [67..68)
         |Indentation.Outdent [68..68)
         |EOF [69..69)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:3: error: `identifier` expected but `new` found
         |      new B(x).ok then c else d
         |      ^""".stripMargin
    runTestError[Stat](code, error)
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
         |KwThen [63..67)
         |Ident(d) [68..69)
         |KwElse [70..74)
         |Ident(e) [75..76)
         |Indentation.Outdent [76..76)
         |LF [76..77)
         |Ident(g) [81..82)
         |LF [82..83)
         |Ident(h) [87..88)
         |Indentation.Outdent [88..88)
         |Indentation.Outdent [88..88)
         |EOF [89..89)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `identifier` expected but `if` found
         |        if (a) b; c then d else e
         |        ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: in an if condition with parentheses and an infix, then on the same line") {
    val code =
      """|object A:
         |  def f = if (a) && xs.exists: x => x > 0 then b else c
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (a && xs.exists {
         |    x => x > 0
         |  }) b else c
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
            tname("a"),
            "&&",
            tapply(tselect("xs", "exists"), blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0))))),
          ),
          tname("b"),
          tname("c"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an if condition with parentheses and an infix, deeper leading infix, next definition") {
    val code =
      """|object A:
         |  def f = if (a) && xs.exists: x => x > 0
         |    && c then b else c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (a && xs.exists {
         |    x => x > 0 && c
         |  }) b else c
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
              tname("a"),
              "&&",
              tapply(
                tselect("xs", "exists"),
                blk(tfunc(tparam("x"))(tinfix(tinfix(tname("x"), ">", lit(0)), "&&", tname("c")))),
              ),
            ),
            tname("b"),
            tname("c"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, two deeper leading infixes, then on the second, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && b
         |      && c
         |      && d then e else g
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b && c && d) e else g
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(
              tinfix(tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")), "&&", tname("d")),
              tname("e"),
              tname("g"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, leading infix two levels deeper with then and else, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && b
         |      && c then d else e
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b && c) d else e
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")),
              tname("d"),
              tname("e"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and an infix, deeper leading infix, do on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a) && b
         |      && c
         |      do d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => while (a && b && c) d
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(
              Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")), tname("d")),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, deeper leading select, leading infix with then, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && b
         |      .foo
         |      && c then d else e
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b.foo && c) d else e
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(
              tinfix(tinfix(tname("a"), "&&", tselect("b", "foo")), "&&", tname("c")),
              tname("d"),
              tname("e"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, deeper then, shallower leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && b
         |      then c else d
         |    + 1
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b) c else d + 1
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tinfix(tname("d"), "+", lit(1))),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, deeper leading infix, then on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && b
         |      && c
         |      then d else e
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b && c) d else e
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")),
              tname("d"),
              tname("e"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, deeper argument in parentheses, then on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && ys.exists
         |        (y => y > 0)
         |      then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && ys.exists(y => y > 0)) c else d
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(
              tinfix(
                tname("a"),
                "&&",
                tapply(tselect("ys", "exists"), tfunc(tparam("y"))(tinfix(tname("y"), ">", lit(0)))),
              ),
              tname("c"),
              tname("d"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test(
    "SIP-75 single-line lambda: if with parentheses and an infix, blank line, then, next definition",
  ) {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && b
         |
         |      then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b) c else d
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, deeper leading select with then, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && b
         |      .foo then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b.foo) c else d
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              tif(tinfix(tname("a"), "&&", tselect("b", "foo")), tname("c"), tname("d")),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses at the line end, deeper leading infix with then, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a)
         |      && b then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b) c else d
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an ascription, then on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a): Boolean
         |      then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a: Boolean) c else d
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              tif(Term.Ascribe(tname("a"), pname("Boolean")), tname("c"), tname("d")),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an ascription, deeper leading infix, then on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a): Boolean
         |      && b
         |      then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a: Boolean && b) c else d
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(
              Term.Ascribe(tname("a"), pinfix(pname("Boolean"), "&&", pname("b"))),
              tname("c"),
              tname("d"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and an ascription, do on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a): Boolean
         |      do c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => while (a: Boolean) c
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(Term.Ascribe(tname("a"), pname("Boolean")), tname("c")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and an ascription, deeper leading infix, do on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => while (a): Boolean
         |      && b
         |      do c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => while (a: Boolean && b) c
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(
              Term.Ascribe(tname("a"), pinfix(pname("Boolean"), "&&", pname("b"))),
              tname("c"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and a body, then on the next line, next definition") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.foreach: x => if (a) b
         |    then c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.foreach {
         |    x => if (a) b
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
          tif(
            tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tif(tname("a"), tname("b"))))),
            tname("c"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and a body, next line with then") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.foreach: x => if (a) b
         |      c then d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if ({
         |    xs.foreach {
         |      x => if (a) b
         |    }
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
        tif(
          blk(
            tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tif(tname("a"), tname("b"))))),
            tname("c"),
          ),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and new in the body, next line with then") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.foreach: x => if (a) g(new B)
         |      c then d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if ({
         |    xs.foreach {
         |      x => if (a) g(new B)
         |    }
         |    c
         |  }) d
         |}
         |""".stripMargin
    val body = tif(tname("a"), tapply(tname("g"), Term.New(init(pname("B")))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          blk(tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(body))), tname("c")),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and an if in the body, next line with then") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.foreach: x => if (a) g(if b then 1 else 2)
         |      c then d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if ({
         |    xs.foreach {
         |      x => if (a) g(if (b) 1 else 2)
         |    }
         |    c
         |  }) d
         |}
         |""".stripMargin
    val body = tif(tname("a"), tapply(tname("g"), tif(tname("b"), lit(1), lit(2))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          blk(tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(body))), tname("c")),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and new in the body, next statement, then on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.foreach: x => if (a) g(new B)
         |      c
         |    then d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if ({
         |    xs.foreach {
         |      x => if (a) g(new B)
         |    }
         |    c
         |  }) d
         |}
         |""".stripMargin
    val body = tif(tname("a"), tapply(tname("g"), Term.New(init(pname("B")))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          blk(tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(body))), tname("c")),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented while condition, while with parentheses and new in the body, next statement, do on the next line") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      xs.foreach: x => while (a) g(new B)
         |      c
         |    do d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while ({
         |    xs.foreach {
         |      x => while (a) g(new B)
         |    }
         |    c
         |  }) d
         |}
         |""".stripMargin
    val body = Term.While(tname("a"), tapply(tname("g"), Term.New(init(pname("B")))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(
          blk(tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(body))), tname("c")),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and new in the body, then on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.foreach: x => if (a) g(new B)
         |    then d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.foreach {
         |    x => if (a) g(new B)
         |  }) d
         |}
         |""".stripMargin
    val body = tif(tname("a"), tapply(tname("g"), Term.New(init(pname("B")))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(body))), tname("d")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses, select and match, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).b match { case _ => true }
         |    then e else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.b match {
         |      case _ => true
         |    }) e else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tmatch(tselect("a", "b"), Case(patwildcard, None, lit(true)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("e"), tname("g"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and match, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) match { case _ => true }
         |    then e else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a match {
         |      case _ => true
         |    }) e else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tmatch(tname("a"), Case(patwildcard, None, lit(true)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("e"), tname("g"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses, an infix and an annotation ascription, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) && b: @unchecked
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a && b: @unchecked) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = Term
      .Annotate(tinfix(tname("a"), "&&", tname("b")), List(Mod.Annot(init(pname("unchecked")))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses, an infix and an annotation ascription, do on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.foreach: x => while (a) && b: @unchecked
         |    do g(x)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foreach {
         |    x => while (a && b: @unchecked) g(x)
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = Term
      .Annotate(tinfix(tname("a"), "&&", tname("b")), List(Mod.Annot(init(pname("unchecked")))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(cond, tapply(tname("g"), tname("x"))))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix with inline as a name, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) && inline(b)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a && `inline`(b)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tinfix(tname("a"), "&&", tapply(tname("inline"), tname("b")))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @unchecked
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @unchecked) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = Term.Annotate(tname("a"), List(Mod.Annot(init(pname("unchecked")))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a singleton type ascription, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): y.type
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: y.type) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = Term.Ascribe(tname("a"), Type.Singleton(tname("y")))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an unparenthesized generator, do on the next line, next definition") {
    val code =
      """|object O:
         |  def f = for y <- xs.filter: x => x > 0
         |    do g(y)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = for (y <- xs.filter {
         |    x => x > 0
         |  }) g(y)
         |  def h = 1
         |}
         |""".stripMargin
    val filter =
      tapply(tselect("xs", "filter"), blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0)))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          Term.For(List(Enumerator.Generator(patvar("y"), filter)), tapply(tname("g"), tname("y"))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented generator, next generator, next definition") {
    val code =
      """|object O:
         |  def f = for
         |    y <- xs.filter: x => x > 0
         |    z <- zs
         |  yield y
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = for (y <- xs.filter {
         |    x => x > 0
         |  }; z <- zs) yield y
         |  def h = 1
         |}
         |""".stripMargin
    val filter =
      tapply(tselect("xs", "filter"), blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", lit(0)))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          Term.ForYield(
            List(
              Enumerator.Generator(patvar("y"), filter),
              Enumerator.Generator(patvar("z"), tname("zs")),
            ),
            tname("y"),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: case with a typed pattern and a lambda body in braces, next definition") {
    val code =
      """|object O:
         |  def f = xs.map { case x: A => B => x }
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    case x: A =>
         |      B => x
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val pf = Term.PartialFunction(List(
      Case(Pat.Typed(patvar("x"), pname("A")), None, tfunc(tparam("B"))(tname("x"))),
    ))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), pf)),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and an XML literal in the body, next line with then") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.foreach: x => if (a) g(<b/>)
         |      c then d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if ({
         |    xs.foreach {
         |      x => if (a) g(<b/>)
         |    }
         |    c
         |  }) d
         |}
         |""".stripMargin
    val body = tif(tname("a"), tapply(tname("g"), Term.Xml(List(str("<b/>")), Nil)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          blk(tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(body))), tname("c")),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented generator, while with parentheses and new in the body, next generator, do") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      y <- xs.map: x => while (a) g(new B)
         |      z <- zs
         |    do h(y)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (y <- xs.map {
         |    x => while (a) g(new B)
         |  }; z <- zs) h(y)
         |}
         |""".stripMargin
    val body = Term.While(tname("a"), tapply(tname("g"), Term.New(init(pname("B")))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.For(
          List(
            Enumerator
              .Generator(patvar("y"), tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
            Enumerator.Generator(patvar("z"), tname("zs")),
          ),
          tapply(tname("h"), tname("y")),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix with extension as a name, then on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && extension(b)
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && `extension`(b)) c else d
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val cond = tinfix(tname("a"), "&&", tapply(tname("extension"), tname("b")))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription with an if argument, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann(if b then 1 else 2)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann(if (b) 1 else 2)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = Term.Annotate(
      tname("a"),
      List(Mod.Annot(init(pname("ann"), Term.ArgClause(List(tif(tname("b"), lit(1), lit(2))))))),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription with an if argument, its then on the next line, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann(if b
         |    then 1 else 2)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann(if (b) 1 else 2)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = Term.Annotate(
      tname("a"),
      List(Mod.Annot(init(pname("ann"), Term.ArgClause(List(tif(tname("b"), lit(1), lit(2))))))),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select applied to an if, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).g(if b then 1 else 2)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.g(if (b) 1 else 2)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tapply(tselect("a", "g"), tif(tname("b"), lit(1), lit(2)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select applied to an if, its then on the next line, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).g(if b
         |    then 1 else 2)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.g(if (b) 1 else 2)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tapply(tselect("a", "g"), tif(tname("b"), lit(1), lit(2)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and match with an if in a case body, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) match { case _ => g(if b then 1 else 2) }
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a match {
         |      case _ =>
         |        g(if (b) 1 else 2)
         |    }) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond =
      tmatch(tname("a"), Case(patwildcard, None, tapply(tname("g"), tif(tname("b"), lit(1), lit(2)))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and match with an if in a case body, its then on the next line, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) match { case _ => g(if b
         |    then 1 else 2) }
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a match {
         |      case _ =>
         |        g(if (b) 1 else 2)
         |    }) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond =
      tmatch(tname("a"), Case(patwildcard, None, tapply(tname("g"), tif(tname("b"), lit(1), lit(2)))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses, a select applied to an if and match, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).g(if b then 1 else 2) match { case _ => true }
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.g(if (b) 1 else 2) match {
         |      case _ => true
         |    }) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tmatch(
      tapply(tselect("a", "g"), tif(tname("b"), lit(1), lit(2))),
      Case(patwildcard, None, lit(true)),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses, a select applied to an if and match, its then on the next line, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).g(if b
         |    then 1 else 2) match { case _ => true }
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.g(if (b) 1 else 2) match {
         |      case _ => true
         |    }) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tmatch(
      tapply(tselect("a", "g"), tif(tname("b"), lit(1), lit(2))),
      Case(patwildcard, None, lit(true)),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and an annotation ascription with a while argument, do on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => while (a): @ann(while b do g)
         |    do h(x)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => while (a: @ann(while (b) g)) h(x)
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = Term.Annotate(
      tname("a"),
      List(Mod.Annot(init(pname("ann"), Term.ArgClause(List(Term.While(tname("b"), tname("g"))))))),
    )
    val body = Term.While(cond, tapply(tname("h"), tname("x")))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and an annotation ascription with a for argument, do on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => while (a): @ann(for y <- ys do g(y))
         |    do h(x)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => while (a: @ann(for (y <- ys) g(y))) h(x)
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val arg = Term
      .For(List(Enumerator.Generator(patvar("y"), tname("ys"))), tapply(tname("g"), tname("y")))
    val cond = Term.Annotate(tname("a"), List(Mod.Annot(init(pname("ann"), Term.ArgClause(List(arg))))))
    val body = Term.While(cond, tapply(tname("h"), tname("x")))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotated type ascription with an if argument, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): T @ann(if b then 1 else 2)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: T @ann(if (b) 1 else 2)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val annot = Mod.Annot(init(pname("ann"), Term.ArgClause(List(tif(tname("b"), lit(1), lit(2))))))
    val cond = Term.Ascribe(tname("a"), Type.Annotate(pname("T"), List(annot)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription with a lambda argument, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann(ys.map(y => if y then 1 else 2))
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann(ys.map(y => if (y) 1 else 2))) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val arg = tapply(tselect("ys", "map"), tfunc(tparam("y"))(tif(tname("y"), lit(1), lit(2))))
    val cond = Term.Annotate(tname("a"), List(Mod.Annot(init(pname("ann"), Term.ArgClause(List(arg))))))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select applied to an if, then on the next line, next definition, other name") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).contains(if b then 1 else 2)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.contains(if (b) 1 else 2)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tapply(tselect("a", "contains"), tif(tname("b"), lit(1), lit(2)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an indented match with an if in a case body, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) match
         |      case true => if b then c else d
         |      case _ => e
         |    then f else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a match {
         |      case true =>
         |        if (b) c else d
         |      case _ =>
         |        e
         |    }) f else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tmatch(
      tname("a"),
      Case(lit(true), None, tif(tname("b"), tname("c"), tname("d"))),
      Case(patwildcard, None, tname("e")),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("f"), tname("g"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and match in braces with an if in a case body, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) match { case true => if b then c else d; case _ => e }
         |    then f else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a match {
         |      case true =>
         |        if (b) c else d
         |      case _ =>
         |        e
         |    }) f else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tmatch(
      tname("a"),
      Case(lit(true), None, tif(tname("b"), tname("c"), tname("d"))),
      Case(patwildcard, None, tname("e")),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("f"), tname("g"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and an infix, then on the next line, then on the next line") {
    val code =
      """|object O:
         |  def f =
         |    if
         |      xs.exists: x => if (a) && b
         |        then c else d
         |    then e else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = if (xs.exists {
         |    x => if (a && b) c else d
         |  }) e else g
         |  def h = 1
         |}
         |""".stripMargin
    val lambda = tfunc(tparam("x"))(tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(tapply(tselect("xs", "exists"), blk(lambda)), tname("e"), tname("g")),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription with an if argument, then, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann(if b then 1 else 2) then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann(if (b) 1 else 2)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = Term.Annotate(
      tname("a"),
      List(Mod.Annot(init(pname("ann"), Term.ArgClause(List(tif(tname("b"), lit(1), lit(2))))))),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an ascription with two annotations, the second with an if argument, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann(1) @bnn(if b then 1 else 2)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann(1) @bnn(if (b) 1 else 2)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = Term.Annotate(
      tname("a"),
      List(
        Mod.Annot(init(pname("ann"), Term.ArgClause(List(lit(1))))),
        Mod.Annot(init(pname("bnn"), Term.ArgClause(List(tif(tname("b"), lit(1), lit(2)))))),
      ),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("c"), tname("d"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription without arguments, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body =
      tif(Term.Annotate(tname("a"), List(Mod.Annot(init(pname("ann"))))), tname("c"), tname("d"))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription with a type argument and an if argument, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann[Int](if b then 1 else 2)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann[Int](if (b) 1 else 2)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Annotate(
        tname("a"),
        List(Mod.Annot(
          init(papply(pname("ann"), pname("Int")), Term.ArgClause(List(tif(tname("b"), lit(1), lit(2))))),
        )),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription with an interpolation argument, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann(s"${if b then 1 else 2}")
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann(s"${if (b) 1 else 2}")) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Annotate(
        tname("a"),
        List(Mod.Annot(init(
          pname("ann"),
          Term.ArgClause(List(Term.Interpolate(
            tname("s"),
            List(lit(""), lit("")),
            List(blk(tif(tname("b"), lit(1), lit(2)))),
          ))),
        ))),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription with an XML argument, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann(<b>{if b then 1 else 2}</b>)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann(<b>{
         |      if (b) 1 else 2
         |    }</b>)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Annotate(
        tname("a"),
        List(Mod.Annot(init(
          pname("ann"),
          Term.ArgClause(List(
            Term.Xml(List(lit("<b>"), lit("</b>")), List(blk(tif(tname("b"), lit(1), lit(2))))),
          )),
        ))),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a refinement type ascription, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): T { def g: Int }
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: T {
         |      def g: Int
         |    }) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Ascribe(
        tname("a"),
        Type.Refine(Some(pname("T")), Stat.Block(List(Decl.Def(Nil, tname("g"), Nil, pname("Int"))))),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a literal type ascription, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): true
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: true) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(Term.Ascribe(tname("a"), lit(true)), tname("c"), tname("d"))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an ascription of a type applied to a type lambda, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): F[[X] =>> G[X]]
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: F[[X] =>> G[X]]) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Ascribe(
        tname("a"),
        papply(pname("F"), Type.Lambda(ppc(pparam("X")), papply(pname("G"), pname("X")))),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a context function type ascription, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): (Int ?=> Boolean)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: (Int ?=> Boolean)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Ascribe(
        tname("a"),
        Type.ContextFunction(Type.FuncParamClause(List(pname("Int"))), pname("Boolean")),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a match type ascription in parentheses, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): (T match { case Int => Boolean })
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: (T match {
         |      case Int => Boolean
         |    })) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Ascribe(
        tname("a"),
        Type.Match(pname("T"), Type.CasesBlock(List(TypeCase(pname("Int"), pname("Boolean"))))),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a colon argument with an indented lambda, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (f): y =>
         |      if y then true else false
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (f {
         |      y => if (y) true else false
         |    }) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      tapply(tname("f"), blk(tfunc(tparam("y"))(tif(tname("y"), lit(true), lit(false))))),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a colon argument with an indented if, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (f):
         |      if b then true else false
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (f {
         |      if (b) true else false
         |    }) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body =
      tif(tapply(tname("f"), blk(tif(tname("b"), lit(true), lit(false)))), tname("c"), tname("d"))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and an ascription in a map lambda, do on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => while (a): Boolean
         |    do h(x)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => while (a: Boolean) h(x)
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = Term.While(Term.Ascribe(tname("a"), pname("Boolean")), tapply(tname("h"), tname("x")))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription with named arguments over three lines, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann(
         |      x = if b then 1 else 2,
         |      y = 3)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann(x = if (b) 1 else 2, y = 3)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Annotate(
        tname("a"),
        List(Mod.Annot(init(
          pname("ann"),
          Term.ArgClause(List(
            Term.Assign(tname("x"), tif(tname("b"), lit(1), lit(2))),
            Term.Assign(tname("y"), lit(3)),
          )),
        ))),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotation ascription with a block argument, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): @ann({ if b then 1 else 2 })
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: @ann({
         |      if (b) 1 else 2
         |    })) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Annotate(
        tname("a"),
        List(Mod.Annot(init(pname("ann"), Term.ArgClause(List(blk(tif(tname("b"), lit(1), lit(2)))))))),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a dependent function type ascription, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): ((y: Int) => y.type)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: ((y: Int) => y.type)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Ascribe(
        tname("a"),
        Type.Function(
          Type.FuncParamClause(List(Type.TypedParam(pname("y"), pname("Int"), Nil))),
          Type.Singleton(tname("y")),
        ),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an annotated type ascription, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a): T @ann
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a: T @ann) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      Term.Ascribe(tname("a"), Type.Annotate(pname("T"), List(Mod.Annot(init(pname("ann")))))),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and a colon argument with an indented for, do on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => while (f):
         |      for y <- ys do g(y)
         |    do h(x)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => while (f {
         |      for (y <- ys) g(y)
         |    }) h(x)
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = Term.While(
      tapply(
        tname("f"),
        blk(
          Term
            .For(List(Enumerator.Generator(patvar("y"), tname("ys"))), tapply(tname("g"), tname("y"))),
        ),
      ),
      tapply(tname("h"), tname("x")),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a colon argument with an indented identifier, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (f):
         |      b
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (f {
         |      b
         |    }) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(tapply(tname("f"), blk(tname("b"))), tname("c"), tname("d"))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select with a colon argument with an indented lambda, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).exists: y =>
         |      if y then true else false
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.exists {
         |      y => if (y) true else false
         |    }) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      tapply(tselect("a", "exists"), blk(tfunc(tparam("y"))(tif(tname("y"), lit(true), lit(false))))),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select applied to a lambda with an annotated if condition, its then on the next line, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).exists(y => if (b): @ann(if y then 1 else 2)
         |      then true else false)
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.exists(y => if (b: @ann(if (y) 1 else 2)) true else false)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      tapply(
        tselect("a", "exists"),
        tfunc(tparam("y"))(tif(
          Term.Annotate(
            tname("b"),
            List(Mod.Annot(init(pname("ann"), Term.ArgClause(List(tif(tname("y"), lit(1), lit(2))))))),
          ),
          lit(true),
          lit(false),
        )),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a colon argument with an indented while, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (f):
         |      while b do g
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (f {
         |      while (b) g
         |    }) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body =
      tif(tapply(tname("f"), blk(Term.While(tname("b"), tname("g")))), tname("c"), tname("d"))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a colon argument with an indented lambda, apply and then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (f): y =>
         |      if y then true else false
         |    .apply(z) then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (f {
         |      y => if (y) true else false
         |    }.apply(z)) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      tapply(
        tselect(
          tapply(tname("f"), blk(tfunc(tparam("y"))(tif(tname("y"), lit(true), lit(false))))),
          tname("apply"),
        ),
        tname("z"),
      ),
      tname("c"),
      tname("d"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix with a backquoted then, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (f) `then` b
         |    then c else d
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (f `then` b) c else d
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(tinfix(tname("f"), tname("then"), tname("b")), tname("c"), tname("d"))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select with a colon argument with an indented if, leading select, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).g:
         |        if b then c else d
         |      .h
         |    then f else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.g {
         |      if (b) c else d
         |    }.h) f else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      tselect(tapply(tselect("a", "g"), blk(tif(tname("b"), tname("c"), tname("d")))), tname("h")),
      tname("f"),
      tname("g"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select applied to an if over two lines, leading select, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).g(
         |        if b then c else d)
         |      .h
         |    then f else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.g(if (b) c else d).h) f else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      tselect(tapply(tselect("a", "g"), tif(tname("b"), tname("c"), tname("d"))), tname("h")),
      tname("f"),
      tname("g"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select with a colon argument with an indented if, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).g:
         |        if b then c else d
         |    then f else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.g {
         |      if (b) c else d
         |    }) f else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      tapply(tselect("a", "g"), blk(tif(tname("b"), tname("c"), tname("d")))),
      tname("f"),
      tname("g"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a colon argument with an indented if, leading select, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (f):
         |        if b then c else d
         |      .h
         |    then f else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (f {
         |      if (b) c else d
         |    }.h) f else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      tselect(tapply(tname("f"), blk(tif(tname("b"), tname("c"), tname("d")))), tname("h")),
      tname("f"),
      tname("g"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select, leading select applied to an if, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).g(b)
         |      .h(if b then c else d)
         |    then f else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.g(b).h(if (b) c else d)) f else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      tapply(
        tselect(tapply(tselect("a", "g"), tname("b")), tname("h")),
        tif(tname("b"), tname("c"), tname("d")),
      ),
      tname("f"),
      tname("g"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select with a colon argument with an indented if, leading infix, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).g:
         |        if b then c else d
         |      && e
         |    then f else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.g {
         |      if (b) c else d
         |    } && e) f else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = tif(
      tinfix(
        tapply(tselect("a", "g"), blk(tif(tname("b"), tname("c"), tname("d")))),
        tname("&&"),
        tname("e"),
      ),
      tname("f"),
      tname("g"),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a select with a colon argument with an indented identifier, leading select, then on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a).g:
         |        b
         |      .h
         |    then f else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a.g {
         |      b
         |    }.h) f else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body =
      tif(tselect(tapply(tselect("a", "g"), blk(tname("b"))), tname("h")), tname("f"), tname("g"))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and a select with a colon argument with an indented for, leading select, do on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => while (a).g:
         |        for y <- ys do k(y)
         |      .h
         |    do m(x)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => while (a.g {
         |      for (y <- ys) k(y)
         |    }.h) m(x)
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val body = Term.While(
      tselect(
        tapply(
          tselect("a", "g"),
          blk(Term.For(
            List(Enumerator.Generator(patvar("y"), tname("ys"))),
            tapply(tname("k"), tname("y")),
          )),
        ),
        tname("h"),
      ),
      tapply(tname("m"), tname("x")),
    )
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn
          .Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(body)))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: ascription body, deeper with, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => x: A
         |    with B
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x: A with B
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.Ascribe(tname("x"), Type.With(pname("A"), pname("B"))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: return at the line end, deeper line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => return
         |    y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => return y
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.Return(tname("y"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: return at the line end, indented package, deeper line") {
    val code =
      """|package p:
         |  def f = xs.map: x => return
         |    y
         |""".stripMargin
    val layout =
      """|package p
         |def f = xs.map {
         |  x => return y
         |}
         |""".stripMargin
    val tree = Source(List(Pkg(
      tname("p"),
      List(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.Return(tname("y"))))),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, object, deeper line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => try a catch case e => b
         |    c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => try a catch {
         |      case e =>
         |        b
         |        c
         |    }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term.Try(tname("a"), List(Case(patvar("e"), None, blk(tname("b"), tname("c")))), None),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, class, deeper line, next definition") {
    val code =
      """|class C:
         |  def f = xs.map: x => try a catch case e => b
         |    c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|class C {
         |  def f = xs.map {
         |    x => try a catch {
         |      case e =>
         |        b
         |        c
         |    }
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("C"),
      Nil,
      ctor,
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term.Try(tname("a"), List(Case(patvar("e"), None, blk(tname("b"), tname("c")))), None),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, package in braces, semicolon") {
    val code =
      """|package p {
         |  def f = xs.map: x => try a catch case e => b; c
         |}
         |""".stripMargin
    val layout =
      """|package p
         |def f = xs.map {
         |  x => try a catch {
         |    case e =>
         |      b
         |      c
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Pkg(
      tname("p"),
      List(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(
            Term.Try(tname("a"), List(Case(patvar("e"), None, blk(tname("b"), tname("c")))), None),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in a catch case body, deeper line, next definition") {
    val code =
      """|object A:
         |  def f = try a catch case e => xs.map: x => x
         |    c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case e =>
         |      xs.map {
         |        x => x
         |      }
         |      c
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
          Term.Try(
            tname("a"),
            List(Case(
              patvar("e"),
              None,
              blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), tname("c")),
            )),
            None,
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: identifier body, top level, deeper line") {
    val code =
      """|def f = xs.map: x => f
         |  g
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of definition `identifier`
         |  g
         |  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: if then else body, top level, deeper line") {
    val code =
      """|def f = xs.map: x => if x then a else b
         |  c
         |""".stripMargin
    val error =
      """|<input>:2: error: illegal start of definition `identifier`
         |  c
         |  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: top level, deeper statement, next definition") {
    val code =
      """|val xs = List(1)
         |val r = xs.map: x => x + 1
         |  println(r)
         |val s = 1
         |""".stripMargin
    val error =
      """|<input>:3: error: illegal start of definition `identifier`
         |  println(r)
         |  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: top level of a script, deeper statement, next definition") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowToplevelTerms(true)
    val code =
      """|val xs = List(1)
         |val r = xs.map: x => x + 1
         |  println(r)
         |val s = 1
         |""".stripMargin
    val layout =
      """|val xs = List(1)
         |val r = xs.map {
         |  x => x + 1
         |}
         |println(r)
         |val s = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Val(Nil, List(patvar("xs")), None, tapply(tname("List"), lit(1))),
      Defn.Val(
        Nil,
        List(patvar("r")),
        None,
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix(tname("x"), "+", lit(1))))),
      ),
      tapply(tname("println"), tname("r")),
      Defn.Val(Nil, List(patvar("s")), None, lit(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test(
    "SIP-75 relaxed lambda: placeholder self type in a class in braces, statement on the same line",
  ) {
    val code = "class C { _: B => println(1) }"
    val tree = Source(List(Defn.Class(
      Nil,
      pname("C"),
      Nil,
      ctor,
      tpl(self(Name.Placeholder(), pname("B")), tapply(tname("println"), lit(1))),
    )))
    runTestAssert[Source](code)(tree)
  }

  test("SIP-75 relaxed lambda: this self type in a class in braces, statement on the same line") {
    val code = "class C { this: B => println(1) }"
    val tree = Source(List(Defn.Class(
      Nil,
      pname("C"),
      Nil,
      ctor,
      tpl(self(Name.This(), pname("B")), tapply(tname("println"), lit(1))),
    )))
    runTestAssert[Source](code)(tree)
  }

  test("SIP-75 single-line lambda: block, typed param name without parens") {
    val code =
      """|object A:
         |  def f = { x: Int => x }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    x {
         |      Int => x
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(Nil, tname("f"), Nil, None, blk(tapply("x", blk(tfunc(tparam("Int"))("x")))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: braces arg, typed param name without parens, infix body") {
    val code =
      """|object A:
         |  def f = xs.map { x: Int => x + 1 }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x {
         |      Int => x + 1
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          blk(tapply("x", blk(tfunc(tparam("Int"))(tinfix(tname("x"), "+", lit(1)))))),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in the body of an if with parentheses, else on the same line") {
    val code =
      """|object A:
         |  def f = if (c) xs.map: x => x else 2
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (c) xs.map {
         |    x => x
         |  } else 2
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tname("c"), tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), lit(2)),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses, deeper else, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) b
         |    else c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a) b else c
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(tname("a"), tname("b"), tname("c"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new body, deeper with, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => new A
         |    with B
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => new A with B
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Object(
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
            blk(tfunc(tparam("x"))(Term.NewAnonymous(tplNoBody(init("A"), init("B"))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: catch case in the body, leading select at the def indent") {
    val code =
      """|object A:
         |  def f = xs.map: x => try f(x) catch case _ => 0
         |  .size
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => try f(x) catch {
         |      case _ => 0
         |    }
         |  }.size
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tselect(
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term.Try(tapply("f", "x"), List(Case(patwildcard, None, lit(0))), None),
            )),
          ),
          "size",
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, leading infix two levels deeper with then and else, shallower leading infix, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && b
         |      && c then d else e
         |    + 1
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b && c) d else e + 1
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Object(
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
              tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")),
              tname("d"),
              tinfix(tname("e"), "+", lit(1)),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in the body of an if with parentheses, if with parentheses and an infix, leading infix two levels deeper with then, next definition") {
    val code =
      """|object A:
         |  def f = if (z) xs.map: x => if (a) && b
         |      && c then d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (z) xs.map {
         |    x => if (a && b && c) d
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tname("z"),
            tapply(
              tselect("xs", "map"),
              blk(tfunc(tparam("x"))(
                tif(tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")), tname("d")),
              )),
            ),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test(
    "SIP-75 single-line lambda: catch case in the body, deeper leading select, next definition",
  ) {
    val code =
      """|object A:
         |  def f = xs.map: x => try a catch case e => b
         |    .foo
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => try a catch {
         |      case e =>
         |        b.foo
         |    }
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Defn.Object(
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
            blk(tfunc(tparam("x"))(
              Term.Try(tname("a"), List(Case(patvar("e"), None, tselect("b", "foo"))), None),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match body, deeper case, leading select at the def indent") {
    val code =
      """|object A:
         |  def f = xs.map: x => x match
         |    case 1 => 2
         |  .foo
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x match {
         |      case 1 => 2
         |    }
         |  }.foo
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tselect(
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tmatch(tname("x"), Case(lit(1), None, lit(2))))),
          ),
          "foo",
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for generator, deeper yield on the next line") {
    val code =
      """|object A:
         |  def f = for x <- xs.map: y => y
         |    yield x
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- xs.map {
         |    y => y
         |  }) yield x
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.ForYield(
          List(
            Enumerator
              .Generator(patvar("x"), tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tname("y"))))),
          ),
          tname("x"),
        ),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: two params in parens, leading select on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: (x, y) => x + y
         |    .sum
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    (x, y) => x + y.sum
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"), tparam("y"))(tinfix(tname("x"), "+", tselect("y", "sum")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: blank line and a comment before the next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => x
         |
         |  // comment
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x
         |  }
         |
         |  // comment
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        ),
        Defn.Def.newBuilder(Nil, tname("g"), Nil, None, lit(1))
          .begComment(detachedComments("// comment")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an object, semicolon and statement after body") {
    val code =
      """|object A:
         |  def f = xs.map: x => x; println(2)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x
         |  }
         |  println(2)
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        ),
        tapply(tname("println"), lit(2)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: ??? body, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => ???
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => ???
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("???")))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: ??? body, deeper statement, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => ???
         |    y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => ???
         |  }
         |  y
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("???")))),
        ),
        tname("y"),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: backquoted identifier body, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => `type`
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => `type`
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("type")))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: symbolic select body, next definition") {
    val code =
      """|object A:
         |  def f = cmds.foreach: c => c.!
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = cmds.foreach {
         |    c => c.!
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
          tapply(tselect("cmds", "foreach"), blk(tfunc(tparam("c"))(tselect("c", "!")))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for generator, if body, yield on the next line") {
    val code =
      """|object A:
         |  val v = for x <- xs.map: y => if y then 1 else 2
         |  yield x
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  val v = for (x <- xs.map {
         |    y => if (y) 1 else 2
         |  }) yield x
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(
          Nil,
          List(patvar("v")),
          None,
          Term.ForYield(
            List(Enumerator.Generator(
              patvar("x"),
              tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tif(tname("y"), lit(1), lit(2))))),
            )),
            tname("x"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 colon: type name param in parens, after try and after then") {
    val code =
      """|object A:
         |  val g = (try f: Int => Int finally ())
         |  val h = (if c then f: Int => Int else k)
         |""".stripMargin
    val layout =
      """|object A {
         |  val g = try f {
         |    Int => Int
         |  } finally ()
         |  val h = if (c) f {
         |    Int => Int
         |  } else k
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(
          Nil,
          List(patvar("g")),
          None,
          Term.Try(tapply(tname("f"), blk(tfunc(tparam("Int"))(tname("Int")))), Nil, Some(lit())),
        ),
        Defn.Val(
          Nil,
          List(patvar("h")),
          None,
          tif(tname("c"), tapply(tname("f"), blk(tfunc(tparam("Int"))(tname("Int")))), tname("k")),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for generator, guard and yield on the same line") {
    val code =
      """|object A:
         |  val v = for x <- xs.map: y => y if x > 0 yield x
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  val v = for (x <- xs.map {
         |    y => y
         |  }; if x > 0) yield x
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(
          Nil,
          List(patvar("v")),
          None,
          Term.ForYield(
            List(
              Enumerator.Generator(
                patvar("x"),
                tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tname("y")))),
              ),
              Enumerator.Guard(tinfix(tname("x"), ">", lit(0))),
            ),
            tname("x"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for generator in indented enumerators, if body") {
    val code =
      """|object A:
         |  val v =
         |    for
         |      x <- xs.map: y => if y then 1 else 2
         |    yield x
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  val v = for (x <- xs.map {
         |    y => if (y) 1 else 2
         |  }) yield x
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(
          Nil,
          List(patvar("v")),
          None,
          Term.ForYield(
            List(Enumerator.Generator(
              patvar("x"),
              tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tif(tname("y"), lit(1), lit(2))))),
            )),
            tname("x"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case with guard, next definition") {
    val code =
      """|object A:
         |  def f = xs.collect: case x if x > 0 => x
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.collect: case x if x > 0 => x
         |                      ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: case with guard, deeper leading select, next definition") {
    val code =
      """|object A:
         |  def f = xs.collect: case x if x > 0 => x
         |    .toList
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.collect: case x if x > 0 => x
         |                      ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: case with guard in parentheses, next definition") {
    val code =
      """|object A:
         |  def f = xs.collect: case x if (x > 0) => x
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.collect: case x if (x > 0) => x
         |                      ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: case with guard, second case at the definition indent") {
    val code =
      """|object A:
         |  def f = xs.collect: case x if x > 0 => x
         |  case y => y
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.collect: case x if x > 0 => x
         |                      ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: case with guard in a match case body, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => xs.collect: case y if y > 0 => y
         |    case 2 => Nil
         |""".stripMargin
    val error =
      """|<input>:3: error: `identifier` expected but `case` found
         |    case 1 => xs.collect: case y if y > 0 => y
         |                          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 relaxed lambda: case with a typed binder pattern, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: case x @ y: Int => x
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.map: case x @ y: Int => x
         |                  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: in an indented if condition, if with parentheses and an infix, deeper leading infix") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      xs.foreach: x => if (a) && b
         |        && c
         |        then d else e
         |      g
         |    then h
         |  def k = 1
         |""".stripMargin
    val error =
      """|<input>:8: error: `outdent` expected but `then` found
         |    then h
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: in an indented while condition, while with parentheses and an infix, deeper leading infix") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      xs.foreach: x => while (a) && b
         |        && c
         |        do d
         |      g
         |    do h
         |  def k = 1
         |""".stripMargin
    val error =
      """|<input>:8: error: `while` expected but `outdent` found
         |    do h
         |        ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: match body with cases on the same line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => x match case 1 => 2 case _ => 3
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x match {
         |      case 1 => 2
         |      case _ => 3
         |    }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              tmatch(tname("x"), Case(lit(1), None, lit(2)), Case(Pat.Wildcard(), None, lit(3))),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case, guard on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: case x
         |    if x > 0 => x
         |  def g = 1
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `case` found
         |  def f = xs.map: case x
         |                  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 single-line lambda: if with parentheses and a trailing infix, condition on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) &&
         |    b then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b) c else d
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: trailing alphanumeric infix, deeper operand, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => a max
         |    b
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => a max b
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix(tname("a"), "max", tname("b"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: two statements of an indented def body") {
    val code =
      """|object A:
         |  def f =
         |    xs.foreach: x => println(x)
         |    ys.foreach: y => println(y)
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    xs.foreach {
         |      x => println(x)
         |    }
         |    ys.foreach {
         |      y => println(y)
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
          tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tapply(tname("println"), tname("x"))))),
          tapply(tselect("ys", "foreach"), blk(tfunc(tparam("y"))(tapply(tname("println"), tname("y"))))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for do body, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for x <- xs do ys.foreach: y => println(y)
         |    next
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs) ys.foreach {
         |      y => println(y)
         |    }
         |    next
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
            List(Enumerator.Generator(patvar("x"), tname("xs"))),
            tapply(
              tselect("ys", "foreach"),
              blk(tfunc(tparam("y"))(tapply(tname("println"), tname("y")))),
            ),
          ),
          tname("next"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: val rhs, leading select with a placeholder on the next line, next definition") {
    val code =
      """|object A:
         |  val f = xs.map: x => x
         |    .filter(_ > 0)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  val f = xs.map {
         |    x => x.filter(_ > 0)
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(
          Nil,
          List(patvar("f")),
          None,
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tapply(
              tselect("x", "filter"),
              Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", lit(0))),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in an interpolated block, next definition") {
    val code =
      """|object A:
         |  def f = s"${xs.map: x => x}"
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = s"${xs.map {
         |    x => x
         |  }}"
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
          Term.Interpolate(
            tname("s"),
            List(str(""), str("")),
            List(blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match body with a case in braces, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => x match { case 1 => 2 }
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x match {
         |      case 1 => 2
         |    }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tmatch(tname("x"), Case(lit(1), None, lit(2))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match body with a case on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => x match
         |    case 1 => 2
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x match {
         |      case 1 => 2
         |    }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tmatch(tname("x"), Case(lit(1), None, lit(2))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: class body statement, next definition") {
    val code =
      """|class A:
         |  xs.foreach: x => println(x)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|class A {
         |  xs.foreach {
         |    x => println(x)
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(
        tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tapply(tname("println"), tname("x"))))),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: try body with catch case, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => try a catch case e => b
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => try a catch {
         |      case e => b
         |    }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term.Try(tname("a"), List(Case(patvar("e"), None, tname("b"))), None),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: return body, next definition") {
    val code =
      """|object A:
         |  def f = xs.foreach: x => return x
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.foreach {
         |    x => return x
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
          tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(Term.Return(tname("x"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in braces, closed on the same line, next definition") {
    val code =
      """|object A:
         |  def f = foo { xs.map: x => x }
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = foo {
         |    xs.map {
         |      x => x
         |    }
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
          tapply(tname("foo"), blk(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: comment before the body, leading select on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => /* c */ x
         |    .foo
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => /* c */ x.foo
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tselect("x", "foo").toBuilder.begComment(Seq("/* c */")).result())),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: nested on the same line, after a select, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => x.bar: y => y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x.bar {
         |      y => y
         |    }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tapply(tselect("x", "bar"), blk(tfunc(tparam("y"))(tname("y")))))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if then else body, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if x then a else b
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (x) a else b
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(tname("x"), tname("a"), tname("b"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for yield body, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => for y <- ys yield y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => for (y <- ys) yield y
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term.ForYield(List(Enumerator.Generator(patvar("y"), tname("ys"))), tname("y")),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while do body, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => while x do y
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => while (x) y
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.While(tname("x"), tname("y"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new body, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => new A
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => new A
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.New(init(pname("A")))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: ascribed body, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => x: Int
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x: Int
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.Ascribe(tname("x"), pname("Int"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: nested on the same line, leading select on the next line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => xs.map: y => y
         |    .foo
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => xs.map {
         |      y => y.foo
         |    }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              tapply(tselect("xs", "map"), blk(tfunc(tparam("y"))(tselect("y", "foo")))),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: body args span lines, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => foo(x,
         |    1)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => foo(x, 1)
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tapply(tname("foo"), tname("x"), lit(1))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses, else on the same line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (x > 0) 1 else 2
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (x > 0) 1 else 2
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tinfix(tname("x"), ">", lit(0)), lit(1), lit(2)))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: context function and placeholder param, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x ?=> x
         |  def g = xs.map: _ => 1
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x ?=> x
         |  }
         |  def g = xs.map {
         |    _ => 1
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
          tapply(tselect("xs", "map"), blk(tctxfunc(tparam("x"))(tname("x")))),
        ),
        Defn
          .Def(Nil, tname("g"), Nil, None, tapply(tselect("xs", "map"), blk(tfunc(tparam("_"))(lit(1))))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, then on the same line, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => if (a) && b then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => if (a && b) c else d
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and a body, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => while (a) b
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => while (a) b
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(Term.While(tname("a"), tname("b"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in a match case guard, next case") {
    val code =
      """|object A:
         |  def f(x: Int) = x match
         |    case y if xs.exists: z => z > y => 1
         |    case _ => 2
         |""".stripMargin
    val layout =
      """|object A {
         |  def f(x: Int) = x match {
         |    case y if xs.exists {
         |      z => z > y
         |    } => 1
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
        List(pcg(tpc(tparam("x", pname("Int"))))),
        None,
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tapply(
              tselect("xs", "exists"),
              blk(tfunc(tparam("z"))(tinfix(tname("z"), ">", tname("y")))),
            )),
            lit(1),
          ),
          Case(Pat.Wildcard(), None, lit(2)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 colon: context bounds of abstract types in a trait") {
    val code =
      """|trait X:
         |  type T: Ordering
         |  type U <: Int: Ordering
         |  def g = 1
         |""".stripMargin
    val layout =
      """|trait X {
         |  type T: Ordering
         |  type U <: Int: Ordering
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Trait(
      Nil,
      pname("X"),
      Nil,
      EmptyCtor(),
      tpl(
        Decl.Type(Nil, pname("T"), Nil, bounds(cb = List(pname("Ordering")))),
        Decl.Type(Nil, pname("U"), Nil, bounds(hi = pname("Int"), cb = List(pname("Ordering")))),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: two leading infixes on the next lines, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: x => x
         |    + 1
         |    + 2
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    x => x + 1 + 2
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tinfix(tinfix(tname("x"), "+", lit(1)), "+", lit(2)))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: val rhs, next val with a typed for generator") {
    val code =
      """|object A:
         |  val p = xs.map: x => y
         |  val q = for x: Int <- ys yield x
         |""".stripMargin
    val layout =
      """|object A {
         |  val p = xs.map {
         |    x => y
         |  }
         |  val q = for (x: Int <- ys) yield x
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(
          Nil,
          List(patvar("p")),
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("y")))),
        ),
        Defn.Val(
          Nil,
          List(patvar("q")),
          None,
          Term.ForYield(
            List(Enumerator.Generator(Pat.Typed(patvar("x"), pname("Int")), tname("ys"))),
            tname("x"),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses, if-then body, next definition") {
    val code =
      """|object O:
         |  def f = xs.foreach: x => while (a) if b then println(x)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foreach {
         |    x => while (a) if (b) println(x)
         |  }
         |  def g = 1
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(
              Term.While(tname("a"), tif(tname("b"), tapply(tname("println"), tname("x")))),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test(
    "SIP-75 single-line lambda: while with parentheses, if with parentheses body, next definition",
  ) {
    val code =
      """|object O:
         |  def f = xs.foreach: x => while (a) if (b) println(x)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foreach {
         |    x => while (a) if (b) println(x)
         |  }
         |  def g = 1
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(
              Term.While(tname("a"), tif(tname("b"), tapply(tname("println"), tname("x")))),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for with parentheses, if-then body, next definition") {
    val code =
      """|object O:
         |  def f = xs.foreach: x => for (y <- ys) if b then println(y)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foreach {
         |    x => for (y <- ys) if (b) println(y)
         |  }
         |  def g = 1
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.For(
              List(Enumerator.Generator(patvar("y"), tname("ys"))),
              tif(tname("b"), tapply(tname("println"), tname("y"))),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for with braces, if-then body, next definition") {
    val code =
      """|object O:
         |  def f = xs.foreach: x => for { y <- ys } if b then println(y)
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foreach {
         |    x => for (y <- ys) if (b) println(y)
         |  }
         |  def g = 1
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.For(
              List(Enumerator.Generator(patvar("y"), tname("ys"))),
              tif(tname("b"), tapply(tname("println"), tname("y"))),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: return of an if-then-else, next statement") {
    val code =
      """|object O:
         |  def f: Int =
         |    xs.foreach: x => return if b then 1 else 2
         |    0
         |""".stripMargin
    val layout =
      """|object O {
         |  def f: Int = {
         |    xs.foreach {
         |      x => return if (b) 1 else 2
         |    }
         |    0
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        Some(pname("Int")),
        blk(
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.Return(tif(tname("b"), lit(1), lit(2))))),
          ),
          lit(0),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in then in parens, name body, else on the same line") {
    val code =
      """|object O:
         |  def f = g(if c then xs.map: x => x else y)
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = g(if (c) xs.map {
         |    x => x
         |  } else y)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tname("g"),
          tif(
            tname("c"),
            tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
            tname("y"),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: in try in parens, name body, finally on the same line") {
    val code =
      """|object O:
         |  def f = g(try xs.map: x => x finally ())
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = g(try xs.map {
         |    x => x
         |  } finally ())
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tname("g"),
          Term.Try(
            tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
            Nil,
            Some(Lit.Unit()),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and two infixes, the second trailing, operand on the next line, leading infix and then on the third line") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) && b &&
         |    c
         |    && d then e else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a && b && c && d) e else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond =
      tinfix(tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")), "&&", tname("d"))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("e"), tname("g"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and two infixes, the second trailing, operand on the next line, then on the third line") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) && b &&
         |    c
         |    then e else g
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a && b && c) e else g
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c"))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("e"), tname("g"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and two infixes, the second trailing, operand and then on the next line") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) || b ||
         |    c then d else e
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a || b || c) d else e
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val cond = tinfix(tinfix(tname("a"), "||", tname("b")), "||", tname("c"))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(cond, tname("d"), tname("e"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match with a case on the same line, deeper leading select, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => x match case 1 => 2
         |    .foo
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => x match {
         |      case 1 =>
         |        2.foo
         |    }
         |  }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tmatch(tname("x"), Case(lit(1), None, tselect(lit(2), "foo"))))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: trailing symbolic infix, deeper operand, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => a +
         |    b
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => a + b
         |  }
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
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix(tname("a"), "+", tname("b"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: match at the line end, deeper case, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => x match
         |    case 1 => 2
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => x match {
         |      case 1 => 2
         |    }
         |  }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tmatch(tname("x"), Case(lit(1), None, lit(2))))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: colon at the line end, deeper argument, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => x.foo:
         |    y
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => x.foo {
         |      y
         |    }
         |  }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tapply(tselect("x", "foo"), blk(tname("y"))))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses at the line end, body one step deeper, next definition") {
    val code =
      """|object O:
         |  def f = xs.foreach: x => while (a)
         |    println(x)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foreach {
         |    x => while (a) println(x)
         |  }
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(tname("a"), tapply(tname("println"), tname("x"))))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test(
    "SIP-75 single-line lambda: val with a refinement type, deeper leading select, next definition",
  ) {
    val code =
      """|object O:
         |  val r: A { type T } = xs.map: x => x
         |    .foo
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  val r: A {
         |    type T
         |  } = xs.map {
         |    x => x.foo
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Val(
          Nil,
          List(patvar("r")),
          Some(
            Type.Refine(Some(pname("A")), Stat.Block(List(Decl.Type(Nil, pname("T"), Nil, noBounds)))),
          ),
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tselect("x", "foo")))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: parameter with a refinement type, deeper leading select, next definition") {
    val code =
      """|object O:
         |  def f(y: A { type T }) = xs.map: x => x
         |    .foo
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f(y: A {
         |    type T
         |  }) = xs.map {
         |    x => x.foo
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val refine = Type.Refine(Some(pname("A")), stats(Decl.Type(Nil, pname("T"), Nil, noBounds)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          List(List(tparam("y", refine))),
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tselect("x", "foo")))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: type argument with a refinement type, deeper leading select, next definition") {
    val code =
      """|object O:
         |  def f = xs.foo[A { type T }]: y => y
         |    .bar
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foo[A {
         |    type T
         |  }] {
         |    y => y.bar
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val refine = Type.Refine(Some(pname("A")), stats(Decl.Type(Nil, pname("T"), Nil, noBounds)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tapplytype(tselect("xs", "foo"), refine), blk(tfunc(tparam("y"))(tselect("y", "bar")))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: result with a refinement type, deeper leading select, next definition") {
    val code =
      """|object O:
         |  def f: A { type T } = xs.map: x => x
         |    .foo
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f: A {
         |    type T
         |  } = xs.map {
         |    x => x.foo
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val refine = Type.Refine(Some(pname("A")), stats(Decl.Type(Nil, pname("T"), Nil, noBounds)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          Some(refine),
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tselect("x", "foo")))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: type alias of a refinement type, semicolon, val, deeper leading select, next definition") {
    val code =
      """|object O:
         |  type T = A { type U }; val v = xs.map: x => x
         |    .foo
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  type T = A {
         |    type U
         |  }
         |  val v = xs.map {
         |    x => x.foo
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val refine = Type.Refine(Some(pname("A")), stats(Decl.Type(Nil, pname("U"), Nil, noBounds)))
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Type(Nil, pname("T"), Nil, refine),
        Defn.Val(
          Nil,
          List(patvar("v")),
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tselect("x", "foo")))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and a body, deeper leading select, next definition") {
    val code =
      """|object O:
         |  def f = xs.foreach: x => while (a) b
         |    .foo
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foreach {
         |    x => while (a) b.foo
         |  }
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(tname("a"), tselect("b", "foo")))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a body, next definition with if, then and else") {
    val code =
      """|object O:
         |  def f = xs.foreach: x => if (a) g(x)
         |  def h = if b then c else d
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foreach {
         |    x => if (a) g(x)
         |  }
         |  def h = if (b) c else d
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
          tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tif(tname("a"), tapply("g", "x"))))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, tif(tname("b"), tname("c"), tname("d"))),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a body, next statement with if, then and else in braces") {
    val code =
      """|object O:
         |  def f =
         |    xs.foreach: x => if (a) g(x)
         |    k { if b then c else d }
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = {
         |    xs.foreach {
         |      x => if (a) g(x)
         |    }
         |    k {
         |      if (b) c else d
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tif(tname("a"), tapply("g", "x"))))),
          tapply(tname("k"), blk(tif(tname("b"), tname("c"), tname("d")))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and a body, next statement with a lambda and if then") {
    val code =
      """|object O:
         |  def f =
         |    xs.foreach: x => if (a) g(x)
         |    ys.foreach: z => if b then c
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = {
         |    xs.foreach {
         |      x => if (a) g(x)
         |    }
         |    ys.foreach {
         |      z => if (b) c
         |    }
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(tselect("xs", "foreach"), blk(tfunc(tparam("x"))(tif(tname("a"), tapply("g", "x"))))),
          tapply(tselect("ys", "foreach"), blk(tfunc(tparam("z"))(tif(tname("b"), tname("c"))))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses and an infix, do on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.foreach: x => while (a) && b
         |    do g(x)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foreach {
         |    x => while (a && b) g(x)
         |  }
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.While(tinfix(tname("a"), "&&", tname("b")), tapply("g", "x")))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: if with parentheses and an infix, deeper leading infix with then and else, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => if (a) && b
         |    && c then d else e
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => if (a && b && c) d else e
         |  }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "&&", tname("c")),
              tname("d"),
              tname("e"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: while with parentheses, an infix and do, deeper leading select, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => while (a) && b do g(x)
         |    .foo
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => while (a && b) g(x).foo
         |  }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(
              Term.While(tinfix(tname("a"), "&&", tname("b")), tselect(tapply("g", "x"), "foo")),
            )),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: for with a generator, deeper generator, do on the next line, next definition") {
    val code =
      """|object O:
         |  def f = xs.foreach: x => for y <- ys
         |      z <- zs
         |    do g(y)
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.foreach {
         |    x => for (y <- ys; z <- zs) g(y)
         |  }
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
          tapply(
            tselect("xs", "foreach"),
            blk(tfunc(tparam("x"))(Term.For(
              List(Enumerator.Generator(patvar("y"), "ys"), Enumerator.Generator(patvar("z"), "zs")),
              tapply("g", "y"),
            ))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: XML literal across the line end, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => <a>
         |    b</a>
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => <a>
         |    b</a>
         |  }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(Term.Xml(List(str("<a>\n    b</a>")), Nil))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    checkTree(source(code), layout)(tree)
  }

  test("SIP-75 single-line lambda: XML literal across the line end, deeper leading select, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => <a>
         |    b</a>
         |    .foo
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => <a>
         |    b</a>.foo
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val xml = Term.Xml(List(str("<a>\n    b</a>")), Nil)
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tselect(xml, "foo")))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    checkTree(source(code), layout)(tree)
  }

  test("SIP-75 single-line lambda: multi-line comment across the line end, select after it, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => x /* a
         |    b */ .foo
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => x /* a
         |    b */ .foo
         |  }
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
          tapply(
            tselect("xs", "map"),
            blk(tfunc(tparam("x"))(tselect(tnameComments("x")()("/* a\n    b */"), "foo"))),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: multi-line comment across the line end, next definition") {
    val code =
      """|object O:
         |  def f = xs.map: x => x /* a
         |    b */
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = xs.map {
         |    x => x /* a
         |    b */
         |  }
         |  def h = 1
         |}
         |""".stripMargin
    val func = Term.Function.newBuilder(Term.ParamClause(List(tparam("x"))), tname("x"))
      .endComment(Seq("/* a\n    b */")).result()
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Def(Nil, tname("f"), Nil, None, tapply(tselect("xs", "map"), blk(func))),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 colon: self type, statement after the arrow") {
    val code = "class C { self: A => B }"
    val tree =
      Source(List(Defn.Class(Nil, pname("C"), Nil, ctor, tpl(self("self", pname("A")), tname("B")))))
    runTestAssert[Source](code)(tree)
  }

  test("SIP-75 colon: self type, two arrows") {
    val code = "class C { self: A => B => def f = 1 }"
    val error =
      """|<input>:1: error: `;` expected but `=>` found
         |class C { self: A => B => def f = 1 }
         |                       ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 colon: case with a typed pattern") {
    val code =
      """|object O:
         |  def f = x match
         |    case y: A => B
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = x match {
         |    case y: A => B
         |  }
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
          tmatch(tname("x"), Case(Pat.Typed(patvar("y"), pname("A")), None, tname("B"))),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 colon: val with a function type") {
    val code =
      """|object O:
         |  val y: A => B = g
         |""".stripMargin
    val layout = "object O { val y: A => B = g }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Val(Nil, List(patvar("y")), Some(pfunc(pname("A"))(pname("B"))), tname("g"))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 colon: generator with a function type") {
    val code =
      """|object O:
         |  def f = for y: A => B <- ys yield y
         |""".stripMargin
    val error =
      """|<input>:2: error: `<-` expected but `=>` found
         |  def f = for y: A => B <- ys yield y
         |                   ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 colon: value definition in for with a function type") {
    val code =
      """|object O:
         |  def f = for x <- xs; y: A => B = g yield y
         |""".stripMargin
    val error =
      """|<input>:2: error: `<-` expected but `=>` found
         |  def f = for x <- xs; y: A => B = g yield y
         |                            ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("SIP-75 colon: self entry point, self type") {
    implicit val parser: String => Self = parseRule(_, Parse.parseSelf)
    runTestAssert[Self]("self: A =>")(self("self", pname("A")))
  }

  test("SIP-75 colon: self entry point, self type and a statement") {
    implicit val parser: String => Self = parseRule(_, Parse.parseSelf)
    val error =
      """|<input>:1: error: `end of file` expected but `identifier` found
         |self: A => B
         |           ^""".stripMargin
    runTestError[Self]("self: A => B", error)
  }

  test("SIP-75 colon: self entry point, function type in parentheses") {
    implicit val parser: String => Self = parseRule(_, Parse.parseSelf)
    val error =
      """|<input>:1: error: expected `=>`
         |self: (A => B)
         |              ^""".stripMargin
    runTestError[Self]("self: (A => B)", error)
  }

  test("SIP-75 colon: pattern entry point, function type") {
    implicit val parser: String => Pat = parseRule(_, Parse.parsePat)
    val error =
      """|<input>:1: error: `end of file` expected but `=>` found
         |y: A => B
         |     ^""".stripMargin
    runTestError[Pat]("y: A => B", error)
  }

  test("SIP-75 colon: enumerator entry point, generator with a function type") {
    implicit val parser: String => Enumerator = parseRule(_, Parse.parseEnumerator)
    val error =
      """|<input>:1: error: `<-` expected but `=>` found
         |y: A => B <- ys
         |     ^""".stripMargin
    runTestError[Enumerator]("y: A => B <- ys", error)
  }

  test("SIP-75 colon: enumerator entry point, value definition with a function type") {
    implicit val parser: String => Enumerator = parseRule(_, Parse.parseEnumerator)
    val error =
      """|<input>:1: error: `<-` expected but `=>` found
         |y: A => B = g
         |     ^""".stripMargin
    runTestError[Enumerator]("y: A => B = g", error)
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

}
