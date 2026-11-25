package scala.meta.tests.parsers.dotty

import scala.meta._

class ControlConditionSuite extends BaseDottySuite {

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
    val error =
      """|<input>:1: error: `;` expected but `then` found
         |def f = if (a) && b match { case _ => true } then c else d
         |                                             ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test(
    "parenthesized condition: an operator and `new` after the parentheses, `then` on the same line",
  ) {
    val code = "def f = if (a) && new B().c then c else d"
    val error =
      """|<input>:1: error: `;` expected but `new` found
         |def f = if (a) && new B().c then c else d
         |                  ^""".stripMargin
    runTestError[Stat](code, error)
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
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |  then c
         |  ^""".stripMargin
    runTestError[Stat](code, error)
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
      """|<input>:3: error: `outdent` expected but `do` found
         |  do c
         |  ^""".stripMargin
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
    val layout =
      """|def f = if (a && xs.exists(x => x > 0)) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix("a", "&&", tapply(tselect("xs", "exists"), tfunc(tparam("x"))(tinfix("x", ">", lit(0))))),
        tname("c"),
        tname("d"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |KwThen [70..74)
         |Ident(c) [75..76)
         |Indentation.Outdent [76..76)
         |KwElse [79..83)
         |Ident(d) [84..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: `outdent` expected but `.` found
         |  if (a) || xs.exists: x =>
         |              ^""".stripMargin
    runTestError[Stat](code, error)
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
         |KwDo [81..83)
         |Ident(c) [84..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: `outdent` expected but `.` found
         |  while (a) || xs.exists: x =>
         |                 ^""".stripMargin
    runTestError[Stat](code, error)
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
         |KwThen [61..65)
         |Ident(c) [66..67)
         |Indentation.Outdent [67..67)
         |KwElse [70..74)
         |Ident(d) [75..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val error =
      """|<input>:2: error: `outdent` expected but `.` found
         |  if (a) || xs.exists: x =>
         |              ^""".stripMargin
    runTestError[Stat](code, error)
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
    val error =
      """|<input>:4: error: `outdent` expected but `then` found
         |  then c
         |  ^""".stripMargin
    runTestError[Stat](code, error)
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
    val error =
      """|<input>:1: error: `;` expected but `xml start` found
         |def f = if (a) || <x/>.isEmpty then c else d
         |                  ^""".stripMargin
    runTestError[Stat](code, error)
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
         |Indentation.Outdent [44..44)
         |KwThen [49..53)
         |Ident(c) [54..55)
         |KwElse [58..62)
         |Ident(d) [63..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if (x = xs.exists {
         |  y => y > 0
         |}) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        Term.Assign(
          tname("x"),
          tapply(tselect("xs", "exists"), blk(tfunc(tparam("y"))(tinfix("y", ">", lit(0))))),
        ),
        tname("c"),
        tname("d"),
      ),
    )
    parseAndCheckTree[Stat](code, layout)(tree)
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
    val layout =
      """|def f = if (a && b || c) d else e
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |Ident(c) [28..29)
         |KwThen [34..38)
         |Ident(d) [39..40)
         |KwElse [43..47)
         |Ident(e) [48..49)
         |Indentation.Outdent [49..49)
         |EOF [50..50)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = if ((a && b) c) d else e
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tpostfix(tinfix(tname("a"), "&&", tname("b")), "c"), tname("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(tree)
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
    val error =
      """|<input>:2: error: `outdent` expected but `.` found
         |  if (a) && xs.exists: x =>
         |              ^""".stripMargin
    runTestError[Stat](code, error)
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
    val error =
      """|<input>:2: error: `outdent` expected but `.` found
         |  if (a) && xs.exists: x =>
         |              ^""".stripMargin
    runTestError[Stat](code, error)
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
         |Indentation.Outdent [69..69)
         |KwCase [72..76)
         |Underscore [77..78)
         |RightArrow [79..81)
         |Constant.Int(2) [82..83)
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
         |Indentation.Outdent [71..71)
         |KwCase [74..78)
         |Underscore [79..80)
         |RightArrow [81..83)
         |Constant.Int(2) [84..85)
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
    val error =
      """|<input>:2: error: `;` expected but `then` found
         |  def f = if (a) && f(if b then c else d) then e else g
         |                                          ^""".stripMargin
    runTestError[Source](code, error)
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
         |  def f = if (if (a) {
         |    b
         |  } c) d else e
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
        tif(tif(tname("a"), tpostfix(blk(tname("b")), "c")), tname("d"), tname("e")),
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
      """|object A { def f = while (while (a) b c) d }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(Term.While(tname("a"), tpostfix(tname("b"), "c")), tname("d")),
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
         |Indentation.Outdent [36..36)
         |KwThen [37..41)
         |Ident(c) [42..43)
         |KwElse [44..48)
         |Ident(d) [49..50)
         |LF [50..51)
         |Ident(g) [53..54)
         |Indentation.Outdent [54..54)
         |EOF [55..55)
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
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |Indentation.Outdent [36..36)
         |KwThen [37..41)
         |Ident(c) [42..43)
         |LF [43..44)
         |Ident(g) [46..47)
         |Indentation.Outdent [47..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (try a catch {
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
        tif(Term.Try(tname("a"), List(Case(patwildcard, None, tname("b"))), None), tname("c")),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |Indentation.Outdent [39..39)
         |KwDo [40..42)
         |Ident(c) [43..44)
         |LF [44..45)
         |Ident(g) [47..48)
         |Indentation.Outdent [48..48)
         |EOF [49..49)
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
         |Indentation.Outdent [18..18)
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
         |Indentation.Outdent [28..28)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
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
         |Indentation.Outdent [28..28)
         |KwThen [29..33)
         |Ident(c) [34..35)
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
         |Indentation.Outdent [30..30)
         |KwThen [31..35)
         |Ident(c) [36..37)
         |KwElse [38..42)
         |Ident(d) [43..44)
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
         |Indentation.Outdent [44..44)
         |KwThen [45..49)
         |Ident(c) [50..51)
         |KwElse [52..56)
         |Ident(d) [57..58)
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
         |Indentation.Outdent [21..21)
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
         |Indentation.Outdent [31..31)
         |KwDo [32..34)
         |Ident(c) [35..36)
         |LF [36..37)
         |Ident(g) [39..40)
         |LF [40..41)
         |Ident(h) [43..44)
         |Indentation.Outdent [44..44)
         |EOF [45..45)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  while (new B(a).ok) c
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
        Term.While(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c")),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |Indentation.Outdent [63..63)
         |KwThen [64..68)
         |Ident(c) [69..70)
         |KwElse [71..75)
         |Ident(d) [76..77)
         |LF [77..78)
         |Ident(g) [80..81)
         |LF [81..82)
         |Ident(h) [84..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])
    val layout =
      """|def f = {
         |  if (x match {
         |    case 1 => true
         |    case _ => false
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
          tmatch(tname("x"), Case(int(1), None, lit(true)), Case(patwildcard, None, lit(false))),
          tname("c"),
          tname("d"),
        ),
        tname("g"),
        tname("h"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |LeftParen [29..30)
         |Ident(c) [30..31)
         |RightParen [31..32)
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
    val layout =
      """|def f = if (a.g(c)) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tapply(tselect("a", "g"), tname("c")), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("unparenthesized condition: a select, deeper argument in parentheses, `then` on a later line") {
    val code =
      """|def f =
         |  if a.g
         |    (c)
         |  then c else d
         |""".stripMargin
    val layout =
      """|def f = if (a.g(c)) c else d
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(tapply(tselect("a", "g"), tname("c")), tname("c"), tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |LeftParen [45..46)
         |Ident(e) [46..47)
         |RightParen [47..48)
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
    val layout =
      """|def f = {
         |  if (a && {
         |    if (b) c else d
         |  }.g(e)) x else y
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
          tinfix(
            "a",
            "&&",
            tapply(tselect(blk(tif(tname("b"), tname("c"), tname("d"))), "g"), tname("e")),
          ),
          tname("x"),
          tname("y"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
         |LeftParen [43..44)
         |Ident(e) [44..45)
         |RightParen [45..46)
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
    val layout =
      """|def f = {
         |  if (a.g(if (b) 1 else 2)(e)) x else y
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
          tapply(tapply(tselect("a", "g"), tif(tname("b"), lit(1), lit(2))), tname("e")),
          tname("x"),
          tname("y"),
        ),
        tname("g"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("parenthesized condition: a colon at the line end in braces, deeper `then` and `else`, blank line") {
    val code =
      """|def f = {
         |  if (a):
         |      then c else d
         |
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a {}) c else d
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, blk(tif(tapply(tname("a"), blk()), tname("c"), tname("d"))))
    runTestAssert[Stat](code, layout)(tree)
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
         |LF [31..32)
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
      """|def f = {
         |  if (new B(a).ok) c
         |  {
         |    c
         |  }
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tselect(
              Term.New(Init(pname("B"), Name.Anonymous(), List(Term.ArgClause(List(tname("a")), None)))),
              "ok",
            ),
            tname("c"),
          ),
          blk(tname("c")),
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
         |Indentation.Outdent [44..44)
         |KwThen [45..49)
         |Ident(d) [50..51)
         |KwElse [52..56)
         |Ident(e) [57..58)
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
         |Indentation.Outdent [50..50)
         |KwDo [51..53)
         |Ident(d) [54..55)
         |LF [55..56)
         |Ident(g) [60..61)
         |LF [61..62)
         |Ident(h) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a) b
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
          Term.While(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
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
         |Indentation.Outdent [44..44)
         |KwThen [45..49)
         |Ident(d) [50..51)
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
         |Indentation.Outdent [67..67)
         |KwDo [68..70)
         |Ident(c) [71..72)
         |LF [72..73)
         |Ident(g) [77..78)
         |LF [78..79)
         |Ident(h) [83..84)
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
         |Indentation.Indent [73..73)
         |Ident(a) [80..81)
         |Indentation.Outdent [81..81)
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
         |Indentation.Outdent [68..68)
         |KwDo [69..71)
         |Ident(step) [72..76)
         |LeftParen [76..77)
         |RightParen [77..78)
         |LF [78..79)
         |Ident(g) [83..84)
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
         |    while ({
         |      i += 1
         |      if (i > n) stop()
         |      i < m
         |    }) step()
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
          Term.While(
            blk(
              tinfix(tname("i"), "+=", int(1)),
              tif(tinfix(tname("i"), ">", tname("n")), tapply(tname("stop"))),
              tinfix(tname("i"), "<", tname("m")),
            ),
            tapply(tname("step")),
          ),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
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
         |KwThen [58..62)
         |Ident(d) [63..64)
         |KwElse [65..69)
         |Ident(e) [70..71)
         |Indentation.Outdent [71..71)
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
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e) g else h
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
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
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
         |KwDo [64..66)
         |Ident(d) [67..68)
         |LF [68..69)
         |Ident(e) [75..76)
         |Indentation.Outdent [76..76)
         |KwDo [81..83)
         |Ident(k) [84..85)
         |LF [85..86)
         |Ident(g) [90..91)
         |Indentation.Outdent [91..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a && b || c) d
         |      e
         |    }) k
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
          Term.While(
            blk(
              Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
              tname("e"),
            ),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
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
         |LeftParen [53..54)
         |Ident(c) [54..55)
         |RightParen [55..56)
         |KwThen [57..61)
         |Ident(d) [62..63)
         |KwElse [64..68)
         |Ident(e) [69..70)
         |Indentation.Outdent [70..70)
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
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && f(c)) d else e) g else h
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
            tif(tinfix(tname("a"), "&&", tapply(tname("f"), tname("c"))), tname("d"), tname("e")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
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
         |Indentation.Outdent [71..71)
         |KwThen [78..82)
         |Ident(y) [83..84)
         |KwElse [85..89)
         |Ident(z) [90..91)
         |LF [91..92)
         |Ident(g) [96..97)
         |LF [97..98)
         |Ident(h) [102..103)
         |Indentation.Outdent [103..103)
         |Indentation.Outdent [103..103)
         |EOF [104..104)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) f {
         |      if (b) c else d
         |    }) y else z
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
            tif(tname("a"), tapply(tname("f"), blk(tif(tname("b"), tname("c"), tname("d"))))),
            tname("y"),
            tname("z"),
          ),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
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
         |Indentation.Outdent [77..77)
         |KwDo [84..86)
         |Ident(y) [87..88)
         |LF [88..89)
         |Ident(g) [93..94)
         |LF [94..95)
         |Ident(h) [99..100)
         |Indentation.Outdent [100..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (while (a) f {
         |      if (b) c else d
         |    }) y
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
          Term.While(
            Term.While(tname("a"), tapply(tname("f"), blk(tif(tname("b"), tname("c"), tname("d"))))),
            tname("y"),
          ),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
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
         |Indentation.Outdent [47..47)
         |KwThen [48..52)
         |Ident(d) [53..54)
         |KwElse [55..59)
         |Ident(e) [60..61)
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
         |Indentation.Outdent [54..54)
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
         |Indentation.Outdent [52..52)
         |KwThen [53..57)
         |Ident(d) [58..59)
         |KwElse [60..64)
         |Ident(e) [65..66)
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
         |Indentation.Outdent [49..49)
         |KwDo [50..52)
         |Ident(d) [53..54)
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
         |    while ({
         |      if (a) b.g
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
          Term.While(blk(tif(tname("a"), tselect(tname("b"), "g")), tname("c")), tname("d")),
          tname("g"),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with a colon argument in its `then` body, the argument on the next line with a comment, a blank line, leading `&&` on the next line, `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if a then foo:
         |          z // c
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
         |Ident(foo) [36..39)
         |Colon [39..40)
         |Indentation.Indent [40..40)
         |Ident(z) [51..52)
         |Indentation.Outdent [58..58)
         |Ident(&&) [67..69)
         |Ident(w) [70..71)
         |KwElse [78..82)
         |Ident(d) [83..84)
         |RightParen [84..85)
         |LF [85..86)
         |Ident(h) [90..91)
         |Indentation.Outdent [91..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) foo {
         |      z // c
         |
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
              tinfix(tapply(tname("foo"), blk(tnameComments("z")()("// c"))), "&&", tname("w")),
              tname("d"),
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`for` with braces within parentheses, a colon argument after `yield`, the argument on the next line, a blank line, leading `&&` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(for { x <- xs } yield foo:
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
         |LeftBrace [30..31)
         |Ident(x) [32..33)
         |LeftArrow [34..36)
         |Ident(xs) [37..39)
         |RightBrace [40..41)
         |KwYield [42..47)
         |Ident(foo) [48..51)
         |Colon [51..52)
         |Indentation.Indent [52..52)
         |Ident(z) [63..64)
         |Indentation.Outdent [65..65)
         |Ident(&&) [74..76)
         |Ident(w) [77..78)
         |RightParen [78..79)
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
         |    g(for (x <- xs) yield foo {
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
              tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("case guard: a colon argument, the argument on the next line, a blank line, leading `&&` and `=>` on the next line, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case y if foo:
         |        z
         |
         |      && w => 1
         |    case _ => 2
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
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Ident(y) [37..38)
         |KwIf [39..41)
         |Ident(foo) [42..45)
         |Colon [45..46)
         |Indentation.Indent [46..46)
         |Ident(z) [55..56)
         |Indentation.Outdent [57..57)
         |Ident(&&) [64..66)
         |Ident(w) [67..68)
         |RightArrow [69..71)
         |Constant.Int(1) [72..73)
         |LF [73..74)
         |KwCase [78..82)
         |Underscore [83..84)
         |RightArrow [85..87)
         |Constant.Int(2) [88..89)
         |Indentation.Outdent [89..89)
         |Indentation.Outdent [89..89)
         |EOF [90..90)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case y if foo {
         |      z
         |    } && w => 1
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
        Nil,
        None,
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w"))),
            int(1),
          ),
          Case(patwildcard, None, int(2)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("case guard: a colon argument, the argument on the next line, two blank lines, leading `&&` and `=>` on the next line, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case y if foo:
         |        z
         |
         |
         |      && w => 1
         |    case _ => 2
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
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Ident(y) [37..38)
         |KwIf [39..41)
         |Ident(foo) [42..45)
         |Colon [45..46)
         |Indentation.Indent [46..46)
         |Ident(z) [55..56)
         |Indentation.Outdent [58..58)
         |Ident(&&) [65..67)
         |Ident(w) [68..69)
         |RightArrow [70..72)
         |Constant.Int(1) [73..74)
         |LF [74..75)
         |KwCase [79..83)
         |Underscore [84..85)
         |RightArrow [86..88)
         |Constant.Int(2) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case y if foo {
         |      z
         |    } && w => 1
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
        Nil,
        None,
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w"))),
            int(1),
          ),
          Case(patwildcard, None, int(2)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("case guard: a colon argument, the argument on the next line with a comment, a blank line, leading `&&` and `=>` on the next line, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case y if foo:
         |        z // c
         |
         |      && w => 1
         |    case _ => 2
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
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Ident(y) [37..38)
         |KwIf [39..41)
         |Ident(foo) [42..45)
         |Colon [45..46)
         |Indentation.Indent [46..46)
         |Ident(z) [55..56)
         |Indentation.Outdent [62..62)
         |Ident(&&) [69..71)
         |Ident(w) [72..73)
         |RightArrow [74..76)
         |Constant.Int(1) [77..78)
         |LF [78..79)
         |KwCase [83..87)
         |Underscore [88..89)
         |RightArrow [90..92)
         |Constant.Int(2) [93..94)
         |Indentation.Outdent [94..94)
         |Indentation.Outdent [94..94)
         |EOF [95..95)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case y if foo {
         |      z // c
         |
         |    } && w => 1
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
        Nil,
        None,
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tinfix(tapply(tname("foo"), blk(tnameComments("z")()("// c"))), "&&", tname("w"))),
            int(1),
          ),
          Case(patwildcard, None, int(2)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("a colon argument within parentheses after type arguments, the argument on the next line, a blank line, leading `&&` and `)` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g[Int](foo:
         |        z
         |
         |      && w)
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
         |LeftBracket [25..26)
         |Ident(Int) [26..29)
         |RightBracket [29..30)
         |LeftParen [30..31)
         |Ident(foo) [31..34)
         |Colon [34..35)
         |Indentation.Indent [35..35)
         |Ident(z) [44..45)
         |Indentation.Outdent [46..46)
         |Ident(&&) [53..55)
         |Ident(w) [56..57)
         |RightParen [57..58)
         |LF [58..59)
         |Ident(h) [63..64)
         |Indentation.Outdent [64..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g[Int](foo {
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
            tapplytype(tname("g"), pname("Int")),
            tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with a colon argument in its `else` body, the argument on the next line, a blank line, leading `&&` and `)` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if a then b else foo:
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
         |KwIf [26..28)
         |Ident(a) [29..30)
         |KwThen [31..35)
         |Ident(b) [36..37)
         |KwElse [38..42)
         |Ident(foo) [43..46)
         |Colon [46..47)
         |Indentation.Indent [47..47)
         |Ident(z) [58..59)
         |Indentation.Outdent [60..60)
         |Ident(&&) [69..71)
         |Ident(w) [72..73)
         |RightParen [73..74)
         |LF [74..75)
         |Ident(h) [79..80)
         |Indentation.Outdent [80..80)
         |Indentation.Outdent [80..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) b else foo {
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
            tif(
              tname("a"),
              tname("b"),
              tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with the condition in parentheses, a deeper `then` body in parentheses on the next line, `else` on that line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if (a)
         |        (x) else y)
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
         |LeftParen [29..30)
         |Ident(a) [30..31)
         |RightParen [31..32)
         |Indentation.Indent [32..32)
         |LeftParen [41..42)
         |Ident(x) [42..43)
         |RightParen [43..44)
         |Indentation.Outdent [44..44)
         |KwElse [45..49)
         |Ident(y) [50..51)
         |RightParen [51..52)
         |LF [52..53)
         |Ident(h) [57..58)
         |Indentation.Outdent [58..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) x else y)
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
        blk(tapply(tname("g"), tif(tname("a"), tname("x"), tname("y"))), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with the condition in parentheses, a deeper `then` body on the next line, `else` on that line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if (a)
         |        x else y)
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
         |LeftParen [29..30)
         |Ident(a) [30..31)
         |RightParen [31..32)
         |Indentation.Indent [32..32)
         |Ident(x) [41..42)
         |Indentation.Outdent [42..42)
         |KwElse [43..47)
         |Ident(y) [48..49)
         |RightParen [49..50)
         |LF [50..51)
         |Ident(h) [55..56)
         |Indentation.Outdent [56..56)
         |Indentation.Outdent [56..56)
         |EOF [57..57)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) x else y)
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
        blk(tapply(tname("g"), tif(tname("a"), tname("x"), tname("y"))), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with the condition in parentheses, the `then` body on the next line at the column of the `if`, `else` on that line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if (a)
         |      x else y)
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
         |LeftParen [29..30)
         |Ident(a) [30..31)
         |RightParen [31..32)
         |Indentation.Indent [32..32)
         |Ident(x) [39..40)
         |Indentation.Outdent [40..40)
         |KwElse [41..45)
         |Ident(y) [46..47)
         |RightParen [47..48)
         |LF [48..49)
         |Ident(h) [53..54)
         |Indentation.Outdent [54..54)
         |Indentation.Outdent [54..54)
         |EOF [55..55)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(if (a) x else y)
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
        blk(tapply(tname("g"), tif(tname("a"), tname("x"), tname("y"))), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` within parentheses with an inline `then` body, a deeper leading `+` on the next line, `else` on that line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    g(if a then b
         |        + c else y)
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
         |Ident(b) [36..37)
         |InfixLF [37..38)
         |Ident(+) [46..47)
         |Ident(c) [48..49)
         |KwElse [50..54)
         |Ident(y) [55..56)
         |RightParen [56..57)
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
         |    g(if (a) b + c else y)
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
          tapply(tname("g"), tif(tname("a"), tinfix(tname("b"), "+", tname("c")), tname("y"))),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`if` with a colon argument in its `then` body, the argument on the next line, a blank line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a then foo:
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
         |Ident(foo) [34..37)
         |Colon [37..38)
         |Indentation.Indent [38..38)
         |Ident(z) [47..48)
         |Indentation.Outdent [49..49)
         |LFLF [49..50)
         |Ident(w) [54..55)
         |Indentation.Outdent [55..55)
         |Indentation.Outdent [55..55)
         |EOF [56..56)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a) foo {
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
        blk(tif(tname("a"), tapply(tname("foo"), blk(tname("z")))), tname("w")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("class parents: a colon argument within parentheses, the argument on the next line, `)` and `derives` on the next line") {
    val code =
      """|object A:
         |  class C extends B(foo:
         |      z
         |    ) derives D
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwClass [12..17)
         |Ident(C) [18..19)
         |KwExtends [20..27)
         |Ident(B) [28..29)
         |LeftParen [29..30)
         |Ident(foo) [30..33)
         |Colon [33..34)
         |Indentation.Indent [34..34)
         |Ident(z) [41..42)
         |Indentation.Outdent [42..42)
         |RightParen [47..48)
         |Ident(derives) [49..56)
         |Ident(D) [57..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  class C extends B(foo {
         |    z
         |  }) derives D
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Class(
        Nil,
        pname("C"),
        Nil,
        ctor,
        Template(
          None,
          List(init(pname("B"), Term.ArgClause(List(tapply(tname("foo"), blk(tname("z"))))))),
          Template.Body(None, Nil),
          List(pname("D")),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("a colon argument, the argument on the next line, a blank line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    foo:
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
         |Ident(foo) [24..27)
         |Colon [27..28)
         |Indentation.Indent [28..28)
         |Ident(z) [35..36)
         |Indentation.Outdent [37..37)
         |LFLF [37..38)
         |Ident(w) [42..43)
         |Indentation.Outdent [43..43)
         |Indentation.Outdent [43..43)
         |EOF [44..44)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    foo {
         |      z
         |    }
         |    w
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(Nil, tname("f"), Nil, None, blk(tapply(tname("foo"), blk(tname("z"))), tname("w")))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("a colon argument, the argument on the next line with a comment, next statement") {
    val code =
      """|object A:
         |  def f =
         |    foo:
         |      z // c
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
         |Ident(foo) [24..27)
         |Colon [27..28)
         |Indentation.Indent [28..28)
         |Ident(z) [35..36)
         |Indentation.Outdent [41..41)
         |Ident(w) [46..47)
         |Indentation.Outdent [47..47)
         |Indentation.Outdent [47..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    foo {
         |      z // c
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
        blk(tapply(tname("foo"), blk(tnameComments("z")()("// c"))), tname("w")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("lambda within parentheses with `if`, `&&` and a colon argument, the argument on the next line, a blank line, leading `&&`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f = g(x => if a && foo:
         |        z
         |
         |      && w then 1 else 2)
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
         |Ident(x) [22..23)
         |RightArrow [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |Ident(&&) [32..34)
         |Ident(foo) [35..38)
         |Colon [38..39)
         |Indentation.Indent [39..39)
         |Ident(z) [48..49)
         |Indentation.Outdent [50..50)
         |InfixLF [50..51)
         |Ident(&&) [57..59)
         |Ident(w) [60..61)
         |KwThen [62..66)
         |Constant.Int(1) [67..68)
         |KwElse [69..73)
         |Constant.Int(2) [74..75)
         |RightParen [75..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = g(x => if (a && foo {
         |    z
         |  } && w) 1 else 2)
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
          tfunc(tparam("x"))(tif(
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            int(1),
            int(2),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("condition in parentheses with `try` and a colon argument, the argument on the next line, a blank line, leading `&&`, `)` and both bodies at the column of the `if`") {
    val code =
      """|object A:
         |  def f =
         |    if (try foo:
         |        z
         |
         |    && w) c else d
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
         |KwTry [28..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(z) [45..46)
         |Indentation.Outdent [47..47)
         |InfixLF [47..48)
         |Ident(&&) [52..54)
         |Ident(w) [55..56)
         |RightParen [56..57)
         |Ident(c) [58..59)
         |KwElse [60..64)
         |Ident(d) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (try foo {
         |    z
         |  } && w) c else d
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
          Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("case guard in parentheses: a colon argument, the argument on the next line, a blank line, leading `&&`, `)` and `=>` on the next line, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case y if (foo:
         |        z
         |
         |      && w) => 1
         |    case _ => 2
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
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Ident(y) [37..38)
         |KwIf [39..41)
         |LeftParen [42..43)
         |Ident(foo) [43..46)
         |Colon [46..47)
         |Indentation.Indent [47..47)
         |Ident(z) [56..57)
         |Indentation.Outdent [58..58)
         |Ident(&&) [65..67)
         |Ident(w) [68..69)
         |RightParen [69..70)
         |RightArrow [71..73)
         |Constant.Int(1) [74..75)
         |LF [75..76)
         |KwCase [80..84)
         |Underscore [85..86)
         |RightArrow [87..89)
         |Constant.Int(2) [90..91)
         |Indentation.Outdent [91..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case y if foo {
         |      z
         |    } && w => 1
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
        Nil,
        None,
        tmatch(
          tname("x"),
          Case(
            patvar("y"),
            Some(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w"))),
            int(1),
          ),
          Case(patwildcard, None, int(2)),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`match` with a colon argument in a case body on the line of the `case`, the argument on the next line, a blank line, next case, next definition") {
    val code =
      """|object A:
         |  def f = x match
         |    case 1 => foo:
         |        z
         |
         |    case 2 => b
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
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Constant.Int(1) [37..38)
         |RightArrow [39..41)
         |Ident(foo) [42..45)
         |Colon [45..46)
         |Indentation.Indent [46..46)
         |Ident(z) [55..56)
         |Indentation.Outdent [57..57)
         |LFLF [57..58)
         |KwCase [62..66)
         |Constant.Int(2) [67..68)
         |RightArrow [69..71)
         |Ident(b) [72..73)
         |Indentation.Outdent [73..73)
         |LF [73..74)
         |KwDef [76..79)
         |Ident(g) [80..81)
         |Equals [82..83)
         |Constant.Int(1) [84..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      foo {
         |        z
         |      }
         |    case 2 =>
         |      b
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
          tmatch(
            tname("x"),
            Case(int(1), None, tapply(tname("foo"), blk(tname("z")))),
            Case(int(2), None, tname("b")),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`for` with a guard that holds a colon argument, the argument on the next line, a blank line, next generator, `do` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    for x <- xs
         |        if foo:
         |          z
         |
         |        y <- ys
         |    do g(x, y)
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
         |LF [35..36)
         |KwIf [44..46)
         |Ident(foo) [47..50)
         |Colon [50..51)
         |Indentation.Indent [51..51)
         |Ident(z) [62..63)
         |Indentation.Outdent [64..64)
         |LFLF [64..65)
         |Ident(y) [73..74)
         |LeftArrow [75..77)
         |Ident(ys) [78..80)
         |LF [80..81)
         |KwDo [85..87)
         |Ident(g) [88..89)
         |LeftParen [89..90)
         |Ident(x) [90..91)
         |Comma [91..92)
         |Ident(y) [93..94)
         |RightParen [94..95)
         |Indentation.Outdent [95..95)
         |Indentation.Outdent [95..95)
         |EOF [96..96)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = for (x <- xs; if foo {
         |    z
         |  }; y <- ys) g(x, y)
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
        Term.For(
          Term.EnumeratorsBlock(List(
            Enumerator.Generator(patvar("x"), tname("xs")),
            Enumerator.Guard(tapply(tname("foo"), blk(tname("z")))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          )),
          tapply(tname("g"), tname("x"), tname("y")),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`for` with braces and a generator that holds a colon argument, the argument on the next line, a blank line, next generator") {
    val code =
      """|object A:
         |  def f =
         |    for {
         |      x <- foo:
         |        z
         |
         |      y <- ys
         |    } yield x
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
         |LeftBrace [28..29)
         |Ident(x) [36..37)
         |LeftArrow [38..40)
         |Ident(foo) [41..44)
         |Colon [44..45)
         |Indentation.Indent [45..45)
         |Ident(z) [54..55)
         |Indentation.Outdent [56..56)
         |LFLF [56..57)
         |Ident(y) [63..64)
         |LeftArrow [65..67)
         |Ident(ys) [68..70)
         |LF [70..71)
         |RightBrace [75..76)
         |KwYield [77..82)
         |Ident(x) [83..84)
         |Indentation.Outdent [84..84)
         |Indentation.Outdent [84..84)
         |EOF [85..85)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = for (x <- foo {
         |    z
         |  }; y <- ys) yield x
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
          Term.EnumeratorsBlock(List(
            Enumerator.Generator(patvar("x"), tapply(tname("foo"), blk(tname("z")))),
            Enumerator.Generator(patvar("y"), tname("ys")),
          )),
          tname("x"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("class body: nested colon arguments, the arguments on the next lines, a blank line, next definition") {
    val code =
      """|class A:
         |  def f = foo:
         |      bar:
         |        z
         |
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwClass [0..5)
         |Ident(A) [6..7)
         |Colon [7..8)
         |Indentation.Indent [8..8)
         |KwDef [11..14)
         |Ident(f) [15..16)
         |Equals [17..18)
         |Ident(foo) [19..22)
         |Colon [22..23)
         |Indentation.Indent [23..23)
         |Ident(bar) [30..33)
         |Colon [33..34)
         |Indentation.Indent [34..34)
         |Ident(z) [43..44)
         |Indentation.Outdent [45..45)
         |Indentation.Outdent [45..45)
         |LFLF [45..46)
         |KwDef [48..51)
         |Ident(g) [52..53)
         |Equals [54..55)
         |Constant.Int(1) [56..57)
         |Indentation.Outdent [57..57)
         |EOF [58..58)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|class A {
         |  def f = foo {
         |    bar {
         |      z
         |    }
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      ctor,
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tapply(tname("foo"), blk(tapply(tname("bar"), blk(tname("z"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`new` with a template body: a colon argument, the argument on the next line, a blank line, next definition in the body, next definition") {
    val code =
      """|object A:
         |  val a = new B:
         |    def f = foo:
         |        z
         |
         |    def g = 1
         |  def h = 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwVal [12..15)
         |Ident(a) [16..17)
         |Equals [18..19)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |Colon [25..26)
         |Indentation.Indent [26..26)
         |KwDef [31..34)
         |Ident(f) [35..36)
         |Equals [37..38)
         |Ident(foo) [39..42)
         |Colon [42..43)
         |Indentation.Indent [43..43)
         |Ident(z) [52..53)
         |Indentation.Outdent [54..54)
         |LFLF [54..55)
         |KwDef [59..62)
         |Ident(g) [63..64)
         |Equals [65..66)
         |Constant.Int(1) [67..68)
         |Indentation.Outdent [68..68)
         |LF [68..69)
         |KwDef [71..74)
         |Ident(h) [75..76)
         |Equals [77..78)
         |Constant.Int(2) [79..80)
         |Indentation.Outdent [80..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  val a = new B {
         |    def f = foo {
         |      z
         |    }
         |    def g = 1
         |  }
         |  def h = 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(
          Nil,
          List(patvar("a")),
          None,
          Term.NewAnonymous(tpl(
            List(init(pname("B"))),
            List(
              Defn.Def(Nil, tname("f"), Nil, None, tapply(tname("foo"), blk(tname("z")))),
              Defn.Def(Nil, tname("g"), Nil, None, int(1)),
            ),
          )),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, int(2)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("default parameter value: a colon argument, the argument on the next line, a blank line, next parameter after a leading comma") {
    val code =
      """|object A:
         |  def f(x: Int = foo:
         |      z
         |
         |    , y: Int) = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |LeftParen [17..18)
         |Ident(x) [18..19)
         |Colon [19..20)
         |Ident(Int) [21..24)
         |Equals [25..26)
         |Ident(foo) [27..30)
         |Colon [30..31)
         |Ident(z) [38..39)
         |Comma [45..46)
         |Ident(y) [47..48)
         |Colon [48..49)
         |Ident(Int) [50..53)
         |RightParen [53..54)
         |Equals [55..56)
         |Constant.Int(1) [57..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A { def f(x: Int = foo: z, y: Int) = 1 }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        List(pcg(tpc(
          Term.Param(Nil, tname("x"), Some(pname("Int")), Some(Term.Ascribe(tname("foo"), pname("z")))),
          Term.Param(Nil, tname("y"), Some(pname("Int")), None),
        ))),
        None,
        int(1),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("a colon argument within parentheses after type arguments, the argument on the next line, a blank line, next argument after a leading comma") {
    val code =
      """|object A:
         |  def f = g[Int](foo:
         |      z
         |
         |    , 2)
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
         |LeftBracket [21..22)
         |Ident(Int) [22..25)
         |RightBracket [25..26)
         |LeftParen [26..27)
         |Ident(foo) [27..30)
         |Colon [30..31)
         |Indentation.Indent [31..31)
         |Ident(z) [38..39)
         |Indentation.Outdent [40..40)
         |Comma [45..46)
         |Constant.Int(2) [47..48)
         |RightParen [48..49)
         |Indentation.Outdent [49..49)
         |EOF [50..50)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = g[Int](foo {
         |    z
         |  }, 2)
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
        tapply(tapplytype(tname("g"), pname("Int")), tapply(tname("foo"), blk(tname("z"))), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`given` with a body: a colon argument, the argument on the next line, a blank line, next definition") {
    val code =
      """|object A:
         |  given B with
         |    def f = foo:
         |        z
         |
         |    def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwGiven [12..17)
         |Ident(B) [18..19)
         |KwWith [20..24)
         |Indentation.Indent [24..24)
         |KwDef [29..32)
         |Ident(f) [33..34)
         |Equals [35..36)
         |Ident(foo) [37..40)
         |Colon [40..41)
         |Indentation.Indent [41..41)
         |Ident(z) [50..51)
         |Indentation.Outdent [52..52)
         |LFLF [52..53)
         |KwDef [57..60)
         |Ident(g) [61..62)
         |Equals [63..64)
         |Constant.Int(1) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  given B with {
         |    def f = foo {
         |      z
         |    }
         |    def g = 1
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Given(
        Nil,
        anon,
        Nil,
        tpl(
          List(init(pname("B"))),
          List(
            Defn.Def(Nil, tname("f"), Nil, None, tapply(tname("foo"), blk(tname("z")))),
            Defn.Def(Nil, tname("g"), Nil, None, int(1)),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon lambda within parentheses, its body on the next line, a blank line, leading `+` and `)` on the next line, next definition") {
    val code =
      """|object A:
         |  def f = g(xs.map: x =>
         |        x
         |
         |      + b)
         |  def h = 1
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
         |Ident(xs) [22..24)
         |Dot [24..25)
         |Ident(map) [25..28)
         |Colon [28..29)
         |Ident(x) [30..31)
         |RightArrow [32..34)
         |Indentation.Indent [34..34)
         |Ident(x) [43..44)
         |Indentation.Outdent [45..45)
         |Ident(+) [52..53)
         |Ident(b) [54..55)
         |RightParen [55..56)
         |LF [56..57)
         |KwDef [59..62)
         |Ident(h) [63..64)
         |Equals [65..66)
         |Constant.Int(1) [67..68)
         |Indentation.Outdent [68..68)
         |EOF [69..69)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = g(xs.map {
         |    x => x
         |  } + b)
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
            tname("g"),
            tinfix(
              tapply(tselect(tname("xs"), "map"), blk(tfunc(tparam("x"))(tname("x")))),
              "+",
              tname("b"),
            ),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued statement, the argument in a `then` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    if c then a && foo:
         |        z
         |      && w
         |    else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
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
        blk(
          Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
          Term.If(
            tname("c"),
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            tname("d"),
            Nil,
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued statement, the argument in a `try` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    try a && foo:
         |        z
         |      && w
         |    finally d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val v = a + b
         |    try a && foo {
         |      z
         |    } && w finally d
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
          Term.Try(
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            Nil,
            Some(tname("d")),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after an indented condition: a continued statement, a condition of two statements, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    if
         |        val q = 1
         |        q
         |      && w
         |    then c else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val v = a + b
         |    if ({
         |      val q = 1
         |      q
         |    } && w) c else d
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
            tinfix(blk(Defn.Val(Nil, List(patvar("q")), None, int(1)), tname("q")), "&&", tname("w")),
            tname("c"),
            tname("d"),
            Nil,
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after an indented `then` body: a continued statement, a body of two statements, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    if c then
         |        val q = 1
         |        q
         |      && w
         |    else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val v = a + b
         |    if (c) {
         |      val q = 1
         |      q
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
        blk(
          Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
          Term.If(
            tname("c"),
            tinfix(blk(Defn.Val(Nil, List(patvar("q")), None, int(1)), tname("q")), "&&", tname("w")),
            tname("d"),
            Nil,
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued `val`, the argument in a `then` body of the next `def`, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  val e = 1
         |    + q
         |  def h = if c then a && foo:
         |      z
         |    && w
         |  else d
         |""".stripMargin
    val layout =
      """|object A {
         |  val e = 1 + q
         |  def h = if (c) a && foo {
         |    z
         |  } && w else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(Nil, List(patvar("e")), None, tinfix(int(1), "+", tname("q"))),
        Defn.Def(
          Nil,
          tname("h"),
          Nil,
          None,
          Term.If(
            tname("c"),
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            tname("d"),
            Nil,
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued `catch` case on the line of the `try`, the argument in a `then` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val r = try a catch case _ => b
         |      + c
         |    if c then a && foo:
         |        z
         |      && w
         |    else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val r = try a catch {
         |      case _ =>
         |        b + c
         |    }
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
          tif(
            tname("c"),
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            tname("d"),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued `catch` case on the line of the `try`, the argument in a `try` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    val r = try a catch case _ => b
         |      + c
         |    try a && foo:
         |        z
         |      && w
         |    finally d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val r = try a catch {
         |      case _ =>
         |        b + c
         |    }
         |    try a && foo {
         |      z
         |    } && w finally d
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
          Term.Try(
            tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            Nil,
            Some(tname("d")),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued statement in a `case` body in braces, the argument in a `then` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f = xs.map {
         |    case 1 =>
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
         |  def f = xs.map {
         |    case 1 =>
         |      val v = a + b
         |      if (c) a && foo {
         |        z
         |      } && w else d
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
          Term.PartialFunction(List(Case(
            int(1),
            None,
            blk(
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
            ),
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a continued statement in a `case` body in braces, the argument in a `try` body, `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f = xs.map {
         |    case 1 =>
         |      val v = a
         |        + b
         |      try a && foo:
         |          z
         |        && w
         |      finally d
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    case 1 =>
         |      val v = a + b
         |      try a && foo {
         |        z
         |      } && w finally d
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
          Term.PartialFunction(List(Case(
            int(1),
            None,
            blk(
              Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
              Term.Try(
                tinfix(
                  tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))),
                  "&&",
                  tname("w"),
                ),
                Nil,
                Some(tname("d")),
              ),
            ),
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a `case` after a `case` on the line of the brace, the argument in a condition in its body, `&&` at the column of the cases") {
    val code =
      """|object A:
         |  def f = xs.map { case 0 => a
         |    case 1 => if x && foo:
         |        z
         |    && w
         |    then c else d
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    case 0 => a
         |    case 1 =>
         |      if (x && foo {
         |        z
         |      } && w) c else d
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
          tselect(tname("xs"), "map"),
          Term.PartialFunction(List(
            Case(int(0), None, tname("a")),
            Case(
              int(1),
              None,
              tif(
                tinfix(
                  tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tname("z")))),
                  "&&",
                  tname("w"),
                ),
                tname("c"),
                tname("d"),
              ),
            ),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a `catch` case after a case on the line of the brace, the argument in a condition in its body, `&&` at the column of the cases") {
    val code =
      """|object A:
         |  def f = try a catch { case _: E => b
         |    case _ => if x && foo:
         |        z
         |    && w
         |    then c else d
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = try a catch {
         |    case _: E =>
         |      b
         |    case _ =>
         |      if (x && foo {
         |        z
         |      } && w) c else d
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
        Term.Try(
          tname("a"),
          List(
            Case(Pat.Typed(Pat.Wildcard(), pname("E")), None, tname("b")),
            Case(
              Pat.Wildcard(),
              None,
              tif(
                tinfix(
                  tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tname("z")))),
                  "&&",
                  tname("w"),
                ),
                tname("c"),
                tname("d"),
              ),
            ),
          ),
          None,
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a generator after one on the line of the brace of a `for`, the argument in a condition, `&&` at the column of the generator") {
    val code =
      """|object A:
         |  def f =
         |    for { a <- as
         |          y <- if x && foo:
         |              z
         |          && w
         |          then ys else zs
         |    } yield y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (a <- as; y <- if (x && foo {
         |    z
         |  } && w) ys else zs) yield y
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
            Enumerator.Generator(patvar("a"), tname("as")),
            Enumerator.Generator(
              patvar("y"),
              tif(
                tinfix(
                  tinfix(tname("x"), tname("&&"), tapply(tname("foo"), blk(tname("z")))),
                  tname("&&"),
                  tname("w"),
                ),
                tname("ys"),
                tname("zs"),
              ),
            ),
          ),
          tname("y"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with a deeper next line, the argument in a condition, `&&` at the column of that line") {
    val code =
      """|object A:
         |  self =>
         |    def g = 1
         |  def f = if c && foo:
         |        z
         |    && w
         |  then 1 else 2
         |""".stripMargin
    val layout =
      """|object A { self =>
         |  def g = 1
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with a deeper next line, a continuation line between the columns of the self type and that line, the argument in a condition in parentheses, `&&` deeper than that line") {
    val code =
      """|object A:
         |  self =>
         |      def g = 1
         |  def f = 1 +
         |   (if c && foo:
         |        z
         |       && w
         |   then 1 else 2)
         |""".stripMargin
    val layout =
      """|object A { self =>
         |  def g = 1
         |  def f = 1 + (if (c && foo {
         |    z && w
         |  }) 1 else 2)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tinfix(
            int(1),
            "+",
            tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            ),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with a deeper next line, a nested class, the argument in a condition in its body, `&&` between the columns of the class body and that line") {
    val code =
      """|object A:
         |  self =>
         |        def g = 1
         |  class B:
         |    def f = if c && foo:
         |          z
         |       && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|object A { self =>
         |  def g = 1
         |  class B {
         |    def f = if (c && foo {
         |      z && w
         |    }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Class(
          Nil,
          pname("B"),
          Nil,
          ctor,
          tpl(Defn.Def(
            Nil,
            tname("f"),
            Nil,
            None,
            tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            ),
          )),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with code after it on its line, the argument in a condition, `&&` deeper than the self type") {
    val code =
      """|object A:
         |  self => def g = 1
         |  def f = if c && foo:
         |        z
         |    && w
         |  then 1 else 2
         |""".stripMargin
    val layout =
      """|object A { self =>
         |  def g = 1
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with a next line at its column, the argument in a condition, `&&` deeper than the self type") {
    val code =
      """|object A:
         |  self =>
         |  def g = 1
         |  def f = if c && foo:
         |        z
         |    && w
         |  then 1 else 2
         |""".stripMargin
    val layout =
      """|object A { self =>
         |  def g = 1
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a self type with a deeper next line in a `given` body in braces after `with`, the argument in a condition, `&&` deeper than that line") {
    val code =
      """|given A with {
         |  self =>
         |    def g = 1
         |  def f = if c && foo:
         |        z
         |      && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|given A with { self =>
         |  def g = 1
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Given(
      Nil,
      anon,
      Nil,
      tpl(
        List(init(pname("A"))),
        self(tname("self")),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda in braces with a parameter on the line after the brace and a deeper next line, the argument in a condition on a shallower line, `&&` at the column of that line") {
    val code =
      """|object O:
         |  def h = xs.foreach {
         |    x =>
         |      g(x)
         |    if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |  }
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = xs.foreach {
         |    x => g(x)
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
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
          blk(
            tfunc(tparam("x"))(tapply(tname("g"), tname("x"))),
            tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            ),
          ),
        ),
      )),
    )))
    parseAndCheckTree[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `x: Int =>` on the line after a brace and a deeper next line, the argument in a condition on a shallower line, `&&` at the column of that line") {
    val code =
      """|object O:
         |  def h = xs.foreach {
         |    x: Int =>
         |      g(x)
         |    if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |  }
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = xs.foreach {
         |    x {
         |      Int => g(x)
         |    }
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
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
          blk(
            tapply(tname("x"), blk(tfunc(tparam("Int"))(tapply(tname("g"), tname("x"))))),
            tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            ),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda in braces with a parameter on the line after the brace, the argument in a condition in its body on the next line, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h = xs.foreach {
         |    x =>
         |      if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |  }
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = xs.foreach {
         |    x => if (c && foo {
         |      z && w
         |    }) 1 else 2
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
          blk(tfunc(tparam("x"))(tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon block on the first line of a body, the argument in a condition in that block, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h =
         |    boundary:
         |      if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = boundary {
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
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
          tname("boundary"),
          blk(tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon block on the first line of an object body, the argument in a condition in a later definition, `&&` deeper than the definition") {
    val code =
      """|object O:
         |  locally:
         |    g
         |  def f = if c && foo:
         |        z
         |    && w
         |  then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  locally {
         |    g
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("locally"), blk(tname("g"))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon lambda on the first line of a body and a deeper next line, the argument in a condition on a shallower line, `&&` between the columns of the condition and that line") {
    val code =
      """|object O:
         |  def h =
         |    run: x =>
         |        g(x)
         |    if c && foo:
         |          z
         |       && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = {
         |    run {
         |      x => g(x)
         |    }
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
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
        blk(
          tapply(tname("run"), blk(tfunc(tparam("x"))(tapply(tname("g"), tname("x"))))),
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon lambda with parameters in parentheses on the first line of a body and a deeper next line, the argument in a condition on a shallower line, `&&` between the columns of the condition and that line") {
    val code =
      """|object O:
         |  def h =
         |    run: (x, y) =>
         |        g(x)
         |    if c && foo:
         |          z
         |       && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = {
         |    run {
         |      (x, y) => g(x)
         |    }
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
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
        blk(
          tapply(tname("run"), blk(tfunc(tparam("x"), tparam("y"))(tapply(tname("g"), tname("x"))))),
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon lambda on the first line of a body and a deeper next line, the argument in a condition in a colon block on a shallower line, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h =
         |    run: x =>
         |        g(x)
         |    locally:
         |      if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = {
         |    run {
         |      x => g(x)
         |    }
         |    locally {
         |      if (c && foo {
         |        z && w
         |      }) 1 else 2
         |    }
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
        blk(
          tapply(tname("run"), blk(tfunc(tparam("x"))(tapply(tname("g"), tname("x"))))),
          tapply(
            tname("locally"),
            blk(tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            )),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a context colon lambda on the first line of a body and a deeper next line, the argument in a condition on a shallower line, `&&` between the columns of the condition and that line") {
    val code =
      """|object O:
         |  def h =
         |    run: x ?=>
         |        g(x)
         |    if c && foo:
         |          z
         |       && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = {
         |    run {
         |      x ?=> g(x)
         |    }
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
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
        blk(
          tapply(
            tname("run"),
            blk(Term.ContextFunction(List(tparam("x")), tapply(tname("g"), tname("x")))),
          ),
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon block and a blank line on the first lines of a body, the argument in a condition in that block, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h =
         |    boundary:
         |
         |      if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = boundary {
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
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
          tname("boundary"),
          blk(tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of a class body in braces, a lambda on a deeper next line, the argument in a condition, `&&` between the columns of the name and the lambda") {
    val code =
      """|class A {
         |  self:
         |    B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|class A {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      ctor,
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: an ascription on the first line of a body, the argument in a condition on the next line, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h =
         |    x: Int
         |    if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = {
         |    x: Int
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
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
        blk(
          Term.Ascribe(tname("x"), pname("Int")),
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a colon block with the colon on the next line, the argument in a condition in that block, `&&` deeper than the condition") {
    val code =
      """|object O:
         |  def h =
         |    boundary
         |      :
         |      if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |""".stripMargin
    val error =
      """|<input>:4: error: expected fewer-braces method body
         |      :
         |      ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces, a type and an arrow at the column of the name, the argument in a condition, `&&` between the columns of the name and the next line") {
    val code =
      """|object O {
         |  self:
         |  B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces, a form feed, a type and an arrow on the next line deeper than the name, the argument in a condition, `&&` between the columns of the name and the next line") {
    val code =
      s"""|object O {
          |  self:
          |\f    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name, a colon, a form feed, a type and an arrow on the first line of an object body in braces, the argument in a condition, `&&` between the columns of the name and the next line") {
    val code =
      s"""|object O {
          |  self:\f  B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name, a colon, a carriage return, a type deeper than the name and an arrow on the first line of an object body in braces, the argument in a condition, `&&` between the columns of the name and the next line") {
    val code =
      s"""|object O {
          |  self:\r    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces, a blank line, a type and an arrow at column 0, the argument in a condition, `&&` between the columns of the name and the next line") {
    val code =
      """|object O {
         |  self:
         |
         |B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val error =
      """|<input>:2: error: `identifier` expected but `\n\n` found
         |  self:
         |       ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces indented with tabs, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |\t\tself:
          |\t\t\t\tB =>
          |\t\t\t\t\t\tdef g = 1
          |\t\tdef f = if c && foo:
          |\t\t\t\t\t\t\t\tz
          |\t\t\t\t\t&& w
          |\t\tthen 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name, a colon, a carriage return and a line feed at the end of the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |  self:\r
          |    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces, a type and an arrow on the next line indented as the name and a tab, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |  self:
          |  \tB =>
          |  \t\t\tdef g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test {
    "leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces indented with a space and a tab, a type and an arrow on the next line indented with a space and two tabs, the argument in a condition, `&&` between the columns of the type and the line after it"
  } {
    val code =
      s"""|object O {
          | \tself:
          | \t\tB =>
          | \t\t\t\tdef g = 1
          | \tdef f = if c && foo:
          | \t\t\t\t\t\t\tz
          | \t\t\t\t&& w
          | \tthen 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test {
    "leading infix after a colon argument: a name and a colon at the end of the first line of an object body in braces indented with a space and a tab, a type and an arrow on the next line indented with a space, a tab and a space, the argument in a condition, `&&` between the columns of the type and the line after it"
  } {
    val code =
      s"""|object O {
          | \tself:
          | \t B =>
          | \t   def g = 1
          | \tdef f = if c && foo:
          | \t\t\t\t\t\t\tz
          | \t\t\t\t&& w
          | \tthen 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a form feed, a name and a colon on the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |\f  self:
          |    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a carriage return, a name and a colon on the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |\r  self:
          |    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: two spaces, a carriage return, a name and a colon on the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |  \r  self:
          |    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: two spaces, a form feed, a name and a colon on the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      s"""|object O {
          |  \f  self:
          |    B =>
          |      def g = 1
          |  def f = if c && foo:
          |        z
          |     && w
          |  then 1 else 2
          |}
          |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a comment, a name and a colon on the first line of an object body in braces, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      """|object O {
         |  /* c */ self:
         |    B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|object O {
         |  /* c */ self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1))))))
          .toBuilder.begComment(Seq("/* c */")).result(),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a multi-line comment after the brace of an object body, a name and a colon after it on its last line, a type and an arrow on a deeper next line, the argument in a condition, `&&` between the columns of the type and the line after it") {
    val code =
      """|object O { /* c
         |  */ self:
         |    B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|object O {
         |  /* c
         |  */ self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1))))))
          .toBuilder.begComment(Seq(
            """|/* c
               |  */""".stripMargin,
          )).result(),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test {
    "leading infix after a colon argument: a name on the first line of an object body in braces, a colon on a deeper next line, a type and an arrow on the line after it, between the columns of the name and the colon, the argument in a condition, `&&` between the columns of the type and the line after it"
  } {
    val code =
      """|object O {
         |  self
         |      :
         |    B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |     && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|object O {
         |  self {
         |    B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("B"))(blk(Defn.Def(Nil, tname("g"), Nil, None, int(1)))))),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end, `try` and `catch` on a deeper next line, its case on a shallower line, the argument in a condition in the case, `&&` at the column of the `try`") {
    val code =
      """|def h: Int =
         |  throw
         |      try g catch
         |    case _ => if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|def h: Int = throw try g catch {
         |  case _ =>
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      Term.Throw(Term.Try(
        tname("g"),
        List(Case(
          Pat.Wildcard(),
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        )),
        None,
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end, `try` and an argument list on a deeper next line, the list closing with `catch` on a shallower line, its case at that column, the argument in a condition in the case, `&&` at the column of the `try`") {
    val code =
      """|def h: Int =
         |  throw
         |      try g(a,
         |    b) catch
         |    case _ => if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|def h: Int = throw try g(a, b) catch {
         |  case _ =>
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      Term.Throw(Term.Try(
        tapply(tname("g"), tname("a"), tname("b")),
        List(Case(
          Pat.Wildcard(),
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        )),
        None,
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end, `while` and a condition on a deeper next line, the condition continued with `do` on a shallower line, the argument in a condition at that column, `&&` at the column of the `while`") {
    val code =
      """|def h: Unit =
         |  throw
         |      while a &&
         |    b do
         |    if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|def h: Unit = throw while (a && b) if (c && foo {
         |  z && w
         |}) 1 else 2
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Unit")),
      Term.Throw(Term.While(
        tinfix(tname("a"), "&&", tname("b")),
        tif(
          tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          int(1),
          int(2),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end in braces, an argument list on a deeper next line, the list closing with `match` and a case in braces on a shallower line, the argument in a condition in the case, `&&` between the columns of that line and the list") {
    val code =
      """|def h: Int = {
         |  throw
         |      g(a,
         |    b) match { case _ => if c && foo:
         |          z
         |     && w
         |    then 1 else 2
         |    }
         |}
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  throw g(a, b) match {
         |    case _ =>
         |      if (c && foo {
         |        z && w
         |      }) 1 else 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Term.Throw(Term.Match(
        tapply(tname("g"), tname("a"), tname("b")),
        List(Case(
          Pat.Wildcard(),
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        )),
      ))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end in braces, `if` and a condition on a deeper next line, `then` and the argument in a condition on a shallower line, `&&` between the columns of `then` and the `if`") {
    val code =
      """|def h: Int = {
         |  throw
         |      if a
         |  then if c && foo:
         |         z
         |    && w
         |       then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  throw if (a) if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Term.Throw(tif(
        tname("a"),
        tif(
          tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          int(1),
          int(2),
        ),
      ))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test {
    "leading infix after a colon argument: a name and a colon at the end of the first line of a class body in braces, a multi-line comment on the next line that ends on the line of a type and an arrow, the argument in a condition, `&&` between the columns of the type and the line after it, with LF and with CR LF"
  } {
    val code =
      """|class O {
         |  self:
         |   /*
         |*/ B =>
         |      def g = 1
         |  def f = if c && foo:
         |        z
         |      && w
         |  then 1 else 2
         |}
         |""".stripMargin
    val layoutCRLF =
      """|class O {
         |  self {
         |    /*
         |*/ B => {
         |      def g = 1
         |    }
         |  }
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Class(
      Nil,
      pname("O"),
      Nil,
      EmptyCtor(),
      tpl(
        self(tname("self"), pname("B").toBuilder.begComment(Seq("/*\n*/")).result()),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    val parsed = source(code)
    assertNotEquals(parsed.structure, tree.structure)
    assertNoDiff(parsed.reprint, layoutCRLF)
    assertNoDiff(source(code.replace("\n", "\r\n")).reprint.replace("\r\n", "\n"), layoutCRLF)
  }

  test("leading infix after a colon argument: a self type on the line of a trait's brace, the argument in a condition in a method body, `&&` between the columns of the condition and the argument") {
    val code =
      """|trait T { self =>
         |  def f =
         |    if c && foo:
         |          z
         |      && w
         |    then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|trait T { self =>
         |  def f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Trait(
      Nil,
      pname("T"),
      Nil,
      EmptyCtor(),
      tpl(
        self(tname("self")),
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda in braces with a parameter on the line of the brace, the argument in a condition in its body, `&&` between the columns of the condition and the argument") {
    val code =
      """|def h = xs.foreach { x =>
         |    val f = if c && foo:
         |        z
         |     && w
         |     then 1 else 2
         |    f
         |  }
         |""".stripMargin
    val layout =
      """|def h = xs.foreach { x =>
         |  val f = if (c && foo {
         |    z && w
         |  }) 1 else 2
         |  f
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      None,
      tapply(
        tselect("xs", "foreach"),
        blk(tfunc(tparam("x"))(blk(
          Defn.Val(
            Nil,
            List(patvar("f")),
            None,
            tif(
              tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
              int(1),
              int(2),
            ),
          ),
          tname("f"),
        ))),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a line that starts with `||` and the argument in a condition in parentheses, `&&` between the columns of that line and the argument") {
    val code =
      """|def h: Int =
         |  val v =
         |      a
         |    || (if c && foo:
         |              z
         |       && w
         |    then 1 else 2)
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  val v = a || (if (c && foo {
         |    z && w
         |  }) 1 else 2)
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Defn.Val(
        Nil,
        List(patvar("v")),
        None,
        tinfix(
          tname("a"),
          "||",
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end, `try` and an argument list on a deeper next line, the list closing with `catch` on a line shallower than the `throw`, the argument in a condition in its case, `&&` between the columns of the case and the `try`") {
    val code =
      """|def h: Int =
         |  throw
         |      try g(a,
         |  b) catch
         |    case _ => if c && foo:
         |                    z
         |     && w
         |    then 1 else 2
         |""".stripMargin
    val layout =
      """|def h: Int = throw try g(a, b) catch {
         |  case _ =>
         |    if (c && foo {
         |      z && w
         |    }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      Term.Throw(Term.Try(
        tapply(tname("g"), tname("a"), tname("b")),
        List(Case(
          Pat.Wildcard(),
          None,
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        )),
        None,
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end in braces, `if` and a condition on a deeper next line, the condition continued with `.b` and `then` on a shallower line, the argument in a condition after `then`, `&&` between the columns of that line and the `if`") {
    val code =
      """|def h: Int = {
         |  throw
         |      if a
         |    .b then if c && foo:
         |                  z
         |     && w
         |    then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  throw if (a.b) if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Term.Throw(Term.If(
        tselect("a", "b"),
        tif(
          tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          int(1),
          int(2),
        ),
        Lit.Unit(),
      ))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end in braces, `for` and an enumerator on a deeper next line, a second enumerator and `do` on a shallower line, the argument in a condition after `do`, `&&` between the columns of that line and the `for`") {
    val code =
      """|def h: Int = {
         |  throw
         |      for x <- xs
         |    y <- ys do if c && foo:
         |                     z
         |     && w
         |    then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  throw for (x <- xs; y <- ys) if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Term.Throw(Term.For(
        List(
          Enumerator.Generator(patvar("x"), tname("xs")),
          Enumerator.Generator(patvar("y"), tname("ys")),
        ),
        tif(
          tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          int(1),
          int(2),
        ),
      ))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end in braces, `for` and an enumerator on a deeper next line, a guard on a shallower line, `yield` and the argument in a condition at that column, `&&` between the columns of that line and the `for`") {
    val code =
      """|def h: Int = {
         |  throw
         |      for x <- xs
         |    if p
         |    yield if c && foo:
         |                z
         |     && w
         |    then 1 else 2
         |}
         |""".stripMargin
    val layout =
      """|def h: Int = {
         |  throw for (x <- xs; if p) yield if (c && foo {
         |    z && w
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      blk(Term.Throw(Term.ForYield(
        List(Enumerator.Generator(patvar("x"), tname("xs")), Enumerator.Guard(tname("p"))),
        tif(
          tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          int(1),
          int(2),
        ),
      ))),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda parameter at the end of the condition's line, its body on a deeper next line, `&&` four columns deeper than the `if`") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists: x =>
         |          x > 0
         |        && ys.isEmpty
         |    then a else b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.exists {
         |    x => x > 0 && ys.isEmpty
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
          tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("x"))(
              tinfix(tinfix(tname("x"), ">", int(0)), "&&", tselect("ys", "isEmpty")),
            )),
          ),
          tname("a"),
          tname("b"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a lambda parameter at the end of the condition's line, its body on a deeper next line, `&&` two columns deeper than the `if`") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists: x =>
         |          x > 0
         |      && ys.isEmpty
         |    then a else b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.exists {
         |    x => x > 0 && ys.isEmpty
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
          tapply(
            tselect("xs", "exists"),
            blk(tfunc(tparam("x"))(
              tinfix(tinfix(tname("x"), ">", int(0)), "&&", tselect("ys", "isEmpty")),
            )),
          ),
          tname("a"),
          tname("b"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a placeholder lambda on a deeper next line, `&&` four columns deeper than the `if`") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists:
         |          _ > 0
         |        && ys.isEmpty
         |    then a else b
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (xs.exists {
         |    _ > 0 && ys.isEmpty
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
          tapply(
            tselect("xs", "exists"),
            blk(Term.AnonymousFunction(
              tinfix(tinfix(Term.Placeholder(), ">", int(0)), "&&", tselect("ys", "isEmpty")),
            )),
          ),
          tname("a"),
          tname("b"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a `for` with its first generator on its line, a second generator on the next line, the argument in a condition after its arrow, `&&` deeper than that line") {
    val code =
      """|object A:
         |  def f =
         |    for x <- xs
         |        y <- if c && foo:
         |            z
         |          && w
         |        then a else b
         |    yield y
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = for (x <- xs; y <- if (c && foo {
         |    z
         |  } && w) a else b) yield y
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
            Enumerator.Generator(patvar("x"), tname("xs")),
            Enumerator.Generator(
              patvar("y"),
              tif(
                tinfix(
                  tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tname("z")))),
                  "&&",
                  tname("w"),
                ),
                tname("a"),
                tname("b"),
              ),
            ),
          ),
          tname("y"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: `throw` at a line end, `if` and two `} else if` in braces on a deeper next line, the argument in the last condition, `&&` deeper than the `if`") {
    val code =
      """|def h: Int =
         |  throw
         |      if (a) {
         |        x
         |      } else if (b) {
         |        y
         |      } else if c && foo:
         |            z
         |        && w
         |      then 1 else 2
         |""".stripMargin
    val layout =
      """|def h: Int = throw if (a) {
         |  x
         |} else if (b) {
         |  y
         |} else if (c && foo {
         |  z && w
         |}) 1 else 2
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("h"),
      Nil,
      Some(pname("Int")),
      Term.Throw(tif(
        tname("a"),
        blk(tname("x")),
        tif(
          tname("b"),
          blk(tname("y")),
          tif(
            tinfix(tname("c"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            int(1),
            int(2),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a condition that starts with `{`, `&&` between the columns of the `if` and the argument") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      if { a } && foo:
         |          z
         |        && w
         |      then 1 else 2
         |    r
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val r = if ({
         |      a
         |    } && foo {
         |      z && w
         |    }) 1 else 2
         |    r
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
            Term.If(
              tinfix(
                blk(tname("a")),
                "&&",
                tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
              ),
              int(1),
              int(2),
              Nil,
            ),
          ),
          tname("r"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("leading infix after a colon argument: a `while` condition that starts with `{`, `&&` between the columns of the `while` and the argument") {
    val code =
      """|object A:
         |  def f =
         |    while { a } && foo:
         |        z
         |      && w
         |    do g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while ({
         |    a
         |  } && foo {
         |    z && w
         |  }) g
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
          tinfix(blk(tname("a")), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("case guard: `if` at the end of a line, a colon argument on the next line, the argument on the line after, an expression at the column of the colon argument, `=>` on the next line") {
    val code =
      """|object A:
         |  def f = x match
         |    case y if
         |        foo:
         |          z
         |        w
         |      => c
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
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Ident(y) [37..38)
         |KwIf [39..41)
         |Ident(foo) [50..53)
         |Colon [53..54)
         |Indentation.Indent [54..54)
         |Ident(z) [65..66)
         |Indentation.Outdent [66..66)
         |Ident(w) [75..76)
         |RightArrow [83..85)
         |Ident(c) [86..87)
         |Indentation.Outdent [87..87)
         |LF [87..88)
         |KwDef [90..93)
         |Ident(g) [94..95)
         |Equals [96..97)
         |Constant.Int(1) [98..99)
         |Indentation.Outdent [99..99)
         |EOF [100..100)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case y if foo {
         |      z
         |    } w => c
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
          tmatch(
            tname("x"),
            Case(patvar("y"), Some(tpostfix(tapply(tname("foo"), blk(tname("z"))), "w")), tname("c")),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
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

  test("indented then body, else on the body line") {
    val code =
      """|def f =
         |  if c then
         |    x else 1
         |""".stripMargin
    val layout = "def f = if (c) x else 1"
    val tree = Defn.Def(Nil, "f", Nil, None, Term.If(tname("c"), tname("x"), lit(1), Nil))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented then body, else at the body indent") {
    val code =
      """|def f =
         |  if c then
         |    x
         |    else y
         |""".stripMargin
    val layout = "def f = if (c) x else y"
    val tree = Defn.Def(Nil, "f", Nil, None, Term.If(tname("c"), tname("x"), tname("y"), Nil))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("nested ifs, an infix continuation with else, outer else on the next line") {
    val code =
      """|def f =
         |  if (foo)
         |    if (bar) baz &&
         |      qux else quux
         |  else xyzzy
         |""".stripMargin
    val layout = "def f = if (foo) if (bar) baz && qux else quux else xyzzy"
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(
        tname("foo"),
        Term.If(tname("bar"), tinfix(tname("baz"), "&&", tname("qux")), tname("quux"), Nil),
        tname("xyzzy"),
        Nil,
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented then body, a comment line, else at the body indent") {
    val code =
      """|def f =
         |  if c then
         |    x
         |    // note
         |    else y
         |""".stripMargin
    val layout =
      """|def f = if (c) x
         |// note
         |else y
         |""".stripMargin
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.If(tname("c"), tname("x"), tnameComments("y")("// note")(), Nil))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented condition, a comment before then at the body indent") {
    val code =
      """|def f =
         |  if
         |    a
         |    /* note */ then b else c
         |""".stripMargin
    val layout =
      """|def f = if ({
         |  a
         |  /* note */
         |}) b else c
         |""".stripMargin
    val b = tname("b")
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.If(blk(tnameComments("a")()("/* note */")), b, tname("c"), Nil))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented condition, a comment right before then at the body indent") {
    val code =
      """|def f =
         |  if
         |    a
         |    /* note */then b else c
         |""".stripMargin
    val layout =
      """|def f = if ({
         |  a
         |  /* note */
         |}) b else c
         |""".stripMargin
    val b = tname("b")
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.If(blk(tnameComments("a")()("/* note */")), b, tname("c"), Nil))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented while condition, a comment before do at the body indent") {
    val code =
      """|def f =
         |  while
         |    a
         |    /* note */ do b
         |""".stripMargin
    val layout =
      """|def f = while ({
         |  a
         |  /* note */
         |}) b
         |""".stripMargin
    val b = tname("b")
    val tree = Defn.Def(Nil, "f", Nil, None, Term.While(blk(tnameComments("a")()("/* note */")), b))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented while condition, a comment right before do at the body indent") {
    val code =
      """|def f =
         |  while
         |    a
         |    /* note */do b
         |""".stripMargin
    val layout =
      """|def f = while ({
         |  a
         |  /* note */
         |}) b
         |""".stripMargin
    val b = tname("b")
    val tree = Defn.Def(Nil, "f", Nil, None, Term.While(blk(tnameComments("a")()("/* note */")), b))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented while condition, a multi-line comment before do at the body indent") {
    val code =
      """|def f =
         |  while
         |    a
         |    /* note
         |    */ do b
         |""".stripMargin
    val layout =
      """|def f = while ({
         |  a
         |  /* note
         |    */
         |}) b
         |""".stripMargin
    val b = tname("b")
    val tree = Defn
      .Def(Nil, "f", Nil, None, Term.While(blk(tnameComments("a")()("/* note\n    */")), b))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented then body, a comment before else at the body indent") {
    val code =
      """|def f =
         |  if c then
         |    x
         |    /* note */ else y
         |""".stripMargin
    val layout =
      """|def f = if (c) x
         |/* note */ else y
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      Term.If(tname("c"), tname("x"), tnameComments("y")("/* note */")(), Nil),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("nested inline ifs, an argument on a deeper line") {
    val code =
      """|def f =
         |  if (a) if (b) foo
         |    (x)
         |  next
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) if (b) foo(x)
         |  next
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      blk(
        Term.If(tname("a"), Term.If(tname("b"), tapply("foo", tname("x")), lit(), Nil), lit(), Nil),
        tname("next"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("nested inline ifs, an argument on a deeper line, else on that line") {
    val code =
      """|def f =
         |  if (a) if (b) foo
         |    (x) else y
         |  next
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) if (b) foo(x) else y
         |  next
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      blk(
        Term.If(
          tname("a"),
          Term.If(tname("b"), tapply("foo", tname("x")), tname("y"), Nil),
          lit(),
          Nil,
        ),
        tname("next"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("nested inline ifs with then, a brace argument on a deeper line") {
    val code =
      """|def f =
         |  if a then if b then foo
         |      { x => x }
         |  next
         |""".stripMargin
    val layout =
      """|def f = {
         |  if (a) if (b) foo {
         |    x => x
         |  }
         |  next
         |}
         |""".stripMargin
    val arg = blk(tfunc(tparam("x"))(tname("x")))
    val tree = Defn.Def(
      Nil,
      "f",
      Nil,
      None,
      blk(
        Term.If(tname("a"), Term.If(tname("b"), tapply("foo", arg), lit(), Nil), lit(), Nil),
        tname("next"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("flat match cases with a then body, a select on the next line") {
    val code =
      """|def g =
         |  x match
         |  case 1 => if p then b
         |  .h
         |""".stripMargin
    val layout =
      """|def g = x match {
         |  case 1 =>
         |    if (p) b.h
         |}
         |""".stripMargin
    val body = Term.If(tname("p"), tselect("b", "h"), lit(), Nil)
    val tree = Defn.Def(Nil, "g", Nil, None, tmatch(tname("x"), Case(lit(1), None, body)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("same-line catch case with an if body, a colon argument in the condition, a leading infix, then at a line start") {
    val code =
      """|object A:
         |  def f =
         |    val r = try a catch case _ => if x && foo:
         |          z
         |        && w
         |      then c else d
         |    r
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    val r = try a catch {
         |      case _ =>
         |        if (x && foo {
         |          z && w
         |        }) c else d
         |    }
         |    r
         |  }
         |}
         |""".stripMargin
    val cond = tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))))
    val handler = Case(Pat.Wildcard(), None, Term.If(cond, tname("c"), tname("d"), Nil))
    val r = Defn.Val(Nil, List(patvar("r")), None, Term.Try(tname("a"), List(handler), None))
    val tree = Defn
      .Object(Nil, tname("A"), tpl(Defn.Def(Nil, tname("f"), Nil, None, blk(r, tname("r")))))
    runTestAssert[Stat](code, layout)(tree)
  }

}
