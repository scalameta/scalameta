package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.parsers.Parse

class RelaxedLambdaSuite extends BaseDottySuite {

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

  test("SIP-75 single-line lambda: new prefix") {
    val code =
      """|def f = new A: x => x
         |""".stripMargin
    val layout =
      """|def f = (new A) {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply(Term.New(init("A")), blk(tfunc(tparam("x"))(tname("x")))))
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

  test("SIP-75 colon: new prefix, function type ascription") {
    val code =
      """|def f = new A: B => C
         |""".stripMargin
    val layout =
      """|def f = (new A) {
         |  B => C
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply(Term.New(init("A")), blk(tfunc(tparam("B"))(tname("C")))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new prefix, leading infix on the next line") {
    val code =
      """|def f = new A: x => x
         |  + 1
         |""".stripMargin
    val layout =
      """|def f = (new A) {
         |  x => x + 1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(Term.New(init("A")), blk(tfunc(tparam("x"))(tinfix(tname("x"), "+", lit(1))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new prefix after an infix operator") {
    val code =
      """|def f = 1 + new A: x => x
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

  test("SIP-75 single-line lambda: scala213") {
    implicit val dialect: Dialect = dialects.Scala213
    val code = "xs.map: x => x + 1"
    val error =
      """|<input>:1: error: `;` expected but `=>` found
         |xs.map: x => x + 1
         |          ^""".stripMargin
    runTestError[Stat](code, error)
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

  test("SIP-75 colon: term parameter entry point, function type") {
    implicit val parser: String => Term.Param = parseRule(_, Parse.parseTermParam)
    runTestAssert[Term.Param]("x: A => B")(tparam("x", pfunc(pname("A"))(pname("B"))))
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

  test("SIP-75 colon: type parameter entry point, context bound with a function type") {
    implicit val parser: String => Type.Param = parseRule(_, Parse.parseTypeParam)
    val tree = pparam("T", bounds(cb = List(pfunc(pname("C"))(pname("D")))))
    runTestAssert[Type.Param]("T: C => D")(tree)
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

}
