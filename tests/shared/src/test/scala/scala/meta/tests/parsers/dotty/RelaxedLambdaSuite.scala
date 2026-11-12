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
         |  x => x + 1
         |}.filter {
         |  y => y > 0
         |}
         |""".stripMargin
    val tree = tapply(
      tselect(
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tinfix(tname("x"), "+", lit(1))))),
        "filter",
      ),
      blk(tfunc(tparam("y"))(tinfix(tname("y"), ">", lit(0)))),
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
    val layout =
      """|val used = score.filter {
         |  case (_, (r, w)) =>
         |    r > 0 || w > 0
         |}
         |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(patvar("used")),
      None,
      tapply(
        tselect("score", "filter"),
        Term.PartialFunction(List(Case(
          Pat.Tuple(List(Pat.Wildcard(), Pat.Tuple(List(patvar("r"), patvar("w"))))),
          None,
          tinfix(tinfix(tname("r"), ">", lit(0)), "||", tinfix(tname("w"), ">", lit(0))),
        ))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case with guard") {
    val code =
      """|xs.collect: case (a, b) if b > 2 => a
         |""".stripMargin
    val layout =
      """|xs.collect {
         |  case (a, b) if b > 2 => a
         |}
         |""".stripMargin
    val tree = tapply(
      tselect("xs", "collect"),
      Term.PartialFunction(List(Case(
        Pat.Tuple(List(patvar("a"), patvar("b"))),
        Some(tinfix(tname("b"), ">", lit(2))),
        tname("a"),
      ))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case, leading select on the next line") {
    val code =
      """|val one = list
         |  .collect: case x => (x, x + 1)
         |  .toMap
         |""".stripMargin
    val layout =
      """|val one = list.collect {
         |  case x =>
         |    (x, x + 1)
         |}.toMap
         |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(patvar("one")),
      None,
      tselect(
        tapply(
          tselect("list", "collect"),
          Term.PartialFunction(List(
            Case(patvar("x"), None, Term.Tuple(List(tname("x"), tinfix(tname("x"), "+", lit(1))))),
          )),
        ),
        "toMap",
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: indented case body") {
    val code =
      """|val three = list
         |  .collect: case x =>
         |    val y = x + 1
         |    (x, y)
         |  .toMap
         |""".stripMargin
    val layout =
      """|val three = list.collect {
         |  case x =>
         |    val y = x + 1
         |    (x, y)
         |}.toMap
         |""".stripMargin
    val tree = Defn.Val(
      Nil,
      List(patvar("three")),
      None,
      tselect(
        tapply(
          tselect("list", "collect"),
          Term.PartialFunction(List(Case(
            patvar("x"),
            None,
            blk(
              Defn.Val(Nil, List(patvar("y")), None, tinfix(tname("x"), "+", lit(1))),
              Term.Tuple(List(tname("x"), tname("y"))),
            ),
          ))),
        ),
        "toMap",
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
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
    val layout =
      """|xs.map {
         |  [T] => (x: T) => List(x)
         |}
         |""".stripMargin
    val tree = tapply(
      tselect("xs", "map"),
      blk(Term.PolyFunction(
        Type.ParamClause(List(
          Type.Param(Nil, pname("T"), Type.ParamClause(Nil), Type.Bounds(None, None, Nil, Nil)),
        )),
        tfunc(tparam("x", pname("T")))(tapply(tname("List"), tname("x"))),
      )),
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
    val layout =
      """|def f = (new A) {
         |  case x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(Term.New(init("A")), Term.PartialFunction(List(Case(patvar("x"), None, tname("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new before else, leading select on the next line") {
    val code =
      """|def f = if c then new A else xs.map: x => x
         |  .foo
         |""".stripMargin
    val layout =
      """|def f = if (c) new A else xs.map {
         |  x => x
         |}.foo
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        "c",
        Term.New(init("A")),
        tselect(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))("x"))), "foo"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: new prefix, type ascription") {
    val code =
      """|def f = new A: B
         |""".stripMargin
    val layout =
      """|def f = new A: B
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, Term.Ascribe(Term.New(init("A")), pname("B")))
    runTestAssert[Stat](code, layout)(tree)
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

  test("SIP-75 colon: new in a case body, typed extractor pattern in the next case") {
    val code =
      """|def f = x match { case 1 => new A; case Some(z): B => 1 }
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    new A
         |  case Some(z): B =>
         |    1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tmatch(
        "x",
        Case(lit(1), None, Term.New(init("A"))),
        Case(Pat.Typed(Pat.Extract(tname("Some"), List(patvar("z"))), pname("B")), None, lit(1)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new prefix alone on its line, select chain on the next") {
    val code =
      """|def f =
         |  new A(1)
         |    .foo: x => x
         |  .bar
         |""".stripMargin
    val layout =
      """|def f = new A(1).foo {
         |  x => x
         |}.bar
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tselect(
        tapply(tselect(Term.New(init("A", List(lit(1)))), "foo"), blk(tfunc(tparam("x"))("x"))),
        "bar",
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 single-line lambda: new prefix, leading infix on the next line") {
    val code =
      """|def f = new A: x => x
         |  + 1
         |""".stripMargin
    val layout =
      """|def f = (new A) {
         |  x => x
         |} + 1
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tinfix(tapply(Term.New(init("A")), blk(tfunc(tparam("x"))(tname("x")))), "+", lit(1)),
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

  test("SIP-75 single-line lambda: parenthesized operand of an infix operator") {
    val code =
      """|def f = a + (b): x => x
         |""".stripMargin
    val layout =
      """|def f = a + b {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tinfix(tname("a"), "+", tapply(tname("b"), blk(tfunc(tparam("x"))(tname("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 colon: parenthesized placeholder operand of an infix operator, type ascription") {
    val code =
      """|def f = a + (_ + 1): Int
         |""".stripMargin
    val layout =
      """|def f = a + (_ + 1): Int
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(
        tinfix(tname("a"), "+", Term.AnonymousFunction(tinfix(Term.Placeholder(), "+", lit(1)))),
        pname("Int"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("colon: new prefix with arguments, empty template body before an end marker") {
    val code =
      """|object A:
         |  val f = new B(1):
         |  end f
         |""".stripMargin
    val layout =
      """|object A {
         |  val f = new B(1)
         |  end f
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(Nil, List(patvar("f")), None, Term.New(init("B", List(lit(1))))),
        Term.EndMarker(tname("f")),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon: new prefix with type arguments, empty template body before an end marker") {
    val code =
      """|object A:
         |  val f = new B[T]:
         |  end f
         |""".stripMargin
    val layout =
      """|object A {
         |  val f = new B[T]
         |  end f
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(Nil, List(patvar("f")), None, Term.New(init(papply("B", "T")))),
        Term.EndMarker(tname("f")),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon: new type projection prefix, empty template body before an end marker") {
    val code =
      """|object A:
         |  val f = new B#C:
         |  end f
         |""".stripMargin
    val layout =
      """|object A {
         |  val f = new B#C
         |  end f
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(Nil, List(patvar("f")), None, Term.New(init(Type.Project(pname("B"), pname("C"))))),
        Term.EndMarker(tname("f")),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon: new select prefix, empty template body before an end marker") {
    val code =
      """|object A:
         |  val f = new a.B:
         |  end f
         |""".stripMargin
    val layout =
      """|object A {
         |  val f = new a.B
         |  end f
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(Nil, List(patvar("f")), None, Term.New(init(pselect(tname("a"), "B")))),
        Term.EndMarker(tname("f")),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon: new this prefix, empty template body before an end marker") {
    val code =
      """|object A:
         |  val f = new this.B:
         |  end f
         |""".stripMargin
    val layout =
      """|object A {
         |  val f = new this.B
         |  end f
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(Nil, List(patvar("f")), None, Term.New(init(pselect(Term.This(anon), "B")))),
        Term.EndMarker(tname("f")),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon: new super prefix, empty template body before an end marker") {
    val code =
      """|object A:
         |  val f = new super.B:
         |  end f
         |""".stripMargin
    val layout =
      """|object A {
         |  val f = new super.B
         |  end f
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(Nil, List(patvar("f")), None, Term.New(init(pselect(Term.Super(anon, anon), "B")))),
        Term.EndMarker(tname("f")),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon: new super[T] prefix, empty template body before an end marker") {
    val code =
      """|object A:
         |  val f = new super[T].B:
         |  end f
         |""".stripMargin
    val layout =
      """|object A {
         |  val f = new super[T].B
         |  end f
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
          Term.New(init(pselect(Term.Super(anon, Name.Indeterminate("T")), "B"))),
        ),
        Term.EndMarker(tname("f")),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon: new with this prefix, empty template body before an end marker") {
    val code =
      """|object A:
         |  val f = new A with this.B:
         |  end f
         |""".stripMargin
    val layout =
      """|object A {
         |  val f = new A with this.B
         |  end f
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
          Term.NewAnonymous(tpl(List(init("A"), init(pselect(Term.This(anon), "B"))), Nil)),
        ),
        Term.EndMarker(tname("f")),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon: new ellipsis unquote prefix, empty template body before an end marker") {
    val code =
      """|object A:
         |  val f = new ..$inits:
         |  end f
         |""".stripMargin
    val structure =
      """|Defn.Object(
         |  Nil,
         |  Term.Name("A"),
         |  Template(
         |    None,
         |    Nil,
         |    Template.Body(
         |      None,
         |      List(
         |        Defn.Val(
         |          Nil,
         |          List(
         |            Pat.Var(Term.Name("f"))
         |          ),
         |          None,
         |          Term.New(Init.Quasi(
         |            1,
         |            Init.Quasi(
         |              0,
         |              Term.Name("inits")
         |            )
         |          ))
         |        ),
         |        Term.EndMarker(Term.Name("f"))
         |      )
         |    ),
         |    Nil
         |  )
         |)""".stripMargin
    assertNoDiff(stat(code)(dialect.unquoteTerm(multiline = true)).structure, structure)
  }

  test("colon: new ellipsis annotation, empty template body before an end marker") {
    val code =
      """|object A:
         |  val f = new A ..@$annots:
         |  end f
         |""".stripMargin
    val structure =
      """|Defn.Object(
         |  Nil,
         |  Term.Name("A"),
         |  Template(
         |    None,
         |    Nil,
         |    Template.Body(
         |      None,
         |      List(
         |        Defn.Val(
         |          Nil,
         |          List(
         |            Pat.Var(Term.Name("f"))
         |          ),
         |          None,
         |          Term.New(Init(
         |            Type.Annotate(
         |              Type.Name("A"),
         |              List(
         |                Mod.Annot.Quasi(
         |                  1,
         |                  Mod.Annot.Quasi(
         |                    0,
         |                    Term.Name("annots")
         |                  )
         |                )
         |              )
         |            ),
         |            Name.Anonymous(),
         |            Nil
         |          ))
         |        ),
         |        Term.EndMarker(Term.Name("f"))
         |      )
         |    ),
         |    Nil
         |  )
         |)""".stripMargin
    assertNoDiff(stat(code)(dialect.unquoteTerm(multiline = true)).structure, structure)
  }

  test("colon: new super[T] prefix, template body before a leading infix line") {
    val code =
      """|object A:
         |  def f = new super[T].B:
         |      def x = 1
         |    + 1
         |""".stripMargin
    val layout = "object A { def f = new super[T].B { def x = 1 } + 1 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tinfix(
          Term.NewAnonymous(tpl(
            List(init(pselect(Term.Super(anon, Name.Indeterminate("T")), "B"))),
            List(Defn.Def(Nil, tname("x"), Nil, None, int(1))),
          )),
          "+",
          int(1),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: new prefix colon, single-line lambda") {
    val code =
      """|def f = g(new A: x => x)
         |""".stripMargin
    val layout =
      """|def f = g(new A: (x => x))
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tapply("g", Term.Ascribe(Term.New(init("A")), pfunc("x")("x"))))
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
    val layout =
      """|def f = xs.map {
         |  x => xs.map {
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
        blk(tfunc(tparam("x"))(
          tapply(tselect("xs", "map"), Term.PartialFunction(List(Case(patvar("y"), None, tname("y"))))),
        )),
      ),
    )
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
         |  } + 1
         |}.bar
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tselect(
        tapply(
          tselect("xs", "map"),
          blk(tfunc(tparam("x"))(tinfix(tapply(tname("foo"), blk(tname("a"))), "+", lit(1)))),
        ),
        "bar",
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

  test("SIP-75 multi-line polymorphic lambda: leading select on the next line") {
    val code =
      """|def f = xs.map: [T] => (x: T) =>
         |    x
         |  .size
         |""".stripMargin
    val error =
      """|<input>:3: error: illegal start of definition `.`
         |  .size
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("SIP-75 parens: select colon, curried lambda arrow at end of line") {
    val code =
      """|def f = g(xs.map: x => y =>
         |  x)
         |""".stripMargin
    val layout =
      """|def f = g(xs.map: (x => y => x))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("g", Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: select colon on its own line, curried lambda arrow at end of line, body at the same indent") {
    val code =
      """|def f = bar(
         |  xs.map: x => y =>
         |  x
         |)
         |""".stripMargin
    val layout =
      """|def f = bar(xs.map: (x => y => x))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("bar", Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: select colon on its own line, curried lambda arrow at end of line, blank line, body at the same indent") {
    val code =
      """|def f = bar(
         |  xs.map: x => y =>
         |
         |  x
         |)
         |""".stripMargin
    val layout =
      """|def f = bar(xs.map: (x => y => x))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("bar", Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: select colon on its own line, curried lambda arrow at end of line, blank line, body past the same indent") {
    val code =
      """|def f = bar(
         |  xs.map: x => y =>
         |
         |    x
         |)
         |""".stripMargin
    val layout =
      """|def f = bar(xs.map: (x => y => x))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("bar", Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: typed name on its own line, curried function type arrow at end of line, result at the same indent") {
    val code =
      """|def f = compose(
         |  first: A => B =>
         |  C,
         |  second
         |)
         |""".stripMargin
    val layout =
      """|def f = compose(first: (A => B => C), second)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("compose", Term.Ascribe(tname("first"), pfunc("A")(pfunc("B")("C"))), tname("second")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: select colon, curried lambda arrow at end of line, body at the indent of the line with the parenthesis") {
    val code =
      """|object O:
         |  def f =
         |    bar(xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(xs.map: (x => y => x)) }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply("bar", Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 infix colon, curried lambda arrow at end of line") {
    val code =
      """|def f = xs foreach: x => y =>
         |  x
         |""".stripMargin
    val layout =
      """|def f = xs foreach: (x => y => x)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Ascribe(tpostfix(tname("xs"), "foreach"), pfunc("x")(pfunc("y")("x"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: typed name colon, curried function type arrow at end of line") {
    val code =
      """|def f = g(a: A => B =>
         |  C)
         |""".stripMargin
    val layout =
      """|def f = g(a: (A => B => C))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("g", Term.Ascribe(tname("a"), pfunc("A")(pfunc("B")("C")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: select colon, curried lambda arrow at end of line, body in the first column") {
    val code =
      """|def f = bar(xs.map: x => y =>
         |x)
         |""".stripMargin
    val layout =
      """|def f = bar(xs.map: (x => y => x))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("bar", Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after an argument line, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(a,
         |    xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(a, xs.map: (x => y => x)) }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply("bar", tname("a"), Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after an argument line, curried lambda arrow at end of line, body past the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(a,
         |    xs.map: x => y =>
         |      x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(a, xs.map: (x => y => x)) }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply("bar", tname("a"), Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: deeper select colon after an argument line, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(a,
         |      xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(a, xs.map: (x => y => x)) }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply("bar", tname("a"), Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: deeper select colon after an argument line, curried lambda arrow at end of line, body at the same indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(a,
         |      xs.map: x => y =>
         |      x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(a, xs.map: (x => y => x)) }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply("bar", tname("a"), Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test(
    "SIP-75 parens: select colon on its own line, three lambda starts, body at the same indent",
  ) {
    val code =
      """|def f = bar(
         |  xs.map: x => y => z =>
         |  x,
         |  z
         |)
         |""".stripMargin
    val layout =
      """|def f = bar(xs.map: (x => y => z => x), z)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        "bar",
        Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")(pfunc("z")("x")))),
        tname("z"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: select colon on its own line, polymorphic curried lambda, body at the same indent") {
    val code =
      """|def f = bar(
         |  xs.map: [T] => x => y =>
         |  x,
         |  z
         |)
         |""".stripMargin
    val layout =
      """|def f = bar(xs.map: ([T] => x => y => x), z)
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        "bar",
        Term.Ascribe(
          tselect("xs", "map"),
          Type.PolyFunction(List(pparam("T")), pfunc("x")(pfunc("y")("x"))),
        ),
        tname("z"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: two select colons, curried lambda arrows at end of line, bodies at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(xs.map: x => y =>
         |    x, xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(xs.map: (x => y => x), xs.map: (x => y => x)) }
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
          "bar",
          Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))),
          Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after a nested argument over two lines, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(g(a,
         |      b), xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(g(a, b), xs.map: (x => y => x)) }
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
          "bar",
          tapply("g", tname("a"), tname("b")),
          Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after a nested argument whose line ends after its parenthesis, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(g(
         |        a), xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(g(a), xs.map: (x => y => x)) }
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
          "bar",
          tapply("g", tname("a")),
          Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after a lambda argument in braces, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(g { a =>
         |      a }, xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = bar(g {
         |    a => a
         |  }, xs.map: (x => y => x))
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
          "bar",
          tapply("g", blk(tfunc(tparam("a"))(tname("a")))),
          Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after a lambda argument in parentheses, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(g(a =>
         |      a), xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(g(a => a), xs.map: (x => y => x)) }
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
          "bar",
          tapply("g", tfunc(tparam("a"))(tname("a"))),
          Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after a doubly nested argument over two lines, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(f(g(a,
         |      b)), xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(f(g(a, b)), xs.map: (x => y => x)) }
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
          "bar",
          tapply("f", tapply("g", tname("a"), tname("b"))),
          Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after a doubly nested argument whose outer line ends after its parenthesis, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(f(
         |      g(a,
         |        b)), xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(f(g(a, b)), xs.map: (x => y => x)) }
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
          "bar",
          tapply("f", tapply("g", tname("a"), tname("b"))),
          Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after an argument in braces over two lines, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(f { g(a,
         |      b) }, xs.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = bar(f {
         |    g(a, b)
         |  }, xs.map: (x => y => x))
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
          "bar",
          tapply("f", blk(tapply("g", tname("a"), tname("b")))),
          Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after a colon lambda argument, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(g(xs.map: a =>
         |      a), ys.map: x => y =>
         |    x)
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = bar(g(xs.map {
         |    a => a
         |  }), ys.map: (x => y => x))
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
          "bar",
          tapply("g", tapply(tselect("xs", "map"), blk(tfunc(tparam("a"))(tname("a"))))),
          Term.Ascribe(tselect("ys", "map"), pfunc("x")(pfunc("y")("x"))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after a brace block whose first statement spans two lines, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f = foo { g(a,
         |      b)
         |    bar(c,
         |      xs.map: u => v =>
         |    u)
         |  }
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = foo {
         |    g(a, b)
         |    bar(c, xs.map: (u => v => u))
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
        tapply(
          "foo",
          blk(
            tapply("g", tname("a"), tname("b")),
            tapply("bar", tname("c"), Term.Ascribe(tselect("xs", "map"), pfunc("u")(pfunc("v")("u")))),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after for generators over two lines, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(for x <- xs
         |            y <- ys
         |        yield x, zs.map: u => v =>
         |    u)
         |""".stripMargin
    val layout =
      """|object O { def f = bar(for (x <- xs; y <- ys) yield x, zs.map: (u => v => u)) }
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
          "bar",
          Term.ForYield(
            List(Enumerator.Generator(patvar("x"), "xs"), Enumerator.Generator(patvar("y"), "ys")),
            tname("x"),
          ),
          Term.Ascribe(tselect("zs", "map"), pfunc("u")(pfunc("v")("u"))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after a comment over two lines, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    bar(/* a
         |      */ xs.map: u => v =>
         |    u)
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = bar( /* a
         |      */ xs.map: (u => v => u))
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
          "bar",
          Term.Ascribe.newBuilder(tselect("xs", "map"), pfunc("u")(pfunc("v")("u")))
            .begComment(Seq("/* a\n      */")).result(),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon after an argument line in braces, curried lambda arrow at end of line, body at the braces indent") {
    val code =
      """|def f = {
         |  bar(a,
         |    xs.map: x => y =>
         |  x)
         |}
         |""".stripMargin
    val layout =
      """|def f = {
         |  bar(a, xs.map: (x => y => x))
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tapply("bar", tname("a"), Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: select colon in a case body, curried lambda arrow at end of line, body at the case indent") {
    val code =
      """|object O:
         |  def f = x match
         |    case a =>
         |      bar(b,
         |        xs.map: u => v =>
         |      u)
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = x match {
         |    case a =>
         |      bar(b, xs.map: (u => v => u))
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
        Term.Match(
          tname("x"),
          List(Case(
            patvar("a"),
            None,
            tapply("bar", tname("b"), Term.Ascribe(tselect("xs", "map"), pfunc("u")(pfunc("v")("u")))),
          )),
          Nil,
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon in the body of a lambda in braces, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f = foo { a =>
         |    bar(b, xs.map: u => v =>
         |    u)
         |  }
         |""".stripMargin
    val layout =
      """|object O {
         |  def f = foo {
         |    a => bar(b, xs.map: (u => v => u))
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
        tapply(
          "foo",
          blk(tfunc(tparam("a"))(
            tapply("bar", tname("b"), Term.Ascribe(tselect("xs", "map"), pfunc("u")(pfunc("v")("u")))),
          )),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 parens: select colon on an infix continuation line, curried lambda arrow at end of line, body at the same indent") {
    val code =
      """|object O:
         |  val x = a +
         |    bar(b, xs.map: u => v =>
         |    u)
         |""".stripMargin
    val layout =
      """|object O { val x = a + bar(b, xs.map: (u => v => u)) }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Val(
        Nil,
        List(patvar("x")),
        None,
        tinfix(
          tname("a"),
          "+",
          tapply("bar", tname("b"), Term.Ascribe(tselect("xs", "map"), pfunc("u")(pfunc("v")("u")))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 nested parens: select colon on the line of the inner parenthesis, curried lambda arrow at end of line, body at the outer indent") {
    val code =
      """|def f = bar(
         |  g(xs.map: x => y =>
         |  x)
         |)
         |""".stripMargin
    val layout =
      """|def f = bar(g(xs.map: (x => y => x)))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply("bar", tapply("g", Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 nested parens: select colon after an argument line, curried lambda arrow at end of line, body past the outer indent") {
    val code =
      """|def f = bar(
         |  g(a,
         |    xs.map: x => y =>
         |    x)
         |)
         |""".stripMargin
    val layout =
      """|def f = bar(g(a, xs.map: (x => y => x)))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        "bar",
        tapply("g", tname("a"), Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: using, select colon on the next line, curried lambda arrow at end of line, body at the same indent") {
    val code =
      """|def f = bar(using
         |  xs.map: x => y =>
         |  x
         |)
         |""".stripMargin
    val layout =
      """|def f = bar(using xs.map: (x => y => x))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Apply(
        tname("bar"),
        Term.ArgClause(
          List(Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x")))),
          Some(Mod.Using()),
        ),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 if condition: select colon after the parenthesis, curried lambda arrow at end of line, body at the statement indent") {
    val code =
      """|object O:
         |  def f =
         |    if (xs.exists: x => y =>
         |    x) 1 else 2
         |""".stripMargin
    val layout =
      """|object O { def f = if (xs.exists: (x => y => x)) 1 else 2 }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.If(
          Term.Ascribe(tselect("xs", "exists"), pfunc("x")(pfunc("y")("x"))),
          lit(1),
          lit(2),
          Nil,
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 if condition: select colon on its own line, curried lambda arrow at end of line, body at the same indent") {
    val code =
      """|object O:
         |  def f =
         |    if (
         |      xs.exists: x => y =>
         |      x) 1 else 2
         |""".stripMargin
    val layout =
      """|object O { def f = if (xs.exists: (x => y => x)) 1 else 2 }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.If(
          Term.Ascribe(tselect("xs", "exists"), pfunc("x")(pfunc("y")("x"))),
          lit(1),
          lit(2),
          Nil,
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 tuple: select colon on its own line, curried lambda arrow at end of line, body at the same indent") {
    val code =
      """|def f = (
         |  a,
         |  xs.map: x => y =>
         |  x
         |)
         |""".stripMargin
    val layout =
      """|def f = (a, xs.map: (x => y => x))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Tuple(List(tname("a"), Term.Ascribe(tselect("xs", "map"), pfunc("x")(pfunc("y")("x"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: placeholder colon, curried function type arrow at end of line") {
    val code =
      """|def f = xs.map(_: A => B =>
         |  C)
         |""".stripMargin
    val layout =
      """|def f = xs.map(_: (A => B => C))
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.AnonymousFunction(
        tapply(tselect("xs", "map"), Term.Ascribe(Term.Placeholder(), pfunc("A")(pfunc("B")("C")))),
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

  test("SIP-75 lambda parameter: curried function type, arrow at end of line") {
    val code =
      """|def f = (g: A => B =>
         |  C) => g
         |""".stripMargin
    val layout =
      """|def f = (g: A => B => C) => g
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tfunc(tparam("g", pfunc("A")(pfunc("B")("C"))))(tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: case guard, argument colon, lambda arrow at end of line") {
    val code =
      """|def f = x match
         |  case y if f(ys.exists: z =>
         |      z.ok) => 1
         |  case _ => 0
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if f(ys.exists {
         |    z => z.ok
         |  }) => 1
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
          Some(tapply(
            tname("f"),
            tapply(tselect("ys", "exists"), blk(tfunc(tparam("z"))(tselect("z", "ok")))),
          )),
          int(1),
        ),
        Case(patwildcard, None, int(0)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 parens: case guard, infix colon, lambda arrow at end of line") {
    val code =
      """|def f = x match
         |  case y if (xs foreach: z =>
         |      z.ok) => 1
         |  case _ => 0
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case y if xs foreach {
         |    z => z.ok
         |  } => 1
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
          Some(tinfix(tname("xs"), tname("foreach"), blk(tfunc(tparam("z"))(tselect("z", "ok"))))),
          int(1),
        ),
        Case(patwildcard, None, int(0)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 lambda parameter: curried function type, lambda arrow on the next line") {
    val code =
      """|def f = (g: A => B =>
         |  C)
         |  => g
         |""".stripMargin
    val layout =
      """|def f = (g: A => B => C) => g
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tfunc(tparam("g", pfunc("A")(pfunc("B")("C"))))(tname("g")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 multi-line curried lambda: leading select on the next line") {
    val code =
      """|def f = xs.map: x => y =>
         |    x
         |  .size
         |""".stripMargin
    val layout =
      """|def f = xs.map {
         |  x => y => x.size
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tfunc(tparam("y"))(tselect("x", "size"))))),
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
      """|def f = xs map {
         |  x => x
         |}
         |""".stripMargin
    val tree = Defn
      .Def(Nil, tname("f"), Nil, None, tinfix(tname("xs"), "map", blk(tfunc(tparam("x"))(tname("x")))))
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
         |  x => x
         |} ++ ys
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tinfix(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), "++", tname("ys")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case lambda in case body, next case") {
    val code =
      """|def f = x match
         |  case 1 => xs.map: case y => y
         |  case 2 => 3
         |""".stripMargin
    val layout =
      """|def f = x match {
         |  case 1 =>
         |    xs.map {
         |      case y => y
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
        tname("x"),
        Case(
          lit(1),
          None,
          tapply(tselect("xs", "map"), Term.PartialFunction(List(Case(patvar("y"), None, tname("y"))))),
        ),
        Case(lit(2), None, lit(3)),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: typed pattern") {
    val code =
      """|def f = ys.collect: case z: Int => z
         |""".stripMargin
    val layout =
      """|def f = ys.collect {
         |  case z: Int => z
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tapply(
        tselect("ys", "collect"),
        Term.PartialFunction(List(Case(Pat.Typed(patvar("z"), pname("Int")), None, tname("z")))),
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
         |}
         |// c
         |.foo
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("t"),
      Nil,
      None,
      Term.Select(
        tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))),
        tnameComments("foo")("// c")(),
      ),
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
         |    x => f
         |  }(y)
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
        tapply(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("f")))), tname("y")),
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
         |    x => if (x) a
         |  } else b
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
          tname("c"),
          tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(tname("x"), tname("a"))))),
          tname("b"),
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
         |      x => g(x)
         |    } {
         |      y
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
            tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tapply(tname("g"), tname("x"))))),
            blk(tname("y")),
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
         |  def f = {
         |    xs.map {
         |      x => x seconds
         |    }
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(
      Import(
        List(Importer(tselect("scala", "language"), Importee.Name(Name.Indeterminate("postfixOps")))),
      ),
      Defn.Object(
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
              blk(tfunc(tparam("x"))(Term.SelectPostfix(tname("x"), tname("seconds")))),
            ),
            tname("g"),
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
         |    x => if (a) b
         |  } + 1
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
          tinfix(
            tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tif(tname("a"), tname("b"))))),
            "+",
            lit(1),
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
         |    (x, y) => x + y
         |  }.sum
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
          tselect(
            tapply(
              tselect("xs", "map"),
              blk(tfunc(tparam("x"), tparam("y"))(tinfix(tname("x"), "+", tname("y")))),
            ),
            "sum",
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
    val layout =
      """|object A {
         |  def f = xs.collect {
         |    case x if x > 0 => x
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
            tselect("xs", "collect"),
            Term.PartialFunction(List(
              Case(patvar("x"), Some(tinfix(tname("x"), ">", lit(0))), tname("x")),
            )),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, lit(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("SIP-75 relaxed lambda: case with a typed binder pattern, next definition") {
    val code =
      """|object A:
         |  def f = xs.map: case x @ y: Int => x
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    case (x @ y): Int => x
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
            Term.PartialFunction(List(
              Case(Pat.Typed(Pat.Bind(patvar("x"), patvar("y")), pname("Int")), None, tname("x")),
            )),
          ),
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
         |    x => x
         |  }.filter(_ > 0)
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
            tselect(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), "filter"),
            Term.AnonymousFunction(tinfix(Term.Placeholder(), ">", lit(0))),
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
         |    x => x
         |  }.foo
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
          Some(Type.Refine(
            Some(pname("A")),
            Stat.Block(List(
              Decl.Type(Nil, pname("T"), Type.ParamClause(Nil), Type.Bounds(None, None, Nil, Nil)),
            )),
          )),
          tselect(tapply(tselect("xs", "map"), blk(tfunc(tparam("x"))(tname("x")))), "foo"),
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
