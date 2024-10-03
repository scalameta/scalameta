package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.tokenizers.TokenizerOptions

class TermSuite extends ParseSuite {
  import Name.Anonymous
  import Name.Indeterminate
  import Term.{Name => _, _}

  implicit val dialect: Dialect = dialects.Scala211
  implicit val parseTerm: String => Term = term(_)

  private def assertTerm(expr: String)(tree: Tree)(implicit loc: munit.Location): Unit =
    assertTree(term(expr))(tree)

  private def checkTerm(expr: String, syntax: String = null)(tree: Tree)(implicit
      loc: munit.Location
  ): Unit = checkTree(term(expr), syntax)(tree)

  test("x")(assertTerm("x")(tname("x")))

  test("`x`")(assertTerm("`x`")(tname("x")))

  test("a.b.c")(assertTerm("a.b.c")(Select(Select(tname("a"), tname("b")), tname("c"))))

  test("a.b c")(assertTerm("a.b c")(Select(Select(tname("a"), tname("b")), tname("c"))))

  test("foo.this")(assertTerm("foo.this")(This(Indeterminate("foo"))))

  test("this")(assertTerm("this")(This(Anonymous())))

  test("a.super[b].c") {
    assertTerm("a.super[b].c")(Select(Super(Indeterminate("a"), Indeterminate("b")), tname("c")))
  }

  test("super[b].c") {
    assertTerm("super[b].c")(Select(Super(Anonymous(), Indeterminate("b")), tname("c")))
  }

  test("a.super.c") {
    assertTerm("a.super.c")(Select(Super(Indeterminate("a"), Anonymous()), tname("c")))
  }

  test("super.c")(assertTerm("super.c")(Select(Super(Anonymous(), Anonymous()), tname("c"))))

  test("s\"a $b c\"") {
    assertTerm("s\"a $b c\"") {
      Interpolate(tname("s"), str("a ") :: str(" c") :: Nil, tname("b") :: Nil)
    }
  }

  test("f(0)")(assertTerm("f(0)")(Apply(tname("f"), int(0) :: Nil)))

  test("f(x = 0)")(assertTerm("f(x = 0)")(Apply(tname("f"), Assign(tname("x"), int(0)) :: Nil)))

  test("f(x: _*)")(assertTerm("f(x: _*)")(Apply(tname("f"), Repeated(tname("x")) :: Nil)))

  test("f((x: _*))")(assertTerm("f((x: _*))")(Apply(tname("f"), Repeated(tname("x")) :: Nil)))

  test("f(x = xs: _*)") {
    assertTerm("f(x = xs: _*)") {
      Term.Apply(tname("f"), List(Assign(tname("x"), Repeated(tname("xs")))))
    }
    matchSubStructure(
      "f(x = xs: _*)",
      {
        case Term.Apply.After_4_6_0(
              Term.Name("f"),
              Term.ArgClause(List(Assign(Term.Name("x"), Repeated(Term.Name("xs")))), None)
            ) => ()
      }
    )

    matchSubStructure(
      "f(x = xs: _*)",
      {
        case Term.Apply.internal.Latest(
              Term.Name("f"),
              Term.ArgClause(List(Assign(Term.Name("x"), Repeated(Term.Name("xs")))), None)
            ) => ()
      }
    )
  }

  test("f(x = (xs: _*))") {
    assertTerm("f(x = (xs: _*))") {
      Term.Apply(tname("f"), List(Assign(tname("x"), Repeated(tname("xs")))))
    }
  }

  test("a + ()")(assertTerm("a + ()")(ApplyInfix(tname("a"), tname("+"), Nil, Nil)))

  test("a + b")(assertTerm("a + b")(ApplyInfix(tname("a"), tname("+"), Nil, tname("b") :: Nil)))

  test("a + b + c") {
    assertTerm("a + b + c") {
      ApplyInfix(
        ApplyInfix(tname("a"), tname("+"), Nil, tname("b") :: Nil),
        tname("+"),
        Nil,
        tname("c") :: Nil
      )
    }
  }

  test("a :: b") {
    assertTerm("a :: b")(ApplyInfix(tname("a"), tname("::"), Nil, tname("b") :: Nil))
  }

  test("a :: b :: c") {
    assertTerm("a :: b :: c") {
      ApplyInfix(
        tname("a"),
        tname("::"),
        Nil,
        ApplyInfix(tname("b"), tname("::"), Nil, tname("c") :: Nil) :: Nil
      )
    }
  }

  test("!a")(assertTerm("!a")(ApplyUnary(tname("!"), tname("a"))))

  test("!(a: _*)")(assertTerm("!(a: _*)")(ApplyUnary(tname("!"), Repeated(tname("a")))))

  test("a = true")(assertTerm("a = true")(Assign(tname("a"), bool(true))))

  test("a(0) = true") {
    assertTerm("a(0) = true")(Assign(Apply(tname("a"), int(0) :: Nil), bool(true)))
  }

  test("return")(assertTerm("return")(Return(Lit.Unit())))

  test("return 1")(assertTerm("return 1")(Return(int(1))))

  test("throw 1")(assertTerm("throw 1")(Throw(int(1))))

  test("1: Int")(assertTerm("1: Int")(Ascribe(int(1), pname("Int"))))

  test("1: @foo") {
    assertTerm("1: @foo") {
      Annotate(int(1), Mod.Annot(Init(pname("foo"), anon, emptyArgClause)) :: Nil)
    }
  }

  test("(true, false)")(assertTerm("(true, false)")(Tuple(bool(true) :: bool(false) :: Nil)))

  test("{ true; false }")(assertTerm("{ true; false }")(Block(bool(true) :: bool(false) :: Nil)))

  test("{ true }")(assertTerm("{ true }")(Block(bool(true) :: Nil)))

  test("if (true) true else false") {
    assertTerm("if (true) true else false")(If(bool(true), bool(true), bool(false)))
  }

  test("if (true) true; else false") {
    assertTerm("if (true) true; else false")(If(bool(true), bool(true), bool(false)))
  }

  test("if (true) true")(assertTerm("if (true) true")(If(bool(true), bool(true), Lit.Unit())))

  test("if (true && '' match...") {
    val file = """|
                  |if (
                  |  true && ("" match {
                  |    case other ⇒ true
                  |  })
                  |  && true
                  |) ""
                  |""".stripMargin

    assertTerm(file) {
      Term.If(
        Term.ApplyInfix(
          Term.ApplyInfix(
            bool(true),
            tname("&&"),
            Nil,
            List(Term.Match(str(""), List(Case(Pat.Var(tname("other")), None, bool(true)))))
          ),
          tname("&&"),
          Nil,
          List(bool(true))
        ),
        str(""),
        Lit.Unit()
      )
    }
  }

  test("() => x") {
    assertTerm("() => x")(Term.Function(Nil, tname("x")))
    assertTree(blockStat("() => x"))(Term.Function(Nil, tname("x")))
    assertTree(templStat("() => x"))(Term.Function(Nil, tname("x")))
  }

  test("(()) => x") {
    assertTerm("(()) => x")(Term.Function(Nil, tname("x")))
    assertTree(blockStat("(()) => x"))(Term.Function(Nil, tname("x")))
    assertTree(templStat("(()) => x"))(Term.Function(Nil, tname("x")))
  }

  test("x => x") {
    assertTerm("x => x")(Term.Function(List(tparam("x")), tname("x")))
    assertTree(blockStat("x => x"))(Term.Function(List(tparam("x")), tname("x")))

    intercept[ParseException](templStat("x => x"))
  }

  test("(x) => x") {
    assertTerm("(x) => x")(Term.Function(List(tparam("x")), tname("x")))
    assertTree(blockStat("(x) => x"))(Term.Function(List(tparam("x")), tname("x")))

    intercept[ParseException](templStat("(x) => x"))
  }

  test("_ => x") {
    assertTerm("_ => x")(Term.Function(List(tparam("_")), tname("x")))
    assertTree(blockStat("_ => x"))(Term.Function(List(tparam("_")), tname("x")))
    intercept[ParseException](templStat("_ => x"))
  }

  test("(_) => x") {
    assertTerm("(_) => x")(Term.Function(List(tparam("_")), tname("x")))
    assertTree(blockStat("(_) => x"))(Term.Function(List(tparam("_")), tname("x")))
    intercept[ParseException](templStat("(_) => x"))
  }

  test("x: Int => x") {
    // LAWL: this is how scalac's parser works
    assertTerm("x: Int => x") {
      Term.Ascribe(tname("x"), Type.Function(List(pname("Int")), pname("x")))
    }
    assertTree(blockStat("x: Int => x"))(Term.Function(List(tparam("x", "Int")), tname("x")))
    intercept[ParseException](templStat("x: Int => x"))
  }

  test("(x: Int) => x") {
    assertTerm("(x: Int) => x")(Term.Function(List(tparam("x", "Int")), tname("x")))
    assertTree(blockStat("(x: Int) => x"))(Term.Function(List(tparam("x", "Int")), tname("x")))

    assertTree(templStat("(x: Int) => x"))(Term.Function(List(tparam("x", "Int")), tname("x")))
  }

  test("_: Int => x") {
    assertTerm("_: Int => x") {
      Ascribe(Placeholder(), Type.Function(List(pname("Int")), pname("x")))
    }
    assertTree(blockStat("_: Int => x"))(Term.Function(List(tparam("_", "Int")), tname("x")))
    intercept[ParseException](templStat("_: Int => x"))
  }

  test("(_: Int) => x") {
    assertTerm("(_: Int) => x")(Term.Function(List(tparam("_", "Int")), tname("x")))
    assertTree(blockStat("(_: Int) => x"))(Term.Function(List(tparam("_", "Int")), tname("x")))
    assertTree(templStat("(_: Int) => x"))(Term.Function(List(tparam("_", "Int")), tname("x")))
  }

  test("x: Int, y: Int => x") {
    intercept[ParseException](term("x: Int, y: Int => x"))
    intercept[ParseException](blockStat("x: Int, y: Int => x"))
    intercept[ParseException](templStat("x: Int, y: Int => x"))
  }

  test("(x: Int, y: Int) => x") {
    assertTerm("(x: Int, y: Int) => x") {
      Term.Function(List(tparam("x", "Int"), tparam("y", "Int")), tname("x"))
    }
    assertTree(blockStat("(x: Int, y: Int) => x"))(
      Term.Function(List(tparam("x", "Int"), tparam("y", "Int")), tname("x"))
    )
    assertTree(templStat("(x: Int, y: Int) => x"))(
      Term.Function(List(tparam("x", "Int"), tparam("y", "Int")), tname("x"))
    )
  }

  test("{ implicit x => () }") {
    assertTerm("{ implicit x => () }") {
      Block(Function(tparam(Mod.Implicit() :: Nil, "x") :: Nil, Lit.Unit()) :: Nil)
    }
  }

  test("1 match { case 1 => true }") {
    assertTerm("1 match { case 1 => true }") {
      Match(int(1), Case(int(1), None, bool(true)) :: Nil)
    }
  }

  test("1 match { case 1 => }") {
    assertTerm("1 match { case 1 => }") {
      Match(int(1), Case(int(1), None, Term.Block(Nil)) :: Nil)
    }
  }

  test("1 match { case 1 if true => }") {
    assertTerm("1 match { case 1 if true => }") {
      Match(int(1), Case(int(1), Some(bool(true)), Term.Block(Nil)) :: Nil)
    }
  }

  test("1 match { case case 1 if true => }") {
    val intercepted = intercept[ParseException](term("1 match { case case 1 if true => }"))
    assertNoDiff(intercepted.shortMessage, "Unexpected `case`")
  }

  test("try 1")(assertTerm("try 1")(Try(int(1), Nil, None)))

  test("try 1 catch 1")(assertTerm("try 1 catch 1")(TryWithHandler(int(1), int(1), None)))

  test("try (2)")(assertTerm("try (2)")(Try(int(2), Nil, None)))

  test("try 1 catch { case _ => }") {
    assertTerm("try 1 catch { case _ => }") {
      Try(int(1), Case(Pat.Wildcard(), None, Term.Block(Nil)) :: Nil, None)
    }
  }

  test("try 1 finally 1")(assertTerm("try 1 finally 1")(Try(int(1), Nil, Some(int(1)))))

  test("{ case 1 => () }") {
    assertTerm("{ case 1 => () }")(PartialFunction(Case(int(1), None, Lit.Unit()) :: Nil))
  }

  test("while (true) false")(assertTerm("while (true) false")(While(bool(true), bool(false))))

  test("do false while(true)")(assertTerm("do false while(true)")(Do(bool(false), bool(true))))

  test("for (a <- b; if c; x = a) x") {
    assertTerm("for (a <- b; if c; x = a) x") {
      For(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("b")),
          Enumerator.Guard(tname("c")),
          Enumerator.Val(Pat.Var(tname("x")), tname("a"))
        ),
        tname("x")
      )
    }
  }

  test("for (a <- b; if c; x = a) yield x") {
    assertTerm("for (a <- b; if c; x = a) yield x") {
      ForYield(
        List(
          Enumerator.Generator(Pat.Var(tname("a")), tname("b")),
          Enumerator.Guard(tname("c")),
          Enumerator.Val(Pat.Var(tname("x")), tname("a"))
        ),
        tname("x")
      )
    }
  }

  test("f(_)")(assertTerm("f(_)")(AnonymousFunction(Apply(tname("f"), List(Placeholder())))))

  test("_ + 1") {
    assertTerm("_ + 1")(AnonymousFunction(ApplyInfix(Placeholder(), tname("+"), Nil, int(1) :: Nil)))
  }

  test("1 + _") {
    assertTerm("1 + _")(AnonymousFunction(ApplyInfix(int(1), tname("+"), Nil, Placeholder() :: Nil)))
  }

  test("f _")(assertTerm("f _")(Eta(tname("f"))))

  test("new {}")(assertTerm("new {}")(NewAnonymous(tpl())))

  test("new { x }") {
    assertTerm("new { x }")(NewAnonymous(Template(Nil, Nil, EmptySelf(), List(tname("x")))))
  }

  test("new A")(assertTerm("new A")(New(Init(pname("A"), anon, emptyArgClause))))

  test("new A(xs: _*)") {
    assertTerm("new A(xs: _*)") {
      New(Init(pname("A"), anon, List(List(Term.Repeated(tname("xs"))))))
    }
  }

  test("new A {}")(assertTerm("new A {}")(NewAnonymous(tpl(init("A") :: Nil, Nil))))

  test("new A with B") {
    assertTerm("new A with B") {
      NewAnonymous(Template(
        Nil,
        Init(pname("A"), anon, emptyArgClause) :: Init(pname("B"), anon, emptyArgClause) :: Nil,
        EmptySelf(),
        Nil
      ))
    }
  }

  test("new { val x: Int = 1 } with A") {
    assertTerm("new { val x: Int = 1 } with A") {
      NewAnonymous(Template(
        Defn.Val(Nil, List(Pat.Var(tname("x"))), Some(pname("Int")), int(1)) :: Nil,
        Init(pname("A"), anon, emptyArgClause) :: Nil,
        EmptySelf(),
        Nil
      ))
    }
  }

  test("new { self: T => }") {
    assertTerm("new { self: T => }")(NewAnonymous(Template(Nil, Nil, self("self", "T"), Nil)))
  }

  test("a + (b = c)") {
    assertTerm("a + (b = c)") {
      ApplyInfix(tname("a"), tname("+"), Nil, Assign(tname("b"), tname("c")) :: Nil)
    }
  }

  test("(a = b) + c") {
    assertTerm("(a = b) + c") {
      ApplyInfix(Assign(tname("a"), tname("b")), tname("+"), Nil, tname("c") :: Nil)
    }
  }

  test("a + (b = c).d") {
    assertTerm("a + (b = c).d") {
      ApplyInfix(
        tname("a"),
        tname("+"),
        Nil,
        Select(Assign(tname("b"), tname("c")), tname("d")) :: Nil
      )
    }
  }

  test("a + (b: _*)") {
    assertTerm("a + (b: _*)") {
      ApplyInfix(tname("a"), tname("+"), Nil, Repeated(tname("b")) :: Nil)
    }
  }

  test("a + ((b: _*))") {
    assertTerm("a + ((b: _*))") {
      ApplyInfix(tname("a"), tname("+"), Nil, Repeated(tname("b")) :: Nil)
    }
  }

  test("local class") {
    assertTerm("{ case class C(x: Int); }") {
      blk(Defn.Class(List(Mod.Case()), pname("C"), Nil, ctorp(tparam("x", "Int")), tplNoBody()))
    }
  }

  test("xml literal - 1") {
    assertTerm(
      """|{
         |  val x = <p/>
         |  val y = x
         |}""".stripMargin
    ) {
      Term.Block(List(
        Defn.Val(Nil, List(Pat.Var(tname("x"))), None, Term.Xml(List(str("<p/>")), Nil)),
        Defn.Val(Nil, List(Pat.Var(tname("y"))), None, tname("x"))
      ))
    }
  }

  test("implicit closure") {
    assertTerm("Action { implicit request: Request[AnyContent] => Ok }") {
      Term.Apply(
        tname("Action"),
        Term.Block(
          Term.Function(
            tparam(
              List(Mod.Implicit()),
              "request",
              Type.Apply(pname("Request"), List(pname("AnyContent")))
            ) :: Nil,
            tname("Ok")
          ) :: Nil
        ) :: Nil
      )
    }
  }

  test("#312") {
    assertTerm(
      """|{
         |  val x = yz: (Y, Z)
         |  (x, x)
         |}""".stripMargin
    ) {
      Term.Block(List(
        Defn.Val(
          Nil,
          List(Pat.Var(tname("x"))),
          None,
          Term.Ascribe(tname("yz"), Type.Tuple(List(pname("Y"), pname("Z"))))
        ),
        Term.Tuple(List(tname("x"), tname("x")))
      ))
    }
  }

  test("spawn { var v: Int = _; ??? }") {
    assertTerm("spawn { var v: Int = _; ??? }") {
      Term.Apply(
        tname("spawn"),
        List(Term.Block(
          List(Defn.Var(Nil, List(Pat.Var(tname("v"))), Some(pname("Int")), None), tname("???"))
        ))
      )
    }
  }

  test("#345") {
    assertTerm(
      """|x match {
         |  case x => true
         |  // sobaka
         |  case y => y
         |}""".stripMargin
    ) {
      Term.Match(
        tname("x"),
        List(Case(Pat.Var(tname("x")), None, bool(true)), Case(Pat.Var(tname("y")), None, tname("y"))),
        Nil
      )
    }
  }

  test("a + (bs: _*) * c")(intercept[ParseException](term("a + (bs: _*) * c")))

  test("a + b: _*")(intercept[ParseException](term("a + b: _*")))

  test("foo(a + b: _*)") {
    assertTerm("foo(a + b: _*)") {
      Term.Apply(
        tname("foo"),
        List(Term.Repeated(Term.ApplyInfix(tname("a"), tname("+"), Nil, List(tname("b")))))
      )
    }
  }

  test("a + (c, d) * e") {
    assertTerm("a + (c, d) * e") {
      Term.ApplyInfix(
        tname("a"),
        tname("+"),
        Nil,
        List(
          Term
            .ApplyInfix(Term.Tuple(List(tname("c"), tname("d"))), tname("*"), Nil, List(tname("e")))
        )
      )
    }
  }

  test("a * (c, d) + e") {
    assertTerm("a * (c, d) + e") {
      Term.ApplyInfix(
        Term.ApplyInfix(tname("a"), tname("*"), Nil, List(tname("c"), tname("d"))),
        tname("+"),
        Nil,
        List(tname("e"))
      )
    }
  }

  test("(a + b) c") {
    assertTerm("(a + b) c") {
      Term.Select(Term.ApplyInfix(tname("a"), tname("+"), Nil, List(tname("b"))), tname("c"))
    }
  }

  test("a + b c") {
    assertTerm("a + b c") {
      Term.Select(Term.ApplyInfix(tname("a"), tname("+"), Nil, List(tname("b"))), tname("c"))
    }
  }

  test("disallow parse[Stat] on statseqs")(intercept[ParseException](stat("hello; world")))

  test("\"stat;\".parse[Stat]")(assertTrees(stat("stat;"))(tname("stat")))

  test("\"stat;\".parse[Term]")(intercept[ParseException](term("stat;")))

  test("$_")(intercept[ParseException](term(""" q"x + $_" """)))

  test("!x = y") {
    assertTerm("!x = y")(Term.Assign(Term.ApplyUnary(tname("!"), tname("x")), tname("y")))
  }

  test("x = (ys: _*)") {
    assertTerm("x = (ys: _*)")(Term.Assign(tname("x"), Term.Repeated(tname("ys"))))
  }

  test("x = (ys: _`*`)") {
    val error = """|<input>:1: error: `identifier` expected but `)` found
                   |x = (ys: _`*`)
                   |             ^""".stripMargin
    runTestError[Stat]("x = (ys: _`*`)", error)
  }

  test("!(arr.cast[Ptr[Byte]] + sizeof[Ptr[_]]).cast[Ptr[Int]] = length") {
    assertTerm("!(arr.cast[Ptr[Byte]] + sizeof[Ptr[_]]).cast[Ptr[Int]] = length") {
      Term.Assign(
        Term.ApplyUnary(
          tname("!"),
          Term.ApplyType(
            Term.Select(
              Term.ApplyInfix(
                Term.ApplyType(
                  Term.Select(tname("arr"), tname("cast")),
                  List(Type.Apply(pname("Ptr"), List(pname("Byte"))))
                ),
                tname("+"),
                Nil,
                List(Term.ApplyType(
                  tname("sizeof"),
                  List(Type.Apply(pname("Ptr"), List(Type.Wildcard(Type.Bounds(None, None)))))
                ))
              ),
              tname("cast")
            ),
            List(Type.Apply(pname("Ptr"), List(pname("Int"))))
          )
        ),
        tname("length")
      )
    }
  }

  test("(x ++ y)[T]") {
    assertTerm("(x ++ y)[T]") {
      Term
        .ApplyType(Term.ApplyInfix(tname("x"), tname("++"), Nil, List(tname("y"))), List(pname("T")))
    }
  }

  test(" structHydrators map { _[K]() } ") {
    assertTerm(" structHydrators map { _[K]() } ") {
      Term.ApplyInfix(
        tname("structHydrators"),
        tname("map"),
        Nil,
        List(Block(List(AnonymousFunction(Apply(ApplyType(Placeholder(), List(pname("K"))), Nil)))))
      )
    }
  }

  test(" new C()[String]() ") {
    assertTerm(" new C()[String]() ") {
      Term
        .Apply(Term.ApplyType(New(Init(pname("C"), anon, List(List()))), List(pname("String"))), Nil)
    }
  }

  test("#492 parse Unit in infix operations") {
    assertTerm("x == () :: Nil") {
      Term.ApplyInfix(
        tname("x"),
        tname("=="),
        Nil,
        List(Term.ApplyInfix(Lit.Unit(), tname("::"), Nil, List(tname("Nil"))))
      )
    }
  }

  test("#492 parse hlist with Unit") {
    assertTerm(""""foo" :: () :: true :: HNil""") {
      Term.ApplyInfix(
        str("foo"),
        tname("::"),
        Nil,
        List(Term.ApplyInfix(
          Lit.Unit(),
          tname("::"),
          Nil,
          List(Term.ApplyInfix(bool(true), tname("::"), Nil, List(tname("HNil"))))
        ))
      )
    }
  }

  test("nested-braces-in-paren") {
    val code = """|(if (bar) {
                  |  if (foo) { doFoo() }
                  |  val x = 2
                  |})
                  |""".stripMargin

    assertTerm(code) {
      Term.If(
        tname("bar"),
        Term.Block(List(
          Term.If(tname("foo"), Term.Block(List(Term.Apply(tname("doFoo"), Nil))), Lit.Unit()),
          Defn.Val(Nil, List(Pat.Var(tname("x"))), None, int(2))
        )),
        Lit.Unit()
      )
    }
  }

  test("fstring-interpolation") {
    assertTerm("""f"\\u$oct%04x"""") {
      Term.Interpolate(tname("f"), List(str("\\\\u"), str("%04x")), List(tname("oct")))
    }
  }

  test("typed-interpolation") {
    assertTerm("""c"something"[String]""") {
      Term
        .ApplyType(Term.Interpolate(tname("c"), List(str("something")), Nil), List(pname("String")))
    }
  }

  test("interpolation-with-escaped-quotes") {
    assertTerm("""s"\"$t\""""") {
      Interpolate(tname("s"), List(str("\\\""), str("\\\"")), List(tname("t")))
    }
  }

  test("implicit-closure") {
    assertTerm(
      """|function { implicit c =>
         |  {
         |    case bar => foo
         |  }
         |}""".stripMargin
    ) {
      Term.Apply(
        tname("function"),
        Term.Block(
          Term.Function(
            List(tparam(List(Mod.Implicit()), "c")),
            Term.PartialFunction(List(Case(Pat.Var(tname("bar")), None, tname("foo"))))
          ) :: Nil
        ) :: Nil
      )
    }
  }

  test("type-partial-function") {
    val res = stat(
      """|val dynamicStrategy = resharding(
         |  { fakePartitionId: Int =>
         |    {
         |      case sendBox: SendBox.Args => 
         |    }: PartialFunction[ThriftStructIface, Unit]
         |  }
         |)
         |""".stripMargin
    )

    val expected = Defn.Val(
      Nil,
      List(Pat.Var(tname("dynamicStrategy"))),
      None,
      Term.Apply(
        tname("resharding"),
        List(Term.Block(List(Term.Function(
          List(tparam("fakePartitionId", "Int")),
          Term.Ascribe(
            Term.PartialFunction(List(Case(
              Pat.Typed(Pat.Var(tname("sendBox")), Type.Select(tname("SendBox"), pname("Args"))),
              None,
              Term.Block(Nil)
            ))),
            Type.Apply(pname("PartialFunction"), List(pname("ThriftStructIface"), pname("Unit")))
          )
        ))))
      )
    )
    assertTree(res)(expected)
  }

  test("partial-function-returning-implicit-closure") {
    assertTerm(
      """|{
         |  case true => implicit i => "xxx"
         |  case false => implicit i => i.toString
         |}""".stripMargin
    ) {
      Term.PartialFunction(List(
        Case(bool(true), None, Term.Function(List(tparam(List(Mod.Implicit()), "i")), str("xxx"))),
        Case(
          bool(false),
          None,
          Term.Function(
            List(tparam(List(Mod.Implicit()), "i")),
            Term.Select(tname("i"), tname("toString"))
          )
        )
      ))
    }
  }

  // https://github.com/scalameta/scalameta/issues/1843
  test("anonymous-function-#1843-1") {
    assertTerm("_ fun (_.bar)")(AnonymousFunction(ApplyInfix(
      Placeholder(),
      tname("fun"),
      Nil,
      List(AnonymousFunction(Select(Placeholder(), tname("bar"))))
    )))
  }
  test("anonymous-function-#1843-2") {
    assertTerm("_ fun _.bar")(AnonymousFunction(
      ApplyInfix(Placeholder(), tname("fun"), Nil, List(Select(Placeholder(), tname("bar"))))
    ))
  }

  // https://scala-lang.org/files/archive/spec/2.13/06-expressions.html#placeholder-syntax-for-anonymous-functions
  test("anonymous-function-spec-1") {
    assertTerm("_ + 1")(AnonymousFunction(ApplyInfix(Placeholder(), tname("+"), Nil, List(int(1)))))
  }
  test("anonymous-function-spec-2") {
    assertTerm("_ * _")(AnonymousFunction(
      ApplyInfix(Placeholder(), tname("*"), Nil, List(Placeholder()))
    ))
  }
  test("anonymous-function-spec-3") {
    assertTerm("(_: Int) * 2")(AnonymousFunction(
      ApplyInfix(Ascribe(Placeholder(), pname("Int")), tname("*"), Nil, List(int(2)))
    ))
  }
  test("anonymous-function-spec-3.1") {
    assertTerm("1 + (_: Int) * 2 + 1")(AnonymousFunction(ApplyInfix(
      ApplyInfix(
        int(1),
        tname("+"),
        Nil,
        List(ApplyInfix(Ascribe(Placeholder(), pname("Int")), tname("*"), Nil, List(int(2))))
      ),
      tname("+"),
      Nil,
      List(int(1))
    )))
  }
  test("anonymous-function-spec-3.2") {
    assertTerm("(_: Int)")(Ascribe(Placeholder(), pname("Int")))
  }
  test("anonymous-function-spec-4") {
    assertTerm("if (_) x else y")(AnonymousFunction(If(Placeholder(), tname("x"), tname("y"))))
  }
  test("anonymous-function-spec-5") {
    assertTerm("_.map(f)")(AnonymousFunction(
      Apply(Select(Placeholder(), tname("map")), List(tname("f")))
    ))
  }
  test("anonymous-function-spec-6") {
    assertTerm("_.map(_ + 1)")(AnonymousFunction(Apply(
      Select(Placeholder(), tname("map")),
      List(AnonymousFunction(ApplyInfix(Placeholder(), tname("+"), Nil, List(int(1)))))
    )))
  }

  test("anonymous-function-scalafmt-1") {
    assertTerm("foo >>= (_.http(registry, port).map(Option.apply(_)).catchSome { bar })")(ApplyInfix(
      tname("foo"),
      tname(">>="),
      Nil,
      List(AnonymousFunction(Apply(
        Select(
          Apply(
            Select(
              Apply(Select(Placeholder(), tname("http")), List(tname("registry"), tname("port"))),
              tname("map")
            ),
            List(AnonymousFunction(Apply(Select(tname("Option"), tname("apply")), List(Placeholder()))))
          ),
          tname("catchSome")
        ),
        List(Block(List(tname("bar"))))
      )))
    ))
  }
  test("anonymous-function-scalafmt-2") {
    assertTerm("""scalacOptions ~= (_ filterNot (_ startsWith "-Xlint"))""")(ApplyInfix(
      tname("scalacOptions"),
      tname("~="),
      Nil,
      List(AnonymousFunction(ApplyInfix(
        Placeholder(),
        tname("filterNot"),
        Nil,
        List(AnonymousFunction(ApplyInfix(Placeholder(), tname("startsWith"), Nil, List(str("-Xlint")))))
      )))
    ))
  }
  test("anonymous-function-scalafmt-3") {
    assertTerm("`field-names` ~> (`private`(_: _*))")(ApplyInfix(
      tname("field-names"),
      tname("~>"),
      Nil,
      List(AnonymousFunction(Apply(tname("private"), List(Repeated(Placeholder())))))
    ))
  }

  test("(a, b, c)") {
    assertTerm("(a, b, c)")(Term.Tuple(List(tname("a"), tname("b"), tname("c"))))
  }

  test("((a, b, c))") {
    assertTerm("((a, b, c))")(Term.Tuple(List(tname("a"), tname("b"), tname("c"))))
  }

  test("(a, b, c) :: ((a, b, c))") {
    assertTerm("(a, b, c) :: ((a, b, c))") {
      Term.ApplyInfix(
        Term.Tuple(List(tname("a"), tname("b"), tname("c"))),
        tname("::"),
        Nil,
        List(Term.Tuple(List(tname("a"), tname("b"), tname("c"))))
      )
    }
  }

  test("((a, b, c)) :: ((a, b, c))") {
    assertTerm("((a, b, c)) :: ((a, b, c))") {
      Term.ApplyInfix(
        Term.Tuple(List(tname("a"), tname("b"), tname("c"))),
        tname("::"),
        Nil,
        List(Term.Tuple(List(tname("a"), tname("b"), tname("c"))))
      )
    }
  }

  test("((a, b, c)) :: (a, b, c)") {
    assertTerm("((a, b, c)) :: (a, b, c)") {
      Term.ApplyInfix(
        Term.Tuple(List(tname("a"), tname("b"), tname("c"))),
        tname("::"),
        Nil,
        List(tname("a"), tname("b"), tname("c"))
      )
    }
  }

  test("#2720 infix with repeated arg last") {
    assertTerm("a foo (b, c: _*)") {
      Term.ApplyInfix(tname("a"), tname("foo"), Nil, List(tname("b"), Term.Repeated(tname("c"))))
    }
  }
  test("#2720-for-comp") {
    assertTerm("for { `j`: Int <- Seq(4, 5, 6, 7)} yield `j`") {
      Term.ForYield(
        List(Enumerator.Generator(
          Pat.Typed(Pat.Var(tname("j")), pname("Int")),
          Term.Apply(tname("Seq"), List(int(4), int(5), int(6), int(7)))
        )),
        tname("j")
      )
    }
  }

  test("#2720 infix with repeated arg not last") {
    assertNoDiff(
      intercept[ParseException](term("a op (b: _*, c)")).getMessage,
      """|<input>:1: error: repeated argument not allowed here
         |a op (b: _*, c)
         |      ^""".stripMargin
    )
  }

  test("#1384 string") {
    val tq = "\"" * 3
    val exprDq = raw"""("\n", "bar\n", "\nbaz")"""
    val exprTq = s"""($tq\n$tq, ${tq}bar\n$tq, $tq\nbaz$tq)"""
    checkTerm(exprDq, exprDq)(Term.Tuple(List(str("\n"), str("bar\n"), str("\nbaz"))))
    checkTerm(exprTq, exprTq)(Term.Tuple(List(str("\n"), str("bar\n"), str("\nbaz"))))
  }

  test("using-call-site in scala2") {
    checkStat("val a = f()(using a)(using 3, true)")(Defn.Val(
      Nil,
      List(Pat.Var(tname("a"))),
      None,
      Term.Apply(
        Term
          .Apply(Term.Apply(tname("f"), Nil), Term.ArgClause(List(tname("a")), Some(Mod.Using()))),
        Term.ArgClause(List(int(3), bool(true)), Some(Mod.Using()))
      )
    ))
  }

  test("scala3-syntax") {

    runTestError[Term](
      """|() match
         |  case _: Unit => ()""".stripMargin,
      "error: `{` expected but `case` found"
    )

  }

  test("using") {
    assertTerm("Set(using)")(Term.Apply(tname("Set"), List(tname("using"))))

    assertTerm("foo(using, bar)")(Term.Apply(tname("foo"), List(tname("using"), tname("bar"))))

    assertTerm(
      """|{
         |  val using ="asdsa"; 
         |  foo(using: String) 
         |}""".stripMargin
    ) {
      Term.Block(List(
        Defn.Val(Nil, List(Pat.Var(tname("using"))), None, str("asdsa")),
        Term.Apply(tname("foo"), List(Term.Ascribe(tname("using"), pname("String"))))
      ))
    }
  }

  test("implicit closure with ascribe") {
    val code = """|foo {
                  |  implicit a => implicit b => {
                  |    case bar => baz
                  |  }: qux
                  |}
                  |""".stripMargin
    checkTerm(code) {
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            List(tparam(List(Mod.Implicit()), "a")),
            Term.Function(
              List(tparam(List(Mod.Implicit()), "b")),
              Term.Ascribe(
                Term.PartialFunction(List(Case(Pat.Var(tname("bar")), None, tname("baz")))),
                pname("qux")
              )
            )
          ) :: Nil
        ) :: Nil
      )
    }
  }

  test("implicit closure with val") {
    val code = """|foo { implicit a =>
                  |  val bar = baz
                  |  bar
                  |}
                  |""".stripMargin
    checkTerm(code) {
      Term.Apply(
        tname("foo"),
        Term.Block(
          Term.Function(
            Term.ParamClause(List(tparam(List(Mod.Implicit()), "a")), Some(Mod.Implicit())),
            Term.Block(
              List(Defn.Val(Nil, List(Pat.Var(tname("bar"))), None, tname("baz")), tname("bar"))
            )
          ) :: Nil
        ) :: Nil
      )
    }
  }

  test("if-with-parens-no-block [scala2]") {
    assertTerm(
      """|if (isEmpty)
         |  (None, None)
         |else {
         |  (Some(e), Some(f))
         |}
         |""".stripMargin
    )(Term.If(
      tname("isEmpty"),
      Term.Tuple(List(tname("None"), tname("None"))),
      Term.Block(
        Term.Tuple(List(
          Term.Apply(tname("Some"), List(tname("e"))),
          Term.Apply(tname("Some"), List(tname("f")))
        )) :: Nil
      ),
      Nil
    ))
  }

  test("#3050 function without body") {
    val code = """|f { (x1: A, x2: B => C) =>
                  |}
                  |""".stripMargin
    checkTerm(code)(Term.Apply(
      tname("f"),
      Term.Block(
        Term.Function(
          List(
            tparam("x1", "A"),
            tparam(Nil, "x2", Type.Function(Type.FuncParamClause(List(pname("B"))), pname("C")))
          ),
          Term.Block(Nil)
        ) :: Nil
      ) :: Nil
    ))
  }

  test("#3136: block catch handler, in braces") {
    val code = """|try ??? catch {
                  |  val a = 10
                  |  handler(a)
                  |}
                  |""".stripMargin
    checkTerm(code, code)(Term.TryWithHandler(
      tname("???"),
      Term.Block(List(
        Defn.Val(Nil, List(Pat.Var(tname("a"))), None, int(10)),
        Term.Apply(tname("handler"), List(tname("a")))
      )),
      None
    ))
  }

  test("#3138: infix with a following block") {
    val codeOnNextLine = """|{
                            |  Foo bar (2, 3)
                            |  { a }
                            |}
                            |""".stripMargin
    val codeOnSameLine = """|{
                            |  Foo bar (2, 3) { a }
                            |}
                            |""".stripMargin
    val syntaxOnSameLine = """|{
                              |  Foo bar (2, 3) {
                              |    a
                              |  }
                              |}
                              |""".stripMargin
    val treeOnSameLine = Term.Block(
      Term.ApplyInfix(
        tname("Foo"),
        tname("bar"),
        Nil,
        Term.Apply(Term.Tuple(List(int(2), int(3))), Term.Block(List(tname("a"))) :: Nil) :: Nil
      ) :: Nil
    )
    runTestAssert[Stat](codeOnNextLine, Some(syntaxOnSameLine))(treeOnSameLine)
    runTestAssert[Stat](codeOnSameLine, Some(syntaxOnSameLine))(treeOnSameLine)
  }

  test("#3138: apply with a following block") {
    val codeOnNextLine = """|{
                            |  Foo.bar (2, 3)
                            |  { a }
                            |}
                            |""".stripMargin
    val codeOnSameLine = """|{
                            |  Foo.bar (2, 3) { a }
                            |}
                            |""".stripMargin
    val syntaxOnSameLine = """|{
                              |  Foo.bar(2, 3) {
                              |    a
                              |  }
                              |}
                              |""".stripMargin
    val treeOnSameLine = Term.Block(
      Term.Apply(
        Term.Apply(Term.Select(tname("Foo"), tname("bar")), List(int(2), int(3))),
        Term.Block(List(tname("a"))) :: Nil
      ) :: Nil
    )
    runTestAssert[Stat](codeOnNextLine, Some(syntaxOnSameLine))(treeOnSameLine)
    runTestAssert[Stat](codeOnSameLine, Some(syntaxOnSameLine))(treeOnSameLine)
  }

  test("case: inside partial function") {
    runTestAssert[Term](
      """|{
         |  case foo
         |    if true =>
         |    List(bar)
         |}
         |""".stripMargin,
      Some(
        """|{
           |  case foo if true =>
           |    List(bar)
           |}""".stripMargin
      )
    )(Term.PartialFunction(
      Case(
        Pat.Var(tname("foo")),
        Some(bool(true)),
        Term.Apply(tname("List"), List(tname("bar")))
      ) :: Nil
    ))
  }

  test("expr with annotation, then match") {
    val code = """|underlyingStableClassRef(mbr.info.loBound): @unchecked match {
                  |  case ref: TypeRef =>
                  |}""".stripMargin
    val layout = """|(underlyingStableClassRef(mbr.info.loBound): @unchecked) match {
                    |  case ref: TypeRef =>
                    |}""".stripMargin
    runTestAssert[Term](code, Some(layout))(Term.Match(
      Term.Annotate(
        Term.Apply(
          tname("underlyingStableClassRef"),
          List(Term.Select(Term.Select(tname("mbr"), tname("info")), tname("loBound")))
        ),
        List(Mod.Annot(Init(pname("unchecked"), anon, emptyArgClause)))
      ),
      List(Case(Pat.Typed(Pat.Var(tname("ref")), pname("TypeRef")), None, Term.Block(Nil))),
      Nil
    ))
  }

  test("#3220") {
    val code = """|for {
                  |  case (a, b) <- pairs
                  |  x <- a to b
                  |} yield x
                  |""".stripMargin
    val layout = "for ( case (a, b) <- pairs; x <- a to b) yield x"
    runTestAssert[Term](code, Some(layout))(Term.ForYield(
      List(
        Enumerator
          .CaseGenerator(Pat.Tuple(List(Pat.Var(tname("a")), Pat.Var(tname("b")))), tname("pairs")),
        Enumerator.Generator(
          Pat.Var(tname("x")),
          Term.ApplyInfix(tname("a"), tname("to"), Nil, List(tname("b")))
        )
      ),
      tname("x")
    ))
  }

  test("#3224") {
    val code = """|for {
                  |  x2 <- x1
                  |} yield x2
                  |  .x3 {
                  |    case x4
                  |        if x5.x6
                  |          .x7(x8) =>
                  |        x9
                  |  }
                  |""".stripMargin
    val layout = """|for (x2 <- x1) yield x2.x3 {
                    |  case x4 if x5.x6.x7(x8) => x9
                    |}
                    |""".stripMargin
    runTestAssert[Term](code, Some(layout))(Term.ForYield(
      List(Enumerator.Generator(Pat.Var(tname("x2")), tname("x1"))),
      Term.Apply(
        Term.Select(tname("x2"), tname("x3")),
        Term.PartialFunction(
          Case(
            Pat.Var(tname("x4")),
            Some(Term.Apply(
              Term.Select(Term.Select(tname("x5"), tname("x6")), tname("x7")),
              List(tname("x8"))
            )),
            tname("x9")
          ) :: Nil
        ) :: Nil
      )
    ))
  }

  test("match on array-of-wildcard") {
    val code = """|obj match { case arr: Array[Array[_]] => }
                  |""".stripMargin
    val layout = """|obj match {
                    |  case arr: Array[Array[_]] =>
                    |}
                    |""".stripMargin
    runTestAssert[Term](code, Some(layout))(Term.Match(
      tname("obj"),
      Case(
        Pat.Typed(
          Pat.Var(tname("arr")),
          Type
            .Apply(pname("Array"), List(Type.Apply(pname("Array"), List(Type.Wildcard(noBounds)))))
        ),
        None,
        Term.Block(Nil)
      ) :: Nil,
      Nil
    ))
  }

  test("apply with arguments of various complexity") {
    val code = """|sc.submitJob(
                  |  rdd,
                  |  (iter: Iterator[Int]) => iter.toArray,
                  |  partitions.getOrElse(rdd.partitions.indices),
                  |  { case (_, _) => return }: (Int, Array[Int]) => Unit,
                  |  { return }
                  |)""".stripMargin
    val layout =
      """|sc.submitJob(rdd, (iter: Iterator[Int]) => iter.toArray, partitions.getOrElse(rdd.partitions.indices), {
         |  case (_, _) =>
         |    return
         |}: (Int, Array[Int]) => Unit, {
         |  return
         |})
         |""".stripMargin
    val tree = Term.Apply(
      Term.Select(tname("sc"), tname("submitJob")),
      List(
        tname("rdd"),
        Term.Function(
          List(tparam("iter", Type.Apply(pname("Iterator"), List(pname("Int"))))),
          Term.Select(tname("iter"), tname("toArray"))
        ),
        Term.Apply(
          Term.Select(tname("partitions"), tname("getOrElse")),
          List(Term.Select(Term.Select(tname("rdd"), tname("partitions")), tname("indices")))
        ),
        Term.Ascribe(
          Term.PartialFunction(List(
            Case(Pat.Tuple(List(Pat.Wildcard(), Pat.Wildcard())), None, Term.Return(Lit.Unit()))
          )),
          Type.Function(
            List(pname("Int"), Type.Apply(pname("Array"), List(pname("Int")))),
            pname("Unit")
          )
        ),
        Term.Block(List(Term.Return(Lit.Unit())))
      )
    )
    runTestAssert[Term](code, Some(layout))(tree)
  }

  test("scalafmt #3911 for in parens, NL after `(`, NL between") {
    val code = """|for (
                  |  a <- fooa
                  |  b <- foob
                  |  if a === b
                  |) yield a
                  |""".stripMargin
    val error = """|<input>:3: error: `)` expected but `<-` found
                   |  b <- foob
                   |    ^""".stripMargin
    runTestError[Term](code, error)
  }

  test("scalafmt #3911 for in parens, NL after `(`, `;` between") {
    val code = """|for (
                  |  a <- fooa;
                  |  b <- foob;
                  |  if a === b
                  |) yield a
                  |""".stripMargin
    val layout = "for (a <- fooa; b <- foob; if a === b) yield a"
    val tree = Term.ForYield(
      List(
        Enumerator.Generator(Pat.Var(tname("a")), tname("fooa")),
        Enumerator.Generator(Pat.Var(tname("b")), tname("foob")),
        Enumerator.Guard(Term.ApplyInfix(tname("a"), tname("==="), Nil, List(tname("b"))))
      ),
      tname("a")
    )
    runTestAssert[Term](code, layout)(tree)
  }

  test("scalafmt #3911 for in parens, no NL after `(`, NL between") {
    val code = """|for (a <- fooa
                  |     b <- foob
                  |     if a === b) yield a
                  |""".stripMargin
    val error = """|<input>:2: error: `)` expected but `<-` found
                   |     b <- foob
                   |       ^""".stripMargin
    runTestError[Term](code, error)
  }

  test("scalafmt #3911 for in parens, no NL after `(`, `;` between") {
    val code = """|for (a <- fooa;
                  |     b <- foob;
                  |     if a === b) yield a
                  |""".stripMargin
    val layout = "for (a <- fooa; b <- foob; if a === b) yield a"
    val tree = Term.ForYield(
      List(
        Enumerator.Generator(Pat.Var(tname("a")), tname("fooa")),
        Enumerator.Generator(Pat.Var(tname("b")), tname("foob")),
        Enumerator.Guard(Term.ApplyInfix(tname("a"), tname("==="), Nil, List(tname("b"))))
      ),
      tname("a")
    )
    runTestAssert[Term](code, layout)(tree)
  }

  test("scalafmt #3911 for in parens, no NL after `(`, NL between, no NL before guard") {
    val code = """|for (a <- fooa if
                  |       a > 0) yield a
                  |""".stripMargin
    val layout = "for (a <- fooa; if a > 0) yield a"
    val tree = Term.ForYield(
      List(
        Enumerator.Generator(Pat.Var(tname("a")), tname("fooa")),
        Enumerator.Guard(Term.ApplyInfix(tname("a"), tname(">"), Nil, List(lit(0))))
      ),
      tname("a")
    )
    runTestAssert[Term](code, layout)(tree)
  }

  test("scalafmt #3911 for in parens, no NL after `(`, `;` between, no NL before guard op") {
    val code = """|for (a <- fooa if a >
                  |       0) yield a
                  |""".stripMargin
    val layout = "for (a <- fooa; if a > 0) yield a"
    val tree = Term.ForYield(
      List(
        Enumerator.Generator(Pat.Var(tname("a")), tname("fooa")),
        Enumerator.Guard(Term.ApplyInfix(tname("a"), tname(">"), Nil, List(lit(0))))
      ),
      tname("a")
    )
    runTestAssert[Term](code, layout)(tree)
  }

  test("#3713 cond in parens within enums") {
    val code = """|for (m <- decls
                  |    if oneCond
                  |      && (cond)
                  |      && satisfiable) {}
                  |
                  |""".stripMargin
    val layout = "for (m <- decls; if oneCond && cond && satisfiable) {}"
    val tree = Term.For(
      List(
        Enumerator.Generator(Pat.Var(tname("m")), tname("decls")),
        Enumerator.Guard(Term.ApplyInfix(
          Term.ApplyInfix(tname("oneCond"), tname("&&"), Nil, List(tname("cond"))),
          tname("&&"),
          Nil,
          List(tname("satisfiable"))
        ))
      ),
      blk()
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  Seq( // tuples of (operator, whether it's an assignment)
    ("foo", false),
    ("!=", false),
    ("+=", true),
    ("~=", true),
    (":=", true)
  ).foreach { case (op, isAssignment) =>
    test(s"isAssignmentOp($op) == $isAssignment") {
      matchSubStructure[Term](op, { case x: Name if x.isAssignmentOp == isAssignment => })
      matchSubStructure[Term](
        s"a $op b",
        { case x: Member.Infix if x.isAssignment == isAssignment => }
      )
    }
  }

  test("#3979 w/ grouped whitespace") {
    implicit def tokenizerOptions: TokenizerOptions = new TokenizerOptions(groupWhitespace = true)

    val code = """|for {
                  |  _ <-
                  |    if (a) {
                  |      b
                  |    } else {
                  |      c
                  |    }
                  |
                  |  _ <- d
                  |} yield e
                  |""".stripMargin
    val layout = """|for (_ <- if (a) {
                    |  b
                    |} else {
                    |  c
                    |}; _ <- d) yield e
                    |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(
        Enumerator.Generator(
          Pat.Wildcard(),
          Term.If(tname("a"), Term.Block(List(tname("b"))), Term.Block(List(tname("c"))), Nil)
        ),
        Enumerator.Generator(Pat.Wildcard(), tname("d"))
      )),
      tname("e")
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3979 w/ granular whitespace") {
    implicit def tokenizerOptions: TokenizerOptions = new TokenizerOptions(groupWhitespace = false)

    val code = """|for {
                  |  _ <-
                  |    if (a) {
                  |      b
                  |    } else {
                  |      c
                  |    }
                  |
                  |  _ <- d
                  |} yield e
                  |""".stripMargin
    val layout = """|for (_ <- if (a) {
                    |  b
                    |} else {
                    |  c
                    |}; _ <- d) yield e
                    |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(
        Enumerator
          .Generator(Pat.Wildcard(), Term.If(tname("a"), blk(tname("b")), blk(tname("c")), Nil)),
        Enumerator.Generator(Pat.Wildcard(), tname("d"))
      )),
      tname("e")
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
