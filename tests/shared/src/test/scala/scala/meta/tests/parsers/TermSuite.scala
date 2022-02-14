package scala.meta.tests
package parsers

import scala.meta._, Term.{Name => _, _}, Name.{Anonymous, Indeterminate}
import scala.meta.dialects.Scala211

class TermSuite extends ParseSuite {

  private def assertTerm(expr: String)(tree: Tree): Unit = {
    assertEquals(term(expr).structure, tree.structure)
  }

  test("x") {
    assertTerm("x") {
      Term.Name("x")
    }
  }

  test("`x`") {
    assertTerm("`x`") {
      Term.Name("x")
    }
  }

  test("a.b.c") {
    assertTerm("a.b.c") {
      Select(Select(Term.Name("a"), Term.Name("b")), Term.Name("c"))
    }
  }

  test("a.b c") {
    assertTerm("a.b c") {
      Select(Select(Term.Name("a"), Term.Name("b")), Term.Name("c"))
    }
  }

  test("foo.this") {
    assertTerm("foo.this") {
      This(Indeterminate("foo"))
    }
  }

  test("this") {
    assertTerm("this") {
      This(Anonymous())
    }
  }

  test("a.super[b].c") {
    assertTerm("a.super[b].c") {
      Select(Super(Indeterminate("a"), Indeterminate("b")), Term.Name("c"))
    }
  }

  test("super[b].c") {
    assertTerm("super[b].c") {
      Select(Super(Anonymous(), Indeterminate("b")), Term.Name("c"))
    }
  }

  test("a.super.c") {
    assertTerm("a.super.c") {
      Select(Super(Indeterminate("a"), Anonymous()), Term.Name("c"))
    }
  }

  test("super.c") {
    assertTerm("super.c") {
      Select(Super(Anonymous(), Anonymous()), Term.Name("c"))
    }
  }

  test("s\"a $b c\"") {
    assertTerm("s\"a $b c\"") {
      Interpolate(
        Term.Name("s"),
        Lit.String("a ") :: Lit.String(" c") :: Nil,
        Term.Name("b") :: Nil
      )
    }
  }

  test("f(0)") {
    assertTerm("f(0)") {
      Apply(Term.Name("f"), Lit.Int(0) :: Nil)
    }
  }

  test("f(x = 0)") {
    assertTerm("f(x = 0)") {
      Apply(Term.Name("f"), Assign(Term.Name("x"), Lit.Int(0)) :: Nil)
    }
  }

  test("f(x: _*)") {
    assertTerm("f(x: _*)") {
      Apply(Term.Name("f"), Repeated(Term.Name("x")) :: Nil)
    }
  }

  test("f((x: _*))") {
    assertTerm("f((x: _*))") {
      Apply(Term.Name("f"), Repeated(Term.Name("x")) :: Nil)
    }
  }

  test("f(x = xs: _*)") {
    assertTerm("f(x = xs: _*)") {
      Term.Apply(Term.Name("f"), List(Assign(Term.Name("x"), Repeated(Term.Name("xs")))))
    }
  }

  test("f(x = (xs: _*))") {
    assertTerm("f(x = (xs: _*))") {
      Term.Apply(Term.Name("f"), List(Assign(Term.Name("x"), Repeated(Term.Name("xs")))))
    }
  }

  test("a + ()") {
    assertTerm("a + ()") {
      ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Nil)
    }
  }

  test("a + b") {
    assertTerm("a + b") {
      ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Term.Name("b") :: Nil)
    }
  }

  test("a + b + c") {
    assertTerm("a + b + c") {
      ApplyInfix(
        ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Term.Name("b") :: Nil),
        Term.Name("+"),
        Nil,
        Term.Name("c") :: Nil
      )
    }
  }

  test("a :: b") {
    assertTerm("a :: b") {
      ApplyInfix(Term.Name("a"), Term.Name("::"), Nil, Term.Name("b") :: Nil)
    }
  }

  test("a :: b :: c") {
    assertTerm("a :: b :: c") {
      ApplyInfix(
        Term.Name("a"),
        Term.Name("::"),
        Nil,
        ApplyInfix(Term.Name("b"), Term.Name("::"), Nil, Term.Name("c") :: Nil) :: Nil
      )
    }
  }

  test("!a") {
    assertTerm("!a") {
      ApplyUnary(Term.Name("!"), Term.Name("a"))
    }
  }

  test("!(a: _*)") {
    assertTerm("!(a: _*)") {
      ApplyUnary(Term.Name("!"), Repeated(Term.Name("a")))
    }
  }

  test("a = true") {
    assertTerm("a = true") {
      Assign(Term.Name("a"), Lit.Boolean(true))
    }
  }

  test("a(0) = true") {
    assertTerm("a(0) = true") {
      Assign(Apply(Term.Name("a"), (Lit.Int(0) :: Nil)), Lit.Boolean(true))
    }
  }

  test("return") {
    assertTerm("return") {
      Return(Lit.Unit())
    }
  }

  test("return 1") {
    assertTerm("return 1") {
      Return(Lit.Int(1))
    }
  }

  test("throw 1") {
    assertTerm("throw 1") {
      Throw(Lit.Int(1))
    }
  }

  test("1: Int") {
    assertTerm("1: Int") {
      Ascribe(Lit.Int(1), Type.Name("Int"))
    }
  }

  test("1: @foo") {
    assertTerm("1: @foo") {
      Annotate(Lit.Int(1), Mod.Annot(Init(Type.Name("foo"), Name.Anonymous(), Nil)) :: Nil)
    }
  }

  test("(true, false)") {
    assertTerm("(true, false)") {
      Tuple(Lit.Boolean(true) :: Lit.Boolean(false) :: Nil)
    }
  }

  test("{ true; false }") {
    assertTerm("{ true; false }") {
      Block(Lit.Boolean(true) :: Lit.Boolean(false) :: Nil)
    }
  }

  test("{ true }") {
    assertTerm("{ true }") {
      Block(Lit.Boolean(true) :: Nil)
    }
  }

  test("if (true) true else false") {
    assertTerm("if (true) true else false") {
      If(Lit.Boolean(true), Lit.Boolean(true), Lit.Boolean(false))
    }
  }

  test("if (true) true; else false") {
    assertTerm("if (true) true; else false") {
      If(Lit.Boolean(true), Lit.Boolean(true), Lit.Boolean(false))
    }
  }

  test("if (true) true") {
    assertTerm("if (true) true") {
      If(Lit.Boolean(true), Lit.Boolean(true), Lit.Unit())
    }
  }

  test("if (true && '' match...") {
    val file =
      """|
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
            Lit.Boolean(true),
            Term.Name("&&"),
            Nil,
            List(
              Term.Match(
                Lit.String(""),
                List(Case(Pat.Var(Term.Name("other")), None, Lit.Boolean(true)))
              )
            )
          ),
          Term.Name("&&"),
          Nil,
          List(Lit.Boolean(true))
        ),
        Lit.String(""),
        Lit.Unit()
      )
    }
  }

  test("() => x") {
    assertTerm("() => x") {
      Term.Function(Nil, Term.Name("x"))
    }
    val Term.Function(Nil, Term.Name("x")) = blockStat("() => x")
    val Term.Function(Nil, Term.Name("x")) = templStat("() => x")
  }

  test("(()) => x") {
    assertTerm("(()) => x") {
      Term.Function(Nil, Term.Name("x"))
    }
    val Term.Function(Nil, Term.Name("x")) = blockStat("(()) => x")
    val Term.Function(Nil, Term.Name("x")) = templStat("(()) => x")
  }

  test("x => x") {
    assertTerm("x => x") {
      Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x"))
    }
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x")) =
      blockStat("x => x")
    intercept[ParseException] { templStat("x => x") }
  }

  test("(x) => x") {
    assertTerm("(x) => x") {
      Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x"))
    }
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x")) =
      blockStat("(x) => x")
    intercept[ParseException] { templStat("(x) => x") }
  }

  test("_ => x") {
    assertTerm("_ => x") {
      Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Name("x"))
    }
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Name("x")) =
      blockStat("_ => x")
    intercept[ParseException] { templStat("_ => x") }
  }

  test("(_) => x") {
    assertTerm("(_) => x") {
      Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Name("x"))
    }
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Name("x")) =
      blockStat("(_) => x")
    intercept[ParseException] { templStat("(_) => x") }
  }

  test("x: Int => x") {
    // LAWL: this is how scalac's parser works
    assertTerm("x: Int => x") {
      Term.Ascribe(Term.Name("x"), Type.Function(List(Type.Name("Int")), Type.Name("x")))
    }
    val Term.Function(
      List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = blockStat("x: Int => x")
    intercept[ParseException] { templStat("x: Int => x") }
  }

  test("(x: Int) => x") {
    assertTerm("(x: Int) => x") {
      Term.Function(
        List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)),
        Term.Name("x")
      )
    }
    val Term.Function(
      List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = blockStat("(x: Int) => x")
    val Term.Function(
      List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = templStat("(x: Int) => x")
  }

  test("_: Int => x") {
    assertTerm("_: Int => x") {
      Term.Ascribe(Term.Placeholder(), Type.Function(List(Type.Name("Int")), Type.Name("x")))
    }
    val Term.Function(
      List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = blockStat("_: Int => x")
    intercept[ParseException] { templStat("_: Int => x") }
  }

  test("(_: Int) => x") {
    assertTerm("(_: Int) => x") {
      Term.Function(
        List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)),
        Term.Name("x")
      )
    }
    val Term.Function(
      List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = blockStat("(_: Int) => x")
    val Term.Function(
      List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = templStat("(_: Int) => x")
  }

  test("x: Int, y: Int => x") {
    intercept[ParseException] { term("x: Int, y: Int => x") }
    intercept[ParseException] { blockStat("x: Int, y: Int => x") }
    intercept[ParseException] { templStat("x: Int, y: Int => x") }
  }

  test("(x: Int, y: Int) => x") {
    assertTerm("(x: Int, y: Int) => x") {
      Term.Function(
        List(
          Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None),
          Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)
        ),
        Term.Name("x")
      )
    }
    val Term.Function(
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)
      ),
      Term.Name("x")
    ) = blockStat("(x: Int, y: Int) => x")
    val Term.Function(
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)
      ),
      Term.Name("x")
    ) = templStat("(x: Int, y: Int) => x")
  }

  test("{ implicit x => () }") {
    assertTerm("{ implicit x => () }") {
      Block(
        Function(
          Term.Param(Mod.Implicit() :: Nil, Term.Name("x"), None, None) :: Nil,
          Lit.Unit()
        ) :: Nil
      )
    }
  }

  test("1 match { case 1 => true }") {
    assertTerm("1 match { case 1 => true }") {
      Match(Lit.Int(1), Case(Lit.Int(1), None, Lit.Boolean(true)) :: Nil)
    }
  }

  test("1 match { case 1 => }") {
    assertTerm("1 match { case 1 => }") {
      Match(Lit.Int(1), Case(Lit.Int(1), None, Term.Block(Nil)) :: Nil)
    }
  }

  test("1 match { case 1 if true => }") {
    assertTerm("1 match { case 1 if true => }") {
      Match(Lit.Int(1), Case(Lit.Int(1), Some(Lit.Boolean(true)), Term.Block(Nil)) :: Nil)
    }
  }

  test("1 match { case case 1 if true => }") {
    val intercepted = intercept[ParseException] {
      term("1 match { case case 1 if true => }")
    }
    assertNoDiff(intercepted.shortMessage, "Unexpected `case`")
  }

  test("try 1") {
    assertTerm("try 1") {
      Try(Lit.Int(1), Nil, None)
    }
  }

  test("try 1 catch 1") {
    assertTerm("try 1 catch 1") {
      TryWithHandler(Lit.Int(1), Lit.Int(1), None)
    }
  }

  test("try (2)") {
    assertTerm("try (2)") {
      Try(Lit.Int(2), Nil, None)
    }
  }

  test("try 1 catch { case _ => }") {
    assertTerm("try 1 catch { case _ => }") {
      Try(Lit.Int(1), Case(Pat.Wildcard(), None, Term.Block(Nil)) :: Nil, None)
    }
  }

  test("try 1 finally 1") {
    assertTerm("try 1 finally 1") {
      Try(Lit.Int(1), Nil, Some(Lit.Int(1)))
    }
  }

  test("{ case 1 => () }") {
    assertTerm("{ case 1 => () }") {
      PartialFunction(Case(Lit.Int(1), None, Lit.Unit()) :: Nil)
    }
  }

  test("while (true) false") {
    assertTerm("while (true) false") {
      While(Lit.Boolean(true), Lit.Boolean(false))
    }
  }

  test("do false while(true)") {
    assertTerm("do false while(true)") {
      Do(Lit.Boolean(false), Lit.Boolean(true))
    }
  }

  test("for (a <- b; if c; x = a) x") {
    assertTerm("for (a <- b; if c; x = a) x") {
      For(
        List(
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("b")),
          Enumerator.Guard(Term.Name("c")),
          Enumerator.Val(Pat.Var(Term.Name("x")), Term.Name("a"))
        ),
        Term.Name("x")
      )
    }
  }

  test("for (a <- b; if c; x = a) yield x") {
    assertTerm("for (a <- b; if c; x = a) yield x") {
      ForYield(
        List(
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("b")),
          Enumerator.Guard(Term.Name("c")),
          Enumerator.Val(Pat.Var(Term.Name("x")), Term.Name("a"))
        ),
        Term.Name("x")
      )
    }
  }

  test("f(_)") {
    assertTerm("f(_)")(
      Apply(Term.Name("f"), List(Placeholder()))
    )
  }

  test("_ + 1") {
    assertTerm("_ + 1")(
      ApplyInfix(Placeholder(), Term.Name("+"), Nil, Lit.Int(1) :: Nil)
    )
  }

  test("1 + _") {
    assertTerm("1 + _")(
      ApplyInfix(Lit.Int(1), Term.Name("+"), Nil, Placeholder() :: Nil)
    )
  }

  test("f _") {
    assertTerm("f _") {
      Eta(Term.Name("f"))
    }
  }

  test("new {}") {
    assertTerm("new {}") {
      NewAnonymous(Template(Nil, Nil, EmptySelf(), Nil))
    }
  }

  test("new { x }") {
    assertTerm("new { x }") {
      NewAnonymous(Template(Nil, Nil, EmptySelf(), List(Term.Name("x"))))
    }
  }

  test("new A") {
    assertTerm("new A") {
      New(Init(Type.Name("A"), Name.Anonymous(), Nil))
    }
  }

  test("new A(xs: _*)") {
    assertTerm("new A(xs: _*)") {
      New(Init(Type.Name("A"), Name.Anonymous(), List(List(Term.Repeated(Term.Name("xs"))))))
    }
  }

  test("new A {}") {
    assertTerm("new A {}") {
      NewAnonymous(
        Template(Nil, Init(Type.Name("A"), Name.Anonymous(), Nil) :: Nil, EmptySelf(), Nil)
      )
    }
  }

  test("new A with B") {
    assertTerm("new A with B") {
      NewAnonymous(
        Template(
          Nil,
          Init(Type.Name("A"), Name.Anonymous(), Nil) ::
            Init(Type.Name("B"), Name.Anonymous(), Nil) :: Nil,
          EmptySelf(),
          Nil
        )
      )
    }
  }

  test("new { val x: Int = 1 } with A") {
    assertTerm("new { val x: Int = 1 } with A") {
      NewAnonymous(
        Template(
          Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), Some(Type.Name("Int")), Lit.Int(1)) :: Nil,
          Init(Type.Name("A"), Name.Anonymous(), Nil) :: Nil,
          EmptySelf(),
          Nil
        )
      )
    }
  }

  test("new { self: T => }") {
    assertTerm("new { self: T => }") {
      NewAnonymous(Template(Nil, Nil, Self(Term.Name("self"), Some(Type.Name("T"))), Nil))
    }
  }

  test("a + (b = c)") {
    assertTerm("a + (b = c)") {
      ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Assign(Term.Name("b"), Term.Name("c")) :: Nil)
    }
  }

  test("(a = b) + c") {
    assertTerm("(a = b) + c") {
      ApplyInfix(Assign(Term.Name("a"), Term.Name("b")), Term.Name("+"), Nil, Term.Name("c") :: Nil)
    }
  }

  test("a + (b = c).d") {
    assertTerm("a + (b = c).d") {
      ApplyInfix(
        Term.Name("a"),
        Term.Name("+"),
        Nil,
        Select(Assign(Term.Name("b"), Term.Name("c")), Term.Name("d")) :: Nil
      )
    }
  }

  test("a + (b: _*)") {
    assertTerm("a + (b: _*)") {
      ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Repeated(Term.Name("b")) :: Nil)
    }
  }

  test("a + ((b: _*))") {
    assertTerm("a + ((b: _*))") {
      ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Repeated(Term.Name("b")) :: Nil)
    }
  }

  test("local class") {
    assertTerm("{ case class C(x: Int); }") {
      Term.Block(
        List(
          Defn.Class(
            List(Mod.Case()),
            Type.Name("C"),
            Nil,
            Ctor.Primary(
              Nil,
              Name.Anonymous(),
              List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)))
            ),
            EmptyTemplate()
          )
        )
      )
    }
  }

  test("xml literal - 1") {
    assertTerm("""{
      val x = <p/>
      val y = x
    }""") {
      Term.Block(
        List(
          Defn
            .Val(Nil, List(Pat.Var(Term.Name("x"))), None, Term.Xml(List(Lit.String("<p/>")), Nil)),
          Defn.Val(Nil, List(Pat.Var(Term.Name("y"))), None, Term.Name("x"))
        )
      )
    }
  }

  test("implicit closure") {
    assertTerm("Action { implicit request: Request[AnyContent] => Ok }") {
      Term.Apply(
        Term.Name("Action"),
        List(
          Term.Block(
            List(
              Term.Function(
                List(
                  Term.Param(
                    List(Mod.Implicit()),
                    Term.Name("request"),
                    Some(Type.Apply(Type.Name("Request"), List(Type.Name("AnyContent")))),
                    None
                  )
                ),
                Term.Name("Ok")
              )
            )
          )
        )
      )
    }
  }

  test("#312") {
    assertTerm("""{
      val x = yz: (Y, Z)
      (x, x)
    }""") {
      Term.Block(
        List(
          Defn.Val(
            Nil,
            List(Pat.Var(Term.Name("x"))),
            None,
            Term.Ascribe(Term.Name("yz"), Type.Tuple(List(Type.Name("Y"), Type.Name("Z"))))
          ),
          Term.Tuple(List(Term.Name("x"), Term.Name("x")))
        )
      )
    }
  }

  test("spawn { var v: Int = _; ??? }") {
    assertTerm("spawn { var v: Int = _; ??? }") {
      Term.Apply(
        Term.Name("spawn"),
        List(
          Term.Block(
            List(
              Defn.Var(Nil, List(Pat.Var(Term.Name("v"))), Some(Type.Name("Int")), None),
              Term.Name("???")
            )
          )
        )
      )
    }
  }

  test("#345") {
    assertTerm("""x match {
      case x => true
      // sobaka
      case y => y
    }""") {
      Term.Match(
        Term.Name("x"),
        List(
          Case(Pat.Var(Term.Name("x")), None, Lit.Boolean(true)),
          Case(Pat.Var(Term.Name("y")), None, Term.Name("y"))
        ),
        Nil
      )
    }
  }

  test("a + (bs: _*) * c") {
    intercept[ParseException] { term("a + (bs: _*) * c") }
  }

  test("a + b: _*") {
    intercept[ParseException] { term("a + b: _*") }
  }

  test("foo(a + b: _*)") {
    assertTerm("foo(a + b: _*)") {
      Term.Apply(
        Term.Name("foo"),
        List(
          Term.Repeated(Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("b"))))
        )
      )
    }
  }

  test("a + (c, d) * e") {
    assertTerm("a + (c, d) * e") {
      Term.ApplyInfix(
        Term.Name("a"),
        Term.Name("+"),
        Nil,
        List(
          Term.ApplyInfix(
            Term.Tuple(List(Term.Name("c"), Term.Name("d"))),
            Term.Name("*"),
            Nil,
            List(Term.Name("e"))
          )
        )
      )
    }
  }

  test("a * (c, d) + e") {
    assertTerm("a * (c, d) + e") {
      Term.ApplyInfix(
        Term.ApplyInfix(Term.Name("a"), Term.Name("*"), Nil, List(Term.Name("c"), Term.Name("d"))),
        Term.Name("+"),
        Nil,
        List(Term.Name("e"))
      )
    }
  }

  test("(a + b) c") {
    assertTerm("(a + b) c") {
      Term.Select(
        Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("b"))),
        Term.Name("c")
      )
    }
  }

  test("a + b c") {
    assertTerm("a + b c") {
      Term.Select(
        Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("b"))),
        Term.Name("c")
      )
    }
  }

  test("disallow parse[Stat] on statseqs") {
    intercept[ParseException] { stat("hello; world") }
  }

  test("\"stat;\".parse[Stat]") {
    val Term.Name("stat") = stat("stat;")
  }

  test("\"stat;\".parse[Term]") {
    intercept[ParseException] { term("stat;") }
  }

  test("$_") {
    intercept[ParseException](term(""" q"x + $_" """))
  }

  test("!x = y") {
    assertTerm("!x = y") {
      Term.Assign(Term.ApplyUnary(Term.Name("!"), Term.Name("x")), Term.Name("y"))
    }
  }

  test("x = (ys: _*)") {
    assertTerm("x = (ys: _*)") {
      Term.Assign(Term.Name("x"), Term.Repeated(Term.Name("ys")))
    }
  }

  test("!(arr.cast[Ptr[Byte]] + sizeof[Ptr[_]]).cast[Ptr[Int]] = length") {
    assertTerm("!(arr.cast[Ptr[Byte]] + sizeof[Ptr[_]]).cast[Ptr[Int]] = length") {
      Term.Assign(
        Term.ApplyUnary(
          Term.Name("!"),
          Term.ApplyType(
            Term.Select(
              Term.ApplyInfix(
                Term.ApplyType(
                  Term.Select(Term.Name("arr"), Term.Name("cast")),
                  List(Type.Apply(Type.Name("Ptr"), List(Type.Name("Byte"))))
                ),
                Term.Name("+"),
                Nil,
                List(
                  Term.ApplyType(
                    Term.Name("sizeof"),
                    List(
                      Type.Apply(Type.Name("Ptr"), List(Type.Placeholder(Type.Bounds(None, None))))
                    )
                  )
                )
              ),
              Term.Name("cast")
            ),
            List(Type.Apply(Type.Name("Ptr"), List(Type.Name("Int"))))
          )
        ),
        Term.Name("length")
      )
    }
  }

  test("(x ++ y)[T]") {
    assertTerm("(x ++ y)[T]") {
      Term.ApplyType(
        Term.ApplyInfix(Term.Name("x"), Term.Name("++"), Nil, List(Term.Name("y"))),
        List(Type.Name("T"))
      )
    }
  }

  test(" structHydrators map { _[K]() } ") {
    assertTerm(" structHydrators map { _[K]() } ") {
      Term.ApplyInfix(
        Term.Name("structHydrators"),
        Term.Name("map"),
        Nil,
        List(
          Term.Block(
            List(Term.Apply(Term.ApplyType(Term.Placeholder(), List(Type.Name("K"))), Nil))
          )
        )
      )
    }
  }

  test(" new C()[String]() ") {
    assertTerm(" new C()[String]() ") {
      Term.Apply(
        Term.ApplyType(
          New(Init(Type.Name("C"), Name.Anonymous(), List(List()))),
          List(Type.Name("String"))
        ),
        Nil
      )
    }
  }

  test("#492 parse Unit in infix operations") {
    assertTerm("x == () :: Nil") {
      Term.ApplyInfix(
        Term.Name("x"),
        Term.Name("=="),
        Nil,
        List(Term.ApplyInfix(Lit.Unit(), Term.Name("::"), Nil, List(Term.Name("Nil"))))
      )
    }
  }

  test("#492 parse hlist with Unit") {
    assertTerm(""""foo" :: () :: true :: HNil""") {
      Term.ApplyInfix(
        Lit.String("foo"),
        Term.Name("::"),
        Nil,
        List(
          Term.ApplyInfix(
            Lit.Unit(),
            Term.Name("::"),
            Nil,
            List(Term.ApplyInfix(Lit.Boolean(true), Term.Name("::"), Nil, List(Term.Name("HNil"))))
          )
        )
      )
    }
  }

  test("nested-braces-in-paren") {
    val code = """(if (bar) {
        if (foo) { doFoo() }
        val x = 2
      })
    """

    assertTerm(code) {
      Term.If(
        Term.Name("bar"),
        Term.Block(
          List(
            Term.If(
              Term.Name("foo"),
              Term.Block(List(Term.Apply(Term.Name("doFoo"), Nil))),
              Lit.Unit()
            ),
            Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.Int(2))
          )
        ),
        Lit.Unit()
      )
    }
  }

  test("fstring-interpolation") {
    assertTerm("""f"\\u$oct%04x"""") {
      Term.Interpolate(
        Term.Name("f"),
        List(Lit.String("\\\\u"), Lit.String("%04x")),
        List(Term.Name("oct"))
      )
    }
  }

  test("typed-interpolation") {
    assertTerm("""c"something"[String]""") {
      Term.ApplyType(
        Term.Interpolate(Term.Name("c"), List(Lit.String("something")), Nil),
        List(Type.Name("String"))
      )
    }
  }

  test("block-arg") {
    val res = term("new Foo({str => str.length})")
    val termList = q"List($res)"
    assertEquals(term(termList.syntax).structure, termList.structure)
  }

  test("implicit-closure") {
    assertTerm("""|function { implicit c =>
                  |  {
                  |    case bar => foo
                  |  }
                  |}""".stripMargin) {
      Term.Apply(
        Term.Name("function"),
        List(
          Term.Block(
            List(
              Term.Function(
                List(Term.Param(List(Mod.Implicit()), Term.Name("c"), None, None)),
                Term.PartialFunction(List(Case(Pat.Var(Term.Name("bar")), None, Term.Name("foo"))))
              )
            )
          )
        )
      )
    }
  }

  test("type-partial-function") {
    val res =
      stat("""|val dynamicStrategy = resharding(
              |  { fakePartitionId: Int =>
              |    {
              |      case sendBox: SendBox.Args => 
              |    }: PartialFunction[ThriftStructIface, Unit]
              |  }
              |)
              |""".stripMargin)

    val Defn.Val(
      Nil,
      List(Pat.Var(Term.Name("dynamicStrategy"))),
      None,
      Term.Apply(
        Term.Name("resharding"),
        List(
          Term.Block(
            List(
              Term.Function(
                List(Term.Param(Nil, Term.Name("fakePartitionId"), Some(Type.Name("Int")), None)),
                Term.Ascribe(
                  Term.PartialFunction(
                    List(
                      Case(
                        Pat.Typed(
                          Pat.Var(Term.Name("sendBox")),
                          Type.Select(Term.Name("SendBox"), Type.Name("Args"))
                        ),
                        None,
                        Term.Block(Nil)
                      )
                    )
                  ),
                  Type.Apply(
                    Type.Name("PartialFunction"),
                    List(Type.Name("ThriftStructIface"), Type.Name("Unit"))
                  )
                )
              )
            )
          )
        )
      )
    ) = res
  }

  test("partial-function-returning-implicit-closure") {
    assertTerm("""|{
                  |  case true => implicit i => "xxx"
                  |  case false => implicit i => i.toString
                  |}""".stripMargin) {
      Term.PartialFunction(
        List(
          Case(
            Lit.Boolean(true),
            None,
            Term.Function(
              List(Term.Param(List(Mod.Implicit()), Term.Name("i"), None, None)),
              Lit.String("xxx")
            )
          ),
          Case(
            Lit.Boolean(false),
            None,
            Term.Function(
              List(Term.Param(List(Mod.Implicit()), Term.Name("i"), None, None)),
              Term.Select(Term.Name("i"), Term.Name("toString"))
            )
          )
        )
      )
    }
  }
}
