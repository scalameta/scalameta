package scala.meta.tests
package parsers

import scala.meta._, Term.{Name => _, _}, Name.{Anonymous, Indeterminate}
import scala.meta.dialects.Scala211

class TermSuite extends ParseSuite {

  implicit def parseTerm(code: String, dialect: Dialect): Term = term(code)(dialect)

  private def assertTerm(expr: String)(tree: Tree): Unit = {
    assertTree(term(expr))(tree)
  }

  private def checkTerm(expr: String, syntax: String = null)(tree: Tree): Unit = {
    checkTree(term(expr), syntax)(tree)
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
    val Term.Apply.After_4_6_0(
      Term.Name("f"),
      Term.ArgClause(List(Assign(Term.Name("x"), Repeated(Term.Name("xs")))), None)
    ) = term("f(x = xs: _*)")
    val Term.Apply.internal.Latest(
      Term.Name("f"),
      Term.ArgClause(List(Assign(Term.Name("x"), Repeated(Term.Name("xs")))), None)
    ) = term("f(x = xs: _*)")
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
      Annotate(
        Lit.Int(1),
        Mod.Annot(Init(Type.Name("foo"), Name.Anonymous(), emptyArgClause)) :: Nil
      )
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
      Term.Function(List(Term.Param(Nil, Name.Placeholder(), None, None)), Term.Name("x"))
    }
    val Term.Function(List(Term.Param(Nil, Name.Placeholder(), None, None)), Term.Name("x")) =
      blockStat("_ => x")
    intercept[ParseException] { templStat("_ => x") }
  }

  test("(_) => x") {
    assertTerm("(_) => x") {
      Term.Function(List(Term.Param(Nil, Name.Placeholder(), None, None)), Term.Name("x"))
    }
    val Term.Function(List(Term.Param(Nil, Name.Placeholder(), None, None)), Term.Name("x")) =
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
      Ascribe(Placeholder(), Type.Function(List(Type.Name("Int")), Type.Name("x")))
    }
    val Term.Function(
      List(Term.Param(Nil, Name.Placeholder(), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = blockStat("_: Int => x")
    intercept[ParseException] { templStat("_: Int => x") }
  }

  test("(_: Int) => x") {
    assertTerm("(_: Int) => x") {
      Term.Function(
        List(Term.Param(Nil, Name.Placeholder(), Some(Type.Name("Int")), None)),
        Term.Name("x")
      )
    }
    val Term.Function(
      List(Term.Param(Nil, Name.Placeholder(), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = blockStat("(_: Int) => x")
    val Term.Function(
      List(Term.Param(Nil, Name.Placeholder(), Some(Type.Name("Int")), None)),
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
      AnonymousFunction(Apply(Term.Name("f"), List(Placeholder())))
    )
  }

  test("_ + 1") {
    assertTerm("_ + 1")(
      AnonymousFunction(ApplyInfix(Placeholder(), Term.Name("+"), Nil, Lit.Int(1) :: Nil))
    )
  }

  test("1 + _") {
    assertTerm("1 + _")(
      AnonymousFunction(ApplyInfix(Lit.Int(1), Term.Name("+"), Nil, Placeholder() :: Nil))
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
      New(Init(Type.Name("A"), Name.Anonymous(), emptyArgClause))
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
        Template(
          Nil,
          Init(Type.Name("A"), Name.Anonymous(), emptyArgClause) :: Nil,
          EmptySelf(),
          Nil
        )
      )
    }
  }

  test("new A with B") {
    assertTerm("new A with B") {
      NewAnonymous(
        Template(
          Nil,
          Init(Type.Name("A"), Name.Anonymous(), emptyArgClause) ::
            Init(Type.Name("B"), Name.Anonymous(), emptyArgClause) :: Nil,
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
          Init(Type.Name("A"), Name.Anonymous(), emptyArgClause) :: Nil,
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
    assertTrees(stat("stat;"))(Term.Name("stat"))
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
                      Type.Apply(Type.Name("Ptr"), List(Type.Wildcard(Type.Bounds(None, None))))
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
          Block(List(AnonymousFunction(Apply(ApplyType(Placeholder(), List(Type.Name("K"))), Nil))))
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

  test("interpolation-with-escaped-quotes") {
    assertTerm("""s"\"$t\""""") {
      Interpolate(
        Term.Name("s"),
        List(Lit.String("\\\""), Lit.String("\\\"")),
        List(Term.Name("t"))
      )
    }
  }

  test("block-arg") {
    val res = term("new Foo({str => str.length})")
    val termList = q"List($res)"
    assertTree(term(termList.syntax))(termList)
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

    val expected = Defn.Val(
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
    )
    assertTree(res)(expected)
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

  // https://github.com/scalameta/scalameta/issues/1843
  test("anonymous-function-#1843-1") {
    assertTerm("_ fun (_.bar)")(
      AnonymousFunction(
        ApplyInfix(
          Placeholder(),
          Term.Name("fun"),
          Nil,
          List(AnonymousFunction(Select(Placeholder(), Term.Name("bar"))))
        )
      )
    )
  }
  test("anonymous-function-#1843-2") {
    assertTerm("_ fun _.bar")(
      AnonymousFunction(
        ApplyInfix(
          Placeholder(),
          Term.Name("fun"),
          Nil,
          List(Select(Placeholder(), Term.Name("bar")))
        )
      )
    )
  }

  // https://scala-lang.org/files/archive/spec/2.13/06-expressions.html#placeholder-syntax-for-anonymous-functions
  test("anonymous-function-spec-1") {
    assertTerm("_ + 1")(
      AnonymousFunction(
        ApplyInfix(Placeholder(), Term.Name("+"), Nil, List(Lit.Int(1)))
      )
    )
  }
  test("anonymous-function-spec-2") {
    assertTerm("_ * _")(
      AnonymousFunction(
        ApplyInfix(Placeholder(), Term.Name("*"), Nil, List(Placeholder()))
      )
    )
  }
  test("anonymous-function-spec-3") {
    assertTerm("(_: Int) * 2")(
      AnonymousFunction(
        ApplyInfix(Ascribe(Placeholder(), Type.Name("Int")), Term.Name("*"), Nil, List(Lit.Int(2)))
      )
    )
  }
  test("anonymous-function-spec-3.1") {
    assertTerm("1 + (_: Int) * 2 + 1")(
      AnonymousFunction(
        ApplyInfix(
          ApplyInfix(
            Lit.Int(1),
            Term.Name("+"),
            Nil,
            List(
              ApplyInfix(
                Ascribe(Placeholder(), Type.Name("Int")),
                Term.Name("*"),
                Nil,
                List(Lit.Int(2))
              )
            )
          ),
          Term.Name("+"),
          Nil,
          List(Lit.Int(1))
        )
      )
    )
  }
  test("anonymous-function-spec-3.2") {
    assertTerm("(_: Int)")(
      Ascribe(Placeholder(), Type.Name("Int"))
    )
  }
  test("anonymous-function-spec-4") {
    assertTerm("if (_) x else y")(
      AnonymousFunction(If(Placeholder(), Term.Name("x"), Term.Name("y")))
    )
  }
  test("anonymous-function-spec-5") {
    assertTerm("_.map(f)")(
      AnonymousFunction(Apply(Select(Placeholder(), Term.Name("map")), List(Term.Name("f"))))
    )
  }
  test("anonymous-function-spec-6") {
    assertTerm("_.map(_ + 1)")(
      AnonymousFunction(
        Apply(
          Select(Placeholder(), Term.Name("map")),
          List(AnonymousFunction(ApplyInfix(Placeholder(), Term.Name("+"), Nil, List(Lit.Int(1)))))
        )
      )
    )
  }

  test("anonymous-function-scalafmt-1") {
    assertTerm("foo >>= (_.http(registry, port).map(Option.apply(_)).catchSome { bar })")(
      ApplyInfix(
        Term.Name("foo"),
        Term.Name(">>="),
        Nil,
        List(
          AnonymousFunction(
            Apply(
              Select(
                Apply(
                  Select(
                    Apply(
                      Select(Placeholder(), Term.Name("http")),
                      List(Term.Name("registry"), Term.Name("port"))
                    ),
                    Term.Name("map")
                  ),
                  List(
                    AnonymousFunction(
                      Apply(
                        Select(Term.Name("Option"), Term.Name("apply")),
                        List(Placeholder())
                      )
                    )
                  )
                ),
                Term.Name("catchSome")
              ),
              List(Block(List(Term.Name("bar"))))
            )
          )
        )
      )
    )
  }
  test("anonymous-function-scalafmt-2") {
    assertTerm("""scalacOptions ~= (_ filterNot (_ startsWith "-Xlint"))""")(
      ApplyInfix(
        Term.Name("scalacOptions"),
        Term.Name("~="),
        Nil,
        List(
          AnonymousFunction(
            ApplyInfix(
              Placeholder(),
              Term.Name("filterNot"),
              Nil,
              List(
                AnonymousFunction(
                  ApplyInfix(
                    Placeholder(),
                    Term.Name("startsWith"),
                    Nil,
                    List(Lit.String("-Xlint"))
                  )
                )
              )
            )
          )
        )
      )
    )
  }
  test("anonymous-function-scalafmt-3") {
    assertTerm("`field-names` ~> (`private`(_: _*))")(
      ApplyInfix(
        Term.Name("field-names"),
        Term.Name("~>"),
        Nil,
        List(AnonymousFunction(Apply(Term.Name("private"), List(Repeated(Placeholder())))))
      )
    )
  }

  test("(a, b, c)") {
    assertTerm("(a, b, c)") {
      Term.Tuple(List(Term.Name("a"), Term.Name("b"), Term.Name("c")))
    }
  }

  test("((a, b, c))") {
    assertTerm("((a, b, c))") {
      Term.Tuple(List(Term.Name("a"), Term.Name("b"), Term.Name("c")))
    }
  }

  test("(a, b, c) :: ((a, b, c))") {
    assertTerm("(a, b, c) :: ((a, b, c))") {
      Term.ApplyInfix(
        Term.Tuple(List(Term.Name("a"), Term.Name("b"), Term.Name("c"))),
        Term.Name("::"),
        Nil,
        List(Term.Tuple(List(Term.Name("a"), Term.Name("b"), Term.Name("c"))))
      )
    }
  }

  test("((a, b, c)) :: ((a, b, c))") {
    assertTerm("((a, b, c)) :: ((a, b, c))") {
      Term.ApplyInfix(
        Term.Tuple(List(Term.Name("a"), Term.Name("b"), Term.Name("c"))),
        Term.Name("::"),
        Nil,
        List(Term.Tuple(List(Term.Name("a"), Term.Name("b"), Term.Name("c"))))
      )
    }
  }

  test("((a, b, c)) :: (a, b, c)") {
    assertTerm("((a, b, c)) :: (a, b, c)") {
      Term.ApplyInfix(
        Term.Tuple(List(Term.Name("a"), Term.Name("b"), Term.Name("c"))),
        Term.Name("::"),
        Nil,
        List(Term.Name("a"), Term.Name("b"), Term.Name("c"))
      )
    }
  }

  test("#2720 infix with repeated arg last") {
    assertTerm("a foo (b, c: _*)") {
      Term.ApplyInfix(
        Term.Name("a"),
        Term.Name("foo"),
        Nil,
        List(Term.Name("b"), Term.Repeated(Term.Name("c")))
      )
    }
  }
  test("#2720-for-comp") {
    assertTerm("for { `j`: Int <- Seq(4, 5, 6, 7)} yield `j`") {
      Term.ForYield(
        List(
          Enumerator.Generator(
            Pat.Typed(Pat.Var(Term.Name("j")), Type.Name("Int")),
            Term.Apply(Term.Name("Seq"), List(Lit.Int(4), Lit.Int(5), Lit.Int(6), Lit.Int(7)))
          )
        ),
        Term.Name("j")
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
    checkTerm(exprDq, exprDq)(
      Term.Tuple(List(Lit.String("\n"), Lit.String("bar\n"), Lit.String("\nbaz")))
    )
    checkTerm(exprTq, exprTq)(
      Term.Tuple(List(Lit.String("\n"), Lit.String("bar\n"), Lit.String("\nbaz")))
    )
  }

  test("using-call-site in scala2") {
    checkStat("val a = f()(using a)(using 3, true)")(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("a"))),
        None,
        Term.Apply(
          Term.Apply(
            Term.Apply(Term.Name("f"), Nil),
            Term.ArgClause(List(Term.Name("a")), Some(Mod.Using()))
          ),
          Term.ArgClause(List(Lit.Int(3), Lit.Boolean(true)), Some(Mod.Using()))
        )
      )
    )
  }

  test("scala3-syntax") {

    runTestError[Term](
      """|() match
         |  case _: Unit => ()""".stripMargin,
      "error: { expected but case found"
    )

  }

  test("using") {
    assertTerm("Set(using)") {
      Term.Apply(Term.Name("Set"), List(Term.Name("using")))
    }

    assertTerm("foo(using, bar)") {
      Term.Apply(Term.Name("foo"), List(Term.Name("using"), Term.Name("bar")))
    }

    assertTerm(
      """|{
         |  val using ="asdsa"; 
         |  foo(using: String) 
         |}""".stripMargin
    ) {
      Term.Block(
        List(
          Defn.Val(Nil, List(Pat.Var(Term.Name("using"))), None, Lit.String("asdsa")),
          Term.Apply(Term.Name("foo"), List(Term.Ascribe(Term.Name("using"), Type.Name("String"))))
        )
      )
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
        Term.Name("foo"),
        Term.ArgClause(
          Term.Block(
            Term.Function(
              List(Term.Param(List(Mod.Implicit()), Term.Name("a"), None, None)),
              Term.Function(
                List(Term.Param(List(Mod.Implicit()), Term.Name("b"), None, None)),
                Term.Ascribe(
                  Term.PartialFunction(
                    List(Case(Pat.Var(Term.Name("bar")), None, Term.Name("baz")))
                  ),
                  Type.Name("qux")
                )
              )
            ) :: Nil
          ) :: Nil,
          None
        )
      )
    }
  }

  test("implicit closure with val") {
    val code =
      """|foo { implicit a =>
         |  val bar = baz
         |  bar
         |}
         |""".stripMargin
    checkTerm(code) {
      Term.Apply(
        Term.Name("foo"),
        Term.ArgClause(
          Term.Block(
            Term.Function(
              Term.ParamClause(
                List(Term.Param(List(Mod.Implicit()), Term.Name("a"), None, None)),
                Some(Mod.Implicit())
              ),
              Term.Block(
                List(
                  Defn.Val(Nil, List(Pat.Var(Term.Name("bar"))), None, Term.Name("baz")),
                  Term.Name("bar")
                )
              )
            ) :: Nil
          ) :: Nil,
          None
        )
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
    )(
      Term.If(
        Term.Name("isEmpty"),
        Term.Tuple(List(Term.Name("None"), Term.Name("None"))),
        Term.Block(
          Term.Tuple(
            List(
              Term.Apply(Term.Name("Some"), List(Term.Name("e"))),
              Term.Apply(Term.Name("Some"), List(Term.Name("f")))
            )
          ) :: Nil
        ),
        Nil
      )
    )
  }

  test("#3050 function without body") {
    val code =
      """|f { (x1: A, x2: B => C) =>
         |}
         |""".stripMargin
    checkTerm(code, code)(
      Term.Apply(
        Term.Name("f"),
        Term.Block(
          Term.Function(
            List(
              Term.Param(Nil, Term.Name("x1"), Some(Type.Name("A")), None),
              Term.Param(
                Nil,
                Term.Name("x2"),
                Some(Type.Function(Type.FuncParamClause(List(Type.Name("B"))), Type.Name("C"))),
                None
              )
            ),
            Term.Block(Nil)
          ) :: Nil
        ) :: Nil
      )
    )
  }

  test("#3136: block catch handler, in braces") {
    val code =
      """|try ??? catch {
         |  val a = 10
         |  handler(a)
         |}
         |""".stripMargin
    checkTerm(code, code)(
      Term.TryWithHandler(
        Term.Name("???"),
        Term.Block(
          List(
            Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Lit.Int(10)),
            Term.Apply(Term.Name("handler"), List(Term.Name("a")))
          )
        ),
        None
      )
    )
  }

  test("#3138: infix with a following block") {
    val codeOnNextLine =
      """|{
         |  Foo bar (2, 3)
         |  { a }
         |}
         |""".stripMargin
    val codeOnSameLine =
      """|{
         |  Foo bar (2, 3) { a }
         |}
         |""".stripMargin
    val syntaxOnSameLine =
      """|{
         |  Foo bar (2, 3) {
         |    a
         |  }
         |}
         |""".stripMargin
    val treeOnSameLine = Term.Block(
      Term.ApplyInfix(
        Term.Name("Foo"),
        Term.Name("bar"),
        Nil,
        Term.Apply(
          Term.Tuple(List(Lit.Int(2), Lit.Int(3))),
          List(Term.Block(List(Term.Name("a"))))
        ) :: Nil
      ) :: Nil
    )
    runTestAssert[Stat](codeOnNextLine, Some(syntaxOnSameLine))(treeOnSameLine)
    runTestAssert[Stat](codeOnSameLine, Some(syntaxOnSameLine))(treeOnSameLine)
  }

  test("#3138: apply with a following block") {
    val codeOnNextLine =
      """|{
         |  Foo.bar (2, 3)
         |  { a }
         |}
         |""".stripMargin
    val codeOnSameLine =
      """|{
         |  Foo.bar (2, 3) { a }
         |}
         |""".stripMargin
    val syntaxOnSameLine =
      """|{
         |  Foo.bar(2, 3) {
         |    a
         |  }
         |}
         |""".stripMargin
    val treeOnSameLine = Term.Block(
      Term.Apply(
        Term.Apply(Term.Select(Term.Name("Foo"), Term.Name("bar")), List(Lit.Int(2), Lit.Int(3))),
        List(Term.Block(List(Term.Name("a"))))
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
    )(
      Term.PartialFunction(
        Case(
          Pat.Var(Term.Name("foo")),
          Some(Lit.Boolean(true)),
          Term.Apply(Term.Name("List"), List(Term.Name("bar")))
        ) :: Nil
      )
    )
  }

  test("expr with annotation, then match") {
    val code =
      """|underlyingStableClassRef(mbr.info.loBound): @unchecked match {
         |  case ref: TypeRef =>
         |}""".stripMargin
    val layout =
      """|(underlyingStableClassRef(mbr.info.loBound): @unchecked) match {
         |  case ref: TypeRef =>
         |}""".stripMargin
    runTestAssert[Term](code, Some(layout))(
      Term.Match(
        Term.Annotate(
          Term.Apply(
            Term.Name("underlyingStableClassRef"),
            List(
              Term.Select(Term.Select(Term.Name("mbr"), Term.Name("info")), Term.Name("loBound"))
            )
          ),
          List(Mod.Annot(Init(Type.Name("unchecked"), Name.Anonymous(), emptyArgClause)))
        ),
        List(
          Case(Pat.Typed(Pat.Var(Term.Name("ref")), Type.Name("TypeRef")), None, Term.Block(Nil))
        ),
        Nil
      )
    )
  }

  test("#3220") {
    val code =
      """|for {
         |  case (a, b) <- pairs
         |  x <- a to b
         |} yield x
         |""".stripMargin
    val layout = "for ( case (a, b) <- pairs; x <- a to b) yield x"
    runTestAssert[Term](code, Some(layout))(
      Term.ForYield(
        List(
          Enumerator.CaseGenerator(
            Pat.Tuple(List(Pat.Var(tname("a")), Pat.Var(tname("b")))),
            tname("pairs")
          ),
          Enumerator.Generator(
            Pat.Var(tname("x")),
            Term.ApplyInfix(tname("a"), tname("to"), Nil, List(tname("b")))
          )
        ),
        tname("x")
      )
    )
  }

  test("#3224") {
    val code =
      """|for {
         |  x2 <- x1
         |} yield x2
         |  .x3 {
         |    case x4
         |        if x5.x6
         |          .x7(x8) =>
         |        x9
         |  }
         |""".stripMargin
    val layout =
      """|for (x2 <- x1) yield x2.x3({
         |  case x4 if x5.x6.x7(x8) => x9
         |})
         |""".stripMargin
    runTestAssert[Term](code, Some(layout))(
      Term.ForYield(
        List(Enumerator.Generator(Pat.Var(tname("x2")), tname("x1"))),
        Term.Apply(
          Term.Select(tname("x2"), tname("x3")),
          Term.PartialFunction(
            Case(
              Pat.Var(tname("x4")),
              Some(
                Term.Apply(
                  Term.Select(Term.Select(tname("x5"), tname("x6")), tname("x7")),
                  List(tname("x8"))
                )
              ),
              tname("x9")
            ) :: Nil
          ) :: Nil
        )
      )
    )
  }

  test("match on array-of-wildcard") {
    val code =
      """|obj match { case arr: Array[Array[_]] => }
         |""".stripMargin
    val layout =
      """|obj match {
         |  case arr: Array[Array[_]] =>
         |}
         |""".stripMargin
    runTestAssert[Term](code, Some(layout))(
      Term.Match(
        tname("obj"),
        Case(
          Pat.Typed(
            Pat.Var(tname("arr")),
            Type.Apply(
              pname("Array"),
              List(Type.Apply(pname("Array"), List(Type.Wildcard(noBounds))))
            )
          ),
          None,
          Term.Block(Nil)
        ) :: Nil,
        Nil
      )
    )
  }

}
