import scala.reflect.core._, Term._, Aux._

class TermSuite extends ParseSuite {
  test("x") {
    val Name("x") = term("x")
  }

  test("`x`") {
    val name @ Name("x") = term("`x`")
    assert(name.isBackquoted === true)
  }

  test("a.b.c") {
    val Select(Select(Name("a"), Name("b")), Name("c")) = term("a.b.c")
  }

  test("foo.this") {
    val This(Some(DualName("foo"))) = term("foo.this")
  }

  test("this") {
    val This(None) = term("this")
  }

  test("a.super[b].c") {
    val Select(Super(Some(DualName("a")), Some(Type.Name("b"))),
               Name("c")) = term("a.super[b].c")
  }

  test("super[b].c") {
    val Select(Super(None, Some(Type.Name("b"))),
               Name("c")) = term("super[b].c")
  }

  test("a.super.c") {
    val Select(Super(Some(DualName("a")), None),
               Name("c")) = term("a.super.c")
  }

  test("super.c") {
    val Select(Super(None, None), Name("c")) = term("super.c")
  }

  test("s\"a $b c\"") {
    val Interpolate(Name("s"), Lit.String("a ") :: Lit.String(" c") :: Nil,
                    Name("b") :: Nil) = term("s\"a $b c\"")
  }

  test("f(0)") {
    val Apply(Name("f"), Lit.Int(0) :: Nil) = term("f(0)")
  }

  test("f(x = 0)") {
    val Apply(Name("f"), Arg.Named(Term.Name("x"), Lit.Int(0)) :: Nil) = term("f(x = 0)")
  }

  test("f(x: _*)") {
    val Apply(Name("f"), Arg.Repeated(Term.Name("x")) :: Nil) = term("f(x: _*)")
  }

  test("a + b") {
    val ApplyInfix(Name("a"), Name("+"), Nil, Name("b") :: Nil) = term("a + b")
  }

  test("a + b + c") {
    val ApplyInfix(ApplyInfix(Name("a"), Name("+"), Nil, Name("b") :: Nil),
                   Name("+"), Nil, Name("c") :: Nil) = term("a + b + c")
  }

  test("a :: b") {
    val ApplyInfix(Name("a"), Name("::"), Nil, Name("b") :: Nil) = term("a :: b")
  }

  test("a :: b :: c") {
    val ApplyInfix(Name("a"), Name("::"), Nil,
                   ApplyInfix(Name("b"), Name("::"), Nil, Name("c") :: Nil) :: Nil) = term("a :: b :: c")
  }

  test("!a") {
    val ApplyUnary(Name("!"), Name("a")) = term("!a")
  }

  test("a = true") {
    val Assign(Name("a"), Lit.Bool(true)) = term("a = true")
  }

  test("a(0) = true") {
    val Update(Apply(Name("a"), Lit.Int(0) :: Nil), Lit.Bool(true)) = term("a(0) = true")
  }

  test("return") {
    val Return(None) = term("return")
  }

  test("return 1") {
    val Return(Some(Lit.Int(1))) = term("return 1")
  }

  test("throw 1") {
    val Throw(Lit.Int(1)) = term("throw 1")
  }

  test("1: Int") {
    val Ascribe(Lit.Int(1), Type.Name("Int")) = term("1: Int")
  }

  test("1: @foo") {
    val Annotate(Lit.Int(1), Mod.Annot(Type.Name("foo"), Nil) :: Nil) = term("1: @foo")
  }

  test("(true, false)") {
    val Tuple(Lit.Bool(true) :: Lit.Bool(false) :: Nil) = term("(true, false)")
  }

  test("{ true; false }") {
    val Block(Lit.Bool(true) :: Lit.Bool(false) :: Nil) = term("{ true; false }")
  }

  test("{ true }") {
    val Block(Lit.Bool(true) :: Nil) = term("{ true }")
  }

  test("if (true) true else false") {
    val If.ThenElse(Lit.Bool(true), Lit.Bool(true), Lit.Bool(false)) = term("if (true) true else false")
  }

  test("if (true) true") {
    val If.Then(Lit.Bool(true), Lit.Bool(true)) = term("if (true) true")
  }

  test("(x => x)") {
    val Function(Param.Named(Nil, Name("x"), None, None) :: Nil,
                 Name("x")) = term("(x => x)")
  }

  test("(x: Int) => x") {
    val Function(Param.Named(Nil, Name("x"), Some(Type.Name("Int")), None) :: Nil,
                 Name("x")) = term("(x: Int) => x")
  }

  test("(_: Int) => x") {
    val Function(Param.Anonymous(Nil, Some(Type.Name("Int"))) :: Nil,
                 Name("x")) = term("(_: Int) => x")
  }

  test("_ => ()") {
    val Function(Param.Anonymous(Nil, None) :: Nil, Lit.Unit()) = term("_ => ()")
  }

  test("{ implicit x => () }") {
    val Block(Function(Param.Named(Mod.Implicit() :: Nil, Name("x"), None, None) :: Nil,
                       Block(Lit.Unit() :: Nil)) :: Nil) = term("{ implicit x => () }")
  }

  test("1 match { case 1 => true }") {
    val Match(Lit.Int(1), Cases(Case(Lit.Int(1), None, Lit.Bool(true) :: Nil) :: Nil)) =
      term("1 match { case 1 => true }")
  }

  test("1 match { case 1 => }") {
    val Match(Lit.Int(1), Cases(Case(Lit.Int(1), None, Nil) :: Nil)) =
      term("1 match { case 1 => }")
  }

  test("1 match { case 1 if true => }") {
    val Match(Lit.Int(1), Cases(Case(Lit.Int(1), Some(Lit.Bool(true)), Nil) :: Nil)) =
      term("1 match { case 1 if true => }")
  }

  test("try 1") {
    val Try(Lit.Int(1), None, None) = term("try 1")
  }

  test("try 1 catch 1") {
    val Try(Lit.Int(1), Some(Lit.Int(1)), None) = term("try 1 catch 1")
  }

  test("try 1 catch { case _ => }") {
    val Try(Lit.Int(1), Some(Cases(Case(Pat.Wildcard(), None, Nil) :: Nil)), None) =
      term("try 1 catch { case _ => }")
  }

  test("try 1 finally 1") {
    val Try(Lit.Int(1), None, Some(Lit.Int(1))) = term("try 1 finally 1")
  }

  test("{ case 1 => () }") {
    val Cases(Case(Lit.Int(1), None, Lit.Unit() :: Nil) :: Nil) =
      term("{ case 1 => () }")
  }

  test("while (true) false") {
    val While(Lit.Bool(true), Lit.Bool(false)) = term("while (true) false")
  }

  test("do false while(true)") {
    val Do(Lit.Bool(false), Lit.Bool(true)) = term("do false while(true)")
  }

  test("for (a <- b; if c; x = a) x") {
    val For(List(Enum.Generator(Name("a"), Name("b")),
                 Enum.Guard(Name("c")),
                 Enum.Val(Name("x"), Name("a"))),
            Name("x")) = term("for (a <- b; if c; x = a) x")

  }
  test("for (a <- b; if c; x = a) yield x") {
    val ForYield(List(Enum.Generator(Name("a"), Name("b")),
                      Enum.Guard(Name("c")),
                      Enum.Val(Name("x"), Name("a"))),
                 Name("x")) = term("for (a <- b; if c; x = a) yield x")
  }

  test("f(_)") {
    val Apply(Name("f"), List(Placeholder())) = term("f(_)")
  }

  test("_ + 1") {
    val ApplyInfix(Placeholder(), Name("+"), Nil, Lit.Int(1) :: Nil) = term("_ + 1")
  }

  test("1 + _") {
    val ApplyInfix(Lit.Int(1), Name("+"), Nil, Placeholder() :: Nil) = term("1 + _")
  }

  test("f _") {
    val Eta(Name("f")) = term("f _")
  }

  test("new {}") {
    val New(Template(Nil, Nil, Self(None, None), Nil)) = term("new {}")
  }

  test("new A") {
    val New(templ @ Template(Nil, Parent(Type.Name("A"), Nil) :: Nil, Self(None, None), Nil)) = term("new A")
    assert(templ.hasExplicitBody === false)
  }

  test("new A {}") {
    val New(templ @ Template(Nil, Parent(Type.Name("A"), Nil) :: Nil, Self(None, None), Nil)) = term("new A {}")
    assert(templ.hasExplicitBody === true) // TODO: fix this
  }

  test("new A with B") {
    val New(Template(Nil, Parent(Type.Name("A"), Nil) ::
                          Parent(Type.Name("B"), Nil) :: Nil,
                     Self(None, None), Nil)) =
      term("new A with B")
  }

  test("new { val x: Int = 1 } with A") {
    val New(Template(Defn.Val(Nil, List(Term.Name("x")), Some(Type.Name("Int")), Lit.Int(1)) :: Nil,
                     Parent(Type.Name("A"), Nil) :: Nil, Self(None, None), Nil)) =
      term("new { val x: Int = 1 } with A")
  }

  test("new { self: T => }") {
    val New(Template(Nil, Nil, Self(Some(Term.Name("self")), Some(Type.Name("T"))), Nil)) =
      term("new { self: T => }")
  }

  test("a + (b = c)") {
    val ApplyInfix(Name("a"), Name("+"), Nil,
                   Arg.Named(Name("b"), Name("c")) :: Nil) = term("a + (b = c)")
  }

  test("(a = b) + c") {
    val ApplyInfix(Assign(Name("a"), Name("b")), Name("+"), Nil,
                   Name("c") :: Nil) = term("(a = b) + c")
  }

  test("a + (b = c).d") {
    val ApplyInfix(Name("a"), Name("+"), Nil,
                   Select(Assign(Name("b"), Name("c")), Name("d")) :: Nil) =
      term("a + (b = c).d")
  }

  test("a + (b: _*)") {
    val ApplyInfix(Name("a"), Name("+"), Nil,
                   Arg.Repeated(Name("b")) :: Nil) = term("a + (b: _*)")
  }
}
