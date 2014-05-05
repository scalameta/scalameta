import scala.reflect.core._, Term._, Aux._

class TermSuite extends ParseSuite {
  test("x") {
    val Ident("x", false) = term("x")
  }

  test("`x`") {
    val Ident("x", true) = term("`x`")
  }

  test("a.b.c") {
    val Select(Select(Ident("a", false), Ident("b", false)), Ident("c", false)) = term("a.b.c")
  }

  test("foo.this") {
    val This(Some(Type.Ident("foo", false))) = term("foo.this")
  }

  test("this") {
    val This(None) = term("this")
  }

  test("a.super[b].c") {
    val SuperSelect(Some(Type.Ident("a", false)), Some(Type.Ident("b", false)),
                    Ident("c", false)) = term("a.super[b].c")
  }

  test("super[b].c") {
    val SuperSelect(None, Some(Type.Ident("b", false)),
                    Ident("c", false)) = term("super[b].c")
  }

  test("a.super.c") {
    val SuperSelect(Some(Type.Ident("a", false)), None,
                    Ident("c", false)) = term("a.super.c")
  }

  test("super.c") {
    val SuperSelect(None, None, Ident("c", false)) = term("super.c")
  }

  test("s\"a $b c\"") {
    val Interpolate(Ident("s", false), Lit.String("a ") :: Lit.String(" c") :: Nil,
                    Ident("b", false) :: Nil) = term("s\"a $b c\"")
  }

  test("f(0)") {
    val Apply(Ident("f", false), Lit.Int(0) :: Nil) = term("f(0)")
  }

  test("a + b") {
    val Apply(Select(Ident("a", false), Ident("+", false)),
              Ident("b", false) :: Nil) = term("a + b")
  }

  test("a + b + c") {
    val Apply(Select(Apply(Select(Ident("a", false), Ident("+", false)), Ident("b", false) :: Nil), Ident("+", false)), Ident("c", false) :: Nil) = term("a + b + c")
  }

  test("a :: b") {
    val ApplyRight(Ident("a", false), Ident("::", false), Nil, Ident("b", false)) = term("a :: b")
  }

  test("a :: b :: c") {
    val ApplyRight(Ident("a", false), Ident("::", false), Nil,
                   ApplyRight(Ident("b", false), Ident("::", false), Nil, Ident("c", false))) = term("a :: b :: c")
  }

  test("!a") {
    val ApplyUnary(Ident("!", false), Ident("a", false)) = term("!a")
  }

  test("a = true") {
    val Assign(Ident("a", false), Lit.True()) = term("a = true")
  }

  test("a(0) = true") {
    val Update(Apply(Ident("a", false), Lit.Int(0) :: Nil), Lit.True()) = term("a(0) = true")
  }

  test("return 1") {
    val Return(Lit.Int(1)) = term("return 1")
  }

  test("throw 1") {
    val Throw(Lit.Int(1)) = term("throw 1")
  }

  test("1: Int") {
    val Ascribe(Lit.Int(1), Type.Ident("Int", false)) = term("1: Int")
  }

  test("1: @foo") {
    val Annotate(Lit.Int(1), Mod.Annot(Type.Ident("foo", false), Nil) :: Nil) = term("1: @foo")
  }

  test("(true, false)") {
    val Tuple(Lit.True() :: Lit.False() :: Nil) = term("(true, false)")
  }

  test("{ true; false }") {
    val Block(Lit.True() :: Lit.False() :: Nil) = term("{ true; false }")
  }

  test("{ true }") {
    val Lit.True() = term("{ true }")
  }

  test("if (true) true else false") {
    val If(Lit.True(), Lit.True(), Some(Lit.False())) = term("if (true) true else false")
  }

  test("if (true) true") {
    val If(Lit.True(), Lit.True(), None) = term("if (true) true")
  }

  test("(x => x)") {
    val Function(Param(Nil, Some(Ident("x", false)), None, None) :: Nil,
                 Ident("x", false)) = term("(x => x)")
  }

  test("(x: Int) => x") {
    val Function(Param(Nil, Some(Ident("x", false)), Some(Type.Ident("Int", false)), None) :: Nil,
                 Ident("x", false)) = term("(x: Int) => x")
  }

  test("(_: Int) => x") {
    val Function(Param(Nil, None, Some(Type.Ident("Int", false)), None) :: Nil,
                 Ident("x", false)) = term("(_: Int) => x")
  }

  test("_ => ()") {
    val Function(Param.empty :: Nil, Lit.Unit()) = term("_ => ()")
  }

  test("implicit x => ()") {
    val Function(Param(Mod.Implicit() :: Nil, Some(Ident("x", false)), None, None) :: Nil,
                       Lit.Unit()) = term("{ implicit x => () }")
  }

  test("1 match { case 1 => true }") {
    val Match(Lit.Int(1), Cases(Case(Lit.Int(1), None, Some(Lit.True())) :: Nil)) =
      term("1 match { case 1 => true }")
  }

  test("1 match { case 1 => }") {
    val Match(Lit.Int(1), Cases(Case(Lit.Int(1), None, None) :: Nil)) =
      term("1 match { case 1 => }")
  }

  test("1 match { case 1 if true => }") {
    val Match(Lit.Int(1), Cases(Case(Lit.Int(1), Some(Lit.True()), None) :: Nil)) =
      term("1 match { case 1 if true => }")
  }

  test("try 1") {
    val Try(Lit.Int(1), None, None) = term("try 1")
  }

  test("try 1 catch 1") {
    val Try(Lit.Int(1), Some(Lit.Int(1)), None) = term("try 1 catch 1")
  }

  test("try 1 catch { case _ => }") {
    val Try(Lit.Int(1), Some(Cases(Case(Pat.Wildcard(), None, None) :: Nil)), None) =
      term("try 1 catch { case _ => }")
  }

  test("try 1 finally 1") {
    val Try(Lit.Int(1), None, Some(Lit.Int(1))) = term("try 1 finally 1")
  }

  test("{ case 1 => () }") {
    val Cases(Case(Lit.Int(1), None, Some(Lit.Unit())) :: Nil) =
      term("{ case 1 => () }")
  }

  test("while (true) false") {
    val While(Lit.True(), Lit.False()) = term("while (true) false")
  }

  test("do false while(true)") {
    val Do(Lit.False(), Lit.True()) = term("do false while(true)")
  }

  test("for (a <- b; if c; x = a) x") {
    val For(List(Enum.Generator(Ident("a", false), Ident("b", false)),
                 Enum.Guard(Ident("c", false)),
                 Enum.Val(Ident("x", false), Ident("a", false))),
            Ident("x", false)) = term("for (a <- b; if c; x = a) x")

  }
  test("for (a <- b; if c; x = a) yield x") {
    val ForYield(List(Enum.Generator(Ident("a", false), Ident("b", false)),
                      Enum.Guard(Ident("c", false)),
                      Enum.Val(Ident("x", false), Ident("a", false))),
                 Ident("x", false)) = term("for (a <- b; if c; x = a) yield x")
  }

  test("f(_)") {
    val Apply(Ident("f", false), List(Placeholder())) = term("f(_)")
  }

  test("_ + 1") {
    val Apply(Select(Placeholder(), Ident("+", false)), List(Lit.Int(1))) = term("_ + 1")
    val Apply(Select(Lit.Int(1), Ident("+", false)), List(Placeholder())) = term("1 + _")
  }

  test("f _") {
    val Eta(Ident("f", false)) = term("f _")
  }

  test("new {}") {
    val New(Template.empty) = term("new {}")
  }

  test("new A") {
    val New(Template(Nil, Parent(Type.Ident("A", false), Nil) :: Nil, Self.empty, Nil)) = term("new A")
  }

  test("new A with B") {
    val New(Template(Nil, Parent(Type.Ident("A", false), Nil) ::
                          Parent(Type.Ident("B", false), Nil) :: Nil,
                     Self.empty, Nil)) =
      term("new A with B")
  }

  test("new { val x: Int = 1 } with A") {
    val New(Template(Defn.Val(Nil, List(Term.Ident("x", false)), Some(Type.Ident("Int", false)), Lit.Int(1)) :: Nil,
                     Parent(Type.Ident("A", false), Nil) :: Nil, Self.empty, Nil)) =
      term("new { val x: Int = 1 } with A")
  }

  test("new { self: T => }") {
    val New(Template(Nil, Nil, Self(Some(Term.Ident("self", false)), Some(Type.Ident("T", false))), Nil)) =
      term("new { self: T => }")
  }
}
