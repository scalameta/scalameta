import scala.reflect.core._, Term._, Aux._

class TermSuite extends ParseSuite {
  test("x") {
    val Name("x", false) = term("x")
  }

  test("`x`") {
    val Name("x", true) = term("`x`")
  }

  test("a.b.c") {
    val Select(Select(Name("a", false), Name("b", false)), Name("c", false)) = term("a.b.c")
  }

  test("foo.this") {
    val This(Some(Type.Name("foo", false))) = term("foo.this")
  }

  test("this") {
    val This(None) = term("this")
  }

  test("a.super[b].c") {
    val SuperSelect(Some(Type.Name("a", false)), Some(Type.Name("b", false)),
                    Name("c", false)) = term("a.super[b].c")
  }

  test("super[b].c") {
    val SuperSelect(None, Some(Type.Name("b", false)),
                    Name("c", false)) = term("super[b].c")
  }

  test("a.super.c") {
    val SuperSelect(Some(Type.Name("a", false)), None,
                    Name("c", false)) = term("a.super.c")
  }

  test("super.c") {
    val SuperSelect(None, None, Name("c", false)) = term("super.c")
  }

  test("s\"a $b c\"") {
    val Interpolate(Name("s", false), Lit.String("a ") :: Lit.String(" c") :: Nil,
                    Name("b", false) :: Nil) = term("s\"a $b c\"")
  }

  test("f(0)") {
    val Apply(Name("f", false), Lit.Int(0) :: Nil) = term("f(0)")
  }

  test("f(x = 0)") {
    val Apply(Name("f", false), Arg.Named(Term.Name("x", false), Lit.Int(0)) :: Nil) = term("f(x = 0)")
  }

  test("f(x: _*)") {
    val Apply(Name("f", false), Arg.Repeated(Term.Name("x", false)) :: Nil) = term("f(x: _*)")
  }

  test("a + b") {
    val Apply(Select(Name("a", false), Name("+", false)),
              Name("b", false) :: Nil) = term("a + b")
  }

  test("a + b + c") {
    val Apply(Select(Apply(Select(Name("a", false), Name("+", false)), Name("b", false) :: Nil), Name("+", false)), Name("c", false) :: Nil) = term("a + b + c")
  }

  test("a :: b") {
    val ApplyRight(Name("a", false), Name("::", false), Nil, Name("b", false)) = term("a :: b")
  }

  test("a :: b :: c") {
    val ApplyRight(Name("a", false), Name("::", false), Nil,
                   ApplyRight(Name("b", false), Name("::", false), Nil, Name("c", false))) = term("a :: b :: c")
  }

  test("!a") {
    val ApplyUnary(Name("!", false), Name("a", false)) = term("!a")
  }

  test("a = true") {
    val Assign(Name("a", false), Lit.True()) = term("a = true")
  }

  test("a(0) = true") {
    val Update(Apply(Name("a", false), Lit.Int(0) :: Nil), Lit.True()) = term("a(0) = true")
  }

  test("return 1") {
    val Return(Lit.Int(1)) = term("return 1")
  }

  test("throw 1") {
    val Throw(Lit.Int(1)) = term("throw 1")
  }

  test("1: Int") {
    val Ascribe(Lit.Int(1), Type.Name("Int", false)) = term("1: Int")
  }

  test("1: @foo") {
    val Annotate(Lit.Int(1), Mod.Annot(Type.Name("foo", false), Nil) :: Nil) = term("1: @foo")
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
    val Function(Param.Named(Name("x", false), None, None, Nil) :: Nil,
                 Name("x", false)) = term("(x => x)")
  }

  test("(x: Int) => x") {
    val Function(Param.Named(Name("x", false), Some(Type.Name("Int", false)), None, Nil) :: Nil,
                 Name("x", false)) = term("(x: Int) => x")
  }

  test("(_: Int) => x") {
    val Function(Param.Anonymous(Some(Type.Name("Int", false)), Nil) :: Nil,
                 Name("x", false)) = term("(_: Int) => x")
  }

  test("_ => ()") {
    val Function(Param.Anonymous.empty :: Nil, Lit.Unit()) = term("_ => ()")
  }

  test("implicit x => ()") {
    val Function(Param.Named(Name("x", false), None, None, Mod.Implicit() :: Nil) :: Nil,
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
    val For(List(Enum.Generator(Name("a", false), Name("b", false)),
                 Enum.Guard(Name("c", false)),
                 Enum.Val(Name("x", false), Name("a", false))),
            Name("x", false)) = term("for (a <- b; if c; x = a) x")

  }
  test("for (a <- b; if c; x = a) yield x") {
    val ForYield(List(Enum.Generator(Name("a", false), Name("b", false)),
                      Enum.Guard(Name("c", false)),
                      Enum.Val(Name("x", false), Name("a", false))),
                 Name("x", false)) = term("for (a <- b; if c; x = a) yield x")
  }

  test("f(_)") {
    val Apply(Name("f", false), List(Placeholder())) = term("f(_)")
  }

  test("_ + 1") {
    val Apply(Select(Placeholder(), Name("+", false)), List(Lit.Int(1))) = term("_ + 1")
    val Apply(Select(Lit.Int(1), Name("+", false)), List(Placeholder())) = term("1 + _")
  }

  test("f _") {
    val Eta(Name("f", false)) = term("f _")
  }

  test("new {}") {
    val New(Template.empty) = term("new {}")
  }

  test("new A") {
    val New(Template(Nil, Parent(Type.Name("A", false), Nil) :: Nil, Self.empty, Nil)) = term("new A")
  }

  test("new A with B") {
    val New(Template(Nil, Parent(Type.Name("A", false), Nil) ::
                          Parent(Type.Name("B", false), Nil) :: Nil,
                     Self.empty, Nil)) =
      term("new A with B")
  }

  test("new { val x: Int = 1 } with A") {
    val New(Template(Defn.Val(Nil, List(Term.Name("x", false)), Some(Type.Name("Int", false)), Lit.Int(1)) :: Nil,
                     Parent(Type.Name("A", false), Nil) :: Nil, Self.empty, Nil)) =
      term("new { val x: Int = 1 } with A")
  }

  test("new { self: T => }") {
    val New(Template(Nil, Nil, Self(Some(Term.Name("self", false)), Some(Type.Name("T", false))), Nil)) =
      term("new { self: T => }")
  }
}
