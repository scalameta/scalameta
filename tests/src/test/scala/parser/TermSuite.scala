import scala.reflect.core._

class TermSuite extends ParseSuite {
  import Term._, Aux._

  test("x") {
    val Ident("x", false) = term("x")
    val Ident("x", true) = term("`x`")
  }

  test("a.b.c") {
    val Select(Select(Ident("a", false), Ident("b", false)), Ident("c", false)) = term("a.b.c")
  }

  test("foo.this") {
    val This(Some(Type.Ident("foo", false))) = term("foo.this")
    val This(None) = term("this")
  }

  test("a.super[b].c") {
    val SuperSelect(Some(Type.Ident("a", false)), Some(Type.Ident("b", false)),
                    Term.Ident("c", false)) = term("a.super[b].c")
    val SuperSelect(None, Some(Type.Ident("b", false)),
                    Term.Ident("c", false)) = term("super[b].c")
    val SuperSelect(Some(Type.Ident("a", false)), None,
                    Term.Ident("c", false)) = term("a.super.c")
    val SuperSelect(None, None, Term.Ident("c", false)) = term("super.c")
  }

  test("s\"a$bc\"") {
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

  test("a :: b") {
    val ApplyRight(Ident("a", false), Ident("::", false), Nil, Ident("b", false)) = term("a :: b")
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

  // TODO:
  // test("1: Int`) {
  //  val Ascribe(Term.Int(1), Type.Ident("Int")) = term("1: Int")
  // }

  // TODO:
  // test("1: @foo") {
  //   val Annotate(Lit.Int(1), Mod.Annot(Type.Ident("foo", false), Nil) :: Nil) = term("1: @foo")
  // }

  test("(true, false)") {
    val Tuple(Lit.True() :: Lit.False() :: Nil) = term("(true, false)")
  }

  test("{ true; false }") {
    val Block(Lit.True() :: Lit.False() :: Nil) = term("{ true; false }")
  }

  // TODO: collapse single-element blocks as trivia?
  test("{ true }") {
    val Block(Lit.True() :: Nil) = term("{ true }")
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

  // TODO:
  // test("(x: Int) => x") {
  //   val Function(Param(Nil, Some(Ident("x", false)), Some(Type.Ident("Int", false)), None) :: Nil,
  //                Ident("x", false)) = term("(x: Int) => x")
  // }
  // test("(_: Int) => x") { }

  test("_ => ()") {
    val Function(Param.empty :: Nil, Lit.Unit()) = term("_ => ()")
  }

  // TODO: too much blocks?
  test("implicit x => ()") {
    val Block(Function(Param(Mod.Implicit() :: Nil, Some(Ident("x", false)), None, None) :: Nil,
                       Block(Lit.Unit() :: Nil)) :: Nil) = term("{ implicit x => () }")
  }

  test("1 match { case 1 => true }") {
    val Term.Match(Lit.Int(1),
                   Term.Cases(Aux.Case(Lit.Int(1), None, Some(Lit.True())) :: Nil)) =
      term("1 match { case 1 => true }")
  }

  test("1 match { case 1 => }") {
    val Term.Match(Lit.Int(1),
                   Term.Cases(Aux.Case(Lit.Int(1), None, None) :: Nil)) =
      term("1 match { case 1 => }")
  }

  test("1 match { case 1 if true => }") {
    val Term.Match(Lit.Int(1),
                   Term.Cases(Aux.Case(Lit.Int(1), Some(Lit.True()), None) :: Nil)) =
      term("1 match { case 1 if true => }")
  }

  test("try 1") {
    val Term.Try(Lit.Int(1), None, None) = term("try 1")
  }

  test("try 1 catch 1") {
    val Term.Try(Lit.Int(1), Some(Lit.Int(1)), None) = term("try 1 catch 1")
  }

  test("try 1 catch { case _ => }") {
    val Term.Try(Lit.Int(1),
                 Some(Term.Cases(Aux.Case(Pat.Wildcard(), None, None) :: Nil)), None) =
      term("try 1 catch { case _ => }")
  }

  test("try 1 finally 1") {
    val Term.Try(Lit.Int(1), None, Some(Lit.Int(1))) = term("try 1 finally 1")
  }

  test("{ case 1 => () }") {
    val Term.Cases(Aux.Case(Lit.Int(1), None, Some(Lit.Unit())) :: Nil) =
      term("{ case 1 => () }")
  }

  test("while (true) false") {
    val While(Lit.True(), Lit.False()) = term("while (true) false")
  }

  test("do false while(true)") {
    val Do(Lit.False(), Lit.True()) = term("do false while(true)")
  }

  test("for") {
    val For(List(Enum.Generator(Ident("a", false), Ident("b", false)),
                 Enum.Guard(Ident("c", false)),
                 Enum.Val(Ident("x", false), Ident("a", false))),
            Ident("x", false)) = term("for (a <- b; if c; x = a) x")

  }
  test("for yield") {
    val ForYield(List(Enum.Generator(Ident("a", false), Ident("b", false)),
                      Enum.Guard(Ident("c", false)),
                      Enum.Val(Ident("x", false), Ident("a", false))),
                 Ident("x", false)) = term("for (a <- b; if c; x = a) yield x")
  }

  // TODO:
  // test("new") { }

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
}
