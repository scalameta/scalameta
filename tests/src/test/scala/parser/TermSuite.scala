import scala.reflect.core._

class TermSuite extends ParseSuite {
  import Term._, Aux._

  test("parse `x`") {
    val Ident("x", false) = term("x")
    val Ident("x", true) = term("`x`")
  }

  test("parse `a.b.c`") {
    val Select(Select(Ident("a", false), Ident("b", false)), Ident("c", false)) = term("a.b.c")
  }

  test("parse `foo.this`") {
    val This(Some(Type.Ident("foo", false))) = term("foo.this")
    val This(None) = term("this")
  }

  test("parse `a.super[b].c`") {
    val SuperSelect(Some(Type.Ident("a", false)), Some(Type.Ident("b", false)),
                    Term.Ident("c", false)) = term("a.super[b].c")
    val SuperSelect(None, Some(Type.Ident("b", false)),
                    Term.Ident("c", false)) = term("super[b].c")
    val SuperSelect(Some(Type.Ident("a", false)), None,
                    Term.Ident("c", false)) = term("a.super.c")
    val SuperSelect(None, None, Term.Ident("c", false)) = term("super.c")
  }

  test("parse `s\"a$bc\"") {
    val Interpolate(Ident("s", false), Lit.String("a ") :: Lit.String(" c") :: Nil,
                    Ident("b", false) :: Nil) = term("s\"a $b c\"")
  }

  test("parse `f(0)`") {
    val Apply(Ident("f", false), Lit.Int(0) :: Nil) = term("f(0)")
  }

  test("parse `a + b`") {
    val Apply(Select(Ident("a", false), Ident("+", false)),
              Ident("b", false) :: Nil) = term("a + b")
  }

  test("parse `a :: b`") {
    val ApplyRight(Ident("a", false), Ident("::", false), Nil, Ident("b", false)) = term("a :: b")
  }

  test("parse `!a`") {
    val ApplyUnary(Ident("!", false), Ident("a", false)) = term("!a")
  }

  test("parse `a = true`") {
    val Assign(Ident("a", false), Lit.True()) = term("a = true")
  }

  test("parse `a(0) = true`") {
    val Update(Apply(Ident("a", false), Lit.Int(0) :: Nil), Lit.True()) = term("a(0) = true")
  }

  test("parse `return 1`") {
    val Return(Lit.Int(1)) = term("return 1")
  }

  test("parse `throw 1`") {
    val Throw(Lit.Int(1)) = term("throw 1")
  }

  // TODO:
  // test("parse `1: Int`) {
  //  val Ascribe(Term.Int(1), Type.Ident("Int")) = term("1: Int")
  // }

  // TODO:
  // test("parse `1: @foo`") {
  //   val Annotate(Lit.Int(1), Mod.Annot(Type.Ident("foo", false), Nil) :: Nil) = term("1: @foo")
  // }

  test("parse `(true, false)`") {
    val Tuple(Lit.True() :: Lit.False() :: Nil) = term("(true, false)")
  }

  test("parse `{ true; false }`") {
    val Block(Lit.True() :: Lit.False() :: Nil) = term("{ true; false }")
  }

  // TODO: collapse single-element blocks as trivia?
  test("parse `{ true }`") {
    val Block(Lit.True() :: Nil) = term("{ true }")
  }

  test("parse `if (true) true else false`") {
    val If(Lit.True(), Lit.True(), Lit.False()) = term("if (true) true else false")
  }

  // TODO: synthetic unit is not good enough
  test("parse `if (true) true`") {
    val If(Lit.True(), Lit.True(), Lit.Unit()) = term("if (true) true")
  }

  test("parse `(x => x)`") {
    val Function(Param(Nil, Some(Ident("x", false)), None, None) :: Nil,
                 Ident("x", false)) = term("(x => x)")
  }

  // TODO:
  // test("parse `(x: Int) => x`") {
  //   val Function(Param(Nil, Some(Ident("x", false)), Some(Type.Ident("Int", false)), None) :: Nil,
  //                Ident("x", false)) = term("(x: Int) => x")
  // }
  // test("parse `(_: Int) => x`") { }

  test("parse `_ => ()`") {
    val Function(Param.empty :: Nil, Lit.Unit()) = term("_ => ()")
  }

  // TODO: too much blocks?
  test("parse `implicit x => ()`") {
    val Block(Function(Param(Mod.Implicit() :: Nil, Some(Ident("x", false)), None, None) :: Nil,
                       Block(Lit.Unit() :: Nil)) :: Nil) = term("{ implicit x => () }")
  }

  // TODO:
  // test("parse match") { }
  // test("parse try") { }
  // test("parse partial function") { }

  test("parse `while (true) false`") {
    val While(Lit.True(), Lit.False()) = term("while (true) false")
  }

  test("parse `do false while(true)`") {
    val Do(Lit.False(), Lit.True()) = term("do false while(true)")
  }

  // TODO:
  // test("parse for") { }
  // test("parse for yield") { }

  // TODO:
  // test("parse `new`") { }

  test("parse `f(_)`") {
    val Apply(Ident("f", false), List(Placeholder())) = term("f(_)")
  }

  test("parse `_ + 1`") {
    val Apply(Select(Placeholder(), Ident("+", false)), List(Lit.Int(1))) = term("_ + 1")
    val Apply(Select(Lit.Int(1), Ident("+", false)), List(Placeholder())) = term("1 + _")
  }

  test("parse `f _`") {
    val Eta(Ident("f", false)) = term("f _")
  }
}
