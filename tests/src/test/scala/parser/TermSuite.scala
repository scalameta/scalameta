import scala.meta.internal.ast._, Term.{Name => TermName, _}, Type.{Name => TypeName}
import scala.meta.dialects.Scala211

class TermSuite extends ParseSuite {
  test("x") {
    val TermName("x") = term("x")
  }

  test("`x`") {
    val name @ TermName("x") = term("`x`")
    // TODO: revisit this once we have trivia in place
    // assert(name.isBackquoted === true)
  }

  test("a.b.c") {
    val outer @ Select(inner @ Select(TermName("a"), TermName("b")), TermName("c")) = term("a.b.c")
    // TODO: revisit this once we have trivia in place
    // assert(outer.isPostfix === false)
    // assert(inner.isPostfix === false)
  }

  test("a.b c") {
    val outer @ Select(inner @ Select(TermName("a"), TermName("b")), TermName("c")) = term("a.b c")
    // TODO: revisit this once we have trivia in place
    // assert(outer.isPostfix === true)
    // assert(inner.isPostfix === false)
  }

  test("foo.this") {
    val This(Some("foo")) = term("foo.this")
  }

  test("this") {
    val This(None) = term("this")
  }

  test("a.super[b].c") {
    val Select(Super(Some("a"), Some("b")),
               TermName("c")) = term("a.super[b].c")
  }

  test("super[b].c") {
    val Select(Super(None, Some("b")),
               TermName("c")) = term("super[b].c")
  }

  test("a.super.c") {
    val Select(Super(Some("a"), None),
               TermName("c")) = term("a.super.c")
  }

  test("super.c") {
    val Select(Super(None, None), TermName("c")) = term("super.c")
  }

  test("s\"a $b c\"") {
    val Interpolate(TermName("s"), Lit.String("a ") :: Lit.String(" c") :: Nil,
                    TermName("b") :: Nil) = term("s\"a $b c\"")
  }

  test("f(0)") {
    val Apply(TermName("f"), Lit.Int(0) :: Nil) = term("f(0)")
  }

  test("f(x = 0)") {
    val Apply(TermName("f"), Arg.Named(TermName("x"), Lit.Int(0)) :: Nil) = term("f(x = 0)")
  }

  test("f(x: _*)") {
    val Apply(TermName("f"), Arg.Repeated(TermName("x")) :: Nil) = term("f(x: _*)")
  }

  test("a + b") {
    val ApplyInfix(TermName("a"), TermName("+"), Nil, TermName("b") :: Nil) = term("a + b")
  }

  test("a + b + c") {
    val ApplyInfix(ApplyInfix(TermName("a"), TermName("+"), Nil, TermName("b") :: Nil),
                   TermName("+"), Nil, TermName("c") :: Nil) = term("a + b + c")
  }

  test("a :: b") {
    val ApplyInfix(TermName("a"), TermName("::"), Nil, TermName("b") :: Nil) = term("a :: b")
  }

  test("a :: b :: c") {
    val ApplyInfix(TermName("a"), TermName("::"), Nil,
                   ApplyInfix(TermName("b"), TermName("::"), Nil, TermName("c") :: Nil) :: Nil) = term("a :: b :: c")
  }

  test("!a") {
    val ApplyUnary(TermName("!"), TermName("a")) = term("!a")
  }

  test("a = true") {
    val Assign(TermName("a"), Lit.Bool(true)) = term("a = true")
  }

  test("a(0) = true") {
    val Update(TermName("a"), (Lit.Int(0) :: Nil) :: Nil, Lit.Bool(true)) = term("a(0) = true")
  }

  test("return") {
    val ret @ Return(Lit.Unit()) = term("return")
    // TODO: revisit this once we have trivia in place
    // assert(ret.hasExpr === false)
  }

  test("return 1") {
    val ret @ Return(Lit.Int(1)) = term("return 1")
    // TODO: revisit this once we have trivia in place
    // assert(ret.hasExpr === true)
  }

  test("throw 1") {
    val Throw(Lit.Int(1)) = term("throw 1")
  }

  test("1: Int") {
    val Ascribe(Lit.Int(1), TypeName("Int")) = term("1: Int")
  }

  test("1: @foo") {
    val Annotate(Lit.Int(1), Mod.Annot(Ctor.Name("foo")) :: Nil) = term("1: @foo")
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
    val iff @ If(Lit.Bool(true), Lit.Bool(true), Lit.Bool(false)) = term("if (true) true else false")
    // TODO: revisit this once we have trivia in place
    // assert(iff.hasElsep === true)
  }

  test("if (true) true; else false") {
    val iff @ If(Lit.Bool(true), Lit.Bool(true), Lit.Bool(false)) = term("if (true) true; else false")
    // TODO: revisit this once we have trivia in place
    // assert(iff.hasElsep === true)
  }

  test("if (true) true") {
    val iff @ If(Lit.Bool(true), Lit.Bool(true), Lit.Unit()) = term("if (true) true")
    // TODO: revisit this once we have trivia in place
    // assert(iff.hasElsep === false)
  }

  test("(x => x)") {
    val Function(Term.Param(Nil, TermName("x"), None, None) :: Nil,
                 TermName("x")) = term("(x => x)")
  }

  test("(x: Int) => x") {
    val Function(Term.Param(Nil, TermName("x"), Some(TypeName("Int")), None) :: Nil,
                 TermName("x")) = term("(x: Int) => x")
  }

  test("(_: Int) => x") {
    val Function(Term.Param(Nil, Name.Anonymous(), Some(TypeName("Int")), None) :: Nil,
                 TermName("x")) = term("(_: Int) => x")
  }

  test("_ => ()") {
    val Function(Term.Param(Nil, Name.Anonymous(), None, None) :: Nil, Lit.Unit()) = term("_ => ()")
  }

  test("{ implicit x => () }") {
    val Block(Function(Term.Param(Mod.Implicit() :: Nil, TermName("x"), None, None) :: Nil,
                       Block(Lit.Unit() :: Nil)) :: Nil) = term("{ implicit x => () }")
  }

  test("1 match { case 1 => true }") {
    val Match(Lit.Int(1), Case(Lit.Int(1), None, Term.Block(Lit.Bool(true) :: Nil)) :: Nil) =
      term("1 match { case 1 => true }")
  }

  test("1 match { case 1 => }") {
    val Match(Lit.Int(1), Case(Lit.Int(1), None, Term.Block(Nil)) :: Nil) =
      term("1 match { case 1 => }")
  }

  test("1 match { case 1 if true => }") {
    val Match(Lit.Int(1), Case(Lit.Int(1), Some(Lit.Bool(true)), Term.Block(Nil)) :: Nil) =
      term("1 match { case 1 if true => }")
  }

  test("try 1") {
    val TryWithCases(Lit.Int(1), Nil, None) = term("try 1")
  }

  test("try 1 catch 1") {
    val TryWithTerm(Lit.Int(1), Lit.Int(1), None) = term("try 1 catch 1")
  }

  test("try 1 catch { case _ => }") {
    val TryWithCases(Lit.Int(1), Case(Pat.Wildcard(), None, Term.Block(Nil)) :: Nil, None) =
      term("try 1 catch { case _ => }")
  }

  test("try 1 finally 1") {
    val TryWithCases(Lit.Int(1), Nil, Some(Lit.Int(1))) = term("try 1 finally 1")
  }

  test("{ case 1 => () }") {
    val PartialFunction(Case(Lit.Int(1), None, Term.Block(Lit.Unit() :: Nil)) :: Nil) =
      term("{ case 1 => () }")
  }

  test("while (true) false") {
    val While(Lit.Bool(true), Lit.Bool(false)) = term("while (true) false")
  }

  test("do false while(true)") {
    val Do(Lit.Bool(false), Lit.Bool(true)) = term("do false while(true)")
  }

  test("for (a <- b; if c; x = a) x") {
    val For(List(Enumerator.Generator(Pat.Var.Term(TermName("a")), TermName("b")),
                 Enumerator.Guard(TermName("c")),
                 Enumerator.Val(Pat.Var.Term(TermName("x")), TermName("a"))),
            TermName("x")) = term("for (a <- b; if c; x = a) x")

  }
  test("for (a <- b; if c; x = a) yield x") {
    val ForYield(List(Enumerator.Generator(Pat.Var.Term(TermName("a")), TermName("b")),
                      Enumerator.Guard(TermName("c")),
                      Enumerator.Val(Pat.Var.Term(TermName("x")), TermName("a"))),
                 TermName("x")) = term("for (a <- b; if c; x = a) yield x")
  }

  test("f(_)") {
    val Apply(TermName("f"), List(Placeholder())) = term("f(_)")
  }

  test("_ + 1") {
    val ApplyInfix(Placeholder(), TermName("+"), Nil, Lit.Int(1) :: Nil) = term("_ + 1")
  }

  test("1 + _") {
    val ApplyInfix(Lit.Int(1), TermName("+"), Nil, Placeholder() :: Nil) = term("1 + _")
  }

  test("f _") {
    val Eta(TermName("f")) = term("f _")
  }

  test("new {}") {
    val New(Template(Nil, Nil, EmptySelf(), Some(Nil))) = term("new {}")
  }

  test("new { x }") {
    val New(Template(Nil, Nil, EmptySelf(), Some(Term.Name("x") :: Nil))) = term("new { x }")
  }

  test("new A") {
    val New(templ @ Template(Nil, Ctor.Name("A") :: Nil, EmptySelf(), None)) = term("new A")
    // TODO: revisit this once we have trivia in place
    // assert(templ.hasStats === false)
  }

  test("new A {}") {
    val New(templ @ Template(Nil, Ctor.Name("A") :: Nil, EmptySelf(), Some(Nil))) = term("new A {}")
    // TODO: revisit this once we have trivia in place
    // assert(templ.hasStats === true)
  }

  test("new A with B") {
    val New(Template(Nil, Ctor.Name("A") :: Ctor.Name("B") :: Nil, EmptySelf(), None)) =
      term("new A with B")
  }

  test("new { val x: Int = 1 } with A") {
    val New(Template(Defn.Val(Nil, List(Pat.Var.Term(TermName("x"))), Some(TypeName("Int")), Lit.Int(1)) :: Nil,
                     Ctor.Name("A") :: Nil, EmptySelf(), None)) =
      term("new { val x: Int = 1 } with A")
  }

  test("new { self: T => }") {
    val New(Template(Nil, Nil, Term.Param(Nil, TermName("self"), Some(TypeName("T")), None), Some(Nil))) =
      term("new { self: T => }")
  }

  test("a + (b = c)") {
    val ApplyInfix(TermName("a"), TermName("+"), Nil,
                   Arg.Named(TermName("b"), TermName("c")) :: Nil) = term("a + (b = c)")
  }

  test("(a = b) + c") {
    val ApplyInfix(Assign(TermName("a"), TermName("b")), TermName("+"), Nil,
                   TermName("c") :: Nil) = term("(a = b) + c")
  }

  test("a + (b = c).d") {
    val ApplyInfix(TermName("a"), TermName("+"), Nil,
                   Select(Assign(TermName("b"), TermName("c")), TermName("d")) :: Nil) =
      term("a + (b = c).d")
  }

  test("a + (b: _*)") {
    val ApplyInfix(TermName("a"), TermName("+"), Nil,
                   Arg.Repeated(TermName("b")) :: Nil) = term("a + (b: _*)")
  }

  test("local class") {
    val Term.Block(List(
      Defn.Class(
        List(Mod.Case()), Type.Name("C"), Nil,
        Ctor.Primary(Nil, Ctor.Name("this"), List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)))),
        EmptyTemplate()))) = term("{ case class C(x: Int); }")
  }
}
