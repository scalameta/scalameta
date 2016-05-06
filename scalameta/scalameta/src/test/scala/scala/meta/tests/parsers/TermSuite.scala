package scala.meta.tests
package parsers

import scala.meta._, Term.{Name => TermName, _}, Type.{Name => TypeName}, Name.{Anonymous, Indeterminate}
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
    val This(Indeterminate("foo")) = term("foo.this")
  }

  test("this") {
    val This(Anonymous()) = term("this")
  }

  test("a.super[b].c") {
    val Select(Super(Indeterminate("a"), Indeterminate("b")),
               TermName("c")) = term("a.super[b].c")
  }

  test("super[b].c") {
    val Select(Super(Anonymous(), Indeterminate("b")),
               TermName("c")) = term("super[b].c")
  }

  test("a.super.c") {
    val Select(Super(Indeterminate("a"), Anonymous()),
               TermName("c")) = term("a.super.c")
  }

  test("super.c") {
    val Select(Super(Anonymous(), Anonymous()), TermName("c")) = term("super.c")
  }

  test("s\"a $b c\"") {
    val Interpolate(TermName("s"), Lit("a ") :: Lit(" c") :: Nil,
                    TermName("b") :: Nil) = term("s\"a $b c\"")
  }

  test("f(0)") {
    val Apply(TermName("f"), Lit(0) :: Nil) = term("f(0)")
  }

  test("f(x = 0)") {
    val Apply(TermName("f"), Arg.Named(TermName("x"), Lit(0)) :: Nil) = term("f(x = 0)")
  }

  test("f(x: _*)") {
    val Apply(TermName("f"), Arg.Repeated(TermName("x")) :: Nil) = term("f(x: _*)")
  }

  test("f(x = xs: _*)") {
    val Term.Apply(Term.Name("f"), Seq(Term.Arg.Named(Term.Name("x"), Term.Arg.Repeated(Term.Name("xs"))))) = term("f(x = xs: _*)")
  }

  test("a + ()") {
    val ApplyInfix(TermName("a"), TermName("+"), Nil, Nil) = term("a + ()")
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
    val Assign(TermName("a"), Lit(true)) = term("a = true")
  }

  test("a(0) = true") {
    val Update(TermName("a"), (Lit(0) :: Nil) :: Nil, Lit(true)) = term("a(0) = true")
  }

  test("return") {
    val ret @ Return(Lit(())) = term("return")
    // TODO: revisit this once we have trivia in place
    // assert(ret.hasExpr === false)
  }

  test("return 1") {
    val ret @ Return(Lit(1)) = term("return 1")
    // TODO: revisit this once we have trivia in place
    // assert(ret.hasExpr === true)
  }

  test("throw 1") {
    val Throw(Lit(1)) = term("throw 1")
  }

  test("1: Int") {
    val Ascribe(Lit(1), TypeName("Int")) = term("1: Int")
  }

  test("1: @foo") {
    val Annotate(Lit(1), Mod.Annot(Ctor.Name("foo")) :: Nil) = term("1: @foo")
  }

  test("(true, false)") {
    val Tuple(Lit(true) :: Lit(false) :: Nil) = term("(true, false)")
  }

  test("{ true; false }") {
    val Block(Lit(true) :: Lit(false) :: Nil) = term("{ true; false }")
  }

  test("{ true }") {
    val Block(Lit(true) :: Nil) = term("{ true }")
  }

  test("if (true) true else false") {
    val iff @ If(Lit(true), Lit(true), Lit(false)) = term("if (true) true else false")
    // TODO: revisit this once we have trivia in place
    // assert(iff.hasElsep === true)
  }

  test("if (true) true; else false") {
    val iff @ If(Lit(true), Lit(true), Lit(false)) = term("if (true) true; else false")
    // TODO: revisit this once we have trivia in place
    // assert(iff.hasElsep === true)
  }

  test("if (true) true") {
    val iff @ If(Lit(true), Lit(true), Lit(())) = term("if (true) true")
    // TODO: revisit this once we have trivia in place
    // assert(iff.hasElsep === false)
  }

  test("() => x") {
    val Term.Function(Nil, Term.Name("x")) = term("() => x")
    val Term.Function(Nil, Term.Block(List(Term.Name("x")))) = blockStat("() => x")
    val Term.Function(Nil, Term.Name("x")) = templStat("() => x")
  }

  test("(()) => x") {
    val Term.Function(Nil, Term.Name("x")) = term("(()) => x")
    val Term.Function(Nil, Term.Block(List(Term.Name("x")))) = blockStat("(()) => x")
    val Term.Function(Nil, Term.Name("x")) = templStat("(()) => x")
  }

  test("x => x") {
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x")) = term("x => x")
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Block(List(Term.Name("x")))) = blockStat("x => x")
    intercept[ParseException] { templStat("x => x") }
  }

  test("(x) => x") {
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x")) = term("(x) => x")
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Block(List(Term.Name("x")))) = blockStat("(x) => x")
    intercept[ParseException] { templStat("(x) => x") }
  }

  test("_ => x") {
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Name("x")) = term("_ => x")
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Block(List(Term.Name("x")))) = blockStat("_ => x")
    intercept[ParseException] { templStat("_ => x") }
  }

  test("(_) => x") {
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Name("x")) = term("(_) => x")
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Block(List(Term.Name("x")))) = blockStat("(_) => x")
    intercept[ParseException] { templStat("(_) => x") }
  }

  test("x: Int => x") {
    // LAWL: this is how scalac's parser works
    val Term.Ascribe(Term.Name("x"), Type.Function(List(Type.Name("Int")), Type.Name("x"))) = term("x: Int => x")
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)), Term.Block(List(Term.Name("x")))) = blockStat("x: Int => x")
    intercept[ParseException] { templStat("x: Int => x") }
  }

  test("(x: Int) => x") {
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)), Term.Name("x")) = term("(x: Int) => x")
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)), Term.Block(List(Term.Name("x")))) = blockStat("(x: Int) => x")
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)), Term.Name("x")) = templStat("(x: Int) => x")
  }

  test("_: Int => x") {
    val Term.Ascribe(Term.Placeholder(), Type.Function(List(Type.Name("Int")), Type.Name("x"))) = term("_: Int => x")
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)), Term.Block(List(Term.Name("x")))) = blockStat("_: Int => x")
    intercept[ParseException] { templStat("_: Int => x") }
  }

  test("(_: Int) => x") {
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)), Term.Name("x")) = term("(_: Int) => x")
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)), Term.Block(List(Term.Name("x")))) = blockStat("(_: Int) => x")
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)), Term.Name("x")) = templStat("(_: Int) => x")
  }

  test("x: Int, y: Int => x") {
    intercept[ParseException] { term("x: Int, y: Int => x") }
    intercept[ParseException] { blockStat("x: Int, y: Int => x") }
    intercept[ParseException] { templStat("x: Int, y: Int => x") }
  }

  test("(x: Int, y: Int) => x") {
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None), Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)), Term.Name("x")) = term("(x: Int, y: Int) => x")
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None), Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)), Term.Block(List(Term.Name("x")))) = blockStat("(x: Int, y: Int) => x")
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None), Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)), Term.Name("x")) = templStat("(x: Int, y: Int) => x")
  }

  test("{ implicit x => () }") {
    val Block(Function(Term.Param(Mod.Implicit() :: Nil, TermName("x"), None, None) :: Nil,
                       Block(Lit(()) :: Nil)) :: Nil) = term("{ implicit x => () }")
  }

  test("1 match { case 1 => true }") {
    val Match(Lit(1), Case(Lit(1), None, Lit(true)) :: Nil) =
      term("1 match { case 1 => true }")
  }

  test("1 match { case 1 => }") {
    val Match(Lit(1), Case(Lit(1), None, Term.Block(Nil)) :: Nil) =
      term("1 match { case 1 => }")
  }

  test("1 match { case 1 if true => }") {
    val Match(Lit(1), Case(Lit(1), Some(Lit(true)), Term.Block(Nil)) :: Nil) =
      term("1 match { case 1 if true => }")
  }

  test("try 1") {
    val TryWithCases(Lit(1), Nil, None) = term("try 1")
  }

  test("try 1 catch 1") {
    val TryWithTerm(Lit(1), Lit(1), None) = term("try 1 catch 1")
  }

  test("try 1 catch { case _ => }") {
    val TryWithCases(Lit(1), Case(Pat.Wildcard(), None, Term.Block(Nil)) :: Nil, None) =
      term("try 1 catch { case _ => }")
  }

  test("try 1 finally 1") {
    val TryWithCases(Lit(1), Nil, Some(Lit(1))) = term("try 1 finally 1")
  }

  test("{ case 1 => () }") {
    val PartialFunction(Case(Lit(1), None, Lit(())) :: Nil) =
      term("{ case 1 => () }")
  }

  test("while (true) false") {
    val While(Lit(true), Lit(false)) = term("while (true) false")
  }

  test("do false while(true)") {
    val Do(Lit(false), Lit(true)) = term("do false while(true)")
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
    val ApplyInfix(Placeholder(), TermName("+"), Nil, Lit(1) :: Nil) = term("_ + 1")
  }

  test("1 + _") {
    val ApplyInfix(Lit(1), TermName("+"), Nil, Placeholder() :: Nil) = term("1 + _")
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
    val New(Template(Defn.Val(Nil, List(Pat.Var.Term(TermName("x"))), Some(TypeName("Int")), Lit(1)) :: Nil,
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

  test("xml literal - 1") {
    val Term.Block(List(
      Defn.Val(Nil, List(Pat.Var.Term(Term.Name("x"))), None, Term.Xml(List(Lit("<p/>")), Nil)),
      Defn.Val(Nil, List(Pat.Var.Term(Term.Name("y"))), None, Term.Name("x")))) =
    term("""{
      val x = <p/>
      val y = x
    }""")
  }

  test("implicit closure") {
    val Term.Apply(Term.Name("Action"), List(
      Term.Block(List(
        Term.Function(
          List(Term.Param(List(Mod.Implicit()), Term.Name("request"), Some(Type.Apply(Type.Name("Request"), List(Type.Name("AnyContent")))), None)),
          Term.Block(List(Term.Name("Ok")))))))) =
      term("Action { implicit request: Request[AnyContent] => Ok }")
  }

  test("#312") {
    val Term.Block(Seq(
      Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("x"))), None, Term.Ascribe(Term.Name("yz"), Type.Tuple(Seq(Type.Name("Y"), Type.Name("Z"))))),
      Term.Tuple(Seq(Term.Name("x"), Term.Name("x"))))) =
    term("""{
      val x = yz: (Y, Z)
      (x, x)
    }""")
  }

  test("spawn { var v: Int = _; ??? }") {
    val Term.Apply(
      Term.Name("spawn"),
      Seq(
        Term.Block(Seq(
          Defn.Var(Nil, Seq(Pat.Var.Term(Term.Name("v"))), Some(Type.Name("Int")), None), Term.Name("???"))))) =
    term("spawn { var v: Int = _; ??? }")
  }

  test("#345") {
    val Term.Match(_, Seq(Case(_, _, rhs), _)) = term("""x match {
      case x => true
      // sobaka
      case y => y
    }""")
    assert(rhs.tokens.show[Structure] === "Tokens(true [26..30))")
  }

  test("a + (bs: _*) * c") {
    intercept[ParseException] { term("a + (bs: _*) * c") }
  }

  test("a + (c, d) * e") {
    val Term.ApplyInfix(
      Term.Name("a"), Term.Name("+"), Nil, Seq(
        Term.ApplyInfix(
          Term.Tuple(Seq(Term.Name("c"), Term.Name("d"))), Term.Name("*"), Nil,
          Seq(Term.Name("e"))))) =
    term("a + (c, d) * e")
  }

  test("a * (c, d) + e") {
    val Term.ApplyInfix(
      Term.ApplyInfix(
        Term.Name("a"), Term.Name("*"), Nil,
        Seq(Term.Name("c"), Term.Name("d"))),
      Term.Name("+"), Nil, Seq(Term.Name("e"))) =
    term("a * (c, d) + e")
  }

  test("(a + b) c") {
    val Term.Select(Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Seq(Term.Name("b"))), Term.Name("c")) =
      term("(a + b) c")
  }

  test("a + b c") {
    val Term.Select(Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Seq(Term.Name("b"))), Term.Name("c")) =
      term("a + b c")
  }

  test("disallow parse[Stat] on statseqs") {
    intercept[ParseException]{ stat("hello; world") }
  }

  test("\"stat;\".parse[Stat]") {
    val Term.Name("stat") = stat("stat;")
  }

  test("\"stat;\".parse[Term]") {
    intercept[ParseException]{ term("stat;") }
  }

  test("$_") {
    intercept[ParseException](term(""" q"x + $_" """))
  }
}
