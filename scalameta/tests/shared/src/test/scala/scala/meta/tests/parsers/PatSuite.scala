package scala.meta.tests
package parsers

import scala.meta._, Pat._
import scala.meta.dialects.Scala211

class PatSuite extends ParseSuite {
  test("_") {
    val Wildcard() = pat("_")
  }

  test("a @ _") {
    val Bind(Var(Term.Name("a")), Wildcard()) = pat("a @ _")
  }

  test("a") {
    val Var(Term.Name("a")) = pat("a")
  }

  test("`a`") {
    val Term.Name("a") = pat("`a`")
  }

  test("a: Int") {
    val Typed(Var(Term.Name("a")), Type.Name("Int")) = pat("a: Int")
  }

  test("_: Int") {
    val Typed(Wildcard(), Type.Name("Int")) = pat("_: Int")
  }

  test("_: t") {
    val Typed(Wildcard(), Type.Name("t")) = pat("_: t")
  }

  test("_: F[t]") {
    val Typed(Wildcard(), Type.Apply(Type.Name("F"), Type.Var(Type.Name("t")) :: Nil)) = pat("_: F[t]")
  }

  test("_: F[_]") {
    val Typed(Wildcard(), Type.Apply(Type.Name("F"), Type.Placeholder(Type.Bounds(None, None)) :: Nil)) = pat("_: F[_]")
  }

  test("_: (t Map u)") {
    val Typed(Wildcard(), Type.ApplyInfix(Type.Name("t"), Type.Name("Map"), Type.Name("u"))) = pat("_: (t Map u)")
  }

  test("_: T Map U") {
    intercept[ParseException] { pat("_: T Map U") }
  }

  test("_: T forSome { type U }") {
    intercept[ParseException] { pat("_: T forSome { type U }") }
  }

  test("x@(__ : Y)") {
    val Pat.Bind(Pat.Var(Term.Name("x")), Pat.Typed(Pat.Var(Term.Name("__")), Type.Name("Y"))) = pat("x@(__ : Y)")
  }

  test("foo(x)") {
    val Extract(Term.Name("foo"), Var(Term.Name("x")) :: Nil) = pat("foo(x)")
  }

  test("foo(_*)") {
    val Extract(Term.Name("foo"), SeqWildcard() :: Nil) = pat("foo(_*)")
  }

  test("foo(x @ _*)") {
    val Extract(Term.Name("foo"), Bind(Var(Term.Name("x")), SeqWildcard()) :: Nil) = pat("foo(x @ _*)")
  }

  test("a :: b") {
    val ExtractInfix(Var(Term.Name("a")), Term.Name("::"), Var(Term.Name("b")) :: Nil) = pat("a :: b")
  }

  test("a :: ()") {
    val ExtractInfix(Var(Term.Name("a")), Term.Name("::"), Nil) = pat("a :: ()")
  }

  test("1 | 2 | 3") {
    val Alternative(Lit(1), Lit(2)) = pat("1 | 2")
    val Alternative(Lit(1), Alternative(Lit(2), Lit(3))) = pat("1 | 2 | 3")
  }

  test("()") {
    val Lit(()) = pat("()")
  }

  test("(true, false)") {
    val Tuple(Lit(true) :: Lit(false) :: Nil) = pat("(true, false)")
  }

  test("foo\"bar\"") {
    val Interpolate(Term.Name("foo"), Lit("bar") :: Nil, Nil) = pat("foo\"bar\"")
  }

  test("foo\"a $b c\"") {
    val Interpolate(Term.Name("foo"), Lit("a ") :: Lit(" c") :: Nil, Var(Term.Name("b")) :: Nil) = pat("foo\"a $b c\"")
  }

  test("foo\"${b @ foo()}\"") {
    val Interpolate(Term.Name("foo"), Lit("") :: Lit("") :: Nil, Bind(Var(Term.Name("b")), Extract(Term.Name("foo"), Nil)) :: Nil) = pat("foo\"${b @ foo()}\"")
  }

  test("$_") {
    val Pat.Interpolate(Term.Name("q"), List(Lit("x + "), Lit("")), List(Pat.Wildcard())) = pat(""" q"x + $_" """)
  }

  test("#501") {
    intercept[ParseException] { pat("case List(_: BlockExpr, _: MatchExpr, x:_*)  ⇒ false") }
  }

  test("<a>{_*}</a>") {
    val Pat.Xml(List(Lit("<a>"), Lit("</a>")), List(SeqWildcard())) = pat("<a>{_*}</a>")
  }

  test("<a>{ns @ _*}</a>") {
    val Pat.Xml(List(Lit("<a>"), Lit("</a>")), List(Bind(Var(Term.Name("ns")), SeqWildcard()))) = pat("<a>{ns @ _*}</a>")
  }

}
