package scala.meta.tests
package scalameta
package parsers

import scala.meta.internal.ast._, Pat.{Type => _, _}
import scala.meta.dialects.Scala211

class PatSuite extends ParseSuite {
  test("_") {
    val Wildcard() = pat("_")
  }

  test("a @ _") {
    val Bind(Var.Term(Term.Name("a")), Wildcard()) = pat("a @ _")
  }

  test("a") {
    val Var.Term(Term.Name("a")) = pat("a")
  }

  test("`a`") {
    val Term.Name("a") = pat("`a`")
  }

  test("a: Int") {
    val Typed(Var.Term(Term.Name("a")), Type.Name("Int")) = pat("a: Int")
  }

  test("_: Int") {
    val Typed(Wildcard(), Type.Name("Int")) = pat("_: Int")
  }

  test("_: t") {
    val Typed(Wildcard(), Type.Name("t")) = pat("_: t")
  }

  test("_: F[t]") {
    val Typed(Wildcard(), Pat.Type.Apply(Type.Name("F"), Var.Type(Type.Name("t")) :: Nil)) = pat("_: F[t]")
  }

  test("_: F[_]") {
    val Typed(Wildcard(), Pat.Type.Apply(Type.Name("F"), Pat.Type.Wildcard() :: Nil)) = pat("_: F[_]")
  }

  test("_: (t Map u)") {
    val Typed(Wildcard(), Pat.Type.ApplyInfix(Type.Name("t"), Type.Name("Map"), Type.Name("u"))) = pat("_: (t Map u)")
  }

  test("foo(x)") {
    val Extract(Term.Name("foo"), Nil, Var.Term(Term.Name("x")) :: Nil) = pat("foo(x)")
  }

  test("foo(_*)") {
    val Extract(Term.Name("foo"), Nil, Arg.SeqWildcard() :: Nil) = pat("foo(_*)")
  }

  test("foo(x @ _*)") {
    val Extract(Term.Name("foo"), Nil, Bind(Var.Term(Term.Name("x")), Arg.SeqWildcard()) :: Nil) = pat("foo(x @ _*)")
  }

  test("a :: b") {
    val ExtractInfix(Var.Term(Term.Name("a")), Term.Name("::"), Var.Term(Term.Name("b")) :: Nil) = pat("a :: b")
  }

  test("1 | 2 | 3") {
    val Alternative(Lit.Int(1), Lit.Int(2)) = pat("1 | 2")
    val Alternative(Lit.Int(1), Alternative(Lit.Int(2), Lit.Int(3))) = pat("1 | 2 | 3")
  }

  test("(true, false)") {
    val Tuple(Lit.Bool(true) :: Lit.Bool(false) :: Nil) = pat("(true, false)")
  }

  test("foo\"bar\"") {
    val Interpolate(Term.Name("foo"), Lit.String("bar") :: Nil, Nil) = pat("foo\"bar\"")
  }

  test("foo\"a $b c\"") {
    val Interpolate(Term.Name("foo"), Lit.String("a ") :: Lit.String(" c") :: Nil, Var.Term(Term.Name("b")) :: Nil) = pat("foo\"a $b c\"")
  }

  test("foo\"${b @ foo()}\"") {
    val Interpolate(Term.Name("foo"), Lit.String("") :: Lit.String("") :: Nil, Bind(Var.Term(Term.Name("b")), Extract(Term.Name("foo"), Nil, Nil)) :: Nil) = pat("foo\"${b @ foo()}\"")
  }
}
