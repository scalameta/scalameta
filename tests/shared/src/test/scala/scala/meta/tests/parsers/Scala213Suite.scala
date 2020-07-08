package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala213

class Scala213Suite extends ParseSuite {
  checkOK("def foo(implicit x: => Int) = 1")
  checkOK("def foo(implicit y: Int, x: => Int) = 1")

  test("literal-types") {
    // https://docs.scala-lang.org/sips/42.type.html
    runAssert(
      "val a: 42 = 42",
      """Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), Some(Lit.Int(42)), Lit.Int(42))"""
    )

    runAssert(
      "val a: -2 = -2",
      """Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), Some(Lit.Int(-2)), Lit.Int(-2))"""
    )

    runAssert(
      "def foo(a: 3.14159f): .1",
      """Decl.Def(Nil, Term.Name("foo"), Nil, List(List(Term.Param(Nil, Term.Name("a"), Some(Lit.Float(3.14159f)), None))), Lit.Double(.1))"""
    )

    runAssert(
      "def foo(x: 'a'): Option['z']",
      """Decl.Def(Nil, Term.Name("foo"), Nil, List(List(Term.Param(Nil, Term.Name("x"), Some(Lit.Char('a')), None))), Type.Apply(Type.Name("Option"), List(Lit.Char('z'))))"""
    )

    runAssert(
      "def bar[T <: 1](t: T): T = t",
      """Defn.Def(Nil, Term.Name("bar"), List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, Some(Lit.Int(1))), Nil, Nil)), List(List(Term.Param(Nil, Term.Name("t"), Some(Type.Name("T")), None))), Some(Type.Name("T")), Term.Name("t"))"""
    )
  }

  test("identifier-types") {
    // "x" is a class with def =>>(x: Int): Int, in dotty ==> is keyword and it will be error here
    assertNoDiff(
      templStat("val c = x =>> 3").structure,
      """Defn.Val(Nil, List(Pat.Var(Term.Name("c"))), None, Term.ApplyInfix(Term.Name("x"), Term.Name("=>>"), Nil, List(Lit.Int(3))))"""
    )

    assertNoDiff(
      templStat("val given = 3").structure,
      """Defn.Val(Nil, List(Pat.Var(Term.Name("given"))), None, Lit.Int(3))"""
    )

    runAssert(
      "val enum = 3",
      """Defn.Val(Nil, List(Pat.Var(Term.Name("enum"))), None, Lit.Int(3))"""
    )
  }

  private def runAssert(code: String, expected: String): Unit = {
    assertNoDiff(
      templStat(code).structure,
      expected
    )
  }
}
