package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala213

class Scala213Suite extends ParseSuite {
  checkOK("def foo(implicit x: => Int) = 1")
  checkOK("def foo(implicit y: Int, x: => Int) = 1")

  test("literal-types") {
    Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), Some(Lit.Int(42)), Lit.Int(42)) == templStat(
      "val a: 42 = 42"
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

    assertNoDiff(
      templStat("val enum = 3").structure,
      """Defn.Val(Nil, List(Pat.Var(Term.Name("enum"))), None, Lit.Int(3))"""
    )
  }
}
