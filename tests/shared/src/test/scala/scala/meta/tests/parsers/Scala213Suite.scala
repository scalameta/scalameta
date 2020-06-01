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

}
