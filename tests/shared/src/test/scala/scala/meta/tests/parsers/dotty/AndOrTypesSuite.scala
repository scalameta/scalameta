package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers.ParseSuite

import scala.meta._, Type.{Name => TypeName, _}
import scala.meta.dialects.Dotty

class AndOrTypesSuite extends ParseSuite {
  
  /** 
   * 
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/other-new-features/named-typeargs.html
   *  
   */

  test("f[A = Int, B = List]()") {
    val namedParam1 = Type.NamedParam(Type.Name("A"), Type.Name("Int"))
    val namedParam2 = Type.NamedParam(Type.Name("B"), Type.Name("List"))
    assertEquals(
      stat("f[A = Int, B = List]()"),
      Term.Apply(Term.ApplyType(Term.Name("f"), List(namedParam1, namedParam2)), Nil): Stat
    )
  }

}

