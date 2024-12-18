package scala.meta.tests
package quasiquotes

import org.scalameta.invariants.InvariantFailedException
import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.trees.Origin

// Deconstructon ascription is a Scala 2 only feature for now,
// so part of the shared SuccessSuite was moved to this Scala 2 only suite
class DeconstructionAscriptionsSuite extends TreeSuiteBase {
  test("deconstruction ascriptions") {
    val q"foo(..${xs: List[Term]})" = q"foo(x, y)"
    assertEquals(xs.toString, "List(x, y)")
    val q"foo(...${xss: List[List[Term]]})" = q"foo(x, y)"
    assertEquals(xss.toString, "List(List(x, y))")
    val q"var foo = ${x: Term}" = q"var foo = x"
    assertEquals(x.toString, "x")
  }

  test("case q\"foo({x: Int})\"") {
    q"foo(42)" match {
      case q"$foo(${x: Int})" =>
        assertTree(foo)(tname("foo"))
        assertEquals(x, 42)
    }
  }

  test("1 q\"foo(x, ..ys, z)\"") {
    val q"foo($x, ..$ys, $z)" = q"foo(1, 2, 3)"
    assertTree(x)(int(1))
    assertEquals(ys.toString, "List(2)")
    assertTrees(ys: _*)(int(2))
    assertTree(z)(int(3))
  }

  test("2 q\"name.this.id\"") {
    val name = q"A"
    val x = q"B"
    // inconsistency with the test above planned, since Name can't be constructed directly
    assertTree(q"$name.this.$x")(Term.Select(Term.This(tname("A")), tname("B")))
  }

  test("1 val q\"def x = {body: Int}\"") {
    val q"def x = ${body: Int}" = q"def x = 42"
    assertEquals(body, 42)
  }

  test("case q\"foo({x: Int}, ..ys, z)\"") {
    q"foo(1, 2, 3)" match {
      case q"$_(${x: Int}, ..$y, $z)" =>
        assertEquals(x, 1)
        assertEquals(y.map(_.structure), List("Lit.Int(2)"))
        assertTree(z)(int(3))
    }
  }
}
