package scala.meta.tests
package invariants

import org.scalatest._
import org.scalameta._
import org.scalameta.invariants._

class InvariantSuite extends FunSuite {
  test("more informative error messages") {
    val x = 2
    try require(x > 3)
    catch {
      case ex: InvariantFailedException =>
        assert(ex.getMessage === """
          |invariant failed:
          |when verifying x.>(3)
          |found that x.>(3) is false
          |where x = 2
        """.trim.stripMargin)
    }
  }

  test("even more informative error messages") {
    val y = 2
    try {
      case class C(x: Int) { require(x != 3 && debug(x, y)) }
      new C(3)
    } catch {
      case ex: InvariantFailedException =>
        assert(ex.getMessage === """
          |invariant failed:
          |when verifying C.this.x.!=(3).&&(org.scalameta.`package`.debug(C.this.x, y))
          |found that C.this.x is equal to 3
          |where C = C(3)
          |where C.this.x = 3
          |where y = 2
        """.trim.stripMargin)
    }
  }

  test("unreachable - 1") {
    try {
      unreachable
    } catch {
      case ex: UnreachableError =>
        assert(ex.getMessage === """
          |this code path should've been unreachable
        """.trim.stripMargin)
    }
  }

  test("unreachable - 2") {
    val y = 2
    try {
      case class C(x: Int) { unreachable(debug(x, y)) }
      new C(3)
    } catch {
      case ex: UnreachableError =>
        assert(ex.getMessage === """
          |this code path should've been unreachable
          |where C.this.x = 3
          |where y = 2
        """.trim.stripMargin)
    }
  }

  test("don't evaluate debug")  {
    require(true && debug(throw new Exception))
  }
}
