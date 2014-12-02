package scala.meta

import org.scalatest._
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
}
