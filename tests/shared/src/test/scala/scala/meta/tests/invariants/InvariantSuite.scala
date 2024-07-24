package scala.meta.tests
package invariants

import org.scalameta._
import org.scalameta.invariants._

import munit._

class InvariantSuite extends TreeSuiteBase {
  test("more informative error messages") {
    interceptMessage[InvariantFailedException](
      """|invariant failed:
         |when verifying x.>(3)
         |found that x.>(3) is false
         |where x = 2
         |""".stripMargin.lf2nl
    ) {
      val x = 2
      require(x > 3)
    }
  }

  test("even more informative error messages") {
    interceptMessage[InvariantFailedException](
      """|invariant failed:
         |when verifying C.this.x.!=(3).&&(org.scalameta.`package`.debug(C.this.x, y))
         |found that C.this.x is equal to 3
         |where C = C(3)
         |where C.this.x = 3
         |where y = 2
         |""".stripMargin.lf2nl
    ) {
      val y = 2
      case class C(x: Int) {
        require(x != 3 && debug(x, y))
      }
      C(3)
    }
  }

  test("unreachable - 1") {
    interceptMessage[UnreachableError]("this code path should've been unreachable")(unreachable)
  }

  test("unreachable - 2") {
    interceptMessage[UnreachableError](
      """|this code path should've been unreachable
         |where C.this.x = 3
         |where y = 2
         |""".stripMargin.lf2nl
    ) {
      val y = 2
      case class C(x: Int) {
        unreachable(debug(x, y))
      }
      C(3)
    }
  }

  test("don't evaluate debug")(require(true && debug(throw new Exception)))
}
