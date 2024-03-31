package scala.meta.tests
package trees

import scala.meta._
import scala.meta.internal.trees._

import munit._

class TreeSuite extends FunSuite {
  test("Name.unapply") {
    assert(Name.unapply(q"a").contains("a"))
    assert(Name.unapply(t"a").contains("a"))
  }

  Seq(("+", Unary.Plus), ("-", Unary.Minus), ("~", Unary.Tilde), ("!", Unary.Not)).foreach {
    case (op, unary) => test(s"Unary.$unary") {
        assertEquals(unary.op, op)
        assertEquals(Unary.opMap.get(op), Some(unary))
        assert(op.isUnaryOp)
      }
  }

  test(s"Unary opMap size")(assertEquals(Unary.opMap.size, 4))

}
