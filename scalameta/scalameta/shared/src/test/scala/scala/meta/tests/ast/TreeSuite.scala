package scala.meta.tests.ast

import scala.meta._

import org.scalatest._

class TreeSuite extends FunSuite {
  test("Name.unapply") {
    assert(Name.unapply(q"a").contains("a"))
    assert(Name.unapply(t"a").contains("a"))
  }
}
