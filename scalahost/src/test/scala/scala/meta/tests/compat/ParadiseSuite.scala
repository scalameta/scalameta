package scala.meta.tests
package compat

import org.scalatest._
import org.scalameta.tests._

class ParadiseSuite extends FunSuite {
  test("macro annotations successfully expand under scalahost") {
    @kase class Point(x: Int, y: Int)
    assert(Point(40, 2).toString === "Point(40,2)")
  }
}