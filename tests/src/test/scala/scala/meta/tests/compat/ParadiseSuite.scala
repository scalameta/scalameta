package scala.meta.tests
package compat

import org.scalatest._

class ParadiseSuite extends FunSuite {
  test("macro annotations successfully expand under scalahost") {
    @hello object M
    assert(M.hello === "hello")
  }
}