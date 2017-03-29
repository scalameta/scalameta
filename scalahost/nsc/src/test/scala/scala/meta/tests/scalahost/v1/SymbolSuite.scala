package scala.meta.tests
package scalahost
package v1

import org.scalatest._
import scala.meta._

class SymbolSuite extends FunSuite {
  test("Symbol.name") {
    assert(Symbol("_root_.foo.Bar#").name.syntax === "Bar")
    assert(Symbol("_root_.foo.bar.").name.syntax === "bar")
    assert(Symbol("_root_.foo.m(I)I.").name.syntax === "m")
    assert(Symbol("_root_.foo.Bar#[T]").name.syntax === "T")
    assert(Symbol("_root_.foo.Bar#(x)").name.syntax === "x")
    assert(Symbol("_root_.foo.Bar#self=>").name.syntax === "self")
  }

  test("Symbol.fullName") {
    assert(Symbol("_root_.").fullName.syntax === "_root_")
    assert(Symbol("_root_#").fullName.syntax === "_root_")
    assert(Symbol("_root_.foo.Bar#").fullName.syntax === "_root_.foo.Bar")
    assert(Symbol("_root_.foo.bar.").fullName.syntax === "_root_.foo.bar")
    assert(Symbol("_root_.foo.m(I)I.").fullName.syntax === "m")
    assert(Symbol("_root_.foo.Bar#[T]").fullName.syntax === "T")
    assert(Symbol("_root_.foo.Bar#(x)").fullName.syntax === "x")
    assert(Symbol("_root_.foo.Bar#self=>").fullName.syntax === "self")
  }
}