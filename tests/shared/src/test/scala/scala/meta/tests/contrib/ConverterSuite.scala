package scala.meta.tests
package contrib

import scala.meta._
import scala.meta.contrib._

import munit.FunSuite

class ConverterSuite extends FunSuite {

  test("asType") {
    val termName = Term.Name("Foo")
    assert(termName.asType.isEqual(Type.Name("Foo")))
  }

  test("asTerm") {
    val typeName = Type.Name("Foo")
    assert(typeName.asTerm.isEqual(Term.Name("Foo")))
  }

  test("asPat") {
    val termName = Term.Name("Foo")
    assert(termName.asPat.isEqual(Pat.Var(Term.Name("Foo"))))
  }
}
