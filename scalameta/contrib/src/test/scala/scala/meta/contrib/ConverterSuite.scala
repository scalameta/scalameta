package scala.meta.contrib.conversion

import org.scalatest.FunSuite

import scala.meta.contrib._
import scala.meta._
import scala.meta.contrib.equality.Structurally

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
    assert(termName.asPat.isEqual(Pat.Var.Term(Term.Name("Foo"))))
  }

  test("asCtorRef") {
    val termName = Term.Name("Foo")
    assert(termName.asCtorRef.isEqual(Ctor.Ref.Name("Foo")))
  }

}
