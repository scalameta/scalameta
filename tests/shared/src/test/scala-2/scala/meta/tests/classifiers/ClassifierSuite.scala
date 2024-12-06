package org.scalameta.tests
package classifiers

import scala.meta.classifiers._
import scala.meta.internal.classifiers.classifier

import munit._

trait Unclassifiable
class Derived extends Unclassifiable

trait MyToken
object MyToken {
  implicit def tokenIsClassifiable[T <: MyToken]: Classifiable[T] = null
}
class MyIdent extends MyToken

trait Manual
object Manual {
  def unapply(x: MyToken): Boolean = x.isInstanceOf[MyIdent]
  implicit def classifier[T <: MyToken]: Classifier[T, Manual] = new Classifier[T, Manual] {
    def apply(x: T): Boolean = Manual.unapply(x)
  }
}

@classifier
trait Auto1
object Auto1 {
  def unapply(x: MyToken): Boolean = x.isInstanceOf[MyIdent]
}

@classifier
trait Auto2 {
  def unapply(x: MyToken): Boolean = x.isInstanceOf[MyIdent]
}

class ClassifierSuite extends FunSuite {
  test("unclassifiable inheritance") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      (??? : Unclassifiable).is[Derived]
    """
      ),
      "value is is not a member of org.scalameta.tests.classifiers.Unclassifiable"
    )
  }

  test("unclassifiable typeclass") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      (??? : Unclassifiable).is[Manual]
      (??? : Unclassifiable).is[Auto1]
      (??? : Unclassifiable).is[Auto2]
    """
      ),
      "value is is not a member of org.scalameta.tests.classifiers.Unclassifiable"
    )
  }

  test("classifiable inheritance") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      (??? : MyToken).is[MyIdent]
    """
      ),
      "don't know how to check whether org.scalameta.tests.classifiers.MyToken is org.scalameta.tests.classifiers.MyIdent"
    )

    assertEquals(
      typecheckError(
        """
      import scala.meta._
      (??? : MyIdent).is[MyIdent]
    """
      ),
      "don't know how to check whether org.scalameta.tests.classifiers.MyIdent is org.scalameta.tests.classifiers.MyIdent"
    )
  }

  test("classifiable typeclass") {
    val ident1: MyToken = new MyIdent
    assert(ident1.is[Manual])
    assert(ident1.is[Auto1])
    assert(ident1.is[Auto2])

    val ident2: MyIdent = new MyIdent
    assert(ident2.is[Manual])
    assert(ident2.is[Auto1])
    assert(ident2.is[Auto2])
  }
}
