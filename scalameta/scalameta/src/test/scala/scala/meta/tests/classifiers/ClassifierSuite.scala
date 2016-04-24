package org.scalameta.tests
package classifiers

import org.scalatest._
import scala.meta.internal.classifiers.classifier
import scala.meta.classifiers._

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
  implicit def classifier[T <: MyToken]: Classifier[T, Manual] = {
    new Classifier[T, Manual] {
      def apply(x: T): Boolean = Manual.unapply(x)
    }
  }
}

@classifier
trait Auto {
  def unapply(x: MyToken): Boolean = x.isInstanceOf[MyIdent]
}

class ClassifierSuite extends FunSuite {
  test("unclassifiable inheritance") {
    assert(typecheckError("""
      import scala.meta._
      (??? : Unclassifiable).is[Derived]
    """) === """
      |value is is not a member of org.scalameta.tests.classifiers.Unclassifiable
    """.trim.stripMargin)
  }

  test("unclassifiable typeclass") {
    assert(typecheckError("""
      import scala.meta._
      (??? : Unclassifiable).is[Manual]
      (??? : Unclassifiable).is[Auto]
    """) === """
      |value is is not a member of org.scalameta.tests.classifiers.Unclassifiable
    """.trim.stripMargin)
  }

  test("classifiable inheritance") {
    assert(typecheckError("""
      import scala.meta._
      (??? : MyToken).is[MyIdent]
    """) === """
      |don't know how to check whether org.scalameta.tests.classifiers.MyToken is org.scalameta.tests.classifiers.MyIdent
    """.trim.stripMargin)

    assert(typecheckError("""
      import scala.meta._
      (??? : MyIdent).is[MyIdent]
    """) === """
      |don't know how to check whether org.scalameta.tests.classifiers.MyIdent is org.scalameta.tests.classifiers.MyIdent
    """.trim.stripMargin)
  }

  test("classifiable typeclass") {
    val ident1: MyToken = new MyIdent
    assert(ident1.is[Manual])
    assert(ident1.is[Auto])

    val ident2: MyIdent = new MyIdent
    assert(ident2.is[Manual])
    assert(ident2.is[Auto])
  }
}