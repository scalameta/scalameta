package scala.meta

import org.scalatest._
import scala.reflect.runtime.universe._
import scala.meta.internal.ast._
import scala.meta.dialects.Scala211

class QuasiSuite extends FunSuite {
  val XtensionQuasiquoteTerm = "shadow scala.metq quasiquotes"
  test("$x") {
    assert(Term.Quasi(q"x", 0).show[Code] === "${x @ Term}")
  }
  test("..$xs") {
    assert(Term.Quasi(Term.Quasi(q"xs", 0), 1).show[Code] === "..${xs @ Term}")
  }
}