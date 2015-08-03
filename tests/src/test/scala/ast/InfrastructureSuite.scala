package scala.meta.tests

import org.scalatest._
import scala.meta.XtensionQuasiquoteTokens
import scala.meta.Structure
import scala.meta.internal.ast._
import scala.meta.dialects.Scala211

class InfrastructureSuite extends FunSuite {
  test("become for Quasi-0") {
    val q = Term.Quasi(0, "hello").withTokens(toks"Hello")
    assert(q.become[Type.Quasi].show[Structure] === """Type.Quasi(0, "hello")""")
    assert(q.become[Type.Quasi].tokens.toString === "Synthetic(Vector(Hello (0..5)))")
  }

  test("become for Quasi-1") {
    val q = Term.Quasi(1, Term.Quasi(0, "hello").withTokens(toks"HelloInner")).withTokens(toks"HelloOuter")
    assert(q.become[Type.Quasi].show[Structure] === """Type.Quasi(1, Type.Quasi(0, "hello"))""")
    assert(q.become[Type.Quasi].tokens.toString === "Synthetic(Vector(HelloOuter (0..10)))")
  }
}
