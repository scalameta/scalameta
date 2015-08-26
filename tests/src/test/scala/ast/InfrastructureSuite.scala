package scala.meta.tests

import org.scalatest._
import scala.meta.XtensionQuasiquoteTokens
import scala.meta.Structure
import scala.meta.Tokens
import scala.meta.internal.ast._
import scala.meta.internal.flags._
import scala.meta.internal.semantic._
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

  test("TYPECHECKED setters") {
    val x1 = Term.Name("foo")
    assert(x1.isTypechecked === false)
    val x2 = x1.setTypechecked
    assert(x2.isTypechecked === true)
    val x3 = x2.resetTypechecked
    assert(x3.isTypechecked === false)
    val x4 = Term.Name("foo").withTypechecked(true)
    assert(x4.isTypechecked === true)
    val x5 = Term.Name("foo").withTypechecked(false)
    assert(x5.isTypechecked === false)
  }

  test("TYPECHECKED doesn't reset when tokens are touched") {
    val y1 = Term.Name("foo").setTypechecked
    val y2 = y1.withTokens(Tokens())
    assert(y1.isTypechecked == true)
    assert(y2.isTypechecked == true)
  }

  test("TYPECHECKED resets when attributes are touched") {
    val y1 = Term.Name("foo").setTypechecked
    val y2 = y1.withDenot(Denotation.Zero)
    val y3 = y1.withTyping(Typing.Zero)
    val y4 = y1.withExpansion(Expansion.Zero)
    assert(y1.isTypechecked == true)
    assert(y2.isTypechecked == false)
    assert(y3.isTypechecked == false)
    assert(y4.isTypechecked == false)
  }

  test("TYPECHECKED resets when the tree is copied") {
    val z1 = Term.Name("foo").setTypechecked
    val z2 = z1.copy(value = "bar")
    assert(z1.isTypechecked == true)
    assert(z2.isTypechecked == false)
  }

  test("TYPECHECKED doesn't reset when the tree is unquoted") {
    val z1 = Term.Name("foo").setTypechecked
    val z2 = Term.Select(z1, Term.Name("bar"))
    assert(z1.isTypechecked == true)
    assert(z2.isTypechecked == false)
    assert(z2.qual.isTypechecked == true)
    assert(z2.name.isTypechecked == false)
  }
}
