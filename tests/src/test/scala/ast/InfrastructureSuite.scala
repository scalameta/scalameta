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
    val x1 = Term.Name("foo").setTypechecked
    val x2 = x1.withTokens(Tokens())
    assert(x1.isTypechecked == true)
    assert(x2.isTypechecked == true)
  }

  test("TYPECHECKED resets when attributes are touched") {
    val x1 = Term.Name("foo").setTypechecked
    val x2 = x1.withDenot(Denotation.Zero)
    val x3 = x1.withTyping(Typing.Zero)
    val x4 = x1.withExpansion(Expansion.Zero)
    assert(x1.isTypechecked == true)
    assert(x2.isTypechecked == false)
    assert(x3.isTypechecked == false)
    assert(x4.isTypechecked == false)
  }

  test("TYPECHECKED resets when the tree is copied") {
    val x1 = Term.Name("foo").setTypechecked
    val x2 = x1.copy(value = "bar")
    assert(x1.isTypechecked == true)
    assert(x2.isTypechecked == false)
  }

  test("TYPECHECKED doesn't reset when the tree is unquoted") {
    val x1 = Term.Name("foo").setTypechecked
    val x2 = Term.Select(x1, Term.Name("bar"))
    assert(x1.isTypechecked == true)
    assert(x2.isTypechecked == false)
    assert(x2.qual.isTypechecked == true)
    assert(x2.name.isTypechecked == false)
  }

  test("TYPECHECKED sets children when set") {
    val x1 = Term.Name("foo")
    val x2 = Term.Name("bar")
    val x3 = Term.Select(x1, x2)
    val y3 @ Term.Select(y1, y2) = x3.setTypechecked
    assert(y1.isTypechecked === true)
    assert(y2.isTypechecked === true)
    assert(y3.isTypechecked === true)
  }

  test("TYPECHECKED resets children when reset") {
    val x1 = Term.Name("foo").setTypechecked
    val x2 = Term.Name("bar").setTypechecked
    val x3 = Term.Select(x1, x2).setTypechecked
    val y3 @ Term.Select(y1, y2) = x3.resetTypechecked
    assert(y1.isTypechecked === false)
    assert(y2.isTypechecked === false)
    assert(y3.isTypechecked === false)
  }

  test("Typing.Nonrecursive is really lazy") {
    val x1 = Typing.Nonrecursive(???)
    val x2 = x1.map(_ => ???)
  }
}
