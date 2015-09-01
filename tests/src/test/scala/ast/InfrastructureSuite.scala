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

  private def attributeName(name: Term.Name): Term.Name = attributeTerm(name.withDenot(Denotation.Single(Prefix.Zero, Symbol.RootPackage))).asInstanceOf[Term.Name]
  private def attributeName(name: Type.Name): Type.Name = name.withDenot(Denotation.Single(Prefix.Zero, Symbol.RootPackage))
  private def attributeTerm(term: Term): Term = term.withTyping(Foo.setTypechecked).withIdentityExpansion
  private def Foo = attributeName(Type.Name("Foo"))
  private def foo = attributeName(Term.Name("foo"))
  private def bar = attributeName(Term.Name("bar"))
  private def foobar = attributeTerm(Term.Select(foo, bar))

  test("TYPECHECKED setters") {
    val x1 = foo
    assert(x1.isTypechecked === false)
    val x2 = x1.setTypechecked
    assert(x2.isTypechecked === true)
    val x3 = x2.resetTypechecked
    assert(x3.isTypechecked === false)
    val x4 = foo.withTypechecked(true)
    assert(x4.isTypechecked === true)
    val x5 = foo.withTypechecked(false)
    assert(x5.isTypechecked === false)
  }

  test("TYPECHECKED doesn't reset when tokens are touched") {
    val x1 = foo.setTypechecked
    val x2 = x1.withTokens(Tokens())
    assert(x1.isTypechecked == true)
    assert(x2.isTypechecked == true)
  }

  test("TYPECHECKED resets when attributes are touched") {
    val x1 = foo.setTypechecked
    val x2 = x1.withDenot(Denotation.Zero)
    val x3 = x1.withTyping(Typing.Zero)
    val x4 = x1.withExpansion(Expansion.Zero)
    assert(x1.isTypechecked == true)
    assert(x2.isTypechecked == false)
    assert(x3.isTypechecked == false)
    assert(x4.isTypechecked == false)
  }

  test("TYPECHECKED resets when the tree is copied") {
    val x1 = foo.setTypechecked
    val x2 = x1.copy(value = "bar")
    assert(x1.isTypechecked == true)
    assert(x2.isTypechecked == false)
  }

  test("TYPECHECKED doesn't reset when the tree is unquoted") {
    val x1 = foo.setTypechecked
    val x2 = Term.Select(x1, Term.Name("bar"))
    assert(x1.isTypechecked == true)
    assert(x2.isTypechecked == false)
    assert(x2.qual.isTypechecked == true)
    assert(x2.name.isTypechecked == false)
  }

  test("TYPECHECKED sets children when set") {
    val x1 = foo
    val x2 = bar
    val x3 = foobar
    val y3 @ Term.Select(y1, y2) = x3.setTypechecked
    assert(y1.isTypechecked === true)
    assert(y2.isTypechecked === true)
    assert(y3.isTypechecked === true)
  }

  test("TYPECHECKED resets children when reset") {
    val x1 = foo.setTypechecked
    val x2 = bar.setTypechecked
    val x3 = foobar.setTypechecked
    val y3 @ Term.Select(y1, y2) = x3.resetTypechecked
    assert(y1.isTypechecked === false)
    assert(y2.isTypechecked === false)
    assert(y3.isTypechecked === false)
  }

  test("TYPECHECKED crashes when denot is zero") {
    val x1 = foo.resetDenot
    intercept[UnsupportedOperationException] { x1.setTypechecked }
  }

  test("TYPECHECKED crashes when typing is zero") {
    val x1 = foo.resetTyping
    intercept[UnsupportedOperationException] { x1.setTypechecked }
  }

  test("TYPECHECKED crashes when expansion is zero") {
    val x1 = foo.resetExpansion
    intercept[UnsupportedOperationException] { x1.setTypechecked }
  }

  test("Typing.Nonrecursive is really lazy") {
    val x1 = Typing.Nonrecursive(???)
    val x2 = x1.map(_ => ???)
  }
}
