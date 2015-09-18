package scala.meta.tests
package ast

import org.scalatest._
import scala.meta.tokens._
import scala.meta.tokenquasiquotes._
import scala.meta.prettyprinters._
import scala.meta.internal.ast._
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

  private def attributeName(name: Term.Name): Term.Name = name.withAttrs(Denotation.Single(Prefix.Zero, Symbol.RootPackage), Foo.setTypechecked)
  private def attributeName(name: Type.Name): Type.Name = name.withAttrs(Denotation.Single(Prefix.Zero, Symbol.RootPackage))
  private def attributeTerm(term: Term): Term = term.withAttrs(Foo.setTypechecked)
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

  // NOTE: This situation is impossible as long as we validate state transitions,
  // i.e. when we require that withAttrs can only be called on unattributed trees
  // and withExpansion can only be called on partially attributed trees.
  //
  // test("TYPECHECKED resets when attributes are touched") {
  //   val x1 = foo.setTypechecked
  //   val x2 = x1.withAttrs(Denotation.Zero, Typing.Zero)
  //   val x3 = x1.withExpansion(Expansion.Zero)
  //   assert(x1.isTypechecked == true)
  //   assert(x2.isTypechecked == false)
  //   assert(x3.isTypechecked == false)
  // }

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
    val x1 = foo.privateCopy(denot = Denotation.Zero)
    intercept[UnsupportedOperationException] { x1.setTypechecked }
  }

  test("TYPECHECKED crashes when typing is zero") {
    val x1 = foo.privateCopy(typing = Typing.Zero)
    intercept[UnsupportedOperationException] { x1.setTypechecked }
  }

  test("TYPECHECKED crashes when expansion is zero") {
    val x1 = foo.privateCopy(expansion = Expansion.Zero)
    intercept[UnsupportedOperationException] { x1.setTypechecked }
  }

  test("Typing.Nonrecursive is really lazy") {
    val x1 = Typing.Nonrecursive(??? : Type)
    val x2 = x1.map(_ => ??? : Type)
    val x3 = Term.Name("foo").withAttrs(foo.denot, ??? : Type)
    assert(x3.isTypechecked === false)
  }

  private def denot = Denotation.Single(Prefix.Zero, Symbol.RootPackage)
  private def typing = Foo.setTypechecked
  private def expansion = pa.setTypechecked
  private def u = Term.Name("u")
  private def pa = u.withAttrs(denot, typing)
  private def fa = pa.withExpansion(expansion)

  implicit class XtensionStateTree(tree: Tree) {
    private def invoke(name: String): Boolean = {
      val m = tree.getClass.getMethods().find(_.getName == name).get
      m.setAccessible(true)
      m.invoke(tree).asInstanceOf[Boolean]
    }
    def isU = invoke("isUnattributed")
    def isPA = invoke("isPartiallyAttributed")
    def isFA = invoke("isFullyAttributed")
  }

  test("states") {
    assert(u.isU === true)
    assert(u.isPA === false)
    assert(u.isFA === false)

    assert(pa.isU === false)
    assert(pa.isPA === true)
    assert(pa.isFA === false)

    assert(fa.isU === false)
    assert(fa.isPA === false)
    assert(fa.isFA === true)
  }

  test("unquoting transitions") {
    val Term.Select(u1, _) = Term.Select(u, Term.Name("bar"))
    assert(u1.isU === true)

    val Term.Select(pa1, _) = Term.Select(pa, Term.Name("bar"))
    assert(pa1.isPA === true)

    val Term.Select(fa1, _) = Term.Select(fa, Term.Name("bar"))
    assert(fa1.isFA === true)
  }

  test("copy transitions") {
    val u1 = u.copy()
    assert(u1.isU === true)

    val pa1 = pa.copy()
    assert(pa1.isU === true)

    val fa1 = fa.copy()
    assert(fa1.isU === true)
  }

  test("withAttrs transitions") {
    val u1 = u.withAttrs(denot, typing)
    assert(u1.isPA === true)

    intercept[UnsupportedOperationException] { pa.withAttrs(denot, typing) }

    intercept[UnsupportedOperationException] { fa.withAttrs(denot, typing) }
  }

  test("withExpansion transitions") {
    intercept[UnsupportedOperationException] { u.withExpansion(expansion) }

    val pa1 = pa.withExpansion(expansion)
    assert(pa1.isFA === true)

    intercept[UnsupportedOperationException] { fa.withExpansion(expansion) }
  }

  test("setTypechecked transitions") {
    intercept[UnsupportedOperationException] { u.setTypechecked }

    val pa1 = pa.setTypechecked
    assert(pa1.isPA === true)

    val fa1 = fa.setTypechecked
    assert(fa1.isFA === true)
  }
}