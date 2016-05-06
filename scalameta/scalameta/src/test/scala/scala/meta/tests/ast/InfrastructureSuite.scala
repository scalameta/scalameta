package scala.meta.tests
package ast

import org.scalatest._
import scala.meta._
import scala.meta.internal.ast._
import scala.meta.internal.semantic._
import scala.meta.dialects.{Scala211, QuasiquoteTerm}

class InfrastructureSuite extends FunSuite {
  test("become for Quasi-0") {
    val dialect = QuasiquoteTerm(Scala211, multiline = false)
    val q = dialect("$hello").parse[Term].get.asInstanceOf[Term.Quasi]
    assert(q.become[Type.Quasi].show[Structure] === """Type.Quasi(0, Term.Name("hello"))""")
    assert(q.become[Type.Quasi].pos.toString === """[0..6) in Input.String("$hello")""")
  }

  test("become for Quasi-1") {
    val dialect = QuasiquoteTerm(Scala211, multiline = false)
    val Term.Block(Seq(q: Stat.Quasi)) = dialect("..$hello").parse[Stat].get
    assert(q.become[Type.Quasi].show[Structure] === """Type.Quasi(1, Type.Quasi(0, Term.Name("hello")))""")
    assert(q.become[Type.Quasi].pos.toString === """[0..8) in Input.String("..$hello")""")
  }

  test("copy flags") {
    val x1 = foobar.setTypechecked
    val x2 = x1.copy()
    assert(x1.isTypechecked === true)
    assert(x2.isTypechecked === false)
  }

  test("copy parent") {
    val Term.Select(x1: Term.Name, _) = foobar
    val x2 = x1.copy()
    assert(x1.parent.nonEmpty === true)
    assert(x2.parent.nonEmpty === false)
  }

  test("copy pos") {
    val x1 = "foo".parse[Term].get.asInstanceOf[Term.Name]
    val x2 = x1.copy()
    assert(x1.pos.nonEmpty === true)
    assert(x2.pos.nonEmpty === false)
  }

  test("copy tokens") {
    val x1 = "foo".parse[Term].get.asInstanceOf[Term.Name]
    val x2 = x1.copy()
    assert(x1.tokens.nonEmpty === true)
    assert(x2.tokens.nonEmpty === true)
  }

  test("copy env") {
    // TODO: fill this in when environments are implemented
  }

  test("copy denot") {
    val x1 = foo
    val x2 = x1.copy()
    assert(x1.denot.nonEmpty === true)
    assert(x2.denot.nonEmpty === false)
  }

  test("copy typing") {
    val x1 = foo
    val x2 = x1.copy()
    assert(x1.typing.nonEmpty === true)
    assert(x2.typing.nonEmpty === false)
  }

  private def attributeName(name: Term.Name): Term.Name = name.withAttrs(Denotation.Single(Prefix.None, Symbol.RootPackage), Foo.setTypechecked)
  private def attributeName(name: Type.Name): Type.Name = name.withAttrs(Denotation.Single(Prefix.None, Symbol.RootPackage))
  private def attributeTerm[T <: Term](term: T): T = term.withAttrs(Foo.setTypechecked)
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

  test("TYPECHECKED doesn't reset when origins are touched") {
    val x1 = foo.setTypechecked
    val x2 = x1.withOrigin(Origin.None)
    assert(x1.isTypechecked == true)
    assert(x2.isTypechecked == true)
  }

  // NOTE: This situation is impossible as long as we validate state transitions,
  // i.e. when we require that withAttrs can only be called on unattributed trees.
  //
  // test("TYPECHECKED resets when attributes are touched") {
  //   val x1 = foo.setTypechecked
  //   val x2 = x1.withAttrs(Denotation.None, Typing.None)
  //   assert(x1.isTypechecked == true)
  //   assert(x2.isTypechecked == false)
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
    val x1 = foo.privateCopy(denot = Denotation.None)
    intercept[UnsupportedOperationException] { x1.setTypechecked }
  }

  test("TYPECHECKED crashes when typing is zero") {
    val x1 = foo.privateCopy(typing = Typing.None)
    intercept[UnsupportedOperationException] { x1.setTypechecked }
  }

  test("Typing.Nonrecursive is really lazy") {
    val x1 = Typing.Nonrecursive(??? : Type)
    val x2 = Term.Name("foo").withAttrs(foo.denot, ??? : Type)
    assert(x2.isTypechecked === false)
  }

  private def denot = Denotation.Single(Prefix.None, Symbol.RootPackage)
  private def typing = Foo.setTypechecked
  private def u = Term.Name("u")
  private def pa = u.withAttrs(denot, typing)
  private def a = pa.setTypechecked

  implicit class XtensionStateTree(tree: Tree) {
    private def invoke(name: String): Boolean = {
      val m = tree.getClass.getMethods().find(_.getName == name).get
      m.setAccessible(true)
      m.invoke(tree).asInstanceOf[Boolean]
    }
    def isU = tree.isUnattributed
    def isPA = tree.isPartiallyAttributed
    def isA = tree.isAttributed
  }

  test("states") {
    assert(u.isU === true)
    assert(u.isPA === false)
    assert(u.isA === false)

    assert(pa.isU === false)
    assert(pa.isPA === true)
    assert(pa.isA === false)

    assert(a.isU === false)
    assert(a.isPA === false)
    assert(a.isA === true)
  }

  test("unquoting transitions") {
    val Term.Select(u1, _) = Term.Select(u, Term.Name("bar"))
    assert(u1.isU === true)

    val Term.Select(pa1, _) = Term.Select(pa, Term.Name("bar"))
    assert(pa1.isPA === true)

    val Term.Select(a1, _) = Term.Select(a, Term.Name("bar"))
    assert(a1.isA === true)
  }

  test("copy transitions") {
    val u1 = u.copy()
    assert(u1.isU === true)

    val pa1 = pa.copy()
    assert(pa1.isU == true)

    val a1 = a.copy()
    assert(a1.isU === true)
  }

  test("withAttrs transitions") {
    val u1 = u.withAttrs(denot, typing)
    assert(u1.isPA === true)

    val pa1 = pa.withAttrs(denot, typing)
    assert(pa1.isPA === true)

    intercept[UnsupportedOperationException] { a.withAttrs(denot, typing) }
  }

  test("setTypechecked transitions") {
    intercept[UnsupportedOperationException] { u.setTypechecked }

    val pa1 = pa.setTypechecked
    assert(pa1.isA === true)

    val a1 = a.setTypechecked
    assert(a1.isA === true)
  }
}