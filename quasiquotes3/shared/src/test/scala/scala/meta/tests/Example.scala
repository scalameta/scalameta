package scala.meta.tests
package quasiquotes

import munit._
import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.quasiquotes._
import munit.Clue.generate
import scala.meta.trees._

import compat.Platform.EOL

// import scala.meta.quasiquotes.Api._
import meta.prettyprinters.XtensionSyntax

class Example extends TreeSuiteBase {
  test("rank-0 liftables") {
    assertTree(q"foo[${42}]")(Term.ApplyType(Term.Name("foo"), List(Lit.Int(42))))
    assertTree(q"${42}")(Lit.Int(42))
  }

  test("rank-1 liftables") {
    implicit def custom: Lift[List[Int], Term.ArgClause] =
      Lift(lst => Term.ArgClause(lst.map(x => q"$x".asInstanceOf[Term])))
    assertTree(q"foo(..${List(1, 2, 3)})")(
      Term.Apply(Term.Name("foo"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3)))
    )
  }

  test("construction ascriptions") {
    val xs = List(q"x", q"y").asInstanceOf[List[Term]]
    assertEquals(q"foo(..${xs})".syntax, "foo(x, y)")
    val xss = List(List(q"x", q"y")).asInstanceOf[List[List[Term]]]
    assertEquals(q"foo(...${xss})".syntax, "foo(x, y)")
    val rhs = q"x".asInstanceOf[Term]
    assertEquals(q"var foo = ${rhs}".syntax, "var foo = x")
  }

  // test("deconstruction ascriptions") {
  //   val q"foo(${xs: Term}, ${ys: Term})" = q"foo(x, y)"
  //   assertEquals(xs.toString, "List(x, y)")
  //   val q"foo(...${xss: List[List[Term]]})" = q"foo(x, y)"
  //   assertEquals(xss.toString, "List(List(x, y))")
  //   val q"var foo = ${x: Term}" = q"var foo = x"
  //   assertEquals(x.toString, "x")
  // }

  test("2 p\"case x @ y => \"") {
    val x = p"x"
    val y = p"List(1, 2, 3)"
    assertTree(p"case $x @ $y => ")(
      Case(
        Pat.Bind(
          Pat.Var(Term.Name("x")),
          Pat.Extract(Term.Name("List"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3)))
        ),
        None,
        Term.Block(Nil)
      )
    )
  }

  test("2 q\"foo(term, ..terms, term)\"") {
    val term = q"x".asInstanceOf[Term]
    val terms = List(q"y", q"z").asInstanceOf[List[Term]]
    assertTree(q"foo($term, ..$terms, $term)")(
      Term.Apply(
        Term.Name("foo"),
        List(Term.Name("x"), Term.Name("y"), Term.Name("z"), Term.Name("x"))
      )
    )
  }

  test("2 q\"foo(x, ..ys, z, ..ts)\"") {
    val x = q"1".asInstanceOf[Term]
    val ys = List(q"2").asInstanceOf[List[Term]]
    val z = q"3".asInstanceOf[Term]
    val ts = Nil
    assertTree(q"foo($x, ..$ys, $z, ..$ts)")(
      Term.Apply(Term.Name("foo"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3)))
    )
  }

  test("2 q\"name.super[name].id\"") {
    val clazz = q"A"
    val tpe = t"B"
    val id = q"x"
    // inconsistency with the test above planned, since Name can't be constructed directly
    assertTree(q"$clazz.super[$tpe].m")(
      Term.Select(Term.Super(Term.Name("A"), Type.Name("B")), Term.Name("m"))
    )
  }

  test("2 super variants") {
    val clazz = t"C"
    val tpe = t"M"
    assertTree(q"super")(Term.Super(Name(""), Name("")))
    assertTree(q"super[$tpe]")(Term.Super(Name(""), Type.Name("M")))
    assertTree(q"$clazz.super")(Term.Super(Type.Name("C"), Name("")))
    assertTree(q"$clazz.super[$tpe]")(Term.Super(Type.Name("C"), Type.Name("M")))
  }
}
