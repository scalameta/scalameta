package scala.meta.tests
package quasiquotes

import munit._
import org.scalameta.invariants.InvariantFailedException

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.trees.Origin

import compat.Platform.EOL

// FIXME: https://github.com/scalatest/scalatest/issues/1112
// I had to remove $ characters from all test names in this file.
// This is because ScalaTest seems to erroneously consider dollars to be name terminators,
// so it would spuriously crash with "duplicated test" exceptions for e.g.:
// test("1 p\"case $x: T => \"") { ... } and test("1 p\"case $x @ $y => \"") { .. }.

class SuccessSuite extends TreeSuiteBase {
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
    val xs = List(q"x", q"y")
    assertEquals(q"foo(..${xs: List[Term]})".syntax, "foo(x, y)")
    val xss = List(List(q"x", q"y"))
    assertEquals(q"foo(...${xss: List[List[Term]]})".syntax, "foo(x, y)")
    val rhs = q"x"
    assertEquals(q"var foo = ${rhs: Term}".syntax, "var foo = x")
  }

  test("deconstruction ascriptions") {
    val q"foo(..${xs: List[Term]})" = q"foo(x, y)"
    assertEquals(xs.toString, "List(x, y)")
    val q"foo(...${xss: List[List[Term]]})" = q"foo(x, y)"
    assertEquals(xss.toString, "List(List(x, y))")
    val q"var foo = ${x: Term}" = q"var foo = x"
    assertEquals(x.toString, "x")
  }

  test("1 Type.Var or Type.Name") {
    val q"1 match { case _: List[..$tpes] => }" = q"1 match { case _: List[t] => }"
    assertTrees(tpes: _*)(Type.Var(Type.Name("t")))
  }

  test("2 Type.Var or Type.Name") {
    val q"1 match {case x: $tpe =>}" = q"1 match {case x: T =>}"
    assertTree(tpe)(Type.Name("T"))
  }

  test("3 Type.Var or Type.Name") {
    val q"1 match {case x: $tpe =>}" = q"1 match {case x: t =>}"
    assertTree(tpe)(Type.Name("t"))
  }

  test("1 p\"case x: T => \"") {
    val p"case $x: T => " = p"case x: T =>"
    assertTree(x)(Pat.Var(Term.Name("x")))
  }

  test("2 p\"case x: T => \"") {
    val x = p"x"
    assertTree(p"case $x: T => ")(
      Case(Pat.Typed(Pat.Var(Term.Name("x")), Type.Name("T")), None, Term.Block(Nil))
    )
  }

  test("1 p\"case x @ y => \"") {
    val p"case $x @ $y => " = p"case x @ List(1, 2, 3) =>"
    assertTree(x)(Pat.Var(Term.Name("x")))
    assertTree(y)(Pat.Extract(Term.Name("List"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3))))
  }

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

  test("1 q\"foo(term, ..terms, term)\"") {
    val q"foo($term1, ..$terms, $term2)" = q"foo(x, y, z, q)"
    assertTree(term1)(Term.Name("x"))
    assertEquals(terms.toString, "List(y, z)")
    assertTrees(terms: _*)(Term.Name("y"), Term.Name("z"))
    assertTree(term2)(Term.Name("q"))
  }

  test("2 q\"foo(term, ..terms, term)\"") {
    val term = q"x"
    val terms = List(q"y", q"z")
    assertTree(q"foo($term, ..$terms, $term)")(
      Term.Apply(
        Term.Name("foo"),
        List(Term.Name("x"), Term.Name("y"), Term.Name("z"), Term.Name("x"))
      )
    )
  }

  test("case q\"foo({x: Int})\"") {
    q"foo(42)" match {
      case q"$foo(${x: Int})" =>
        assertTree(foo)(Term.Name("foo"))
        assertEquals(x, 42)
    }
  }

  test("case q\"foo({x: Int}, ..ys, z)\"") {
    q"foo(1, 2, 3)" match {
      case q"$_(${x: Int}, ..$y, $z)" =>
        assertEquals(x, 1)
        assertEquals(y.map(_.structure), List("Lit.Int(2)"))
        assertTree(z)(Lit.Int(3))
    }
  }

  test("1 q\"foo(x, ..ys, z)\"") {
    val q"foo($x, ..$ys, $z)" = q"foo(1, 2, 3)"
    assertTree(x)(Lit.Int(1))
    assertEquals(ys.toString, "List(2)")
    assertTrees(ys: _*)(Lit.Int(2))
    assertTree(z)(Lit.Int(3))
  }

  test("2 q\"foo(x, ..ys, z, ..ts)\"") {
    val x = q"1"
    val ys = List(q"2")
    val z = q"3"
    val ts = Nil
    assertTree(q"foo($x, ..$ys, $z, ..$ts)")(
      Term.Apply(Term.Name("foo"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3)))
    )
  }

  test("1 val q\"type name[_] = _\"") {
    val q"type $name[$_] = $_" = q"type List[+A] = List[A]"
    assertTree(name)(Type.Name("List"))
  }

  test("2 val q\"type name[a] = b\"") {
    val q"type $name[$a] = $b" = q"type List[+A] = List[A]"
    assertTree(name)(Type.Name("List"))
    assertTree(a)(
      Type.Param(List(Mod.Covariant()), Type.Name("A"), Nil, Type.Bounds(None, None), Nil, Nil)
    )
    assertTree(b)(Type.Apply(Type.Name("List"), List(Type.Name("A"))))
  }

  test("3 val q\"type name[a] = b\"") {
    val name = t"List"
    val a = tparam"+A"
    val b = t"B"
    assertTree(q"type $name[$a] = $b")(
      Defn.Type(
        Nil,
        Type.Name("List"),
        List(
          Type.Param(List(Mod.Covariant()), Type.Name("A"), Nil, Type.Bounds(None, None), Nil, Nil)
        ),
        Type.Name("B"),
        Type.Bounds(None, None)
      )
    )
  }

  test("1 val q\"def x = {body: Int}\"") {
    val q"def x = ${body: Int}" = q"def x = 42"
    assertEquals(body, 42)
  }

  test("2 val q\"def x = {body: Int}\"") {
    val body = 42
    assertTree(q"def x = ${body: Int}")(Defn.Def(Nil, Term.Name("x"), Nil, Nil, None, Lit.Int(42)))
  }

  test("1 q\"name.this.id\"") {
    val q"$name.this.$x" = q"SuccessSuite.this.x"
    assertTree(name)(Name("SuccessSuite"))
    assertTree(x)(Term.Name("x"))
  }

  test("2 q\"name.this.id\"") {
    val name = q"A"
    val x = q"B"
    // inconsistency with the test above planned, since Name can't be constructed directly
    assertTree(q"$name.this.$x")(Term.Select(Term.This(Term.Name("A")), Term.Name("B")))
  }

  test("1 this variants") {
    val q"this" = q"this"
    val q"$clazz.this" = q"C.this"
    assertTree(clazz)(Name("C"))
  }

  test("2 this variants") {
    val clazz = t"C"
    assertTree(q"this")(Term.This(Name("")))
    assertTree(q"$clazz.this")(Term.This(Type.Name("C")))
  }

  test("1 q\"name.super[name].id\"") {
    val q"$clazz.super[$tpe].$id" = q"A.super[B].x"
    assertTree(clazz)(Name("A"))
    assertTree(tpe)(Name("B"))
    assertTree(id)(Term.Name("x"))
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

  test("1 super variants") {
    val q"super" = q"super"
    val q"super[$tpe1]" = q"super[M]"
    val q"$clazz1.super" = q"C.super"
    val q"$clazz2.super[$tpe2]" = q"C.super[M]"
    assertTree(tpe1)(Name("M"))
    assertTree(tpe2)(Name("M"))
    assertTree(clazz1)(Name("C"))
    assertTree(clazz2)(Name("C"))
  }

  test("2 super variants") {
    val clazz = t"C"
    val tpe = t"M"
    assertTree(q"super")(Term.Super(Name(""), Name("")))
    assertTree(q"super[$tpe]")(Term.Super(Name(""), Type.Name("M")))
    assertTree(q"$clazz.super")(Term.Super(Type.Name("C"), Name("")))
    assertTree(q"$clazz.super[$tpe]")(Term.Super(Type.Name("C"), Type.Name("M")))
  }

  test("1 q\"expr.name\"") {
    val q"$expr.$name" = q"foo.bar"
    assertTree(expr)(Term.Name("foo"))
    assertTree(name)(Term.Name("bar"))
  }

  test("2 q\"expr.name\"") {
    val expr = q"foo"
    val name = q"bar"
    assertTree(q"$expr.$name")(Term.Select(Term.Name("foo"), Term.Name("bar")))
  }

  test("1 q\"expr(name)\"") {
    val q"$expr($name)" = q"foo(bar)"
    assertTree(expr)(Term.Name("foo"))
    assertTree(name)(Term.Name("bar"))
  }

  test("2 q\"expr(name)\"") {
    val expr = q"foo"
    val name = q"bar"
    assertTree(q"$expr($name)")(Term.Apply(Term.Name("foo"), List(Term.Name("bar"))))
  }

  test("1 q\"foo[..tpes]\"") {
    val q"$foo[..$types]" = q"foo[T, U]"
    assertEquals(foo.toString, "foo")
    assertEquals(types.toString, "[T, U]")
    assertTree(types)(Type.ArgClause(List(Type.Name("T"), Type.Name("U"))))
  }

  test("2 q\"foo[..tpes]\"") {
    val foo = q"foo"
    val types = List(t"T", t"U")
    assertTree(q"$foo[..$types]")(
      Term.ApplyType(Term.Name("foo"), List(Type.Name("T"), Type.Name("U")))
    )
  }

  test("4 q\"foo[..tpes]\"") {
    val foo = q"foo"
    val types = List.empty[Type]
    val error =
      """|invariant failed:
         |when verifying targClause.!=(null).&&(targClause.isInstanceOf[scala.meta.internal.trees.Quasi].||(targClause.nonEmpty))
         |found that targClause.isInstanceOf[scala.meta.internal.trees.Quasi] is false
         |and also targClause.nonEmpty is false
         |where targClause = """.stripMargin.replace("\n", EOL)
    interceptMessage[InvariantFailedException](error)(q"$foo[..$types]")
    interceptMessage[InvariantFailedException](error)(q"$foo[..$types]()")
  }

  test("1 q\"expr name[..tpes] (..exprs)\"") {
    val q"$expr $name[..$tpes] (..$exprs)" = q"x method[T, U] (1, b)"
    assertTree(expr)(Term.Name("x"))
    assertTree(name)(Term.Name("method"))
    assertEquals(tpes.toString, "[T, U]")
    assertTree(tpes)(Type.ArgClause(List(Type.Name("T"), Type.Name("U"))))
    assertEquals(exprs.toString, "(1, b)")
    assertTrees(exprs: _*)(Lit.Int(1), Term.Name("b"))
  }

  test("2 q\"expr name[..tpes] (..exprs)\"") {
    val expr = q"x"
    val name = q"method"
    val tpes = List(t"T", t"U")
    val exprs = List(q"1", q"b")
    assertTree(q"$expr $name[..$tpes] (..$exprs)")(
      Term.ApplyInfix(
        Term.Name("x"),
        Term.Name("method"),
        List(Type.Name("T"), Type.Name("U")),
        List(Lit.Int(1), Term.Name("b"))
      )
    )
  }

  test("1 q\"a b c\"") {
    val q"$a $b $c" = q"x y z"
    assertTree(a)(Term.Name("x"))
    assertTree(b)(Term.Name("y"))
    assertTree(c)(Term.Name("z"))
  }

  test("2 q\"a b c\"") {
    val a = q"x"
    val b = q"y"
    val c = q"z"
    assertTree(q"$a $b $c")(
      Term.ApplyInfix(Term.Name("x"), Term.Name("y"), Nil, List(Term.Name("z")))
    )
  }

  test("1 q\"!expr\"") {
    val q"!$x" = q"!foo"
    assertTree(x)(Term.Name("foo"))
  }

  test("2 q\"!expr\"") {
    val x = q"foo"
    assertTree(q"!$x")(Term.ApplyUnary(Term.Name("!"), Term.Name("foo")))
  }

  test("1 q\"~expr\"") {
    val q"~$x" = q"~foo"
    assertTree(x)(Term.Name("foo"))
  }

  test("2 q\"~expr\"") {
    val expr = q"foo"
    assertTree(q"~$expr")(Term.ApplyUnary(Term.Name("~"), Term.Name("foo")))
  }

  test("1 q\"-expr\"") {
    val q"-$x" = q"-foo"
    assertTree(x)(Term.Name("foo"))
  }

  test("2 q\"-expr\"") {
    val x = q"foo"
    assertTree(q"-$x")(Term.ApplyUnary(Term.Name("-"), Term.Name("foo")))
  }

  test("1 q\"+expr\"") {
    val q"+$x" = q"+foo"
    assertTree(x)(Term.Name("foo"))
  }

  test("2 q\"+expr\"") {
    val x = q"foo"
    assertTree(q"+$x")(Term.ApplyUnary(Term.Name("+"), Term.Name("foo")))
  }

  test("1 q\"ref = expr\"") {
    val q"$ref = $expr" = q"a = b"
    assertTree(ref)(Term.Name("a"))
    assertTree(expr)(Term.Name("b"))
  }

  test("2 q\"ref = expr\"") {
    val ref = q"a"
    val expr = q"b"
    assertTree(q"$ref = $expr")(Term.Assign(Term.Name("a"), Term.Name("b")))
  }

  test("1 val q\"x.y = z.w\" = q\"a.b = c.d\"") {
    val q"$x.$y = $z.$w" = q"a.b = c.d"
    assertTree(x)(Term.Name("a"))
    assertTree(y)(Term.Name("b"))
    assertTree(z)(Term.Name("c"))
    assertTree(w)(Term.Name("d"))
  }

  test("2 val q\"x.y = z.w\" = q\"a.b = c.d\"") {
    val x = q"a"
    val y = q"b"
    val z = q"c"
    val w = q"d"
    assertTree(q"$x.$y = $z.$w")(
      Term.Assign(
        Term.Select(Term.Name("a"), Term.Name("b")),
        Term.Select(Term.Name("c"), Term.Name("d"))
      )
    )
  }

  test("q\"1 expr(...exprs) = expr\"") {
    val q"$expr1(...$exprs) = $expr2" = q"foo(a, b) = bar"
    assertTree(expr1)(Term.Name("foo"))
    assertEquals(exprs.map(_.toString), List("(a, b)"))
    assertTrees(exprs: _*)(Term.ArgClause(List(Term.Name("a"), Term.Name("b"))))
    assertTree(expr2)(Term.Name("bar"))
  }

  test("2 q\"expr(...exprs) = expr\"") {
    val expr1 = q"foo"
    val exprs = List(List(q"a", q"b"))
    val expr2 = q"bar"
    assertTree(q"$expr1(...$exprs) = $expr2")(
      Term.Assign(
        Term.Apply(Term.Name("foo"), List(Term.Name("a"), Term.Name("b"))),
        Term.Name("bar")
      )
    )
  }

  test("1 q\"expr(..exprs)(..exprs) = expr\"") {
    val q"$expr1(..$exprs1)(..$exprs2) = $expr2" = q"foo(a, b)(c) = bar"
    assertTree(expr1)(Term.Name("foo"))
    assertEquals(exprs1.toString, "(a, b)")
    assertEquals(exprs2.toString, "(c)")
    assertTrees(exprs1: _*)(Term.Name("a"), Term.Name("b"))
    assertTrees(exprs2: _*)(Term.Name("c"))
    assertTree(expr2)(Term.Name("bar"))
  }

  test("2 q\"expr(..exprs)(..exprs) = expr\"") {
    val expr1 = q"foo"
    val exprs1 = List(q"a", q"b")
    val exprs2 = List(q"c")
    val expr2 = q"bar"
    assertTree(q"$expr1(..$exprs1)(..$exprs2) = $expr2")(
      Term.Assign(
        Term.Apply(
          Term.Apply(Term.Name("foo"), List(Term.Name("a"), Term.Name("b"))),
          List(Term.Name("c"))
        ),
        Term.Name("bar")
      )
    )
  }

  test("1 q\"(x, y: Int)\"") {
    val q"($x, y: Int)" = q"(x: X, y: Int)"
    assertTree(x)(Term.Ascribe(Term.Name("x"), Type.Name("X")))
  }

  test("2 q\"(x, y: Int)\"") {
    val x = q"x: X"
    assertTree(q"($x, y: Int)")(
      Term.Tuple(
        List(
          Term.Ascribe(Term.Name("x"), Type.Name("X")),
          Term.Ascribe(Term.Name("y"), Type.Name("Int"))
        )
      )
    )
  }

  test("1 q\"f(q, y: Y)") {
    val q"f($q, y: Y) = $r" = q"f(x: X, y: Y) = 1"
    assertTree(q)(Term.Ascribe(Term.Name("x"), Type.Name("X")))
    assertTree(r)(Lit.Int(1))
  }

  test("2 q\"f(q, y: Y)") {
    val q = q"x: X"
    val r = q"1"
    assertTree(q"f($q, y: Y) = $r")(
      Term.Assign(
        Term.Apply(
          Term.Name("f"),
          List(
            Term.Ascribe(Term.Name("x"), Type.Name("X")),
            Term.Ascribe(Term.Name("y"), Type.Name("Y"))
          )
        ),
        Lit.Int(1)
      )
    )
  }

  test("1 q\"return expr\"") {
    val q"return $expr" = q"return foo == bar"
    assertTree(expr)(
      Term.ApplyInfix(Term.Name("foo"), Term.Name("=="), Nil, List(Term.Name("bar")))
    )
  }

  test("2 q\"return expr\"") {
    val expr = q"foo == bar"
    assertTree(q"return $expr")(
      Term.Return(Term.ApplyInfix(Term.Name("foo"), Term.Name("=="), Nil, List(Term.Name("bar"))))
    )
  }

  test("1 q\"throw expr\"") {
    val q"throw $expr" = q"throw new RuntimeException"
    assertTree(expr)(Term.New(Init(Type.Name("RuntimeException"), Name(""), emptyArgClause)))
  }

  test("2 q\"throw expr\"") {
    val expr = q"new RuntimeException"
    assertTree(q"throw $expr")(
      Term.Throw(Term.New(Init(Type.Name("RuntimeException"), Name(""), emptyArgClause)))
    )
  }

  test("1 q\"expr: tpe\"") {
    val q"$exp: $tpe" = q"1: Double"
    assertTree(exp)(Lit.Int(1))
    assertTree(tpe)(Type.Name("Double"))
  }

  test("2 q\"expr: tpe\"") {
    val exp = q"1"
    val tpe = t"Double"
    assertTree(q"$exp: $tpe")(Term.Ascribe(Lit.Int(1), Type.Name("Double")))
  }

  test("1 q\"expr: ..@annots\"") {
    val q"$exprr: @q ..@$annotz @$ar" = q"foo: @q @w @e @r"
    assertTree(exprr)(Term.Name("foo"))
    assertEquals(annotz.toString, "List(@w, @e)")
    assertTrees(annotz: _*)(
      Mod.Annot(Init(Type.Name("w"), Name(""), emptyArgClause)),
      Mod.Annot(Init(Type.Name("e"), Name(""), emptyArgClause))
    )
    assertTree(ar)(Mod.Annot(Init(Type.Name("r"), Name(""), Nil)))
  }

  test("2 q\"expr: ..@annots\"") {
    val mods = List(mod"@w", mod"@e")
    assertTree(q"foo: @q ..@$mods @r")(
      Term.Annotate(
        Term.Name("foo"),
        List(
          Mod.Annot(Init(Type.Name("q"), Name(""), emptyArgClause)),
          Mod.Annot(Init(Type.Name("w"), Name(""), emptyArgClause)),
          Mod.Annot(Init(Type.Name("e"), Name(""), emptyArgClause)),
          Mod.Annot(Init(Type.Name("r"), Name(""), emptyArgClause))
        )
      )
    )
  }

  test("q\"(..exprs)\"") {
    val q"(..$terms)" = q"(y, z)"
    assertEquals(terms.toString, "List(y, z)")
    assertTrees(terms: _*)(Term.Name("y"), Term.Name("z"))
  }

  test("2 q\"(..exprs)\"") {
    val terms = List(q"y", q"z")
    assertTree(q"(..$terms)")(Term.Tuple(List(Term.Name("y"), Term.Name("z"))))
  }

  test("1 val q\"(..params)\" = q\"(x: Int, y: String)\"") {
    val q"(..$params)" = q"(x: Int, y: String)"
    assertEquals(params.toString, "List(x: Int, y: String)")
    assertTrees(params: _*)(
      Term.Ascribe(Term.Name("x"), Type.Name("Int")),
      Term.Ascribe(Term.Name("y"), Type.Name("String"))
    )
  }

  test("2 val q\"(..params)\" = q\"(x: Int, y: String)\"") {
    val params = List(q"x: Int", q"y: String")
    assertTree(q"(..$params)")(
      Term.Tuple(
        List(
          Term.Ascribe(Term.Name("x"), Type.Name("Int")),
          Term.Ascribe(Term.Name("y"), Type.Name("String"))
        )
      )
    )
  }

  test("1 q\"{ ..stats }\"") {
    val q"{foo; ..$statz; $astat}" = q"{foo; val a = x; val b = y; val c = z}"
    assertEquals(statz.toString, "List(val a = x, val b = y)")
    assertTrees(statz: _*)(
      Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Term.Name("x")),
      Defn.Val(Nil, List(Pat.Var(Term.Name("b"))), None, Term.Name("y"))
    )
    assertTree(astat)(Defn.Val(Nil, List(Pat.Var(Term.Name("c"))), None, Term.Name("z")))
  }

  test("2 q\"{ ..stats }\"") {
    val stats = List(q"val x = 1", q"val y = 2")
    assertTree(q"{ ..$stats }")(
      Term.Block(
        List(
          Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.Int(1)),
          Defn.Val(Nil, List(Pat.Var(Term.Name("y"))), None, Lit.Int(2))
        )
      )
    )
  }

  test("1 q\"if (expr) expr else expr\"") {
    val q"if ($expr1) $expr2 else $expr3" = q"if (1 > 2) a else b"
    assertTree(expr1)(Term.ApplyInfix(Lit.Int(1), Term.Name(">"), Nil, List(Lit.Int(2))))
    assertTree(expr2)(Term.Name("a"))
    assertTree(expr3)(Term.Name("b"))
  }

  test("2 q\"if (expr) expr else expr\"") {
    val expr1 = q"1 > 2"
    val expr2 = q"a"
    val expr3 = q"b"
    assertTree(q"if ($expr1) $expr2 else $expr3")(
      Term.If(
        Term.ApplyInfix(Lit.Int(1), Term.Name(">"), Nil, List(Lit.Int(2))),
        Term.Name("a"),
        Term.Name("b"),
        Nil
      )
    )
  }

  test("1 q\"expr match { ..case cases }\"") {
    val q"$expr match { case bar => baz; ..case $casez; case q => w}" =
      q"foo match { case bar => baz; case _ => foo ; case q => w }"
    assertTree(expr)(Term.Name("foo"))
    assertWithOriginalSyntax(casez: _*)(
      "case _ => foo"
    )(
      "case _ => foo"
    )
    assertTrees(casez: _*)(Case(Pat.Wildcard(), None, Term.Name("foo")))
  }

  test("2 q\"expr match { ..case cases }\"") {
    val q"$expr match { case bar => baz; ..case $casez; case _ => foo }" =
      q"foo match { case bar => baz; case _ => foo }"
    assertTree(expr)(Term.Name("foo"))
    assert(casez.isEmpty)
  }

  test("3 q\"expr match { ..case cases }\"") {
    val q"$expr match { ..case $casez }" = q"foo match { case bar => baz; case _ => foo }"
    assertTree(expr)(Term.Name("foo"))
    assertWithOriginalSyntax(casez: _*)(
      "case bar => baz",
      "case _ => foo"
    )(
      "case bar => baz",
      "case _ => foo"
    )
    assertTrees(casez: _*)(
      Case(Pat.Var(Term.Name("bar")), None, Term.Name("baz")),
      Case(Pat.Wildcard(), None, Term.Name("foo"))
    )
  }

  test("4 q\"expr match { ..case cases }\"") {
    val expr = q"foo"
    val casez = List(p"case a => b", p"case q => w")
    assertTree(q"$expr match { ..case $casez }")(
      Term.Match(
        Term.Name("foo"),
        List(
          Case(Pat.Var(Term.Name("a")), None, Term.Name("b")),
          Case(Pat.Var(Term.Name("q")), None, Term.Name("w"))
        ),
        Nil
      )
    )
  }

  test("1 q\"try expr catch { ..case cases } finally expropt\"") {
    val q"try $expr catch { case $case1 ..case $cases; case $case2 } finally $expropt" =
      q"try foo catch { case a => b; case _ => bar; case 1 => 2; case q => w} finally baz"
    assertTree(expr)(Term.Name("foo"))
    assertWithOriginalSyntax(cases: _*)(
      "case _ => bar",
      "case 1 => 2"
    )(
      "case _ => bar",
      "case 1 => 2"
    )
    assertTrees(cases: _*)(
      Case(Pat.Wildcard(), None, Term.Name("bar")),
      Case(Lit.Int(1), None, Lit.Int(2))
    )
    assertTree(case1)(Case(Pat.Var(Term.Name("a")), None, Term.Name("b")))
    assertTree(case2)(Case(Pat.Var(Term.Name("q")), None, Term.Name("w")))
    assertTree(expropt)(Some(Term.Name("baz")))
  }

  test("2 q\"try expr catch { ..case cases } finally expropt\"") {
    val expr = q"foo"
    val cases = List(p"case _ => bar", p"case 1 => 2")
    val case1 = p"case a => b"
    val case2 = p"case q => w"
    val expropt = q"baz"
    assertTree(q"try $expr catch { case $case1 ..case $cases; case $case2 } finally $expropt")(
      Term.Try(
        Term.Name("foo"),
        List(
          Case(Pat.Var(Term.Name("a")), None, Term.Name("b")),
          Case(Pat.Wildcard(), None, Term.Name("bar")),
          Case(Lit.Int(1), None, Lit.Int(2)),
          Case(Pat.Var(Term.Name("q")), None, Term.Name("w"))
        ),
        Some(Term.Name("baz"))
      )
    )
  }

  test("0 q\"try expr catch expr finally expropt\"") {
    val q"try $expr catch $exprr finally $expropt" = q"try foo catch pf finally bar"
    assertTree(expr)(Term.Name("foo"))
    assertTree(exprr)(Term.Name("pf"))
    assertTree(expropt)(Some(Term.Name("bar")))
  }

  test("1 q\"try expr catch expr finally expropt\"") {
    val q"try $expr catch $exprr finally $expropt" = q"try { foo } catch { pf } finally { bar }"
    assertTree(expr)(Term.Block(List(Term.Name("foo"))))
    assertTree(exprr)(Term.Block(List(Term.Name("pf"))))
    assertTree(expropt)(Some(Term.Block(List(Term.Name("bar")))))
  }

  test("2 q\"try expr catch expr finally expropt\"") {
    val expr = q"{ foo }"
    val exprr = q"pf"
    val expropt = q"{ bar }"
    assertTree(q"try $expr catch $exprr finally $expropt")(
      Term.TryWithHandler(
        Term.Block(List(Term.Name("foo"))),
        Term.Name("pf"),
        Some(Term.Block(List(Term.Name("bar"))))
      )
    )
  }

  test("q\"(i: Int) => 42\"") {
    assertTree(q"(i: Int) => 42")(
      Term.Function(
        List(Term.Param(Nil, Term.Name("i"), Some(Type.Name("Int")), None)),
        Lit.Int(42)
      )
    )
  }

  test("1 q\"(..params) => expr\"") {
    val q"(..$paramz) => $expr" = q"(x: Int, y: String) => 42"
    assertEquals(paramz.toString, "(x: Int, y: String)")
    assertTrees(paramz: _*)(
      Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None),
      Term.Param(Nil, Term.Name("y"), Some(Type.Name("String")), None)
    )
    assertTree(expr)(Lit.Int(42))
  }

  test("2 q\"(..params) => expr\"") {
    val paramz = List(param"x: Int", param"y: String")
    val expr = q"42"
    assertTree(q"(..$paramz) => $expr")(
      Term.Function(
        List(
          Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None),
          Term.Param(Nil, Term.Name("y"), Some(Type.Name("String")), None)
        ),
        Lit.Int(42)
      )
    )
  }

  test("1 val q\"(..q, y: Y, e) => r\" = q\"(x: X, y: Y, z: Z) => 1\"") {
    val q"(..$q, y: Y, $e) => $r" = q"(x: X, y: Y, z: Z) => 1"
    assertEquals(q.toString, "List(x: X)")
    assertTrees(q: _*)(Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None))
    assertTree(e)(Term.Param(Nil, Term.Name("z"), Some(Type.Name("Z")), None))
    assertTree(r)(Lit.Int(1))
  }

  test("2 val q\"(..q, y: Y, e) => r\" = q\"(x: X, y: Y, z: Z) => 1\"") {
    val q = List(param"x: X")
    val e = param"z: Z"
    val r = q"1"
    assertTree(q"(..$q, y: Y, $e) => $r")(
      Term.Function(
        List(
          Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
          Term.Param(Nil, Term.Name("y"), Some(Type.Name("Y")), None),
          Term.Param(Nil, Term.Name("z"), Some(Type.Name("Z")), None)
        ),
        Lit.Int(1)
      )
    )
  }

  test("1 q\"{ ..case cases }\"") {
    val q"{ ..case $cases }" = q"{ case i: Int => i + 1 }"
    assertTrees(cases: _*)(
      Case(
        Pat.Typed(Pat.Var(Term.Name("i")), Type.Name("Int")),
        None,
        Term.ApplyInfix(Term.Name("i"), Term.Name("+"), Nil, List(Lit.Int(1)))
      )
    )
  }

  test("2 q\"{ ..case cases }\"") {
    val cases = List(p"case i: Int => i + 1")
    assertTree(q"{ ..case $cases }")(
      Term.PartialFunction(
        List(
          Case(
            Pat.Typed(Pat.Var(Term.Name("i")), Type.Name("Int")),
            None,
            Term.ApplyInfix(Term.Name("i"), Term.Name("+"), Nil, List(Lit.Int(1)))
          )
        )
      )
    )
  }

  test("1 q\"while (expr) expr\"") {
    val q"while ($expr1) $expr2" = q"while (foo) bar"
    assertTree(expr1)(Term.Name("foo"))
    assertTree(expr2)(Term.Name("bar"))
  }

  test("2 q\"while (expr) expr\"") {
    val expr1 = q"foo"
    val expr2 = q"bar"
    assertTree(q"while ($expr1) $expr2")(Term.While(Term.Name("foo"), Term.Name("bar")))
  }

  test("1 q\"do expr while(expr)\"") {
    val q"do $expr1 while($expr2)" = q"do foo while (bar)"
    assertTree(expr1)(Term.Name("foo"))
    assertTree(expr2)(Term.Name("bar"))
  }

  test("2 q\"do expr while(expr)\"") {
    val expr1 = q"foo"
    val expr2 = q"bar"
    assertTree(q"do $expr1 while($expr2)")(Term.Do(Term.Name("foo"), Term.Name("bar")))
  }

  test("1 q\"for (..enumerators) expr\"") {
    val q"for ($enum1; ..$enumerators; if $cond; $enum2) $exprr" =
      q"for (a <- as; x <- xs; y <- ys; if bar; b <- bs) foo(x, y)"
    assertEquals(enumerators.toString, "List(x <- xs, y <- ys)")
    assertTrees(enumerators: _*)(
      Enumerator.Generator(Pat.Var(Term.Name("x")), Term.Name("xs")),
      Enumerator.Generator(Pat.Var(Term.Name("y")), Term.Name("ys"))
    )
    assertTree(cond)(Term.Name("bar"))
    assertTree(enum1)(Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("as")))
    assertTree(enum2)(Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("bs")))
    assertTree(exprr)(Term.Apply(Term.Name("foo"), List(Term.Name("x"), Term.Name("y"))))
  }

  test("2 q\"for (..enumerators) expr\"") {
    val a = enumerator"a <- as"
    val b = enumerator"b <- bs"
    val ab = List(a, b)
    assertTree(q"for (..$ab) foo")(
      Term.For(
        List(
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("as")),
          Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("bs"))
        ),
        Term.Name("foo")
      )
    )
  }

  test("1 q\"for (..enumerators) yield expr\"") {
    val q"for (a <- as; ..$enumerators; b <- bs) yield $expr" =
      q"for (a <- as; x <- xs; y <- ys; b <- bs) yield foo(x, y)"
    assertEquals(enumerators.toString, "List(x <- xs, y <- ys)")
    assertTrees(enumerators: _*)(
      Enumerator.Generator(Pat.Var(Term.Name("x")), Term.Name("xs")),
      Enumerator.Generator(Pat.Var(Term.Name("y")), Term.Name("ys"))
    )
    assertTree(expr)(Term.Apply(Term.Name("foo"), List(Term.Name("x"), Term.Name("y"))))
  }

  test("2 q\"for (..enumerators) yield expr\"") {
    val a = enumerator"a <- as"
    val b = enumerator"b <- bs"
    val ab = List(a, b)
    assertTree(q"for (..$ab) yield foo")(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("as")),
          Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("bs"))
        ),
        Term.Name("foo")
      )
    )
  }

  test("1 q\"new { ..stat } with ..inits { self => ..stats }\"") {
    val q"new $x" = q"new Foo"
    assertTree(x)(Init(Type.Name("Foo"), Name(""), emptyArgClause))
  }

  test("2 q\"new { ..stat } with ..inits { self => ..stats }\"") {
    val q"new {..$stats; val b = 4} with $a {$self => ..$statz}" =
      q"new {val a = 2; val b = 4} with A { self => val b = 3 }"
    assertEquals(stats.toString, "List(val a = 2)")
    assertTrees(stats: _*)(Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Lit.Int(2)))
    assertTree(a)(Init(Type.Name("A"), Name(""), emptyArgClause))
    assertTree(self)(Self(Term.Name("self"), None))
    assertEquals(statz.toString, "List(val b = 3)")
    assertTrees(statz: _*)(Defn.Val(Nil, List(Pat.Var(Term.Name("b"))), None, Lit.Int(3)))
  }

  test("3 q\"new { ..stat } with ..inits { self => ..stats }\"") {
    val q"new X with T { $self => def m = 42}" = q"new X with T { def m = 42 }"
    assertTree(self)(Self(Name(""), None))
  }

  test("4 q\"new { ..stat } with ..inits { self => ..stats }\"") {
    val stats = List(q"val a = 2")
    val a = init"A"
    val self = self"self: A"
    val statz = List(q"val b = 3")
    assertTree(q"new {..$stats; val b = 4} with $a {$self => ..$statz}")(
      Term.NewAnonymous(
        Template(
          List(
            Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Lit.Int(2)),
            Defn.Val(Nil, List(Pat.Var(Term.Name("b"))), None, Lit.Int(4))
          ),
          List(Init(Type.Name("A"), Name(""), emptyArgClause)),
          Self(Term.Name("self"), Some(Type.Name("A"))),
          List(Defn.Val(Nil, List(Pat.Var(Term.Name("b"))), None, Lit.Int(3))),
          Nil
        )
      )
    )
  }

  test("q\"_\"") {
    assertTree(q"_")(Term.Placeholder())
  }

  test("1 q\"expr _\"") {
    val q"$expr _" = q"foo _"
    assertTree(expr)(Term.Name("foo"))
  }

  test("2 q\"expr _\"") {
    val expr = q"foo"
    assertTree(q"$expr _")(Term.Eta(Term.Name("foo")))
  }

  test("1 q\"expr: _*\"") {
    val q"$expr: _*" = q"foo: _*"
    assertTree(expr)(Term.Name("foo"))
  }

  test("2 arg\"expr: _*\"") {
    val expr = q"foo"
    assertTree(q"$expr: _*")(Term.Repeated(Term.Name("foo")))
  }

  test("1 q\"lit\"") {
    val q"$x" = q"42"
    assertTree(x)(Lit.Int(42))
  }

  test("2 q\"lit\"") {
    val lit = q"42"
    assertTree(q"$lit")(Lit.Int(42))
  }

  test("1 t\"ref.tname\"") {
    val t"$ref.$tname" = t"X.Y"
    assertTree(ref)(Term.Name("X"))
    assertTree(tname)(Type.Name("Y"))
  }

  test("2 t\"ref.tname\"") {
    val ref = q"X"
    val tname = t"Y"
    assertTree(t"$ref.$tname")(Type.Select(Term.Name("X"), Type.Name("Y")))
  }

  test("1 t\"tpe#tname\"") {
    val t"$tpe#$tname" = t"X#Y"
    assertTree(tpe)(Type.Name("X"))
    assertTree(tname)(Type.Name("Y"))
  }

  test("2 t\"tpe#tname\"") {
    val tpe = t"X"
    val tname = t"Y"
    assertTree(t"$tpe#$tname")(Type.Project(Type.Name("X"), Type.Name("Y")))
  }

  test("1 t\"ref.type\"") {
    val t"$ref.type" = t"X.type"
    assertTree(ref)(Term.Name("X"))
  }

  test("2 t\"ref.type\"") {
    val ref = q"X"
    assertTree(t"$ref.type")(Type.Singleton(Term.Name("X")))
  }
  /*
   Issue #462
   */
  test("3 t\"ref.type\"") {
    val ref = q"X.a"
    assertTree(t"$ref.type")(Type.Singleton(Term.Select(Term.Name("X"), Term.Name("a"))))
  }

  test("1 t\"tpe[..tpes]") {
    val t"$tpe[..$tpes]" = t"X[Y, Z]"
    assertTree(tpe)(Type.Name("X"))
    assertEquals(tpes.toString, "[Y, Z]")
    assertTree(tpes)(Type.ArgClause(List(Type.Name("Y"), Type.Name("Z"))))
  }

  test("2 t\"tpe[..tpes]") {
    val tpe = t"X"
    val tpes = List(t"Y", t"Z")
    assertTree(t"$tpe[..$tpes]")(Type.Apply(Type.Name("X"), List(Type.Name("Y"), Type.Name("Z"))))
  }

  test("1 t\"tpe tname tpe\"") {
    val t"$tpe1 $tname $tpe2" = t"X Y Z"
    assertTree(tpe1)(Type.Name("X"))
    assertTree(tname)(Type.Name("Y"))
    assertTree(tpe2)(Type.Name("Z"))
  }

  test("2 t\"tpe tname tpe\"") {
    val tpe1 = t"X"
    val tname = t"Y"
    val tpe2 = t"Z"
    assertTree(t"$tpe1 $tname $tpe2")(
      Type.ApplyInfix(Type.Name("X"), Type.Name("Y"), Type.Name("Z"))
    )
  }

  test("1 t\"(..tpes) => tpe\"") {
    val t"(..$tpes) => $tpe" = t"(X, Y) => Z"
    assertEquals(tpes.toString, "(X, Y)")
    assertTree(tpes)(Type.FuncParamClause(List(Type.Name("X"), Type.Name("Y"))))
    assertTree(tpe)(Type.Name("Z"))
  }

  test("2 t\"(..tpes) => tpe\"") {
    val tpes: List[Type] = List(t"X", t"Y")
    val tpe = t"Z"
    assertTree(t"(..$tpes) => $tpe")(
      Type.Function(List(Type.Name("X"), Type.Name("Y")), Type.Name("Z"))
    )
  }

  test("1 t\"(..tpes)\"") {
    val t"(..$tpes)" = t"(X, Y)"
    assertEquals(tpes.toString, "List(X, Y)")
    assertTrees(tpes: _*)(Type.Name("X"), Type.Name("Y"))
  }

  test("t\"(..tpes)\"") {
    val tpes = List(t"X", t"Y")
    assertTree(t"(..$tpes)")(Type.Tuple(List(Type.Name("X"), Type.Name("Y"))))
  }

  test("1 t\"tpe { ..stats }\"") {
    val t"$tpe {..$stats}" = t"A with B with C { val a: A; val b: B }"
    assertEquals(tpe.toString, "Some(A with B with C)")
    assertTree(tpe)(Some(Type.With(Type.With(Type.Name("A"), Type.Name("B")), Type.Name("C"))))
    assertEquals(stats.toString, "List(val a: A, val b: B)")
    assertTrees(stats: _*)(
      Decl.Val(Nil, List(Pat.Var(Term.Name("a"))), Type.Name("A")),
      Decl.Val(Nil, List(Pat.Var(Term.Name("b"))), Type.Name("B"))
    )
  }

  test("2 t\"tpe { ..stats }\"") {
    val tpe = t"X with Y"
    val stats = List(q"val a: A", q"val b: B")
    assertTree(t"$tpe { ..$stats }")(
      Type.Refine(
        Some(Type.With(Type.Name("X"), Type.Name("Y"))),
        List(
          Decl.Val(Nil, List(Pat.Var(Term.Name("a"))), Type.Name("A")),
          Decl.Val(Nil, List(Pat.Var(Term.Name("b"))), Type.Name("B"))
        )
      )
    )
  }

  test("1 t\"tpe forSome { ..stats }\"") {
    val t"$tpe forSome { ..$stats }" = t"X forSome { val a: A; val b: B }"
    assertTree(tpe)(Type.Name("X"))
    assertEquals(stats.toString, "List(val a: A, val b: B)")
    assertTrees(stats: _*)(
      Decl.Val(Nil, List(Pat.Var(Term.Name("a"))), Type.Name("A")),
      Decl.Val(Nil, List(Pat.Var(Term.Name("b"))), Type.Name("B"))
    )
  }

  test("2 t\"tpe forSome { ..stats }\"") {
    val tpe = t"X"
    val stats = List(q"val a:A", q"val b:B")
    assertTree(t"$tpe forSome { ..$stats }")(
      Type.Existential(
        Type.Name("X"),
        List(
          Decl.Val(Nil, List(Pat.Var(Term.Name("a"))), Type.Name("A")),
          Decl.Val(Nil, List(Pat.Var(Term.Name("b"))), Type.Name("B"))
        )
      )
    )
  }

  test("1 t\"tpe ..@annots\"") {
    val t"$tpe ..@$annots" = t"X @a @b"
    assertTree(tpe)(Type.Name("X"))
    assertEquals(annots.toString, "List(@a, @b)")
    assertTrees(annots: _*)(
      Mod.Annot(Init(Type.Name("a"), Name(""), emptyArgClause)),
      Mod.Annot(Init(Type.Name("b"), Name(""), emptyArgClause))
    )
  }

  test("2 t\"tpe ..@annots\"") {
    val tpe = t"X"
    val annots = List(mod"@a", mod"@b")
    assertTree(t"$tpe ..@$annots")(
      Type.Annotate(
        Type.Name("X"),
        List(
          Mod.Annot(Init(Type.Name("a"), Name(""), emptyArgClause)),
          Mod.Annot(Init(Type.Name("b"), Name(""), emptyArgClause))
        )
      )
    )
  }

  test("1 t\"[..tparams] =>> tpe\"") {
    val t"[..$tparams] =>> $tpe" = t"[T] =>> (T, T)"
    assertEquals(tparams.toString, "[T]")
    assertTree(tparams)(Type.ParamClause {
      Type.Param(
        Nil,
        Type.Name("T"),
        Type.ParamClause(Nil),
        Type.Bounds(None, None),
        Nil,
        Nil
      ) :: Nil
    })
    assertEquals(tpe.toString, "(T, T)")
  }

  test("2 t\"(..tparams) =>> tpe\"") {
    val tparams = List(tparam"T")
    val tpe = t"(T, T)"
    assertTree(t"[..$tparams] =>> $tpe")(
      Type.Lambda(
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Type.Tuple(List(Type.Name("T"), Type.Name("T")))
      )
    )
  }

  test("1 t\"_ >: tpeopt <: tpeopt\"") {
    val t"_ >: $tpe1 <: $tpe2" = t"_ >: X <: Y"
    assertTree(tpe1)(Some(Type.Name("X")))
    assertTree(tpe2)(Some(Type.Name("Y")))
  }

  test("2 t\"_ >: tpeopt <: tpeopt\"") {
    val tpe1 = t"X"
    val tpe2 = t"Y"
    assertTree(t"_ >: $tpe1 <: $tpe2")(
      Type.Wildcard(Type.Bounds(Some(Type.Name("X")), Some(Type.Name("Y"))))
    )
  }

  test("1 t\"=> tpe\"") {
    val t"=> $tpe" = t"=> X"
    assertTree(tpe)(Type.Name("X"))
  }

  test("2 t\"=> tpe\"") {
    val tpe = t"X"
    assertTree(t"=> $tpe")(Type.ByName(Type.Name("X")))
  }

  test("1 t\"tpe *\"") {
    val t"$tpe*" = t"X*"
    assertTree(tpe)(Type.Name("X"))
  }

  test("2 t\"tpe *\"") {
    val tpe = t"X"
    assertTree(t"$tpe*")(Type.Repeated(Type.Name("X")))
  }

  test("t\"lit\"") {
    val lit = q"1"
    assertTree(t"$lit")(Lit.Int(1))
  }

  test("p\"_\"") {
    assertTree(p"_")(Pat.Wildcard())
  }

  test("p\"name\"") {
    assertTree(p"name")(Pat.Var(Term.Name("name")))
  }

  test("p\"x\"") {
    assertTree(p"x")(Pat.Var(Term.Name("x")))
  }

  test("p\"X\"") {
    assertTree(p"X")(Pat.Var(Term.Name("X")))
  }

  test("p\"`x`\"") {
    assertTree(p"`x`")(Term.Name("x"))
  }

  test("p\"`X`\"") {
    assertTree(p"`X`")(Term.Name("X"))
  }

  test("1 p\"pat @ pat\"") {
    val p"$pat1 @ $pat2" = p"x @ y"
    assertTree(pat1)(Pat.Var(Term.Name("x")))
    assertTree(pat2)(Pat.Var(Term.Name("y")))
  }

  test("2 p\"pat1 @ pat\"") {
    val pat1 = p"x"
    val pat2 = p"y"
    assertTree(p"$pat1 @ $pat2")(Pat.Bind(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))))
  }

  test("1 p\"pat | pat\"") {
    val p"$pat1 | $pat2" = p"x | y"
    assertTree(pat1)(Pat.Var(Term.Name("x")))
    assertTree(pat2)(Pat.Var(Term.Name("y")))
  }

  test("2 p\"pat | pat\"") {
    val pat1 = q"X"
    val pat2 = q"Y"
    assertTree(p"$pat1 | $pat2")(Pat.Alternative(Term.Name("X"), Term.Name("Y")))
  }

  test("3 p\"pat | pat\"") {
    val pat1 = p"`X`"
    val pat2 = q"Y"
    assertTree(p"$pat1 | $pat2")(Pat.Alternative(Term.Name("X"), Term.Name("Y")))
  }

  test("1 p\"(..pats)\"") {
    val p"(..$pats)" = p"(X, Y)"
    assertEquals(pats.toString, "List(X, Y)")
    assertTrees(pats: _*)(Term.Name("X"), Term.Name("Y"))
  }

  test("2 p\"(..pats)\"") {
    val pats = List(p"x", p"y")
    assertTree(p"(..$pats)")(Pat.Tuple(List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y")))))
  }

  test("3 p\"(..pats)\"") {
    val pats = List(p"`X`", q"Y")
    assertTree(p"(..$pats)")(Pat.Tuple(List(Term.Name("X"), Term.Name("Y"))))
  }

  test("1 p\"expr(..pats)\"") {
    val p"$expr(..$pats)" = p"x[A, B](Q, W)"
    assertTree(expr)(Term.ApplyType(Term.Name("x"), List(Type.Name("A"), Type.Name("B"))))
    assertEquals(pats.toString, "(Q, W)")
    assertTrees(pats: _*)(Term.Name("Q"), Term.Name("W"))
  }

  test("2 p\"expr(..pats)\"") {
    val p"$expr(..$pats)" = p"x(Q, W)"
    assertTree(expr)(Term.Name("x"))
    assertEquals(pats.toString, "(Q, W)")
    assertTrees(pats: _*)(Term.Name("Q"), Term.Name("W"))
  }

  test("3 p\"expr(..pats)\"") {
    val ref = q"x"
    val tpes = List(t"A", t"B")
    val pats = List(q"Q", q"W")
    assertTree(p"$ref[..$tpes](..$pats)")(
      Pat.Extract(
        Term.ApplyType(Term.Name("x"), List(Type.Name("A"), Type.Name("B"))),
        List(Term.Name("Q"), Term.Name("W"))
      )
    )
  }

  test("4 p\"expr(..pats)\"") {
    val ref = q"`x`"
    val tpes = List(t"`A`", t"B")
    val pats = List(p"`Q`", q"W")
    assertTree(p"$ref[..$tpes](..$pats)")(
      Pat.Extract(
        Term.ApplyType(Term.Name("x"), List(Type.Name("A"), Type.Name("B"))),
        List(Term.Name("Q"), Term.Name("W"))
      )
    )
  }

  /*
   Issue #462
   */
  test("5 p\"expr(..pats)\"") {
    val ref = q"x.a"
    val tpes = List(t"A", t"B")
    val pats = List(q"Q", q"W")
    assertTree(p"$ref[..$tpes](..$pats)")(
      Pat.Extract(
        Term.ApplyType(
          Term.Select(Term.Name("x"), Term.Name("a")),
          List(Type.Name("A"), Type.Name("B"))
        ),
        List(Term.Name("Q"), Term.Name("W"))
      )
    )
  }

  test("1 p\"pat name (..pats)\"") {
    val p"$pat $name (..$pats)" = p"x y (Q, W)"
    assertTree(pat)(Pat.Var(Term.Name("x")))
    assertTree(name)(Term.Name("y"))
    assertEquals(pats.toString, "(Q, W)")
    assertTrees(pats: _*)(Term.Name("Q"), Term.Name("W"))
  }

  test("2 p\"pat name (..pats)\"") {
    val pat = p"x"
    val name = q"y"
    val pats = List(q"Q", q"W")
    assertTree(p"$pat $name (..$pats)")(
      Pat.ExtractInfix(
        Pat.Var(Term.Name("x")),
        Term.Name("y"),
        List(Term.Name("Q"), Term.Name("W"))
      )
    )
  }

  test("3 p\"pat name (..pats)\"") {
    val pat = p"`x`"
    val name = q"y"
    val pats = List(q"Q", q"W")
    assertTree(p"$pat $name (..$pats)")(
      Pat.ExtractInfix(Term.Name("x"), Term.Name("y"), List(Term.Name("Q"), Term.Name("W")))
    )
  }

  test("1 p\"pat: ptpe\"") {
    val p"$pat: $ptpe" = p"x: Y"
    assertTree(pat)(Pat.Var(Term.Name("x")))
    assertTree(ptpe)(Type.Name("Y"))
  }

  test("2 p\"pat: ptpe\"") {
    val pat = p"x"
    val ptpe = t"Y"
    assertTree(p"$pat: $ptpe")(Pat.Typed(Pat.Var(Term.Name("x")), Type.Name("Y")))
  }

  test("1 p\"expr.name\"") {
    val p"$expr.$name" = p"x.y"
    assertTree(expr)(Term.Name("x"))
    assertTree(name)(Term.Name("y"))
  }

  test("2 p\"expr.name\"") {
    val expr = q"x"
    val name = q"y"
    assertTree(p"$expr.$name")(Term.Select(Term.Name("x"), Term.Name("y")))
  }

  test("3 p\"expr.name\"") {
    val expr = q"`x`"
    val name = q"y"
    assertTree(p"$expr.$name")(Term.Select(Term.Name("x"), Term.Name("y")))
  }

  test("p\"lit\"") {
    val lit = q"1"
    assertTree(p"$lit")(Lit.Int(1))
  }

  test("1 p\"case pat if expropt => expr\"") {
    val p"case $pat if $expropt => $expr" = p"case X if foo => bar"
    assertTree(pat)(Term.Name("X"))
    assertTree(expropt)(Some(Term.Name("foo")))
    assertTree(expr)(Term.Name("bar"))
  }

  test("2 p\"case pat if expropt => expr\"") {
    val pat = q"X"
    val expropt = q"foo"
    val expr = q"bar"
    assertTree(p"case $pat if $expropt => $expr")(
      Case(Term.Name("X"), Some(Term.Name("foo")), Term.Name("bar"))
    )
  }

  test("3 p\"case pat if expropt => expr\"") {
    val pat = p"`X`"
    val expropt = q"`foo`"
    val expr = q"`bar`"
    assertTree(p"case $pat if $expropt => $expr")(
      Case(Term.Name("X"), Some(Term.Name("foo")), Term.Name("bar"))
    )
  }

  test("1 p\"_*\"") {
    assertTree(p"case List(_*) =>")(
      Case(Pat.Extract(Term.Name("List"), List(Pat.SeqWildcard())), None, Term.Block(Nil))
    )
  }

  test("2 p\"_*\"") {
    assertTree(p"_*")(Pat.SeqWildcard())
  }

  test("1 p\"pat\"") {
    val pat = p"X"
    assertTree(p"$pat")(Pat.Var(Term.Name("X")))
  }

  test("2 p\"pat\"") {
    val pat = p"`X`"
    assertTree(p"$pat")(Term.Name("X"))
  }

  test("1 q\"..mods val ..pats: tpe\"") {
    val q"..$mods val ..$pats: $tpe" = q"private final val x, y: T"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertEquals(pats.toString, "List(x, y)")
    assertTrees(pats: _*)(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y")))
    assertTree(tpe)(Type.Name("T"))
  }

  test("2 q\"..mods val ..pats: tpe\"") {
    val mods = List(mod"private", mod"final")
    val pats = List(p"x", p"y")
    val tpe = t"T"
    assertTree(q"..$mods val ..$pats: $tpe")(
      Decl.Val(
        List(Mod.Private(Name("")), Mod.Final()),
        List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))),
        Type.Name("T")
      )
    )
  }

  test("1 q\"..mods var ..pats: tpe\"") {
    val q"..$mods var ..$pats: $tpe" = q"private final var x, y: T"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertEquals(pats.toString, "List(x, y)")
    assertTrees(pats: _*)(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y")))
    assertTree(tpe)(Type.Name("T"))
  }

  test("2 q\"..mods var ..pats: tpe\"") {
    val mods = List(mod"private", mod"final")
    val pats = List(p"x", p"y")
    val tpe = t"T"
    assertTree(q"..$mods var ..$pats: $tpe")(
      Decl.Var(
        List(Mod.Private(Name("")), Mod.Final()),
        List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))),
        Type.Name("T")
      )
    )
  }

  test("1 q\"..mods def name[..tparams](...paramss): tpe\"") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpe" =
      q"private final def m[T, W](x: X, y: Y): R"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertTree(name)(Term.Name("m"))
    checkTree(tparams, "[T, W]")(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("T"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("W"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    assertEquals(paramss.lengthCompare(1), 0)
    checkTreesWithSyntax(paramss: _*)("(x: X, y: Y)")(Term.ParamClause {
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Y")), None)
      )
    })
    assertTree(tpe)(Type.Name("R"))
  }

  test("2 q\"..mods def name[..tparams](...paramss): tpe\"") {
    val mods = List(mod"private", mod"final")
    val name = q"m"
    val tparams = List(tparam"T", tparam"W")
    val paramss = List(List(param"x: X", param"x: Y"))
    val tpe = t"R"
    assertTree(q"..$mods def $name[..$tparams](...$paramss): $tpe")(
      Decl.Def(
        List(Mod.Private(Name("")), Mod.Final()),
        Term.Name("m"),
        List(
          Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil),
          Type.Param(Nil, Type.Name("W"), Nil, Type.Bounds(None, None), Nil, Nil)
        ),
        List(
          List(
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("Y")), None)
          )
        ),
        Type.Name("R")
      )
    )
  }

  test("1 q\"..mods type tname[..tparams] >: tpeopt <: tpeopt\"") {
    val q"..$mods type $tname[..$tparams] >: $tpeopt1 <: $tpeopt2" =
      q"private final type T[T, W] >: A <: B"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertTree(tname)(Type.Name("T"))
    assertEquals(tparams.toString, "[T, W]")
    assertTree(tparams)(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("T"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("W"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    assertTree(tpeopt1)(Some(Type.Name("A")))
    assertTree(tpeopt2)(Some(Type.Name("B")))
  }

  test("2 q\"..mods type tname[..tparams] >: tpeopt <: tpeopt\"") {
    val mods = List(mod"private", mod"final")
    val tname = t"T"
    val tparams = List(tparam"T", tparam"W")
    val tpeopt1 = t"A"
    val tpeopt2 = t"A"
    assertTree(q"..$mods type $tname[..$tparams] >: $tpeopt1 <: $tpeopt2")(
      Decl.Type(
        List(Mod.Private(Name("")), Mod.Final()),
        Type.Name("T"),
        List(
          Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil),
          Type.Param(Nil, Type.Name("W"), Nil, Type.Bounds(None, None), Nil, Nil)
        ),
        Type.Bounds(Some(Type.Name("A")), Some(Type.Name("A")))
      )
    )
  }

  test("1 q\"..mods val ..pats: tpeopt = expr\"") {
    val q"..$mods val ..$pats: $tpeopt = $expr" = q"private final val x, y: T = t"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertEquals(pats.toString, "List(x, y)")
    assertTrees(pats: _*)(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y")))
    assertTree(tpeopt)(Some(Type.Name("T")))
    assertTree(expr)(Term.Name("t"))
  }

  test("2 q\"..mods val ..pats: tpeopt = expr\"") {
    val mods = List(mod"private", mod"final")
    val pats = List(p"x", p"y")
    val tpeopt = t"T"
    val expr = q"t"
    assertTree(q"..$mods val ..$pats: $tpeopt = $expr")(
      Defn.Val(
        List(Mod.Private(Name("")), Mod.Final()),
        List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))),
        Some(Type.Name("T")),
        Term.Name("t")
      )
    )
  }

  test("1 q\"..mods var ..pats: tpeopt = expropt\"") {
    val q"..$mods var ..$pats: $tpeopt = $expr" = q"private final var x, y: T = t"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertEquals(pats.toString, "List(x, y)")
    assertTrees(pats: _*)(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y")))
    assertTree(tpeopt)(Some(Type.Name("T")))
    assertTree(expr)(Term.Name("t"))
  }

  test("2 q\"..mods var ..pats: tpeopt = expropt\"") {
    val mods = List(mod"private", mod"final")
    val pats = List(p"x", p"y")
    val tpeopt = t"T"
    val expropt = q"t"
    assertTree(q"..$mods var ..$pats: $tpeopt = $expropt")(
      Defn.Var(
        List(Mod.Private(Name("")), Mod.Final()),
        List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))),
        Some(Type.Name("T")),
        Some(Term.Name("t"))
      )
    )
  }

  test("1 q\"..mods def name[..tparams](...paramss): tpeopt = expr\"") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" =
      q"private final def m[T, W](x: X, y: Y): R = r"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertTree(name)(Term.Name("m"))
    checkTree(tparams, "[T, W]")(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("T"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("W"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    checkTreesWithSyntax(paramss: _*)("(x: X, y: Y)")(Term.ParamClause {
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Y")), None)
      )
    })
    assertTree(tpeopt)(Some(Type.Name("R")))
    assertTree(expr)(Term.Name("r"))
  }

  test("2 q\"..mods def name[..tparams](...paramss): tpeopt = expr\"") {
    val mods = List(mod"private", mod"final")
    val name = q"m"
    val tparams = List(tparam"T", tparam"W")
    val paramss = List(List(param"x: X", param"x: Y"))
    val tpeopt = t"R"
    val expr = q"r"
    assertTree(q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr")(
      Defn.Def(
        List(Mod.Private(Name("")), Mod.Final()),
        Term.Name("m"),
        List(
          Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil),
          Type.Param(Nil, Type.Name("W"), Nil, Type.Bounds(None, None), Nil, Nil)
        ),
        List(
          List(
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("Y")), None)
          )
        ),
        Some(Type.Name("R")),
        Term.Name("r")
      )
    )
  }

  test("1 q\"..mods def name[..tparams](...paramss): tpeopt = macro expr\"") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr" =
      q"private final def m[T, W](x: X, y: Y): R = macro r"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertTree(name)(Term.Name("m"))
    checkTree(tparams, "[T, W]")(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("T"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("W"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    checkTreesWithSyntax(paramss: _*)("(x: X, y: Y)")(Term.ParamClause {
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Y")), None)
      )
    })
    assertTree(tpeopt)(Some(Type.Name("R")))
    assertTree(expr)(Term.Name("r"))
  }

  test("2 q\"..mods def name[..tparams](...paramss): tpeopt = macro expr\"") {
    val mods = List(mod"private", mod"final")
    val name = q"m"
    val tparams = List(tparam"T", tparam"W")
    val paramss = List(List(param"x: X", param"x: Y"))
    val tpeopt = Some(t"R")
    val expr = q"r"
    assertTree(q"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr")(
      Defn.Macro(
        List(Mod.Private(Name("")), Mod.Final()),
        Term.Name("m"),
        List(
          Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil),
          Type.Param(Nil, Type.Name("W"), Nil, Type.Bounds(None, None), Nil, Nil)
        ),
        List(
          List(
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("Y")), None)
          )
        ),
        Some(Type.Name("R")),
        Term.Name("r")
      )
    )
  }

  test("1 q\"..mods type tname[..tparams] = tpe\"") {
    val q"..$mods type $tname[..$tparams] = $tpe" = q"private final type Q[T, W] = R"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertTree(tname)(Type.Name("Q"))
    assertEquals(tparams.toString, "[T, W]")
    assertTree(tparams)(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("T"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("W"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    assertTree(tpe)(Type.Name("R"))
  }

  test("2 q\"..mods type tname[..tparams] = tpe\"") {
    val mods = List(mod"private", mod"final")
    val tname = t"Q"
    val tparams = List(tparam"T", tparam"W")
    val tpe = t"R"
    assertTree(q"..$mods type $tname[..$tparams] = $tpe")(
      Defn.Type(
        List(Mod.Private(Name("")), Mod.Final()),
        Type.Name("Q"),
        List(
          Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil),
          Type.Param(Nil, Type.Name("W"), Nil, Type.Bounds(None, None), Nil, Nil)
        ),
        Type.Name("R"),
        Type.Bounds(None, None)
      )
    )
  }

  test("1 q\"..mods class tname[..tparams] mod (...paramss) template\"") {
    val q"..$mods class $tname[..$tparams] $mod (...$paramss) $template" =
      q"private final class Q[T, W] private (x: X, y: Y) extends Y"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertTree(tname)(Type.Name("Q"))
    assertEquals(tparams.toString, "[T, W]")
    assertTree(tparams)(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("T"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("W"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    assertTree(mod)(Mod.Private(Name("")))
    checkTreesWithSyntax(paramss: _*)("(x: X, y: Y)")(Term.ParamClause {
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Y")), None)
      )
    })
    assertTree(template)(
      Template(
        Nil,
        List(Init(Type.Name("Y"), Name(""), emptyArgClause)),
        Self(Name(""), None),
        Nil,
        Nil
      )
    )
  }

  test("2 q\"..mods class tname[..tparams] mod (...paramss) template\"") {
    val q"..$mods class $tname[..$tparams] $mod (...$paramss) $template" =
      q"private final class Q[T, W] protected (x: X, y: Y) extends { def m1 = 42; def m2 = 666 }"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertTree(tname)(Type.Name("Q"))
    assertEquals(tparams.toString, "[T, W]")
    assertTree(tparams)(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("T"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("W"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    assertTree(mod)(Mod.Protected(Name("")))
    checkTreesWithSyntax(paramss: _*)("(x: X, y: Y)")(Term.ParamClause {
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Y")), None)
      )
    })
    assertTree(template)(
      Template(
        Nil,
        Nil,
        Self(Name(""), None),
        List(
          Defn.Def(Nil, Term.Name("m1"), Nil, Nil, None, Lit.Int(42)),
          Defn.Def(Nil, Term.Name("m2"), Nil, Nil, None, Lit.Int(666))
        ),
        Nil
      )
    )
  }

  test("3 q\"..mods class tname[..tparams] mod (...paramss) template\"") {
    val mods = List(mod"private", mod"final")
    val tname = t"Q"
    val tparams = List(tparam"T", tparam"W")
    val mod = mod"protected"
    val paramss = List(List(param"x: X", param"x: Y"))
    val template = template"F { def m = 42 }"
    assertTree(q"..$mods class $tname[..$tparams] $mod (...$paramss) $template")(
      Defn.Class(
        List(Mod.Private(Name("")), Mod.Final()),
        Type.Name("Q"),
        List(
          Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil),
          Type.Param(Nil, Type.Name("W"), Nil, Type.Bounds(None, None), Nil, Nil)
        ),
        Ctor.Primary(
          List(Mod.Protected(Name(""))),
          Name(""),
          List(
            List(
              Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
              Term.Param(Nil, Term.Name("x"), Some(Type.Name("Y")), None)
            )
          )
        ),
        Template(
          Nil,
          List(Init(Type.Name("F"), Name(""), emptyArgClause)),
          Self(Name(""), None),
          List(Defn.Def(Nil, Term.Name("m"), Nil, Nil, None, Lit.Int(42))),
          Nil
        )
      )
    )
  }

  test("1 q\"..mods trait tname[..tparams] template\"") {
    val q"..$mods trait $tname[..$tparams] $template" =
      q"private sealed trait Q[T, W] extends Y"
    assertEquals(mods.toString, "List(private, sealed)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Sealed())
    assertTree(tname)(Type.Name("Q"))
    assertEquals(tparams.toString, "[T, W]")
    assertTree(tparams)(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("T"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("W"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    assertTree(template)(
      Template(
        Nil,
        List(Init(Type.Name("Y"), Name(""), emptyArgClause)),
        Self(Name(""), None),
        Nil,
        Nil
      )
    )
  }

  test("2 q\"..mods trait tname[..tparams] template\"") {
    val q"..$mods trait $tname[..$tparams] $template" =
      q"private sealed trait Q[T, W] extends { def m1 = 42; def m2 = 666 }"
    assertEquals(mods.toString, "List(private, sealed)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Sealed())
    assertTree(tname)(Type.Name("Q"))
    assertEquals(tparams.toString, "[T, W]")
    assertTree(tparams)(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("T"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("W"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    assertTree(template)(
      Template(
        Nil,
        Nil,
        Self(Name(""), None),
        List(
          Defn.Def(Nil, Term.Name("m1"), Nil, Nil, None, Lit.Int(42)),
          Defn.Def(Nil, Term.Name("m2"), Nil, Nil, None, Lit.Int(666))
        ),
        Nil
      )
    )
  }

  test("3 q\"..mods trait tname[..tparams] template\"") {
    val mods = List(mod"private", mod"sealed")
    val tname = t"Q"
    val tparams = List(tparam"T", tparam"W")
    val template = template"F { def m = 42 }"
    assertTree(q"..$mods trait $tname[..$tparams] $template")(
      Defn.Trait(
        List(Mod.Private(Name("")), Mod.Sealed()),
        Type.Name("Q"),
        List(
          Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil),
          Type.Param(Nil, Type.Name("W"), Nil, Type.Bounds(None, None), Nil, Nil)
        ),
        EmptyCtor(),
        Template(
          Nil,
          List(Init(Type.Name("F"), Name(""), emptyArgClause)),
          Self(Name(""), None),
          List(Defn.Def(Nil, Term.Name("m"), Nil, Nil, None, Lit.Int(42))),
          Nil
        )
      )
    )
  }

  test("1 q\"..mods object name template\"") {
    val q"..$mods object $name $template" = q"private final object Q extends Y"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertTree(name)(Term.Name("Q"))
    assertTree(template)(
      Template(
        Nil,
        List(Init(Type.Name("Y"), Name(""), emptyArgClause)),
        Self(Name(""), None),
        Nil,
        Nil
      )
    )
  }

  test("2 q\"..mods object name template\"") {
    val q"..$mods object $name $template" =
      q"private final object Q extends { def m1 = 42; def m2 = 666 }"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    assertTree(name)(Term.Name("Q"))
    assertTree(template)(
      Template(
        Nil,
        Nil,
        Self(Name(""), None),
        List(
          Defn.Def(Nil, Term.Name("m1"), Nil, Nil, None, Lit.Int(42)),
          Defn.Def(Nil, Term.Name("m2"), Nil, Nil, None, Lit.Int(666))
        ),
        Nil
      )
    )
  }

  test("3 q\"..mods object name template\"") {
    val mods = List(mod"private", mod"final")
    val name = q"Q"
    val template = template"F { def m = 42 }"
    assertTree(q"..$mods object $name $template")(
      Defn.Object(
        List(Mod.Private(Name("")), Mod.Final()),
        Term.Name("Q"),
        Template(
          Nil,
          List(Init(Type.Name("F"), Name(""), emptyArgClause)),
          Self(Name(""), None),
          List(Defn.Def(Nil, Term.Name("m"), Nil, Nil, None, Lit.Int(42))),
          Nil
        )
      )
    )
  }

  test("1 q\"package object name template\"") {
    val q"package object $name $template" = q"package object Q extends Y"
    assertTree(name)(Term.Name("Q"))
    assertTree(template)(
      Template(
        Nil,
        List(Init(Type.Name("Y"), Name(""), emptyArgClause)),
        Self(Name(""), None),
        Nil,
        Nil
      )
    )
  }

  test("2 q\"package object name template\"") {
    val q"package object $name $template" =
      q"package object Q extends { def m1 = 42; def m2 = 666 }"
    assertTree(name)(Term.Name("Q"))
    assertTree(template)(
      Template(
        Nil,
        Nil,
        Self(Name(""), None),
        List(
          Defn.Def(Nil, Term.Name("m1"), Nil, Nil, None, Lit.Int(42)),
          Defn.Def(Nil, Term.Name("m2"), Nil, Nil, None, Lit.Int(666))
        ),
        Nil
      )
    )
  }

  test("3 q\"package object name template\"") {
    val name = q"Q"
    val template = template"F { def m = 42 }"
    assertTree(q"package object $name $template")(
      Pkg.Object(
        Nil,
        Term.Name("Q"),
        Template(
          Nil,
          List(Init(Type.Name("F"), Name(""), emptyArgClause)),
          Self(Name(""), None),
          List(Defn.Def(Nil, Term.Name("m"), Nil, Nil, None, Lit.Int(42))),
          Nil
        )
      )
    )
  }

  test("1 q\"package ref { ..stats }\"") {
    val q"package $ref { ..$stats }" = q"package p { class A; object B }"
    assertTree(ref)(Term.Name("p"))
    assertEquals(stats.toString, "List(class A, object B)")
    assertTrees(stats: _*)(
      Defn.Class(
        Nil,
        Type.Name("A"),
        Nil,
        EmptyCtor(),
        Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
      ),
      Defn.Object(Nil, Term.Name("B"), Template(Nil, Nil, Self(Name(""), None), Nil, Nil))
    )
  }

  test("2 q\"package ref { ..stats }\"") {
    val ref = q"p"
    val stats = List(q"class A", q"object B")
    assertTree(q"package $ref { ..$stats }")(
      Pkg(
        Term.Name("p"),
        List(
          Defn.Class(
            Nil,
            Type.Name("A"),
            Nil,
            EmptyCtor(),
            Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
          ),
          Defn.Object(Nil, Term.Name("B"), Template(Nil, Nil, Self(Name(""), None), Nil, Nil))
        )
      )
    )
  }

  /*
   Issue #462
   */
  test("3 q\"package ref { ..stats }\"") {
    val ref = q"p.a"
    val stats = List(q"class A", q"object B")
    assertTree(q"package $ref { ..$stats }")(
      Pkg(
        Term.Select(Term.Name("p"), Term.Name("a")),
        List(
          Defn.Class(
            Nil,
            Type.Name("A"),
            Nil,
            EmptyCtor(),
            Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
          ),
          Defn.Object(Nil, Term.Name("B"), Template(Nil, Nil, Self(Name(""), None), Nil, Nil))
        )
      )
    )
  }

  test("1 q\"..mods def this(...paramss)\"") {
    val q"..$mods def this(...$paramss)" = q"private def this(x: X, y: Y)"
    assertEquals(mods.toString, "List(private)")
    assertTrees(mods: _*)(Mod.Private(Name("")))
    checkTreesWithSyntax(paramss: _*)("(x: X, y: Y)")(Term.ParamClause {
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Y")), None)
      )
    })
  }

  test("2 q\"..mods def this(...paramss)\"") {
    val mods = List(mod"private")
    val paramss = List(List(param"x: X", param"x: Y"))
    assertTree(q"..$mods def this(...$paramss)")(
      Ctor.Primary(
        List(Mod.Private(Name(""))),
        Name(""),
        List(
          List(
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("Y")), None)
          )
        )
      )
    )
  }

  test("1 q\"..mods def this(...paramss) = expr\"") {
    val q"..$mods def this(...$paramss) = $init" =
      q"private final def this(x: X, y: Y) = this(foo, bar)"
    assertEquals(mods.toString, "List(private, final)")
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final())
    checkTreesWithSyntax(paramss: _*)("(x: X, y: Y)")(Term.ParamClause {
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Y")), None)
      )
    })
    assertTree(init)(
      Init(
        Type.Singleton(Term.This(Name(""))),
        Name(""),
        List(List(Term.Name("foo"), Term.Name("bar")))
      )
    )
  }

  test("2 q\"..mods def this(...paramss) = expr\"") {
    val mods = List(mod"private", mod"final")
    val paramss = List(List(param"x: X", param"x: Y"))
    val init = init"C(foo, bar)"
    assertTree(q"..$mods def this(...$paramss) = $init")(
      Ctor.Secondary(
        List(Mod.Private(Name("")), Mod.Final()),
        Name.This(),
        List(
          List(
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("Y")), None)
          )
        ),
        Init(Type.Name("C"), Name(""), List(List(Term.Name("foo"), Term.Name("bar")))),
        Nil
      )
    )
  }

  test("1 param\"..mods paramname: tpeopt = expropt\"") {
    val param"..$mods $paramname: $tpeopt = $expropt" = param"private final val x: X = 42"
    assertTrees(mods: _*)(Mod.Private(Name("")), Mod.Final(), Mod.ValParam())
    assertTree(paramname)(Term.Name("x"))
    assertTree(tpeopt)(Some(Type.Name("X")))
    assertTree(expropt)(Some(Lit.Int(42)))
  }

  test("2 param\"..mods paramname: tpeopt = expropt\"") {
    val mods = List(mod"private", mod"final")
    val paramname = q"x"
    val tpeopt = t"X"
    val expropt = q"42"
    assertTree(param"..$mods $paramname: $tpeopt = $expropt")(
      Term.Param(
        List(Mod.Private(Name("")), Mod.Final()),
        Term.Name("x"),
        Some(Type.Name("X")),
        Some(Lit.Int(42))
      )
    )
  }

  test("1 tparam\"..mods tparamname[..tparams] >: tpeopt <: tpeopt <% ..tpes : ..tpes\"") {
    val tparam"..$mods $tparamname[..$tparams] >: $tpeopt1 <: $tpeopt2 <% ..$tpes1 : ..$tpes2" =
      tparam"+Z[Q,W] >: E <: R <% T with Y : U with I"
    assertEquals(mods.toString, "List(+)")
    assertTrees(mods: _*)(Mod.Covariant())
    assertTree(tparamname)(Type.Name("Z"))
    assertWithOriginalSyntax(tparams)(
      "[Q, W]"
    )(
      "[Q, W]"
    )
    assertTree(tparams)(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("Q"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("W"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    assertTree(tpeopt1)(Some(Type.Name("E")))
    assertTree(tpeopt2)(Some(Type.Name("R")))
    assertEquals(tpes1.toString, "List(T with Y)")
    assertTrees(tpes1: _*)(Type.With(Type.Name("T"), Type.Name("Y")))
    assertEquals(tpes2.toString, "List(U with I)")
    assertTrees(tpes2: _*)(Type.With(Type.Name("U"), Type.Name("I")))
  }

  test("2 tparam\"..mods tparamname[..tparams] >: tpeopt <: tpeopt <% ..tpes : ..tpes\"") {
    val mods = List(mod"+")
    val tparamname = t"Z"
    val tparams = List(tparam"Q", tparam"W")
    val tpeopt1 = t"E"
    val tpeopt2 = t"R"
    val tpes1 = List(t"T with Y")
    val tpes2 = List(t"U with I")
    assertTree(
      tparam"..$mods $tparamname[..$tparams] >: $tpeopt1 <: $tpeopt2 <% ..$tpes1 : ..$tpes2"
    )(
      Type.Param(
        List(Mod.Covariant()),
        Type.Name("Z"),
        List(
          Type.Param(Nil, Type.Name("Q"), Nil, Type.Bounds(None, None), Nil, Nil),
          Type.Param(Nil, Type.Name("W"), Nil, Type.Bounds(None, None), Nil, Nil)
        ),
        Type.Bounds(Some(Type.Name("E")), Some(Type.Name("R"))),
        List(Type.With(Type.Name("T"), Type.Name("Y"))),
        List(Type.With(Type.Name("U"), Type.Name("I")))
      )
    )
  }

  test("1 init\"tpe(...exprss)\"") {
    val init"$tpe(...$exprss)" = init"C(40)(2)"
    assertEquals(tpe.toString, "C")
    assertTree(tpe)(Type.Name("C"))
    assertEquals(exprss.map(_.toString), List("(40)", "(2)"))
    assertTrees(exprss: _*)(
      Term.ArgClause(List(Lit.Int(40))),
      Term.ArgClause(List(Lit.Int(2)))
    )
  }

  test("2 init\"tpe(...exprss)\"") {
    val tpe = t"C"
    val exprss = List(List(q"40"), List(q"2"))
    assertTree(init"$tpe(...$exprss)")(
      Init(Type.Name("C"), Name(""), List(List(Lit.Int(40)), List(Lit.Int(2))))
    )
  }

  test("1 init\"this(...exprss)\"") {
    val init"this(...$exprss)" = init"this(40)(2)"
    assertEquals(exprss.map(_.toString), List("(40)", "(2)"))
    assertTrees(exprss: _*)(
      Term.ArgClause(List(Lit.Int(40))),
      Term.ArgClause(List(Lit.Int(2)))
    )
  }

  test("2 init\"this(...exprss)\"") {
    val exprss = List(List(q"40"), List(q"2"))
    assertTree(init"this(...$exprss)")(
      Init(Type.Singleton(Term.This(Name(""))), Name(""), List(List(Lit.Int(40)), List(Lit.Int(2))))
    )
  }

  test("1 self\"name: tpeopt\"") {
    val self"$name: $tpeopt" = self"x: T"
    assertEquals(name.toString, "x")
    assertTree(name)(Term.Name("x"))
    assertEquals(tpeopt.toString, "Some(T)")
    assertTree(tpeopt)(Some(Type.Name("T")))
  }

  test("2 self\"name: tpeopt\"") {
    val name = q"x"
    val tpeopt = t"T"
    assertTree(self"$name: $tpeopt")(Self(Term.Name("x"), Some(Type.Name("T"))))
  }

  test("1 self\"this: tpeopt\"") {
    val self"$name: $tpeopt" = self"this: T"
    assertEquals(name.toString, "this")
    assertTree(name)(Name.This())
    assertEquals(tpeopt.toString, "Some(T)")
    assertTree(tpeopt)(Some(Type.Name("T")))
  }

  test("2 self\"this: tpeopt\"") {
    val tpeopt = t"T"
    assertTree(self"this: $tpeopt")(Self(Name.This(), Some(Type.Name("T"))))
  }

  test("1 template\"{ ..stats } with ..inits { self => ..stats }\"") {
    val template"{ ..$stats1 } with ..$inits { $self => ..$stats2 }" =
      template"{ val a = 2; val b = 2 } with T with U { self: Z => def m = 2; def n = 2 }"
    assertEquals(stats1.toString, "List(val a = 2, val b = 2)")
    assertTrees(stats1: _*)(
      Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Lit.Int(2)),
      Defn.Val(Nil, List(Pat.Var(Term.Name("b"))), None, Lit.Int(2))
    )
    assertEquals(inits.toString, "List(T, U)")
    assertTrees(inits: _*)(
      Init(Type.Name("T"), Name(""), emptyArgClause),
      Init(Type.Name("U"), Name(""), emptyArgClause)
    )
    assertTree(self)(Self(Term.Name("self"), Some(Type.Name("Z"))))
    assertEquals(stats2.toString, "List(def m = 2, def n = 2)")
    assertTrees(stats2: _*)(
      Defn.Def(Nil, Term.Name("m"), Nil, Nil, None, Lit.Int(2)),
      Defn.Def(Nil, Term.Name("n"), Nil, Nil, None, Lit.Int(2))
    )
  }

  test("2 template\"{ ..stats } with ..inits { self => ..stats }\"") {
    val stats1 = List(q"val a = 2", q"val b = 2")
    val inits = List(init"T", init"U")
    val self = self"self: S"
    val stats2 = List(q"def m = 2", q"def n = 2")
    assertTree(template"{ ..$stats1 } with ..$inits { $self => ..$stats2 }")(
      Template(
        List(
          Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Lit.Int(2)),
          Defn.Val(Nil, List(Pat.Var(Term.Name("b"))), None, Lit.Int(2))
        ),
        List(
          Init(Type.Name("T"), Name(""), emptyArgClause),
          Init(Type.Name("U"), Name(""), emptyArgClause)
        ),
        Self(Term.Name("self"), Some(Type.Name("S"))),
        List(
          Defn.Def(Nil, Term.Name("m"), Nil, Nil, None, Lit.Int(2)),
          Defn.Def(Nil, Term.Name("n"), Nil, Nil, None, Lit.Int(2))
        ),
        Nil
      )
    )
  }

  test("1 mod\"@expr\"") {
    val mod"@$expr" = mod"@a"
    assertTree(expr)(Mod.Annot(Init(Type.Name("a"), Name(""), emptyArgClause)))
  }

  test("2 mod\"@expr\"") {
    val expr = mod"@a"
    assertTree(mod"@$expr")(Mod.Annot(Init(Type.Name("a"), Name(""), emptyArgClause)))
  }

  test("1 mod\"private[name]\"") {
    val mod"private[$name]" = mod"private[X]"
    assertTree(name)(Name("X"))
  }

  test("2 mod\"private[name]\"") {
    val mod"private[$name]" = mod"private"
    assertTree(name)(Name(""))
  }

  test("3 mod\"private[this]\"") {
    val mod"private[this]" = mod"private[this]"
  }

  test("4 mod\"private[name]\"") {
    val name = q"q"
    assertTree(mod"private[$name]")(Mod.Private(Term.Name("q")))
  }

  test("1 mod\"protected[name]\"") {
    val mod"protected[$name]" = mod"protected[X]"
    assertTree(name)(Name("X"))
  }

  test("2 mod\"protected[name]\"") {
    val mod"protected[$name]" = mod"protected"
    assertTree(name)(Name(""))
  }

  test("3 mod\"protected[this]\"") {
    val mod"protected[this]" = mod"protected[this]"
  }

  test("4 mod\"protected[name]\"") {
    val name = q"q"
    assertTree(mod"protected[$name]")(Mod.Protected(Term.Name("q")))
  }

  test("mod\"implicit\"") {
    assertTree(mod"implicit")(Mod.Implicit())
  }

  test("mod\"final\"") {
    assertTree(mod"final")(Mod.Final())
  }

  test("mod\"sealed\"") {
    assertTree(mod"sealed")(Mod.Sealed())
  }

  test("mod\"override\"") {
    assertTree(mod"override")(Mod.Override())
  }

  test("mod\"case\"") {
    assertTree(mod"case")(Mod.Case())
  }

  test("mod\"abstract\"") {
    assertTree(mod"abstract")(Mod.Abstract())
  }

  test("mod\"+\"") {
    assertTree(mod"+")(Mod.Covariant())
  }

  test("mod\"-\"") {
    assertTree(mod"-")(Mod.Contravariant())
  }

  test("mod\"lazy\"") {
    assertTree(mod"lazy")(Mod.Lazy())
  }

  test("mod\"val\"") {
    assertTree(mod"valparam")(Mod.ValParam())
  }

  test("mod\"var\"") {
    assertTree(mod"varparam")(Mod.VarParam())
  }

  test("1 enumerator\"pat <- expr\"") {
    val enumerator"$pat <- $expr" = enumerator"x <- xs"
    assertTree(pat)(Pat.Var(Term.Name("x")))
  }

  test("2 enumerator\"pat <- expr\"") {
    val pat = p"x"
    val expr = q"xs"
    assertTree(enumerator"$pat <- $expr")(
      Enumerator.Generator(Pat.Var(Term.Name("x")), Term.Name("xs"))
    )
  }

  test("3 enumerator\"pat <- expr\"") {
    val pat = p"X"
    val expr = q"xs"
    assertTree(enumerator"$pat <- $expr")(
      Enumerator.Generator(Pat.Var(Term.Name("X")), Term.Name("xs"))
    )
  }

  test("1 enumerator\"pat = expr\"") {
    val enumerator"$pat = $expr" = enumerator"x = xs"
    assertTree(pat)(Pat.Var(Term.Name("x")))
  }

  test("2 enumerator\"pat = expr\"") {
    val pat = p"x"
    val expr = q"xs"
    assertTree(enumerator"$pat = $expr")(Enumerator.Val(Pat.Var(Term.Name("x")), Term.Name("xs")))
  }

  test("1 enumerator\"if expr\"") {
    val enumerator"if $expr" = enumerator"if x"
    assertTree(expr)(Term.Name("x"))
  }

  test("2 enumerator\"if expr\"") {
    val expr = q"x"
    assertTree(enumerator"if $expr")(Enumerator.Guard(Term.Name("x")))
  }

  test("1 q\"import ..importers\"") {
    val importers = List(importer"foo.bar", importer"bar.{baz, _}")
    assertEquals(q"import ..$importers".syntax, "import foo.bar, bar.{ baz, _ }")
  }

  test("2 q\"import ..importers\"") {
    val q"import ..$importers" = q"import a.A"
    assertEquals(importers.map(_.syntax), List("a.A"))
  }

  test("1 importer\"ref.{..importees}\"") {
    val ref = q"bar"
    val importees = List(importee"baz", importee"_")
    assertEquals(importer"$ref.{..$importees}".syntax, "bar.{ baz, _ }")
  }

  test("2 importer\"ref. ..importees\"") {
    val importer"$ref.{..$importees}" = importer"bar.{baz, _}"
    assertEquals(ref.syntax, "bar")
    assertEquals(importees.map(_.syntax), List("baz", "_"))
  }

  /*
   Issue #462
   */
  test("3 importer\"ref.{..importees}\"") {
    val ref = q"bar.a"
    val importees = List(importee"baz", importee"_")
    assertEquals(importer"$ref.{..$importees}".syntax, "bar.a.{ baz, _ }")
  }

  test("1 importee\"iname\"") {
    val importee"$iname" = importee"x"
    assertTree(iname)(Name("x"))
  }

  test("2 importee\"iname\"") {
    // $iname can't be constructed, only extracted from importee"..." and mod"..."
    val importee"$iname" = importee"x"
    assertTree(importee"$iname")(Importee.Name(Name("x")))
  }

  test("1 importee\"iname => iname\"") {
    val importee"$iname1 => $iname2" = importee"x => y"
    assertTree(iname1)(Name("x"))
    assertTree(iname2)(Name("y"))
  }

  test("2 importee\"iname => iname\"") {
    // $iname can't be constructed, only extracted from importee"..." and mod"..."
    val importee"$iname1 => $iname2" = importee"x => y"
    assertTree(importee"$iname1 => $iname2")(Importee.Rename(Name("x"), Name("y")))
  }

  test("1 importee\"iname => _\"") {
    val importee"$iname => _" = importee"x => _"
    assertTree(iname)(Name("x"))
  }

  test("2 importee\"iname => _\"") {
    // $iname can't be constructed, only extracted from importee"..." and mod"..."
    val importee"$iname => _" = importee"x => _"
    assertTree(importee"$iname => _")(Importee.Unimport(Name("x")))
  }

  test("importee\"_\"") {
    assertTree(importee"_")(Importee.Wildcard())
  }

  test("1 source\"..stats\"") {
    val source"..$stats" = source"class A { val a = 'a'}"
    assertWithOriginalSyntax(stats: _*)(
      "class A { val a = 'a' }"
    )(
      "class A { val a = 'a' }"
    )
    assertTrees(stats: _*)(
      Defn.Class(
        Nil,
        Type.Name("A"),
        Nil,
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Lit.Char('a'))),
          Nil
        )
      )
    )
  }

  test("2 source\"..stats\"") {
    val source"class B { val b = 'b'}; ..$stats" =
      source"class B { val b = 'b'}; class A { val a = 'a'}"
    assertWithOriginalSyntax(stats: _*)(
      "class A { val a = 'a' }"
    )(
      "class A { val a = 'a' }"
    )
    assertTrees(stats: _*)(
      Defn.Class(
        Nil,
        Type.Name("A"),
        Nil,
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Lit.Char('a'))),
          Nil
        )
      )
    )
  }

  test("3 source\"..stats\"") {
    val stats = List(q"class A { val x = 1 }", q"object B")
    assertTree(source"..$stats")(
      Source(
        List(
          Defn.Class(
            Nil,
            Type.Name("A"),
            Nil,
            EmptyCtor(),
            Template(
              Nil,
              Nil,
              Self(Name(""), None),
              List(Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.Int(1))),
              Nil
            )
          ),
          Defn.Object(Nil, Term.Name("B"), Template(Nil, Nil, Self(Name(""), None), Nil, Nil))
        )
      )
    )
  }

  test("unquote T into Option[T]") {
    val cond = q"cond"
    assertTree(p"case _ if $cond =>")(
      Case(Pat.Wildcard(), Some(Term.Name("cond")), Term.Block(Nil))
    )
  }

  test("unquote Option[T] into Option[T]") {
    val condopt = Some(q"cond")
    assertTree(p"case _ if $condopt =>")(
      Case(Pat.Wildcard(), Some(Term.Name("cond")), Term.Block(Nil))
    )
  }

  test("extract Some[T] from Option[T]") {
    val p"case _ if $condopt =>" = p"case _ if cond =>"
    assertTree(condopt)(Some(Term.Name("cond")))
  }

  test("extract None from Option[T]") {
    val p"case _ if $condopt =>" = p"case _ =>"
    assertTree(condopt)(None)
  }

  test("initial support for ...") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpe = $rhs" = q"def f(x: Int) = ???"
    checkTree(tparams, "")(Type.ParamClause(Nil))
    checkTreesWithSyntax(paramss: _*)("(x: Int)")(Term.ParamClause {
      List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None))
    })
    assertTree(q"..$mods def $name[..$tparams](...$paramss): $tpe = $rhs")(
      Defn.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None))),
        None,
        Term.Name("???")
      )
    )
  }

  test("initial support for ..., with tparams") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpe = $rhs" = q"def f[A](x: Int) = ???"
    assertTree(tparams)(Type.ParamClause {
      List(
        Type.Param(Nil, Type.Name("A"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    })
    checkTreesWithSyntax(paramss: _*)("(x: Int)")(Term.ParamClause {
      List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None))
    })
    assertTree(q"..$mods def $name[..$tparams](...$paramss): $tpe = $rhs")(
      Defn.Def(
        Nil,
        Term.Name("f"),
        List(Type.Param(Nil, Type.Name("A"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None))),
        None,
        Term.Name("???")
      )
    )
  }

  test("ellipses in template stats") {
    val mods = List(mod"private")
    val tree = q"class C { ..$mods def x = 2 }"
    assertTree(tree)(
      Defn.Class(
        Nil,
        Type.Name("C"),
        Nil,
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Defn.Def(List(Mod.Private(Name(""))), Term.Name("x"), Nil, Nil, None, Lit.Int(2))),
          Nil
        )
      )
    )
  }

  test("#300") {
    val q"class $tname1 ..$mods1" = q"class C"
    assertEquals(q"class $tname1 ..$mods1".syntax, "class C")
    val q"class $tname2 ..$mods2" = q"class C private"
    assertEquals(q"class $tname2 ..$mods2".syntax, "class C private")
  }

  test("#448") {
    val parent = init"_root_.scala.AnyVal"
    val template = template"$parent"
    assertEquals(q"class C $template".syntax, "class C extends _root_.scala.AnyVal")
    assertEquals(q"class C extends $parent {}".syntax, "class C extends _root_.scala.AnyVal")
    assertEquals(q"class C extends $parent".syntax, "class C extends _root_.scala.AnyVal")
    assert(
      q"class C extends $parent with $parent".syntax ==
        "class C extends _root_.scala.AnyVal with _root_.scala.AnyVal"
    )
  }

  test("#452") {
    val stat = q"class C"
    assertEquals(
      q"$stat; $stat".syntax,
      """
    |{
    |  class C
    |  class C
    |}""".trim.stripMargin.split('\n').mkString(EOL)
    )
    assertEquals(
      q"{ $stat; $stat }".syntax,
      """
    |{
    |  class C
    |  class C
    |}""".trim.stripMargin.split('\n').mkString(EOL)
    )
  }

  test("#450") {
    val defDefns = List(q"def baz {}")
    val objectDefn = q"""
      object M {
        def foo = bar
        println("another stat")
        ..$defDefns
      }
    """
    assertEquals(
      objectDefn.syntax,
      """
      |object M {
      |  def foo = bar
      |  println("another stat")
      |  def baz: Unit = {}
      |}
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
  }

  test("#458") {
    val name = q"x"
    val tpe = t"T"
    val lambda = q"($name: $tpe) => ???"
    assertEquals(lambda.syntax, "(x: T) => ???")
  }

  test("#458 II") {
    val name = q"x"
    val lambda = q"($name: T) => ???"
    assertEquals(lambda.syntax, "(x: T) => ???")
  }

  test("#455 - unquote None") {
    val defnopt: Option[Stat] = None
    assertTree(q"..$defnopt")(Term.Block(Nil))
  }

  test("#455 - unquote Some") {
    val defnoptSomeOption: Some[Stat] = Some(q"val x = 42")
    assertTree(q"..$defnoptSomeOption")(
      Term.Block(List(Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.Int(42))))
    )
  }

  test("#455 - unquote Option") {
    val defnopt: Option[Stat] = Option(q"val x = 42")
    assertTree(q"..$defnopt")(
      Term.Block(List(Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.Int(42))))
    )
  }

  test("#468 - primary constructor I") {
    val q"case class A($param)" = q"case class A(a: Int)"
    assertEquals(param.syntax, "a: Int")
  }

  test("#468 - primary constructor II") {
    val q"case class A($param, ..$params)" = q"case class A(a: Int, b: Int, c: Int)"
    assertEquals(param.syntax, "a: Int")
    assertEquals(params.map(_.syntax), List("b: Int", "c: Int"))
  }

  test("#468 - primary constructor III") {
    val q"case class A(..$params)" = q"case class A(a: Int, b: String)"
    checkTreesWithSyntax(params)("(a: Int, b: String)")(Term.ParamClause {
      List(
        Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None),
        Term.Param(Nil, Term.Name("b"), Some(Type.Name("String")), None)
      )
    })
  }

  test("#468 - primary constructor IV") {
    val q"case class A(...$paramss)" = q"case class A(a: Int)(b: String)"
    assertEquals(paramss.length, 2)
    checkTreesWithSyntax(paramss: _*)("(a: Int)", "(b: String)")(
      Term.ParamClause(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None))),
      Term.ParamClause(List(Term.Param(Nil, Term.Name("b"), Some(Type.Name("String")), None)))
    )
  }

  test("#468 - function parameter list I") {
    val q"def foo($param): Int = a" = q"def foo(a: Int): Int = a"
    assertEquals(param.syntax, "a: Int")
  }

  test("#468 - function parameter list II") {
    val q"def foo($param, ..$params): Int = a" = q"def foo(a: Int, b: Int, c: Int): Int = a"
    assertEquals(param.syntax, "a: Int")
    assertEquals(params.map(_.syntax), List("b: Int", "c: Int"))
  }

  test("#468 - function parameter list III") {
    val q"def foo(..$params): Int = a" = q"def foo(a: Int, b: String): Int = a"
    checkTreesWithSyntax(params)("(a: Int, b: String)")(Term.ParamClause {
      List(
        Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None),
        Term.Param(Nil, Term.Name("b"), Some(Type.Name("String")), None)
      )
    })
  }

  test("#468 - function parameter list IV") {
    val q"def foo(...$paramss): Int = a" = q"def foo(a: Int)(b: String): Int = a"
    assertEquals(paramss.length, 2)
    checkTreesWithSyntax(paramss: _*)("(a: Int)", "(b: String)")(
      Term.ParamClause(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None))),
      Term.ParamClause(List(Term.Param(Nil, Term.Name("b"), Some(Type.Name("String")), None)))
    )
  }

  test("#468 - function parameter list V") {
    val q"def foo(...$paramss)(..$params)($param): Int = a" =
      q"def foo(a: Int)(b: String)(c: Long): Int = a"
    checkTreesWithSyntax(paramss: _*)("(a: Int)")(Term.ParamClause {
      List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None))
    })
    checkTree(params, "(b: String)") {
      Term.ParamClause(Term.Param(Nil, Term.Name("b"), Some(Type.Name("String")), None) :: Nil)
    }
    checkTree(param, "c: Long") {
      Term.Param(Nil, Term.Name("c"), Some(Type.Name("Long")), None)
    }
  }

  test("#230 - tparam extensions I") {
    val tparam = tparam"@foo ${Mod.Covariant()} T"
    assertEquals(tparam.syntax, "@foo +T")
  }

  test("#1006 - tparam extensions II") {
    val t1 = Type.Name("T1")
    val t2 = Type.Name("T2")
    val tparam1 = tparam"$t1"
    assertEquals(tparam1.syntax, "T1")
    val tparam2 = tparam"$t1 : $t2"
    assertEquals(tparam2.syntax, "T1: T2")
  }

  test("#829 - lambda extensions I") {
    val param = param"x:Int"
    val lambda = q"map($param => 3)"
    assertEquals(lambda.syntax, "map((x: Int) => 3)")
  }

  test("#843") {
    val t = t"x.${Type.Name("T")}"
    assertEquals(t.syntax, "x.T")
  }

  test("#915") {
    val a = q"a"
    val importer = importer"$a.b"
    assertEquals(importer.syntax, "a.b")
  }

  test("#833") {
    val ys = List(Term.Name("y"))
    val block = q"x; ..$ys; z"
    assertEquals(
      block.syntax,
      """
      |{
      |  x
      |  y
      |  z
      |}
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
  }

  test("#2841 empty, with extends") {
    val q"..$mods object $ename extends $template" = q"object X extends Y"
    assertTree(template) {
      Init(Type.Name("Y"), Name(""), emptyArgClause)
    }
    assertEquals(mods, Nil)
  }

  test("#2841 empty") {
    val q"..$mods object $ename $template" = q"object X extends Y"
    assertTree(template) {
      Template(
        Nil,
        List(Init(Type.Name("Y"), Name(""), emptyArgClause)),
        Self(Name(""), None),
        Nil,
        Nil
      )
    }
    assertEquals(mods, Nil)
  }

  test("#2841 non-empty, with extends") {
    intercept[MatchError] {
      val q"..$mods object $ename extends $template" = q"object X extends Y { def foo }"
    }
  }

  test("#2841 non-empty, no extends") {
    val q"..$mods object $ename $template" = q"object X extends Y { def foo }"
    assertTree(template) {
      Template(
        Nil,
        List(Init(Type.Name("Y"), Name(""), emptyArgClause)),
        Self(Name(""), None),
        List(Decl.Def(Nil, Term.Name("foo"), Nil, Nil, Type.Name("Unit"))),
        Nil
      )
    }
    assertEquals(mods, Nil)
  }

  test("#2841 empty, full sig") {
    val q"..$mods object $ename extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =
      q"object X extends Y"
    assertEquals(mods, Nil)
    assertEquals(earlydefns, Nil)
    assertTrees(parents: _*) {
      Init(Type.Name("Y"), Name.Anonymous(), emptyArgClause)
    }
    assertTree(self)(Self(Name(""), None))
    assertEquals(stats, Nil)
  }

  test("#2841 non-empty, full sig") {
    val q"..$mods object $ename extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =
      q"object X extends Y { def foo }"
    assertEquals(mods, Nil)
    assertEquals(earlydefns, Nil)
    assertTrees(parents: _*) {
      Init(Type.Name("Y"), Name.Anonymous(), emptyArgClause)
    }
    assertTree(self)(Self(Name(""), None))
    assertTrees(stats: _*) {
      Decl.Def(Nil, Term.Name("foo"), Nil, Nil, Type.Name("Unit"))
    }
  }

  test("#2841 empty, partial sig") {
    val q"..$mods object $ename extends ..$parents { ..$stats }" =
      q"object X extends Y"
    assertEquals(mods, Nil)
    assertTrees(parents: _*) {
      Init(Type.Name("Y"), Name.Anonymous(), emptyArgClause)
    }
    assertEquals(stats, Nil)
  }

  test("#2841 non-empty, partial sig") {
    val q"..$mods object $ename extends ..$parents { ..$stats }" =
      q"object X extends Y { def foo }"
    assertEquals(mods, Nil)
    assertTrees(parents: _*) {
      Init(Type.Name("Y"), Name.Anonymous(), emptyArgClause)
    }
    assertTrees(stats: _*) {
      Decl.Def(Nil, Term.Name("foo"), Nil, Nil, Type.Name("Unit"))
    }
  }

  test("#3388") {
    val term = q"""new Foo(a = a, b = b)"""
    val assignA = Term.Assign(Term.Name("a"), Term.Name("a"))
    val assignB = Term.Assign(Term.Name("b"), Term.Name("b"))

    val q"""new Foo(..$params2)""" = term
    assertTrees(params2: _*)(assignA, assignB)

    val q"""new Foo(...$params3)""" = term
    assertTrees(params3: _*)(Term.ArgClause(List(assignA, assignB), None))
  }

  test("#3409") {
    val code: Tree = source"object Generated {}"
    code.privateOrigin match {
      case Origin.None =>
      case x => fail(s"origin doesn't match: $x")
    }
  }

  test("#3409 check interpolated origin") {
    val valX = q"x // X"
    val fOfX = q"func( $valX )"

    assertWithOriginalSyntax(valX, "x", "x")
    assertWithOriginalSyntax(fOfX, "func(x)", "func(x)")

    def assertOriginType(obtained: Tree, expected: Class[_ <: Origin]): Unit =
      assertEquals(
        obtained.origin.getClass.asInstanceOf[Class[Origin]],
        expected.asInstanceOf[Class[Origin]]
      )

    assertOriginType(valX, Origin.None.getClass)
    assertOriginType(fOfX, Origin.None.getClass)
  }

}
