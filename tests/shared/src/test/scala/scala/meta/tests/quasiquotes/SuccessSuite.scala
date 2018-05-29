package scala.meta.tests
package quasiquotes

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

import compat.Platform.EOL

// FIXME: https://github.com/scalatest/scalatest/issues/1112
// I had to remove $ characters from all test names in this file.
// This is because ScalaTest seems to erroneously consider dollars to be name terminators,
// so it would spuriously crash with "duplicated test" exceptions for e.g.:
// test("1 p\"case $x: T => \"") { ... } and test("1 p\"case $x @ $y => \"") { .. }.

class SuccessSuite extends FunSuite {
  test("rank-0 liftables") {
    assert(q"foo[${42}]".structure === "Term.ApplyType(Term.Name(\"foo\"), List(Lit.Int(42)))")
    assert(q"${42}".structure === "Lit.Int(42)")
  }

  test("rank-1 liftables") {
    implicit def custom[U >: List[Term]]: Lift[List[Int], U] = Lift(_.map(x => q"$x".asInstanceOf[Term]))
    assert(q"foo(..${List(1, 2, 3)})".structure === "Term.Apply(Term.Name(\"foo\"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3)))")
  }

  test("construction ascriptions") {
    val xs = List(q"x", q"y")
    assert(q"foo(..${xs: List[Term]})".syntax === "foo(x, y)")
    val xss = List(List(q"x", q"y"))
    assert(q"foo(...${xss: List[List[Term]]})".syntax === "foo(x, y)")
    val rhs = Some(q"x")
    assert(q"var foo = ${rhs : Option[Term]}".syntax === "var foo = x")
  }

  test("deconstruction ascriptions") {
    val q"foo(..${xs: List[Term]})" = q"foo(x, y)"
    assert(xs.toString === "List(x, y)")
    val q"foo(...${xss: List[List[Term]]})" = q"foo(x, y)"
    assert(xss.toString === "List(List(x, y))")
    val q"var foo = ${x: Option[Term]}" = q"var foo = x"
    assert(x.toString === "Some(x)")
  }

  test("1 Type.Var or Type.Name") {
    val q"1 match { case _: List[..$tpes] => }" = q"1 match { case _: List[t] => }"
    assert(tpes(0).structure === "Type.Var(Type.Name(\"t\"))")
  }

  test("2 Type.Var or Type.Name") {
    val q"1 match {case x: $tpe =>}" = q"1 match {case x: T =>}"
    assert(tpe.structure === "Type.Name(\"T\")")
  }

  test("3 Type.Var or Type.Name") {
    val q"1 match {case x: $tpe =>}" = q"1 match {case x: t =>}"
    assert(tpe.structure === "Type.Name(\"t\")")
  }

  test("1 p\"case x: T => \"") {
    val p"case $x: T => " = p"case x: T =>"
    assert(x.structure === "Pat.Var(Term.Name(\"x\"))")
  }

  test("2 p\"case x: T => \"") {
    val x = p"x"
    assert(p"case $x: T => ".structure === "Case(Pat.Typed(Pat.Var(Term.Name(\"x\")), Type.Name(\"T\")), None, Term.Block(Nil))")
  }

  test("1 p\"case x @ y => \"") {
    val p"case $x @ $y => " = p"case x @ List(1, 2, 3) =>"
    assert(x.structure === "Pat.Var(Term.Name(\"x\"))")
    assert(y.structure === "Pat.Extract(Term.Name(\"List\"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3)))")
  }

  test("2 p\"case x @ y => \"") {
    val x = p"x"
    val y = p"List(1, 2, 3)"
    assert(p"case $x @ $y => ".structure === "Case(Pat.Bind(Pat.Var(Term.Name(\"x\")), Pat.Extract(Term.Name(\"List\"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3)))), None, Term.Block(Nil))")
  }

  test("1 q\"foo(term, ..terms, term)\"") {
    val q"foo($term1, ..$terms, $term2)" = q"foo(x, y, z, q)"
    assert(term1.structure === "Term.Name(\"x\")")
    assert(terms.toString === "List(y, z)")
    assert(terms(0).structure === "Term.Name(\"y\")")
    assert(terms(1).structure === "Term.Name(\"z\")")
    assert(term2.structure === "Term.Name(\"q\")")
  }

  test("2 q\"foo(term, ..terms, term)\"") {
    val term = q"x"
    val terms = List(q"y", q"z")
    assert(q"foo($term, ..$terms, $term)".structure === "Term.Apply(Term.Name(\"foo\"), List(Term.Name(\"x\"), Term.Name(\"y\"), Term.Name(\"z\"), Term.Name(\"x\")))")
  }

  test("case q\"foo({x: Int})\"") {
    q"foo(42)" match {
      case q"$foo(${x: Int})" =>
        assert(foo.structure === "Term.Name(\"foo\")")
        assert(x == 42)
    }
  }

  test("case q\"foo({x: Int}, ..ys, z)\"") {
    q"foo(1, 2, 3)" match {
      case q"$_(${x: Int}, ..$y, $z)" =>
        assert(x === 1)
        assert(y.map(_.structure) === List("Lit.Int(2)"))
        assert(z.structure === "Lit.Int(3)")
    }
  }

  test("1 q\"foo(x, ..ys, z)\"") {
    val q"foo($x, ..$ys, $z)" = q"foo(1, 2, 3)"
    assert(x.structure === "Lit.Int(1)")
    assert(ys.toString === "List(2)")
    assert(ys(0).structure === "Lit.Int(2)")
    assert(z.structure === "Lit.Int(3)")
  }

  test("2 q\"foo(x, ..ys, z, ..ts)\"") {
    val x = q"1"
    val ys = List(q"2")
    val z = q"3"
    val ts = Nil
    assert(q"foo($x, ..$ys, $z, ..$ts)".structure === "Term.Apply(Term.Name(\"foo\"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3)))")
  }

  test("1 val q\"type name[_] = _\"") {
    val q"type $name[$_] = $_" = q"type List[+A] = List[A]"
    assert(name.structure === "Type.Name(\"List\")")
  }

  test("2 val q\"type name[a] = b\"") {
    val q"type $name[$a] = $b" = q"type List[+A] = List[A]"
    assert(name.structure === "Type.Name(\"List\")")
    assert(a.structure === "Type.Param(List(Mod.Covariant()), Type.Name(\"A\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(b.structure === "Type.Apply(Type.Name(\"List\"), List(Type.Name(\"A\")))")
  }

  test("3 val q\"type name[a] = b\"") {
    val name = t"List"
    val a = tparam"+A"
    val b = t"B"
    assert(q"type $name[$a] = $b".structure === "Defn.Type(Nil, Type.Name(\"List\"), List(Type.Param(List(Mod.Covariant()), Type.Name(\"A\"), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Name(\"B\"))")
  }

  test("1 val q\"def x = {body: Int}\"") {
    val q"def x = ${body: Int}" = q"def x = 42"
    assert(body === 42)
  }

  test("2 val q\"def x = {body: Int}\"") {
    val body = 42
    assert(q"def x = ${body: Int}".structure === "Defn.Def(Nil, Term.Name(\"x\"), Nil, Nil, None, Lit.Int(42))")
  }

  test("1 q\"name.this.id\"") {
    val q"$name.this.$x" = q"SuccessSuite.this.x"
    assert(name.structure === "Name(\"SuccessSuite\")")
    assert(x.structure === "Term.Name(\"x\")")
  }

  test("2 q\"name.this.id\"") {
    val name = q"A"
    val x = q"B"
    // inconsistency with the test above planned, since Name can't be constructed directly
    assert(q"$name.this.$x".structure === "Term.Select(Term.This(Term.Name(\"A\")), Term.Name(\"B\"))")
  }

  test("1 this variants") {
    val q"this" = q"this"
    val q"$clazz.this" = q"C.this"
    assert(clazz.structure === "Name(\"C\")")
  }

  test("2 this variants") {
    val clazz = t"C"
    assert(q"this".structure === "Term.This(Name(\"\"))")
    assert(q"$clazz.this".structure === "Term.This(Type.Name(\"C\"))")
  }

  test("1 q\"name.super[name].id\"") {
    val q"$clazz.super[$tpe].$id" = q"A.super[B].x"
    assert(clazz.structure === "Name(\"A\")")
    assert(tpe.structure === "Name(\"B\")")
    assert(id.structure === "Term.Name(\"x\")")
  }

  test("2 q\"name.super[name].id\"") {
    val clazz = q"A"
    val tpe = t"B"
    val id = q"x"
    // inconsistency with the test above planned, since Name can't be constructed directly
    assert(q"$clazz.super[$tpe].m".structure === "Term.Select(Term.Super(Term.Name(\"A\"), Type.Name(\"B\")), Term.Name(\"m\"))")
  }

  test("1 super variants") {
    val q"super" = q"super"
    val q"super[$tpe1]" = q"super[M]"
    val q"$clazz1.super" = q"C.super"
    val q"$clazz2.super[$tpe2]" = q"C.super[M]"
    assert(tpe1.structure === "Name(\"M\")")
    assert(tpe2.structure === "Name(\"M\")")
    assert(clazz1.structure === "Name(\"C\")")
    assert(clazz2.structure === "Name(\"C\")")
  }

  test("2 super variants") {
    val clazz = t"C"
    val tpe = t"M"
    assert(q"super".structure === "Term.Super(Name(\"\"), Name(\"\"))")
    assert(q"super[$tpe]".structure === "Term.Super(Name(\"\"), Type.Name(\"M\"))")
    assert(q"$clazz.super".structure === "Term.Super(Type.Name(\"C\"), Name(\"\"))")
    assert(q"$clazz.super[$tpe]".structure === "Term.Super(Type.Name(\"C\"), Type.Name(\"M\"))")
  }

  test("1 q\"expr.name\"") {
    val q"$expr.$name" = q"foo.bar"
    assert(expr.structure === "Term.Name(\"foo\")")
    assert(name.structure === "Term.Name(\"bar\")")
  }

  test("2 q\"expr.name\"") {
    val expr = q"foo"
    val name = q"bar"
    assert(q"$expr.$name".structure === "Term.Select(Term.Name(\"foo\"), Term.Name(\"bar\"))")
  }

  test("1 q\"expr(name)\"") {
    val q"$expr($name)" = q"foo(bar)"
    assert(expr.structure === "Term.Name(\"foo\")")
    assert(name.structure === "Term.Name(\"bar\")")
  }

  test("2 q\"expr(name)\"") {
    val expr = q"foo"
    val name = q"bar"
    assert(q"$expr($name)".structure === "Term.Apply(Term.Name(\"foo\"), List(Term.Name(\"bar\")))")
  }

  test("1 q\"foo[..tpes]\"") {
    val q"$foo[..$types]" = q"foo[T, U]"
    assert(foo.toString == "foo")
    assert(types.toString === "List(T, U)")
    assert(types(0).structure === "Type.Name(\"T\")")
    assert(types(1).structure === "Type.Name(\"U\")")
  }

  test("2 q\"foo[..tpes]\"") {
    val foo = q"foo"
    val types = List(t"T", t"U")
    assert(q"$foo[..$types]".structure === "Term.ApplyType(Term.Name(\"foo\"), List(Type.Name(\"T\"), Type.Name(\"U\")))")
  }

  test("1 q\"expr name[..tpes] (..exprs)\"") {
    val q"$expr $name[..$tpes] (..$exprs)" = q"x method[T, U] (1, b)"
    assert(expr.structure === "Term.Name(\"x\")")
    assert(name.structure === "Term.Name(\"method\")")
    assert(tpes.toString === "List(T, U)")
    assert(tpes(0).structure === "Type.Name(\"T\")")
    assert(tpes(1).structure === "Type.Name(\"U\")")
    assert(exprs.toString === "List(1, b)")
    assert(exprs(0).structure === "Lit.Int(1)")
    assert(exprs(1).structure === "Term.Name(\"b\")")
  }

  test("2 q\"expr name[..tpes] (..exprs)\"") {
    val expr = q"x"
    val name = q"method"
    val tpes = List(t"T", t"U")
    val exprs = List(q"1", q"b")
    assert(q"$expr $name[..$tpes] (..$exprs)".structure === "Term.ApplyInfix(Term.Name(\"x\"), Term.Name(\"method\"), List(Type.Name(\"T\"), Type.Name(\"U\")), List(Lit.Int(1), Term.Name(\"b\")))")
  }

  test("1 q\"a b c\"") {
    val q"$a $b $c" = q"x y z"
    assert(a.structure === "Term.Name(\"x\")")
    assert(b.structure === "Term.Name(\"y\")")
    assert(c.structure === "Term.Name(\"z\")")
  }

  test("2 q\"a b c\"") {
    val a = q"x"
    val b = q"y"
    val c = q"z"
    assert(q"$a $b $c".structure === "Term.ApplyInfix(Term.Name(\"x\"), Term.Name(\"y\"), Nil, List(Term.Name(\"z\")))")
  }

  test("1 q\"!expr\"") {
    val q"!$x" = q"!foo"
    assert(x.structure === "Term.Name(\"foo\")")
  }

  test("2 q\"!expr\"") {
    val x = q"foo"
    assert(q"!$x".structure === "Term.ApplyUnary(Term.Name(\"!\"), Term.Name(\"foo\"))")
  }

  test("1 q\"~expr\"") {
    val q"~$x" = q"~foo"
    assert(x.structure === "Term.Name(\"foo\")")
  }

  test("2 q\"~expr\"") {
    val expr = q"foo"
    assert(q"~$expr".structure === "Term.ApplyUnary(Term.Name(\"~\"), Term.Name(\"foo\"))")
  }

  test("1 q\"-expr\"") {
    val q"-$x" = q"-foo"
    assert(x.structure === "Term.Name(\"foo\")")
  }

  test("2 q\"-expr\"") {
    val x = q"foo"
    assert(q"-$x".structure === "Term.ApplyUnary(Term.Name(\"-\"), Term.Name(\"foo\"))")
  }

  test("1 q\"+expr\"") {
    val q"+$x" = q"+foo"
    assert(x.structure === "Term.Name(\"foo\")")
  }

  test("2 q\"+expr\"") {
    val x = q"foo"
    assert(q"+$x".structure === "Term.ApplyUnary(Term.Name(\"+\"), Term.Name(\"foo\"))")
  }

  test("1 q\"ref = expr\"") {
    val q"$ref = $expr" = q"a = b"
    assert(ref.structure === "Term.Name(\"a\")")
    assert(expr.structure === "Term.Name(\"b\")")
  }

  test("2 q\"ref = expr\"") {
    val ref = q"a"
    val expr = q"b"
    assert(q"$ref = $expr".structure === "Term.Assign(Term.Name(\"a\"), Term.Name(\"b\"))")
  }

  test("1 val q\"x.y = z.w\" = q\"a.b = c.d\"") {
    val q"$x.$y = $z.$w" = q"a.b = c.d"
    assert(x.structure === "Term.Name(\"a\")")
    assert(y.structure === "Term.Name(\"b\")")
    assert(z.structure === "Term.Name(\"c\")")
    assert(w.structure === "Term.Name(\"d\")")
  }

  test("2 val q\"x.y = z.w\" = q\"a.b = c.d\"") {
    val x = q"a"
    val y = q"b"
    val z = q"c"
    val w = q"d"
    assert(q"$x.$y = $z.$w".structure === "Term.Assign(Term.Select(Term.Name(\"a\"), Term.Name(\"b\")), Term.Select(Term.Name(\"c\"), Term.Name(\"d\")))")
  }

  test("q\"1 expr(...exprs) = expr\"") {
    val q"$expr1(...$exprs) = $expr2" = q"foo(a, b) = bar"
    assert(expr1.structure === "Term.Name(\"foo\")")
    assert(exprs.toString === "List(List(a, b))")
    assert(exprs(0)(0).structure === "Term.Name(\"a\")")
    assert(exprs(0)(1).structure === "Term.Name(\"b\")")
    assert(expr2.structure === "Term.Name(\"bar\")")
  }

  test("2 q\"expr(...exprs) = expr\"") {
    val expr1 = q"foo"
    val exprs = List(List(q"a", q"b"))
    val expr2 = q"bar"
    assert(q"$expr1(...$exprs) = $expr2".structure === "Term.Assign(Term.Apply(Term.Name(\"foo\"), List(Term.Name(\"a\"), Term.Name(\"b\"))), Term.Name(\"bar\"))")
  }

  test("1 q\"(x, y: Int)\"") {
    val q"($x, y: Int)" = q"(x: X, y: Int)"
    assert(x.structure === "Term.Ascribe(Term.Name(\"x\"), Type.Name(\"X\"))")
  }

  test("2 q\"(x, y: Int)\"") {
    val x = q"x: X"
    assert(q"($x, y: Int)".structure === "Term.Tuple(List(Term.Ascribe(Term.Name(\"x\"), Type.Name(\"X\")), Term.Ascribe(Term.Name(\"y\"), Type.Name(\"Int\"))))")
  }

  test("1 q\"f(q, y: Y)") {
    val q"f($q, y: Y) = $r" = q"f(x: X, y: Y) = 1"
    assert(q.structure === "Term.Ascribe(Term.Name(\"x\"), Type.Name(\"X\"))")
    assert(r.structure === "Lit.Int(1)")
  }

  test("2 q\"f(q, y: Y)") {
    val q = q"x: X"
    val r = q"1"
    assert(q"f($q, y: Y) = $r".structure === "Term.Assign(Term.Apply(Term.Name(\"f\"), List(Term.Ascribe(Term.Name(\"x\"), Type.Name(\"X\")), Term.Ascribe(Term.Name(\"y\"), Type.Name(\"Y\")))), Lit.Int(1))")
  }

  test("1 q\"return expr\"") {
    val q"return $expr" = q"return foo == bar"
    assert(expr.structure === "Term.ApplyInfix(Term.Name(\"foo\"), Term.Name(\"==\"), Nil, List(Term.Name(\"bar\")))")
  }

  test("2 q\"return expr\"") {
    val expr = q"foo == bar"
    assert(q"return $expr".structure === "Term.Return(Term.ApplyInfix(Term.Name(\"foo\"), Term.Name(\"==\"), Nil, List(Term.Name(\"bar\"))))")
  }

  test("1 q\"throw expr\"") {
    val q"throw $expr" = q"throw new RuntimeException"
    assert(expr.structure === "Term.New(Init(Type.Name(\"RuntimeException\"), Name(\"\"), Nil))")
  }

  test("2 q\"throw expr\"") {
    val expr = q"new RuntimeException"
    assert(q"throw $expr".structure === "Term.Throw(Term.New(Init(Type.Name(\"RuntimeException\"), Name(\"\"), Nil)))")
  }

  test("1 q\"expr: tpe\"") {
    val q"$exp: $tpe" = q"1: Double"
    assert(exp.structure === "Lit.Int(1)")
    assert(tpe.structure === "Type.Name(\"Double\")")
  }

  test("2 q\"expr: tpe\"") {
    val exp = q"1"
    val tpe = t"Double"
    assert(q"$exp: $tpe".structure === "Term.Ascribe(Lit.Int(1), Type.Name(\"Double\"))")
  }

  test("1 q\"expr: ..@annots\"") {
    val q"$exprr: @q ..@$annotz @$ar" = q"foo: @q @w @e @r"
    assert(exprr.structure === "Term.Name(\"foo\")")
    assert(annotz.toString === "List(@w, @e)")
    assert(annotz(0).structure === "Mod.Annot(Init(Type.Name(\"w\"), Name(\"\"), Nil))")
    assert(annotz(1).structure === "Mod.Annot(Init(Type.Name(\"e\"), Name(\"\"), Nil))")
    assert(ar.structure === "Mod.Annot(Init(Type.Name(\"r\"), Name(\"\"), Nil))")
  }

  test("2 q\"expr: ..@annots\"") {
    val mods = List(mod"@w", mod"@e")
    assert(q"foo: @q ..@$mods @r".structure === "Term.Annotate(Term.Name(\"foo\"), List(Mod.Annot(Init(Type.Name(\"q\"), Name(\"\"), Nil)), Mod.Annot(Init(Type.Name(\"w\"), Name(\"\"), Nil)), Mod.Annot(Init(Type.Name(\"e\"), Name(\"\"), Nil)), Mod.Annot(Init(Type.Name(\"r\"), Name(\"\"), Nil))))")
  }

  test("q\"(..exprs)\"") {
    val q"(..$terms)" = q"(y, z)"
    assert(terms.toString === "List(y, z)")
    assert(terms(0).structure === "Term.Name(\"y\")")
    assert(terms(1).structure === "Term.Name(\"z\")")
  }

  test("2 q\"(..exprs)\"") {
    val terms = List(q"y", q"z")
    assert(q"(..$terms)".structure === "Term.Tuple(List(Term.Name(\"y\"), Term.Name(\"z\")))")
  }

  test("1 val q\"(..params)\" = q\"(x: Int, y: String)\"") {
    val q"(..$params)" = q"(x: Int, y: String)"
    assert(params.toString === "List(x: Int, y: String)")
    assert(params(0).structure === "Term.Ascribe(Term.Name(\"x\"), Type.Name(\"Int\"))")
    assert(params(1).structure === "Term.Ascribe(Term.Name(\"y\"), Type.Name(\"String\"))")
  }

  test("2 val q\"(..params)\" = q\"(x: Int, y: String)\"") {
    val params = List(q"x: Int", q"y: String")
    assert(q"(..$params)".structure === "Term.Tuple(List(Term.Ascribe(Term.Name(\"x\"), Type.Name(\"Int\")), Term.Ascribe(Term.Name(\"y\"), Type.Name(\"String\"))))")
  }

  test("1 q\"{ ..stats }\"") {
    val q"{foo; ..$statz; $astat}" = q"{foo; val a = x; val b = y; val c = z}"
    assert(statz.toString === "List(val a = x, val b = y)")
    assert(statz(0).structure === "Defn.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), None, Term.Name(\"x\"))")
    assert(statz(1).structure === "Defn.Val(Nil, List(Pat.Var(Term.Name(\"b\"))), None, Term.Name(\"y\"))")
    assert(astat.structure === "Defn.Val(Nil, List(Pat.Var(Term.Name(\"c\"))), None, Term.Name(\"z\"))")
  }

  test("2 q\"{ ..stats }\"") {
    val stats = List(q"val x = 1", q"val y = 2")
    assert(q"{ ..$stats }".structure === "Term.Block(List(Defn.Val(Nil, List(Pat.Var(Term.Name(\"x\"))), None, Lit.Int(1)), Defn.Val(Nil, List(Pat.Var(Term.Name(\"y\"))), None, Lit.Int(2))))")
  }

  test("1 q\"if (expr) expr else expr\"") {
    val q"if ($expr1) $expr2 else $expr3" = q"if (1 > 2) a else b"
    assert(expr1.structure === "Term.ApplyInfix(Lit.Int(1), Term.Name(\">\"), Nil, List(Lit.Int(2)))")
    assert(expr2.structure === "Term.Name(\"a\")")
    assert(expr3.structure === "Term.Name(\"b\")")
  }

  test("2 q\"if (expr) expr else expr\"") {
    val expr1 = q"1 > 2"
    val expr2 = q"a"
    val expr3 = q"b"
    assert(q"if ($expr1) $expr2 else $expr3".structure === "Term.If(Term.ApplyInfix(Lit.Int(1), Term.Name(\">\"), Nil, List(Lit.Int(2))), Term.Name(\"a\"), Term.Name(\"b\"))")
  }

  test("1 q\"expr match { ..case cases }\"") {
    val q"$expr match { case bar => baz; ..case $casez; case q => w}" = q"foo match { case bar => baz; case _ => foo ; case q => w }"
    assert(expr.structure === "Term.Name(\"foo\")")
    assert(casez.toString === "List(case _ => foo)")
    assert(casez(0).structure === "Case(Pat.Wildcard(), None, Term.Name(\"foo\"))")
  }

  test("2 q\"expr match { ..case cases }\"") {
    val q"$expr match { case bar => baz; ..case $casez; case _ => foo }" = q"foo match { case bar => baz; case _ => foo }"
    assert(expr.structure === "Term.Name(\"foo\")")
    assert(casez.isEmpty)
  }

  test("3 q\"expr match { ..case cases }\"") {
    val q"$expr match { ..case $casez }" = q"foo match { case bar => baz; case _ => foo }"
    assert(expr.structure === "Term.Name(\"foo\")")
    assert(casez.toString === "List(case bar => baz, case _ => foo)")
    assert(casez(0).structure === "Case(Pat.Var(Term.Name(\"bar\")), None, Term.Name(\"baz\"))")
    assert(casez(1).structure === "Case(Pat.Wildcard(), None, Term.Name(\"foo\"))")
  }

  test("4 q\"expr match { ..case cases }\"") {
    val expr = q"foo"
    val casez = List(p"case a => b", p"case q => w")
    assert(q"$expr match { ..case $casez }".structure === "Term.Match(Term.Name(\"foo\"), List(Case(Pat.Var(Term.Name(\"a\")), None, Term.Name(\"b\")), Case(Pat.Var(Term.Name(\"q\")), None, Term.Name(\"w\"))))")
  }

  test("1 q\"try expr catch { ..case cases } finally expropt\"") {
    val q"try $expr catch { case $case1 ..case $cases; case $case2 } finally $expropt" = q"try foo catch { case a => b; case _ => bar; case 1 => 2; case q => w} finally baz"
    assert(expr.structure === "Term.Name(\"foo\")")
    assert(cases.toString === "List(case _ => bar, case 1 => 2)")
    assert(cases(0).structure === "Case(Pat.Wildcard(), None, Term.Name(\"bar\"))")
    assert(cases(1).structure === "Case(Lit.Int(1), None, Lit.Int(2))")
    assert(case1.structure === "Case(Pat.Var(Term.Name(\"a\")), None, Term.Name(\"b\"))")
    assert(case2.structure === "Case(Pat.Var(Term.Name(\"q\")), None, Term.Name(\"w\"))")
    assert(expropt.structure === "Some(Term.Name(\"baz\"))")
  }

  test("2 q\"try expr catch { ..case cases } finally expropt\"") {
    val expr = q"foo"
    val cases = List(p"case _ => bar", p"case 1 => 2")
    val case1 = p"case a => b"
    val case2 = p"case q => w"
    val expropt = q"baz"
    assert(q"try $expr catch { case $case1 ..case $cases; case $case2 } finally $expropt".structure === "Term.Try(Term.Name(\"foo\"), List(Case(Pat.Var(Term.Name(\"a\")), None, Term.Name(\"b\")), Case(Pat.Wildcard(), None, Term.Name(\"bar\")), Case(Lit.Int(1), None, Lit.Int(2)), Case(Pat.Var(Term.Name(\"q\")), None, Term.Name(\"w\"))), Some(Term.Name(\"baz\")))")
  }

  test("1 q\"try expr catch expr finally expropt\"") {
    val q"try $expr catch $exprr finally $expropt" = q"try { foo } catch { pf } finally { bar }"
    assert(expr.structure === "Term.Block(List(Term.Name(\"foo\")))")
    assert(exprr.structure === "Term.Name(\"pf\")")
    assert(expropt.structure === "Some(Term.Block(List(Term.Name(\"bar\"))))")
  }

  test("2 q\"try expr catch expr finally expropt\"") {
    val expr = q"{ foo }"
    val exprr = q"pf"
    val expropt = q"{ bar }"
    assert(q"try $expr catch $exprr finally $expropt".structure === "Term.TryWithHandler(Term.Block(List(Term.Name(\"foo\"))), Term.Name(\"pf\"), Some(Term.Block(List(Term.Name(\"bar\")))))")
  }

  test("q\"(i: Int) => 42\"") {
    assert(q"(i: Int) => 42".structure === "Term.Function(List(Term.Param(Nil, Term.Name(\"i\"), Some(Type.Name(\"Int\")), None)), Lit.Int(42))")
  }

  test("1 q\"(..params) => expr\"") {
    val q"(..$paramz) => $expr" = q"(x: Int, y: String) => 42"
    assert(paramz.toString === "List(x: Int, y: String)")
    assert(paramz(0).structure === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Int\")), None)")
    assert(paramz(1).structure === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"String\")), None)")
    assert(expr.structure === "Lit.Int(42)")
  }

  test("2 q\"(..params) => expr\"") {
    val paramz = List(param"x: Int", param"y: String")
    val expr = q"42"
    assert(q"(..$paramz) => $expr".structure === "Term.Function(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Int\")), None), Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"String\")), None)), Lit.Int(42))")
  }

  test("1 val q\"(..q, y: Y, e) => r\" = q\"(x: X, y: Y, z: Z) => 1\"") {
    val q"(..$q, y: Y, $e) => $r" = q"(x: X, y: Y, z: Z) => 1"
    assert(q.toString === "List(x: X)")
    assert(q(0).structure === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(e.structure === "Term.Param(Nil, Term.Name(\"z\"), Some(Type.Name(\"Z\")), None)")
    assert(r.structure === "Lit.Int(1)")
  }

  test("2 val q\"(..q, y: Y, e) => r\" = q\"(x: X, y: Y, z: Z) => 1\"") {
    val q = List(param"x: X")
    val e = param"z: Z"
    val r = q"1"
    assert(q"(..$q, y: Y, $e) => $r".structure === "Term.Function(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None), Term.Param(Nil, Term.Name(\"z\"), Some(Type.Name(\"Z\")), None)), Lit.Int(1))")
  }

  test("1 q\"{ ..case cases }\"") {
    val q"{ ..case $cases }" = q"{ case i: Int => i + 1 }"
    assert(cases(0).structure === "Case(Pat.Typed(Pat.Var(Term.Name(\"i\")), Type.Name(\"Int\")), None, Term.ApplyInfix(Term.Name(\"i\"), Term.Name(\"+\"), Nil, List(Lit.Int(1))))")
  }

  test("2 q\"{ ..case cases }\"") {
    val cases = List(p"case i: Int => i + 1")
    assert(q"{ ..case $cases }".structure === "Term.PartialFunction(List(Case(Pat.Typed(Pat.Var(Term.Name(\"i\")), Type.Name(\"Int\")), None, Term.ApplyInfix(Term.Name(\"i\"), Term.Name(\"+\"), Nil, List(Lit.Int(1))))))")
  }

  test("1 q\"while (expr) expr\"") {
    val q"while ($expr1) $expr2" = q"while (foo) bar"
    assert(expr1.structure === "Term.Name(\"foo\")")
    assert(expr2.structure === "Term.Name(\"bar\")")
  }

  test("2 q\"while (expr) expr\"") {
    val expr1 = q"foo"
    val expr2 = q"bar"
    assert(q"while ($expr1) $expr2".structure === "Term.While(Term.Name(\"foo\"), Term.Name(\"bar\"))")
  }

  test("1 q\"do expr while(expr)\"") {
    val q"do $expr1 while($expr2)" = q"do foo while (bar)"
    assert(expr1.structure === "Term.Name(\"foo\")")
    assert(expr2.structure === "Term.Name(\"bar\")")
  }

  test("2 q\"do expr while(expr)\"") {
    val expr1 = q"foo"
    val expr2 = q"bar"
    assert(q"do $expr1 while($expr2)".structure === "Term.Do(Term.Name(\"foo\"), Term.Name(\"bar\"))")
  }

  test("1 q\"for (..enumerators) expr\"") {
    val q"for ($enum1; ..$enumerators; if $cond; $enum2) $exprr" = q"for (a <- as; x <- xs; y <- ys; if bar; b <- bs) foo(x, y)"
    assert(enumerators.toString === "List(x <- xs, y <- ys)")
    assert(enumerators(0).structure === "Enumerator.Generator(Pat.Var(Term.Name(\"x\")), Term.Name(\"xs\"))")
    assert(enumerators(1).structure === "Enumerator.Generator(Pat.Var(Term.Name(\"y\")), Term.Name(\"ys\"))")
    assert(cond.structure === "Term.Name(\"bar\")")
    assert(enum1.structure === "Enumerator.Generator(Pat.Var(Term.Name(\"a\")), Term.Name(\"as\"))")
    assert(enum2.structure === "Enumerator.Generator(Pat.Var(Term.Name(\"b\")), Term.Name(\"bs\"))")
    assert(exprr.structure === "Term.Apply(Term.Name(\"foo\"), List(Term.Name(\"x\"), Term.Name(\"y\")))")
  }

  test("2 q\"for (..enumerators) expr\"") {
    val a = enumerator"a <- as"
    val b = enumerator"b <- bs"
    val ab = List(a,b)
    assert(q"for (..$ab) foo".structure === "Term.For(List(Enumerator.Generator(Pat.Var(Term.Name(\"a\")), Term.Name(\"as\")), Enumerator.Generator(Pat.Var(Term.Name(\"b\")), Term.Name(\"bs\"))), Term.Name(\"foo\"))")
  }

  test("1 q\"for (..enumerators) yield expr\"") {
    val q"for (a <- as; ..$enumerators; b <- bs) yield $expr" = q"for (a <- as; x <- xs; y <- ys; b <- bs) yield foo(x, y)"
    assert(enumerators.toString === "List(x <- xs, y <- ys)")
    assert(enumerators(0).structure === "Enumerator.Generator(Pat.Var(Term.Name(\"x\")), Term.Name(\"xs\"))")
    assert(enumerators(1).structure === "Enumerator.Generator(Pat.Var(Term.Name(\"y\")), Term.Name(\"ys\"))")
    assert(expr.structure === "Term.Apply(Term.Name(\"foo\"), List(Term.Name(\"x\"), Term.Name(\"y\")))")
  }

  test("2 q\"for (..enumerators) yield expr\"") {
    val a = enumerator"a <- as"
    val b = enumerator"b <- bs"
    val ab = List(a,b)
    assert(q"for (..$ab) yield foo".structure === "Term.ForYield(List(Enumerator.Generator(Pat.Var(Term.Name(\"a\")), Term.Name(\"as\")), Enumerator.Generator(Pat.Var(Term.Name(\"b\")), Term.Name(\"bs\"))), Term.Name(\"foo\"))")
  }

  test("1 q\"new { ..stat } with ..inits { self => ..stats }\"") {
    val q"new $x" = q"new Foo"
    assert(x.structure === "Init(Type.Name(\"Foo\"), Name(\"\"), Nil)")
  }

  test("2 q\"new { ..stat } with ..inits { self => ..stats }\"") {
    val q"new {..$stats; val b = 4} with $a {$self => ..$statz}" = q"new {val a = 2; val b = 4} with A { self => val b = 3 }"
    assert(stats.toString === "List(val a = 2)")
    assert(stats(0).structure === "Defn.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), None, Lit.Int(2))")
    assert(a.structure === "Init(Type.Name(\"A\"), Name(\"\"), Nil)")
    assert(self.structure === "Self(Term.Name(\"self\"), None)")
    assert(statz.toString === "List(val b = 3)")
    assert(statz(0).structure === "Defn.Val(Nil, List(Pat.Var(Term.Name(\"b\"))), None, Lit.Int(3))")
  }

  test("3 q\"new { ..stat } with ..inits { self => ..stats }\"") {
    val q"new X with T { $self => def m = 42}" = q"new X with T { def m = 42 }"
    assert(self.structure === "Self(Name(\"\"), None)")
  }

  test("4 q\"new { ..stat } with ..inits { self => ..stats }\"") {
    val stats = List(q"val a = 2")
    val a = init"A"
    val self = self"self: A"
    val statz = List(q"val b = 3")
    assert(q"new {..$stats; val b = 4} with $a {$self => ..$statz}".structure === "Term.NewAnonymous(Template(List(Defn.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), None, Lit.Int(2)), Defn.Val(Nil, List(Pat.Var(Term.Name(\"b\"))), None, Lit.Int(4))), List(Init(Type.Name(\"A\"), Name(\"\"), Nil)), Self(Term.Name(\"self\"), Some(Type.Name(\"A\"))), List(Defn.Val(Nil, List(Pat.Var(Term.Name(\"b\"))), None, Lit.Int(3)))))")
  }

  test("q\"_\"") {
    assert(q"_".structure === "Term.Placeholder()")
  }

  test("1 q\"expr _\"") {
    val q"$expr _" = q"foo _"
    assert(expr.structure === "Term.Name(\"foo\")")
  }

  test("2 q\"expr _\"") {
    val expr = q"foo"
    assert(q"$expr _".structure === "Term.Eta(Term.Name(\"foo\"))")
  }

  test("1 q\"expr: _*\"") {
    val q"$expr: _*" = q"foo: _*"
    assert(expr.structure === "Term.Name(\"foo\")")
  }

  test("2 arg\"expr: _*\"") {
    val expr = q"foo"
    assert(q"$expr: _*".structure === "Term.Repeated(Term.Name(\"foo\"))")
  }

  test("1 q\"lit\"") {
    val q"$x" = q"42"
    assert(x.structure === "Lit.Int(42)")
  }

  test("2 q\"lit\"") {
    val lit = q"42"
    assert(q"$lit".structure === "Lit.Int(42)")
  }

  test("1 t\"ref.tname\"") {
    val t"$ref.$tname" = t"X.Y"
    assert(ref.structure === "Term.Name(\"X\")")
    assert(tname.structure === "Type.Name(\"Y\")")
  }

  test("2 t\"ref.tname\"") {
    val ref = q"X"
    val tname = t"Y"
    assert(t"$ref.$tname".structure === "Type.Select(Term.Name(\"X\"), Type.Name(\"Y\"))")
  }

  test("1 t\"tpe#tname\"") {
    val t"$tpe#$tname" = t"X#Y"
    assert(tpe.structure === "Type.Name(\"X\")")
    assert(tname.structure === "Type.Name(\"Y\")")
  }

  test("2 t\"tpe#tname\"") {
    val tpe = t"X"
    val tname = t"Y"
    assert(t"$tpe#$tname".structure === "Type.Project(Type.Name(\"X\"), Type.Name(\"Y\"))")
  }

  test("1 t\"ref.type\"") {
    val t"$ref.type" = t"X.type"
    assert(ref.structure === "Term.Name(\"X\")")
  }

  test("2 t\"ref.type\"") {
    val ref = q"X"
    assert(t"$ref.type".structure === "Type.Singleton(Term.Name(\"X\"))")
  }
  /*
   Issue #462
   */
  test("3 t\"ref.type\"") {
    val ref = q"X.a"
    assert(t"$ref.type".structure === "Type.Singleton(Term.Select(Term.Name(\"X\"), Term.Name(\"a\")))")
  }

  test("1 t\"tpe[..tpes]") {
    val t"$tpe[..$tpes]" = t"X[Y, Z]"
    assert(tpe.structure === "Type.Name(\"X\")")
    assert(tpes.toString === "List(Y, Z)")
    assert(tpes(0).structure === "Type.Name(\"Y\")")
    assert(tpes(1).structure === "Type.Name(\"Z\")")
  }

  test("2 t\"tpe[..tpes]") {
    val tpe = t"X"
    val tpes = List(t"Y", t"Z")
    assert(t"$tpe[..$tpes]".structure === "Type.Apply(Type.Name(\"X\"), List(Type.Name(\"Y\"), Type.Name(\"Z\")))")
  }

  test("1 t\"tpe tname tpe\"") {
    val t"$tpe1 $tname $tpe2" = t"X Y Z"
    assert(tpe1.structure === "Type.Name(\"X\")")
    assert(tname.structure === "Type.Name(\"Y\")")
    assert(tpe2.structure === "Type.Name(\"Z\")")
  }

  test("2 t\"tpe tname tpe\"") {
    val tpe1 = t"X"
    val tname = t"Y"
    val tpe2 = t"Z"
    assert(t"$tpe1 $tname $tpe2".structure === "Type.ApplyInfix(Type.Name(\"X\"), Type.Name(\"Y\"), Type.Name(\"Z\"))")
  }

  test("1 t\"(..tpes) => tpe\"") {
    val t"(..$tpes) => $tpe" = t"(X, Y) => Z"
    assert(tpes.toString === "List(X, Y)")
    assert(tpes(0).structure === "Type.Name(\"X\")")
    assert(tpes(1).structure === "Type.Name(\"Y\")")
    assert(tpe.structure === "Type.Name(\"Z\")")
  }

  test("2 t\"(..tpes) => tpe\"") {
    val tpes: List[Type] = List(t"X", t"Y")
    val tpe = t"Z"
    assert(t"(..$tpes) => $tpe".structure === "Type.Function(List(Type.Name(\"X\"), Type.Name(\"Y\")), Type.Name(\"Z\"))")
  }

  test("1 t\"implicit (..tpes) => tpe\"") {
    val Scala211 = "shadow scala.meta.dialects.Scala211"
    import scala.meta.dialects.Dotty
    val t"implicit (..$tpes) => $tpe" = t"implicit (X, Y) => Z"
    assert(tpes.toString === "List(X, Y)")
    assert(tpes(0).show[Structure] === "Type.Name(\"X\")")
    assert(tpes(1).show[Structure] === "Type.Name(\"Y\")")
    assert(tpe.show[Structure] === "Type.Name(\"Z\")")
  }

  test("2 t\"implicit (..tpes) => tpe\"") {
    val Scala211 = "shadow scala.meta.dialects.Scala211"
    import scala.meta.dialects.Dotty
    val tpes: List[Type] = List(t"X", t"Y")
    val tpe = t"Z"
    assert(t"implicit (..$tpes) => $tpe".show[Structure] === "Type.ImplicitFunction(List(Type.Name(\"X\"), Type.Name(\"Y\")), Type.Name(\"Z\"))")
  }

  test("1 t\"(..tpes)\"") {
    val t"(..$tpes)" = t"(X, Y)"
    assert(tpes.toString === "List(X, Y)")
    assert(tpes(0).structure === "Type.Name(\"X\")")
    assert(tpes(1).structure === "Type.Name(\"Y\")")
  }

  test("t\"(..tpes)\"") {
    val tpes = List(t"X", t"Y")
    assert(t"(..$tpes)".structure === "Type.Tuple(List(Type.Name(\"X\"), Type.Name(\"Y\")))")
  }

  test("1 t\"tpe { ..stats }\"") {
    val t"$tpe {..$stats}" = t"A with B with C { val a: A; val b: B }"
    assert(tpe.toString === "Some(A with B with C)")
    assert(tpe.structure === "Some(Type.With(Type.With(Type.Name(\"A\"), Type.Name(\"B\")), Type.Name(\"C\")))")
    assert(stats.toString === "List(val a: A, val b: B)")
    assert(stats(0).structure === "Decl.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), Type.Name(\"A\"))")
    assert(stats(1).structure === "Decl.Val(Nil, List(Pat.Var(Term.Name(\"b\"))), Type.Name(\"B\"))")
  }

  test("2 t\"tpe { ..stats }\"") {
    val tpe = t"X with Y"
    val stats = List(q"val a: A", q"val b: B")
    assert(t"$tpe { ..$stats }".structure === "Type.Refine(Some(Type.With(Type.Name(\"X\"), Type.Name(\"Y\"))), List(Decl.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), Type.Name(\"A\")), Decl.Val(Nil, List(Pat.Var(Term.Name(\"b\"))), Type.Name(\"B\"))))")
  }

  test("1 t\"tpe forSome { ..stats }\"") {
    val t"$tpe forSome { ..$stats }" = t"X forSome { val a: A; val b: B }"
    assert(tpe.structure === "Type.Name(\"X\")")
    assert(stats.toString === "List(val a: A, val b: B)")
    assert(stats(0).structure === "Decl.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), Type.Name(\"A\"))")
    assert(stats(1).structure === "Decl.Val(Nil, List(Pat.Var(Term.Name(\"b\"))), Type.Name(\"B\"))")
  }

  test("2 t\"tpe forSome { ..stats }\"") {
    val tpe = t"X"
    val stats = List(q"val a:A", q"val b:B")
    assert(t"$tpe forSome { ..$stats }".structure === "Type.Existential(Type.Name(\"X\"), List(Decl.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), Type.Name(\"A\")), Decl.Val(Nil, List(Pat.Var(Term.Name(\"b\"))), Type.Name(\"B\"))))")
  }

  test("1 t\"tpe ..@annots\"") {
    val t"$tpe ..@$annots" = t"X @a @b"
    assert(tpe.structure === "Type.Name(\"X\")")
    assert(annots.toString === "List(@a, @b)")
    assert(annots(0).structure === "Mod.Annot(Init(Type.Name(\"a\"), Name(\"\"), Nil))")
    assert(annots(1).structure === "Mod.Annot(Init(Type.Name(\"b\"), Name(\"\"), Nil))")
  }

  test("2 t\"tpe ..@annots\"") {
    val tpe = t"X"
    val annots = List(mod"@a", mod"@b")
    assert(t"$tpe ..@$annots".structure === "Type.Annotate(Type.Name(\"X\"), List(Mod.Annot(Init(Type.Name(\"a\"), Name(\"\"), Nil)), Mod.Annot(Init(Type.Name(\"b\"), Name(\"\"), Nil))))")
  }

  test("1 t\"[..tparams] => tpe\"") {
    val t"[..$tparams] => $tpe" = t"[T] => (T, T)"
    assert(tparams.toString === "List(T)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tpe.toString === "(T, T)")
  }

  test("2 t\"(..tparams) => tpe\"") {
    val tparams = List(tparam"T")
    val tpe = t"(T, T)"
    assert(t"[..$tparams] => $tpe".structure === "Type.Lambda(List(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Tuple(List(Type.Name(\"T\"), Type.Name(\"T\"))))")
  }

  test("1 t\"(...paramss): tpe\"") {
    val t"(...$paramss): $tpe" = t"(x: X): x.T"
    assert(paramss.toString === "List(List(x: X))")
    assert(tpe.toString === "x.T")
  }

  test("2 t\"(...paramss): tpe\"") {
    val paramss = List(List(param"x: X"))
    val tpe = t"x.T"
    assert(t"(...$paramss): $tpe".structure === "Type.Method(List(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None))), Type.Select(Term.Name(\"x\"), Type.Name(\"T\")))")
  }

  test("1 t\"_ >: tpeopt <: tpeopt\"") {
    val t"_ >: $tpe1 <: $tpe2" = t"_ >: X <: Y"
    assert(tpe1.structure === "Some(Type.Name(\"X\"))")
    assert(tpe2.structure === "Some(Type.Name(\"Y\"))")
  }

  test("2 t\"_ >: tpeopt <: tpeopt\"") {
    val tpe1 = t"X"
    val tpe2 = t"Y"
    assert(t"_ >: $tpe1 <: $tpe2".structure === "Type.Placeholder(Type.Bounds(Some(Type.Name(\"X\")), Some(Type.Name(\"Y\"))))")
  }

  test("1 t\"=> tpe\"") {
    val t"=> $tpe" = t"=> X"
    assert(tpe.structure === "Type.Name(\"X\")")
  }

  test("2 t\"=> tpe\"") {
    val tpe = t"X"
    assert(t"=> $tpe".structure === "Type.ByName(Type.Name(\"X\"))")
  }

  test("1 t\"tpe *\"") {
    val t"$tpe*" = t"X*"
    assert(tpe.structure === "Type.Name(\"X\")")
  }

  test("2 t\"tpe *\"") {
    val tpe = t"X"
    assert(t"$tpe*".structure === "Type.Repeated(Type.Name(\"X\"))")
  }

  test("t\"lit\"") {
    val lit = q"1"
    assert(t"$lit".structure === "Lit.Int(1)")
  }

  test("p\"_\"") {
    assert(p"_".structure === "Pat.Wildcard()")
  }

  test("p\"name\"") {
    assert(p"name".structure === "Pat.Var(Term.Name(\"name\"))")
  }

  test("p\"x\"") {
    assert(p"x".structure === "Pat.Var(Term.Name(\"x\"))")
  }

  test("p\"X\"") {
    assert(p"X".structure ===  "Pat.Var(Term.Name(\"X\"))")
  }

  test("p\"`x`\"") {
    assert(p"`x`".structure === "Term.Name(\"x\")")
  }

  test("p\"`X`\"") {
    assert(p"`X`".structure === "Term.Name(\"X\")")
  }

  test("1 p\"pat @ pat\"") {
    val p"$pat1 @ $pat2" = p"x @ y"
    assert(pat1.structure === "Pat.Var(Term.Name(\"x\"))")
    assert(pat2.structure === "Pat.Var(Term.Name(\"y\"))")
  }

  test("2 p\"pat1 @ pat\"") {
    val pat1 = p"x"
    val pat2 = p"y"
    assert(p"$pat1 @ $pat2".structure === "Pat.Bind(Pat.Var(Term.Name(\"x\")), Pat.Var(Term.Name(\"y\")))")
  }

  test("1 p\"pat | pat\"") {
    val p"$pat1 | $pat2" = p"x | y"
    assert(pat1.structure === "Pat.Var(Term.Name(\"x\"))")
    assert(pat2.structure === "Pat.Var(Term.Name(\"y\"))")
  }

  test("2 p\"pat | pat\"") {
    val pat1 = q"X"
    val pat2 = q"Y"
    assert(p"$pat1 | $pat2".structure === "Pat.Alternative(Term.Name(\"X\"), Term.Name(\"Y\"))")
  }

  test("3 p\"pat | pat\"") {
    val pat1 = p"`X`"
    val pat2 = q"Y"
    assert(p"$pat1 | $pat2".structure === "Pat.Alternative(Term.Name(\"X\"), Term.Name(\"Y\"))")
  }

  test("1 p\"(..pats)\"") {
    val p"(..$pats)" = p"(X, Y)"
    assert(pats.toString === "List(X, Y)")
    assert(pats(0).structure === "Term.Name(\"X\")")
    assert(pats(1).structure === "Term.Name(\"Y\")")
  }

  test("2 p\"(..pats)\"") {
    val pats = List(p"x", p"y")
    assert(p"(..$pats)".structure === "Pat.Tuple(List(Pat.Var(Term.Name(\"x\")), Pat.Var(Term.Name(\"y\"))))")
  }

  test("3 p\"(..pats)\"") {
    val pats = List(p"`X`", q"Y")
    assert(p"(..$pats)".structure === "Pat.Tuple(List(Term.Name(\"X\"), Term.Name(\"Y\")))")
  }

  test("1 p\"expr(..pats)\"") {
    val p"$expr(..$pats)" = p"x[A, B](Q, W)"
    assert(expr.structure === "Term.ApplyType(Term.Name(\"x\"), List(Type.Name(\"A\"), Type.Name(\"B\")))")
    assert(pats.toString === "List(Q, W)")
    assert(pats(0).structure === "Term.Name(\"Q\")")
    assert(pats(1).structure === "Term.Name(\"W\")")
  }

  test("2 p\"expr(..pats)\"") {
    val p"$expr(..$pats)" = p"x(Q, W)"
    assert(expr.structure === "Term.Name(\"x\")")
    assert(pats.toString === "List(Q, W)")
    assert(pats(0).structure === "Term.Name(\"Q\")")
    assert(pats(1).structure === "Term.Name(\"W\")")
  }

  test("3 p\"expr(..pats)\"") {
    val ref = q"x"
    val tpes = List(t"A", t"B")
    val pats = List(q"Q", q"W")
    assert(p"$ref[..$tpes](..$pats)".structure === "Pat.Extract(Term.ApplyType(Term.Name(\"x\"), List(Type.Name(\"A\"), Type.Name(\"B\"))), List(Term.Name(\"Q\"), Term.Name(\"W\")))")
  }

  test("4 p\"expr(..pats)\"") {
    val ref = q"`x`"
    val tpes = List(t"`A`", t"B")
    val pats = List(p"`Q`", q"W")
    assert(p"$ref[..$tpes](..$pats)".structure === "Pat.Extract(Term.ApplyType(Term.Name(\"x\"), List(Type.Name(\"A\"), Type.Name(\"B\"))), List(Term.Name(\"Q\"), Term.Name(\"W\")))")
  }

  /*
   Issue #462
   */
  test("5 p\"expr(..pats)\"") {
    val ref = q"x.a"
    val tpes = List(t"A", t"B")
    val pats = List(q"Q", q"W")
    assert(p"$ref[..$tpes](..$pats)".structure === "Pat.Extract(Term.ApplyType(Term.Select(Term.Name(\"x\"), Term.Name(\"a\")), List(Type.Name(\"A\"), Type.Name(\"B\"))), List(Term.Name(\"Q\"), Term.Name(\"W\")))")
  }

  test("1 p\"pat name (..pats)\"") {
    val p"$pat $name (..$pats)" = p"x y (Q, W)"
    assert(pat.structure === "Pat.Var(Term.Name(\"x\"))")
    assert(name.structure === "Term.Name(\"y\")")
    assert(pats.toString === "List(Q, W)")
    assert(pats(0).structure === "Term.Name(\"Q\")")
    assert(pats(1).structure === "Term.Name(\"W\")")
  }

  test("2 p\"pat name (..pats)\"") {
    val pat = p"x"
    val name = q"y"
    val pats = List(q"Q", q"W")
    assert(p"$pat $name (..$pats)".structure === "Pat.ExtractInfix(Pat.Var(Term.Name(\"x\")), Term.Name(\"y\"), List(Term.Name(\"Q\"), Term.Name(\"W\")))")
  }

  test("3 p\"pat name (..pats)\"") {
    val pat = p"`x`"
    val name = q"y"
    val pats = List(q"Q", q"W")
    assert(p"$pat $name (..$pats)".structure === "Pat.ExtractInfix(Term.Name(\"x\"), Term.Name(\"y\"), List(Term.Name(\"Q\"), Term.Name(\"W\")))")
  }

  test("1 p\"pat: ptpe\"") {
    val p"$pat: $ptpe" = p"x: Y"
    assert(pat.structure === "Pat.Var(Term.Name(\"x\"))")
    assert(ptpe.structure === "Type.Name(\"Y\")")
  }

  test("2 p\"pat: ptpe\"") {
    val pat = p"x"
    val ptpe = t"Y"
    assert(p"$pat: $ptpe".structure === "Pat.Typed(Pat.Var(Term.Name(\"x\")), Type.Name(\"Y\"))")
  }

  test("1 p\"expr.name\"") {
    val p"$expr.$name" = p"x.y"
    assert(expr.structure === "Term.Name(\"x\")")
    assert(name.structure === "Term.Name(\"y\")")
  }

  test("2 p\"expr.name\"") {
    val expr = q"x"
    val name = q"y"
    assert(p"$expr.$name".structure === "Term.Select(Term.Name(\"x\"), Term.Name(\"y\"))")
  }

  test("3 p\"expr.name\"") {
    val expr = q"`x`"
    val name = q"y"
    assert(p"$expr.$name".structure === "Term.Select(Term.Name(\"x\"), Term.Name(\"y\"))")
  }

  test("p\"lit\"") {
    val lit = q"1"
    assert(p"$lit".structure === "Lit.Int(1)")
  }

  test("1 p\"case pat if expropt => expr\"") {
    val p"case $pat if $expropt => $expr" = p"case X if foo => bar"
    assert(pat.structure === "Term.Name(\"X\")")
    assert(expropt.structure === "Some(Term.Name(\"foo\"))")
    assert(expr.structure === "Term.Name(\"bar\")")
  }

  test("2 p\"case pat if expropt => expr\"") {
    val pat = q"X"
    val expropt = q"foo"
    val expr = q"bar"
    assert(p"case $pat if $expropt => $expr".structure === "Case(Term.Name(\"X\"), Some(Term.Name(\"foo\")), Term.Name(\"bar\"))")
  }

  test("3 p\"case pat if expropt => expr\"") {
    val pat = p"`X`"
    val expropt = q"`foo`"
    val expr = q"`bar`"
    assert(p"case $pat if $expropt => $expr".structure === "Case(Term.Name(\"X\"), Some(Term.Name(\"foo\")), Term.Name(\"bar\"))")
  }

  test("1 p\"_*\"") {
    assert(p"case List(_*) =>".structure === "Case(Pat.Extract(Term.Name(\"List\"), List(Pat.SeqWildcard())), None, Term.Block(Nil))")
  }

  test("2 p\"_*\"") {
    assert(p"_*".structure === "Pat.SeqWildcard()")
  }

  test("1 p\"pat\"") {
    val pat = p"X"
    assert(p"$pat".structure === "Pat.Var(Term.Name(\"X\"))")
  }

  test("2 p\"pat\"") {
    val pat = p"`X`"
    assert(p"$pat".structure === "Term.Name(\"X\")")
  }

  test("1 q\"..mods val ..pats: tpe\"") {
    val q"..$mods val ..$pats: $tpe" = q"private final val x, y: T"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(pats.toString === "List(x, y)")
    assert(pats(0).structure === "Pat.Var(Term.Name(\"x\"))")
    assert(pats(1).structure === "Pat.Var(Term.Name(\"y\"))")
    assert(tpe.structure === "Type.Name(\"T\")")
  }

  test("2 q\"..mods val ..pats: tpe\"") {
    val mods = List(mod"private", mod"final")
    val pats = List(p"x", p"y")
    val tpe = t"T"
    assert(q"..$mods val ..$pats: $tpe".structure === "Decl.Val(List(Mod.Private(Name(\"\")), Mod.Final()), List(Pat.Var(Term.Name(\"x\")), Pat.Var(Term.Name(\"y\"))), Type.Name(\"T\"))")
  }

  test("1 q\"..mods var ..pats: tpe\"") {
    val q"..$mods var ..$pats: $tpe" = q"private final var x, y: T"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(pats.toString === "List(x, y)")
    assert(pats(0).structure === "Pat.Var(Term.Name(\"x\"))")
    assert(pats(1).structure === "Pat.Var(Term.Name(\"y\"))")
    assert(tpe.structure === "Type.Name(\"T\")")
  }

  test("2 q\"..mods var ..pats: tpe\"") {
    val mods = List(mod"private", mod"final")
    val pats = List(p"x", p"y")
    val tpe = t"T"
    assert(q"..$mods var ..$pats: $tpe".structure === "Decl.Var(List(Mod.Private(Name(\"\")), Mod.Final()), List(Pat.Var(Term.Name(\"x\")), Pat.Var(Term.Name(\"y\"))), Type.Name(\"T\"))")
  }

  test("1 q\"..mods def name[..tparams](...paramss): tpe\"") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpe" = q"private final def m[T, W](x: X, y: Y): R"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(name.structure === "Term.Name(\"m\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).structure === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).structure === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).structure === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(tpe.structure === "Type.Name(\"R\")")
  }

  test("2 q\"..mods def name[..tparams](...paramss): tpe\"") {
    val mods = List(mod"private", mod"final")
    val name = q"m"
    val tparams = List(tparam"T", tparam"W")
    val paramss = List(List(param"x: X", param"x: Y"))
    val tpe = t"R"
    assert(q"..$mods def $name[..$tparams](...$paramss): $tpe".structure === "Decl.Def(List(Mod.Private(Name(\"\")), Mod.Final()), Term.Name(\"m\"), List(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), List(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None))), Type.Name(\"R\"))")
  }

  test("1 q\"..mods type tname[..tparams] >: tpeopt <: tpeopt\"") {
    val q"..$mods type $tname[..$tparams] >: $tpeopt1 <: $tpeopt2" = q"private final type T[T, W] >: A <: B"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(tname.structure === "Type.Name(\"T\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).structure === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tpeopt1.structure === "Some(Type.Name(\"A\"))")
    assert(tpeopt2.structure === "Some(Type.Name(\"B\"))")
  }

  test("2 q\"..mods type tname[..tparams] >: tpeopt <: tpeopt\"") {
    val mods = List(mod"private", mod"final")
    val tname = t"T"
    val tparams = List(tparam"T", tparam"W")
    val tpeopt1 = t"A"
    val tpeopt2 = t"A"
    assert(q"..$mods type $tname[..$tparams] >: $tpeopt1 <: $tpeopt2".structure === "Decl.Type(List(Mod.Private(Name(\"\")), Mod.Final()), Type.Name(\"T\"), List(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Bounds(Some(Type.Name(\"A\")), Some(Type.Name(\"A\"))))")
  }

  test("1 q\"..mods val ..pats: tpeopt = expr\"") {
    val q"..$mods val ..$pats: $tpeopt = $expr" = q"private final val x, y: T = t"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(pats.toString === "List(x, y)")
    assert(pats(0).structure === "Pat.Var(Term.Name(\"x\"))")
    assert(pats(1).structure === "Pat.Var(Term.Name(\"y\"))")
    assert(tpeopt.structure === "Some(Type.Name(\"T\"))")
    assert(expr.structure === "Term.Name(\"t\")")
  }

  test("2 q\"..mods val ..pats: tpeopt = expr\"") {
    val mods = List(mod"private", mod"final")
    val pats = List(p"x", p"y")
    val tpeopt = t"T"
    val expr = q"t"
    assert(q"..$mods val ..$pats: $tpeopt = $expr".structure === "Defn.Val(List(Mod.Private(Name(\"\")), Mod.Final()), List(Pat.Var(Term.Name(\"x\")), Pat.Var(Term.Name(\"y\"))), Some(Type.Name(\"T\")), Term.Name(\"t\"))")
  }

  test("1 q\"..mods var ..pats: tpeopt = expropt\"") {
    val q"..$mods var ..$pats: $tpeopt = $expropt" = q"private final var x, y: T = t"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(pats.toString === "List(x, y)")
    assert(pats(0).structure === "Pat.Var(Term.Name(\"x\"))")
    assert(pats(1).structure === "Pat.Var(Term.Name(\"y\"))")
    assert(tpeopt.structure === "Some(Type.Name(\"T\"))")
    assert(expropt.structure === "Some(Term.Name(\"t\"))")
  }

  test("2 q\"..mods var ..pats: tpeopt = expropt\"") {
    val mods = List(mod"private", mod"final")
    val pats = List(p"x", p"y")
    val tpeopt = t"T"
    val expropt = q"t"
    assert(q"..$mods var ..$pats: $tpeopt = $expropt".structure === "Defn.Var(List(Mod.Private(Name(\"\")), Mod.Final()), List(Pat.Var(Term.Name(\"x\")), Pat.Var(Term.Name(\"y\"))), Some(Type.Name(\"T\")), Some(Term.Name(\"t\")))")
  }

  test("1 q\"..mods def name[..tparams](...paramss): tpeopt = expr\"") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" = q"private final def m[T, W](x: X, y: Y): R = r"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(name.structure === "Term.Name(\"m\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).structure === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).structure === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).structure === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(tpeopt.structure === "Some(Type.Name(\"R\"))")
    assert(expr.structure === "Term.Name(\"r\")")
  }

  test("2 q\"..mods def name[..tparams](...paramss): tpeopt = expr\"") {
    val mods = List(mod"private", mod"final")
    val name = q"m"
    val tparams = List(tparam"T", tparam"W")
    val paramss = List(List(param"x: X", param"x: Y"))
    val tpeopt = t"R"
    val expr = q"r"
    assert(q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr".structure === "Defn.Def(List(Mod.Private(Name(\"\")), Mod.Final()), Term.Name(\"m\"), List(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), List(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None))), Some(Type.Name(\"R\")), Term.Name(\"r\"))")
  }

  test("1 q\"..mods def name[..tparams](...paramss): tpeopt = macro expr\"") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr" = q"private final def m[T, W](x: X, y: Y): R = macro r"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(name.structure === "Term.Name(\"m\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).structure === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).structure === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).structure === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(tpeopt.structure === "Some(Type.Name(\"R\"))")
    assert(expr.structure === "Term.Name(\"r\")")
  }

  test("2 q\"..mods def name[..tparams](...paramss): tpeopt = macro expr\"") {
    val mods = List(mod"private", mod"final")
    val name = q"m"
    val tparams = List(tparam"T", tparam"W")
    val paramss = List(List(param"x: X", param"x: Y"))
    val tpeopt = Some(t"R")
    val expr = q"r"
    assert(q"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr".structure === "Defn.Macro(List(Mod.Private(Name(\"\")), Mod.Final()), Term.Name(\"m\"), List(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), List(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None))), Some(Type.Name(\"R\")), Term.Name(\"r\"))")
  }

  test("1 q\"..mods type tname[..tparams] = tpe\"") {
    val q"..$mods type $tname[..$tparams] = $tpe" = q"private final type Q[T, W] = R"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(tname.structure === "Type.Name(\"Q\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).structure === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tpe.structure === "Type.Name(\"R\")")
  }

  test("2 q\"..mods type tname[..tparams] = tpe\"") {
    val mods = List(mod"private", mod"final")
    val tname = t"Q"
    val tparams = List(tparam"T", tparam"W")
    val tpe = t"R"
    assert(q"..$mods type $tname[..$tparams] = $tpe".structure === "Defn.Type(List(Mod.Private(Name(\"\")), Mod.Final()), Type.Name(\"Q\"), List(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Name(\"R\"))")
  }

  test("1 q\"..mods class tname[..tparams] mod (...paramss) extends template\"") {
  val q"..$mods class $tname[..$tparams] $mod (...$paramss) extends $template" = q"private final class Q[T, W] private (x: X, y: Y) extends Y"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(tname.structure === "Type.Name(\"Q\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).structure === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(mod.structure === "Mod.Private(Name(\"\"))")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).structure === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).structure === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(template.structure === "Template(Nil, List(Init(Type.Name(\"Y\"), Name(\"\"), Nil)), Self(Name(\"\"), None), Nil)")
  }

  test("2 q\"..mods class tname[..tparams] mod (...paramss) extends template\"") {
  val q"..$mods class $tname[..$tparams] $mod (...$paramss) extends $template" = q"private final class Q[T, W] protected (x: X, y: Y) extends { def m1 = 42; def m2 = 666 }"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(tname.structure === "Type.Name(\"Q\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).structure === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(mod.structure === "Mod.Protected(Name(\"\"))")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).structure === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).structure === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(template.structure === "Template(Nil, Nil, Self(Name(\"\"), None), List(Defn.Def(Nil, Term.Name(\"m1\"), Nil, Nil, None, Lit.Int(42)), Defn.Def(Nil, Term.Name(\"m2\"), Nil, Nil, None, Lit.Int(666))))")
  }

  test("3 q\"..mods class tname[..tparams] mod (...paramss) extends template\"") {
  val mods = List(mod"private", mod"final")
    val tname = t"Q"
    val tparams = List(tparam"T", tparam"W")
    val mod = mod"protected"
    val paramss = List(List(param"x: X", param"x: Y"))
    val template = template"F { def m = 42 }"
    assert(q"..$mods class $tname[..$tparams] $mod (...$paramss) extends $template".structure === "Defn.Class(List(Mod.Private(Name(\"\")), Mod.Final()), Type.Name(\"Q\"), List(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Ctor.Primary(List(Mod.Protected(Name(\"\"))), Name(\"\"), List(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None)))), Template(Nil, List(Init(Type.Name(\"F\"), Name(\"\"), Nil)), Self(Name(\"\"), None), List(Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit.Int(42)))))")
  }

  test("1 q\"..mods trait tname[..tparams] extends template\"") {
    val q"..$mods trait $tname[..$tparams] extends $template" = q"private sealed trait Q[T, W] extends Y"
    assert(mods.toString === "List(private, sealed)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Sealed()")
    assert(tname.structure === "Type.Name(\"Q\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).structure === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(template.structure === "Template(Nil, List(Init(Type.Name(\"Y\"), Name(\"\"), Nil)), Self(Name(\"\"), None), Nil)")
  }

  test("2 q\"..mods trait tname[..tparams] extends template\"") {
    val q"..$mods trait $tname[..$tparams] extends $template" = q"private sealed trait Q[T, W] extends { def m1 = 42; def m2 = 666 }"
    assert(mods.toString === "List(private, sealed)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Sealed()")
    assert(tname.structure === "Type.Name(\"Q\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).structure === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(template.structure === "Template(Nil, Nil, Self(Name(\"\"), None), List(Defn.Def(Nil, Term.Name(\"m1\"), Nil, Nil, None, Lit.Int(42)), Defn.Def(Nil, Term.Name(\"m2\"), Nil, Nil, None, Lit.Int(666))))")
  }

  test("3 q\"..mods trait tname[..tparams] extends template\"") {
    val mods = List(mod"private", mod"sealed")
    val tname = t"Q"
    val tparams = List(tparam"T", tparam"W")
    val template = template"F { def m = 42 }"
    assert(q"..$mods trait $tname[..$tparams] extends $template".structure === "Defn.Trait(List(Mod.Private(Name(\"\")), Mod.Sealed()), Type.Name(\"Q\"), List(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Ctor.Primary(Nil, Name(\"\"), Nil), Template(Nil, List(Init(Type.Name(\"F\"), Name(\"\"), Nil)), Self(Name(\"\"), None), List(Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit.Int(42)))))")
  }

  test("1 q\"..mods object name extends template\"") {
    val q"..$mods object $name extends $template" = q"private final object Q extends Y"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(name.structure === "Term.Name(\"Q\")")
    assert(template.structure === "Template(Nil, List(Init(Type.Name(\"Y\"), Name(\"\"), Nil)), Self(Name(\"\"), None), Nil)")
  }

  test("2 q\"..mods object name extends template\"") {
    val q"..$mods object $name extends $template" = q"private final object Q extends { def m1 = 42; def m2 = 666 }"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(name.structure === "Term.Name(\"Q\")")
    assert(template.structure === "Template(Nil, Nil, Self(Name(\"\"), None), List(Defn.Def(Nil, Term.Name(\"m1\"), Nil, Nil, None, Lit.Int(42)), Defn.Def(Nil, Term.Name(\"m2\"), Nil, Nil, None, Lit.Int(666))))")
  }

  test("3 q\"..mods object name extends template\"") {
    val mods = List(mod"private", mod"final")
    val name = q"Q"
    val template = template"F { def m = 42 }"
    assert(q"..$mods object $name extends $template".structure === "Defn.Object(List(Mod.Private(Name(\"\")), Mod.Final()), Term.Name(\"Q\"), Template(Nil, List(Init(Type.Name(\"F\"), Name(\"\"), Nil)), Self(Name(\"\"), None), List(Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit.Int(42)))))")
  }

  test("1 q\"package object name extends template\"") {
    val q"package object $name extends $template" = q"package object Q extends Y"
    assert(name.structure === "Term.Name(\"Q\")")
    assert(template.structure === "Template(Nil, List(Init(Type.Name(\"Y\"), Name(\"\"), Nil)), Self(Name(\"\"), None), Nil)")
  }

  test("2 q\"package object name extends template\"") {
    val q"package object $name extends $template" = q"package object Q extends { def m1 = 42; def m2 = 666 }"
    assert(name.structure === "Term.Name(\"Q\")")
    assert(template.structure === "Template(Nil, Nil, Self(Name(\"\"), None), List(Defn.Def(Nil, Term.Name(\"m1\"), Nil, Nil, None, Lit.Int(42)), Defn.Def(Nil, Term.Name(\"m2\"), Nil, Nil, None, Lit.Int(666))))")
  }

  test("3 q\"package object name extends template\"") {
    val name = q"Q"
    val template = template"F { def m = 42 }"
    assert(q"package object $name extends $template".structure === "Pkg.Object(Nil, Term.Name(\"Q\"), Template(Nil, List(Init(Type.Name(\"F\"), Name(\"\"), Nil)), Self(Name(\"\"), None), List(Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit.Int(42)))))")
  }

  test("1 q\"package ref { ..stats }\"") {
    val q"package $ref { ..$stats }" = q"package p { class A; object B }"
    assert(ref.structure === "Term.Name(\"p\")")
    assert(stats.toString === "List(class A, object B)")
    assert(stats(0).structure === "Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Name(\"\"), Nil), Template(Nil, Nil, Self(Name(\"\"), None), Nil))")
    assert(stats(1).structure === "Defn.Object(Nil, Term.Name(\"B\"), Template(Nil, Nil, Self(Name(\"\"), None), Nil))")
  }

  test("2 q\"package ref { ..stats }\"") {
    val ref = q"p"
    val stats = List(q"class A", q"object B")
    assert(q"package $ref { ..$stats }".structure === "Pkg(Term.Name(\"p\"), List(Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Name(\"\"), Nil), Template(Nil, Nil, Self(Name(\"\"), None), Nil)), Defn.Object(Nil, Term.Name(\"B\"), Template(Nil, Nil, Self(Name(\"\"), None), Nil))))")
  }

  /*
   Issue #462
   */
  test("3 q\"package ref { ..stats }\"") {
    val ref = q"p.a"
    val stats = List(q"class A", q"object B")
    assert(q"package $ref { ..$stats }".structure === "Pkg(Term.Select(Term.Name(\"p\"), Term.Name(\"a\")), List(Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Name(\"\"), Nil), Template(Nil, Nil, Self(Name(\"\"), None), Nil)), Defn.Object(Nil, Term.Name(\"B\"), Template(Nil, Nil, Self(Name(\"\"), None), Nil))))")
  }

  test("1 q\"..mods def this(...paramss)\"") {
    val q"..$mods def this(...$paramss)" = q"private def this(x: X, y: Y)"
    assert(mods.toString === "List(private)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).structure === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).structure === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
  }

   test("2 q\"..mods def this(...paramss)\"") {
     val mods = List(mod"private")
     val paramss = List(List(param"x: X", param"x: Y"))
     assert(q"..$mods def this(...$paramss)".structure === "Ctor.Primary(List(Mod.Private(Name(\"\"))), Name(\"\"), List(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None))))")
   }

  test("1 q\"..mods def this(...paramss) = expr\"") {
    val q"..$mods def this(...$paramss) = $init" = q"private final def this(x: X, y: Y) = this(foo, bar)"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).structure === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).structure === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(init.structure === "Init(Type.Singleton(Term.This(Name(\"\"))), Name(\"\"), List(List(Term.Name(\"foo\"), Term.Name(\"bar\"))))")
  }

   test("2 q\"..mods def this(...paramss) = expr\"") {
     val mods = List(mod"private", mod"final")
     val paramss = List(List(param"x: X", param"x: Y"))
     val init = init"C(foo, bar)"
     assert(q"..$mods def this(...$paramss) = $init".structure === "Ctor.Secondary(List(Mod.Private(Name(\"\")), Mod.Final()), Name(\"\"), List(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None))), Init(Type.Name(\"C\"), Name(\"\"), List(List(Term.Name(\"foo\"), Term.Name(\"bar\")))), Nil)")
   }

  test("1 param\"..mods paramname: tpeopt = expropt\"") {
    val param"..$mods $paramname: $tpeopt = $expropt" = param"private final x: X = 42"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).structure === "Mod.Private(Name(\"\"))")
    assert(mods(1).structure === "Mod.Final()")
    assert(paramname.structure === "Term.Name(\"x\")")
    assert(tpeopt.structure === "Some(Type.Name(\"X\"))")
    assert(expropt.structure === "Some(Lit.Int(42))")
  }

  test("2 param\"..mods paramname: tpeopt = expropt\"") {
    val mods = List(mod"private", mod"final")
    val paramname = q"x"
    val tpeopt = t"X"
    val expropt = q"42"
    assert(param"..$mods $paramname: $tpeopt = $expropt".structure === "Term.Param(List(Mod.Private(Name(\"\")), Mod.Final()), Term.Name(\"x\"), Some(Type.Name(\"X\")), Some(Lit.Int(42)))")
  }

  test("1 tparam\"..mods tparamname[..tparams] >: tpeopt <: tpeopt <% ..tpes : ..tpes\"") {
    val tparam"..$mods $tparamname[..$tparams] >: $tpeopt1 <: $tpeopt2 <% ..$tpes1 : ..$tpes2" = tparam"+Z[Q,W] >: E <: R <% T with Y : U with I"
    assert(mods.toString === "List(+)")
    assert(mods(0).structure === "Mod.Covariant()")
    assert(tparamname.structure === "Type.Name(\"Z\")")
    assert(tparams.toString === "List(Q, W)")
    assert(tparams(0).structure === "Type.Param(Nil, Type.Name(\"Q\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).structure === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tpeopt1.structure === "Some(Type.Name(\"E\"))")
    assert(tpeopt2.structure === "Some(Type.Name(\"R\"))")
    assert(tpes1.toString === "List(T with Y)")
    assert(tpes1(0).structure === "Type.With(Type.Name(\"T\"), Type.Name(\"Y\"))")
    assert(tpes2.toString === "List(U with I)")
    assert(tpes2(0).structure === "Type.With(Type.Name(\"U\"), Type.Name(\"I\"))")
  }

  test("2 tparam\"..mods tparamname[..tparams] >: tpeopt <: tpeopt <% ..tpes : ..tpes\"") {
    val mods = List(mod"+")
    val tparamname = t"Z"
    val tparams = List(tparam"Q", tparam"W")
    val tpeopt1 = t"E"
    val tpeopt2 = t"R"
    val tpes1 = List(t"T with Y")
    val tpes2 = List(t"U with I")
    assert(tparam"..$mods $tparamname[..$tparams] >: $tpeopt1 <: $tpeopt2 <% ..$tpes1 : ..$tpes2".structure === "Type.Param(List(Mod.Covariant()), Type.Name(\"Z\"), List(Type.Param(Nil, Type.Name(\"Q\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Bounds(Some(Type.Name(\"E\")), Some(Type.Name(\"R\"))), List(Type.With(Type.Name(\"T\"), Type.Name(\"Y\"))), List(Type.With(Type.Name(\"U\"), Type.Name(\"I\"))))")
  }

  test("1 init\"tpe(...exprss)\"") {
    val init"$tpe(...$exprss)" = init"C(40)(2)"
    assert(tpe.toString === "C")
    assert(tpe.structure === "Type.Name(\"C\")")
    assert(exprss.toString === "List(List(40), List(2))")
    assert(exprss(0)(0).structure === "Lit.Int(40)")
    assert(exprss(1)(0).structure === "Lit.Int(2)")
  }

  test("2 init\"tpe(...exprss)\"") {
    val tpe = t"C"
    val exprss = List(List(q"40"), List(q"2"))
    assert(init"$tpe(...$exprss)".structure === "Init(Type.Name(\"C\"), Name(\"\"), List(List(Lit.Int(40)), List(Lit.Int(2))))")
  }

  test("1 init\"this(...exprss)\"") {
    val init"this(...$exprss)" = init"this(40)(2)"
    assert(exprss.toString === "List(List(40), List(2))")
    assert(exprss(0)(0).structure === "Lit.Int(40)")
    assert(exprss(1)(0).structure === "Lit.Int(2)")
  }

  test("2 init\"this(...exprss)\"") {
    val exprss = List(List(q"40"), List(q"2"))
    assert(init"this(...$exprss)".structure === "Init(Type.Singleton(Term.This(Name(\"\"))), Name(\"\"), List(List(Lit.Int(40)), List(Lit.Int(2))))")
  }

  test("1 self\"name: tpeopt\"") {
    val self"$name: $tpeopt" = self"x: T"
    assert(name.toString === "x")
    assert(name.structure === "Term.Name(\"x\")")
    assert(tpeopt.toString === "Some(T)")
    assert(tpeopt.structure === "Some(Type.Name(\"T\"))")
  }

  test("2 self\"name: tpeopt\"") {
    val name = q"x"
    val tpeopt = t"T"
    assert(self"$name: $tpeopt".structure === "Self(Term.Name(\"x\"), Some(Type.Name(\"T\")))")
  }

  test("1 self\"this: tpeopt\"") {
    val self"$name: $tpeopt" = self"this: T"
    assert(name.toString === "_")
    assert(name.structure === "Name(\"\")")
    assert(tpeopt.toString === "Some(T)")
    assert(tpeopt.structure === "Some(Type.Name(\"T\"))")
  }

  test("2 self\"this: tpeopt\"") {
    val tpeopt = t"T"
    assert(self"this: $tpeopt".structure === "Self(Name(\"\"), Some(Type.Name(\"T\")))")
  }

  test("1 template\"{ ..stats } with ..inits { self => ..stats }\"") {
    val template"{ ..$stats1 } with ..$inits { $self => ..$stats2 }" = template"{ val a = 2; val b = 2 } with T with U { self: Z => def m = 2; def n = 2 }"
    assert(stats1.toString === "List(val a = 2, val b = 2)")
    assert(stats1(0).structure === "Defn.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), None, Lit.Int(2))")
    assert(stats1(1).structure === "Defn.Val(Nil, List(Pat.Var(Term.Name(\"b\"))), None, Lit.Int(2))")
    assert(inits.toString === "List(T, U)")
    assert(inits(0).structure === "Init(Type.Name(\"T\"), Name(\"\"), Nil)")
    assert(inits(1).structure === "Init(Type.Name(\"U\"), Name(\"\"), Nil)")
    assert(self.structure === "Self(Term.Name(\"self\"), Some(Type.Name(\"Z\")))")
    assert(stats2.toString === "List(def m = 2, def n = 2)")
    assert(stats2(0).structure === "Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit.Int(2))")
    assert(stats2(1).structure === "Defn.Def(Nil, Term.Name(\"n\"), Nil, Nil, None, Lit.Int(2))")
  }

   test("2 template\"{ ..stats } with ..inits { self => ..stats }\"") {
     val stats1 = List(q"val a = 2", q"val b = 2")
     val inits = List(init"T", init"U")
     val self = self"self: S"
     val stats2 = List(q"def m = 2", q"def n = 2")
     assert(template"{ ..$stats1 } with ..$inits { $self => ..$stats2 }".structure === "Template(List(Defn.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), None, Lit.Int(2)), Defn.Val(Nil, List(Pat.Var(Term.Name(\"b\"))), None, Lit.Int(2))), List(Init(Type.Name(\"T\"), Name(\"\"), Nil), Init(Type.Name(\"U\"), Name(\"\"), Nil)), Self(Term.Name(\"self\"), Some(Type.Name(\"S\"))), List(Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit.Int(2)), Defn.Def(Nil, Term.Name(\"n\"), Nil, Nil, None, Lit.Int(2))))")
   }

  test("1 mod\"@expr\"") {
    val mod"@$expr" = mod"@a"
    assert(expr.structure === "Mod.Annot(Init(Type.Name(\"a\"), Name(\"\"), Nil))")
  }

  test("2 mod\"@expr\"") {
    val expr = mod"@a"
    assert(mod"@$expr".structure === "Mod.Annot(Init(Type.Name(\"a\"), Name(\"\"), Nil))")
  }

  test("1 mod\"private[name]\"") {
    val mod"private[$name]" = mod"private[X]"
    assert(name.structure === "Name(\"X\")")
  }

  test("2 mod\"private[name]\"") {
    val mod"private[$name]" = mod"private"
    assert(name.structure === "Name(\"\")")
  }

  test("3 mod\"private[this]\"") {
    val mod"private[this]" = mod"private[this]"
  }

  test("4 mod\"private[name]\"") {
    val name = q"q"
    assert(mod"private[$name]".structure === "Mod.Private(Term.Name(\"q\"))")
  }

  test("1 mod\"protected[name]\"") {
    val mod"protected[$name]" = mod"protected[X]"
    assert(name.structure === "Name(\"X\")")
  }

  test("2 mod\"protected[name]\"") {
    val mod"protected[$name]" = mod"protected"
    assert(name.structure === "Name(\"\")")
  }

  test("3 mod\"protected[this]\"") {
    val mod"protected[this]" = mod"protected[this]"
  }

  test("4 mod\"protected[name]\"") {
    val name = q"q"
    assert(mod"protected[$name]".structure === "Mod.Protected(Term.Name(\"q\"))")
  }

  test("mod\"implicit\"") {
    assert(mod"implicit".structure === "Mod.Implicit()")
  }

  test("mod\"final\"") {
    assert(mod"final".structure === "Mod.Final()")
  }

  test("mod\"sealed\"") {
    assert(mod"sealed".structure === "Mod.Sealed()")
  }

  test("mod\"override\"") {
    assert(mod"override".structure === "Mod.Override()")
  }

  test("mod\"case\"") {
    assert(mod"case".structure === "Mod.Case()")
  }

  test("mod\"abstract\"") {
    assert(mod"abstract".structure === "Mod.Abstract()")
  }

  test("mod\"+\"") {
    assert(mod"+".structure === "Mod.Covariant()")
  }

  test("mod\"-\"") {
    assert(mod"-".structure === "Mod.Contravariant()")
  }

  test("mod\"lazy\"") {
    assert(mod"lazy".structure === "Mod.Lazy()")
  }

  test("mod\"val\"") {
    assert(mod"valparam".structure === "Mod.ValParam()")
  }

  test("mod\"var\"") {
    assert(mod"varparam".structure === "Mod.VarParam()")
  }

  test("1 enumerator\"pat <- expr\"") {
    val enumerator"$pat <- $expr" = enumerator"x <- xs"
    assert(pat.structure === "Pat.Var(Term.Name(\"x\"))")
  }

  test("2 enumerator\"pat <- expr\"") {
    val pat = p"x"
    val expr = q"xs"
    assert(enumerator"$pat <- $expr".structure === "Enumerator.Generator(Pat.Var(Term.Name(\"x\")), Term.Name(\"xs\"))")
  }

  test("3 enumerator\"pat <- expr\"") {
    val pat = p"X"
    val expr = q"xs"
    assert(enumerator"$pat <- $expr".structure === "Enumerator.Generator(Pat.Var(Term.Name(\"X\")), Term.Name(\"xs\"))")
  }

  test("1 enumerator\"pat = expr\"") {
    val enumerator"$pat = $expr" = enumerator"x = xs"
    assert(pat.structure === "Pat.Var(Term.Name(\"x\"))")
  }

  test("2 enumerator\"pat = expr\"") {
    val pat = p"x"
    val expr = q"xs"
    assert(enumerator"$pat = $expr".structure === "Enumerator.Val(Pat.Var(Term.Name(\"x\")), Term.Name(\"xs\"))")
  }

  test("1 enumerator\"if expr\"") {
    val enumerator"if $expr" = enumerator"if x"
    assert(expr.structure === "Term.Name(\"x\")")
  }

  test("2 enumerator\"if expr\"") {
    val expr = q"x"
    assert(enumerator"if $expr".structure === "Enumerator.Guard(Term.Name(\"x\"))")
  }

  test("1 q\"import ..importers\"") {
    val importers = List(importer"foo.bar", importer"bar.{baz, _}")
    assert(q"import ..$importers".syntax === "import foo.bar, bar.{ baz, _ }")
  }

  test("2 q\"import ..importers\"") {
    val q"import ..$importers" = q"import a.A"
    assert(importers.length == 1)
    assert(importers(0).syntax === "a.A")
  }

  test("1 importer\"ref.{..importees}\"") {
    val ref = q"bar"
    val importees = List(importee"baz", importee"_")
    assert(importer"$ref.{..$importees}".syntax === "bar.{ baz, _ }")
  }

  test("2 importer\"ref. ..importees\"") {
    val importer"$ref.{..$importees}" = importer"bar.{baz, _}"
    assert(ref.syntax === "bar")
    assert(importees.length == 2)
    assert(importees(0).syntax === "baz")
    assert(importees(1).syntax === "_")
  }

  /*
   Issue #462
   */
  test("3 importer\"ref.{..importees}\"") {
    val ref = q"bar.a"
    val importees = List(importee"baz", importee"_")
    assert(importer"$ref.{..$importees}".syntax === "bar.a.{ baz, _ }")
  }

  test("1 importee\"iname\"") {
    val importee"$iname" = importee"x"
    assert(iname.structure === "Name(\"x\")")
  }

  test("2 importee\"iname\"") {
    // $iname can't be constructed, only extracted from importee"..." and mod"..."
    val importee"$iname" = importee"x"
    assert(importee"$iname".structure === "Importee.Name(Name(\"x\"))")
  }

  test("1 importee\"iname => iname\"") {
    val importee"$iname1 => $iname2" = importee"x => y"
    assert(iname1.structure === "Name(\"x\")")
    assert(iname2.structure === "Name(\"y\")")
  }

  test("2 importee\"iname => iname\"") {
    // $iname can't be constructed, only extracted from importee"..." and mod"..."
    val importee"$iname1 => $iname2" = importee"x => y"
    assert(importee"$iname1 => $iname2".structure === "Importee.Rename(Name(\"x\"), Name(\"y\"))")
  }

  test("1 importee\"iname => _\"") {
    val importee"$iname => _" = importee"x => _"
    assert(iname.structure === "Name(\"x\")")
  }

  test("2 importee\"iname => _\"") {
  // $iname can't be constructed, only extracted from importee"..." and mod"..."
  val importee"$iname => _" = importee"x => _"
    assert(importee"$iname => _".structure === "Importee.Unimport(Name(\"x\"))")
  }

  test("importee\"_\"") {
    assert(importee"_".structure === "Importee.Wildcard()")
  }

  test("1 source\"..stats\"") {
    val source"..$stats" = source"class A { val a = 'a'}"
    assert(stats.toString === "List(class A { val a = 'a' })")
    assert(stats(0).structure === "Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Name(\"\"), Nil), Template(Nil, Nil, Self(Name(\"\"), None), List(Defn.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), None, Lit.Char('a')))))")
  }

  test("2 source\"..stats\"") {
    val source"class B { val b = 'b'}; ..$stats" = source"class B { val b = 'b'}; class A { val a = 'a'}"
    assert(stats.toString === "List(class A { val a = 'a' })")
    assert(stats(0).structure === "Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Name(\"\"), Nil), Template(Nil, Nil, Self(Name(\"\"), None), List(Defn.Val(Nil, List(Pat.Var(Term.Name(\"a\"))), None, Lit.Char('a')))))")
  }

  test("3 source\"..stats\"") {
    val stats = List(q"class A { val x = 1 }", q"object B")
    assert(source"..$stats".structure === "Source(List(Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Name(\"\"), Nil), Template(Nil, Nil, Self(Name(\"\"), None), List(Defn.Val(Nil, List(Pat.Var(Term.Name(\"x\"))), None, Lit.Int(1))))), Defn.Object(Nil, Term.Name(\"B\"), Template(Nil, Nil, Self(Name(\"\"), None), Nil))))")
  }

  test("unquote T into Option[T]") {
    val cond = q"cond"
    assert(p"case _ if $cond =>".structure === "Case(Pat.Wildcard(), Some(Term.Name(\"cond\")), Term.Block(Nil))")
  }

  test("unquote Option[T] into Option[T]") {
    val condopt = Some(q"cond")
    assert(p"case _ if $condopt =>".structure === "Case(Pat.Wildcard(), Some(Term.Name(\"cond\")), Term.Block(Nil))")
  }

  test("extract Some[T] from Option[T]") {
    val p"case _ if $condopt =>" = p"case _ if cond =>"
    assert(condopt.structure === "Some(Term.Name(\"cond\"))")
  }

  test("extract None from Option[T]") {
    val p"case _ if $condopt =>" = p"case _ =>"
    assert(condopt.structure === "None")
  }

  test("initial support for ...") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpe = $rhs" = q"def f(x: Int) = ???"
    assert(paramss.structure === "List(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Int\")), None)))")
    assert(q"..$mods def $name[..$tparams](...$paramss): $tpe = $rhs".structure === "Defn.Def(Nil, Term.Name(\"f\"), Nil, List(List(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Int\")), None))), None, Term.Name(\"???\"))")
  }

  test("ellipses in template stats") {
    val mods = List(mod"private")
    val tree = q"class C { ..$mods def x = 2 }"
    assert(tree.structure === "Defn.Class(Nil, Type.Name(\"C\"), Nil, Ctor.Primary(Nil, Name(\"\"), Nil), Template(Nil, Nil, Self(Name(\"\"), None), List(Defn.Def(List(Mod.Private(Name(\"\"))), Term.Name(\"x\"), Nil, Nil, None, Lit.Int(2)))))")
  }

  test("#300") {
    val q"class $tname1 ..$mods1" = q"class C"
    assert(q"class $tname1 ..$mods1".syntax === "class C")
    val q"class $tname2 ..$mods2" = q"class C private"
    assert(q"class $tname2 ..$mods2".syntax === "class C private")
  }

  test("#448") {
    val parent = init"_root_.scala.AnyVal"
    val template = template"$parent"
    assert(q"class C extends $template".syntax === "class C extends _root_.scala.AnyVal")
    assert(q"class C extends $parent {}".syntax === "class C extends _root_.scala.AnyVal")
    assert(q"class C extends $parent with $parent".syntax === "class C extends _root_.scala.AnyVal with _root_.scala.AnyVal")
  }

  test("#452") {
    val stat = q"class C"
    assert(q"$stat; $stat".syntax === """
    |{
    |  class C
    |  class C
    |}""".trim.stripMargin.split('\n').mkString(EOL))
    assert(q"{ $stat; $stat }".syntax === """
    |{
    |  class C
    |  class C
    |}""".trim.stripMargin.split('\n').mkString(EOL))
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
    assert(objectDefn.syntax === """
      |object M {
      |  def foo = bar
      |  println("another stat")
      |  def baz: Unit = {}
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("#458") {
    val name = q"x"
    val tpe = t"T"
    val lambda = q"($name: $tpe) => ???"
    assert(lambda.syntax === "(x: T) => ???")
  }

  test("#458 II") {
    val name = q"x"
    val lambda = q"($name: T) => ???"
    assert(lambda.syntax === "(x: T) => ???")
  }

  test("#455 - unquote None") {
    val defnopt: Option[Stat] = None
    assert(q"..$defnopt".structure === "Term.Block(Nil)")
  }

  test("#455 - unquote Some") {
    val defnoptSomeOption: Some[Stat] = Some(q"val x = 42")
    assert(q"..$defnoptSomeOption".structure === "Term.Block(List(Defn.Val(Nil, List(Pat.Var(Term.Name(\"x\"))), None, Lit.Int(42))))")
  }

  test("#455 - unquote Option") {
    val defnopt: Option[Stat] = Option(q"val x = 42")
    assert(q"..$defnopt".structure === "Term.Block(List(Defn.Val(Nil, List(Pat.Var(Term.Name(\"x\"))), None, Lit.Int(42))))")
  }

  test("#468 - primary constructor") {
    val q"case class A($param)" = q"case class A(a: Int)"
    assert(param.syntax === "a: Int")
  }

  test("#468 - primary constructor II") {
    val q"case class A($param, ..$params)" = q"case class A(a: Int, b: Int, c: Int)"
    assert(param.syntax === "a: Int")
    assert(params.size === 2)
    assert(params.head.syntax === "b: Int")
    assert(params.tail.head.syntax === "c: Int")
  }

  test("#468 - function parameter list") {
    val q"def foo($param): Int = a" = q"def foo(a: Int): Int = a"
    assert(param.syntax === "a: Int")
  }

  test("#468 - function parameter list II") {
    val q"def foo($param, ..$params): Int = a" = q"def foo(a: Int, b: Int, c: Int): Int = a"
    assert(param.syntax === "a: Int")
    assert(params.size === 2)
    assert(params.head.syntax === "b: Int")
    assert(params.tail.head.syntax === "c: Int")
  }

  test("#230 - tparam extensions I") {
    val tparam = tparam"@foo ${Mod.Covariant()} T"
    assert(tparam.syntax === "@foo +T")
  }

  test("#1006 - tparam extensions II") {
    val t1 = Type.Name("T1")
    val t2 = Type.Name("T2")
    val tparam1 = tparam"$t1"
    assert(tparam1.syntax === "T1")
    val tparam2 = tparam"$t1 : $t2"
    assert(tparam2.syntax === "T1: T2")
  }

  test("#829 - lambda extensions I") {
    val param = param"x:Int"
    val lambda = q"map($param => 3)"
    assert(lambda.syntax == "map { (x: Int) => 3 }")
  }

  test("#843") {
    val t = t"x.${Type.Name("T")}"
    assert(t.syntax == "x.T")
  }

  test("#915") {
    val a = q"a"
    val importer = importer"$a.b"
    assert(importer.syntax == "a.b")
  }

  test("#833") {
    val ys = List(Term.Name("y"))
    val block = q"x; ..$ys; z"
    assert(block.syntax == """
      |{
      |  x
      |  y
      |  z
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
  }
}
