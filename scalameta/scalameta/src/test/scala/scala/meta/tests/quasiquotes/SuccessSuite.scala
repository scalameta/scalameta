package scala.meta.tests
package quasiquotes

import org.scalatest._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.dialects.Scala211

class SuccessSuite extends FunSuite {
  test("rank-0 liftables") {
    assert(q"foo[${42}]".show[Structure] === "Term.ApplyType(Term.Name(\"foo\"), Seq(Lit(42)))")
    assert(q"${42}".show[Structure] === "Lit(42)")
  }

  test("rank-1 liftables") {
    implicit def custom[U >: List[Term]]: Lift[List[Int], U] = Lift(_.map(x => q"$x"))
    assert(q"foo(..${List(1, 2, 3)})".show[Structure] === "Term.Apply(Term.Name(\"foo\"), Seq(Lit(1), Lit(2), Lit(3)))")
  }

  test("construction ascriptions") {
    val xs = List(q"x", q"y")
    assert(q"foo(..${xs: List[Term]})".show[Syntax] === "foo(x, y)")
    val xss = List(List(q"x", q"y"))
    assert(q"foo(...${xss: List[List[Term]]})".show[Syntax] === "foo(x, y)")
    val rhs = Some(q"x")
    assert(q"var foo = ${rhs : Option[Term]}".show[Syntax] === "var foo = x")
  }

  test("deconstruction ascriptions") {
    val q"foo(..${xs: Seq[Term.Arg]})" = q"foo(x, y)"
    assert(xs.toString === "List(x, y)")
    val q"foo(...${xss: Seq[Seq[Term.Arg]]})" = q"foo(x, y)"
    assert(xss.toString === "List(List(x, y))")
    val q"var foo = ${x: Option[Term]}" = q"var foo = x"
    assert(x.toString === "Some(x)")
  }

  test("1 Pat.Type or Type.Name") {
    val q"1 match { case _: List[..$tpes] => }" = q"1 match { case _: List[t] => }"
    assert(tpes(0).show[Structure] === "Pat.Var.Type(Type.Name(\"t\"))")
  }

  test("2 Pat.Type or Type.Name") {
    val q"1 match {case x: $tpe =>}" = q"1 match {case x: T =>}"
    assert(tpe.show[Structure] === "Type.Name(\"T\")")
  }

  test("3 Pat.Type or Type.Name") {
    val q"1 match {case x: $tpe =>}" = q"1 match {case x: t =>}"
    assert(tpe.show[Structure] === "Type.Name(\"t\")")
  }

  test("1 p\"case $x: T => \"") {
    val p"case $x: T => " = p"case x: T =>"
    assert(x.show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
  }

  test("2 p\"case $x: T => \"") {
    val x = p"x"
    assert(p"case $x: T => ".show[Structure] === "Case(Pat.Typed(Pat.Var.Term(Term.Name(\"x\")), Type.Name(\"T\")), None, Term.Block(Nil))")
  }

  test("1 p\"case $x @ $y => \"") {
    val p"case $x @ $y => " = p"case x @ List(1, 2, 3) =>"
    assert(x.show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
    assert(y.show[Structure] === "Pat.Extract(Term.Name(\"List\"), Nil, Seq(Lit(1), Lit(2), Lit(3)))")
  }

  test("2 p\"case $x @ $y => \"") {
    val x = p"x"
    val y = p"List(1, 2, 3)"
    assert(p"case $x @ $y => ".show[Structure] === "Case(Pat.Bind(Pat.Var.Term(Term.Name(\"x\")), Pat.Extract(Term.Name(\"List\"), Nil, Seq(Lit(1), Lit(2), Lit(3)))), None, Term.Block(Nil))")
  }

  test("1 q\"foo($term, ..$terms, $term)\"") {
    val q"foo($term1, ..$terms, $term2)" = q"foo(x, y, z, q)"
    assert(term1.show[Structure] === "Term.Name(\"x\")")
    assert(terms.toString === "List(y, z)")
    assert(terms(0).show[Structure] === "Term.Name(\"y\")")
    assert(terms(1).show[Structure] === "Term.Name(\"z\")")
    assert(term2.show[Structure] === "Term.Name(\"q\")")
  }

  test("2 q\"foo($term, ..$terms, $term)\"") {
    val term = q"x"
    val terms = List(q"y", q"z")
    assert(q"foo($term, ..$terms, $term)".show[Structure] === "Term.Apply(Term.Name(\"foo\"), Seq(Term.Name(\"x\"), Term.Name(\"y\"), Term.Name(\"z\"), Term.Name(\"x\")))")
  }

  test("case q\"$foo(${x: Int})\"") {
    q"foo(42)" match {
      case q"$foo(${x: Int})" =>
        assert(foo.show[Structure] === "Term.Name(\"foo\")")
        assert(x == 42)
    }
  }

  test("case q\"$foo(${x: Int}, ..$ys, $z)\"") {
    q"foo(1, 2, 3)" match {
      case q"$_(${x: Int}, ..$y, $z)" =>
        assert(x === 1)
        assert(y.map(_.show[Structure]) === List("Lit(2)"))
        assert(z.show[Structure] === "Lit(3)")
    }
  }

  test("1 q\"foo($x, ..$ys, $z)\"") {
    val q"foo($x, ..$ys, $z)" = q"foo(1, 2, 3)"
    assert(x.show[Structure] === "Lit(1)")
    assert(ys.toString === "List(2)")
    assert(ys(0).show[Structure] === "Lit(2)")
    assert(z.show[Structure] === "Lit(3)")
  }

  test("2 q\"foo($x, ..$ys, $z, ..$ts)\"") {
    val x = q"1"
    val ys = List(q"2")
    val z = q"3"
    val ts = Nil
    assert(q"foo($x, ..$ys, $z, ..$ts)".show[Structure] === "Term.Apply(Term.Name(\"foo\"), Seq(Lit(1), Lit(2), Lit(3)))")
  }

  test("1 val q\"type $name[$_] = $_\"") {
    val q"type $name[$_] = $_" = q"type List[+A] = List[A]"
    assert(name.show[Structure] === "Type.Name(\"List\")")
  }

  test("2 val q\"type $name[$a] = $b\"") {
    val q"type $name[$a] = $b" = q"type List[+A] = List[A]"
    assert(name.show[Structure] === "Type.Name(\"List\")")
    assert(a.show[Structure] === "Type.Param(Seq(Mod.Covariant()), Type.Name(\"A\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(b.show[Structure] === "Type.Apply(Type.Name(\"List\"), Seq(Type.Name(\"A\")))")
  }

  test("3 val q\"type $name[$a] = $b\"") {
    val name = t"List"
    val a = tparam"+A"
    val b = t"B"
    assert(q"type $name[$a] = $b".show[Structure] === "Defn.Type(Nil, Type.Name(\"List\"), Seq(Type.Param(Seq(Mod.Covariant()), Type.Name(\"A\"), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Name(\"B\"))")
  }

  test("1 val q\"def x = ${body: Int}\"") {
    val q"def x = ${body: Int}" = q"def x = 42"
    assert(body === 42)
  }

  test("2 val q\"def x = ${body: Int}\"") {
    val body = 42
    assert(q"def x = ${body: Int}".show[Structure] === "Defn.Def(Nil, Term.Name(\"x\"), Nil, Nil, None, Lit(42))")
  }

  test("1 q\"$qname.this.$id\"") {
    val q"$qname.this.$x" = q"SuccessSuite.this.x"
    assert(qname.show[Structure] === "Name.Indeterminate(\"SuccessSuite\")")
    assert(x.show[Structure] === "Term.Name(\"x\")")
  }

  test("2 q\"$qname.this.$id\"") {
    val qname = q"A"
    val x = q"B"
    // inconsistency with the test above planned, since Name.Indeterminate can't be constructed directly
    assert(q"$qname.this.$x".show[Structure] === "Term.Select(Term.This(Term.Name(\"A\")), Term.Name(\"B\"))")
  }

  test("1 this variants") {
    val q"this" = q"this"
    val q"$clazz.this" = q"C.this"
    assert(clazz.show[Structure] === "Name.Indeterminate(\"C\")")
  }

  test("2 this variants") {
    val clazz = t"C"
    assert(q"this".show[Structure] === "Term.This(Name.Anonymous())")
    assert(q"$clazz.this".show[Structure] === "Term.This(Type.Name(\"C\"))")
  }

  test("1 q\"$qname.super[$qname].$id\"") {
    val q"$clazz.super[$tpe].$id" = q"A.super[B].x"
    assert(clazz.show[Structure] === "Name.Indeterminate(\"A\")")
    assert(tpe.show[Structure] === "Name.Indeterminate(\"B\")")
    assert(id.show[Structure] === "Term.Name(\"x\")")
  }

  test("2 q\"$qname.super[$qname].$id\"") {
    val clazz = q"A"
    val tpe = t"B"
    val id = q"x"
    // inconsistency with the test above planned, since Name.Indeterminate can't be constructed directly
    assert(q"$clazz.super[$tpe].m".show[Structure] === "Term.Select(Term.Super(Term.Name(\"A\"), Type.Name(\"B\")), Term.Name(\"m\"))")
  }

  test("1 super variants") {
    val q"super" = q"super"
    val q"super[$tpe1]" = q"super[M]"
    val q"$clazz1.super" = q"C.super"
    val q"$clazz2.super[$tpe2]" = q"C.super[M]"
    assert(tpe1.show[Structure] === "Name.Indeterminate(\"M\")")
    assert(tpe2.show[Structure] === "Name.Indeterminate(\"M\")")
    assert(clazz1.show[Structure] === "Name.Indeterminate(\"C\")")
    assert(clazz2.show[Structure] === "Name.Indeterminate(\"C\")")
  }

  test("2 super variants") {
    val clazz = t"C"
    val tpe = t"M"
    assert(q"super".show[Structure] === "Term.Super(Name.Anonymous(), Name.Anonymous())")
    assert(q"super[$tpe]".show[Structure] === "Term.Super(Name.Anonymous(), Type.Name(\"M\"))")
    assert(q"$clazz.super".show[Structure] === "Term.Super(Type.Name(\"C\"), Name.Anonymous())")
    assert(q"$clazz.super[$tpe]".show[Structure] === "Term.Super(Type.Name(\"C\"), Type.Name(\"M\"))")
  }

  test("1 q\"$expr.$name\"") {
    val q"$expr.$name" = q"foo.bar"
    assert(expr.show[Structure] === "Term.Name(\"foo\")")
    assert(name.show[Structure] === "Term.Name(\"bar\")")
  }

  test("2 q\"$expr.$name\"") {
    val expr = q"foo"
    val name = q"bar"
    assert(q"$expr.$name".show[Structure] === "Term.Select(Term.Name(\"foo\"), Term.Name(\"bar\"))")
  }

  test("1 q\"$expr($name)\"") {
    val q"$expr($name)" = q"foo(bar)"
    assert(expr.show[Structure] === "Term.Name(\"foo\")")
    assert(name.show[Structure] === "Term.Name(\"bar\")")
  }

  test("2 q\"$expr($name)\"") {
    val expr = q"foo"
    val name = q"bar"
    assert(q"$expr($name)".show[Structure] === "Term.Apply(Term.Name(\"foo\"), Seq(Term.Name(\"bar\")))")
  }

  test("1 q\"foo[..$tpes]\"") {
    val q"foo[..$types]" = q"foo[T, U]"
    assert(types.toString === "List(T, U)")
    assert(types(0).show[Structure] === "Type.Name(\"T\")")
    assert(types(1).show[Structure] === "Type.Name(\"U\")")
  }

  test("2 q\"foo[..$tpes]\"") {
    val types = List(t"T", t"U")
    assert(q"foo[..$types]".show[Structure] === "Term.ApplyType(Term.Name(\"foo\"), Seq(Type.Name(\"T\"), Type.Name(\"U\")))")
  }

  test("1 q\"$foo[..$tpes]\"") {
    val q"$foo[..$types]" = q"foo[T, U]"
    assert(foo.toString == "foo")
    assert(types.toString === "List(T, U)")
    assert(types(0).show[Structure] === "Type.Name(\"T\")")
    assert(types(1).show[Structure] === "Type.Name(\"U\")")
  }

  test("2 q\"$foo[..$tpes]\"") {
    val foo = q"foo"
    val types = List(t"T", t"U")
    assert(q"$foo[..$types]".show[Structure] === "Term.ApplyType(Term.Name(\"foo\"), Seq(Type.Name(\"T\"), Type.Name(\"U\")))")
  }

  test("1 q\"$expr $name[..$tpes] (..$aexprs)\"") {
    val q"$expr $name[..$tpes] (..$aexprs)" = q"x method[T, U] (1, b)"
    assert(expr.show[Structure] === "Term.Name(\"x\")")
    assert(name.show[Structure] === "Term.Name(\"method\")")
    assert(tpes.toString === "List(T, U)")
    assert(tpes(0).show[Structure] === "Type.Name(\"T\")")
    assert(tpes(1).show[Structure] === "Type.Name(\"U\")")
    assert(aexprs.toString === "List(1, b)")
    assert(aexprs(0).show[Structure] === "Lit(1)")
    assert(aexprs(1).show[Structure] === "Term.Name(\"b\")")
  }

  test("2 q\"$expr $name[..$tpes] (..$aexprs)\"") {
    val expr = q"x"
    val name = q"method"
    val tpes = List(t"T", t"U")
    val aexprs = List(q"1", q"b")
    assert(q"$expr $name[..$tpes] (..$aexprs)".show[Structure] === """Term.ApplyInfix(Term.Name("x"), Term.Name("method"), Seq(Type.Name("T"), Type.Name("U")), Seq(Lit(1), Term.Name("b")))""")
  }

  test("1 q\"$a $b $c\"") {
    val q"$a $b $c" = q"x y z"
    assert(a.show[Structure] === "Term.Name(\"x\")")
    assert(b.show[Structure] === "Term.Name(\"y\")")
    assert(c.show[Structure] === "Term.Name(\"z\")")
  }

  test("2 q\"$a $b $c\"") {
    val a = q"x"
    val b = q"y"
    val c = q"z"
    assert(q"$a $b $c".show[Structure] === "Term.ApplyInfix(Term.Name(\"x\"), Term.Name(\"y\"), Nil, Seq(Term.Name(\"z\")))")
  }

  test("1 q\"!$expr\"") {
    val q"!$x" = q"!foo"
    assert(x.show[Structure] === "Term.Name(\"foo\")")
  }

  test("2 q\"!$expr\"") {
    val x = q"foo"
    assert(q"!$x".show[Structure] === "Term.ApplyUnary(Term.Name(\"!\"), Term.Name(\"foo\"))")
  }

  test("1 q\"~$expr\"") {
    val q"~$x" = q"~foo"
    assert(x.show[Structure] === "Term.Name(\"foo\")")
  }

  test("2 q\"~$expr\"") {
    val expr = q"foo"
    assert(q"~$expr".show[Structure] === "Term.ApplyUnary(Term.Name(\"~\"), Term.Name(\"foo\"))")
  }

  test("1 q\"-$expr\"") {
    val q"-$x" = q"-foo"
    assert(x.show[Structure] === "Term.Name(\"foo\")")
  }

  test("2 q\"-$expr\"") {
    val x = q"foo"
    assert(q"-$x".show[Structure] === "Term.ApplyUnary(Term.Name(\"-\"), Term.Name(\"foo\"))")
  }

  test("1 q\"+$expr\"") {
    val q"+$x" = q"+foo"
    assert(x.show[Structure] === "Term.Name(\"foo\")")
  }

  test("2 q\"+$expr\"") {
    val x = q"foo"
    assert(q"+$x".show[Structure] === "Term.ApplyUnary(Term.Name(\"+\"), Term.Name(\"foo\"))")
  }

  test("1 q\"$ref = $expr\"") {
    val q"$ref = $expr" = q"a = b"
    assert(ref.show[Structure] === "Term.Name(\"a\")")
    assert(expr.show[Structure] === "Term.Name(\"b\")")
  }

  test("2 q\"$ref = $expr\"") {
    val ref = q"a"
    val expr = q"b"
    assert(q"$ref = $expr".show[Structure] === "Term.Assign(Term.Name(\"a\"), Term.Name(\"b\"))")
  }

  test("""1 val q"$x.$y = $z.$w" = q"a.b = c.d"""") {
    val q"$x.$y = $z.$w" = q"a.b = c.d"
    assert(x.show[Structure] === "Term.Name(\"a\")")
    assert(y.show[Structure] === "Term.Name(\"b\")")
    assert(z.show[Structure] === "Term.Name(\"c\")")
    assert(w.show[Structure] === "Term.Name(\"d\")")
  }

  test("""2 val q"$x.$y = $z.$w" = q"a.b = c.d"""") {
    val x = q"a"
    val y = q"b"
    val z = q"c"
    val w = q"d"
    assert(q"$x.$y = $z.$w".show[Structure] === "Term.Assign(Term.Select(Term.Name(\"a\"), Term.Name(\"b\")), Term.Select(Term.Name(\"c\"), Term.Name(\"d\")))")
  }

  test("q\"1 $expr(...$aexprs) = $expr\"") {
    val q"$expr1(...$aexprs) = $expr2" = q"foo(a, b) = bar"
    assert(expr1.show[Structure] === "Term.Name(\"foo\")")
    assert(aexprs.toString === "List(List(a, b))")
    assert(aexprs(0)(0).show[Structure] === "Term.Name(\"a\")")
    assert(aexprs(0)(1).show[Structure] === "Term.Name(\"b\")")
    assert(expr2.show[Structure] === "Term.Name(\"bar\")")
  }

  test("2 q\"$expr(...$aexprs) = $expr\"") {
    val expr1 = q"foo"
    val aexprs = List(List(q"a", q"b"))
    val expr2 = q"bar"
    assert(q"$expr1(...$aexprs) = $expr2".show[Structure] === "Term.Update(Term.Name(\"foo\"), Seq(Seq(Term.Name(\"a\"), Term.Name(\"b\"))), Term.Name(\"bar\"))")
  }

  test("1 q\"($x, y: Int)\"") {
    val q"($x, y: Int)" = q"(x: X, y: Int)"
    assert(x.show[Structure] === "Term.Ascribe(Term.Name(\"x\"), Type.Name(\"X\"))")
  }

  test("2 q\"($x, y: Int)\"") {
    val x = q"x: X"
    assert(q"($x, y: Int)".show[Structure] === "Term.Tuple(Seq(Term.Ascribe(Term.Name(\"x\"), Type.Name(\"X\")), Term.Ascribe(Term.Name(\"y\"), Type.Name(\"Int\"))))")
  }

  test("1 q\"f($q, y: Y)") {
    val q"f($q, y: Y) = $r" = q"f(x: X, y: Y) = 1"
    assert(q.show[Structure] === "Term.Ascribe(Term.Name(\"x\"), Type.Name(\"X\"))")
    assert(r.show[Structure] === "Lit(1)")
  }

  test("2 q\"f($q, y: Y)") {
    val q = q"x: X"
    val r = q"1"
    assert(q"f($q, y: Y) = $r".show[Structure] === "Term.Update(Term.Name(\"f\"), Seq(Seq(Term.Ascribe(Term.Name(\"x\"), Type.Name(\"X\")), Term.Ascribe(Term.Name(\"y\"), Type.Name(\"Y\")))), Lit(1))")
  }

  test("1 q\"return $expr\"") {
    val q"return $expr" = q"return foo == bar"
    assert(expr.show[Structure] === "Term.ApplyInfix(Term.Name(\"foo\"), Term.Name(\"==\"), Nil, Seq(Term.Name(\"bar\")))")
  }

  test("2 q\"return $expr\"") {
    val expr = q"foo == bar"
    assert(q"return $expr".show[Structure] === "Term.Return(Term.ApplyInfix(Term.Name(\"foo\"), Term.Name(\"==\"), Nil, Seq(Term.Name(\"bar\"))))")
  }

  test("1 q\"throw $expr\"") {
    val q"throw $expr" = q"throw new RuntimeException"
    assert(expr.show[Structure] === "Term.New(Template(Nil, Seq(Ctor.Ref.Name(\"RuntimeException\")), Term.Param(Nil, Name.Anonymous(), None, None), None))")
  }

  test("2 q\"throw $expr\"") {
    val expr = q"new RuntimeException"
    assert(q"throw $expr".show[Structure] === "Term.Throw(Term.New(Template(Nil, Seq(Ctor.Ref.Name(\"RuntimeException\")), Term.Param(Nil, Name.Anonymous(), None, None), None)))")
  }

  test("1 q\"$expr: $tpe\"") {
    val q"$exp: $tpe" = q"1: Double"
    assert(exp.show[Structure] === "Lit(1)")
    assert(tpe.show[Structure] === "Type.Name(\"Double\")")
  }

  test("2 q\"$expr: $tpe\"") {
    val exp = q"1"
    val tpe = t"Double"
    assert(q"$exp: $tpe".show[Structure] === "Term.Ascribe(Lit(1), Type.Name(\"Double\"))")
  }

  test("1 q\"$expr: ..$@annots\"") {
    val q"$exprr: @q ..@$annotz @$ar" = q"foo: @q @w @e @r"
    assert(exprr.show[Structure] === "Term.Name(\"foo\")")
    assert(annotz.toString === "List(@w, @e)")
    assert(annotz(0).show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"w\"))")
    assert(annotz(1).show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"e\"))")
    assert(ar.show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"r\"))")
  }

  test("2 q\"$expr: ..$@annots\"") {
    val mods = List(mod"@w", mod"@e")
    assert(q"foo: @q ..@$mods @r".show[Structure] === "Term.Annotate(Term.Name(\"foo\"), Seq(Mod.Annot(Ctor.Ref.Name(\"q\")), Mod.Annot(Ctor.Ref.Name(\"w\")), Mod.Annot(Ctor.Ref.Name(\"e\")), Mod.Annot(Ctor.Ref.Name(\"r\"))))")
  }

  test("q\"(..$exprs)\"") {
    val q"(..$terms)" = q"(y, z)"
    assert(terms.toString === "List(y, z)")
    assert(terms(0).show[Structure] === "Term.Name(\"y\")")
    assert(terms(1).show[Structure] === "Term.Name(\"z\")")
  }

  test("2 q\"(..$exprs)\"") {
    val terms = List(q"y", q"z")
    assert(q"(..$terms)".show[Structure] === "Term.Tuple(Seq(Term.Name(\"y\"), Term.Name(\"z\")))")
  }

  test("""1 val q"(..$params)" = q"(x: Int, y: String)" """) {
    val q"(..$params)" = q"(x: Int, y: String)"
    assert(params.toString === "List(x: Int, y: String)")
    assert(params(0).show[Structure] === "Term.Ascribe(Term.Name(\"x\"), Type.Name(\"Int\"))")
    assert(params(1).show[Structure] === "Term.Ascribe(Term.Name(\"y\"), Type.Name(\"String\"))")
  }

  test("""2 val q"(..$params)" = q"(x: Int, y: String)" """) {
    val params = List(q"x: Int", q"y: String")
    assert(q"(..$params)".show[Structure] === "Term.Tuple(Seq(Term.Ascribe(Term.Name(\"x\"), Type.Name(\"Int\")), Term.Ascribe(Term.Name(\"y\"), Type.Name(\"String\"))))")
  }

  test("1 q\"{ ..$stats }\"") {
    val q"{foo; ..$statz; $astat}" = q"{foo; val a = x; val b = y; val c = z}"
    assert(statz.toString === "List(val a = x, val b = y)")
    assert(statz(0).show[Structure] === "Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), None, Term.Name(\"x\"))")
    assert(statz(1).show[Structure] === "Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"b\"))), None, Term.Name(\"y\"))")
    assert(astat.show[Structure] === "Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"c\"))), None, Term.Name(\"z\"))")
  }

  test("2 q\"{ ..$stats }\"") {
    val stats = List(q"val x = 1", q"val y = 2")
    assert(q"{ ..$stats }".show[Structure] === "Term.Block(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"x\"))), None, Lit(1)), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"y\"))), None, Lit(2))))")
  }

  test("1 q\"if ($expr) $expr else $expr\"") {
    val q"if ($expr1) $expr2 else $expr3" = q"if (1 > 2) a else b"
    assert(expr1.show[Structure] === "Term.ApplyInfix(Lit(1), Term.Name(\">\"), Nil, Seq(Lit(2)))")
    assert(expr2.show[Structure] === "Term.Name(\"a\")")
    assert(expr3.show[Structure] === "Term.Name(\"b\")")
  }

  test("2 q\"if ($expr) $expr else $expr\"") {
    val expr1 = q"1 > 2"
    val expr2 = q"a"
    val expr3 = q"b"
    assert(q"if ($expr1) $expr2 else $expr3".show[Structure] === "Term.If(Term.ApplyInfix(Lit(1), Term.Name(\">\"), Nil, Seq(Lit(2))), Term.Name(\"a\"), Term.Name(\"b\"))")
  }

  test("1 q\"$expr match { ..case $cases }\"") {
    val q"$expr match { case bar => baz; ..case $casez; case q => w}" = q"foo match { case bar => baz; case _ => foo ; case q => w }"
    assert(expr.show[Structure] === "Term.Name(\"foo\")")
    assert(casez.toString === "List(case _ => foo)")
    assert(casez(0).show[Structure] === "Case(Pat.Wildcard(), None, Term.Name(\"foo\"))")
  }

  test("2 q\"$expr match { ..case $cases }\"") {
    val q"$expr match { case bar => baz; ..case $casez; case _ => foo }" = q"foo match { case bar => baz; case _ => foo }"
    assert(expr.show[Structure] === "Term.Name(\"foo\")")
    assert(casez.isEmpty)
  }

  test("3 q\"$expr match { ..case $cases }\"") {
    val q"$expr match { ..case $casez }" = q"foo match { case bar => baz; case _ => foo }"
    assert(expr.show[Structure] === "Term.Name(\"foo\")")
    assert(casez.toString === "List(case bar => baz, case _ => foo)")
    assert(casez(0).show[Structure] === "Case(Pat.Var.Term(Term.Name(\"bar\")), None, Term.Name(\"baz\"))")
    assert(casez(1).show[Structure] === "Case(Pat.Wildcard(), None, Term.Name(\"foo\"))")
  }

  test("4 q\"$expr match { ..case $cases }\"") {
    val expr = q"foo"
    val casez = List(p"case a => b", p"case q => w")
    assert(q"$expr match { ..case $casez }".show[Structure] === "Term.Match(Term.Name(\"foo\"), Seq(Case(Pat.Var.Term(Term.Name(\"a\")), None, Term.Name(\"b\")), Case(Pat.Var.Term(Term.Name(\"q\")), None, Term.Name(\"w\"))))")
  }

  test("1 q\"try $expr catch { ..case $cases } finally $expropt\"") {
    val q"try $expr catch { case $case1 ..case $cases; case $case2 } finally $expropt" = q"try foo catch { case a => b; case _ => bar; case 1 => 2; case q => w} finally baz"
    assert(expr.show[Structure] === "Term.Name(\"foo\")")
    assert(cases.toString === "List(case _ => bar, case 1 => 2)")
    assert(cases(0).show[Structure] === "Case(Pat.Wildcard(), None, Term.Name(\"bar\"))")
    assert(cases(1).show[Structure] === "Case(Lit(1), None, Lit(2))")
    assert(case1.show[Structure] === "Case(Pat.Var.Term(Term.Name(\"a\")), None, Term.Name(\"b\"))")
    assert(case2.show[Structure] === "Case(Pat.Var.Term(Term.Name(\"q\")), None, Term.Name(\"w\"))")
    assert(expropt.show[Structure] === "Some(Term.Name(\"baz\"))")
  }

  test("2 q\"try $expr catch { ..case $cases } finally $expropt\"") {
    val expr = q"foo"
    val cases = List(p"case _ => bar", p"case 1 => 2")
    val case1 = p"case a => b"
    val case2 = p"case q => w"
    val expropt = q"baz"
    assert(q"try $expr catch { case $case1 ..case $cases; case $case2 } finally $expropt".show[Structure] === "Term.TryWithCases(Term.Name(\"foo\"), Seq(Case(Pat.Var.Term(Term.Name(\"a\")), None, Term.Name(\"b\")), Case(Pat.Wildcard(), None, Term.Name(\"bar\")), Case(Lit(1), None, Lit(2)), Case(Pat.Var.Term(Term.Name(\"q\")), None, Term.Name(\"w\"))), Some(Term.Name(\"baz\")))")
  }

  test("1 q\"try $expr catch $expr finally $expropt\"") {
    val q"try $expr catch $exprr finally $expropt" = q"try { foo } catch { pf } finally { bar }"
    assert(expr.show[Structure] === "Term.Block(Seq(Term.Name(\"foo\")))")
    assert(exprr.show[Structure] === "Term.Name(\"pf\")")
    assert(expropt.show[Structure] === "Some(Term.Block(Seq(Term.Name(\"bar\"))))")
  }

  test("2 q\"try $expr catch $expr finally $expropt\"") {
    val expr = q"{ foo }"
    val exprr = q"pf"
    val expropt = q"{ bar }"
    assert(q"try $expr catch $exprr finally $expropt".show[Structure] === "Term.TryWithTerm(Term.Block(Seq(Term.Name(\"foo\"))), Term.Name(\"pf\"), Some(Term.Block(Seq(Term.Name(\"bar\")))))")
  }

  test("""q"(i: Int) => 42" """) {
    assert(q"(i: Int) => 42".show[Structure] === "Term.Function(Seq(Term.Param(Nil, Term.Name(\"i\"), Some(Type.Name(\"Int\")), None)), Lit(42))")
  }

  test("1 q\"(..$params) => $expr\"") {
    val q"(..$paramz) => $expr" = q"(x: Int, y: String) => 42"
    assert(paramz.toString === "List(x: Int, y: String)")
    assert(paramz(0).show[Structure] === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Int\")), None)")
    assert(paramz(1).show[Structure] === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"String\")), None)")
    assert(expr.show[Structure] === "Lit(42)")
  }

  test("2 q\"(..$params) => $expr\"") {
    val paramz = List(param"x: Int", param"y: String")
    val expr = q"42"
    assert(q"(..$paramz) => $expr".show[Structure] === "Term.Function(Seq(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Int\")), None), Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"String\")), None)), Lit(42))")
  }

  test("1 val q\"(..$q, y: Y, $e) => $r\" = q\"(x: X, y: Y, z: Z) => 1\"") {
    val q"(..$q, y: Y, $e) => $r" = q"(x: X, y: Y, z: Z) => 1"
    assert(q.toString === "List(x: X)")
    assert(q(0).show[Structure] === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(e.show[Structure] === "Term.Param(Nil, Term.Name(\"z\"), Some(Type.Name(\"Z\")), None)")
    assert(r.show[Structure] === "Lit(1)")
  }

  test("2 val q\"(..$q, y: Y, $e) => $r\" = q\"(x: X, y: Y, z: Z) => 1\"") {
    val q = List(param"x: X")
    val e = param"z: Z"
    val r = q"1"
    assert(q"(..$q, y: Y, $e) => $r".show[Structure] === "Term.Function(Seq(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None), Term.Param(Nil, Term.Name(\"z\"), Some(Type.Name(\"Z\")), None)), Lit(1))")
  }

  test("1 q\"{ ..case $cases }\"") {
    val q"{ ..case $cases }" = q"{ case i: Int => i + 1 }"
    assert(cases(0).show[Structure] === "Case(Pat.Typed(Pat.Var.Term(Term.Name(\"i\")), Type.Name(\"Int\")), None, Term.ApplyInfix(Term.Name(\"i\"), Term.Name(\"+\"), Nil, Seq(Lit(1))))")
  }

  test("2 q\"{ ..case $cases }\"") {
    val cases = List(p"case i: Int => i + 1")
    assert(q"{ ..case $cases }".show[Structure] === "Term.PartialFunction(Seq(Case(Pat.Typed(Pat.Var.Term(Term.Name(\"i\")), Type.Name(\"Int\")), None, Term.ApplyInfix(Term.Name(\"i\"), Term.Name(\"+\"), Nil, Seq(Lit(1))))))")
  }

  test("1 q\"while ($expr) $expr\"") {
    val q"while ($expr1) $expr2" = q"while (foo) bar"
    assert(expr1.show[Structure] === "Term.Name(\"foo\")")
    assert(expr2.show[Structure] === "Term.Name(\"bar\")")
  }

  test("2 q\"while ($expr) $expr\"") {
    val expr1 = q"foo"
    val expr2 = q"bar"
    assert(q"while ($expr1) $expr2".show[Structure] === "Term.While(Term.Name(\"foo\"), Term.Name(\"bar\"))")
  }

  test("1 q\"do $expr while($expr)\"") {
    val q"do $expr1 while($expr2)" = q"do foo while (bar)"
    assert(expr1.show[Structure] === "Term.Name(\"foo\")")
    assert(expr2.show[Structure] === "Term.Name(\"bar\")")
  }

  test("2 q\"do $expr while($expr)\"") {
    val expr1 = q"foo"
    val expr2 = q"bar"
    assert(q"do $expr1 while($expr2)".show[Structure] === "Term.Do(Term.Name(\"foo\"), Term.Name(\"bar\"))")
  }

  test("1 q\"for (..$enumerators) $expr\"") {
    val q"for ($enum1; ..$enumerators; if $cond; $enum2) $exprr" = q"for (a <- as; x <- xs; y <- ys; if bar; b <- bs) foo(x, y)"
    assert(enumerators.toString === "List(x <- xs, y <- ys)")
    assert(enumerators(0).show[Structure] === "Enumerator.Generator(Pat.Var.Term(Term.Name(\"x\")), Term.Name(\"xs\"))")
    assert(enumerators(1).show[Structure] === "Enumerator.Generator(Pat.Var.Term(Term.Name(\"y\")), Term.Name(\"ys\"))")
    assert(cond.show[Structure] === "Term.Name(\"bar\")")
    assert(enum1.show[Structure] === "Enumerator.Generator(Pat.Var.Term(Term.Name(\"a\")), Term.Name(\"as\"))")
    assert(enum2.show[Structure] === "Enumerator.Generator(Pat.Var.Term(Term.Name(\"b\")), Term.Name(\"bs\"))")
    assert(exprr.show[Structure] === "Term.Apply(Term.Name(\"foo\"), Seq(Term.Name(\"x\"), Term.Name(\"y\")))")
  }

  test("2 q\"for (..$enumerators) $expr\"") {
    val a = enumerator"a <- as"
    val b = enumerator"b <- bs"
    val ab = List(a,b)
    assert(q"for (..$ab) foo".show[Structure] === "Term.For(Seq(Enumerator.Generator(Pat.Var.Term(Term.Name(\"a\")), Term.Name(\"as\")), Enumerator.Generator(Pat.Var.Term(Term.Name(\"b\")), Term.Name(\"bs\"))), Term.Name(\"foo\"))")
  }

//  test("3 q\"for (..$enumerators) $expr\"") {
//    val q"for (a <- as; if $cond; ..$enums) bar" = q"for (a <- as; if foo; b <- bs) bar" // TODO review after #203 resolved
//  }

  test("1 q\"for (..$enumerators) yield $expr\"") {
    val q"for (a <- as; ..$enumerators; b <- bs) yield $expr" = q"for (a <- as; x <- xs; y <- ys; b <- bs) yield foo(x, y)"
    assert(enumerators.toString === "List(x <- xs, y <- ys)")
    assert(enumerators(0).show[Structure] === "Enumerator.Generator(Pat.Var.Term(Term.Name(\"x\")), Term.Name(\"xs\"))")
    assert(enumerators(1).show[Structure] === "Enumerator.Generator(Pat.Var.Term(Term.Name(\"y\")), Term.Name(\"ys\"))")
    assert(expr.show[Structure] === "Term.Apply(Term.Name(\"foo\"), Seq(Term.Name(\"x\"), Term.Name(\"y\")))")
  }

  test("2 q\"for (..$enumerators) yield $expr\"") {
    val a = enumerator"a <- as"
    val b = enumerator"b <- bs"
    val ab = List(a,b)
    assert(q"for (..$ab) yield foo".show[Structure] === "Term.ForYield(Seq(Enumerator.Generator(Pat.Var.Term(Term.Name(\"a\")), Term.Name(\"as\")), Enumerator.Generator(Pat.Var.Term(Term.Name(\"b\")), Term.Name(\"bs\"))), Term.Name(\"foo\"))")
  }

  test("1 q\"new { ..$stat } with ..$exprs { $param => ..$stats }\"") {
    val q"new $x" = q"new Foo"
    assert(x.show[Structure] === "Template(Nil, Seq(Ctor.Ref.Name(\"Foo\")), Term.Param(Nil, Name.Anonymous(), None, None), None)")
  }

  test("2 q\"new { ..$stat } with ..$exprs { $param => ..$stats }\"") {
    val q"new {..$stats; val b = 4} with $a {$selff => ..$statz}" = q"new {val a = 2; val b = 4} with A { self => val b = 3 }"
    assert(stats.toString === "List(val a = 2)")
    assert(stats(0).show[Structure] === "Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), None, Lit(2))")
    assert(a.show[Structure] === "Ctor.Ref.Name(\"A\")")
    assert(selff.show[Structure] === "Term.Param(Nil, Term.Name(\"self\"), None, None)")
    assert(statz.toString === "List(val b = 3)")
    assert(statz(0).show[Structure] === "Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"b\"))), None, Lit(3))")
  }

  test("3 q\"new { ..$stat } with ..$exprs { $param => ..$stats }\"") {
    val q"new X with T { $self => def m = 42}" = q"new X with T { def m = 42 }"
    assert(self.show[Structure] === "Term.Param(Nil, Name.Anonymous(), None, None)")
  }

  test("4 q\"new { ..$stat } with ..$exprs { $param => ..$stats }\"") {
    val stats = List(q"val a = 2")
    val a = ctor"A"
    val selff = param"self: A"
    val statz = List(q"val b = 3")
    assert(q"new {..$stats; val b = 4} with $a {$selff => ..$statz}".show[Structure] === "Term.New(Template(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), None, Lit(2)), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"b\"))), None, Lit(4))), Seq(Ctor.Ref.Name(\"A\")), Term.Param(Nil, Term.Name(\"self\"), Some(Type.Name(\"A\")), None), Some(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"b\"))), None, Lit(3))))))")
  }

  test("q\"_\"") {
    assert(q"_".show[Structure] === "Term.Placeholder()")
  }

  test("1 q\"$expr _\"") {
    val q"$expr _" = q"foo _"
    assert(expr.show[Structure] === "Term.Name(\"foo\")")
  }

  test("2 q\"$expr _\"") {
    val expr = q"foo"
    assert(q"$expr _".show[Structure] === "Term.Eta(Term.Name(\"foo\"))")
  }

  test("1 q\"$lit\"") {
    val q"$x" = q"42"
    assert(x.show[Structure] === "Lit(42)")
  }

  test("2 q\"$lit\"") {
    val lit = q"42"
    assert(q"$lit".show[Structure] === "Lit(42)")
  }

  test("1 arg\"$name = $expr\"") {
    val arg"$name = $expr" = q"x = foo"
    assert(name.show[Structure] === "Term.Name(\"x\")")
    assert(expr.show[Structure] === "Term.Name(\"foo\")")
  }

  test("2 arg\"$name = $expr\"") {
    val name = q"x"
    val expr = q"foo"
    assert(arg"$name = $expr".show[Structure] === "Term.Assign(Term.Name(\"x\"), Term.Name(\"foo\"))")
  }

  test("1 arg\"$expr: _*\"") {
    // q"foo: _*" should not parse, so wrap into apply
    val arg"f($expr: _*)" = q"f(foo: _*)"
    assert(expr.show[Structure] === "Term.Name(\"foo\")")
  }

  test("2 arg\"$expr: _*\"") {
    val expr = q"foo"
    assert(arg"$expr: _*".show[Structure] === "Term.Arg.Repeated(Term.Name(\"foo\"))")
  }

  test("arg\"$expr\"") {
    val expr = q"foo"
    assert(arg"$expr".show[Structure] === "Term.Name(\"foo\")")
  }

  test("1 t\"$ref.$tname\"") {
    val t"$ref.$tname" = t"X.Y"
    assert(ref.show[Structure] === "Term.Name(\"X\")")
    assert(tname.show[Structure] === "Type.Name(\"Y\")")
  }

  test("2 t\"$ref.$tname\"") {
    val ref = q"X"
    val tname = t"Y"
    assert(t"$ref.$tname".show[Structure] === "Type.Select(Term.Name(\"X\"), Type.Name(\"Y\"))")
  }

  test("1 t\"$tpe#$tname\"") {
    val t"$tpe#$tname" = t"X#Y"
    assert(tpe.show[Structure] === "Type.Name(\"X\")")
    assert(tname.show[Structure] === "Type.Name(\"Y\")")
  }

  test("2 t\"$tpe#$tname\"") {
    val tpe = t"X"
    val tname = t"Y"
    assert(t"$tpe#$tname".show[Structure] === "Type.Project(Type.Name(\"X\"), Type.Name(\"Y\"))")
  }

  test("1 t\"$ref.type\"") {
    val t"$ref.type" = t"X.type"
    assert(ref.show[Structure] === "Term.Name(\"X\")")
  }

  test("2 t\"$ref.type\"") {
    val ref = q"X"
    assert(t"$ref.type".show[Structure] === "Type.Singleton(Term.Name(\"X\"))")
  }

  test("1 t\"$tpe[..$tpes]") {
    val t"$tpe[..$tpes]" = t"X[Y, Z]"
    assert(tpe.show[Structure] === "Type.Name(\"X\")")
    assert(tpes.toString === "List(Y, Z)")
    assert(tpes(0).show[Structure] === "Type.Name(\"Y\")")
    assert(tpes(1).show[Structure] === "Type.Name(\"Z\")")
  }

  test("2 t\"$tpe[..$tpes]") {
    val tpe = t"X"
    val tpes = List(t"Y", t"Z")
    assert(t"$tpe[..$tpes]".show[Structure] === "Type.Apply(Type.Name(\"X\"), Seq(Type.Name(\"Y\"), Type.Name(\"Z\")))")
  }

  test("1 t\"$tpe $tname $tpe\"") {
    val t"$tpe1 $tname $tpe2" = t"X Y Z"
    assert(tpe1.show[Structure] === "Type.Name(\"X\")")
    assert(tname.show[Structure] === "Type.Name(\"Y\")")
    assert(tpe2.show[Structure] === "Type.Name(\"Z\")")
  }

  test("2 t\"$tpe $tname $tpe\"") {
    val tpe1 = t"X"
    val tname = t"Y"
    val tpe2 = t"Z"
    assert(t"$tpe1 $tname $tpe2".show[Structure] === "Type.ApplyInfix(Type.Name(\"X\"), Type.Name(\"Y\"), Type.Name(\"Z\"))")
  }

  test("1 t\"(..$atpes) => $tpe\"") {
    val t"(..$atpes) => $tpe" = t"(X, Y) => Z"
    assert(atpes.toString === "List(X, Y)")
    assert(atpes(0).show[Structure] === "Type.Name(\"X\")")
    assert(atpes(1).show[Structure] === "Type.Name(\"Y\")")
    assert(tpe.show[Structure] === "Type.Name(\"Z\")")
  }

  test("2 t\"(..$atpes) => $tpe\"") {
    val atpes: List[Type.Arg] = List(t"X", t"Y")
    val tpe = t"Z"
    assert(t"(..$atpes) => $tpe".show[Structure] === "Type.Function(Seq(Type.Name(\"X\"), Type.Name(\"Y\")), Type.Name(\"Z\"))")
  }

  test("1 t\"(..$tpes)\"") {
    val t"(..$tpes)" = t"(X, Y)"
    assert(tpes.toString === "List(X, Y)")
    assert(tpes(0).show[Structure] === "Type.Name(\"X\")")
    assert(tpes(1).show[Structure] === "Type.Name(\"Y\")")
  }

  test("t\"(..$tpes)\"") {
    val tpes = List(t"X", t"Y")
    assert(t"(..$tpes)".show[Structure] === "Type.Tuple(Seq(Type.Name(\"X\"), Type.Name(\"Y\")))")
  }

  test("1 t\"..$tpes { ..$stats }\"") {
    val t"..$tpes {..$stats}" = t"A with B with C { val a: A; val b: B }"
    assert(tpes.toString === "List(A, B, C)")
    assert(tpes(0).show[Structure] === "Type.Name(\"A\")")
    assert(tpes(1).show[Structure] === "Type.Name(\"B\")")
    assert(tpes(2).show[Structure] === "Type.Name(\"C\")")
    assert(stats.toString === "List(val a: A, val b: B)")
    assert(stats(0).show[Structure] === "Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), Type.Name(\"A\"))")
    assert(stats(1).show[Structure] === "Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"b\"))), Type.Name(\"B\"))")
  }

  test("2 t\"..$tpes { ..$stats }\"") {
    val tpes = List(t"X", t"Y")
    val stats = List(q"val a: A", q"val b: B")
    assert(t"..$tpes { ..$stats }".show[Structure] === "Type.Compound(Seq(Type.Name(\"X\"), Type.Name(\"Y\")), Seq(Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), Type.Name(\"A\")), Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"b\"))), Type.Name(\"B\"))))")
  }

  test("1 t\"$tpe forSome { ..$stats }\"") {
    import scala.language.existentials
    val t"$tpe forSome { ..$stats }" = t"X forSome { val a: A; val b: B }"
    assert(tpe.show[Structure] === "Type.Name(\"X\")")
    assert(stats.toString === "List(val a: A, val b: B)")
    assert(stats(0).show[Structure] === "Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), Type.Name(\"A\"))")
    assert(stats(1).show[Structure] === "Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"b\"))), Type.Name(\"B\"))")
  }

  test("2 t\"$tpe forSome { ..$stats }\"") {
    val tpe = t"X"
    val stats = List(q"val a:A", q"val b:B")
    assert(t"$tpe forSome { ..$stats }".show[Structure] === "Type.Existential(Type.Name(\"X\"), Seq(Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), Type.Name(\"A\")), Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"b\"))), Type.Name(\"B\"))))")
  }

  test("1 t\"$tpe ..@$annots\"") {
    val t"$tpe ..@$annots" = t"X @a @b"
    assert(tpe.show[Structure] === "Type.Name(\"X\")")
    assert(annots.toString === "List(@a, @b)")
    assert(annots(0).show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"a\"))")
    assert(annots(1).show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"b\"))")
  }

  test("2 t\"$tpe ..@$annots\"") {
    val tpe = t"X"
    val annots = List(mod"@a", mod"@b")
    assert(t"$tpe ..@$annots".show[Structure] === "Type.Annotate(Type.Name(\"X\"), Seq(Mod.Annot(Ctor.Ref.Name(\"a\")), Mod.Annot(Ctor.Ref.Name(\"b\"))))")
  }

  test("1 t\"_ >: $tpeopt <: $tpeopt\"") {
    val t"_ >: $tpe1 <: $tpe2" = t"_ >: X <: Y"
    assert(tpe1.show[Structure] === "Some(Type.Name(\"X\"))")
    assert(tpe2.show[Structure] === "Some(Type.Name(\"Y\"))")
  }

  test("2 t\"_ >: $tpeopt <: $tpeopt\"") {
    val tpe1 = t"X"
    val tpe2 = t"Y"
    assert(t"_ >: $tpe1 <: $tpe2".show[Structure] === "Type.Placeholder(Type.Bounds(Some(Type.Name(\"X\")), Some(Type.Name(\"Y\"))))")
  }

  test("t\"$lit\"") {
    val lit = q"1"
    assert(t"$lit".show[Structure] === "Lit(1)")
  }

  test("1 t\"=> $tpe\"") {
    val targ"=> $tpe" = targ"=> X"
    assert(tpe.show[Structure] === "Type.Name(\"X\")")
  }

  test("2 t\"=> $tpe\"") {
    val tpe = t"X"
    assert(targ"=> $tpe".show[Structure] === "Type.Arg.ByName(Type.Name(\"X\"))")
  }

  test("1 t\"$tpe *\"") {
    val targ"$tpe*" = targ"X*"
    assert(tpe.show[Structure] === "Type.Name(\"X\")")
  }

  test("2 t\"$tpe *\"") {
    val tpe = t"X"
    assert(targ"$tpe*".show[Structure] === "Type.Arg.Repeated(Type.Name(\"X\"))")
  }

  test("1 t\"$tpe\"") {
    val t"$tpe" = t"X"
    assert(tpe.show[Structure] === "Type.Name(\"X\")")
  }

  test("2 t\"$tpe\"") {
    val tpe = t"X"
    assert(t"$tpe".show[Structure] === "Type.Name(\"X\")")
  }

  test("p\"_\"") {
    assert(p"_".show[Structure] === "Pat.Wildcard()")
  }

  test("p\"name\"") {
    assert(p"name".show[Structure] === "Pat.Var.Term(Term.Name(\"name\"))")
  }

  test("p\"x\"") {
    assert(p"x".show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
  }

  test("p\"X\"") {
    assert(p"X".show[Structure] ===  "Pat.Var.Term(Term.Name(\"X\"))")
  }

  test("p\"`x`\"") {
    assert(p"`x`".show[Structure] === "Term.Name(\"x\")")
  }

  test("p\"`X`\"") {
    assert(p"`X`".show[Structure] === "Term.Name(\"X\")")
  }

  test("1 p\"$pname @ $apat\"") {
    val p"$pname @ $apat" = p"x @ y"
    assert(pname.show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
    assert(apat.show[Structure] === "Pat.Var.Term(Term.Name(\"y\"))")
  }

  test("2 p\"$pname @ $apat\"") {
    val pname = p"x"
    val apat = p"y"
    assert(p"$pname @ $apat".show[Structure] === "Pat.Bind(Pat.Var.Term(Term.Name(\"x\")), Pat.Var.Term(Term.Name(\"y\")))")
  }

  test("1 p\"$pat | $pat\"") {
    val p"$pat1 | $pat2" = p"x | y"
    assert(pat1.show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
    assert(pat2.show[Structure] === "Pat.Var.Term(Term.Name(\"y\"))")
  }

  test("2 p\"$pat | $pat\"") {
    val pat1 = q"X"
    val pat2 = q"Y"
    assert(p"$pat1 | $pat2".show[Structure] === "Pat.Alternative(Term.Name(\"X\"), Term.Name(\"Y\"))")
  }

  test("3 p\"$pat | $pat\"") {
    val pat1 = p"`X`"
    val pat2 = q"Y"
    assert(p"$pat1 | $pat2".show[Structure] === "Pat.Alternative(Term.Name(\"X\"), Term.Name(\"Y\"))")
  }

  test("1 p\"(..$pats)\"") {
    val p"(..$pats)" = p"(X, Y)"
    assert(pats.toString === "List(X, Y)")
    assert(pats(0).show[Structure] === "Term.Name(\"X\")")
    assert(pats(1).show[Structure] === "Term.Name(\"Y\")")
  }

  test("2 p\"(..$pats)\"") {
    val pats = List(p"x", p"y")
    assert(p"(..$pats)".show[Structure] === "Pat.Tuple(Seq(Pat.Var.Term(Term.Name(\"x\")), Pat.Var.Term(Term.Name(\"y\"))))")
  }

  test("3 p\"(..$pats)\"") {
    val pats = List(p"`X`", q"Y")
    assert(p"(..$pats)".show[Structure] === "Pat.Tuple(Seq(Term.Name(\"X\"), Term.Name(\"Y\")))")
  }

  test("1 p\"$ref[..$tpes](..$apats)\"") {
    val p"$ref[..$tpes](..$apats)" = p"x[A, B](Q, W)"
    assert(ref.show[Structure] === "Term.Name(\"x\")")
    assert(tpes.toString === "List(A, B)")
    assert(tpes(0).show[Structure] === "Type.Name(\"A\")")
    assert(tpes(1).show[Structure] === "Type.Name(\"B\")")
    assert(apats.toString === "List(Q, W)")
    assert(apats(0).show[Structure] === "Term.Name(\"Q\")")
    assert(apats(1).show[Structure] === "Term.Name(\"W\")")
  }

  test("2 p\"$ref[..$tpes](..$apats)\"") {
    val p"$ref[..$tpes](..$apats)" = p"x(Q, W)"
    assert(ref.show[Structure] === "Term.Name(\"x\")")
    assert(tpes.toString === "List()")
    assert(apats.toString === "List(Q, W)")
    assert(apats(0).show[Structure] === "Term.Name(\"Q\")")
    assert(apats(1).show[Structure] === "Term.Name(\"W\")")
  }

  test("3 p\"$ref[..$tpes](..$apats)\"") {
    val ref = q"x"
    val tpes = List(t"A", t"B")
    val apats = List(q"Q", q"W")
    assert(p"$ref[..$tpes](..$apats)".show[Structure] === "Pat.Extract(Term.Name(\"x\"), Seq(Type.Name(\"A\"), Type.Name(\"B\")), Seq(Term.Name(\"Q\"), Term.Name(\"W\")))")
  }

  test("4 p\"$ref[..$tpes](..$apats)\"") {
    val ref = q"`x`"
    val tpes = List(t"`A`", t"B")
    val apats = List(p"`Q`", q"W")
    assert(p"$ref[..$tpes](..$apats)".show[Structure] === "Pat.Extract(Term.Name(\"x\"), Seq(Type.Name(\"A\"), Type.Name(\"B\")), Seq(Term.Name(\"Q\"), Term.Name(\"W\")))")
  }

  test("1 p\"$pat $name (..$apats)\"") {
    val p"$pat $name (..$apats)" = p"x y (Q, W)"
    assert(pat.show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
    assert(name.show[Structure] === "Term.Name(\"y\")")
    assert(apats.toString === "List(Q, W)")
    assert(apats(0).show[Structure] === "Term.Name(\"Q\")")
    assert(apats(1).show[Structure] === "Term.Name(\"W\")")
  }

  test("2 p\"$pat $name (..$apats)\"") {
    val pat = p"x"
    val name = q"y"
    val apats = List(q"Q", q"W")
    assert(p"$pat $name (..$apats)".show[Structure] === "Pat.ExtractInfix(Pat.Var.Term(Term.Name(\"x\")), Term.Name(\"y\"), Seq(Term.Name(\"Q\"), Term.Name(\"W\")))")
  }

  test("3 p\"$pat $name (..$apats)\"") {
    val pat = p"`x`"
    val name = q"y"
    val apats = List(q"Q", q"W")
    assert(p"$pat $name (..$apats)".show[Structure] === "Pat.ExtractInfix(Term.Name(\"x\"), Term.Name(\"y\"), Seq(Term.Name(\"Q\"), Term.Name(\"W\")))")
  }

  test("1 p\"$pat: $ptpe\"") {
    val p"$pat: $ptpe" = p"x: Y"
    assert(pat.show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
    assert(ptpe.show[Structure] === "Type.Name(\"Y\")")
  }

  test("2 p\"$pat: $ptpe\"") {
    val pat = p"x"
    val ptpe = t"Y"
    assert(p"$pat: $ptpe".show[Structure] === "Pat.Typed(Pat.Var.Term(Term.Name(\"x\")), Type.Name(\"Y\"))")
  }

  test("1 p\"$expr.$name\"") {
    val p"$expr.$name" = p"x.y"
    assert(expr.show[Structure] === "Term.Name(\"x\")")
    assert(name.show[Structure] === "Term.Name(\"y\")")
  }

  test("2 p\"$expr.$name\"") {
    val expr = q"x"
    val name = q"y"
    assert(p"$expr.$name".show[Structure] === "Term.Select(Term.Name(\"x\"), Term.Name(\"y\"))")
  }

  test("3 p\"$expr.$name\"") {
    val expr = q"`x`"
    val name = q"y"
    assert(p"$expr.$name".show[Structure] === "Term.Select(Term.Name(\"x\"), Term.Name(\"y\"))")
  }

  test("p\"$lit\"") {
    val lit = q"1"
    assert(p"$lit".show[Structure] === "Lit(1)")
  }

  test("1 p\"case $pat if $expropt => $expr\"") {
    val p"case $pat if $expropt => $expr" = p"case X if foo => bar"
    assert(pat.show[Structure] === "Term.Name(\"X\")")
    assert(expropt.show[Structure] === "Some(Term.Name(\"foo\"))")
    assert(expr.show[Structure] === "Term.Name(\"bar\")")
  }

  test("2 p\"case $pat if $expropt => $expr\"") {
    val pat = q"X"
    val expropt = q"foo"
    val expr = q"bar"
    assert(p"case $pat if $expropt => $expr".show[Structure] === "Case(Term.Name(\"X\"), Some(Term.Name(\"foo\")), Term.Name(\"bar\"))")
  }

  test("3 p\"case $pat if $expropt => $expr\"") {
    val pat = p"`X`"
    val expropt = q"`foo`"
    val expr = q"`bar`"
    assert(p"case $pat if $expropt => $expr".show[Structure] === "Case(Term.Name(\"X\"), Some(Term.Name(\"foo\")), Term.Name(\"bar\"))")
  }

  test("p\"_*\"") {
    assert(p"case List(_*) =>".show[Structure] === "Case(Pat.Extract(Term.Name(\"List\"), Nil, Seq(Pat.Arg.SeqWildcard())), None, Term.Block(Nil))")
  }

  test("parg\"_*\"") {
    assert(parg"_*".show[Structure] === "Pat.Wildcard()")
  }

  test("1 p\"$pat\"") {
    val pat = p"X"
    assert(p"$pat".show[Structure] === "Pat.Var.Term(Term.Name(\"X\"))")
  }

  test("2 p\"$pat\"") {
    val pat = p"`X`"
    assert(p"$pat".show[Structure] === "Term.Name(\"X\")")
  }

  test("pt\"_\"") {
    assert(pt"_".show[Structure] === "Pat.Type.Wildcard()")
  }

  test("pt\"x\"") {
    assert(pt"x".show[Structure] === "Pat.Var.Type(Type.Name(\"x\"))")
  }

  // NOTE: compilation error
  // test("pt\"X\"") {
  //   assert(pt"X".show[Structure] === "Pat.Var.Type(Type.Name(\"X\"))")
  // }

  // NOTE: compilation error
  // test("pt\"`x`\"") {
  //   assert(pt"`x`".show[Structure] === "Type.Name(\"x\")")
  // }

  // NOTE: compilation error
  // test("pt\"`X`\"") {
  //   assert(pt"`X`".show[Structure] === "Type.Name(\"X\")")
  // }

  test("1 pt\"$ref.$tname\"") {
    val pt"$ref.$tname" = pt"x.a"
    assert(ref.show[Structure] === "Term.Name(\"x\")")
    assert(tname.show[Structure] === "Type.Name(\"a\")")
  }

  test("2 pt\"$ref.$tname\"") {
    val ref = q"x"
    val tname = t"a"
    assert(pt"$ref.$tname".show[Structure] === "Type.Select(Term.Name(\"x\"), Type.Name(\"a\"))")
  }

  test("3 pt\"$ref.$tname\"") {
    val ref = q"`x`"
    val tname = t"`a`"
    assert(pt"$ref.$tname".show[Structure] === "Type.Select(Term.Name(\"x\"), Type.Name(\"a\"))")
  }

  test("1 pt\"$ptpe#$tname\"") {
    val pt"$ptpe#$tname" = pt"X#a"
    assert(ptpe.show[Structure] === "Type.Name(\"X\")")
    assert(tname.show[Structure] === "Type.Name(\"a\")")
  }

  test("2 pt\"$ptpe#$tname\"") {
    val ptpe = t"X"
    val tname = t"a"
    assert(pt"$ptpe#$tname".show[Structure] === "Pat.Type.Project(Type.Name(\"X\"), Type.Name(\"a\"))")
  }

  test("3 pt\"$ptpe#$tname\"") {
    val ptpe = t"`x`"
    val tname = t"a"
    assert(pt"$ptpe#$tname".show[Structure] === "Pat.Type.Project(Type.Name(\"x\"), Type.Name(\"a\"))")
  }

  test("4 pt\"$ptpe#$tname\"") {
    val ptpe = t"x"
    val tname = t"a"
    assert(pt"$ptpe#$tname".show[Structure] === "Pat.Type.Project(Type.Name(\"x\"), Type.Name(\"a\"))")
  }

  test("1 pt\"$ref.type\"") {
    val pt"$ref.type" = pt"x.type"
    assert(ref.show[Structure] === "Term.Name(\"x\")")
  }

  test("2 pt\"$ref.type\"") {
    val ref = q"x"
    assert(pt"$ref.type".show[Structure] === "Type.Singleton(Term.Name(\"x\"))")
  }

  test("3 pt\"$ref.type\"") {
    val ref = q"`X`"
    assert(pt"$ref.type".show[Structure] === "Type.Singleton(Term.Name(\"X\"))")
  }

  test("1 pt\"$ptpe[..$ptpes]") {
    val pt"$ptpe[..$ptpes]" = pt"X[Y, Z]"
    assert(ptpe.show[Structure] === "Type.Name(\"X\")")
    assert(ptpes.toString === "List(Y, Z)")
    assert(ptpes(0).show[Structure] === "Type.Name(\"Y\")")
    assert(ptpes(1).show[Structure] === "Type.Name(\"Z\")")
  }

  test("2 pt\"$ptpe[..$ptpes]") {
    val pt"$ptpe[..$ptpes]" = pt"x[y, z]"
    assert(ptpe.show[Structure] === "Type.Name(\"x\")")
    assert(ptpes.toString === "List(y, z)")
    assert(ptpes(0).show[Structure] === "Pat.Var.Type(Type.Name(\"y\"))")
    assert(ptpes(1).show[Structure] === "Pat.Var.Type(Type.Name(\"z\"))")
  }

  test("3 pt\"$ptpe[..$ptpes]") {
    val ptpe = t"x"
    val ptpes = List(pt"y", pt"z")
    assert(pt"$ptpe[..$ptpes]".show[Structure] === "Pat.Type.Apply(Type.Name(\"x\"), Seq(Pat.Var.Type(Type.Name(\"y\")), Pat.Var.Type(Type.Name(\"z\"))))")
  }

  test("1 pt\"$ptpe $tname $ptpe\"") {
    val pt"$ptpe1 $tname $ptpe2" = pt"X Y Z"
    assert(ptpe1.show[Structure] === "Type.Name(\"X\")")
    assert(tname.show[Structure] === "Type.Name(\"Y\")")
    assert(ptpe2.show[Structure] === "Type.Name(\"Z\")")
  }

  test("2 pt\"$ptpe $tname $ptpe\"") {
    val pt"$ptpe1 $tname $ptpe2" = pt"x y z"
    assert(ptpe1.show[Structure] === "Type.Name(\"x\")")
    assert(tname.show[Structure] === "Type.Name(\"y\")")
    assert(ptpe2.show[Structure] === "Type.Name(\"z\")")
  }

  // TODO: compilation error
  // test("3 pt\"$ptpe $tname $ptpe\"") {
  //   val ptpe1 = pt"x"
  //   val tname = pt"y"
  //   val ptpe2 = pt"z"
  //   assert(pt"$ptpe1 $tname $ptpe2".show[Structure] === "")
  // }

  test("1 pt\"(..$ptpes) => $ptpe\"") {
    val pt"(..$ptpes) => $ptpe" = pt"(X, Y) => Z"
    assert(ptpes.toString === "List(X, Y)")
    assert(ptpes(0).show[Structure] === "Type.Name(\"X\")")
    assert(ptpes(1).show[Structure] === "Type.Name(\"Y\")")
    assert(ptpe.show[Structure] === "Type.Name(\"Z\")")
  }

  test("2 pt\"(..$ptpes) => $ptpe\"") {
    val pt"(..$ptpes) => $ptpe" = pt"(x, y) => z"
    assert(ptpes.toString === "List(x, y)")
    assert(ptpes(0).show[Structure] === "Pat.Var.Type(Type.Name(\"x\"))")
    assert(ptpes(1).show[Structure] === "Pat.Var.Type(Type.Name(\"y\"))")
    assert(ptpe.show[Structure] === "Type.Name(\"z\")")
  }

  test("3 pt\"(..$ptpes) => $ptpe\"") {
    val ptpes = List(pt"x", pt"y")
    val ptpe = pt"z"
    assert(pt"(..$ptpes) => $ptpe".show[Structure] === "Pat.Type.Function(Seq(Pat.Var.Type(Type.Name(\"x\")), Pat.Var.Type(Type.Name(\"y\"))), Pat.Var.Type(Type.Name(\"z\")))")
  }

  // TODO: compilation error
  // test("3 pt\"(..$ptpes) => $ptpe\"") {
  //   val ptpes = List(pt"x", pt"y")
  //   val ptpe = pt"z"
  //   assert(pt"(..$ptpes) => $ptpe".show[Structure] === "")
  // }

  test("1 pt\"(..$ptpes)\"") {
    val pt"(..$ptpes)" = pt"(x, y)"
    assert(ptpes.toString === "List(x, y)")
    assert(ptpes(0).show[Structure] === "Pat.Var.Type(Type.Name(\"x\"))")
    assert(ptpes(1).show[Structure] === "Pat.Var.Type(Type.Name(\"y\"))")
  }

  test("2 pt\"(..$ptpes)\"") {
    val ptpes = List(pt"x", pt"y")
    assert(pt"(..$ptpes)".show[Structure] === "Pat.Type.Tuple(Seq(Pat.Var.Type(Type.Name(\"x\")), Pat.Var.Type(Type.Name(\"y\"))))")
  }

  test("1 pt\"..$ptpes { ..$stats }\"") {
    val pt"..$ptpes { ..$stats }" = pt"x with y { val a: A }"
    assert(ptpes.toString === "List(x, y)")
    assert(ptpes(0).show[Structure] === "Type.Name(\"x\")")
    assert(ptpes(1).show[Structure] === "Type.Name(\"y\")")
    assert(stats.toString === "List(val a: A)")
    assert(stats(0).show[Structure] === "Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), Type.Name(\"A\"))")
  }

  // TODO: compilation error
  // test("2 pt\"..$ptpes { ..$stats }\"") {
  //   val ptpes = List(pt"x", pt"y")
  //   val stats = List(q"val `a`: `A`", q"val `b`: `B`")
  //   assert(pt"..$ptpes { ..$stats }".show[Structure] === "")
  // }

  test("1 pt\"$ptpe forSome { ..$stats }\"") {
    val pt"$ptpe forSome { ..$stats }" = pt"x forSome { val a: A }"
    assert(ptpe.show[Structure] === "Type.Name(\"x\")")
    assert(stats.toString === "List(val a: A)")
    assert(stats(0).show[Structure] === "Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), Type.Name(\"A\"))")
  }

  // TODO: compilation error
  // test("2 pt\"$ptpe forSome { ..$stats }\"") {
  //   val ptpe = pt"x"
  //   val stats = List(q"val a: A")
  //   assert(pt"$ptpe forSome { ..$stats }".show[Structure] === "")
  // }

  test("1 pt\"$ptpe ..$@annots\"") {
    val pt"$ptpe ..@$annots" = pt"x @q @w"
    assert(ptpe.show[Structure] === "Type.Name(\"x\")")
    assert(annots.toString === "List(@q, @w)")
    assert(annots(0).show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"q\"))")
    assert(annots(1).show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"w\"))")
  }

  // TODO: compilation error
  // test("2 pt\"$ptpe ..$@annots\"") {
  //   val ptpe = pt"x"
  //   val annots = List(mod"@q", mod"@w")
  //   assert(pt"$ptpe ..@$annots".show[Structure] === "")
  // }

  test("1 t\"_ >: $tpeopt <: tpeopt\"") {
    val pt"_ >: $tpe1 <: $tpe2" = pt"_ >: X <: Y"
    assert(tpe1.show[Structure] === "Some(Type.Name(\"X\"))")
    assert(tpe2.show[Structure] === "Some(Type.Name(\"Y\"))")
  }

  test("2 t\"_ >: $tpeopt <: tpeopt\"") {
    val tpe1 = t"`X`"
    val tpe2 = t"`Y`"
    assert(pt"_ >: $tpe1 <: $tpe2".show[Structure] === "Pat.Type.Placeholder(Type.Bounds(Some(Type.Name(\"X\")), Some(Type.Name(\"Y\"))))")
  }

  test("3 t\"_ >: $tpeopt <: tpeopt\"") {
    val tpe1 = t"x"
    val tpe2 = t"y"
    assert(pt"_ >: $tpe1 <: $tpe2".show[Structure] === "Pat.Type.Placeholder(Type.Bounds(Some(Type.Name(\"x\")), Some(Type.Name(\"y\"))))")
  }

  test("pt\"$lit\"") {
    val lit = q"1"
    assert(pt"$lit".show[Structure] === "Lit(1)")
  }

  test("1 q\"..$mods val ..$pnames: $tpe\"") {
    val q"..$mods val ..$pnames: $tpe" = q"private final val x, y: T"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(pnames.toString === "List(x, y)")
    assert(pnames(0).show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
    assert(pnames(1).show[Structure] === "Pat.Var.Term(Term.Name(\"y\"))")
    assert(tpe.show[Structure] === "Type.Name(\"T\")")
  }

  test("2 q\"..$mods val ..$pnames: $tpe\"") {
    val mods = List(mod"private", mod"final")
    val pnames = List(p"x", p"y")
    val tpe = t"T"
    assert(q"..$mods val ..$pnames: $tpe".show[Structure] === "Decl.Val(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Seq(Pat.Var.Term(Term.Name(\"x\")), Pat.Var.Term(Term.Name(\"y\"))), Type.Name(\"T\"))")
  }

  test("1 q\"..$mods var ..$pnames: $tpe\"") {
    val q"..$mods var ..$pnames: $tpe" = q"private final var x, y: T"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(pnames.toString === "List(x, y)")
    assert(pnames(0).show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
    assert(pnames(1).show[Structure] === "Pat.Var.Term(Term.Name(\"y\"))")
    assert(tpe.show[Structure] === "Type.Name(\"T\")")
  }

  test("2 q\"..$mods var ..$pnames: $tpe\"") {
    val mods = List(mod"private", mod"final")
    val pnames = List(p"x", p"y")
    val tpe = t"T"
    assert(q"..$mods var ..$pnames: $tpe".show[Structure] === "Decl.Var(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Seq(Pat.Var.Term(Term.Name(\"x\")), Pat.Var.Term(Term.Name(\"y\"))), Type.Name(\"T\"))")
  }

  test("1 q\"..$mods def $name[..$tparams](...$paramss): $tpe\"") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpe" = q"private final def m[T, W](x: X, y: Y): R"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(name.show[Structure] === "Term.Name(\"m\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).show[Structure] === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).show[Structure] === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).show[Structure] === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).show[Structure] === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(tpe.show[Structure] === "Type.Name(\"R\")")
  }

  test("2 q\"..$mods def $name[..$tparams](...$paramss): $tpe\"") {
    val mods = List(mod"private", mod"final")
    val name = q"m"
    val tparams = List(tparam"T", tparam"W")
    val paramss = List(List(param"x: X", param"x: Y"))
    val tpe = t"R"
    assert(q"..$mods def $name[..$tparams](...$paramss): $tpe".show[Structure] === "Decl.Def(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Term.Name(\"m\"), Seq(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Seq(Seq(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None))), Type.Name(\"R\"))")
  }

  test("1 q\"..$mods type $tname[..$tparams] >: $tpeopt <: $tpeopt\"") {
    val q"..$mods type $tname[..$tparams] >: $tpeopt1 <: $tpeopt2" = q"private final type T[T, W] >: A <: B"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(tname.show[Structure] === "Type.Name(\"T\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).show[Structure] === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).show[Structure] === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tpeopt1.show[Structure] === "Some(Type.Name(\"A\"))")
    assert(tpeopt2.show[Structure] === "Some(Type.Name(\"B\"))")
  }

  test("2 q\"..$mods type $tname[..$tparams] >: $tpeopt <: $tpeopt\"") {
    val mods = List(mod"private", mod"final")
    val tname = t"T"
    val tparams = List(tparam"T", tparam"W")
    val tpeopt1 = t"A"
    val tpeopt2 = t"A"
    assert(q"..$mods type $tname[..$tparams] >: $tpeopt1 <: $tpeopt2".show[Structure] === "Decl.Type(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Type.Name(\"T\"), Seq(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Bounds(Some(Type.Name(\"A\")), Some(Type.Name(\"A\"))))")
  }

  test("1 q\"..$mods val ..$pats: $tpeopt = $expr\"") {
    val q"..$mods val ..$pats: $tpeopt = $expr" = q"private final val x, y: T = t"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(pats.toString === "List(x, y)")
    assert(pats(0).show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
    assert(pats(1).show[Structure] === "Pat.Var.Term(Term.Name(\"y\"))")
    assert(tpeopt.show[Structure] === "Some(Type.Name(\"T\"))")
    assert(expr.show[Structure] === "Term.Name(\"t\")")
  }

  test("2 q\"..$mods val ..$pats: $tpeopt = $expr\"") {
    val mods = List(mod"private", mod"final")
    val pats = List(p"x", p"y")
    val tpeopt = t"T"
    val expr = q"t"
    assert(q"..$mods val ..$pats: $tpeopt = $expr".show[Structure] === "Defn.Val(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Seq(Pat.Var.Term(Term.Name(\"x\")), Pat.Var.Term(Term.Name(\"y\"))), Some(Type.Name(\"T\")), Term.Name(\"t\"))")
  }

  test("1 q\"..$mods var ..$pats: $tpeopt = $expropt\"") {
    val q"..$mods var ..$pats: $tpeopt = $expropt" = q"private final var x, y: T = t"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(pats.toString === "List(x, y)")
    assert(pats(0).show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
    assert(pats(1).show[Structure] === "Pat.Var.Term(Term.Name(\"y\"))")
    assert(tpeopt.show[Structure] === "Some(Type.Name(\"T\"))")
    assert(expropt.show[Structure] === "Some(Term.Name(\"t\"))")
  }

  test("2 q\"..$mods var ..$pats: $tpeopt = $expropt\"") {
    val mods = List(mod"private", mod"final")
    val pats = List(p"x", p"y")
    val tpeopt = t"T"
    val expropt = q"t"
    assert(q"..$mods var ..$pats: $tpeopt = $expropt".show[Structure] === "Defn.Var(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Seq(Pat.Var.Term(Term.Name(\"x\")), Pat.Var.Term(Term.Name(\"y\"))), Some(Type.Name(\"T\")), Some(Term.Name(\"t\")))")
  }

  test("1 q\"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr\"") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" = q"private final def m[T, W](x: X, y: Y): R = r"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(name.show[Structure] === "Term.Name(\"m\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).show[Structure] === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).show[Structure] === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).show[Structure] === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).show[Structure] === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(tpeopt.show[Structure] === "Some(Type.Name(\"R\"))")
    assert(expr.show[Structure] === "Term.Name(\"r\")")
  }

  test("2 q\"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr\"") {
    val mods = List(mod"private", mod"final")
    val name = q"m"
    val tparams = List(tparam"T", tparam"W")
    val paramss = List(List(param"x: X", param"x: Y"))
    val tpeopt = t"R"
    val expr = q"r"
    assert(q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr".show[Structure] === "Defn.Def(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Term.Name(\"m\"), Seq(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Seq(Seq(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None))), Some(Type.Name(\"R\")), Term.Name(\"r\"))")
  }

  test("1 q\"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr\"") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr" = q"private final def m[T, W](x: X, y: Y): R = macro r"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(name.show[Structure] === "Term.Name(\"m\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).show[Structure] === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).show[Structure] === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).show[Structure] === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).show[Structure] === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(tpeopt.show[Structure] === "Some(Type.Name(\"R\"))")
    assert(expr.show[Structure] === "Term.Name(\"r\")")
  }

  test("2 q\"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr\"") {
    val mods = List(mod"private", mod"final")
    val name = q"m"
    val tparams = List(tparam"T", tparam"W")
    val paramss = List(List(param"x: X", param"x: Y"))
    val tpeopt = Some(t"R")
    val expr = q"r"
    assert(q"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr".show[Structure] === "Defn.Macro(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Term.Name(\"m\"), Seq(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Seq(Seq(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None))), Some(Type.Name(\"R\")), Term.Name(\"r\"))")
  }

  test("1 q\"..$mods type $tname[..$tparams] = $tpe\"") {
    val q"..$mods type $tname[..$tparams] = $tpe" = q"private final type Q[T, W] = R"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(tname.show[Structure] === "Type.Name(\"Q\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).show[Structure] === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).show[Structure] === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tpe.show[Structure] === "Type.Name(\"R\")")
  }

  test("2 q\"..$mods type $tname[..$tparams] = $tpe\"") {
    val mods = List(mod"private", mod"final")
    val tname = t"Q"
    val tparams = List(tparam"T", tparam"W")
    val tpe = t"R"
    assert(q"..$mods type $tname[..$tparams] = $tpe".show[Structure] === "Defn.Type(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Type.Name(\"Q\"), Seq(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Name(\"R\"))")
  }

  test("1 q\"..$mods class $tname[..$tparams] $mod (...$paramss) extends $template\"") {
  val q"..$mods class $tname[..$tparams] $mod (...$paramss) extends $template" = q"private final class Q[T, W] private (x: X, y: Y) extends Y"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(tname.show[Structure] === "Type.Name(\"Q\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).show[Structure] === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).show[Structure] === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(mod.show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).show[Structure] === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).show[Structure] === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(template.show[Structure] === "Template(Nil, Seq(Ctor.Ref.Name(\"Y\")), Term.Param(Nil, Name.Anonymous(), None, None), None)")
  }

  test("2 q\"..$mods class $tname[..$tparams] $mod (...$paramss) extends $template\"") {
  val q"..$mods class $tname[..$tparams] $mod (...$paramss) extends $template" = q"private final class Q[T, W] protected (x: X, y: Y) extends { def m1 = 42; def m2 = 666 }"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(tname.show[Structure] === "Type.Name(\"Q\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).show[Structure] === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).show[Structure] === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(mod.show[Structure] === "Mod.Protected(Name.Anonymous())")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).show[Structure] === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).show[Structure] === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(template.show[Structure] === "Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Nil, Term.Name(\"m1\"), Nil, Nil, None, Lit(42)), Defn.Def(Nil, Term.Name(\"m2\"), Nil, Nil, None, Lit(666)))))")
  }

  test("3 q\"..$mods class $tname[..$tparams] $mod (...$paramss) extends $template\"") {
  val mods = List(mod"private", mod"final")
    val tname = t"Q"
    val tparams = List(tparam"T", tparam"W")
    val mod = mod"protected"
    val paramss = List(List(param"x: X", param"x: Y"))
    val template = template"F { def m = 42 }"
    assert(q"..$mods class $tname[..$tparams] $mod (...$paramss) extends $template".show[Structure] === "Defn.Class(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Type.Name(\"Q\"), Seq(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Ctor.Primary(Seq(Mod.Protected(Name.Anonymous())), Ctor.Ref.Name(\"this\"), Seq(Seq(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None)))), Template(Nil, Seq(Ctor.Ref.Name(\"F\")), Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit(42))))))")
  }

  test("1 q\"..$mods trait $tname[..$tparams] extends $template\"") {
    val q"..$mods trait $tname[..$tparams] extends $template" = q"private final trait Q[T, W] extends Y"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(tname.show[Structure] === "Type.Name(\"Q\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).show[Structure] === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).show[Structure] === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(template.show[Structure] === "Template(Nil, Seq(Ctor.Ref.Name(\"Y\")), Term.Param(Nil, Name.Anonymous(), None, None), None)")
  }

  test("2 q\"..$mods trait $tname[..$tparams] extends $template\"") {
    val q"..$mods trait $tname[..$tparams] extends $template" = q"private final trait Q[T, W] extends { def m1 = 42; def m2 = 666 }"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(tname.show[Structure] === "Type.Name(\"Q\")")
    assert(tparams.toString === "List(T, W)")
    assert(tparams(0).show[Structure] === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).show[Structure] === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(template.show[Structure] === "Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Nil, Term.Name(\"m1\"), Nil, Nil, None, Lit(42)), Defn.Def(Nil, Term.Name(\"m2\"), Nil, Nil, None, Lit(666)))))")
  }

  test("3 q\"..$mods trait $tname[..$tparams] extends $template\"") {
    val mods = List(mod"private", mod"final")
    val tname = t"Q"
    val tparams = List(tparam"T", tparam"W")
    val template = template"F { def m = 42 }"
    assert(q"..$mods trait $tname[..$tparams] extends $template".show[Structure] === "Defn.Trait(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Type.Name(\"Q\"), Seq(Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Seq(Ctor.Ref.Name(\"F\")), Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit(42))))))")
  }

  test("1 q\"..$mods object $name extends $template\"") {
    val q"..$mods object $name extends $template" = q"private final object Q extends Y"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(name.show[Structure] === "Term.Name(\"Q\")")
    assert(template.show[Structure] === "Template(Nil, Seq(Ctor.Ref.Name(\"Y\")), Term.Param(Nil, Name.Anonymous(), None, None), None)")
  }

  test("2 q\"..$mods object $name extends $template\"") {
    val q"..$mods object $name extends $template" = q"private final object Q extends { def m1 = 42; def m2 = 666 }"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(name.show[Structure] === "Term.Name(\"Q\")")
    assert(template.show[Structure] === "Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Nil, Term.Name(\"m1\"), Nil, Nil, None, Lit(42)), Defn.Def(Nil, Term.Name(\"m2\"), Nil, Nil, None, Lit(666)))))")
  }

  test("3 q\"..$mods object $name extends $template\"") {
    val mods = List(mod"private", mod"final")
    val name = q"Q"
    val template = template"F { def m = 42 }"
    assert(q"..$mods object $name extends $template".show[Structure] === "Defn.Object(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Term.Name(\"Q\"), Template(Nil, Seq(Ctor.Ref.Name(\"F\")), Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit(42))))))")
  }

  test("1 q\"package object $name extends $template\"") {
    val q"package object $name extends $template" = q"package object Q extends Y"
    assert(name.show[Structure] === "Term.Name(\"Q\")")
    assert(template.show[Structure] === "Template(Nil, Seq(Ctor.Ref.Name(\"Y\")), Term.Param(Nil, Name.Anonymous(), None, None), None)")
  }

  test("2 q\"package object $name extends $template\"") {
    val q"package object $name extends $template" = q"package object Q extends { def m1 = 42; def m2 = 666 }"
    assert(name.show[Structure] === "Term.Name(\"Q\")")
    assert(template.show[Structure] === "Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Nil, Term.Name(\"m1\"), Nil, Nil, None, Lit(42)), Defn.Def(Nil, Term.Name(\"m2\"), Nil, Nil, None, Lit(666)))))")
  }

  test("3 q\"package object $name extends $template\"") {
    val name = q"Q"
    val template = template"F { def m = 42 }"
    assert(q"package object $name extends $template".show[Structure] === "Pkg.Object(Nil, Term.Name(\"Q\"), Template(Nil, Seq(Ctor.Ref.Name(\"F\")), Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit(42))))))")
  }

  test("1 q\"package $ref { ..$stats }\"") {
    val q"package $ref { ..$stats }" = q"package p { class A; object B }"
    assert(ref.show[Structure] === "Term.Name(\"p\")")
    assert(stats.toString === "List(class A, object B)")
    assert(stats(0).show[Structure] === "Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), None))")
    assert(stats(1).show[Structure] === "Defn.Object(Nil, Term.Name(\"B\"), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), None))")
  }

  test("2 q\"package $ref { ..$stats }\"") {
    val ref = q"p"
    val stats = List(q"class A", q"object B")
    assert(q"package $ref { ..$stats }".show[Structure] === "Pkg(Term.Name(\"p\"), Seq(Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), None)), Defn.Object(Nil, Term.Name(\"B\"), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), None))))")
  }

  test("1 q\"..$mods def this(...$paramss)\"") {
    val q"..$mods def this(...$paramss)" = q"private def this(x: X, y: Y)"
    assert(mods.toString === "List(private)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).show[Structure] === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).show[Structure] === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
  }

   test("2 q\"..$mods def this(...$paramss)\"") {
     val mods = List(mod"private")
     val paramss = List(List(param"x: X", param"x: Y"))
     assert(q"..$mods def this(...$paramss)".show[Structure] === "Ctor.Primary(Seq(Mod.Private(Name.Anonymous())), Ctor.Ref.Name(\"this\"), Seq(Seq(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None))))")
   }

  test("1 q\"..$mods def this(...$paramss) = $expr\"") {
    val q"..$mods def this(...$paramss) = $expr" = q"private final def this(x: X, y: Y) = this(foo, bar)"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(paramss.toString === "List(List(x: X, y: Y))")
    assert(paramss(0)(0).show[Structure] === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None)")
    assert(paramss(0)(1).show[Structure] === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"Y\")), None)")
    assert(expr.show[Structure] === "Term.Apply(Ctor.Ref.Name(\"this\"), Seq(Term.Name(\"foo\"), Term.Name(\"bar\")))")
  }

   test("2 q\"..$mods def this(...$paramss) = $expr\"") {
     val mods = List(mod"private", mod"final")
     val paramss = List(List(param"x: X", param"x: Y"))
     val expr = ctor"C(foo, bar)"
     assert(q"..$mods def this(...$paramss) = $expr".show[Structure] === "Ctor.Secondary(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Ctor.Ref.Name(\"this\"), Seq(Seq(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"X\")), None), Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Y\")), None))), Term.Apply(Ctor.Ref.Name(\"C\"), Seq(Term.Name(\"foo\"), Term.Name(\"bar\"))))")
   }

  test("1 param\"..$mods $paramname: $atpeopt = $expropt\"") {
    val param"..$mods $paramname: $atpeopt = $expropt" = param"private final x: X = 42"
    assert(mods.toString === "List(private, final)")
    assert(mods(0).show[Structure] === "Mod.Private(Name.Anonymous())")
    assert(mods(1).show[Structure] === "Mod.Final()")
    assert(paramname.show[Structure] === "Term.Name(\"x\")")
    assert(atpeopt.show[Structure] === "Some(Type.Name(\"X\"))")
    assert(expropt.show[Structure] === "Some(Lit(42))")
  }

  test("2 param\"..$mods $paramname: $atpeopt = $expropt\"") {
    val mods = List(mod"private", mod"final")
    val paramname = q"x"
    val atpeopt = t"X"
    val expropt = q"42"
    assert(param"..$mods $paramname: $atpeopt = $expropt".show[Structure] === "Term.Param(Seq(Mod.Private(Name.Anonymous()), Mod.Final()), Term.Name(\"x\"), Some(Type.Name(\"X\")), Some(Lit(42)))")
  }

  test("1 tparam\"..$mods $tparamname[..$tparams] >: $tpeopt <: $tpeopt <% ..$tpes : ..$tpes\"") {
    val tparam"..$mods $tparamname[..$tparams] >: $tpeopt1 <: $tpeopt2 <% ..$tpes1 : ..$tpes2" = tparam"+Z[Q,W] >: E <: R <% T with Y : U with I"
    assert(mods.toString === "List(+)")
    assert(mods(0).show[Structure] === "Mod.Covariant()")
    assert(tparamname.show[Structure] === "Type.Name(\"Z\")")
    assert(tparams.toString === "List(Q, W)")
    assert(tparams(0).show[Structure] === "Type.Param(Nil, Type.Name(\"Q\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tparams(1).show[Structure] === "Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)")
    assert(tpeopt1.show[Structure] === "Some(Type.Name(\"E\"))")
    assert(tpeopt2.show[Structure] === "Some(Type.Name(\"R\"))")
    assert(tpes1.toString === "List(T with Y)")
    assert(tpes1(0).show[Structure] === "Type.Compound(Seq(Type.Name(\"T\"), Type.Name(\"Y\")), Nil)")
    assert(tpes2.toString === "List(U with I)")
    assert(tpes2(0).show[Structure] === "Type.Compound(Seq(Type.Name(\"U\"), Type.Name(\"I\")), Nil)")
  }

  test("2 tparam\"..$mods $tparamname[..$tparams] >: $tpeopt <: $tpeopt <% ..$tpes : ..$tpes\"") {
    val mods = List(mod"+")
    val tparamname = t"Z"
    val tparams = List(tparam"Q", tparam"W")
    val tpeopt1 = t"E"
    val tpeopt2 = t"R"
    val tpes1 = List(t"T with Y")
    val tpes2 = List(t"U with I")
    assert(tparam"..$mods $tparamname[..$tparams] >: $tpeopt1 <: $tpeopt2 <% ..$tpes1 : ..$tpes2".show[Structure] === "Type.Param(Seq(Mod.Covariant()), Type.Name(\"Z\"), Seq(Type.Param(Nil, Type.Name(\"Q\"), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name(\"W\"), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Bounds(Some(Type.Name(\"E\")), Some(Type.Name(\"R\"))), Seq(Type.Compound(Seq(Type.Name(\"T\"), Type.Name(\"Y\")), Nil)), Seq(Type.Compound(Seq(Type.Name(\"U\"), Type.Name(\"I\")), Nil)))")
  }

  test("1 ctor\"$ctorname\"") {
    val ctor"$ctorname" = ctor"x" // TODO after #227 types should be precise (Ctor.Call)
    assert(ctorname.show[Structure] === "Ctor.Ref.Name(\"x\")")
  }

  test("2 ctor\"$ctorname\"") {
    val ctorname = ctor"x"
    assert(ctor"$ctorname".show[Structure] === "Ctor.Ref.Name(\"x\")")
  }

  test("1 ctor\"$ref.$ctorname\"") {
    val ctor"$ref.$ctorname" = ctor"x.y" // TODO after #227 types should be precise (Ctor.Call)
    assert(ref.show[Structure] === "Term.Name(\"x\")")
    assert(ctorname.show[Structure] === "Ctor.Ref.Name(\"y\")")
  }

  test("2 ctor\"$ref.$ctorname\"") {
    val ref = q"x"
    val ctorname = ctor"y"
    assert(ctor"$ref.$ctorname".show[Structure] === "Ctor.Ref.Select(Term.Name(\"x\"), Ctor.Ref.Name(\"y\"))")
  }

  test("1 ctor\"tpe#$ctorname\"") {
    val ctor"$tpe#$ctorname" = ctor"x#y" // TODO after #227 types should be precise (Ctor.Call)
    assert(tpe.show[Structure] === "Type.Name(\"x\")")
    assert(ctorname.show[Structure] === "Ctor.Ref.Name(\"y\")")
  }

  test("2 ctor\"$tpe#$ctorname\"") {
    val tpe = t"x"
    val ctorname = ctor"y"
    assert(ctor"$tpe#$ctorname".show[Structure] === "Ctor.Ref.Project(Type.Name(\"x\"), Ctor.Ref.Name(\"y\"))")
  }

  test("1 ctor\"(..$tpes) => $tpe\"") {
    val ctor"(x, ..$tpes) => $tpe" = ctor"(x, y) => z" // TODO after #227 types should be precise (Ctor.Call)
    assert(tpes.toString === "List(y)")
    assert(tpes(0).show[Structure] === "Type.Name(\"y\")")
    assert(tpe.show[Structure] === "Type.Name(\"z\")")
  }

  test("2 ctor\"(..$tpes) => $tpe\"") {
    val tpes = List(t"x", t"y")
    val tpe = t"z"
    assert(ctor"(x, ..$tpes) => $tpe".show[Structure] === "Term.ApplyType(Ctor.Ref.Function(Ctor.Ref.Name(\"=>\")), Seq(Type.Name(\"x\"), Type.Name(\"x\"), Type.Name(\"y\"), Type.Name(\"z\")))")
  }

  test("1 ctor\"$ctorname ..@annots\"") {
    val ctor"$ctorname ..@$annots" = ctor"x @q @w" // TODO after #227 types should be precise (Ctor.Call)
    assert(ctorname.show[Structure] === "Ctor.Ref.Name(\"x\")")
    assert(annots.toString === "List(@q, @w)")
    assert(annots(0).show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"q\"))")
    assert(annots(1).show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"w\"))")
  }

  test("2 ctor\"$ctorname ..@annots\"") {
    val ctorname = ctor"x"
    val annots = List(mod"@q", mod"@w")
    assert(ctor"$ctorname ..@$annots".show[Structure] === "Term.Annotate(Ctor.Ref.Name(\"x\"), Seq(Mod.Annot(Ctor.Ref.Name(\"q\")), Mod.Annot(Ctor.Ref.Name(\"w\"))))")
  }

  test("1 ctor\"$ctorref(...$aexprss)\"") {
  val ctor"$ctorref(...$aexprss)" = ctor"x(y, z)" // TODO after #227 types should be precise (Ctor.Call)
    assert(ctorref.show[Structure] === "Ctor.Ref.Name(\"x\")")
    assert(aexprss.toString === "List(List(y, z))")
    assert(aexprss(0)(0).show[Structure] === "Term.Name(\"y\")")
    assert(aexprss(0)(1).show[Structure] === "Term.Name(\"z\")")
  }

  test("2 ctor\"$ctorref(...$aexprss)\"") {
    val ctorref = ctor"x"
    val aexprss = List(List(arg"y", arg"z"))
    assert(ctor"$ctorref(...$aexprss)".show[Structure] === "Term.Apply(Ctor.Ref.Name(\"x\"), Seq(Term.Name(\"y\"), Term.Name(\"z\")))")
  }

  test("1 ctor\"$ctorref[..$atpes]\"") {
    val ctor"$ctorref[..$atpes]" = ctor"x[y, z]" // TODO after #227 types should be precise (Ctor.Call)
    assert(ctorref.show[Structure] === "Ctor.Ref.Name(\"x\")")
    assert(atpes.toString === "List(y, z)")
    assert(atpes(0).show[Structure] === "Type.Name(\"y\")")
    assert(atpes(1).show[Structure] === "Type.Name(\"z\")")
  }

  test("2 ctor\"$ctorref[..$atpes]\"") {
    val ctorref = ctor"x"
    val atpes = List(t"y", t"z")
    assert(ctor"$ctorref[..$atpes]".show[Structure] === "Term.ApplyType(Ctor.Ref.Name(\"x\"), Seq(Type.Name(\"y\"), Type.Name(\"z\")))")
  }

  test("1 template\"{ ..$stats } with ..$exprs { $param => ..$stats }\"") {
    val template"{ ..$stats1 } with ..$exprs { $param => ..$stats2 }" = template"{ val a = 2; val b = 2 } with T with U { self: Z => def m = 2; def n = 2 }"
    assert(stats1.toString === "List(val a = 2, val b = 2)")
    assert(stats1(0).show[Structure] === "Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), None, Lit(2))")
    assert(stats1(1).show[Structure] === "Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"b\"))), None, Lit(2))")
    assert(exprs.toString === "List(T, U)")
    assert(exprs(0).show[Structure] === "Ctor.Ref.Name(\"T\")")
    assert(exprs(1).show[Structure] === "Ctor.Ref.Name(\"U\")")
    assert(param.show[Structure] === "Term.Param(Nil, Term.Name(\"self\"), Some(Type.Name(\"Z\")), None)")
    assert(stats2.toString === "List(def m = 2, def n = 2)")
    assert(stats2(0).show[Structure] === "Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit(2))")
    assert(stats2(1).show[Structure] === "Defn.Def(Nil, Term.Name(\"n\"), Nil, Nil, None, Lit(2))")
  }

   test("2 template\"{ ..$stats } with ..$exprs { $param => ..$stats }\"") {
     val stats1 = List(q"val a = 2", q"val b = 2")
     val exprs = List(ctor"T", ctor"U")
     val param = param"self: S"
     val stats2 = List(q"def m = 2", q"def n = 2")
     assert(template"{ ..$stats1 } with ..$exprs { $param => ..$stats2 }".show[Structure] === "Template(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), None, Lit(2)), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"b\"))), None, Lit(2))), Seq(Ctor.Ref.Name(\"T\"), Ctor.Ref.Name(\"U\")), Term.Param(Nil, Term.Name(\"self\"), Some(Type.Name(\"S\")), None), Some(Seq(Defn.Def(Nil, Term.Name(\"m\"), Nil, Nil, None, Lit(2)), Defn.Def(Nil, Term.Name(\"n\"), Nil, Nil, None, Lit(2)))))")
   }

  test("1 mod\"@$expr\"") {
    val mod"@$expr" = mod"@a"
    assert(expr.show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"a\"))")
  }

  test("2 mod\"@$expr\"") {
    val expr = mod"@a"
    assert(mod"@$expr".show[Structure] === "Mod.Annot(Ctor.Ref.Name(\"a\"))")
  }

  test("1 mod\"private[$qname]\"") {
    val mod"private[$qname]" = mod"private[X]"
    assert(qname.show[Structure] === "Name.Indeterminate(\"X\")")
  }

  test("2 mod\"private[$qname]\"") {
    val mod"private[$qname]" = mod"private"
    assert(qname.show[Structure] === "Name.Anonymous()")
  }

  test("3 mod\"private[$qname]\"") {
    val mod"private[$qname]" = mod"private[this]"
    assert(qname.show[Structure] === "Term.This(Name.Anonymous())")
  }

  test("4 mod\"private[$qname]\"") {
    val qname = q"q"
    assert(mod"private[$qname]".show[Structure] === "Mod.Private(Term.Name(\"q\"))")
  }

  test("1 mod\"protected[$qname]\"") {
    val mod"protected[$qname]" = mod"protected[X]"
    assert(qname.show[Structure] === "Name.Indeterminate(\"X\")")
  }

  test("2 mod\"protected[$qname]\"") {
    val mod"protected[$qname]" = mod"protected"
    assert(qname.show[Structure] === "Name.Anonymous()")
  }

  test("3 mod\"protected[$qname]\"") {
    val mod"protected[$qname]" = mod"protected[this]"
    assert(qname.show[Structure] === "Term.This(Name.Anonymous())")
  }

  test("4 mod\"protected[$qname]\"") {
    val qname = q"q"
    assert(mod"protected[$qname]".show[Structure] === "Mod.Protected(Term.Name(\"q\"))")
  }

  test("mod\"implicit\"") {
    assert(mod"implicit".show[Structure] === "Mod.Implicit()")
  }

  test("mod\"final\"") {
    assert(mod"final".show[Structure] === "Mod.Final()")
  }

  test("mod\"sealed\"") {
    assert(mod"sealed".show[Structure] === "Mod.Sealed()")
  }

  test("mod\"override\"") {
    assert(mod"override".show[Structure] === "Mod.Override()")
  }

  test("mod\"case\"") {
    assert(mod"case".show[Structure] === "Mod.Case()")
  }

  test("mod\"abstract\"") {
    assert(mod"abstract".show[Structure] === "Mod.Abstract()")
  }

  test("mod\"+\"") {
    assert(mod"+".show[Structure] === "Mod.Covariant()")
  }

  test("mod\"-\"") {
    assert(mod"-".show[Structure] === "Mod.Contravariant()")
  }

  test("mod\"lazy\"") {
    assert(mod"lazy".show[Structure] === "Mod.Lazy()")
  }

  test("mod\"val\"") {
    assert(mod"valparam".show[Structure] === "Mod.ValParam()")
  }

  test("mod\"var\"") {
    assert(mod"varparam".show[Structure] === "Mod.VarParam()")
  }

  test("1 enumerator\"$pat <- $expr\"") {
    val enumerator"$pat <- $expr" = enumerator"x <- xs"
    assert(pat.show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
  }

  test("2 enumerator\"$pat <- $expr\"") {
    val pat = p"x"
    val expr = q"xs"
    assert(enumerator"$pat <- $expr".show[Structure] === "Enumerator.Generator(Pat.Var.Term(Term.Name(\"x\")), Term.Name(\"xs\"))")
  }

  test("3 enumerator\"$pat <- $expr\"") {
    val pat = p"X"
    val expr = q"xs"
    assert(enumerator"$pat <- $expr".show[Structure] === "Enumerator.Generator(Pat.Var.Term(Term.Name(\"X\")), Term.Name(\"xs\"))")
  }

  test("1 enumerator\"$pat = $expr\"") {
    val enumerator"$pat = $expr" = enumerator"x = xs"
    assert(pat.show[Structure] === "Pat.Var.Term(Term.Name(\"x\"))")
  }

  test("2 enumerator\"$pat = $expr\"") {
    val pat = p"x"
    val expr = q"xs"
    assert(enumerator"$pat = $expr".show[Structure] === "Enumerator.Val(Pat.Var.Term(Term.Name(\"x\")), Term.Name(\"xs\"))")
  }

  test("1 enumerator\"if $expr\"") {
    val enumerator"if $expr" = enumerator"if x"
    assert(expr.show[Structure] === "Term.Name(\"x\")")
  }

  test("2 enumerator\"if $expr\"") {
    val expr = q"x"
    assert(enumerator"if $expr".show[Structure] === "Enumerator.Guard(Term.Name(\"x\"))")
  }

  test("1 q\"import ..$importers\"") {
    val importers = List(importer"foo.bar", importer"bar.{baz, _}")
    assert(q"import ..$importers".show[Syntax] === "import foo.bar, bar.{ baz, _ }")
  }

  test("2 q\"import ..$importers\"") {
    val q"import ..$importers" = q"import a.A"
    assert(importers.length == 1)
    assert(importers(0).show[Syntax] === "a.A")
  }

  test("1 importer\"$ref.{..$importees}\"") {
    val ref = q"bar"
    val importees = List(importee"baz", importee"_")
    assert(importer"$ref.{..$importees}".show[Syntax] === "bar.{ baz, _ }")
  }

  test("2 importer\"$ref. ..$importees\"") {
    val importer"$ref.{..$importees}" = importer"bar.{baz, _}"
    assert(ref.show[Syntax] === "bar")
    assert(importees.length == 2)
    assert(importees(0).show[Syntax] === "baz")
    assert(importees(1).show[Syntax] === "_")
  }

  test("1 importee\"$iname\"") {
    val importee"$iname" = importee"x"
    assert(iname.show[Structure] === "Name.Indeterminate(\"x\")")
  }

  test("2 importee\"$iname\"") {
    // $iname can't be constructed, only extracted from importee"..." and mod"..."
    val importee"$iname" = importee"x"
    assert(importee"$iname".show[Structure] === "Importee.Name(Name.Indeterminate(\"x\"))")
  }

  test("1 importee\"$iname => $iname\"") {
    val importee"$iname1 => $iname2" = importee"x => y"
    assert(iname1.show[Structure] === "Name.Indeterminate(\"x\")")
    assert(iname2.show[Structure] === "Name.Indeterminate(\"y\")")
  }

  test("2 importee\"$iname => $iname\"") {
    // $iname can't be constructed, only extracted from importee"..." and mod"..."
    val importee"$iname1 => $iname2" = importee"x => y"
    assert(importee"$iname1 => $iname2".show[Structure] === "Importee.Rename(Name.Indeterminate(\"x\"), Name.Indeterminate(\"y\"))")
  }

  test("1 importee\"$iname => _\"") {
    val importee"$iname => _" = importee"x => _"
    assert(iname.show[Structure] === "Name.Indeterminate(\"x\")")
  }

  test("2 importee\"$iname => _\"") { // TODO review after #219 solved
  // $iname can't be constructed, only extracted from importee"..." and mod"..."
  val importee"$iname => _" = importee"x => _"
    assert(importee"$iname => _".show[Structure] === "Importee.Unimport(Name.Indeterminate(\"x\"))")
  }

  test("importee\"_\"") {
    assert(importee"_".show[Structure] === "Importee.Wildcard()")
  }

  test("1 source\"..$stats\"") {
    val source"..$stats" = source"class A { val a = 'a'}"
    assert(stats.toString === "List(class A { val a = 'a' })")
    assert(stats(0).show[Structure] === "Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), None, Lit('a'))))))")
  }

  test("2 source\"..$stats\"") {
    val source"class B { val b = 'b'}; ..$stats" = source"class B { val b = 'b'}; class A { val a = 'a'}"
    assert(stats.toString === "List(class A { val a = 'a' })")
    assert(stats(0).show[Structure] === "Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"a\"))), None, Lit('a'))))))")
  }

  test("3 source\"..$stats\"") {
    val stats = List(q"class A { val x = 1 }", q"object B")
    assert(source"..$stats".show[Structure] === "Source(Seq(Defn.Class(Nil, Type.Name(\"A\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"x\"))), None, Lit(1)))))), Defn.Object(Nil, Term.Name(\"B\"), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), None))))")
  }

  test("unquote T into Option[T]") {
    val cond = q"cond"
    assert(p"case _ if $cond =>".show[Structure] === "Case(Pat.Wildcard(), Some(Term.Name(\"cond\")), Term.Block(Nil))")
  }

  test("unquote Option[T] into Option[T]") {
    val condopt = Some(q"cond")
    assert(p"case _ if $condopt =>".show[Structure] === "Case(Pat.Wildcard(), Some(Term.Name(\"cond\")), Term.Block(Nil))")
  }

  test("extract Some[T] from Option[T]") {
    val p"case _ if $condopt =>" = p"case _ if cond =>"
    assert(condopt.show[Structure] === "Some(Term.Name(\"cond\"))")
  }

  test("extract None from Option[T]") {
    val p"case _ if $condopt =>" = p"case _ =>"
    assert(condopt.show[Structure] === "None")
  }

  test("unquote T into Option[Seq[T]]") {
    val stat = q"def x = 42"
    assert(q"class C { $stat }".show[Structure] === "Defn.Class(Nil, Type.Name(\"C\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Nil, Term.Name(\"x\"), Nil, Nil, None, Lit(42))))))")
  }

  test("unquote Seq[T] into Option[Seq[T]]") {
    val stats = List(q"def x = 42")
    assert(q"class C { ..$stats }".show[Structure] === "Defn.Class(Nil, Type.Name(\"C\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Nil, Term.Name(\"x\"), Nil, Nil, None, Lit(42))))))")
  }

  test("extract T from Option[Seq[T]]") {
    val q"class $_ { $stat }" = q"class C { def x = 42 }"
    assert(stat.show[Structure] === "Defn.Def(Nil, Term.Name(\"x\"), Nil, Nil, None, Lit(42))")
  }

  test("extract Seq[T] from Option[Seq[T]]") {
    val q"class $_ { ..$stats }" = q"class C { def x = 42 }"
    assert(stats.show[Structure] === "Seq(Defn.Def(Nil, Term.Name(\"x\"), Nil, Nil, None, Lit(42)))")
  }

  test("extract Nil from Option[Seq[T]]") {
    val q"class $_ { ..$stats }" = q"class C"
    assert(stats.show[Structure] === "Seq()")
  }

  test("initial support for ...") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpe = $rhs" = q"def f(x: Int) = ???"
    assert(paramss.show[Structure] === "Seq(Seq(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Int\")), None)))")
    assert(q"..$mods def $name[..$tparams](...$paramss): $tpe = $rhs".show[Structure] === "Defn.Def(Nil, Term.Name(\"f\"), Nil, Seq(Seq(Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Int\")), None))), None, Term.Name(\"???\"))")
  }

  test("1 identity") {
    val arg"${expr: Term}" = q"x"
    assert(expr.show[Structure] === "Term.Name(\"x\")")
  }

  test("2 identity") {
    val expr = q"x"
    val arg = arg"${expr: Term}"
    assert(arg.show[Structure] === "Term.Name(\"x\")")
  }

  test("ellipses in template stats") {
    val mods = List(mod"private")
    val tree = q"class C { ..$mods def x = 2 }"
    assert(tree.show[Structure] === """Defn.Class(Nil, Type.Name("C"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name("this"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Def(Seq(Mod.Private(Name.Anonymous())), Term.Name("x"), Nil, Nil, None, Lit(2))))))""")
  }
}
