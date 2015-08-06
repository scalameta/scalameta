import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class QuasiquoteSuite extends FunSuite {
  test("rank-0 liftables") {
    assert(q"foo[${42}]".show[Syntax] === "foo[42]")
    assert(q"${42}".show[Syntax] === "42")
  }

  test("rank-1 liftables") {
    implicit def custom[U >: List[Term]]: Lift[List[Int], U] = Lift(_.map(x => q"$x"))
    assert(q"foo(..${List(1, 2, 3)})".show[Syntax] === "foo(1, 2, 3)")
  }

  test("p\"case $x: T => \"") {
    val x = p"x"
    assert(p"case $x: T => ".show[Syntax] === "case x: T =>")
  }

  test("p\"case $x @ $y => \"") {
    val x = p"x"
    val y = p"List(1, 2, 3)"
    assert(p"case $x @ $y => ".show[Syntax] === "case x @ List(1, 2, 3) =>")
  }

  test("q\"foo($term, ..$terms, $term)\"") {
    val term = q"x"
    val terms = List(q"y", q"z")
    assert(q"foo($term, ..$terms, $term)".show[Syntax] === "foo(x, y, z, x)")
  }

  test("case q\"$foo(${x: Int})\"") {
    q"foo(42)" match {
      case q"$foo(${x: Int})" =>
        assert(foo.show[Syntax] === "foo")
        assert(x == 42)
    }
  }

  test("case q\"$foo(${x: Int}, ..$ys, $z)\"") {
    q"foo(1, 2, 3)" match {
      case q"$_(${x: Int}, ..$y, $z)" =>
        assert(x === 1)
        assert(y.map(_.show[Syntax]) === List("2"))
        assert(z.show[Syntax] === "3")
    }
  }

  test("q\"foo($x, ..$ys, $z, ..$ts)\"") {
    val x = q"1"
    val ys = List(q"2")
    val z = q"3"
    val ts = Nil
    assert(q"foo($x, ..$ys, $z, ..$ts)".show[Syntax] === "foo(1, 2, 3)")
  }

  test("val q\"type $name[$_] = $_\"") {
    val member = q"type List[+A] = List[A]"
    val q"type $name[$_] = $_" = member
    assert(name.show[Syntax] === "List")
  }

  test("val q\"def x = ${body: Int}\"") {
    val q"def x = ${body: Int}" = q"def x = 42"
    assert(body === 42)
  }

  test("q\"$qname.this\"") {
    val q"$qname.this.$x" = q"QuasiquoteSuite.this.x"
    assert(qname.show[Syntax] === "QuasiquoteSuite")
    assert(x.show[Syntax] === "x")
    assert(q"$qname.this".show[Syntax] === "QuasiquoteSuite.this")
  }

  test("q\"$qname.super[$qname]\"") {
    val q"$clazz.super[$tpe].$id" = q"A.super[B].x"
    assert(clazz.show[Syntax] === "A")
    assert(tpe.show[Syntax] === "B")
    assert(id.show[Syntax] === "x")
    assert(q"$clazz.super[$tpe].m".show[Syntax] === "A.super[B].m")
  }

  test("q\"$expr.$name\"") {
    val expr = q"foo"
    val name = q"bar"
    assert(q"$expr.$name".show[Syntax] === "foo.bar")
  }

  test("q\"$expr($name)\"") {
    val expr = q"foo"
    val name = q"bar"
    assert(q"$expr($name)".show[Syntax] === "foo(bar)")
  }

  test("q\"foo[..$tpes]\"") {
    val types = List(t"T", t"U")
    assert(q"foo[..$types]".show[Syntax] === "foo[T, U]")
  }

  test("q\"$expr $name[..$tpes] (..$aexprs)\"") {
    val expr = q"x"
    val name = q"method"
    val tpes = List(t"T", t"U")
    val aexprs = List(q"1", q"b")
    assert(q"$expr $name[..$tpes] (..$aexprs)".show[Structure] // show[Syntax] does not show types
      === """Term.ApplyInfix(Term.Name("x"), Term.Name("method"), List(Type.Name("T"), Type.Name("U")), List(Lit.Int(1), Term.Name("b")))""")
  }

  test("q\"$a $b $c\"") {
    val a = q"x"
    val b = q"y"
    val c = q"z"
    assert(q"$a $b $c".show[Syntax] === "x y z")
  }

  test("q\"!$expr\"") {
    val q"!$x" = q"!foo"
    assert(x.show[Syntax] === "foo")
  }

  test("q\"~$expr\"") {
    val expr = q"foo"
    assert(q"~$expr".show[Syntax] === "~foo")
  }

  test("q\"-$expr\"") {
    val q"-$x" = q"-foo"
    assert(x.show[Syntax] === "foo")
  }

  test("q\"+$expr\"") {
    val q"+$x" = q"+foo"
    assert(x.show[Syntax] === "foo")
  }

  test("q\"$ref = $expr\"") {
    val q"$ref = $expr" = q"a = b"
    assert(ref.show[Syntax] === "a")
    assert(expr.show[Syntax] === "b")
  }

  test(""" val q"$x.$y = $z.$w" = q"a.b = c.d"""") {
    val q"$x.$y = $z.$w" = q"a.b = c.d"
    assert(x.show[Syntax] === "a")
    assert(y.show[Syntax] === "b")
    assert(z.show[Syntax] === "c")
    assert(w.show[Syntax] === "d")
  }

  test("q\"$expr(..$aexprs) = $expr\"") {
    val q"$expr1(..$aexprs) = $expr2" = q"foo(a, b) = bar"
    assert(expr1.show[Syntax] === "foo")
    assert(aexprs(0).show[Syntax] === "a")
    assert(aexprs(1).show[Syntax] === "b")
    assert(expr2.show[Syntax] === "bar")
  }

  test("q\"($x, y: Int)\"") {
    val x = q"x: X"
    assert(q"($x, y: Int)".show[Syntax] === "(x: X, y: Int)")
  }

  test("val q\"f($q, y: Y) = $r\" = q\"f(x: X, y: Y) = 1\"") {
    val q"f($q, y: Y) = $r" = q"f(x: X, y: Y) = 1"
    assert(q.show[Syntax] === "x: X")
    assert(r.show[Syntax] === "1")
  }

  test("q\"return $expr\"") {
    val q"return $exp" = q"return foo == bar"
    assert(exp.show[Syntax] === "foo == bar")
  }

  test("q\"throw $expr\"") {
    val q"throw $expr" = q"throw new RuntimeException"
    assert(expr.show[Syntax] === "new RuntimeException")
  }

  test("q\"$expr: $tpe\"") {
    val q"$exp: $tpe" = q"1: Double"
    assert(exp.show[Syntax] === "1")
    assert(tpe.show[Syntax] === "Double")
  }

  test("1 q\"$expr: ..$@annots\"") {
    val q"$exprr: @q ..@$annotz @$ar" = q"foo: @q @w @e @r"
    assert(exprr.show[Syntax] === "foo")
    assert(annotz.toString === "List(@w, @e)")
    assert(annotz(0).show[Syntax] === "@w")
    assert(annotz(1).show[Syntax] === "@e")
    assert(ar.show[Syntax] === "@r")
  }

  test("2 q\"$expr: ..$@annots\"") {
    val mods = List(mod"@w", mod"@e")
    assert(q"foo: @q ..@$mods @r".show[Syntax] === "foo: @q @w @e @r")
  }

  test("q\"(..$exprs)\"") {
    val terms = List(q"y", q"z")
    assert(q"(..$terms)".show[Syntax] === "(y, z)")
  }

  test("""val q"(..$params)" = q"(x: Int, y: String)" """) {
    val q"(..$params)" = q"(x: Int, y: String)"
    assert(params.toString === "List(x: Int, y: String)")
    assert(params(0).show[Syntax] === "x: Int")
    assert(params(1).show[Syntax] === "y: String")
  }

  test("1 q\"{ ..$stats }\"") {
    val stats = List(q"val x = 1", q"val y = 2")
    assert(q"{ ..$stats }".show[Syntax] ===
      """
        |{
        |  val x = 1
        |  val y = 2
        |}
      """.stripMargin.trim)
  }

  test("2 q\"{ ..$stats }\"") {
    val q"{foo; ..$statz; $astat}" = q"{foo; val a = x; val b = y; val c = z}"
    assert(statz.toString === "List(val a = x, val b = y)")
    assert(statz(0).show[Syntax] === "val a = x")
    assert(statz(1).show[Syntax] === "val b = y")
    assert(astat.show[Syntax] === "val c = z")
  }

  test("q\"if ($expr) $expr else $expr\"") {
    val q"if ($expr1) $expr2 else $expr3" = q"if (1 > 2) a else b"
    assert(expr1.show[Syntax] === "1 > 2")
    assert(expr2.show[Syntax] === "a")
    assert(expr3.show[Syntax] === "b")
  }

  test("1 q\"$expr match { ..case $cases }\"") {
    val q"$expr match { case bar => baz; ..case $casez; case q => w}" = q"foo match { case bar => baz; case _ => foo ; case q => w }"
    assert(expr.show[Syntax] === "foo")
    assert(casez.toString === "List(case _ => foo)")
    assert(casez(0).show[Syntax] === "case _ => foo")
  }

  test("2 q\"$expr match { ..case $cases }\"") {
    val q"$expr match { case bar => baz; ..case $casez; case _ => foo }" = q"foo match { case bar => baz; case _ => foo }"
    assert(expr.show[Syntax] === "foo")
    assert(casez.isEmpty)
  }

  test("3 q\"$expr match { ..case $cases }\"") {
    val q"$expr match { ..case $casez }" = q"foo match { case bar => baz; case _ => foo }"
    assert(expr.show[Syntax] === "foo")
    assert(casez.toString === "List(case bar => baz, case _ => foo)")
    assert(casez(0).show[Syntax] === "case bar => baz")
    assert(casez(1).show[Syntax] === "case _ => foo")
  }

  // TODO change to expropt (and test it) after issue #199 resolved

  test("q\"try $expr catch { ..case $cases } finally $expr\"") {
    val q"try $exp catch { case $case1 ..case $cases; case $case2 } finally $exprr" = q"try foo catch { case a => b; case _ => bar; case 1 => 2; case q => w} finally baz"
    assert(exp.show[Syntax] === "foo")
    assert(cases.toString === "List(case _ => bar, case 1 => 2)")
    assert(cases(0).show[Syntax] === "case _ => bar")
    assert(cases(1).show[Syntax] === "case 1 => 2")
    assert(case1.show[Syntax] === "case a => b")
    assert(case2.show[Syntax] === "case q => w")
    assert(exprr.show[Syntax] === "baz")
  }

  test("q\"try $expr catch $expr finally $expr\"") {
    val q"try $exp catch $exprr finally $exprrr" = q"try { foo } catch { pf } finally { bar }"
    assert(exp.show[Syntax].replace("\n", "") === "{  foo}")
    assert(exprr.show[Syntax] === "pf")
    assert(exprrr.show[Syntax].replace("\n", "") === "{  bar}")
  }

  test("""q"(i: Int) => 42" """) {
    assert(q"(i: Int) => 42".show[Syntax] === "(i: Int) => 42")
  }

  test("q\"(..$params) => $expr\"") {
    val q"(..$paramz) => $expr" = q"(x: Int, y: String) => 42"
    assert(paramz.toString === "List(x: Int, y: String)")
    assert(paramz(0).show[Syntax] === "x: Int")
    assert(paramz(1).show[Syntax] === "y: String")
    assert(expr.show[Syntax] === "42")
  }

  test("val q\"(..$q, y: Y, $e) => $r\" = q\"(x: X, y: Y, z: Z) => 1\"") {
    val q"(..$q, y: Y, $e) => $r" = q"(x: X, y: Y, z: Z) => 1"
    assert(q.toString === "List(x: X)")
    assert(q(0).show[Syntax] === "x: X")
    assert(e.show[Syntax] === "z: Z")
    assert(r.show[Syntax] === "1")
  }

  test("q\"{ ..case $cases }\"") {
    val q"{ ..case $cases }" = q"{ case i: Int => i + 1 }"
    assert(cases(0).show[Syntax] === "case i: Int => i + 1")
  }

  test("q\"while ($expr) $expr\"") {
    val q"while ($expr1) $expr2" = q"while (foo) bar"
    assert(expr1.show[Syntax] === "foo")
    assert(expr2.show[Syntax] === "bar")
  }

  test("q\"do $expr while($expr)\"") {
    val q"do $expr1 while($expr2)" = q"do foo while (bar)"
    assert(expr1.show[Syntax] === "foo")
    assert(expr2.show[Syntax] === "bar")
  }

  test("1 q\"for (..$enumerators) $expr\"") {
    val q"for ($enum1; ..$enumerators; if $cond; $enum2) $exprr" = q"for (a <- as; x <- xs; y <- ys; if bar; b <- bs) foo(x, y)"
    assert(enumerators.toString === "List(x <- xs, y <- ys)")
    assert(enumerators(0).show[Syntax] === "x <- xs")
    assert(enumerators(1).show[Syntax] === "y <- ys")
    assert(cond.show[Syntax] === "bar")
    assert(enum1.show[Syntax] === "a <- as")
    assert(enum2.show[Syntax] === "b <- bs")
    assert(exprr.show[Syntax] === "foo(x, y)")
  }

  test("2 q\"for (..$enumerators) $expr\"") {
    val a = enumerator"a <- as"
    val b = enumerator"b <- bs"
    val ab = List(a,b)
    assert(q"for (..$ab) foo".show[Syntax] === "for (a <- as; b <- bs) foo")
  }

//  test("3 q\"for (..$enumerators) $expr\"") {
//    val q"for (a <- as; if $cond; ..$enums) bar" = q"for (a <- as; if foo; b <- bs) bar" // fixme does not compile, see #203
//  }

  test("1 q\"for (..$enumerators) yield $expr\"") {
    val q"for (a <- as; ..$enumerators; b <- bs) yield $expr" = q"for (a <- as; x <- xs; y <- ys; b <- bs) yield foo(x, y)"
    assert(enumerators.toString === "List(x <- xs, y <- ys)")
    assert(enumerators(0).show[Syntax] === "x <- xs")
    assert(enumerators(1).show[Syntax] === "y <- ys")
    assert(expr.show[Syntax] === "foo(x, y)")
  }

  test("2 q\"for (..$enumerators) yield $expr\"") {
    val a = enumerator"a <- as"
    val b = enumerator"b <- bs"
    val ab = List(a,b)
    assert(q"for (..$ab) yield foo".show[Syntax] === "for (a <- as; b <- bs) yield foo")
  }

  test("1 q\"new { ..$stat } with ..$exprs { $param => ..$stats }\"") {
    val q"new $x" = q"new Foo"
    assert(x.show[Structure] === "Ctor.Ref.Name(\"Foo\")")
  }

  test("2 q\"new { ..$stat } with ..$exprs { $param => ..$stats }\"") {
    val q"new {..$stats; val b = 4} with $a {$selff => ..$statz}" = q"new {val a = 2; val b = 4} with A { self => val b = 3 }"
    assert(stats.toString === "List(val a = 2)")
    assert(stats(0).show[Syntax] === "val a = 2")
    assert(a.show[Structure] === "Ctor.Ref.Name(\"A\")")
    assert(selff.show[Structure] === "Term.Param(Nil, Term.Name(\"self\"), None, None)")
    assert(statz.toString === "List(val b = 3)")
    assert(statz(0).show[Syntax] === "val b = 3")
  }

  test("3 q\"new { ..$stat } with ..$exprs { $param => ..$stats }\"") {
    val q"new X with T { $self => def m = 42}" = q"new X with T { def m = 42 }"
    assert(self.show[Structure] === "Term.Param(Nil, Name.Anonymous(), None, None)")
  }

  // TODO fails to compile, uncomment after issue #199 resolved

//  test("4 q\"new { ..$stat } with ..$exprs { $param => ..$stats }\"") {
//    val q"new {..$stats; val b = 4} with $a {$selff => ..$statz}" = q"new {val a = 2; val b = 4}"
//  }

  test("q\"_\"") {
    assert(q"_".show[Syntax] === "_")
  }

  test("q\"$expr _\"") {
    val q"$expr _" = q"foo _"
    assert(expr.show[Syntax] === "foo")
  }

  test("q\"$lit\"") {
    val lit = q"42"
    assert(q"$lit".show[Syntax] === "42")
  }

  test("val q\"$x\" = ...") {
    val q"$x" = q"42"
    assert(x.show[Syntax] === "42")
  }

  test("arg\"$name = $expr\"") {
    val name = q"x"
    val expr = q"foo"
    assert(arg"$name = $expr".show[Syntax] === "x = foo")
  }

  test("arg\"$expr: _*\"") {
    val expr = q"foo"
    assert(arg"$expr: _*".show[Syntax] === "foo: _*")
  }

  test("arg\"$expr\"") {
    val expr = q"foo"
    assert(arg"$expr".show[Syntax] === "foo")
  }

  test("t\"$ref.$tname\"") {
    val ref = q"X"
    val tname = t"Y"
    assert(t"$ref.$tname".show[Structure] === "Type.Select(Term.Name(\"X\"), Type.Name(\"Y\"))")
  }

  test("t\"$tpe#$tname\"") {
    val tpe = t"X"
    val tname = t"Y"
    assert(t"$tpe#$tname".show[Syntax] === "X#Y")
  }

  test("t\"$ref.type\"") {
    val ref = q"X"
    assert(t"$ref.type".show[Syntax] === "X.type")
  }

  test("t\"$tpe[..$tpes]") {
    val tpe = t"X"
    val tpes = List(t"Y", t"Z")
    assert(t"$tpe[..$tpes]".show[Syntax] === "X[Y, Z]")
  }

  test("t\"$tpe $tname $tpe\"") {
    val tpe1 = t"X"
    val tname = t"Y"
    val tpe2 = t"Z"
    assert(t"$tpe1 $tname $tpe2".show[Syntax] === "X Y Z")
  }

  test("t\"(..$atpes) => $tpe\"") {
    val atpes: List[Type.Arg] = List(t"X", t"Y")
    val tpe = t"Z"
    assert(t"(..$atpes) => $tpe".show[Syntax] === "(X, Y) => Z")
  }

  test("t\"(..$tpes)\"") {
    val tpes = List(t"X", t"Y")
    assert(t"(..$tpes)".show[Syntax] === "(X, Y)")
  }

  test("1 t\"..$tpes { ..$stats }\"") {
    val t"..$tpes {..$stats}" = t"A with B with C { val a: A; val b: B }"
    assert(tpes.toString === "List(A, B, C)")
    assert(tpes(0).show[Syntax] === "A")
    assert(tpes(1).show[Syntax] === "B")
    assert(tpes(2).show[Syntax] === "C")
    assert(stats.toString === "List(val a: A, val b: B)")
    assert(stats(0).show[Syntax] === "val a: A")
    assert(stats(1).show[Syntax] === "val b: B")
  }

  test("2 t\"..$tpes { ..$stats }\"") {
    val tpes = List(t"X", t"Y")
    val stats = List(q"val a: A", q"val b: B")
    assert(t"..$tpes { ..$stats }".show[Syntax] === "X with Y { val a: A; val b: B }")
  }

  test("t\"$tpe forSome { ..$stats }\"") {
    val tpe = t"X"
    val stats = List(q"val a:A", q"val b:B")
    assert(t"$tpe forSome { ..$stats }".show[Syntax] === "X forSome { val a: A; val b: B }")
  }

  test("t\"$tpe ..@$annots\"") {
    val tpe = t"X"
    val annots = List(mod"@a", mod"@b")
    assert(t"$tpe ..@$annots".show[Syntax] === "X @a @b")
  }

  // TODO test for 'opt' after issue #199 resolved

  test("t\"_ >: $tpeopt <: $tpeopt\"") {
    val tpe1 = t"X"
    val tpe2 = t"Y"
    assert(t"_ >: $tpe1 <: $tpe2".show[Syntax] === "_ >: X <: Y")
  }

  test("1 t\"[..$tparams] => $tpe\"") {
    val t"[..$tparams] => $tpe" = t"[X, Y] => (X, Int) => Y"
    assert(tparams.toString === "List(X, Y)")
    assert(tparams(0).show[Syntax] === "X")
    assert(tparams(1).show[Syntax] === "Y")
    assert(tpe.show[Syntax] === "(X, Int) => Y")
  }

  test("2 t\"[..$tparams] => $tpe\"") {
    val tparams = List(tparam"X", tparam"Y")
    val tpe = t"Z"
    assert(t"[..$tparams] => $tpe".show[Syntax] === "[X, Y] => Z")
  }

  test("t\"$lit\"") {
    val lit = q"1"
    assert(t"$lit".show[Syntax] === "1")
  }

  test("t\"=> $tpe\"") {
    val tpe = t"X"
    assert(t"=> $tpe".show[Syntax] === "=> X")
  }

  test("t\"$tpe *\"") {
    val tpe = t"X"
    assert(t"$tpe*".show[Syntax] === "X*")
  }

  test("t\"$tpe\"") {
    val tpe = t"X"
    assert(t"$tpe".show[Syntax] === "X")
  }

  test("1 p\"$pname @ $apat\"") {
    val p"$pname @ $apat" = p"x @ y"
    assert(pname.show[Syntax] === "x")
    assert(apat.show[Syntax] === "y")
  }

  test("2 p\"$pname @ $apat\"") {
    val pname = p"x"
    val apat = p"y"
    assert(p"$pname @ $apat".show[Syntax] === "x @ y")
  }

  test("1 p\"$pat | $pat\"") {
    val p"$pat1 | $pat2" = p"x | y"
    assert(pat1.show[Syntax] === "x")
    assert(pat2.show[Syntax] === "y")
  }

  test("2 p\"$pat | $pat\"") {
    val pat1 = p"X"
    val pat2 = p"Y"
    assert(p"$pat1 | $pat2".show[Syntax] === "X | Y")
  }

  test("1 p\"(..$pats)\"") {
    val p"(..$pats)" = p"(X, Y)"
    assert(pats.toString === "List(X, Y)")
    assert(pats(0).show[Syntax] === "X")
    assert(pats(1).show[Syntax] === "Y")
  }

  test("2 p\"(..$pats)\"") {
    val pats = List(p"X", p"Y")
    assert(p"(..$pats)".show[Syntax] === "(X, Y)")
    assert(p"${pats.head}".show[Syntax] === "X")
  }

  test("p\"$ref[..$tpes](..$apats)\"") {
    val ref = q"x"
    val tpes = List(t"A", t"B")
    val apats = List(p"Q", p"W")
    assert(p"$ref[..$tpes](..$apats)".show[Syntax] === "x[A, B](Q, W)")
  }

//  test("p\"$pat $name (..$apats)\"") {
//    val pat = p"x"
//    val name = q"y"
//    val apats = List(p"Q", p"W")
//    assert(p"$pat $name (..$apats)".show[Syntax] === "...")
//  }

//  test("p\"\"\" $name\"$${..$pats}\" \"\"\"") {
//    val name = q"x"
//    val pats = List(p"X", p"Y")
//    assert(p""" $name"$${..$pats}" """.show[Syntax] === "...")
//  }

  test("1 p\"$pat: $ptpe\"") {
    val p"$pat: $ptpe" = p"x: Y"
    assert(pat.show[Syntax] === "x")
    assert(ptpe.show[Syntax] === "Y")
  }

  test("2 p\"$pat: $ptpe\"") {
    val pat = p"x"
    val ptpe = pt"Y"
    assert(p"$pat: $ptpe".show[Syntax] === "x: Y")
  }

  test("1 p\"$expr.$name\"") {
    val p"$expr.$name" = p"x.y"
    assert(p"$expr".show[Syntax] === "x")
    assert(p"$name".show[Syntax] === "y")
  }

  test("2 p\"$expr.$name\"") {
    val expr = q"foo(bar)"
    val name = q"x"
    assert(p"$expr.$name".show[Syntax] === "foo(bar).x")
  }

  test("p\"$lit\"") {
    val lit = q"1"
    assert(p"$lit".show[Syntax] === "1")
  }

  test("p\"case $pat if $expropt => $expr\"") {
    val pat = p"X"
    val expropt = q"foo"
    val expr = q"bar"
    assert(p"case $pat if $expropt => $expr".show[Syntax] === "case X if foo => bar")
  }

  test("p\"$pat\"") {
    val pat = p"X"
    assert(p"$pat".show[Syntax] === "X")
  }

  test("pt\"$ref.$tname\"") {
    val ref = q"x"
    val tname = t"A"
    assert(pt"$ref.$tname".show[Syntax] === "x.A")
  }

//  test("pt\"$ref#$tname\"") {
//    val ref = q"x"
//    val tname = t"A"
//    assert(pt"$ref#$tname".show[Syntax] === "x#A")
//  }

  test("pt\"$ref.type\"") {
    val ref = q"x"
    assert(pt"$ref.type".show[Syntax] === "x.type")
  }

  test("pt\"$ptpe[..$ptpes]") {
    val ptpe = pt"X"
    val ptpes = List(pt"Y", pt"Z")
    assert(pt"$ptpe[..$ptpes]".show[Syntax] === "X[Y, Z]")
  }

//  test("pt\"$ptpe $tname $ptpe\"") {
//    val ptpe1 = pt"X"
//    val tname = t"Y"
//    val ptpe2 = pt"Z"
//    assert(pt"$ptpe $tname $ptpe")
//  }

//  test("pt\"(..$ptpes) => $ptpe\"") {
//    val ptpes = List(pt"X", pt"Y")
//    val ptpe = pt"Z"
//    assert(pt"(..$ptpes).show[Syntax] => $ptpe" === "...")
//  }

  test("pt\"(..$ptpes)\"") {
    val ptpes = List(pt"X", pt"Y")
    assert(pt"(..$ptpes)".show[Syntax] === "(X, Y)")
  }

  test("1 pt\"..$ptpes { ..$stats }\"") {
    val ptpes = List(pt"X", pt"Y")
    val stats = List(q"val a: A", q"val b: B")
    assert(pt"..$ptpes { ..$stats }".show[Syntax] === "X with Y { val a: A; val b: B }")
  }

  test("2 pt\"..$ptpes { ..$stats }\"") {
    val pt"..$ptpes { ..$stats }" = pt"X with Y { val a: A }"
    assert(ptpes.toString === "List(X, Y)")
    assert(ptpes(0).show[Syntax] === "X")
    assert(ptpes(1).show[Syntax] === "Y")
    assert(stats.toString === "List(val a: A)")
    assert(stats(0).show[Syntax] === "val a: A")
  }

//  test("pt\"$ptpe forSome { ..$stats }\"") {
//    val ptpe = pt"X"
//    val stats = List(q"val a: A")
//    assert(pt"$ptpe forSome { ..$stats }".show[Syntax] === "X forSome { val a: A }")
//  }

  test("pt\"$ptpe ..$@annots\"") {
    val ptpe = pt"X"
    val annots = List(mod"@q", mod"@w")
    assert(pt"$ptpe ..@$annots".show[Syntax] === "X @q @w")
  }

  test("t\"_ >: $tpeopt <: tpeopt\"") {
    val tpe1 = t"X"
    val tpe2 = t"Y"
    assert(pt"_ >: $tpe1 <: $tpe2".show[Syntax] === "_ >: X <: Y")
  }

  test("pt\"$lit\"") {
    val lit = q"1"
    assert(pt"$lit".show[Syntax] === "1")
  }

//  test("1 q\"import ..($ref.{..$importees})\"") {
//    val ref = q"x"
//    val importees = List(importee"A", importee"B")
//    assert(q"import ..($ref.{..$importees})".show[Syntax] === "")
//  }

//  test("2 q\"import ..($ref.{..$importees})\"") {
//    val q"import ..(x.{..$importees})" = q"import a.A"
//  }
}
