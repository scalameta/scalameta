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

  test("q\"... match { ..$cases }\"") {
    val cases = List(p"case foo => bar")
    assert(q"x match { ..$cases }".show[Syntax] === """
      |x match {
      |  case foo => bar
      |}
    """.trim.stripMargin)
  }

//  test("q\"$qname.this\"") { // fixme test is broken, so even does not compile
//    val q"$qname.this" = q"QuasiquoteSuite.this"
//    assert(qname.show[Syntax] === "QuasiquoteSuite")
//  }

//  test("q\"$qname.super[$qname]\"") { // fixme test is broken, so even does not compile
//    val q"$clazz.super[$tpe].$id" = q"A.super[B].x"
//    assert(clazz.show[Syntax] === "A")
//    assert(tpe.show[Syntax] === "B")
//    assert(method.show[Syntax] === "x")
//  }

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

//  test("q\"$expr $name[..$tpes] (..$aexprs)\"") {
//    val expr = q"x"
//    val name = q"method"
//    val tpes = List(t"T", t"U")
//    val aexprs = List(q"1", q"b")
//    assert(q"$expr $name[..$tpes] (..$aexprs)".show[Syntax] === "x method[T, U](1, b)") // fixme test is broken, so even does not compile
//  }

//  test("q\"$a $b $c\"") {
//    val a = q"x"
//    val b = q"y"
//    val c = q"z"
//    assert(q"$a $b $c".show[Syntax] === "x y z") // fixme test is broken, so even does not compile
//  }

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

  test("q\"return $expropt\"") {
    val q"return $expropt" = q"return foo == bar"
    assert(expropt.show[Syntax] === "foo == bar")
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

//  test("q\"$expr: ..@$expr\"") {
//    val q"$a: ..@$b" = q"foo: @bar @baz"
//  }

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

//  test("q\"{ ..$stats }\"") {
//    val stats = List(q"val x = 1", q"val y = 2")
//    q"{ ..$stats }"
//  }

  test("q\"if ($expr) $expr else $expr\"") {
    val q"if ($expr1) $expr2 else $expr3" = q"if (1 > 2) a else b"
    assert(expr1.show[Syntax] === "1 > 2")
    assert(expr2.show[Syntax] === "a")
    assert(expr3.show[Syntax] === "b")
  }

//  test("q\"$expr match { ..case $cases }\"") {
//    val q"$expr match { ..case $cases }" =
//      q"foo match { case bar => baz; case _ => foo }" // fixme test is broken, so even does not compile
//  }

//  test("q\"try $expr catch { ..case $cases } finally $expropt\"") {
//    val q"try $expr catch { ..case $cases } finally $expropt" = // fixme test is broken, so even does not compile
//      q"try foo catch { case _ => bar} finally baz"
//  }

//  test("q\"try $expr catch $expr finally $expropt\"") {
//    val q"try $expr catch $expr finally $expropt" =
//      q"try foo catch { case _ => } finally bar" // fixme test is broken, so even does not compile
//  }

  test("""q"(i: Int) => 42" """) {
    q"(i: Int) => 42" // should compile
  }

//
//  test("q\"(..$params) => $expr\"") {
//    val q"(..$params) => $expr" = q"(x: Int, y: String) => 42"
//  }

  test("q\"{ ..case $cases }\"") {
    val q"{ case ..$cases }" = q"{ case i: Int => i + 1 }"
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

//  test("q\"for (..$enumerators) $expr\"") {
//    val q"for (..$enumerators) $expr" = q"for (x <- xs; y <- ys) foo(x, y)" // fixme test is broken, so even does not compile
//  }

//  test("q\"for (..$enumerators) yield $expr\"") {
//    val q"for (..$enumerators) yield $expr" =  q"for (x <- xs; y <- ys) yield foo(x, y)" // fixme test is broken, so even does not compile
//  }

//  test("q\"new $template\"") {
//    val q"new $template" = q"new Foo" // fixme test is broken, so even does not compile
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
}
