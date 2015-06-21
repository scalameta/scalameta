import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class QuasiquoteSuite extends FunSuite {
  test("rank-0 liftables") {
    assert(q"foo[${42}]".show[Code] === "foo[42]")
    assert(q"${42}".show[Code] === "42")
  }

  test("rank-1 liftables") {
    implicit def custom[U >: List[Term]]: Lift[List[Int], U] = Lift(_.map(x => q"$x"))
    assert(q"foo(..${List(1, 2, 3)})".show[Code] === "foo(1, 2, 3)")
  }

  test("p\"case $x: T => \"") {
    val x = p"x"
    assert(p"case $x: T => ".show[Code] === "case x: T =>")
  }

  test("p\"case $x @ $y => \"") {
    val x = p"x"
    val y = p"List(1, 2, 3)"
    assert(p"case $x @ $y => ".show[Code] === "case x @ List(1, 2, 3) =>")
  }

  test("q\"foo($term, ..$terms, $term)\"") {
    val term = q"x"
    val terms = List(q"y", q"z")
    assert(q"foo($term, ..$terms, $term)".show[Code] === "foo(x, y, z, x)")
  }

  test("case q\"$foo(${x: Int})\"") {
    q"foo(42)" match {
      case q"$foo(${x: Int})" =>
        assert(foo.show[Code] === "foo")
        assert(x == 42)
    }
  }

  test("case q\"$foo(${x: Int}, ..$ys, $z)\"") {
    q"foo(1, 2, 3)" match {
      case q"$_(${x: Int}, ..$y, $z)" =>
        assert(x === 1)
        assert(y.map(_.show[Code]) === List("2"))
        assert(z.show[Code] === "3")
    }
  }

  test("q\"foo($x, ..$ys, $z, ..$ts)\"") {
    val x = q"1"
    val ys = List(q"2")
    val z = q"3"
    val ts = Nil
    assert(q"foo($x, ..$ys, $z, ..$ts)".show[Code] === "foo(1, 2, 3)")
  }

  test("val q\"type $name[$_] = $_\"") {
    val member = q"type List[+A] = List[A]"
    val q"type $name[$_] = $_" = member
    assert(name.show[Code] === "List")
  }

  test("val q\"def x = ${body: Int}\"") {
    val q"def x = ${body: Int}" = q"def x = 42"
    assert(body === 42)
  }

  test("q\"... match { ..$cases }\"") {
    val cases = List(p"case foo => bar")
    assert(q"x match { ..$cases }".show[Code] === """
      |x match {
      |  case foo => bar
      |}
    """.trim.stripMargin)
  }

//  test("q\"$qname.this\"") { // fixme test is broken, so even does not compile
//    val q"$qname.this" = q"QuasiquoteSuite.this"
//    assert(qname.show[Code] === "QuasiquoteSuite")
//  }

//  test("q\"$qname.super[$qname]\"") { // fixme test is broken, so even does not compile
//    val q"$clazz.super[$tpe].$id" = q"A.super[B].x"
//    assert(clazz.show[Code] === "A")
//    assert(tpe.show[Code] === "B")
//    assert(method.show[Code] === "x")
//  }

  test("q\"$expr.$name\"") {
    val expr = q"foo"
    val name = q"bar"
    assert(q"$expr.$name".show[Code] === "foo.bar")
  }

  test("q\"$expr($name)\"") {
    val expr = q"foo"
    val name = q"bar"
    assert(q"$expr($name)".show[Code] === "foo(bar)")
  }

  test("q\"foo[..$tpes]\"") {
    val types = List(t"T", t"U")
    assert(q"foo[..$types]".show[Code] === "foo[T, U]")
  }

//  test("q\"$expr $name[..$tpes] (..$aexprs)\"") {
//    val expr = q"x"
//    val name = q"method"
//    val tpes = List(t"T", t"U")
//    val aexprs = List(q"1", q"b")
//    assert(q"$expr $name[..$tpes] (..$aexprs)".show[Code] === "x method[T, U](1, b)") // fixme test is broken, so even does not compile
//  }

  test("q\"!$expr\"") {
    val q"!$x" = q"!foo"
    assert(x.show[Code] === "foo")
  }

  test("q\"~$expr\"") {
    val expr = q"foo"
    assert(q"~$expr".show[Code] === "~foo")
  }

  test("q\"-$expr\"") {
    val q"-$x" = q"-foo"
    assert(x.show[Code] === "foo")
  }

  test("q\"+$expr\"") {
    val q"+$x" = q"+foo"
    assert(x.show[Code] === "foo")
  }

//  test("q\"$ref = $expr\"") {
//    val q"$ref = $expr" = q"a = b" // fixme test is broken, so even does not compile
//    assert(ref.show[Code] === "a")
//    assert(expr.show[Code] === "b")
//  }

  test("q\"$expr(..$aexprs) = $expr\"") {
    val q"$expr1(..$aexprs) = $expr2" = q"foo(a, b) = bar"
    assert(expr1.show[Code] === "foo")
    assert(aexprs(0).show[Code] === "a")
    assert(aexprs(1).show[Code] === "b")
    assert(expr2.show[Code] === "bar")
  }

  test("q\"return $expropt\"") {
    val q"return $expropt" = q"return foo == bar"
    assert(expropt.show[Code] === "foo == bar")
  }

  test("q\"throw $expr\"") {
    val q"throw $expr" = q"throw new RuntimeException"
    assert(expr.show[Code] === "new RuntimeException")
  }

  test("q\"$expr: $tpe\"") {
    val q"$exp: $tpe" = q"1: Double"
    assert(exp.show[Code] === "1")
    assert(tpe.show[Code] === "Double")
  }

//  test("q\"$expr: ..@$expr\"") {
//    val q"$a: ..@$b" = q"foo: @bar @baz"
//  }

//  test("q\"(..$exprs)\"") {
//    val terms = List(q"y", q"z")
//    assert(q"(..$terms)".show[Code] === "(y, z)") // fixme test is broken, so even does not compile
//  }

//  test("q\"{ ..$stats }\"") {
//    val stats = List(q"val x = 1", q"val y = 2")
//    q"{ ..$stats }"
//  }

  test("q\"if ($expr) $expr else $expr\"") {
    val q"if ($expr1) $expr2 else $expr3" = q"if (1 > 2) a else b"
    assert(expr1.show[Code] === "1 > 2")
    assert(expr2.show[Code] === "a")
    assert(expr3.show[Code] === "b")
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

//  test("q\"(..$params) => $expr\"") {
//    val q"(..$params) => $expr" = q"(x: Int, y: String) => 42" // fixme test is broken, so even does not compile
//  }

  test("q\"{ ..case $cases }\"") {
    val q"{ case ..$cases }" = q"{ case i: Int => i + 1 }"
    assert(cases(0).show[Code] === "case i: Int => i + 1")
  }

  test("q\"while ($expr) $expr\"") {
    val q"while ($expr1) $expr2" = q"while (foo) bar"
    assert(expr1.show[Code] === "foo")
    assert(expr2.show[Code] === "bar")
  }

  test("q\"do $expr while($expr)\"") {
    val q"do $expr1 while($expr2)" = q"do foo while (bar)"
    assert(expr1.show[Code] === "foo")
    assert(expr2.show[Code] === "bar")
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
    assert(q"_".show[Code] === "_")
  }

  test("q\"$expr _\"") {
    val q"$expr _" = q"foo _"
    assert(expr.show[Code] === "foo")
  }

  test("q\"$lit\"") {
    val lit = q"42"
    assert(q"$lit".show[Code] === "42")
  }

  test("val q\"$x\" = ...") {
    val q"$x" = q"42"
    assert(x.show[Code] === "42")
  }
}
