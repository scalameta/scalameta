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

  test("unquote q\"foo($term, ..$terms, $term)\"") {
    val term = q"x"
    val terms = List(q"y", q"z")
    assert(q"foo($term, ..$terms, $term)".show[Code] === "foo(x, y, z, x)")
  }

  test("unlift q\"foo($term, ..$terms, $term)\"") {
    val q"foo($term1, ..$terms, $term2)" = q"foo(q, w, e, 42)"
    assert(term1.show[Code] == "q")
    assert(terms(0).show[Code] == "w")
    assert(terms(1).show[Code] == "e")
    assert(term2.show[Code] === "42")
  }

  test("unlift case q\"$foo(${x: Int})\"") {
    q"foo(42)" match {
      case q"$foo(${x: Int})" =>
        assert(foo.show[Code] === "foo")
        assert(x == 42)
    }
  }

  test("unlift case q\"$foo(${x: Int}, ..$ys, $z)\"") {
    q"foo(1, 2, 3)" match {
      case q"$_(${x: Int}, ..$y, $z)" =>
        assert(x === 1)
        assert(y.map(_.show[Code]) === List("2"))
        assert(z.show[Code] === "3")
    }
  }

  test("unquote q\"foo($x, ..$ys, $z, ..$ts)\"") {
    val x = q"1"
    val ys = List(q"2")
    val z = q"3"
    val ts = Nil
    assert(q"foo($x, ..$ys, $z, ..$ts)".show[Code] === "foo(1, 2, 3)")
  }

  test("unlift q\"foo($x, ..$ys, $z)\"") {
    val q"foo($x, ..$ys, $z)" = q"foo(1, 2, 3)"
    assert(x.show[Code] === "1")
    assert(ys(0).show[Code] === "2")
    assert(z.show[Code] === "3")
  }

  test("unquote val q\"type $name[$_] = $_\"") {
    val member = q"type List[+A] = List[A]"
    val q"type $name[$_] = $_" = member
    assert(name.show[Code] === "List")
  }

  test("unlift val q\"def x = ${body: Int}\"") {
    val q"def x = ${body: Int}" = q"def x = 42"
    assert(body === 42)
  }

  test("unquote q\"... match { ..$cases }\"") {
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

  test("unquote q\"$expr.$name\"") {
    val expr = q"foo"
    val name = q"bar"
    assert(q"$expr.$name".show[Code] === "foo.bar")
  }

  //  test("unlift q\"$expr.$name\"") {
  //    val q"$expr.$name" = q"foo.bar" // fixme test is broken, so even does not compile (change `name` => `nme`)
  //    assert(expr.show[Code] === "foo")
  //    assert(name.show[Code] === "bar")
  //  }

  test("unquote q\"$expr($name)\"") {
    val expr = q"foo"
    val name = q"bar"
    assert(q"$expr($name)".show[Code] === "foo(bar)")
  }

  test("unlift q\"$expr($name)\"") {
    val q"$expr($name)" = q"foo(bar)" // compare to test("unlift q\"$expr.$name\""), this one compiles
    assert(expr.show[Code] === "foo")
    assert(name.show[Code] === "bar")
  }

  test("unquote q\"foo[..$tpes]\"") {
    val types = List(t"T", t"U")
    assert(q"foo[..$types]".show[Code] === "foo[T, U]")
  }

  test("unlift q\"foo[..$types]\"") {
    val q"foo[..$types]" = q"foo[T, U]"
    assert(types(0).show[Code] === "T")
    assert(types(1).show[Code] === "U")
  }

//  test("unquote q\"$expr $name[..$tpes] (..$aexprs)\"") {
//    val expr = q"x"
//    val name = q"method"
//    val tpes = List(t"T", t"U")
//    val aexprs = List(q"1", q"b")
//    assert(q"$expr $name[..$tpes] (..$aexprs)".show[Code] === "x method[T, U](1, b)") fixme test is broken, so even does not compile
//  }
}
