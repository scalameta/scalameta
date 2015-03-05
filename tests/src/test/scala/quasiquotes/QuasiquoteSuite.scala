import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class QuasiquoteSuite extends FunSuite {
  test("q\"foo($term, ..$terms, $term)\"") {
    val term = q"x"
    val terms = List(q"y", q"z")
    assert(q"foo($term, ..$terms, $term)".show[Code] === "foo(x, y, z, x)")
  }

  test("q\"foo[..$types]\"") {
    val types = List(t"T", t"U")
    assert(q"foo[..$types]".show[Code] === "foo[T, U]")
  }
}