import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class QuasiquoteSuite extends FunSuite {
  test("q\"foo($tree, ..$trees, $tree)\"") {
    val tree = q"x"
    val trees = List(q"y", q"z")
    assert(q"foo($tree, ..$trees, $tree)".show[Code] === "foo(x, y, z, x)")
  }
}