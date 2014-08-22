import org.scalatest._
import scala.meta.syntactic.show._

class ShowSuite extends ParseSuite {
  test("val x: Int (raw)") {
    val valx = templStat("val x: Int")
    assert(valx.show[Raw] === "Decl.Val(List(), List(Term.Name(\"x\")), Type.Name(\"Int\"))")
  }

  test("val x: Int (code)") {
    val valx = templStat("val x: Int")
    assert(valx.show[Code] === "val x: Int")
  }

  test("~(1 + 2) + ~x.y(z) + (~x).y(z)") {
    val valx = templStat("~(1 + 2) + ~x.y(z) + (~x).y(z)")
    assert(valx.show[Code] === "~(1 + 2) + ~x.y(z) + (~x).y(z)")
  }

  test("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)") {
    val valx = templStat("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)")
    assert(valx.show[Code] === "a + b + c && a + (b + c) && (a :: b :: c) && ((a :: b) :: c)")
  }

  test("(x map y).foo") {
    val valx = templStat("(x map y).foo")
    assert(valx.show[Code] === "(x map y).foo")
  }
}
