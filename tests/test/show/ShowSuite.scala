import org.scalatest._
import scala.meta.syntactic.show._

class ShowSuite extends ParseSuite {
  test("val x: Int (raw)") {
    val valx = templStat("val x: Int")
    assert(valx.show[Raw] === "Decl.Val(List(), List(Term.Name(x)), Type.Name(Int))")
  }

  test("val x: Int (code)") {
    val valx = templStat("val x: Int")
    assert(valx.show[Code] === "val x: Int")
  }
}
