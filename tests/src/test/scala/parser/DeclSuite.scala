import scala.reflect.core._

class DeclSuite extends ParseSuite {
  import Aux._

  test("val x: Int") {
    val Decl.Val(Nil, List(Term.Ident("x", false)), Type.Ident("Int", false)) = templStat("val x: Int")
  }
}
