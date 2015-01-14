import org.scalatest._

class InterfaceSuite extends FunSuite {
  test("String.parse (partial import)") {
    import scala.meta.syntactic.RichOrigin
    import scala.meta.dialects.Scala211
    val scala.meta.internal.ast.Term.Name("x") = "x".parse[scala.meta.Term]
  }

  test("String.parse (full import)") {
    import scala.meta.syntactic._
    import scala.meta.dialects.Scala211
    val scala.meta.internal.ast.Term.Name("x") = "x".parse[scala.meta.Term]
  }
}