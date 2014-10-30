import org.scalatest._

class InterfaceSuite extends FunSuite {
  test("String.parse (partial import)") {
    import scala.meta.syntactic.parsers.RichSource
    val scala.meta.syntactic.ast.Term.Name("x") = "x".parse[scala.meta.Term]
  }

  test("String.parse (full import)") {
    import scala.meta.syntactic.parsers._
    val scala.meta.syntactic.ast.Term.Name("x") = "x".parse[scala.meta.Term]
  }
}