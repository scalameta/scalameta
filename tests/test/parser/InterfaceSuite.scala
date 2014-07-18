import org.scalatest._
import scala.meta._

class InterfaceSuite extends FunSuite {
  test("String.parse (partial import)") {
    import scala.meta.syntactic.parsers.RichSource
    val Term.Name("x") = "x".parse[Term]
  }

  test("String.parse (full import)") {
    import scala.meta.syntactic.parsers._
    val Term.Name("x") = "x".parse[Term]
  }
}