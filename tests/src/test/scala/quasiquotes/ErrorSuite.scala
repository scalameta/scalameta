import org.scalatest._
import org.scalameta.tests._

class ErrorSuite extends FunSuite {
  implicit val errorsWithPositionsPlease = Style.WithPositions

  test("q\"foo + class\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"foo + class"
    """) === """
      |<macro>:4: ; expected but class found.
      |      q"foo + class"
      |              ^
    """.trim.stripMargin)
  }
}