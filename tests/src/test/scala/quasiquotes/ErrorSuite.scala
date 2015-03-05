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

  test("q\"foo($x)\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val x = 42
      q"foo($x)"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : Int
      | required: scala.meta.Term.Arg
      |      q"foo($x)"
      |            ^
    """.trim.stripMargin)
  }

  test("q\"foo(..$xs)\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xs = List(42)
      q"foo(..$xs)"
    """) === """
      |<macro>:5: type mismatch when splicing;
      | found   : List[Int]
      | required: scala.collection.immutable.Seq[scala.meta.Term.Arg]
      |      q"foo(..$xs)"
      |              ^
    """.trim.stripMargin)
  }
}