import org.scalatest.FunSuite
import scala.language.experimental.macros
import scala.reflect.core._

class BasicSuite extends FunSuite {
  test("hello world") {
    macro hello: String = Lit.String("Hello world!")
    assert(hello === "Hello world!")
  }
}