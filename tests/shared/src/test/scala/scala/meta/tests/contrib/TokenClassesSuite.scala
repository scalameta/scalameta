package scala.meta.tests
package contrib

import scala.meta._
import scala.meta.contrib._

class TokenClassesSuite extends munit.FunSuite {
  test("example usage") {
    val obtained = "foo /* comment */ bar\n\t // this is a comment".tokenize.get.filterNot {
      case Trivia() => true
      case _ => false
    }.mkString
    val expected = "foobar"
    assertEquals(obtained, expected)
  }
}
