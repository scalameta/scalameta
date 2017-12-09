package scala.meta.tests
package contrib

import scala.meta._
import scala.meta.contrib._

class TokenClassesSuite extends org.scalatest.FunSuite {
  test("example usage") {
    val obtained = "foo /* comment */ bar\n\t // this is a comment".tokenize.get
      .filterNot(_.is[Trivia])
      .mkString
    val expected = "foobar"
    assert(obtained == expected)
  }
}
