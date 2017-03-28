package scala.meta
package contrib

class TokenClassesSuite extends org.scalatest.FunSuite {
  test("example usage") {
    val obtained = "foo /* comment */ bar\n\t // this is a comment".tokenize.get
      .filterNot(_.is[Trivia])
      .mkString
    val expected = "foobar"
    assert(obtained == expected)
  }
}
