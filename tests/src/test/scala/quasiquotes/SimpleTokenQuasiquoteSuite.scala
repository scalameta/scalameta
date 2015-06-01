import scala.meta._
import scala.meta.dialects.Scala211

import scala.meta.Token._

class SimpleTokenQuasiquoteSuite extends TokenQuasiquoteSuite {
  test("`hello world` is correctly tokenized") {
    toks"hello world" shouldConformTo (
      _ isIdentNamed "hello",
      _.isWhitespace,
      _ isIdentNamed "world"
    )
  }

  test("`hello, world!` is correctly tokenized") {
    toks"hello, world!".stripWhitespaces shouldConformTo (
      _ isIdentNamed "hello",
      _.isComma,
      _ isIdentNamed "world",
      _ isIdentNamed "!"
    )
  }

  test("Integers should be correctly tokenized") {
    toks"123" shouldConformTo (
      _ isIntLit 123
    )
  }

  test("Negative integers should be correctly tokenized") {
    toks"-123" shouldConformTo (
      _.isMinus,
      _ isIntLit 123 // The second token taken alone is 123
    )
  }

  test("Single token splicing should work") {
    val hola  = toks"hello"
    val mundo = toks"world"

    toks"$hola, $mundo!".stripWhitespaces shouldConformTo (
      _ isIdentNamed "hello",
      _.isComma,
      _ isIdentNamed "world",
      _ isIdentNamed "!"
    )
  }

  test("Multiple token splicing should work") {
    val insertMe = toks"ipsum dolor sit"

    insertMe.stripWhitespaces shouldConformTo (
      _ isIdentNamed "ipsum",
      _ isIdentNamed "dolor",
      _ isIdentNamed "sit"
    )

    toks"lorem $insertMe amet".stripWhitespaces shouldConformTo (
      _ isIdentNamed "lorem",
      _ isIdentNamed "ipsum",
      _ isIdentNamed "dolor",
      _ isIdentNamed "sit",
      _ isIdentNamed "amet"
    )
  }

  test("Splicing multiple tokens at multiple locations") {
    val insertMe1 = toks"My first program"
    val insertMe2 = toks"hello world"

    toks"$insertMe1 said $insertMe2".stripWhitespaces shouldConformTo (
      _ isIdentNamed "My",
      _ isIdentNamed "first",
      _ isIdentNamed "program",
      _ isIdentNamed "said",
      _ isIdentNamed "hello",
      _ isIdentNamed "world"
    )
  }

  test("Token quasiquotes inside token quasiquotes") {
    toks"""token quasiquotes ${toks"are awesome"}!""".stripWhitespaces shouldConformTo (
      _ isIdentNamed "token",
      _ isIdentNamed "quasiquotes",
      _ isIdentNamed "are",
      _ isIdentNamed "awesome",
      _ isIdentNamed "!"
    )
  }

  test("Simple pattern matching should work") {
    val toks"hello world" = toks"hello world"
  }

  test("Pattern matching should only match actually matching tokens") {
    toks"hello world" match {
      case toks"hola mundo" => fail("Should not have matched this")
      case toks"hello world" => ()
    }
  }

  test("Pattern extraction should world") {
    val toks"$hello $world" = toks"hola mundo"
    assert(hello isIdentNamed "hola")
    assert(world isIdentNamed "mundo")
  }

  test("Pattern extraction with types specified") {
    import scala.meta.Token
    val toks"${hello: Token.Ident} ${world: Token.Ident} ${number: Token.Literal.Int}" = toks"hola mundo 123"
    assert(hello isIdentNamed "hola")
    assert(world isIdentNamed "mundo")
    assert(number isIntLit 123)
  }

  test("Extracting only with an ellipsis should world") {
    val toks"..$tokens" = toks"hello, world!"
    tokens.stripWhitespaces shouldConformTo (
      _ isIdentNamed "hello",
      _.isComma,
      _ isIdentNamed "world",
      _ isIdentNamed "!"
    )
  }

  test("Extracting using an ellipsis at the beginning should work") {
    val toks"..$beginning world!" = toks"hello, world!"
    beginning shouldConformTo (
      _ isIdentNamed "hello",
      _.isComma
    )
  }

  test("Extracting using an ellipsis in the middle should work") {
    val toks"my ..$words work" = toks"my implementation appears to work"
    words.stripWhitespaces shouldConformTo (
      _ isIdentNamed "implementation",
      _ isIdentNamed "appears",
      _ isIdentNamed "to"
    )
  }

  test("Extracting using an ellipsis in the end should work") {
    val toks"hello ..$rest" = toks"hello wonderful world!"
    rest.stripWhitespaces shouldConformTo (
      _ isIdentNamed "wonderful",
      _ isIdentNamed "world",
      _ isIdentNamed "!"
    )
  }

  test("Extracting using an ellipsis and normal unquotes should work") {
    val toks"I am ..$activity using $what, and it $result!" = toks"I am testing the extraction of tokens using ellipsis, and it works!"
    activity.stripWhitespaces shouldConformTo (
      _ isIdentNamed "testing",
      _ isIdentNamed "the",
      _ isIdentNamed "extraction",
      _ isIdentNamed "of",
      _ isIdentNamed "tokens"
    )
    assert(what isIdentNamed "ellipsis")
    assert(result isIdentNamed "works")
  }

}
