package scala.meta.tests
package tokens

import scala.meta.Token._
import scala.meta._

import munit._

// NOTE: don't run anything, just make sure that stuff compiles
class TokensSuite {

  implicit val dialect: Dialect = dialects.Scala211

  def newToken: Token = ???
  val d: Tokens = ???
  val d1: Seq[Token] = d ++ d
  val d2: Seq[Token] = newToken +: d
  val d3: Seq[Token] = d :+ newToken
  val (d41: Token) +: (d42: Seq[Token]) :+ (d43: Token) = d
  val d5a: Seq[Int] = d.map(_ => 42)
  val d5b: Seq[Token] = d5a.map(_ => newToken)
  val d5c: Seq[Token] = d.map(_.toString).flatMap(_ => List(newToken))
  val d5d: Seq[Token] = d.flatMap(_ => d)
  val d6a: Seq[String] = d.zip(List(3, 4, 5)).zipWithIndex.map { case ((x, y), _) =>
    x.toString + y.toString
  }
  val d6b: Seq[Token] = d.zip(List(3, 4, 5)).zipWithIndex.map { case ((x, y), _) => newToken }
  val d6c: Seq[Token] = d.zip(List(3, 4, 5)).zipWithIndex.flatMap { case ((x, y), _) => d }
  val d7a: Seq[(Token, Int)] = d.zipWithIndex

  // Return Tokens where possible when using collections API
  val slice: Tokens = d.slice(0, 1)
  val segmentLengthRight = d.segmentLengthRight(_.isNot[LeftParen], 0)
  val take: Tokens = d.take(2)
  val takeRight: Tokens = d.takeRight(2)
  val drop: Tokens = d.drop(2)
  val dropRight: Tokens = d.dropRight(2)
  val takeWhile: Tokens = d.takeWhile(_.isNot[RightParen])
  val takeRightWhile: Tokens = d.takeRightWhile(_.isNot[LeftParen])
  val dropWhile: Tokens = d.dropWhile(_.isNot[RightParen])
  val dropRightWhile: Tokens = d.dropRightWhile(_.isNot[LeftParen])
  val splitAt: (Tokens, Tokens) = d.splitAt(1)
  def span: (Tokens, Tokens) = d.span(_.isNot[RightParen])
  def spanRight: (Tokens, Tokens) = d.spanRight(_.isNot[LeftParen])
}

class TokensApiSuite extends FunSuite {

  implicit val dialect: Dialect = dialects.Scala211

  def tokenize(code: String): Tokens = tokenizers.Tokenize.scalametaTokenize
    .apply(inputs.Input.String(code), dialect).get

  test("Maintains Tokens type when implementing collections API methods") {
    // Drop BOF and EOF to make tests more readable
    val tokens = tokenize("((1 + 1) == 2)").drop(1).dropRight(1)

    assertEquals(tokens.length, 13)
    assertEquals(tokens.segmentLength(_.is[LeftParen]), 2)
    assertEquals(tokens.segmentLength(_.is[BOF]), 0)
    assertEquals(tokens.segmentLength(!_.is[BOF]), 13)
    assertEquals(tokens.segmentLengthRight(_.is[RightParen]), 1)
    assertEquals(tokens.segmentLengthRight(_.is[EOF]), 0)
    assertEquals(tokens.segmentLengthRight(!_.is[EOF]), 13)
    assertEquals(tokens.drop(1).segmentLengthRight(!_.is[LeftParen]), 11)
    assertEquals(tokens.take(2).syntax, "((")
    assertEquals(tokens.slice(11, 13).syntax, "2)")
    assertEquals(tokens.takeRight(2).syntax, "2)")
    assertEquals(tokens.drop(11).syntax, "2)")
    assertEquals(tokens.dropRight(11).syntax, "((")
    assertEquals(tokens.takeWhile(_.is[LeftParen]).syntax, "((")
    assertEquals(tokens.takeRightWhile(_.is[RightParen]).syntax, ")")
    assertEquals(tokens.dropWhile(_.is[LeftParen]).syntax, "1 + 1) == 2)")
    assertEquals(tokens.dropRightWhile(_.is[RightParen]).syntax, "((1 + 1) == 2")
    assert {
      val (front, back) = tokens.splitAt(8)
      front.syntax == "((1 + 1)" && back.syntax == " == 2)"
    }
    assert {
      val (front, back) = tokens.span(_.isNot[RightParen])
      front.syntax == "((1 + 1" && back.syntax == ") == 2)"
    }
    assert {
      val (front, back) = tokens.spanRight(_.isNot[LeftParen])
      front.syntax == "((" && back.syntax == "1 + 1) == 2)"
    }
  }

  test("Tokens.slice - 'from' < 'until'") {
    val tokens = tokenize("val foo = List(1, 2, 3)")

    val slice = tokens.slice(0, 5)
    assertEquals(slice.length, 5)
    for (i <- 0 until 5) assertEquals(slice(i), tokens(i))
  }

  test("Tokens.slice - 'from' == 'until'") {
    val tokens = tokenize("val foo = 1")

    assertEquals(tokens.slice(1, 1).length, 0)
  }

  test("Tokens.slice - 'from' > 'until'") {
    val tokens = tokenize("val foo = 1")

    assertEquals(tokens.slice(5, 1).length, 0)
  }

  test("Tokens.slice - 'from' < 0") {
    val tokens = tokenize("1 + 2")

    val slice = tokens.slice(-100, 1)

    assertEquals(slice.length, 1)
    assertEquals(slice.head, tokens.head)
  }

  test("Tokens.slice - 'from' > 'length'") {
    val tokens = tokenize("1 + 2")

    assertEquals(tokens.slice(100, 101).length, 0)
  }

  test("Tokens.slice - 'until' > 'length'") {
    val tokens = tokenize("val foo = 0")

    assertEquals(tokens.slice(0, 100), tokens)
  }

  test("Tokens.slice - multiple calls") {
    val tokens = tokenize("def foo(bar: String): Unit = ???")

    val slice = tokens.slice(0, 18).slice(5, 15).slice(6, 8)

    assertEquals(slice.length, 2)
    assertEquals(slice(0), tokens(11))
    assertEquals(slice(1), tokens(12))
  }

  test("Tokens.span - predicate is always true") {
    val tokens = tokenize("val foo = 0")
    val (before, after) = tokens.span(_ => true)

    assertEquals(before, tokens)
    assert(after.isEmpty)
  }

  test("Tokens.spanRight - predicate is always true") {
    val tokens = tokenize("val foo = 0")
    val (before, after) = tokens.spanRight(_ => true)

    assertEquals(after, tokens)
    assert(before.isEmpty)
  }

  test("Tokens.span/spanRight") {
    def notIdent(t: Token): Boolean = t.name != "identifier"

    val tokens = tokenize("val foo = 0")
    val (beforeL, afterL) = tokens.span(notIdent)
    val (beforeR, afterR) = tokens.spanRight(notIdent)

    assertEquals(afterL.head.text, "foo")
    assertEquals(beforeR.last.text, "foo")

    assertEquals(afterL.tail, afterR)
    assertEquals(beforeL, beforeR.dropRight(1))

    val beforeLlen = beforeL.length
    val beforeLbeg = tokens.length - beforeLlen
    assertEquals(beforeL.rskipWideIf(notIdent, beforeLbeg, -1), beforeLlen)
    assertEquals(beforeL.getWideOpt(beforeLlen).orNull, afterL.head)

    val afterLlen = afterL.length
    val afterLbeg = afterLlen - tokens.length
    assertEquals(afterL.skipWideIf(notIdent, afterLbeg, tokens.length), 0)

    val beforeRlen = beforeR.length
    val beforeRbeg = tokens.length - beforeRlen
    assertEquals(beforeR.rskipWideIf(notIdent, beforeRbeg, -1), beforeRlen - 1)

    val afterRlen = afterR.length
    val afterRbeg = afterRlen - tokens.length
    assertEquals(afterR.skipWideIf(notIdent, afterRbeg, tokens.length), -1)
    assertEquals(afterR.getWideOpt(-1).orNull, afterL.head)
  }

}
