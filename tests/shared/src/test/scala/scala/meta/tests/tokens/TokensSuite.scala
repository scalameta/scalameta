package scala.meta.tests
package tokens

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.Token._
import org.scalatest._

// NOTE: don't run anything, just make sure that stuff compiles
class TokensSuite {
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
  val d6a: Seq[String] = d.zip(List(3, 4, 5)).zipWithIndex.map{ case ((x, y), _) => x.toString + y.toString }
  val d6b: Seq[Token] = d.zip(List(3, 4, 5)).zipWithIndex.map{ case ((x, y), _) => newToken }
  val d6c: Seq[Token] = d.zip(List(3, 4, 5)).zipWithIndex.flatMap{ case ((x, y), _) => d }
  val d7a: Seq[(Token, Int)] = d.zipWithIndex

  //Return Tokens where possible when using collections API
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
  def tokenize(code: String): Tokens = {
    val convert = scala.meta.inputs.stringToInput
    val tokenize = scala.meta.tokenizers.Tokenize.scalametaTokenize
    val dialect = Scala211
    code.tokenize(convert, tokenize, dialect).get
  }

  test("Maintains Tokens type when implementing collections API methods") {
    //Drop BOF and EOF to make tests more readable
    var tokens = tokenize("((1 + 1) == 2)").drop(1).dropRight(1)

    assert(tokens.length == 13)
    assert(tokens.segmentLength(_.is[LeftParen]) === 2)
    assert(tokens.segmentLengthRight(_.is[RightParen]) === 1)
    assert(tokens.take(2).syntax === "((")
    assert(tokens.slice(11, 13).syntax === "2)")
    assert(tokens.slice(11, 13).syntax === "2)")
    assert(tokens.takeRight(2).syntax === "2)")
    assert(tokens.drop(11).syntax === "2)")
    assert(tokens.dropRight(11).syntax === "((")
    assert(tokens.takeWhile(_.is[LeftParen]).syntax === "((")
    assert(tokens.segmentLengthRight(_.is[RightParen]) == 1)
    assert(tokens.takeRightWhile(_.is[RightParen]).syntax === ")")
    assert(tokens.segmentLength(_.is[LeftParen]) == 2)
    assert(tokens.dropWhile(_.is[LeftParen]).syntax === "1 + 1) == 2)")
    assert(tokens.dropRightWhile(_.is[RightParen]).syntax === "((1 + 1) == 2")
    assert{
      val (front, back) = tokens.splitAt(8)
      front.syntax === "((1 + 1)" && back.syntax === " == 2)"
    }
    assert{
      val (front, back) = tokens.span(_.isNot[RightParen])
      front.syntax === "((1 + 1" && back.syntax === ") == 2)"
    }
    assert{
      val (front, back) = tokens.spanRight(_.isNot[LeftParen])
      front.syntax === "((" && back.syntax === "1 + 1) == 2)"
    }
  }
}
