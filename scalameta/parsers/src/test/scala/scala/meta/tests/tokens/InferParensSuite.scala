package scala.meta.tests
package tokens

import scala.meta._
import org.scalatest._

import scala.meta.dialects.Scala211
import scala.meta.internal.{ ast => impl }
import scala.meta.tql._
import scala.meta.parsers._
import scala.meta.tokens._
import scala.meta.tokenizers._
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters.inferTokens

import org.scalatest.FunSuite

class InferParensSuite extends InferSuite {

  private def compareTokenCodes(a: Tree, b: Tree): Unit = {
    def trimTokens(tks: Tokens) = tks.filterNot(tk => tk.isInstanceOf[Token.BOF] || tk.isInstanceOf[Token.EOF])
    val (t1, t2) = (trimTokens(a.tokens).map(_.show[Syntax]), trimTokens(b.tokens).map(_.show[Syntax]))
    if (t1 != t2) {
      println(a.show[Syntax] + "\n" + b.show[Syntax])
    }
    assert(t1 == t2)
  }

  private def test(name: String)(code: String) {
    super.test(name) {
      val tree = code.stripMargin.parse[Term].get
      compareTokenCodes(tree, forceInferAll(tree))
    }
  }

  test("SameOpSucc1") {
    """x1 :: x2 :: xs"""
  }
  test("SameOpSucc2") {
    """x1 :: x2 :: x3 :: xs"""
  }
  test("CheckMixedAssoc1") {
    """(x :+ y) :: xs"""
  }
  test("CheckMixedAssoc2") {
    """x :+ (y :: xs)"""
  }
  test("CheckMixedAssoc3") {
    """x :: y +: z"""
  }
  test("CheckMixedAssoc4") {
    """(x :: y) +: z"""
  }
  test("CheckMixedAssoc5") {
    """x :+ y :+ z"""
  }
  test("CheckMixedAssoc6") {
    """x :+ (y :+ z)"""
  }
  test("CheckMixedAssoc7") {
    """x eq (y :: xs)"""
  }
  test("CheckMixedAssoc8") {
    """(x eq y) :: xs"""
  }
  test("CheckMixedAssoc9") {
    """a == b || c == d || y && d"""
  }
  test("CheckMixedAssoc10") {
    """a && (b || c)"""
  }
  test("CheckMixedAssoc11") {
    """a && b || c"""
  }
  test("CheckMixedAssoc12") {
    """x :+ y :+ z"""
  }
  test("CheckMixedAssoc14") {
    """(x + y) / z"""
  }
}