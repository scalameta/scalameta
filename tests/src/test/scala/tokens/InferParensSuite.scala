import scala.meta._
import org.scalatest._

import scala.meta.internal.ui.inferTokens
import scala.meta.dialects.Scala211
import scala.meta.internal.{ ast => impl }
import scala.meta.tql._

import org.scalatest.FunSuite

class InferParensSuite extends InferSuite {

  private def compareTokenCodes(a: Tree, b: Tree): Unit = {
    def trimTokens(tks: Tokens) = tks.filterNot(tk => tk.isInstanceOf[Token.BOF] || tk.isInstanceOf[Token.EOF])
    val (t1, t2) = (trimTokens(a.tokens).map(_.show[Code]), trimTokens(b.tokens).map(_.show[Code]))
    if (t1 != t2) {
      println(a.show[Code] + "\n" + b.show[Code])
    }
    assert(t1 == t2)
  }

  private def test(name: String)(tree: Tree) {
    super.test(name) {
      compareTokenCodes(tree, forceInferAll(tree))
    }
  }

  test("SameOpSucc1") {
    """x1 :: x2 :: xs""".stripMargin.parse[Term]
  }
  test("SameOpSucc2") {
    """x1 :: x2 :: x3 :: xs""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc1") {
    """(x :+ y) :: xs""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc2") {
    """x :+ (y :: xs)""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc3") {
    """x :: y +: z""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc4") {
    """(x :: y) +: z""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc5") {
    """x :+ y :+ z""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc6") {
    """x :+ (y :+ z)""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc7") {
    """x eq (y :: xs)""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc8") {
    """(x eq y) :: xs""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc9") {
    """a == b || c == d || y && d""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc10") {
    """a && (b || c)""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc11") {
    """a && b || c""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc12") {
    """x :+ y :+ z""".stripMargin.parse[Term]
  }
  test("CheckMixedAssoc14") {
    """(x + y) / z""".stripMargin.parse[Term]
  }
}