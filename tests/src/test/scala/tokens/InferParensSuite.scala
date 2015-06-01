import scala.meta._
import org.scalatest._

import scala.meta.internal.ui.inferTokens
import scala.meta.dialects.Scala211
import scala.meta.internal.{ ast => impl }
import scala.meta.tql._

import org.scalatest.FunSuite

class InferParensSuite extends FunSuite {

  // Force synthetic tokens for all tree nodes using TQL
  val forceInfer = topDown(transform {

    case t: impl.Term.This => t.copy() andCollect Unit
    case t: impl.Term.Super => t.copy() andCollect Unit
    case t: impl.Term.Select => t.copy() andCollect Unit
    case t: impl.Term.Interpolate => t.copy() andCollect Unit
    case t: impl.Term.Apply => t.copy() andCollect Unit
    case t: impl.Term.ApplyType => t.copy() andCollect Unit
    case t: impl.Term.ApplyInfix => t.copy() andCollect Unit
    case t: impl.Term.ApplyUnary => t.copy() andCollect Unit
    case t: impl.Term.Assign => t.copy() andCollect Unit
    case t: impl.Term.Update => t.copy() andCollect Unit
    case t: impl.Term.Return => t.copy() andCollect Unit
    case t: impl.Term.Throw => t.copy() andCollect Unit
    case t: impl.Term.Ascribe => t.copy() andCollect Unit
    case t: impl.Term.Annotate => t.copy() andCollect Unit
    case t: impl.Term.Tuple => t.copy() andCollect Unit
    case t: impl.Term.Block => t.copy() andCollect Unit
    case t: impl.Term.If => t.copy() andCollect Unit
    case t: impl.Term.TryWithCases => t.copy() andCollect Unit
    case t: impl.Term.TryWithTerm => t.copy() andCollect Unit
    case t: impl.Term.Function => t.copy() andCollect Unit
    case t: impl.Term.PartialFunction => t.copy() andCollect Unit
    case t: impl.Term.While => t.copy() andCollect Unit
    case t: impl.Term.Do => t.copy() andCollect Unit
    case t: impl.Term.For => t.copy() andCollect Unit
    case t: impl.Term.ForYield => t.copy() andCollect Unit
    case t: impl.Term.New => t.copy() andCollect Unit
    case t: impl.Term.Placeholder => t.copy() andCollect Unit
    case t: impl.Term.Eta => t.copy() andCollect Unit
    case t: impl.Term.Arg.Named => t.copy() andCollect Unit
    case t: impl.Term.Arg.Repeated => t.copy() andCollect Unit
    case t: impl.Term.Param => t.copy() andCollect Unit

    case t: impl.Type.Select => t.copy() andCollect Unit
    case t: impl.Type.Project => t.copy() andCollect Unit
    case t: impl.Type.Singleton => t.copy() andCollect Unit
    case t: impl.Type.Apply => t.copy() andCollect Unit
    case t: impl.Type.ApplyInfix => t.copy() andCollect Unit
    case t: impl.Type.Function => t.copy() andCollect Unit
    case t: impl.Type.Tuple => t.copy() andCollect Unit
    case t: impl.Type.Compound => t.copy() andCollect Unit
    case t: impl.Type.Existential => t.copy() andCollect Unit
    case t: impl.Type.Annotate => t.copy() andCollect Unit
    case t: impl.Type.Placeholder => t.copy() andCollect Unit
    case t: impl.Type.Arg.Repeated => t.copy() andCollect Unit
    case t: impl.Type.Arg.ByName => t.copy() andCollect Unit
    case t: impl.Type.Param => t.copy() andCollect Unit

    case t: impl.Pat.Var.Term => t.copy() andCollect Unit
    case t: impl.Pat.Wildcard => t.copy() andCollect Unit
    case t: impl.Pat.Bind => t.copy() andCollect Unit
    case t: impl.Pat.Alternative => t.copy() andCollect Unit
    case t: impl.Pat.Tuple => t.copy() andCollect Unit
    case t: impl.Pat.Extract => t.copy() andCollect Unit
    case t: impl.Pat.ExtractInfix => t.copy() andCollect Unit
    case t: impl.Pat.Interpolate => t.copy() andCollect Unit
    case t: impl.Pat.Typed => t.copy() andCollect Unit

    case t: impl.Pat.Type.Apply => t.copy() andCollect Unit
    case t: impl.Pat.Type.ApplyInfix => t.copy() andCollect Unit
  })

  private def trimTokens(tks: Tokens) = tks.filterNot(tk => tk.isInstanceOf[Token.BOF] || tk.isInstanceOf[Token.EOF])

  private def compareTokenCodes(a: Tree, b: Tree, ignoreSpaces: Boolean = false): Unit = {
    def getCodes(t: Tree) = {
      val codes = trimTokens(t.tokens).map(_.show[Code])
      if (ignoreSpaces) codes.filter(_ != " ")
      else codes
    }
    val t1 = getCodes(a)
    val t2 = getCodes(b)
    if (t1 != t2) {
      println(a.show[Raw] + "\n" + b.show[Raw])
      println(t1 + "\n" + t2)
    }
    assert(t1 == t2)
  }

  private def debug(trees: Tree*): Unit = {
    trees.foreach { tree =>
      println
      println(tree.show[Raw])
      println(tree.tokens)
    }
  }

  private def test(name: String)(tree: Tree) {
    super.test(name) {
      val inferred = forceInfer(tree).tree.get
      compareTokenCodes(tree, inferred)
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
    """a == b || c == d || y & d""".stripMargin.parse[Term]
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
}