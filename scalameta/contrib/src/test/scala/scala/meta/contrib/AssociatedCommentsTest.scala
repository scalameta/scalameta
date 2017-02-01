package scala.meta.contrib

import scala.meta._
import scala.collection.immutable.Seq
import scala.meta.tokens.Token
import org.scalatest.FunSuite

class AssociatedCommentsTest extends FunSuite {
  val input: Source =
    """|import a.b
       |/** leading docstring */
       |object a  {
       |  // leading 2
       |  val x = 2 // trailing
       |}
       |""".stripMargin.parse[Source].get
  val comments = AssociatedComments(input)
  test("leading") {
    val defnObject = input.find(_.is[Defn.Object]).get
    val defnVal    = input.find(_.is[Defn.Val]).get
    val lit        = input.find(_.is[Lit]).get
    comments.leading(defnVal).head match {
      case Token.Comment(a) =>
        println(s"'$a'")
    }
    val Seq(Token.Comment("* leading docstring ")) =
      comments.leading(defnObject).to[Seq]
    val Seq() = comments.trailing(defnObject).to[Seq]
    val Seq(Token.Comment(" leading 2")) =
      comments.leading(defnVal).to[Seq]
    val Seq(Token.Comment(" trailing")) =
      comments.trailing(defnVal).to[Seq]
    val Seq(Token.Comment(" trailing")) =
      comments.trailing(lit).to[Seq]
  }
}
