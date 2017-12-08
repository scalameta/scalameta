package scala.meta.tests
package contrib

import scala.meta._
import scala.meta.contrib._
import scala.meta.tokens.Token
import org.scalatest.FunSuite

class AssociatedCommentsTest extends FunSuite {
  test("leading") {
    val input: Source =
      """|import a.b
         |/** leading docstring */
         |object a  {
         |  // leading 2
         |  val x = 2 // trailing
         |}
         |""".stripMargin.parse[Source].get
    val comments = AssociatedComments(input)
    val defnObject = input.find(_.is[Defn.Object]).get
    val defnVal = input.find(_.is[Defn.Val]).get
    val lit = input.find(_.is[Lit]).get
    val Token.Comment(a) = comments.leading(defnVal).head
    val List(Token.Comment("* leading docstring ")) =
      comments.leading(defnObject).to[List]
    val List() = comments.trailing(defnObject).to[List]
    val List(Token.Comment(" leading 2")) =
      comments.leading(defnVal).to[List]
    val List(Token.Comment(" trailing")) =
      comments.trailing(defnVal).to[List]
    val List(Token.Comment(" trailing")) =
      comments.trailing(lit).to[List]
  }

  test("#897 first comment in file") {
    val input = """|/** Scaladoc for class A
                   |  */
                   |class A
                   |/** Scaladoc for object A
                   |  */
                   |object A""".stripMargin.parse[Source].get
    val comments = AssociatedComments(input.tokens)
    val source @ Source(stats) = input
    val result = stats.map(comments.leading)
    assert(result.forall(_.nonEmpty))
    assert(
      comments.syntax ===
        """AssociatedComments(
          |  Leading =
          |    class [30..35) => List(/**∙Scaladoc∙for∙class∙A¶∙∙*/)
          |    object [69..75) => List(/**∙Scaladoc∙for∙object∙A¶∙∙*/)
          |
          |  Trailing =
          |
          |)""".stripMargin)
  }
}
