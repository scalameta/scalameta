package scala.meta.contrib

import org.scalatest.FunSuite

import scala.meta.{Defn, _}
import scala.meta.contrib.DocToken._
import scala.meta.tokens.Token.Comment

/**
  * Test for [[ScaladocParser]]
  */
class ScaladocParserTest extends FunSuite {

  def parseString(commentCode: String): Seq[DocToken] = {
    val code = commentCode.parse[Source].get
    val comments = AssociatedComments(code.tokens)
    val defnClass = code.collectFirst { case t: Defn.Class => t }.get
    val comment: Comment = comments.leading(defnClass).head
    ScaladocParser.parseScaladoc(comment)
  }

  test("example usage") {
    assert(
      parseString(
        """
        | /** Example scaladoc **/
        | case class foo(bar: String)
        """.stripMargin
      ).toString() === "List(DocText(Example scaladoc))"
    )
  }

  test("Indentation checks") {

    val expectedBody: String = "BODY"
    val expectedResult: Seq[DocToken] = Seq(DocToken(DocText, expectedBody))

    assert(
      parseString(
        s"""
         /** $expectedBody*/
          case class foo(bar: String)
         """
      ) === expectedResult
    )
    assert(
      parseString(
        s"""
         /** $expectedBody
          */
          case class foo(bar: String)
         """
      ) === expectedResult
    )
    assert(
      parseString(
        s"""
         /**       $expectedBody
          */
          case class foo(bar: String)
         """
      ) === expectedResult
    )
    assert(
      parseString(
        s"""
         /**
          *$expectedBody
          */
          case class foo(bar: String)
         """
      ) === expectedResult
    )
    assert(
      parseString(
        s"""
         /**
          *
          *
          *
          *$expectedBody
          */
          case class foo(bar: String)
         """
      ) === expectedResult
    )
    assert(
      parseString(
        s"""
         /**
          *
            *
        *  $expectedBody
         *
          */
          case class foo(bar: String)
         """.stripMargin
      ) === expectedResult
    )
  }

  // TODO: Add the rest of the labels
  test("Label parsing") {

    // DocText
    val textPrefix = "Body"

    // DocConstructor
    val constructorBody = s"$textPrefix-Constructor"

    // DocParam
    val paramName = "parameterName"
    val paramBody = s"parameterBody"

    // DocTypeParam
    val tparamName = "T"
    val tparamBody = "Simple parameter"

    // DocThrows
    val throwsName = "IllegalArgumentException"
    val throwsBody = "when something fails"

    // DocSee
    val seeBody = "See body"

    // Expectations
    val expectedStructure = Seq(
      DocToken(DocText, textPrefix),
      DocToken(DocConstructor, constructorBody),
      DocToken(DocParam, paramName, paramBody),
      DocToken(DocTypeParam, tparamName, tparamBody),
      DocToken(DocThrows, throwsName, throwsBody),
      DocToken(DocSee, seeBody)
    )

    assert(
      parseString(
        s"""
        /**
         * $textPrefix
         * ${DocConstructor.label} $constructorBody
         * ${DocParam.label} $paramName $paramBody
         * ${DocTypeParam.label} $tparamName $tparamBody
         * ${DocThrows.label} $throwsName $throwsBody
         * ${DocSee.label} $seeBody
         */
         case class foo(bar: String)
         """
      ) === expectedStructure
    )
  }

  // TODO: Test for multine token merging
}
