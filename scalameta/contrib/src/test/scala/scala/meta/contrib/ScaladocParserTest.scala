package scala.meta.contrib

import org.scalatest.FunSuite

import scala.meta.{Defn, _}
import scala.meta.contrib.DocToken._
import scala.meta.tokens.Token.Comment

/**
  * Test for [[ScaladocParser]]
  */
class ScaladocParserTest extends FunSuite {

  private[this] def parseString(commentCode: String): Seq[DocToken] = {
    val code = commentCode.parse[Source].get
    val comments = AssociatedComments(code.tokens)
    val defnClass = code.collectFirst { case t: Defn.Class => t }.get
    val comment: Comment = comments.leading(defnClass).head
    ScaladocParser.parseScaladoc(comment)
  }

  private[this] def generateTestString(docKind: Kind): String =
    s"${docKind.label} ${(0 until docKind.numberParameters).map(i => s"Test$docKind$i").mkString(" ")}"

  test("example usage") {
    assert(
      parseString(
        """
          | /** Example scaladoc **/
          | case class foo(bar: String)
        """.stripMargin
      ).toString() === "List(Description(Example scaladoc))"
    )
  }

  test("indentation checks") {

    val expectedBody: String = "BODY"
    val expectedResult: Seq[DocToken] = Seq(DocToken(Description, expectedBody))

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
  test("label parsing") {

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
      DocToken(Description, textPrefix),
      DocToken(Constructor, constructorBody),
      DocToken(Param, paramName, paramBody),
      DocToken(TypeParam, tparamName, tparamBody),
      DocToken(Throws, throwsName, throwsBody),
      DocToken(See, seeBody)
    )

    assert(
      parseString(
        s"""
        /**
         * $textPrefix
         * ${Constructor.label} $constructorBody
         * ${Param.label} $paramName $paramBody
         * ${TypeParam.label} $tparamName $tparamBody
         * ${Throws.label} $throwsName $throwsBody
         * ${See.label} $seeBody
         */
         case class foo(bar: String)
         """
      ) === expectedStructure
    )
  }

  test("label merging") {
    val testStringToMerge = "Test DocText"
    val scaladoc: String =
      DocToken
        .labelledTokenKinds
        .flatMap(token => Seq(generateTestString(token), testStringToMerge))
        .mkString("/*\n * ", "\n * ", "\n */")

    val codeToParse : String =
      s"""
         |$scaladoc
         |case class Foo(bar: String)
      """.stripMargin

    println(codeToParse)

    val parsedScaladoc: Seq[DocToken] = parseString(codeToParse)

    assert(parsedScaladoc.size === DocToken.labelledTokenKinds.size)
    assert(parsedScaladoc.forall(_.body.endsWith(testStringToMerge)))
  }
}
