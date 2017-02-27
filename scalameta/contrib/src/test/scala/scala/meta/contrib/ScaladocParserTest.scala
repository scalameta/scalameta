package scala.meta.contrib

import org.scalatest.FunSuite

import scala.meta.contrib.{AssociatedComments, _}
import scala.meta.{Defn, Source, _}
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

  private[this] def generateTestString(docKind: TagKind): String =
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
  }

  test("paragraph parsing") {
    val expectedBody = "BODY"
    assert(
      parseString(
        s"""
         /**
          *
          *$expectedBody
          *
          */
          case class foo(bar: String)
         """
      ) === Seq(DocToken(Paragraph), DocToken(Description, expectedBody), DocToken(Paragraph))
    )
    assert(
      parseString(
        s"""
         /**
          *
          *$expectedBody
          *
          *$expectedBody
          *
          */
          case class foo(bar: String)
         """
      ) === Seq(
        DocToken(Paragraph),
        DocToken(Description, expectedBody),
        DocToken(Paragraph),
        DocToken(Description, expectedBody),
        DocToken(Paragraph)
      )
    )
  }

  test("code blocks") {

    val testDescription = "This is a codeblock:"

    val codeBlock1 = "\"HELLO MARIANO\""
    val codeBlock2 = "\"HELLO SORAYA\""
    val complexCodeBlock =
      """
        |ggmqwogmwogmqwomgq
        |gmqwgoiqmgoqmwomw
      """.stripMargin.trim

    val result: Seq[DocToken] =
      parseString(
        s"""
          /**
            * $testDescription {{{ $codeBlock1 }}}
            * $testDescription
            * {{{ $codeBlock2 }}}
            *
            * $testDescription
            *
            * {{{
            *   $complexCodeBlock
            * }}}
            */
            case class foo(bar : String)
       """.stripMargin
      )

    val expectation = Seq(
      DocToken(Description, testDescription),
      DocToken(CodeBlock, codeBlock1),
      DocToken(Description, testDescription),
      DocToken(CodeBlock, codeBlock2),
      DocToken(Paragraph),
      DocToken(Description, testDescription),
      DocToken(Paragraph),
      DocToken(CodeBlock, complexCodeBlock)
    )
    assert(result === expectation)
  }

  test("label parsing/merging") {
    val testStringToMerge = "Test DocText"
    val scaladoc: String =
      DocToken.tagTokenKinds
        .flatMap(token => Seq(generateTestString(token), testStringToMerge))
        .mkString("/*\n * ", "\n * ", "\n */")

    val codeToParse: String =
      s"""
         |$scaladoc
         |case class Foo(bar: String)
      """.stripMargin

    val parsedScaladoc: Seq[DocToken] = parseString(codeToParse)

    // Inherit doc does not merge
    assert(parsedScaladoc.size === DocToken.tagTokenKinds.size)

    // Inherit doc does not merge
    assert(
      parsedScaladoc
        .filterNot(_.kind == DocToken.InheritDoc)
        .forall(_.body.getOrElse("").endsWith(testStringToMerge))
    )
  }
}
