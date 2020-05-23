package scala.meta.tests.parsers

import scala.meta.internal.Scaladoc
import scala.meta.internal.Scaladoc._
import scala.meta.internal.parsers.ScaladocParser

import munit.FunSuite

/**
 * Test for [[ScaladocParser]]
 */
class ScaladocParserSuite extends FunSuite {

  private def parseString(comment: String) =
    ScaladocParser.parse(comment.trim)

  private def generateTestString(tagType: TagType): String = {
    val sb = new StringBuilder
    sb.append(tagType.tag)
    if (tagType.hasLabel) sb.append(" TestLabel")
    if (tagType.hasDesc) sb.append("\n *  Test Description")
    sb.result()
  }

  test("example usage") {
    assertEquals(
      parseString("/** Example scaladoc */"),
      Some(Scaladoc(Paragraph(Text(Word("Example"), Word("scaladoc")))))
    )
  }

  test("indentation checks") {

    val expectedBody: String = "BODY"
    val expectedBodyToken = Text(Word(expectedBody))

    assertEquals(
      parseString(s"/** $expectedBody*/"),
      Some(Scaladoc(Paragraph(expectedBodyToken)))
    )
    assertEquals(
      parseString(
        s"""
         /** $expectedBody
          */
         """
      ),
      Some(Scaladoc(Paragraph(expectedBodyToken)))
    )
    assertEquals(
      parseString(
        s"""
         /**       $expectedBody
          */
         """
      ),
      Some(Scaladoc(Paragraph(expectedBodyToken)))
    )
    assertEquals(
      parseString(
        s"""
         /**
          *$expectedBody
          */
         """
      ),
      Some(Scaladoc(Paragraph(expectedBodyToken)))
    )
  }

  test("paragraph parsing") {
    val descriptionBody = "Description Body"
    val words = descriptionBody.split("\\s+").map(Word.apply)
    assertEquals(
      parseString(
        s"""
         /**
          *
          *$descriptionBody
          *
          *$descriptionBody
          *
          */
         """
      ),
      Option(
        Scaladoc(
          Paragraph(Text(words: _*)),
          Paragraph(Text(words: _*))
        )
      )
    )
  }

  test("paragraph parsing with leading space") {
    val descriptionBody = "Description Body"
    val words = descriptionBody.split("\\s+").map(Word.apply)
    assertEquals(
      parseString(
        s"""
         /**
          *
          * $descriptionBody
          *
          * $descriptionBody
          *
          */
         """
      ),
      Option(
        Scaladoc(
          Paragraph(Text(words: _*)),
          Paragraph(Text(words: _*))
        )
      )
    )
  }

  test("paragraph parsing with references") {
    val descriptionBody = "Description Body"
    val words = descriptionBody.split("\\s+").toSeq.map(Word.apply)
    val ref = Seq(Link("Description", "Body"))
    assertEquals(
      parseString(
        s"""
         /**
          *
          * $descriptionBody [[ $descriptionBody ]]
          *
          * $descriptionBody [[ $descriptionBody ]]
          * $descriptionBody
          *
          * [[ $descriptionBody ]] $descriptionBody
          *
          * $descriptionBody
          * [[ $descriptionBody ]] $descriptionBody
          *
          */
         """
      ),
      Option(
        Scaladoc(
          Paragraph(Text(words ++ ref: _*)),
          Paragraph(Text(words ++ ref ++ words: _*)),
          Paragraph(Text(ref ++ words: _*)),
          Paragraph(Text(words ++ ref ++ words: _*))
        )
      )
    )
  }

  test("code blocks") {

    val testDescription = "This is a codeblock:"
    val words: Seq[Word] = testDescription.split("\\s+").map(Word.apply)

    val codeBlock1 = "\"HELLO MARIANO\""
    val codeBlock2 = "\"HELLO SORAYA\""
    val complexCodeBlock = // keep all newlines and leading spaces
      """|  ggmqwogmwogmqwomgq
         |    val x = 1 // sdfdfh
         |   // zzz
         |   gmqwgoiqmgoqmwomw""".stripMargin.split("\n")
    val complexCodeBlockAsComment = complexCodeBlock.mkString("\n *")

    val result =
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
            *$complexCodeBlockAsComment
            * }}}
            * {{{
            *$complexCodeBlockAsComment }}}
            */
       """.stripMargin
      )

    val expectation = Option(
      Scaladoc(
        Paragraph(Text((words :+ CodeExpr(codeBlock1)) ++ (words :+ CodeExpr(codeBlock2)): _*)),
        Paragraph(Text(words: _*)),
        Paragraph(CodeBlock(complexCodeBlock: _*), CodeBlock(complexCodeBlock: _*))
      )
    )
    assertEquals(result, expectation)
  }

  test("headings") {
    val level1HeadingBody = "Level 1"
    val level2HeadingBody = "Level 2"
    val level3HeadingBody = "Level 3"
    val level4HeadingBody = "Level 4"
    val level5HeadingBody = "Level 5"
    val level6HeadingBody = "Level 6"
    val bad1 = "=======7isTooMuch======="
    val bad2 = "======6left,5right====="
    val bad3 = "=====5left,5right=====debris"
    val bad4 = "debris=====5left,5right====="

    val result =
      parseString(
        s"""
        /**
          * =$level1HeadingBody=
          * ==$level2HeadingBody==
          * ===$level3HeadingBody===
          * ====$level4HeadingBody====
          * =====$level5HeadingBody=====
          * ======$level6HeadingBody======
          * $bad1
          * $bad2
          * $bad3
          * $bad4
          */
         """
      )
    val expectation = Option(
      Scaladoc(
        Paragraph(
          Heading(1, level1HeadingBody),
          Heading(2, level2HeadingBody),
          Heading(3, level3HeadingBody),
          Heading(4, level4HeadingBody),
          Heading(5, level5HeadingBody),
          Heading(6, level6HeadingBody),
          Text(Word(bad1)),
          Text(Word(bad2)),
          Text(Word(bad3), Word(bad4))
        )
      )
    )
    assertEquals(result, expectation)
  }

  test("lists 1") {
    val list11 = "List11"
    val list12 = "List12"
    val list21 = "List21"
    val list22 = "List22"

    val result =
      parseString(
        s"""
        /**
          * Some text:
          * - $list11
          *    1. $list21
          * - $list12
          *    a. $list21
          *    a. $list22
          */
         """
      )
    val expected = Option(
      Scaladoc(
        Paragraph(
          Text(Word("Some"), Word("text:")),
          ListBlock(
            "-",
            ListItem(Text(Word(list11)), Some(ListBlock("1.", ListItem(Text(Word(list21)))))),
            ListItem(
              Text(Word(list12)),
              Some(ListBlock("a.", ListItem(Text(Word(list21))), ListItem(Text(Word(list22)))))
            )
          )
        )
      )
    )
    assertEquals(result, expected)
  }

  test("lists 2") {
    val list11 = "List11"
    val list12 = "List12"
    val list21 = "List21"
    val list22 = "List22"
    val list31 = "List31"
    val list32 = "List32"

    val result =
      parseString(
        s"""
        /**
          * Some text:
          * 1. $list11
          *  i. $list21
          *   I. $list31
          * - $list12
          *  I. $list22
          *   I. $list32
          */
         """
      )
    val expected = Option(
      Scaladoc(
        Paragraph(
          Text(Word("Some"), Word("text:")),
          ListBlock(
            "1.",
            ListItem(
              Text(Word(list11)),
              Some(
                ListBlock(
                  "i.",
                  ListItem(Text(Word(list21)), Some(ListBlock("I.", ListItem(Text(Word(list31))))))
                )
              )
            )
          ),
          ListBlock(
            "-",
            ListItem(
              Text(Word(list12)),
              Some(
                ListBlock(
                  "I.",
                  ListItem(Text(Word(list22)), Some(ListBlock("I.", ListItem(Text(Word(list32))))))
                )
              )
            )
          )
        )
      )
    )
    assertEquals(result, expected)
  }

  test("lists 3") {
    val list11 = "List11"
    val list12 = "List12"
    val list21 = "List21"

    val result =
      parseString(
        s"""
        /**
          * Some text:
          * - $list11
          * $list11
          *    - $list21
          * - $list12
          */
         """
      )
    val expected = Option(
      Scaladoc(
        Paragraph(
          Text(Word("Some"), Word("text:")),
          ListBlock(
            "-",
            ListItem(
              Text(Word(list11), Word(list11)),
              Some(ListBlock("-", ListItem(Text(Word(list21)))))
            ),
            ListItem(Text(Word(list12)))
          )
        )
      )
    )
    assertEquals(result, expected)
  }

  test("lists 4") {
    val list11 = "List11"
    val list12 = "List12"

    val result =
      parseString(
        s"""
        /**
          * @inheritdoc Some text:
          * 1. $list11
          * - $list12
          */
         """
      )
    val expected = Option(
      Scaladoc(
        Paragraph(
          Unknown(""" @inheritdoc Some text:
                    | 1. List11
                    | - List12""".stripMargin)
        )
      )
    )
    assertEquals(result, expected)
  }

  test("label parsing/merging") {
    val testStringToMerge = "Test DocText"
    val scaladoc: String = TagType.predefined
      .flatMap(token => List(generateTestString(token), testStringToMerge))
      .mkString("/** ", "\n * ", " */")
    val words = testStringToMerge.split("\\s+").map(Word.apply).toSeq

    val parsedScaladoc = parseString(scaladoc)

    val expectedCount = TagType.predefined.length + TagType.predefined.count(!_.hasDesc)
    assertEquals(parsedScaladoc.map(_.para.length), Option(1))
    assertEquals(parsedScaladoc.map(_.para.head.term.length), Option(expectedCount))

    // Inherit doc does not merge
    parsedScaladoc.foreach {
      _.para.head.term.foreach {
        case t: Tag if t.tag.hasDesc =>
          assertEquals(t.desc.parts.takeRight(words.length), words)
        case _ =>
      }
    }
  }

  test("parse tag") {
    assertEquals(
      parseString(
        s"""
         /**
          * @param foo
          * bar-baz
          */
         """
      ),
      Option(Scaladoc(Paragraph(Tag(TagType.Param, Word("foo"), Text(Word("bar-baz"))))))
    )
  }

  test("failing to parse 1") {
    assertEquals(
      parseString(
        """
         /**
          * @param
          */
         """
      ),
      Option(Scaladoc(Paragraph(Unknown(" @param"))))
    )
  }

  test("failing to parse 2") {
    assertEquals(
      parseString(
        """
         /**
          * @param
          * @return
          */
         """
      ),
      Option(Scaladoc(Paragraph(Unknown(" @param\n @return"))))
    )
  }

  test("using an unrecognized tag") {
    assertEquals(
      parseString(
        """
         /**
          * @newtag
          * newtag text
          */
         """
      ),
      Option(
        Scaladoc(
          Paragraph(Tag(TagType.UnknownTag("@newtag"), desc = Text(Word("newtag"), Word("text"))))
        )
      )
    )
  }

}
