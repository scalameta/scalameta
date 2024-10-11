package scala.meta.tests.parsers

import scala.meta.internal.Scaladoc
import scala.meta.internal.Scaladoc._
import scala.meta.internal.parsers.ScaladocParser

import munit.FunSuite

/**
 * Test for [[ScaladocParser]]
 */
class ScaladocParserSuite extends FunSuite {

  private def parseString(comment: String) = ScaladocParser.parse(comment.trim)

  private def generateTestString(tagType: TagType, testStringToMerge: String)(implicit
      sb: StringBuilder
  ): Unit = {
    val nl = "\n *"
    sb.append(' ').append(tagType.tag)
    if (tagType.optDesc) {
      if (tagType.hasLabel) sb.append(" TestLabel")
      // another, with description
      sb.append(nl).append(' ').append(tagType.tag)
      if (tagType.hasLabel) sb.append(" TestLabel")
      sb.append("  Test Description")
      sb.append(nl).append("  ").append(testStringToMerge)
    } else
    // label with spaces
    if (tagType.hasLabel) sb.append(" Test Label")
    sb.append(nl)
  }

  test("example usage") {
    assertEquals(
      parseString("/** Example scaladoc */"),
      Some(Scaladoc(Seq(Paragraph(Seq(Text(Seq(Word("Example"), Word("scaladoc"))))))))
    )
  }

  test("indentation checks") {

    val expectedBody: String = "BODY"
    val expectedBodyToken = Text(Seq(Word(expectedBody)))

    assertEquals(
      parseString(s"/** $expectedBody*/"),
      Some(Scaladoc(Seq(Paragraph(Seq(expectedBodyToken)))))
    )
    assertEquals(
      parseString(
        s"""
         /** $expectedBody
          */
         """
      ),
      Some(Scaladoc(Seq(Paragraph(Seq(expectedBodyToken)))))
    )
    assertEquals(
      parseString(
        s"""
         /**       $expectedBody
          */
         """
      ),
      Some(Scaladoc(Seq(Paragraph(Seq(expectedBodyToken)))))
    )
    assertEquals(
      parseString(
        s"""
         /**
          *$expectedBody
          */
         """
      ),
      Some(Scaladoc(Seq(Paragraph(Seq(expectedBodyToken)))))
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
      Option(Scaladoc(Seq(Paragraph(Seq(Text(words))), Paragraph(Seq(Text(words))))))
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
      Option(Scaladoc(Seq(Paragraph(Seq(Text(words))), Paragraph(Seq(Text(words))))))
    )
  }

  test("paragraph parsing with references") {
    val descriptionBody = "Description Body"
    val words = descriptionBody.split("\\s+").toSeq.map(Word.apply)
    val refNone = Seq(Link("Description", Seq("Body")))
    val refDots = Seq(Link("Description", Seq("Body"), "..."))
    val refWithSuffix = Seq(Link("Description", Seq("Body"), "'s"))
    assertEquals(
      parseString(
        s"""
         /**
          *
          * $descriptionBody [[ $descriptionBody ]]...
          *
          * $descriptionBody [[ $descriptionBody ]]
          * $descriptionBody
          *
          * $descriptionBody
          * [[ $descriptionBody ]] $descriptionBody
          * 
          * [[ $descriptionBody ]]'s
          *
          */
         """
      ),
      Option(Scaladoc(Seq(
        Paragraph(Seq(Text(words ++ refDots))),
        Paragraph(Seq(Text(words ++ refNone ++ words))),
        Paragraph(Seq(Text(words ++ refNone ++ words))),
        Paragraph(Seq(Text(refWithSuffix)))
      )))
    )
  }

  test("paragraph parsing with complex references and parens") {
    val ref = "baz qux"
    val link = new Link(ref.split("\\s+"), ")")
    assertEquals(link.syntax, "[[baz qux]])")

    val text1 = Seq(Word("(foo"), Word("bar"), link)
    val text2 = Seq(Word("foo"), Word("bar"), link)

    assertEquals(
      parseString(
        s"""
         /**
          * (foo bar
          * [[$ref]])
          *
          * foo bar
          * [[$ref]])
          */
         """
      ),
      Option(Scaladoc(Seq(Paragraph(Seq(Text(text1))), Paragraph(Seq(Text(text2))))))
    )
  }

  test("code blocks") {

    val testDescription = "This is a codeblock:"
    val words: Seq[Word] = testDescription.split("\\s+").map(Word.apply)

    val codeBlock1 = "{\"HELLO MARIANO\"}"
    val codeBlock2 = "\"HELLO SORAYA\""
    val complexCodeBlock = // keep all newlines and leading spaces
      """|  ggmqwogmwogmqwomgq
         |    val x = 1 // sdfdfh
         |   // zzz
         |   gmqwgoiqmgoqmwomw""".stripMargin.split("\n")
    val complexCodeBlockAsComment = complexCodeBlock.mkString("\n *")

    val result = parseString(
      s"""
          /**
            * $testDescription {{{$codeBlock1}}}?$testDescription
            * {{{ $codeBlock2 }}}
            *
            * $testDescription
            *
            * {{{
            *$complexCodeBlockAsComment
            *
            * foo
            * }}}
            * {{{
            *$complexCodeBlockAsComment }}}             */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(
      Paragraph(Seq(Text((words :+ CodeExpr(codeBlock1, "?")) ++ (words :+ CodeExpr(codeBlock2))))),
      Paragraph(Seq(Text(words))),
      Paragraph(Seq(CodeBlock(complexCodeBlock ++ Seq("", " foo")), CodeBlock(complexCodeBlock)))
    )))
    assertEquals(result, expectation)
  }

  test("code blocks multiline") {
    val complexCodeBlock = // keep all newlines and leading spaces
      """|  ggmqwogmwogmqwomgq
         |    val x = 1 // sdfdfh
         |   // zzz
         |   gmqwgoiqmgoqmwomw""".stripMargin.split("\n")
    val complexCodeBlockAsComment = complexCodeBlock.mkString("\n *")

    val result = parseString(
      s"""
          /**
            * {{{
            *$complexCodeBlockAsComment
            *
            * foo
            * }}}
            */
       """.stripMargin
    )

    val expectation =
      Option(Scaladoc(Seq(Paragraph(Seq(CodeBlock(complexCodeBlock ++ Seq("", " foo")))))))
    assertEquals(result, expectation)
  }

  test("code blocks inline") {
    val codeBlock1 = "{\"HELLO MARIANO\"}"
    val codeBlock2 = "\"HELLO SORAYA\""
    val result = parseString(
      s"""
          /**
            * {{{$codeBlock1}}}
            * {{{ $codeBlock2 }}}
            */
       """.stripMargin
    )

    val expectation =
      Option(Scaladoc(Seq(Paragraph(Seq(Text(Seq(CodeExpr(codeBlock1), CodeExpr(codeBlock2))))))))
    assertEquals(result, expectation)
  }

  test("markdown code blocks 1") {
    val result = parseString(
      s"""
          /**   ```
            *   ```
            *      ```
            *      ```
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(
      Seq(Paragraph(Seq(MdCodeBlock(Nil, Nil, "```"), Text(Seq(Word("```"), Word("```"))))))
    ))
    assertEquals(result, expectation)
  }

  test("markdown code blocks 2") {
    val result = parseString(
      s"""
          /**  ```scala
            *  ```
            *  ```foo bar baz
            *  ```
            *   ~~~bar baz
            *        foo
            *   ~~~~~~~~~~
            *   ``~
            *   foo
            *   ``~
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      MdCodeBlock(Seq("scala"), Nil, "```"),
      MdCodeBlock(Seq("foo", "bar", "baz"), Nil, "```"),
      MdCodeBlock(Seq("bar", "baz"), Seq("     foo"), "~~~"),
      Text(Seq(Word("``~"), Word("foo"), Word("``~")))
    )))))
    assertEquals(result, expectation)
  }

  test("markdown code blocks 3") {
    val codeBlock = // keep all newlines and leading spaces
      """|  object Foo {
         |    def bar(): String = "foobar"
         |  }
         |""".stripMargin.split("\n")
    val codeBlockAsComment = codeBlock.mkString("\n *  ")

    val result = parseString(
      s"""
          /**   foo
            *  ```bar qux
            *  $codeBlockAsComment
            *  ```
            *
            *  `````scala
            *  $codeBlockAsComment
            *  ```
            *  ````````
            *
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(
      Paragraph(Seq(Text(Seq(Word("foo"))), MdCodeBlock(Seq("bar", "qux"), codeBlock, fence = "```"))),
      Paragraph(Seq(MdCodeBlock(Seq("scala"), codeBlock :+ "```", fence = "`````")))
    )))
    assertEquals(result, expectation)
  }

  test("markdown code blocks - no leading spaces") {
    val result = parseString(
      s"""
          /**```scala
            *println(42)
            *```
            */
       """.stripMargin
    )

    val expectation =
      Option(Scaladoc(Seq(Paragraph(Seq(MdCodeBlock(Seq("scala"), Seq("println(42)"), "```"))))))
    assertEquals(result, expectation)
  }

  test("markdown code span 1") {
    val result = parseString(
      s"""
          /**   foo `` bar ` ` baz ``, and
            *   qux `quux xyzzy`
            */
       """.stripMargin
    )

    val expectation = Some(Scaladoc(Seq(Paragraph(Seq(Text(Seq(
      Word("foo"),
      MdCodeSpan(" bar ` ` baz ", "``", ","),
      Word("and"),
      Word("qux"),
      MdCodeSpan("quux xyzzy", "`", "")
    )))))))

    assertEquals(result, expectation)
  }

  test("pseudo-markdown code span within list") {
    val result = parseString(
      s"""
          /**   - foo `bar
            *   - baz qux`
            */
       """.stripMargin
    )

    val expectation = Some(Scaladoc(Seq(Paragraph(Seq(ListBlock(
      ListType.Bullet,
      Seq(
        ListItem("-", Text(Seq(Word("foo"), Word("`bar"))), Nil),
        ListItem("-", Text(Seq(Word("baz"), Word("qux`"))), Nil)
      )
    ))))))

    assertEquals(result, expectation)
  }

  test("markdown code span vs code block") {
    val result = parseString(
      s"""
          /**   ``` foo ` ` bar ```
            *   ```foo
            *   ```
            */
       """.stripMargin
    )

    val expectation = Some(Scaladoc(Seq(Paragraph(
      Seq(Text(Seq(MdCodeSpan(" foo ` ` bar ", "```", ""))), MdCodeBlock(Seq("foo"), Nil, "```"))
    ))))
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

    val result = parseString(
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
    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Heading(1, level1HeadingBody),
      Heading(2, level2HeadingBody),
      Heading(3, level3HeadingBody),
      Heading(4, level4HeadingBody),
      Heading(5, level5HeadingBody),
      Heading(6, level6HeadingBody),
      Text(Seq(Word(bad1))),
      Text(Seq(Word(bad2))),
      Text(Seq(Word(bad3), Word(bad4)))
    )))))
    assertEquals(result, expectation)
  }

  test("list - no leading spaces") {
    val list11 = "List11"
    val list12 = "List12"

    val result = parseString(
      s"""
        /**
          * Some text:
          *- $list11
          *- $list12
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      Text(Seq(Word("-"), Word(list11))),
      Text(Seq(Word("-"), Word(list12)))
    )))))
    assertEquals(result, expected)
  }

  test("lists 1") {
    val list11 = "List11"
    val list12 = "List12"
    val list21 = "List21"
    val list22 = "List22"

    val result = parseString(
      s"""
        /**
          * Some text:
          * - $list11
          *    1. $list21
          *    2. $list22
          *
          * - $list12
          *    a. $list21
          *    b. $list22
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Bullet,
        Seq(
          ListItem(
            "-",
            Text(Seq(Word(list11))),
            Seq(ListBlock(
              ListType.Decimal,
              Seq(ListItem("1.", Text(Seq(Word(list21)))), ListItem("2.", Text(Seq(Word(list22)))))
            ))
          ),
          ListItem(
            "-",
            Text(Seq(Word(list12))),
            Seq(ListBlock(
              ListType.Alpha,
              Seq(ListItem("a.", Text(Seq(Word(list21)))), ListItem("b.", Text(Seq(Word(list22)))))
            ))
          )
        )
      )
    )))))
    assertEquals(result, expected)
  }

  test("lists 2") {
    val list11 = "List11"
    val list12 = "List12"
    val list21 = "List21"
    val list22 = "List22"
    val list31 = "List31"
    val list32 = "List32"

    val result = parseString(
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
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Decimal,
        Seq(ListItem(
          "1.",
          Text(Seq(Word(list11))),
          Seq(ListBlock(
            ListType.Roman,
            Seq(ListItem(
              "i.",
              Text(Seq(Word(list21))),
              Seq(ListBlock(ListType.Roman, Seq(ListItem("I.", Text(Seq(Word(list31)))))))
            ))
          ))
        ))
      ),
      ListBlock(
        ListType.Bullet,
        Seq(ListItem(
          "-",
          Text(Seq(Word(list12))),
          Seq(ListBlock(
            ListType.Roman,
            Seq(ListItem(
              "I.",
              Text(Seq(Word(list22))),
              Seq(ListBlock(ListType.Roman, Seq(ListItem("I.", Text(Seq(Word(list32)))))))
            ))
          ))
        ))
      )
    )))))
    assertEquals(result, expected)
  }

  test("lists 2: #3145") {
    val list11 = "List11"
    val list12 = "List12"
    val list21 = "List21"
    val list22 = "List22"
    val list32 = "List32"

    val result = parseString(
      s"""
      /**
        * Some text:
        * 1. $list11
        *  i.e. foo
        *  i. $list21
        *  ii. $list22
        * - $list12
        *  i. $list21
        *  ix. $list22
        *  i.e. bar
        *   i. $list32
        *  baz
        */
       """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Decimal,
        Seq(ListItem(
          "1.",
          Text(Seq(Word(list11), Word("i.e."), Word("foo"))),
          Seq(ListBlock(
            ListType.Roman,
            Seq(ListItem("i.", Text(Seq(Word(list21)))), ListItem("ii.", Text(Seq(Word(list22)))))
          ))
        ))
      ),
      ListBlock(
        ListType.Bullet,
        Seq(ListItem(
          "-",
          Text(Seq(Word(list12))),
          Seq(ListBlock(
            ListType.Roman,
            Seq(
              ListItem("i.", Text(Seq(Word(list21)))),
              ListItem(
                "ix.",
                Text(Seq(Word(list22), Word("i.e."), Word("bar"))),
                Seq(
                  ListBlock(ListType.Roman, Seq(ListItem("i.", Text(Seq(Word(list32)))))),
                  Text(Seq(Word("baz")))
                )
              )
            )
          ))
        ))
      )
    )))))
    assertEquals(result, expected)
  }

  test("lists 3") {
    val list11 = "List11"
    val list12 = "List12"
    val list21 = "List21"

    val result = parseString(
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
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Bullet,
        Seq(
          ListItem(
            "-",
            Text(Seq(Word(list11), Word(list11))),
            Seq(ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word(list21)))))))
          ),
          ListItem("-", Text(Seq(Word(list12))))
        )
      )
    )))))
    assertEquals(result, expected)
  }

  test("lists 4") {
    val list11 = "List11"
    val list12 = "List12"

    val result = parseString(
      s"""
        /**
          * @inheritdoc Some text:
          * 1. $list11
          * - $list12
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Tag(TagType.InheritDoc),
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(ListType.Decimal, Seq(ListItem("1.", Text(Seq(Word(list11)))))),
      ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word(list12))))))
    )))))
    assertEquals(result, expected)
  }

  test("lists 5") {
    // looks like list but isn't
    val result = parseString(
      """
        /**
          * -5
          *
          * 1.0%
          */
         """
    )
    val expected = Option(
      Scaladoc(Seq(Paragraph(Seq(Text(Seq(Word("-5"))))), Paragraph(Seq(Text(Seq(Word("1.0%")))))))
    )
    assertEquals(result, expected)
  }

  test("lists 6") {
    val result = parseString(
      """
        /** 1. a
          * 1. b
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(ListBlock(
      ListType.Decimal,
      Seq(ListItem("1.", Text(Seq(Word("a")))), ListItem("1.", Text(Seq(Word("b")))))
    ))))))
    assertEquals(result, expected)
  }

  test("list 7, indented markdown code") {
    val list11 = "List11"
    val list12 = "List12"
    val list13 = "List13"
    val list21 = "List21"

    val result = parseString(
      s"""
        /**
          * Some text:
          * - $list11
          * - $list12
          * continue text
          *    ```scala
          *    println(42)
          *    ```
          * and some text
          *   - $list21
          * - $list13
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Bullet,
        Seq(
          ListItem("-", Text(Seq(Word(list11)))),
          ListItem(
            "-",
            Text(Seq(Word(list12), Word("continue"), Word("text"))),
            Seq(
              MdCodeBlock(Seq("scala"), Seq("println(42)"), "```"),
              Text(Seq(Word("and"), Word("some"), Word("text"))),
              ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word(list21))))))
            )
          ),
          ListItem("-", Text(Seq(Word(list13))))
        )
      )
    )))))
    assertEquals(result, expected)
  }

  test("list 7, over-indented markdown-like code") {
    val list11 = "List11"
    val list12 = "List12"
    val list13 = "List13"
    val list21 = "List21"

    val result = parseString(
      s"""
        /**
          * Some text:
          * - $list11
          * - $list12
          * continue text
          *       ```scala
          *    println(42)
          *       ```
          * and some text
          *   - $list21
          * - $list13
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Bullet,
        Seq(
          ListItem("-", Text(Seq(Word(list11)))),
          ListItem(
            "-",
            Text(
              Seq(Word(list12), Word("continue"), Word("text")) ++
                Seq(Word("```scala"), Word("println(42)"), Word("```")) ++
                Seq(Word("and"), Word("some"), Word("text"))
            ),
            Seq(ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word(list21)))))))
          ),
          ListItem("-", Text(Seq(Word(list13))))
        )
      )
    )))))
    assertEquals(result, expected)
  }

  test("list 7, non-indented markdown code") {
    val list11 = "List11"
    val list12 = "List12"
    val list13 = "List13"
    val list21 = "List21"

    val result = parseString(
      s"""
        /**
          * Some text:
          * - $list11
          * - $list12
          * continue text
          *```scala
          *println(42)
          *```
          * and some text
          *   - $list21
          * - $list13
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Bullet,
        Seq(
          ListItem("-", Text(Seq(Word(list11)))),
          ListItem("-", Text(Seq(Word(list12), Word("continue"), Word("text"))))
        )
      ),
      MdCodeBlock(Seq("scala"), Seq("println(42)"), "```"),
      Text(Seq(Word("and"), Word("some"), Word("text"))),
      ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word(list21)))))),
      ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word(list13))))))
    )))))
    assertEquals(result, expected)
  }

  test("list 7, indented code") {
    val list11 = "List11"
    val list12 = "List12"
    val list13 = "List13"
    val list21 = "List21"

    val result = parseString(
      s"""
        /**
          * Some text:
          * - $list11
          * - $list12
          * continue text
          *    {{{
          *     println(42)
          *    }}}
          * and some text
          *   - $list21
          * - $list13
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Bullet,
        Seq(
          ListItem("-", Text(Seq(Word(list11)))),
          ListItem(
            "-",
            Text(Seq(Word(list12), Word("continue"), Word("text"))),
            Seq(
              CodeBlock(Seq("     println(42)")),
              Text(Seq(Word("and"), Word("some"), Word("text"))),
              ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word(list21))))))
            )
          ),
          ListItem("-", Text(Seq(Word(list13))))
        )
      )
    )))))
    assertEquals(result, expected)
  }

  test("list 7, over-indented code") {
    val list11 = "List11"
    val list12 = "List12"
    val list13 = "List13"
    val list21 = "List21"

    val result = parseString(
      s"""
        /**
          * Some text:
          * - $list11
          * - $list12
          * continue text
          *      {{{
          *     println(42) }}}
          * and some text
          *   - $list21
          * - $list13
          *
          * next para
          */
         """
    )
    val expected = Option(Scaladoc(Seq(
      Paragraph(Seq(
        Text(Seq(Word("Some"), Word("text:"))),
        ListBlock(
          ListType.Bullet,
          Seq(
            ListItem("-", Text(Seq(Word(list11)))),
            ListItem(
              "-",
              Text(Seq(Word(list12), Word("continue"), Word("text"))),
              Seq(
                CodeBlock(Seq("     println(42)")),
                Text(Seq(Word("and"), Word("some"), Word("text"))),
                ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word(list21))))))
              )
            ),
            ListItem("-", Text(Seq(Word(list13))))
          )
        )
      )),
      Paragraph(Seq(Text(Seq(Word("next"), Word("para")))))
    )))
    assertEquals(result, expected)
  }

  test("list 7, non-indented code") {
    val list11 = "List11"
    val list12 = "List12"
    val list13 = "List13"
    val list21 = "List21"

    val result = parseString(
      s"""
        /**
          * Some text:
          * - $list11
          * - $list12
          * continue text
          *{{{
          * println(42)
          *}}}
          * and some text
          *   - $list21
          * - $list13
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Bullet,
        Seq(
          ListItem("-", Text(Seq(Word(list11)))),
          ListItem(
            "-",
            Text(Seq(Word(list12), Word("continue"), Word("text"))),
            Seq(
              CodeBlock(Seq(" println(42)")),
              Text(Seq(Word("and"), Word("some"), Word("text"))),
              ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word(list21))))))
            )
          ),
          ListItem("-", Text(Seq(Word(list13))))
        )
      )
    )))))
    assertEquals(result, expected)
  }

  test("list 8, indented table") {
    val list11 = "List11"
    val list12 = "List12"

    val result = parseString(
      s"""
        /**
          * Some text:
          * - $list11
          * - $list12
          *   |one|two|
          *   |---|---|
          *   |1  | 2 |
          * and some text
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Bullet,
        Seq(
          ListItem("-", Text(Seq(Word(list11)))),
          ListItem(
            "-",
            Text(Seq(Word(list12))),
            Seq(
              Table(
                Table.Row(Seq("one", "two")),
                Seq(Table.Left, Table.Left),
                Seq(Table.Row(Seq("1", "2")))
              ),
              Text(Seq(Word("and"), Word("some"), Word("text")))
            )
          )
        )
      )
    )))))
    assertEquals(result, expected)
  }

  test("list 8, over-indented table") {
    val list11 = "List11"
    val list12 = "List12"

    val result = parseString(
      s"""
        /**
          * Some text:
          * - $list11
          * - $list12
          *       |one|two|
          *       |---|---|
          *       |1  | 2 |
          * and some text
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Bullet,
        Seq(
          ListItem("-", Text(Seq(Word(list11)))),
          ListItem(
            "-",
            Text(Seq(Word(list12))),
            Seq(
              Table(
                Table.Row(Seq("one", "two")),
                Seq(Table.Left, Table.Left),
                Seq(Table.Row(Seq("1", "2")))
              ),
              Text(Seq(Word("and"), Word("some"), Word("text")))
            )
          )
        )
      )
    )))))
    assertEquals(result, expected)
  }

  test("list 8, non-indented table") {
    val list11 = "List11"
    val list12 = "List12"

    val result = parseString(
      s"""
        /**
          * Some text:
          * - $list11
          * - $list12
          * |one|two|
          * |---|---|
          * |1  | 2 |
          * and some text
          */
         """
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("Some"), Word("text:"))),
      ListBlock(
        ListType.Bullet,
        Seq(
          ListItem("-", Text(Seq(Word(list11)))),
          ListItem(
            "-",
            Text(Seq(Word(list12))),
            Seq(
              Table(
                Table.Row(Seq("one", "two")),
                Seq(Table.Left, Table.Left),
                Seq(Table.Row(Seq("1", "2")))
              ),
              Text(Seq(Word("and"), Word("some"), Word("text")))
            )
          )
        )
      )
    )))))
    assertEquals(result, expected)
  }

  test("list 9, complex embedded elements 1") {
    // table ends in a delim line
    val result = parseString(
      s"""
         |  /** 1. foo
         |   *      {{{
         |   *         embedded code
         |   *      }}}
         |   *      - foo1
         |   *  | hdr1   |  hdr22 |
         |   *   |:-------|-------:|
         |   *    | r1 1   |   r1 2 |
         |   *    +--------+--------+
         |   *        - foo2
         |   *   {{{
         |   *         embedded code
         |   *   }}}
         |   *           ```
         |   *         code foo2
         |   *           ```
         |   *        ```
         |   *         code foo1
         |   *        ```
         |   *      ```
         |   *         code foo
         |   *      ```
         |   * 1. bar
         |   */
         |""".stripMargin
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(ListBlock(
      ListType.Decimal,
      Seq(
        ListItem(
          "1.",
          Text(Seq(Word("foo"))),
          Seq(
            CodeBlock(Seq("         embedded code")),
            ListBlock(
              ListType.Bullet,
              Seq(ListItem(
                "-",
                Text(Seq(Word("foo1"))),
                Seq(
                  Table(
                    Table.Row(Seq("hdr1", "hdr22")),
                    Seq(Table.Left, Table.Right),
                    Seq(Table.Row(Seq("r1 1", "r1 2")))
                  ),
                  ListBlock(
                    ListType.Bullet,
                    Seq(ListItem(
                      "-",
                      Text(Seq(Word("foo2"))),
                      Seq(
                        CodeBlock(Seq("         embedded code")),
                        MdCodeBlock(Nil, Seq("code foo2"), "```")
                      )
                    ))
                  ),
                  MdCodeBlock(Nil, Seq(" code foo1"), "```")
                )
              ))
            ),
            MdCodeBlock(Nil, Seq("   code foo"), "```")
          )
        ),
        ListItem("1.", Text(Seq(Word("bar"))))
      )
    ))))))
    assertEquals(result, expected)
  }

  test("list 9, complex embedded elements 2") {
    // table doesn't end in a delim line, followed by "-" list
    val result = parseString(
      s"""
         |  /** 1. foo
         |   *      {{{
         |   *         embedded code
         |   *      }}}
         |   *      - foo1
         |   *  | hdr1   |  hdr22 |
         |   *   |:-------|-------:|
         |   *    | r1 1   |   r1 2 |
         |   *        - foo2
         |   *   {{{
         |   *         embedded code
         |   *   }}}
         |   *           ```
         |   *         code foo2
         |   *           ```
         |   *        ```
         |   *         code foo1
         |   *        ```
         |   *      ```
         |   *         code foo
         |   *      ```
         |   * 1. bar
         |   */
         |""".stripMargin
    )
    val expected = Option(Scaladoc(Seq(Paragraph(Seq(ListBlock(
      ListType.Decimal,
      Seq(
        ListItem(
          "1.",
          Text(Seq(Word("foo"))),
          Seq(
            CodeBlock(Seq("         embedded code")),
            ListBlock(
              ListType.Bullet,
              Seq(ListItem(
                "-",
                Text(Seq(Word("foo1"))),
                Seq(
                  Table(
                    Table.Row(Seq("hdr1", "hdr22")),
                    Seq(Table.Left, Table.Right),
                    Seq(Table.Row(Seq("r1 1", "r1 2")))
                  ),
                  ListBlock(
                    ListType.Bullet,
                    Seq(ListItem(
                      "-",
                      Text(Seq(Word("foo2"))),
                      Seq(
                        CodeBlock(Seq("         embedded code")),
                        MdCodeBlock(Nil, Seq("code foo2"), "```")
                      )
                    ))
                  ),
                  MdCodeBlock(Nil, Seq(" code foo1"), "```")
                )
              ))
            ),
            MdCodeBlock(Nil, Seq("   code foo"), "```")
          )
        ),
        ListItem("1.", Text(Seq(Word("bar"))))
      )
    ))))))
    assertEquals(result, expected)
  }

  test("label parsing/merging") {
    val testStringToMerge = "Test DocText"
    implicit val sb = new StringBuilder
    sb.append("/** ")
    TagType.predefined.foreach(generateTestString(_, testStringToMerge))
    sb.append('/')
    val words = testStringToMerge.split("\\s+").map(Word.apply).toSeq

    val parsedScaladocOpt = parseString(sb.result())
    assertNotEquals(parsedScaladocOpt, None: Option[Scaladoc])
    val parsedScaladocParas = parsedScaladocOpt.get.para
    assertEquals(parsedScaladocParas.length, 1)
    val parsedScaladocTerms = parsedScaladocParas.head.terms
    val parsedTags = parsedScaladocTerms.collect { case t: Tag => t }

    val tagsWithDesc = TagType.predefined.count(_.optDesc)
    assertEquals(parsedTags.count(t => t.tag.optDesc && t.desc.isEmpty), tagsWithDesc)
    assertEquals(parsedTags.count(t => t.tag.optDesc && t.desc.nonEmpty), tagsWithDesc)
    assertEquals(
      parsedTags.count(t => !t.tag.optDesc && t.desc.isEmpty),
      TagType.predefined.length - tagsWithDesc
    )
    assertEquals(parsedTags.count(t => !t.tag.optDesc && t.desc.nonEmpty), 0)

    // Inherit doc does not merge
    parsedTags.foreach { // check label
      case Tag(t, label, _) if t.hasLabel =>
        val expected = if (t.optDesc) "TestLabel" else "Test Label"
        assertEquals(label, Some(Word(expected)))
      case _ =>
    }
    parsedTags.foreach { // check desc
      case Tag(t, _, desc) if !t.optDesc => assertEquals(desc, Nil)
      case Tag(_, _, desc) if desc.nonEmpty =>
        val actual = desc.map {
          case x: Text => x.parts.takeRight(words.length)
          case x => x
        }
        assertEquals(actual, Seq(words))
      case _ =>
    }
  }

  test("tags valid then invalid") {
    val result = parseString(
      """
          /** @param foo - bar baz
            * @return
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Tag(TagType.Param, Some(Word("foo")), Seq(Text(Seq(Word("-"), Word("bar"), Word("baz"))))),
      Tag(TagType.Return)
    )))))
    assertEquals(result, expectation)
  }

  test("known tag looks like list") {
    val result = parseString(
      """
          /** @param foo
            *   - bar baz
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(Tag(
      TagType.Param,
      Some(Word("foo")),
      Seq(ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word("bar"), Word("baz")))))))
    ))))))
    assertEquals(result, expectation)
  }

  test("unknown tag looks like list") {
    val result = parseString(
      """
          /** @foo
            *   - bar baz
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(Tag(
      TagType.UnknownTag("@foo"),
      desc = Seq(ListBlock(ListType.Bullet, Seq(ListItem("-", Text(Seq(Word("bar"), Word("baz")))))))
    ))))))
    assertEquals(result, expectation)
  }

  test("parse throws tag") {
    assertEquals(
      parseString(
        s"""
         /**
          * @throws e1 foo bar
          * @throws e2 baz qux
          * bar-baz
          */
         """
      ),
      Option(Scaladoc(Seq(Paragraph(Seq(
        Tag(TagType.Throws, Some(Word("e1")), Seq(Text(Seq(Word("foo"), Word("bar"))))),
        Tag(TagType.Throws, Some(Word("e2")), Seq(Text(Seq(Word("baz"), Word("qux"), Word("bar-baz")))))
      )))))
    )
  }

  test("parse since, version tags") {
    assertEquals(
      parseString(
        s"""
         /**
          * @version 1.0 foo bar
          * @since 1.0 baz qux
          * bar-baz
          */
         """
      ),
      Option(Scaladoc(Seq(Paragraph(Seq(
        Tag(TagType.Version, Some(Word("1.0 foo bar"))),
        Tag(TagType.Since, Some(Word("1.0")), Seq(Text(Seq(Word("baz"), Word("qux"), Word("bar-baz")))))
      )))))
    )
  }

  test("parse define macro tags") {
    assertEquals(
      parseString(
        s"""
         /**
          * @define what for what reason
          * bar-baz
          */
         """
      ),
      Option(Scaladoc(Seq(Paragraph(Seq(Tag(
        TagType.Define,
        Some(Word("what")),
        Seq(Text(Seq(Word("for"), Word("what"), Word("reason"), Word("bar-baz"))))
      ))))))
    )
  }

  test("parse usecase tags") {
    assertEquals(
      parseString(
        s"""
         /**
          * @usecase foo bar
          * baz qux
          */
         """
      ),
      Option {
        val label = Some(Word("foo bar"))
        val rest = Seq(Text(Seq(Word("baz"), Word("qux"))))
        Scaladoc(Seq(Paragraph(Tag(TagType.UseCase, label = label) +: rest)))
      }
    )
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
      Option(Scaladoc(
        Seq(Paragraph(Seq(Tag(TagType.Param, Some(Word("foo")), Seq(Text(Seq(Word("bar-baz"))))))))
      ))
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
      Option(Scaladoc(Seq(Paragraph(Seq(Text(Seq(Word("@param"))))))))
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
      Option(Scaladoc(Seq(Paragraph(Seq(Text(Seq(Word("@param"))), Tag(TagType.Return))))))
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
      Option(Scaladoc(Seq(Paragraph(
        Seq(Tag(TagType.UnknownTag("@newtag"), desc = Seq(Text(Seq(Word("newtag"), Word("text"))))))
      ))))
    )
  }

  test("tag description with multiline code blocks") {
    val complexCodeBlock = // keep all newlines and leading spaces
      """|  ggmqwogmwogmqwomgq
         |    val x = 1 // sdfdfh
         |   // zzz
         |   gmqwgoiqmgoqmwomw""".stripMargin.split("\n")
    val complexCodeBlockAsComment = complexCodeBlock.mkString("\n *")

    val result = parseString(
      s"""
          /** blah
            * @example
            * {{{
            *$complexCodeBlockAsComment
            * }}}
            * bar baz
            * {{{
            *$complexCodeBlockAsComment }}}
            * baz qux
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("blah"))),
      Tag(
        TagType.Example,
        desc = Seq(
          CodeBlock(complexCodeBlock),
          Text(Seq(Word("bar"), Word("baz"))),
          CodeBlock(complexCodeBlock),
          Text(Seq(Word("baz"), Word("qux")))
        )
      )
    )))))
    assertEquals(result, expectation)
  }

  test("tag description with markdown code blocks") {
    val complexCodeBlock = // keep all newlines and leading spaces
      """|  ggmqwogmwogmqwomgq
         |    val x = 1 // sdfdfh
         |   // zzz
         |   gmqwgoiqmgoqmwomw""".stripMargin.split("\n")
    val complexCodeBlockAsComment = complexCodeBlock.mkString(" ", "\n * ", "")

    val result = parseString(
      s"""
          /** blah
            * @example
            * ```info1 info2
            *$complexCodeBlockAsComment
            * ```
            * bar baz
            * ```info3 info4
            *$complexCodeBlockAsComment
            * ```
            * baz qux
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("blah"))),
      Tag(
        TagType.Example,
        desc = Seq(
          MdCodeBlock(Seq("info1", "info2"), complexCodeBlock, "```"),
          Text(Seq(Word("bar"), Word("baz"))),
          MdCodeBlock(Seq("info3", "info4"), complexCodeBlock, "```"),
          Text(Seq(Word("baz"), Word("qux")))
        )
      )
    )))))
    assertEquals(result, expectation)
  }

  test("enclosed java tag") {
    val javaTag1 = EnclosedJavaTag("@tag1")
    val javaTag2 = EnclosedJavaTag("@tag2", List("with", "desc"))
    assertEquals(
      parseString(
        """
       /**
        * {@tag1}
        * {@tag2 with desc}
        * {@not a
        * tag}
        */
       """
      ),
      Option(Scaladoc(
        Paragraph(Text(Seq(javaTag1, javaTag2, Word("{@not"), Word("a"), Word("tag}"))) :: Nil) ::
          Nil
      ))
    )
    assertEquals(javaTag1.syntax, "{@tag1}")
    assertEquals(javaTag2.syntax, "{@tag2 with desc}")
  }

  test("table escaped pipe") {
    val result = parseString(
      """
          /**
            * text1 text2
            * |hdr1|hdr2|hdr3|h\|4|
            * |----|:---|---:|:--:|
            * |row1|row2|row3|row4|
            * text3 text4
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("text1"), Word("text2"))),
      Table(
        Table.Row(Seq("hdr1", "hdr2", "hdr3", "h\\|4")),
        Seq(Table.Left, Table.Left, Table.Right, Table.Center),
        Seq(Table.Row(Seq("row1", "row2", "row3", "row4")))
      ),
      Text(Seq(Word("text3"), Word("text4")))
    )))))
    assertEquals(result, expectation)
  }

  test("table missing trailing pipe") {
    val result = parseString(
      """
          /**
            * text1 text2
            * |hdr1|hdr2|hdr3|hdr4|
            * |----|:---|---:|:--:|
            * |row1|row2|row3|row4
            * text3 text4
            */
       """.stripMargin
    )

    assertEquals(result, None)
  }

  test("table different number of cols") {
    val result = parseString(
      """
          /**
            * text1 text2
            * |hdr1|hdr2|hdr3|hdr4|
            * |----|:---|---:|:--:|---:|:--:|
            * |row1|row2|row3|row4|row5|
            * text3 text4
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("text1"), Word("text2"))),
      Table(
        Table.Row(Seq("hdr1", "hdr2", "hdr3", "hdr4")),
        Seq(Table.Left, Table.Left, Table.Right, Table.Center, Table.Right, Table.Center),
        Seq(Table.Row(Seq("row1", "row2", "row3", "row4", "row5")))
      ),
      Text(Seq(Word("text3"), Word("text4")))
    )))))
    assertEquals(result, expectation)
  }

  test("table missing alignment row") {
    val result = parseString(
      """
          /**
            * text1 text2
            * | hdr1 | hdr2  |  hdr3 |  hdr4   |
            * |row1|row2|row3|row4|
            * text3 text4
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("text1"), Word("text2"))),
      Table(
        Table.Row(Seq("hdr1", "hdr2", "hdr3", "hdr4")),
        Seq(Table.Left, Table.Left, Table.Right, Table.Center),
        Seq(Table.Row(Seq("row1", "row2", "row3", "row4")))
      ),
      Text(Seq(Word("text3"), Word("text4")))
    )))))
    assertEquals(result, expectation)
  }

  test("table missing alignment row with +- delim line") {
    val result = parseString(
      """
          /**
            * text1 text2
            * +------+-------+-------+---------+
            * | hdr1 | hdr2  |  hdr3 |  hdr4   |
            * +------+-------+-------+---------+
            * |row1  |row2   |row3   |row4     |
            * +------+-------+-------+---------+
            * text3 text4
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("text1"), Word("text2"))),
      Table(
        Table.Row(Seq("hdr1", "hdr2", "hdr3", "hdr4")),
        Seq(Table.Left, Table.Left, Table.Right, Table.Center),
        Seq(Table.Row(Seq("row1", "row2", "row3", "row4")))
      ),
      Text(Seq(Word("text3"), Word("text4")))
    )))))
    assertEquals(result, expectation)
  }

  test("table duplicate alignment row") {
    val result = parseString(
      """
          /**
            * text1 text2
            * |hdr1|hdr2|hdr3|hdr4|
            * |----|:---|---:|:--:|
            * |----|:---|---:|:--:|
            * |row1|row2|row3|row4|
            * text3 text4
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("text1"), Word("text2"))),
      Table(
        Table.Row(Seq("hdr1", "hdr2", "hdr3", "hdr4")),
        Seq(Table.Left, Table.Left, Table.Right, Table.Center),
        Seq(
          Table.Row(Seq("----", ":---", "---:", ":--:")),
          Table.Row(Seq("row1", "row2", "row3", "row4"))
        )
      ),
      Text(Seq(Word("text3"), Word("text4")))
    )))))
    assertEquals(result, expectation)
  }

  test("table empty single column") {
    val result = parseString(
      """
          /**
            * text1 text2
            * ||
            * |-|
            * ||
            * text3 text4
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("text1"), Word("text2"))),
      Table(Table.Row(Seq("")), Seq(Table.Left), Seq(Table.Row(Seq("")))),
      Text(Seq(Word("text3"), Word("text4")))
    )))))
    assertEquals(result, expectation)
  }

  test("table empty 'align' row") {
    val result = parseString(
      """
          /**
            * text1 text2
            * |a|
            * ||
            * text3 text4
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("text1"), Word("text2"))),
      Table(Table.Row(Seq("a")), Seq(Table.Left), Seq(Table.Row(Seq("")))),
      Text(Seq(Word("text3"), Word("text4")))
    )))))
    assertEquals(result, expectation)
  }

  test("table only header row") {
    val result = parseString(
      """
          /**

            *

            * text1 text2
            * |hdr1  |hdr2   |hdr3|  hdr4||
            * text3 text4
            *
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Text(Seq(Word("text1"), Word("text2"))),
      Text(Seq(
        Word("|hdr1"),
        Word("|hdr2"),
        Word("|hdr3|"),
        Word("hdr4||"),
        Word("text3"),
        Word("text4")
      ))
    )))))
    assertEquals(result, expectation)
  }

  test("table - no leading spaces") {
    val result = parseString(
      """
          /**
            *|one|two|
            *|---|---|
            *|1  |  2|
            */
       """.stripMargin
    )

    val expectation = Option(Scaladoc(Seq(Paragraph(Seq(
      Table(Table.Row(Seq("one", "two")), Seq(Table.Left, Table.Left), Seq(Table.Row(Seq("1", "2"))))
    )))))
    assertEquals(result, expectation)
  }

  test("table alignment") {
    assertEquals(Table.Left.syntax(0), ":-")
    assertEquals(Table.Right.syntax(0), "-:")
    assertEquals(Table.Center.syntax(0), "::")

    assertEquals(Table.Left.syntax(1), ":--")
    assertEquals(Table.Right.syntax(1), "--:")
    assertEquals(Table.Center.syntax(1), ":-:")

    assertEquals(Table.Left.leftPad(7), 0)
    assertEquals(Table.Right.leftPad(7), 7)
    assertEquals(Table.Center.leftPad(7), 3)
  }

}
