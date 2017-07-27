package scala.meta.tests.parsers

import scala.meta._
import scala.meta.dialects.Scala211

import org.scalameta.logger

class XmlSuite extends ParseSuite {

  def skip(original: String, expected: String): Unit = ignore(logger.revealWhitespace(original)) {}

  def checkToken(original: String, expected: String): Unit = {
    def tokensStructure(tokenized: Tokens): String = {
      tokenized
        .map(x => f"${x.structure}%22s ----> ${x.getClass}")
        .mkString("\n")
    }

    def tokenize(code: String): Tokens = {
      val convert = scala.meta.inputs.stringToInput
      val tokenize = scala.meta.tokenizers.Tokenize.scalametaTokenize
      val dialect = Scala211
      code.tokenize(convert, tokenize, dialect).get
    }

    test(logger.revealWhitespace(original)) {
      val obtained = tokensStructure(tokenize(original))
      assert(obtained.trim == expected.trim)
    }
  }

  /** Default catch only ParseException */
  override def checkError(stat: String)(implicit dialect: Dialect): Unit =
    test(logger.revealWhitespace(stat).take(50)) {
      val thrown = intercept[Exception] {
        templStat(stat)(dialect)
      }
      assert(thrown.isInstanceOf[ParseException] || thrown.isInstanceOf[TokenizeException])
    }

  checkToken(
    "<foo>bar</foo>",
    """
      |            BOF [0..0) ----> class scala.meta.tokens.Token$BOF
      |                [0..0) ----> class scala.meta.tokens.Token$Xml$Start
      |<foo>bar</foo> [0..14) ----> class scala.meta.tokens.Token$Xml$Part
      |              [14..14) ----> class scala.meta.tokens.Token$Xml$End
      |          EOF [14..14) ----> class scala.meta.tokens.Token$EOF
    """.stripMargin
  )
  checkToken(
    "<foo>{bar}</foo> ",
    """
      |            BOF [0..0) ----> class scala.meta.tokens.Token$BOF
      |                [0..0) ----> class scala.meta.tokens.Token$Xml$Start
      |          <foo> [0..5) ----> class scala.meta.tokens.Token$Xml$Part
      |                [5..5) ----> class scala.meta.tokens.Token$Xml$SpliceStart
      |              { [5..6) ----> class scala.meta.tokens.Token$LeftBrace
      |            bar [6..9) ----> class scala.meta.tokens.Token$Ident
      |             } [9..10) ----> class scala.meta.tokens.Token$RightBrace
      |              [10..10) ----> class scala.meta.tokens.Token$Xml$SpliceEnd
      |       </foo> [10..16) ----> class scala.meta.tokens.Token$Xml$Part
      |              [16..16) ----> class scala.meta.tokens.Token$Xml$End
      |              [16..17) ----> class scala.meta.tokens.Token$Space
      |          EOF [17..17) ----> class scala.meta.tokens.Token$EOF
    """.stripMargin
  )
  checkToken(
    "<b>{1}{2}</b>",
    """
      |            BOF [0..0) ----> class scala.meta.tokens.Token$BOF
      |                [0..0) ----> class scala.meta.tokens.Token$Xml$Start
      |            <b> [0..3) ----> class scala.meta.tokens.Token$Xml$Part
      |                [3..3) ----> class scala.meta.tokens.Token$Xml$SpliceStart
      |              { [3..4) ----> class scala.meta.tokens.Token$LeftBrace
      |              1 [4..5) ----> class scala.meta.tokens.Token$Constant$Int
      |              } [5..6) ----> class scala.meta.tokens.Token$RightBrace
      |                [6..6) ----> class scala.meta.tokens.Token$Xml$SpliceEnd
      |                [6..6) ----> class scala.meta.tokens.Token$Xml$Part
      |                [6..6) ----> class scala.meta.tokens.Token$Xml$SpliceStart
      |              { [6..7) ----> class scala.meta.tokens.Token$LeftBrace
      |              2 [7..8) ----> class scala.meta.tokens.Token$Constant$Int
      |              } [8..9) ----> class scala.meta.tokens.Token$RightBrace
      |                [9..9) ----> class scala.meta.tokens.Token$Xml$SpliceEnd
      |          </b> [9..13) ----> class scala.meta.tokens.Token$Xml$Part
      |              [13..13) ----> class scala.meta.tokens.Token$Xml$End
      |          EOF [13..13) ----> class scala.meta.tokens.Token$EOF
    """.stripMargin
  )
  checkToken(
    """<foo>{"{" + `{`}</foo>""",
    """
      |            BOF [0..0) ----> class scala.meta.tokens.Token$BOF
      |                [0..0) ----> class scala.meta.tokens.Token$Xml$Start
      |          <foo> [0..5) ----> class scala.meta.tokens.Token$Xml$Part
      |                [5..5) ----> class scala.meta.tokens.Token$Xml$SpliceStart
      |              { [5..6) ----> class scala.meta.tokens.Token$LeftBrace
      |            "{" [6..9) ----> class scala.meta.tokens.Token$Constant$String
      |               [9..10) ----> class scala.meta.tokens.Token$Space
      |            + [10..11) ----> class scala.meta.tokens.Token$Ident
      |              [11..12) ----> class scala.meta.tokens.Token$Space
      |          `{` [12..15) ----> class scala.meta.tokens.Token$Ident
      |            } [15..16) ----> class scala.meta.tokens.Token$RightBrace
      |              [16..16) ----> class scala.meta.tokens.Token$Xml$SpliceEnd
      |       </foo> [16..22) ----> class scala.meta.tokens.Token$Xml$Part
      |              [22..22) ----> class scala.meta.tokens.Token$Xml$End
      |          EOF [22..22) ----> class scala.meta.tokens.Token$EOF
    """.stripMargin
  )
  checkToken(
    "<foo/>{1}",
    """
      |            BOF [0..0) ----> class scala.meta.tokens.Token$BOF
      |                [0..0) ----> class scala.meta.tokens.Token$Xml$Start
      |         <foo/> [0..6) ----> class scala.meta.tokens.Token$Xml$Part
      |                [6..6) ----> class scala.meta.tokens.Token$Xml$End
      |              { [6..7) ----> class scala.meta.tokens.Token$LeftBrace
      |              1 [7..8) ----> class scala.meta.tokens.Token$Constant$Int
      |              } [8..9) ----> class scala.meta.tokens.Token$RightBrace
      |            EOF [9..9) ----> class scala.meta.tokens.Token$EOF
    """.stripMargin
  )

  private val trickyXml =
    """
      |{
      |val x = <div href={"/" + url}>Hello {name}</div>;
      |val noSemicolon = <h1>{msg infix upper}</h1>
      |val y = 2
      |}
    """.trim.stripMargin

  checkToken(
    trickyXml,
    """
      |            BOF [0..0) ----> class scala.meta.tokens.Token$BOF
      |              { [0..1) ----> class scala.meta.tokens.Token$LeftBrace
      |             \n [1..2) ----> class scala.meta.tokens.Token$LF
      |            val [2..5) ----> class scala.meta.tokens.Token$KwVal
      |                [5..6) ----> class scala.meta.tokens.Token$Space
      |              x [6..7) ----> class scala.meta.tokens.Token$Ident
      |                [7..8) ----> class scala.meta.tokens.Token$Space
      |              = [8..9) ----> class scala.meta.tokens.Token$Equals
      |               [9..10) ----> class scala.meta.tokens.Token$Space
      |              [10..10) ----> class scala.meta.tokens.Token$Xml$Start
      |   <div href= [10..20) ----> class scala.meta.tokens.Token$Xml$Part
      |              [20..20) ----> class scala.meta.tokens.Token$Xml$SpliceStart
      |            { [20..21) ----> class scala.meta.tokens.Token$LeftBrace
      |          "/" [21..24) ----> class scala.meta.tokens.Token$Constant$String
      |              [24..25) ----> class scala.meta.tokens.Token$Space
      |            + [25..26) ----> class scala.meta.tokens.Token$Ident
      |              [26..27) ----> class scala.meta.tokens.Token$Space
      |          url [27..30) ----> class scala.meta.tokens.Token$Ident
      |            } [30..31) ----> class scala.meta.tokens.Token$RightBrace
      |              [31..31) ----> class scala.meta.tokens.Token$Xml$SpliceEnd
      |      >Hello  [31..38) ----> class scala.meta.tokens.Token$Xml$Part
      |              [38..38) ----> class scala.meta.tokens.Token$Xml$SpliceStart
      |            { [38..39) ----> class scala.meta.tokens.Token$LeftBrace
      |         name [39..43) ----> class scala.meta.tokens.Token$Ident
      |            } [43..44) ----> class scala.meta.tokens.Token$RightBrace
      |              [44..44) ----> class scala.meta.tokens.Token$Xml$SpliceEnd
      |       </div> [44..50) ----> class scala.meta.tokens.Token$Xml$Part
      |              [50..50) ----> class scala.meta.tokens.Token$Xml$End
      |            ; [50..51) ----> class scala.meta.tokens.Token$Semicolon
      |           \n [51..52) ----> class scala.meta.tokens.Token$LF
      |          val [52..55) ----> class scala.meta.tokens.Token$KwVal
      |              [55..56) ----> class scala.meta.tokens.Token$Space
      |  noSemicolon [56..67) ----> class scala.meta.tokens.Token$Ident
      |              [67..68) ----> class scala.meta.tokens.Token$Space
      |            = [68..69) ----> class scala.meta.tokens.Token$Equals
      |              [69..70) ----> class scala.meta.tokens.Token$Space
      |              [70..70) ----> class scala.meta.tokens.Token$Xml$Start
      |         <h1> [70..74) ----> class scala.meta.tokens.Token$Xml$Part
      |              [74..74) ----> class scala.meta.tokens.Token$Xml$SpliceStart
      |            { [74..75) ----> class scala.meta.tokens.Token$LeftBrace
      |          msg [75..78) ----> class scala.meta.tokens.Token$Ident
      |              [78..79) ----> class scala.meta.tokens.Token$Space
      |        infix [79..84) ----> class scala.meta.tokens.Token$Ident
      |              [84..85) ----> class scala.meta.tokens.Token$Space
      |        upper [85..90) ----> class scala.meta.tokens.Token$Ident
      |            } [90..91) ----> class scala.meta.tokens.Token$RightBrace
      |              [91..91) ----> class scala.meta.tokens.Token$Xml$SpliceEnd
      |        </h1> [91..96) ----> class scala.meta.tokens.Token$Xml$Part
      |              [96..96) ----> class scala.meta.tokens.Token$Xml$End
      |           \n [96..97) ----> class scala.meta.tokens.Token$LF
      |         val [97..100) ----> class scala.meta.tokens.Token$KwVal
      |            [100..101) ----> class scala.meta.tokens.Token$Space
      |          y [101..102) ----> class scala.meta.tokens.Token$Ident
      |            [102..103) ----> class scala.meta.tokens.Token$Space
      |          = [103..104) ----> class scala.meta.tokens.Token$Equals
      |            [104..105) ----> class scala.meta.tokens.Token$Space
      |          2 [105..106) ----> class scala.meta.tokens.Token$Constant$Int
      |         \n [106..107) ----> class scala.meta.tokens.Token$LF
      |          } [107..108) ----> class scala.meta.tokens.Token$RightBrace
      |        EOF [108..108) ----> class scala.meta.tokens.Token$EOF
    """.stripMargin
  )

  test("deconstruct") {
    val parsedTricky = term(trickyXml)
    // format: off
    val Term.Block(
    List(
    Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None,
    Term.Xml(List(Lit("<div href="), Lit(">Hello "), Lit("</div>")),
    List(Term.ApplyInfix(Lit("/"), Term.Name("+"), Nil, List(Term.Name("url"))), Term.Name("name")))),
    Defn.Val(Nil, List(Pat.Var(Term.Name("noSemicolon"))), None,
    Term.Xml(List(Lit("<h1>"), Lit("</h1>")),
    List(Term.ApplyInfix(Term.Name("msg"), Term.Name("infix"), Nil, List(Term.Name("upper")))))),
    Defn.Val(Nil, List(Pat.Var(Term.Name("y"))), None, Lit(2)))) = parsedTricky
    // format: on
  }

  checkOK("<foo/>")
  checkOK("<foo></foo>")
  checkOK("<foo/><bar/>")
  checkOK("<foo:bar/>")
  checkOK("<foo><bar/></foo>")
  checkOK("<foo\n>\t <bar   />\t  </foo\n>")
  checkOK("""<foo a="a" b="b"/>""")
  checkOK("""<foo a:a="a" b:b="b"/>""")
  checkOK("""<foo a="'"/>""")
  checkOK("""<foo a='"'/>""")
  checkOK("<foo>&name;</foo>")
  checkOK("<foo>&na:me;</foo>")
  checkOK("<foo>&lt;</foo>")
  checkOK("<foo>Hello &name;!</foo>")
  checkOK("""<foo a="&name;" />""")
  checkOK("""<foo a="&na:me;" />""")
  checkOK("""<foo a="&lt;" />""")
  checkOK("""<foo a="Hello &name;!"/>""")
  checkOK("""<foo a="1 &lt; 2"/>""")
  checkOK("<foo>&#1234;</foo>")
  checkOK("<foo>&#x1234;</foo>")
  checkOK("<foo>Hello&#x1234;Allan</foo>")
  checkOK("""<foo a="&#1234;" />""")
  checkOK("""<foo a="&#x1234;" />""")
  checkOK("""<foo a="Hello&#x1234;Allan" />""")
  checkOK("<xml:group><foo/><bar/></xml:group>")
  checkOK("<xml:group></xml:group>")
  checkOK("<!---->")
  checkOK("<!----->")
  checkOK("<!--foo-->")
  checkOK("<!--a-b-->")
  checkOK("<!-- -- -->") // fails at runtime
  checkOK("<!------>") // fails at runtime
  checkOK("<![CDATA[foo]]>")
  checkOK("<![CDATA[]]>")
  checkOK("<![CDATA[>]]>")
  checkOK("<![CDATA[]>]]>")
  checkOK("<![CDATA[]]]]>")
  checkOK("<?foo bar?>")
  checkOK("<?foo  bar?>")
  checkOK("<?foo?>")
  checkOK("<?foo     ?>")
  checkOK("<?foo??>")
  checkOK("<?foo<bar?>")
  checkOK("<?xml bar?>") // fails at runtime
  checkOK("<xml:unparsed>foo</xml:unparsed>")
  checkOK("<xml:unparsed>{</xml:unparsed>")
  checkOK("<xml:unparsed><</xml:unparsed>")
  checkOK("<![CDATA[hello, world]]>")
  checkOK("<![CDATA[hello, world]]><![CDATA[hello, world]]>")
  checkOK("<foo>x<![CDATA[hello, world]]></foo>")
  checkOK("<foo><![CDATA[hello, world]]></foo>")
  checkOK("<foo><![CDATA[hello, world]]><![CDATA[hello, world]]></foo>")
  checkOK("<a><b/>start<![CDATA[hi & bye]]><c/>world<d/>stuff<![CDATA[red & black]]></a>")
  checkOK("<foo>{foo}</foo>")
  checkOK("<foo>{foo }</foo>")
  checkOK("<foo>{ foo}</foo>")
  checkOK("<foo>{ foo }</foo>")
  checkOK("<foo>{2}</foo>")
  checkOK("""<foo>{"bar"}</foo>""")
  checkOK("<foo>{1}</foo>")
  checkOK("<foo>{<bar/>}</foo>")
  checkOK("<foo>{<bar/><bat/>}</foo>")
  checkOK("""<foo a={"foo"}/>""")
  checkOK("<foo a={<bar/>}/>")
  checkOK("<foo a={<bar/><bat/>}/>")
  checkOK("<a>{ List(1, 2) }</a>")
  checkOK("<a>{}</a>")
  checkOK("<a>{1}{2}</a>")
  checkOK("<a>{1}{2}<b/>{3}</a>")
  checkOK("<a>{<b>{1}{2}</b>}</a>")
  checkOK("e match { case <a>{_*}</a> => }")
  checkOK("<a>{{</a>")
  checkOK("<a>}}</a>")
  checkOK(
    """
      |<a>
      |  <b/>
      |</a>
    """.trim.stripMargin)
  checkOK(
    """|object a {
       |  <tr>
       |    <td> {session} </td>
       |    <td> <a href={sessionLink}> {session} </a> </td>
       |    <td> {formatDate(session)} </td>
       |    <td> {foo} </td>
       |  </tr>
       |}""".stripMargin
  )
  checkOK("<a>{<b>{1}</b>}</a>")
  checkOK(
    """
      |val ips = <ips>{
      |  for {
      |    field <-ipsList
      |    JString(ip) <- field.value
      |  } yield <ip>{ ip }</ip>
      |}</ips>
    """.trim.stripMargin)

  // Matches bugs in scalac
  checkOK("""<a b="&#;"/>""")
  checkOK("""<a b="&#x;"/>""")
  checkOK("<a>&#;</a>")
  checkOK("<a>&#x;</a>")
  checkOK("<a>]]></a>")
  checkOK("<a/>{0}")
  //checkOK("""<a b="&:;"/>""") // FIXME
  //checkOK("""<a b="&:a;"/>""") //FIXME
  //checkOK("""<a b="&a:;"/>""") // FIXME
  //checkOK("e match { case <a>&</a> => () }") // FIXME

  checkError("<a:/>")
  checkError("""<a b:="Hello"/>""")
  checkError("<a>&b:;</a>")
  checkError("<a>&:b;</a>")
  checkError("<a><a:/>")
  checkError("<a>")
  checkError("<!--->")
  checkError("<!-- >")
  checkError("<!-- ->")
  checkError("<![CDATA[]]>]]>")
  checkError("<a></ a>")
  checkError("<a></\na>")
  checkError("e match { case <a>{}</a> => ??? }")
  checkError("<a>}</a>")
  checkError("<a>{</a>")
  checkError("<a>}{</a>")
  //checkError("<a></b>") // FIXME: Should not parse

  // FIXME These should not parse: we need to differentiate between expression and pattern position
  //checkError("""e match { case <a b="foo"/> => () }""" )
  //checkError("e match { case <xml:unparsed><</xml:unparsed> => () }")
  //checkError("e match { case <!-- comment --> => () }" )
  //checkError("e match { case <![CDATA[foo]]> => () }" )
  //checkError("e match { case <?foo bar?> => () }" )

}
