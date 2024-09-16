package scala.meta.tests.parsers

import org.scalameta.logger
import scala.meta._

class XmlSuite extends ParseSuite {

  implicit val dialect: Dialect = dialects.Scala211

  def skip(original: String, expected: String): Unit =
    test(logger.revealWhitespace(original).ignore) {}

  def checkToken(original: String, expected: String): Unit = {
    def tokensStructure(tokenized: Tokens): String = tokenized.map(_.structure)
      .mkString("", "\n", "\n")

    test(logger.revealWhitespace(original)) {
      val obtained = tokensStructure(tokenize(original))
      assertEquals(obtained, expected)
    }
  }

  /** Default catch only ParseException */
  override def checkError(stat: String)(implicit dialect: Dialect): Unit =
    test(logger.revealWhitespace(stat).take(50)) {
      val thrown = intercept[Exception](templStat(stat))
      assert(thrown.isInstanceOf[ParseException] || thrown.isInstanceOf[TokenizeException])
    }

  checkToken(
    "<foo>bar</foo>",
    """|BOF [0..0)
       |Xml.Start [0..0)
       |Xml.Part(<foo>bar</foo>) [0..14)
       |Xml.End [14..14)
       |EOF [14..14)
       |""".stripMargin
  )
  checkToken(
    "<foo>{bar}</foo> ",
    """|BOF [0..0)
       |Xml.Start [0..0)
       |Xml.Part(<foo>) [0..5)
       |Xml.SpliceStart [5..5)
       |LeftBrace [5..6)
       |Ident(bar) [6..9)
       |RightBrace [9..10)
       |Xml.SpliceEnd [10..10)
       |Xml.Part(</foo>) [10..16)
       |Xml.End [16..16)
       |Space [16..17)
       |EOF [17..17)
       |""".stripMargin
  )
  checkToken(
    "<b>{1}{2}</b>",
    """|BOF [0..0)
       |Xml.Start [0..0)
       |Xml.Part(<b>) [0..3)
       |Xml.SpliceStart [3..3)
       |LeftBrace [3..4)
       |Constant.Int(1) [4..5)
       |RightBrace [5..6)
       |Xml.SpliceEnd [6..6)
       |Xml.Part() [6..6)
       |Xml.SpliceStart [6..6)
       |LeftBrace [6..7)
       |Constant.Int(2) [7..8)
       |RightBrace [8..9)
       |Xml.SpliceEnd [9..9)
       |Xml.Part(</b>) [9..13)
       |Xml.End [13..13)
       |EOF [13..13)
       |""".stripMargin
  )
  checkToken(
    """<foo>{"{" + `{`}</foo>""",
    """|BOF [0..0)
       |Xml.Start [0..0)
       |Xml.Part(<foo>) [0..5)
       |Xml.SpliceStart [5..5)
       |LeftBrace [5..6)
       |Constant.String({) [6..9)
       |Space [9..10)
       |Ident(+) [10..11)
       |Space [11..12)
       |Ident({) [12..15)
       |RightBrace [15..16)
       |Xml.SpliceEnd [16..16)
       |Xml.Part(</foo>) [16..22)
       |Xml.End [22..22)
       |EOF [22..22)
       |""".stripMargin
  )
  checkToken(
    "<foo/>{1}",
    """|BOF [0..0)
       |Xml.Start [0..0)
       |Xml.Part(<foo/>) [0..6)
       |Xml.End [6..6)
       |LeftBrace [6..7)
       |Constant.Int(1) [7..8)
       |RightBrace [8..9)
       |EOF [9..9)
       |""".stripMargin
  )

  private val trickyXml = """|{
                             |val x = <div href={"/" + url}>Hello {name}</div>;
                             |val noSemicolon = <h1>{msg infix upper}</h1>
                             |val y = 2
                             |}""".stripMargin

  checkToken(
    trickyXml,
    """|BOF [0..0)
       |LeftBrace [0..1)
       |LF [1..2)
       |KwVal [2..5)
       |Space [5..6)
       |Ident(x) [6..7)
       |Space [7..8)
       |Equals [8..9)
       |Space [9..10)
       |Xml.Start [10..10)
       |Xml.Part(<div href=) [10..20)
       |Xml.SpliceStart [20..20)
       |LeftBrace [20..21)
       |Constant.String(/) [21..24)
       |Space [24..25)
       |Ident(+) [25..26)
       |Space [26..27)
       |Ident(url) [27..30)
       |RightBrace [30..31)
       |Xml.SpliceEnd [31..31)
       |Xml.Part(>Hello ) [31..38)
       |Xml.SpliceStart [38..38)
       |LeftBrace [38..39)
       |Ident(name) [39..43)
       |RightBrace [43..44)
       |Xml.SpliceEnd [44..44)
       |Xml.Part(</div>) [44..50)
       |Xml.End [50..50)
       |Semicolon [50..51)
       |LF [51..52)
       |KwVal [52..55)
       |Space [55..56)
       |Ident(noSemicolon) [56..67)
       |Space [67..68)
       |Equals [68..69)
       |Space [69..70)
       |Xml.Start [70..70)
       |Xml.Part(<h1>) [70..74)
       |Xml.SpliceStart [74..74)
       |LeftBrace [74..75)
       |Ident(msg) [75..78)
       |Space [78..79)
       |Ident(infix) [79..84)
       |Space [84..85)
       |Ident(upper) [85..90)
       |RightBrace [90..91)
       |Xml.SpliceEnd [91..91)
       |Xml.Part(</h1>) [91..96)
       |Xml.End [96..96)
       |LF [96..97)
       |KwVal [97..100)
       |Space [100..101)
       |Ident(y) [101..102)
       |Space [102..103)
       |Equals [103..104)
       |Space [104..105)
       |Constant.Int(2) [105..106)
       |LF [106..107)
       |RightBrace [107..108)
       |EOF [108..108)
       |""".stripMargin
  )

  test("deconstruct") {
    assertTree(term(trickyXml)) {
      blk(
        Defn.Val(
          Nil,
          List(Pat.Var(tname("x"))),
          None,
          Term.Xml(
            List(str("<div href="), str(">Hello "), str("</div>")),
            List(
              blk(Term.ApplyInfix(str("/"), tname("+"), Nil, List(tname("url")))),
              blk(tname("name"))
            )
          )
        ),
        Defn.Val(
          Nil,
          List(Pat.Var(tname("noSemicolon"))),
          None,
          Term.Xml(
            List(str("<h1>"), str("</h1>")),
            blk(Term.ApplyInfix(tname("msg"), tname("infix"), Nil, List(tname("upper")))) :: Nil
          )
        ),
        Defn.Val(Nil, List(Pat.Var(tname("y"))), None, int(2))
      )
    }
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
    """|
       |<a>
       |  <b/>
       |</a>
       |""".stripMargin
  )
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
    """|
       |val ips = <ips>{
       |  for {
       |    field <-ipsList
       |    JString(ip) <- field.value
       |  } yield <ip>{ ip }</ip>
       |}</ips>
       |""".stripMargin
  )

  // Matches bugs in scalac
  checkOK("""<a b="&#;"/>""")
  checkOK("""<a b="&#x;"/>""")
  checkOK("<a>&#;</a>")
  checkOK("<a>&#x;</a>")
  checkOK("<a>]]></a>")
  checkOK("<a/>{0}")
  // checkOK("""<a b="&:;"/>""") // FIXME
  // checkOK("""<a b="&:a;"/>""") //FIXME
  // checkOK("""<a b="&a:;"/>""") // FIXME
  // checkOK("e match { case <a>&</a> => () }") // FIXME

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
  // checkError("<a></b>") // FIXME: Should not parse

  test("SeqWildcard with trailing NL") {
    val code = """|def foo =
                  |  e match {
                  |    case <title>{
                  |      _*
                  |    }</title> =>
                  |  }
                  |""".stripMargin
    val layout = """|def foo = e match {
                    |  case <title>{_*}</title> =>
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      None,
      Term.Match(
        tname("e"),
        List(
          Case(Pat.Xml(List(lit("<title>"), lit("</title>")), List(Pat.SeqWildcard())), None, blk())
        ),
        Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // FIXME These should not parse: we need to differentiate between expression and pattern position
  // checkError("""e match { case <a b="foo"/> => () }""" )
  // checkError("e match { case <xml:unparsed><</xml:unparsed> => () }")
  // checkError("e match { case <!-- comment --> => () }" )
  // checkError("e match { case <![CDATA[foo]]> => () }" )
  // checkError("e match { case <?foo bar?> => () }" )

}
