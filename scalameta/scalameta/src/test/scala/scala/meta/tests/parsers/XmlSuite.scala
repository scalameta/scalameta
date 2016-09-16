package scala.meta.tests.parsers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.tests.DiffAssertions

class XmlSuite extends ParseSuite with DiffAssertions {
  def tokenize(code: String): Tokens = {
    val convert = scala.meta.inputs.Input.stringToInput
    val tokenize = scala.meta.tokenizers.Tokenize.scalametaTokenize
    val dialect = Scala211
    code.tokenize(convert, tokenize, dialect).get
  }

  def tokensStructure(tokenized: Tokens): String = {
    tokenized
      .map(x => f"${x.structure}%22s ----> ${x.getClass}")
      .mkString("\n")
  }

  test("simple xml literal - 1") {
    assert(
      tokensStructure(tokenize("<foo>bar</foo>")) ===
        """|
           |            BOF [0..0) ----> class scala.meta.tokens.Token$BOF
           |                [0..0) ----> class scala.meta.tokens.Token$Xml$Start
           |<foo>bar</foo> [0..14) ----> class scala.meta.tokens.Token$Xml$Part
           |              [14..14) ----> class scala.meta.tokens.Token$Xml$End
           |          EOF [14..14) ----> class scala.meta.tokens.Token$EOF
           |""".stripMargin.trim.replace("QQQ", "\"\"\""))
  }

  test("simple xml literal - 2") {
    assert(
      tokensStructure(tokenize("<foo>bar</foo> ")) ===
        """|
           |            BOF [0..0) ----> class scala.meta.tokens.Token$BOF
           |                [0..0) ----> class scala.meta.tokens.Token$Xml$Start
           |<foo>bar</foo> [0..14) ----> class scala.meta.tokens.Token$Xml$Part
           |              [14..14) ----> class scala.meta.tokens.Token$Xml$End
           |              [14..15) ----> class scala.meta.tokens.Token$Space
           |          EOF [15..15) ----> class scala.meta.tokens.Token$EOF
           |""".stripMargin.trim.replace("QQQ", "\"\"\""))
  }

  // TODO(olafur) currently crashes, see https://github.com/scalameta/scalameta/pull/469/files/e2324548b3896d5e7b0993f4063316602d9583ff#r78293500
  ignore("curlies inside scala expr") {
    assert(
      tokensStructure(tokenize("""<foo>{"{" + `{`}</foo>""")) ===
          """|
             |            BOF [0..0) ----> class scala.meta.tokens.Token$BOF
             |                [0..0) ----> class scala.meta.tokens.Token$Xml$Start
             |<foo>bar</foo> [0..14) ----> class scala.meta.tokens.Token$Xml$Part
             |              [14..14) ----> class scala.meta.tokens.Token$Xml$End
             |          EOF [14..14) ----> class scala.meta.tokens.Token$EOF
             |""".stripMargin.trim.replace("QQQ", "\"\"\""))
  }

  test("tricky xml") {
    import scala.meta._
    val input =
      """|{
         |val x = <div href={"/" + url}>Hello {name}</div>;
         |val noSemicolon = <h1>{msg infix upper}</h1>
         |val y = 2
         |}""".stripMargin
    val tokenized = input.tokenize.get
    val x = term(input)
    assert(
      tokensStructure(tokenized) ===
        """|
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
           |              [19..19) ----> class scala.meta.tokens.Token$Xml$SpliceStart
           |            { [20..21) ----> class scala.meta.tokens.Token$LeftBrace
           |          "/" [21..24) ----> class scala.meta.tokens.Token$Constant$String
           |              [24..25) ----> class scala.meta.tokens.Token$Space
           |            + [25..26) ----> class scala.meta.tokens.Token$Ident
           |              [26..27) ----> class scala.meta.tokens.Token$Space
           |          url [27..30) ----> class scala.meta.tokens.Token$Ident
           |            } [30..31) ----> class scala.meta.tokens.Token$RightBrace
           |              [31..31) ----> class scala.meta.tokens.Token$Xml$SpliceEnd
           |      >Hello  [31..38) ----> class scala.meta.tokens.Token$Xml$Part
           |              [37..37) ----> class scala.meta.tokens.Token$Xml$SpliceStart
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
           |              [73..73) ----> class scala.meta.tokens.Token$Xml$SpliceStart
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
        """.stripMargin)

    // format: off
    val test @ Term.Block(
      Seq(
        Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("x"))), None,
          Term.Xml(Seq(Lit("<div href="), Lit(">Hello "), Lit("</div>")),
            Seq(Term.ApplyInfix(Lit("/"), Term.Name("+"), Nil, Seq(Term.Name("url"))), Term.Name("name")))),
        Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("noSemicolon"))), None,
          Term.Xml(Seq(Lit("<h1>"), Lit("</h1>")),
            Seq(Term.ApplyInfix(Term.Name("msg"), Term.Name("infix"), Nil, Seq(Term.Name("upper")))))),
        Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("y"))), None, Lit(2)))) = x
    // format: on

  }

}
