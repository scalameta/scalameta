package scala.meta.internal.tokenizers

import scala.meta.Dialect
import scala.meta.inputs.Input

import scala.annotation.{switch, tailrec}

import fastparse.NoWhitespace._
import fastparse._

/**
 * Copy-pasta from this lihaoyi comment:
 * [[https://github.com/scalameta/fastparse/pull/1#issuecomment-244940542]] and adapted to more
 * closely match scala-xml and then adapted to fastparse 2.3.1
 */
class XmlParser(dialect: Dialect) {

  val blockParser = new ScalaExprPositionParser(dialect)

  def splicePositions: List[blockParser.XmlTokenRange] = blockParser.splicePositions
  def Patterns[A: P]: P0 = Fail

  def S[A: P]: P0 = P(CharsWhileIn("\t\n\r "))
  def XmlExpr[A: P]: P0 = P(Xml.XmlContent.rep(min = 1, sep = S.?))
  def XmlPattern[A: P]: P0 = P(Xml.ElemPattern)

  private[this] object Xml {
    def Element[A: P] = P(TagHeader ~/ ("/>" | ">" ~/ Content ~/ ETag)) // FIXME tag must be balanced
    def TagHeader[A: P] = P("<" ~ Name ~/ (S ~ Attribute).rep ~ S.?)
    def ETag[A: P] = P("</" ~ Name ~ S.? ~ ">")

    def Attribute[A: P] = P(Name ~/ Eq ~/ AttValue)
    def Eq[A: P] = P(S.? ~ "=" ~ S.?)
    def AttValue[A: P] =
      P("\"" ~/ (CharQ | Reference).rep ~ "\"" | "'" ~/ (CharA | Reference).rep ~ "'" | ScalaExpr)

    def Content[A: P] = P((CharData | Reference | ScalaExpr | XmlContent).rep)
    def XmlContent[A: P]: P0 = P(Unparsed | CDSect | PI | Comment | Element)

    def ScalaExpr[A: P] = P("{" ~ blockParser.blockRun(implicitly[P[_]]) ~ "}")

    def Unparsed[A: P] = P(UnpStart ~/ UnpData ~ UnpEnd)
    def UnpStart[A: P] = P("<xml:unparsed" ~/ (S ~ Attribute).rep ~ S.? ~ ">")
    def UnpEnd[A: P] = P("</xml:unparsed>")
    def UnpData[A: P] = P((!UnpEnd ~ Char).rep)

    def CDSect[A: P] = P(CDStart ~/ CData ~ CDEnd)
    def CDStart[A: P] = P("<![CDATA[")
    def CData[A: P] = P((!"]]>" ~ Char).rep)
    def CDEnd[A: P] = P("]]>")

    def Comment[A: P] = P("<!--" ~/ ComText ~ "-->")
    def ComText[A: P] = P((!"-->" ~ Char).rep)

    def PI[A: P] = P("<?" ~ Name ~ S.? ~ PIProcText ~ "?>")
    def PIProcText[A: P] = P((!"?>" ~ Char).rep)

    def Reference[A: P] = P(EntityRef | CharRef)
    def EntityRef[A: P] = P("&" ~ Name ~/ ";")
    def CharRef[A: P] = P("&#" ~ Num ~ ";" | "&#x" ~ HexNum ~ ";")
    def Num[A: P] = P(CharIn("0-9").rep)
    def HexNum[A: P] = P(CharIn("0-9", "a-f", "A-F").rep)

    def CharData[A: P] = P((CharB | "{{" | "}}").rep(1))

    def Char[A: P] = P(AnyChar)
    def Char1[A: P] = P(!("<" | "&") ~ Char)
    def CharQ[A: P] = P(!"\"" ~ Char1)
    def CharA[A: P] = P(!"'" ~ Char1)
    def CharB[A: P] = P(!("{" | "}") ~ Char1)

    // discard result
    def Name[A: P]: P0 = P(NameStart ~ NameChar.rep).!.filter(_.last != ':').opaque("Name").map(_ => ())
    def NameStart[A: P] = P(CharPred(isNameStart))
    def NameChar[A: P] = P(CharPred(isNameChar))

    def ElemPattern[A: P]: P0 = P(TagPHeader ~/ ("/>" | ">" ~/ ContentP ~/ ETag))
    def TagPHeader[A: P] = P("<" ~ Name ~ S.?)

    def ContentP[A: P]: P0 = P((CharDataP | ScalaPatterns | ElemPattern).rep)
    def ScalaPatterns[A: P] = P("{" ~ Patterns ~ "}")
    // matches weirdness of scalac parser on xml reference.
    def CharDataP[A: P] = P("&" ~ CharData.? | CharData)

    // ======================================================
    // From `scala.xml.parsing.TokenTests`
    // ======================================================

    /**
     * {{{
     *  NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
     *             | CombiningChar | Extender
     * }}}
     * See [4] and Appendix B of XML 1.0 specification.
     */
    def isNameChar(ch: Char) = {
      import java.lang.Character._
      // The constants represent groups Mc, Me, Mn, Lm, and Nd.

      isNameStart(ch) ||
      (getType(ch).toByte match {
        case COMBINING_SPACING_MARK | ENCLOSING_MARK | NON_SPACING_MARK | MODIFIER_LETTER |
            DECIMAL_DIGIT_NUMBER => true
        case _ => ".-:".contains(ch)
      })
    }

    /**
     * {{{
     *  NameStart ::= ( Letter | '_' )
     * }}}
     * where Letter means in one of the Unicode general categories {{{Ll, Lu, Lo, Lt, Nl}}}.
     *
     * We do not allow a name to start with `:`. See [3] and Appendix B of XML 1.0 specification
     */
    def isNameStart(ch: Char) = {
      import java.lang.Character._

      getType(ch).toByte match {
        case LOWERCASE_LETTER | UPPERCASE_LETTER | OTHER_LETTER | TITLECASE_LETTER |
            LETTER_NUMBER => true
        case _ => ch == '_'
      }
    }
  }
}

/**
 * Collects start and end positions of scala expressions inside xml literals.
 *
 * Doesn't really parse scala expressions, only reads until the curly brace balance hits 0.
 */
class ScalaExprPositionParser(dialect: Dialect) {
  case class XmlTokenRange(from: Int, to: Int) // from is inclusive, to is exclusive
  private val _splicePositions = List.newBuilder[XmlTokenRange]
  def splicePositions: List[XmlTokenRange] = _splicePositions.result()

  def blockRun = { implicit ctx: ParsingRun[_] =>
    val index = ctx.index
    val input = Input.String(ctx.input.slice(index, ctx.input.length))
    val scanner = new LegacyScanner(input, dialect)
    scanner.initialize()

    def getNextIndex(ltd: LegacyTokenData) = index + ltd.offset

    @tailrec
    def rec(curlyBraceCount: Int): ParsingRun[Unit] = {
      val ltd = scanner.nextToken()
      (ltd.token: @switch) match {
        case LegacyToken.RBRACE if curlyBraceCount == 0 =>
          val nextIndex = getNextIndex(ltd)
          _splicePositions += XmlTokenRange(index, nextIndex)
          ctx.freshSuccessUnit(index = nextIndex)
        case LegacyToken.EOF => ctx.freshFailure(getNextIndex(ltd))
        case LegacyToken.LBRACE => rec(curlyBraceCount + 1)
        case LegacyToken.RBRACE => rec(curlyBraceCount - 1)
        case _ => rec(curlyBraceCount)
      }
    }
    rec(0)
  }
}
