package scala.meta.internal.tokenizers

/**
  * Copy-pasta from this lihaoyi comment:
  * https://github.com/scalameta/fastparse/pull/1#issuecomment-244940542*/
import scala.annotation.switch
import scala.meta.Dialect
import scala.meta.inputs.Input

import fastparse.all._

class XmlParser(Block: P0,
                Patterns: P0 = Fail,
                WL: P0 = CharIn("\n ").rep(),
                WS: P0 = " ".rep(),
                HexNum: P0 = Fail) {
  // format: off
  val BaseChar = P(CharIn(
    '\u0041' to '\u005A', '\u0061' to '\u007A', '\u00C0' to '\u00D6', '\u00D8' to '\u00F6',
    '\u00F8' to '\u00FF', '\u0100' to '\u0131', '\u0134' to '\u013E', '\u0141' to '\u0148',
    '\u014A' to '\u017E', '\u0180' to '\u01C3', '\u01CD' to '\u01F0', '\u01F4' to '\u01F5',
    '\u01FA' to '\u0217', '\u0250' to '\u02A8', '\u02BB' to '\u02C1',  "\u0386",
    '\u0388' to '\u038A', "\u038C", '\u038E' to '\u03A1', '\u03A3' to '\u03CE',
    '\u03D0' to '\u03D6', "\u03DA", "\u03DC",  "\u03DE",  "\u03E0", '\u03E2' to '\u03F3',
    '\u0401' to '\u040C', '\u040E' to '\u044F', '\u0451' to '\u045C', '\u045E' to '\u0481',
    '\u0490' to '\u04C4', '\u04C7' to '\u04C8', '\u04CB' to '\u04CC', '\u04D0' to '\u04EB',
    '\u04EE' to '\u04F5', '\u04F8' to '\u04F9', '\u0531' to '\u0556',  "\u0559",
    '\u0561' to '\u0586', '\u05D0' to '\u05EA', '\u05F0' to '\u05F2', '\u0621' to '\u063A',
    '\u0641' to '\u064A', '\u0671' to '\u06B7', '\u06BA' to '\u06BE', '\u06C0' to '\u06CE',
    '\u06D0' to '\u06D3',  "\u06D5", '\u06E5' to '\u06E6', '\u0905' to '\u0939',  "\u093D",
    '\u0958' to '\u0961', '\u0985' to '\u098C', '\u098F' to '\u0990', '\u0993' to '\u09A8',
    '\u09AA' to '\u09B0',  "\u09B2", '\u09B6' to '\u09B9', '\u09DC' to '\u09DD',
    '\u09DF' to '\u09E1', '\u09F0' to '\u09F1', '\u0A05' to '\u0A0A', '\u0A0F' to '\u0A10',
    '\u0A13' to '\u0A28', '\u0A2A' to '\u0A30', '\u0A32' to '\u0A33', '\u0A35' to '\u0A36',
    '\u0A38' to '\u0A39', '\u0A59' to '\u0A5C',  "\u0A5E", '\u0A72' to '\u0A74',
    '\u0A85' to '\u0A8B',  "\u0A8D", '\u0A8F' to '\u0A91', '\u0A93' to '\u0AA8',
    '\u0AAA' to '\u0AB0', '\u0AB2' to '\u0AB3', '\u0AB5' to '\u0AB9',  "\u0ABD",  "\u0AE0",
    '\u0B05' to '\u0B0C', '\u0B0F' to '\u0B10', '\u0B13' to '\u0B28', '\u0B2A' to '\u0B30',
    '\u0B32' to '\u0B33', '\u0B36' to '\u0B39',  "\u0B3D", '\u0B5C' to '\u0B5D',
    '\u0B5F' to '\u0B61', '\u0B85' to '\u0B8A', '\u0B8E' to '\u0B90', '\u0B92' to '\u0B95',
    '\u0B99' to '\u0B9A',  "\u0B9C", '\u0B9E' to '\u0B9F', '\u0BA3' to '\u0BA4',
    '\u0BA8' to '\u0BAA', '\u0BAE' to '\u0BB5', '\u0BB7' to '\u0BB9', '\u0C05' to '\u0C0C',
    '\u0C0E' to '\u0C10', '\u0C12' to '\u0C28', '\u0C2A' to '\u0C33', '\u0C35' to '\u0C39',
    '\u0C60' to '\u0C61', '\u0C85' to '\u0C8C', '\u0C8E' to '\u0C90', '\u0C92' to '\u0CA8',
    '\u0CAA' to '\u0CB3', '\u0CB5' to '\u0CB9',  "\u0CDE", '\u0CE0' to '\u0CE1',
    '\u0D05' to '\u0D0C', '\u0D0E' to '\u0D10', '\u0D12' to '\u0D28', '\u0D2A' to '\u0D39',
    '\u0D60' to '\u0D61', '\u0E01' to '\u0E2E',  "\u0E30", '\u0E32' to '\u0E33',
    '\u0E40' to '\u0E45', '\u0E81' to '\u0E82',  "\u0E84", '\u0E87' to '\u0E88',  "\u0E8A",
    "\u0E8D", '\u0E94' to '\u0E97', '\u0E99' to '\u0E9F', '\u0EA1' to '\u0EA3',  "\u0EA5",
    "\u0EA7", '\u0EAA' to '\u0EAB', '\u0EAD' to '\u0EAE',  "\u0EB0", '\u0EB2' to '\u0EB3',
    "\u0EBD", '\u0EC0' to '\u0EC4', '\u0F40' to '\u0F47', '\u0F49' to '\u0F69',
    '\u10A0' to '\u10C5', '\u10D0' to '\u10F6',  "\u1100", '\u1102' to '\u1103',
    '\u1105' to '\u1107',  "\u1109", '\u110B' to '\u110C', '\u110E' to '\u1112',
    "\u113C",  "\u113E",  "\u1140",  "\u114C",  "\u114E",  "\u1150", '\u1154' to '\u1155',
    "\u1159", '\u115F' to '\u1161',  "\u1163",  "\u1165",  "\u1167",  "\u1169",
    '\u116D' to '\u116E', '\u1172' to '\u1173',  "\u1175",  "\u119E",  "\u11A8",  "\u11AB",
    '\u11AE' to '\u11AF', '\u11B7' to '\u11B8',  "\u11BA", '\u11BC' to '\u11C2',  "\u11EB",
    "\u11F0",  "\u11F9", '\u1E00' to '\u1E9B', '\u1EA0' to '\u1EF9', '\u1F00' to '\u1F15',
    '\u1F18' to '\u1F1D', '\u1F20' to '\u1F45', '\u1F48' to '\u1F4D', '\u1F50' to '\u1F57',
    "\u1F59",  "\u1F5B",  "\u1F5D", '\u1F5F' to '\u1F7D', '\u1F80' to '\u1FB4',
    '\u1FB6' to '\u1FBC',  "\u1FBE", '\u1FC2' to '\u1FC4', '\u1FC6' to '\u1FCC',
    '\u1FD0' to '\u1FD3', '\u1FD6' to '\u1FDB', '\u1FE0' to '\u1FEC', '\u1FF2' to '\u1FF4',
    '\u1FF6' to '\u1FFC',  "\u2126", '\u212A' to '\u212B',  "\u212E", '\u2180' to '\u2182',
    '\u3041' to '\u3094', '\u30A1' to '\u30FA', '\u3105' to '\u312C', '\uAC00' to '\uD7A3'
  ))
  val NameStartChar = P(CharIn(
    ":", 'A' to 'Z', "_", 'a' to 'z', '\u00C0' to '\u00D6', '\u00D8' to '\u00F6',
    '\u00F8' to '\u02FF', '\u0370' to '\u037D', '\u037F' to '\u1FFF', '\u200C' to '\u200D',
    '\u2070' to '\u218F', '\u2C00' to '\u2FEF', '\u3001' to '\uD7FF', '\uF900' to '\uFDCF',
    '\uFDF0' to '\uFFFD' // ++ [#x10000-#xEFFFF] ???? don't chars max out at \uffff ????
  ))
  val NameChar = P( NameStartChar | CharIn(
    "-", ".", '0' to '9', "\u00B7", '\u0300' to '\u036F', '\u203F' to '\u2040'
  ))
  val Ideographic = P( CharIn(
    '\u4E00' to '\u9FA5',  "\u3007", '\u3021' to '\u3029'
  ))
  // format: on
  val Eq = P(WL.? ~ "=" ~ WL.?)
  val ScalaExpr = P("{" ~ WS ~ Block ~ WL ~ "}")
  val XNameStart = P("_" | BaseChar | Ideographic)
  val Name = P(XNameStart ~ NameChar.rep)

  val Char = P(AnyChar)
  val Char1 = P(!("<" | "&") ~ Char)
  val CharQ = P(!"\"" ~ Char1)
  val CharA = P(!"'" ~ Char1)
  val CharB = P(!"{" ~ Char1)

  val Comment = P("<!--" ~ ((!"-" ~ Char) | ("-" ~ (!"-" ~ Char))).rep ~ "-->")

  val EntityRef = P("&" ~ Name ~ ";")
  val CharRef = P("&#" ~ CharIn('0' to '9').rep(1) ~ ";" | "&#x" ~ HexNum ~ ";")
  val Reference = P(EntityRef | CharRef)
  val AttValue = P(
    "\"" ~ (CharQ | Reference).rep ~ "\"" |
      "'" ~ (CharA | Reference).rep ~ "'" |
      ScalaExpr
  )
  val Attribute = P(Name ~ Eq ~ AttValue)
  val TagHeader = P("<" ~ Name ~ (WL ~ Attribute).rep ~ WL.?)
  val CharData = P((!("{" | "]]>" | CharRef) ~ Char1 | "{{").rep(1))
  val EmptyElemTagEnd = P("/>")
  val STagEnd = P(">")

  val CDStart = P("<![CDATA[")
  val CData = P((!"]]>" ~ Char).rep)
  val CDEnd = P("]]>")
  val CDSect = P(CDStart ~ CData ~ CDEnd)

  val PITarget = P(!(("X" | "x") ~ ("M" | "m") ~ ("L" | "l")) ~ Name)
  val PI = P("<?" ~ PITarget ~ (WL ~ (!"?>" ~ Char).rep).? ~ "?>")

  val ETagP = P("</" ~ Name ~ WL.? ~ ">")

  val XmlContent: P0 = P(Element | CDSect | PI | Comment)
  val Content1 = P(XmlContent | Reference | ScalaExpr)
  val Content = P((CharData | Content1).rep)
  val Element = P(TagHeader ~ (EmptyElemTagEnd | STagEnd ~ Content ~ ETag))

  val ETag = P("</" ~ Name ~ WL.? ~ ">")

  val TagPHeader = P("<" ~ Name ~ WL.?)
  val EmptyElemTagPEnd = P("/>")
  val STagPEnd = P(">")
  val ScalaPatterns = P("{" ~ Patterns ~ WL ~ "}")
  val ElemPattern: P0 = P(TagPHeader ~ (EmptyElemTagPEnd | STagPEnd ~ ContentP ~ ETagP))
  val ContentP = P(CharData.? ~ ((ElemPattern | ScalaPatterns) ~ CharData.?).rep)
  val ContentP1 = P(ElemPattern | Reference | CDSect | PI | Comment | ScalaPatterns)
  val XmlExpr = P(WL ~ XmlContent ~ (WL ~ Element).rep)
  val XmlPattern = P(WL ~ ElemPattern)
}

case class RangePosition(from: Int, to: Int)

/** Collects start and end positions of scala expressions inside xml literals.
  *
  * Doesn't really parse scala expressions, only reads until the curly brace
  * balance hits 0.
  */
class ScalaExprPositionParser(input: Input, dialect: Dialect) extends Parser[Unit] {
  private val splicePositions = Set.newBuilder[RangePosition]
  def getSplicePositions = splicePositions.result()

  def parseRec(cfg: ParseCtx, index: Int) = {
    var current = 0
    var curlyBraceCount = 1
    val scanner =
      new LegacyScanner(Input.Slice(input, index, input.chars.length), dialect)
    scanner.reader.nextChar()
    while (curlyBraceCount > 0) {
      scanner.nextToken()
      current += scanner.curr.endOffset - scanner.curr.offset + 1
      (scanner.curr.token: @switch) match {
        case LegacyToken.LBRACE => curlyBraceCount += 1
        case LegacyToken.RBRACE => curlyBraceCount -= 1
        case _ =>
      }
    }
    current -= 1 // account for }
    splicePositions += RangePosition(index, index + current)
    success(cfg.success, (), index + current, Set.empty, false)
  }
}
