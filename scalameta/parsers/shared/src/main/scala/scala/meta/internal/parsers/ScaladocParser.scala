package scala.meta.internal.parsers

import scala.meta.internal.Scaladoc

import java.nio.CharBuffer
import java.util.regex.Pattern

import fastparse.NoWhitespace._
import fastparse._

/**
 * Represents a scaladoc line.
 */
object ScaladocParser {

  import Scaladoc._

  private val numberOfSupportedHeadingLevels = 6

  private def hspace[$: P] = CharIn("\t\r ")
  private def hspacesMin[$: P](min: Int) = P(CharsWhileIn("\t\r ", min))
  private def hspaces0[$: P] = hspacesMin(0)
  private def hspaces1[$: P] = hspacesMin(1)
  private def hspacesMinWithLen[$: P](min: Int): P[Int] = (Index ~ hspacesMin(min) ~ Index).map {
    case (b, e) => e - b
  }

  private def nl[$: P]: P0 = P("\n")
  private def nlOrEndPeek[$: P] = &(End | nl)

  private def space[$: P] = CharIn("\t\r \n")
  private def spacesMin[$: P](min: Int) = CharsWhileIn("\t\r \n", min)
  private def spaces1[$: P] = spacesMin(1)

  private def punctParser[$: P] = CharsWhileIn(".,:!?;)", 0)
  private def labelParser[$: P]: P[Unit] = (!space ~ AnyChar).rep(1)
  private def wordParser[$: P]: P[Word] = P(labelParser.!.map(Word.apply))

  private def listPrefixDash[$: P] = P("-")
  private def listPrefixDecimal[$: P] = // works 1-99, standard really only covers 1
    CharIn("1-9") ~ CharIn("0-9").? ~ "."
  private def listPrefixAlpha[$: P] = // works 'a-z', standard really only covers 'a'
    CharIn("a-z") ~ "."
  private def listPrefixRoman[$: P] = { // works 1-10, standard really only covers 1
    def pat1to10(one: String, fiveTen: => P[Unit]) = one ~ (one ~ one.? | fiveTen).? | fiveTen
    pat1to10(one = "i", fiveTen = CharIn("vx")) | pat1to10(one = "I", fiveTen = CharIn("VX"))
  } ~ "."
  private def listPrefix[$: P] =
    P(listPrefixDash | listPrefixDecimal | listPrefixRoman | listPrefixAlpha)

  private def escape[$: P] = P("\\")
  private def tableDelim[$: P] = P("+" ~ ("-".rep ~ "+").rep(1))
  private def tableSep[$: P] = P("|")

  private def codePrefix[$: P] = P("{{{")
  private def codeSuffix[$: P] = P(hspaces0 ~ "}}}" ~~ !"}")

  private def linkPrefix[$: P] = P("[[" ~ hspaces0)
  private def linkSuffix[$: P] = P(hspaces0 ~ "]]")

  private def codeLineParser[$: P]: P[String] = P {
    def codeLineEnd = P(nl | codeSuffix)
    (!codeLineEnd ~ AnyChar).rep.!
  }

  private def codeExprParser[$: P]: P[CodeExpr] = P {
    def pattern = codePrefix ~ hspaces0 ~ codeLineParser ~ codeSuffix ~ punctParser.!
    pattern.map { case (x, y) => CodeExpr(x.trim, y) }
  }

  private def codeBlockParser[$: P]: P[CodeBlock] = P {
    def code = codeLineParser.rep(1, sep = nl)
    def pattern = hspaces0 ~ codePrefix ~ nl ~ code ~ codeSuffix
    pattern.map(x => CodeBlock(if (x.last.nonEmpty) x else x.dropRight(1)))
  }

  /*
   * Markdown fenced code blocks
   * https://spec.commonmark.org/0.29/#fenced-code-blocks
   */

  /* while spec mentions that a fence marker can be indented between 0 and 3 spaces,
   * examples under "list item" show that it's *relative* to the containing element;
   * also, lists are included in "container blocks" which have that property.
   * however, for non-indented case (mdOffset = 0), since we don't capture the actual
   * offset, let's allow 0 (next to asterisk) and 1 (one space from asterisk). */
  private def getMdOffsetMax(mdOffset: Int) = math.max(1, mdOffset) + 3

  private def mdCodeBlockFence[$: P] = "`".rep(3) | "~".rep(3)

  private def mdCodeBlockParser[$: P](mdOffset: Int = 0): P[MdCodeBlock] = P {
    def mdCodeBlockIndent = hspace.rep(min = mdOffset, max = getMdOffsetMax(mdOffset))

    (mdCodeBlockIndent.! ~ mdCodeBlockFence.!).flatMap { case (indent, fence) =>
      def info = hspaces0 ~ labelParser.!.rep(0, sep = hspaces0) ~ nl
      info.flatMap { infoSeq =>
        def codeEnd = mdCodeBlockIndent ~ fence.substring(0, 1).rep(fence.length)
        def line = hspace.rep(max = indent.length) ~ (!nl ~ AnyChar).rep.!
        def lines = !codeEnd ~ line.rep(1, sep = nl ~ !codeEnd)
        def block = (lines ~ nl).? ~ codeEnd ~ nlOrEndPeek
        block.map(x => MdCodeBlock(infoSeq, x.getOrElse(Nil), fence))
      }
    }
  }

  private def mdCodeSpanParser[$: P]: P[MdCodeSpan] = P {
    val tick = "`"
    tick.rep(1).!.flatMap { fence =>
      def end = fence ~ !tick
      def expr = (!end ~ !nl ~ AnyChar).rep.! ~ end ~ punctParser.!
      expr.map { case (code, punct) => MdCodeSpan(code, fence, punct) }
    }
  }

  private def headingParser[$: P]: P[Heading] = P {
    // heading delimiter
    hspaces0 ~ CharsWhileIn("=", 1).!.flatMap { delim =>
      val level = delim.length
      if (level > numberOfSupportedHeadingLevels) Fail
      else {
        def title = (!delim ~ AnyChar).rep(1)
        // Heading description and delimiter
        (title.! ~ delim ~ &(nl)).map(x => Heading(level, x.trim))
      }
    }
  }

  private def linkParser[$: P]: P[Link] = P {
    def end = space | linkSuffix
    def anchor = P((!end ~ AnyChar).rep(1).!.rep(1, sep = spaces1))
    def pattern = linkPrefix ~ (anchor ~ linkSuffix ~ labelParser.?.!)
    pattern.map { case (x, y) => new Link(x, y) }
  }

  private def nextPartParser[$: P](indent: Int, mdOffset: Int = 0): P[Unit] = P {
    // used to terminate previous part, hence indent can be less
    nl | hspacesMinWithLen(0).flatMap { offset =>
      def dedented = if (offset < indent) Pass else Fail
      def mdCodeBlockPrefix = if (offset <= getMdOffsetMax(mdOffset)) mdCodeBlockFence else Fail
      dedented | CharIn("@=") | (codePrefix ~ nl) | mdCodeBlockPrefix | tableSep | tableDelim |
        listPrefix ~ &(" ")
    }
  }

  private def textParser[$: P](indent: Int, mdOffset: Int = 0): P[Text] = P {
    def end = P(nl ~/ nextPartParser(indent, mdOffset))
    def part: P[TextPart] =
      P(codeExprParser | mdCodeSpanParser | linkParser | enclosedJavaTagParser | wordParser)
    def sep = P(!end ~ nl.? ~ hspaces0)
    hspaces0 ~ part.rep(1, sep = sep).map(x => Text(x))
  }

  // this is to be used at the start of a line
  private def leadTextParser[$: P](indent: Int, mdOffset: Int = 0) =
    !nextPartParser(indent, mdOffset) ~ textParser(indent, mdOffset)

  private def tagParser[$: P](indent: Int): P[Tag] = P {
    def label = P((nl ~ !nextPartParser(indent)).? ~ hspaces0 ~ wordParser)
    // special case: @usecase takes a single code line, on the same line
    def labelInline = P(hspaces0 ~ (!nl ~ AnyChar).rep(1).!.map(Word))
    def desc = P {
      (textParser(indent).? ~ embeddedTermsParser()).map { case (x, terms) =>
        x.fold(terms)(_ +: terms)
      }
    }
    hspaces0 ~ ("@" ~ labelParser).!.flatMap { tag =>
      val tagType = TagType.getTag(tag)
      def labelOpt =
        if (!tagType.hasLabel) Pass(None)
        else (if (tagType.optDesc) label else labelInline).map(x => Some(x))
      def descOpt = if (tagType.optDesc) desc else Pass(Nil)
      (labelOpt ~ descOpt).map { case (label, desc) => Tag(tagType, label, desc) }
    }
  }

  private def enclosedJavaTagParser[$: P]: P[EnclosedJavaTag] = P {
    def enclosed = (!(space | "}") ~ AnyChar).rep(1)
    def ltBrace = "{" ~ hspaces0
    def rtBrace = hspaces0 ~ "}"
    def tag = ("@" ~ enclosed).!
    def desc = (hspaces1 ~ enclosed.!).rep
    (ltBrace ~ tag ~ desc ~ rtBrace).map { case (tag, desc) => EnclosedJavaTag(tag, desc) }
  }

  private def listBlockParser[$: P](indent: Int = 0): P[ListBlock] = P {
    /* https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html#other-formatting-notes
     * For list blocks, "you must have extra space in front", hence min `indent + 1`. */
    /* however, for markdown lists, the outer list does not need to have an extra space
     * but the nested list item must be offset by "marker + space after it" + [0, 3] */
    def listMarker = hspacesMinWithLen(indent + 1) ~ listPrefix.! ~ hspacesMinWithLen(1)
    listMarker.flatMap { case (listIndent, prefix, ws) =>
      val mdOffset = listIndent + prefix.length + ws
      val listType = prefix(0) match {
        case '-' => ListType.Bullet
        case 'i' | 'I' => ListType.Roman
        case x => if (Character.isDigit(x)) ListType.Decimal else ListType.Alpha
      }
      def patPrefix = listType match {
        case ListType.Bullet => listPrefixDash
        case ListType.Roman => listPrefixRoman
        case ListType.Decimal => listPrefixDecimal
        case ListType.Alpha => listPrefixAlpha
      }
      def sep = nl ~ nl.? ~ hspace.rep(exactly = listIndent) ~ patPrefix.! ~ hspaces1
      def items = listItemParser(listIndent, mdOffset, prefix) ~
        sep.flatMap(listItemParser(listIndent, mdOffset, _)).rep
      items.map { case (x, xs) => ListBlock(listType, x +: xs) }
    }
  }

  private def listItemParser[$: P](indent: Int, mdOffset: Int, prefix: String) = P {
    (textParser(indent, mdOffset) ~ embeddedTermsParser(indent, mdOffset)).map { case (x, terms) =>
      ListItem(prefix, x, terms)
    }
  }

  private def tableParser[$: P]: P[Table] = P {
    def toRow(x: Seq[String]): Table.Row = Table.Row(x)
    def toAlign(x: String): Option[Table.Align] = {
      def isEnd(y: Char) = y match {
        case ':' => Some(true)
        case '-' => Some(false)
        case _ => None
      }
      for {
        head <- x.headOption
        isLeft <- isEnd(head)
        isRight <- isEnd(x.last)
        // covers "not found" (-1) and found at the end (x.length - 1)
        if 0 == (1 + x.indexWhere(_ != '-', 1)) % x.length
      } yield if (!isRight) Table.Left else if (!isLeft) Table.Right else Table.Center
    }

    def cell = P((escape ~/ AnyChar | !(nl | tableSep) ~ AnyChar).rep)
    def row = P((cell.! ~ tableSep).rep(1))

    def lineEnd = nl ~ hspaces0
    // non-standard but frequently used delimiter line, e.g.: +-----+-------+
    def delimLineNL = P(tableDelim ~ lineEnd)
    def nlDelimLine = P(lineEnd ~ tableDelim ~ nlOrEndPeek)

    def sep = P(lineEnd ~ delimLineNL.rep ~ tableSep)
    def tableBeg = P(hspaces0 ~ delimLineNL.? ~ tableSep)
    def tableEnd = P(nlDelimLine.?)

    // according to spec, the table must contain at least two rows
    def table = row.rep(2, sep = sep).map { x =>
      // we'll trim the header later; we might need it if align is missing
      val rest = x.tail.map(_.map(_.trim))
      val alignRow = rest.head
      val align = alignRow.flatMap(toAlign)
      if (align.length != alignRow.length) {
        // determine alignment from the header row
        val head = x.head
        val headBuilder = Seq.newBuilder[String]
        val alignBuilder = Seq.newBuilder[Table.Align]
        headBuilder.sizeHint(head.length)
        alignBuilder.sizeHint(head.length)
        head.foreach { cell =>
          val padLeft = cell.indexWhere(_ > ' ')
          if (padLeft < 0) {
            headBuilder += ""
            alignBuilder += Table.Left
          } else {
            val idxRight = cell.lastIndexWhere(_ > ' ') + 1
            headBuilder += cell.substring(padLeft, idxRight)
            alignBuilder += {
              val padRight = cell.length - idxRight
              if (padLeft < math.max(padRight - 1, 2)) Table.Left
              else if (padRight < math.max(padLeft - 1, 2)) Table.Right
              else Table.Center
            }
          }
        }
        Table(toRow(headBuilder.result()), alignBuilder.result(), rest.map(toRow))
      } else Table(toRow(x.head.map(_.trim)), align, rest.tail.map(toRow))
    }

    tableBeg ~ table ~ tableEnd
  }

  private def termParser[$: P] = P {
    def leadingParser = // might not consume full line
      tagParser(0)
    def completeParser = // will consume full line
      listBlockParser() | mdCodeBlockParser() | codeBlockParser | headingParser | tableParser |
        textParser(0) // keep at the end, this is the fallback
    (nl | Start) ~ (leadingParser | (completeParser ~ nlOrEndPeek)) | textParser(0) // could be following an element leaving trailing text, e.g. tagParser
  }

  private def embeddedTermsParser[$: P](indent: Int = 0, mdOffset: Int = 0): P[Seq[Term]] = P {
    def completeParser = // will consume full line
      listBlockParser(indent) | mdCodeBlockParser(mdOffset) | codeBlockParser | tableParser |
        leadTextParser(indent, mdOffset) // keep at the end, this is the fallback
    (nl ~ completeParser ~ nlOrEndPeek).rep
  }

  /** Contains all scaladoc parsers */
  private def parser[$: P]: P[Scaladoc] = P {
    def paraSep = P((nl ~ &(nl)).rep(1))
    def paraParser = P(termParser.rep(1).map(x => Scaladoc.Paragraph(x)))
    def docParser = P(paraParser.rep(sep = paraSep).map(x => Scaladoc(x)))
    paraSep.? ~ docParser ~ spacesMin(0) ~ End
  }

  private val ws = "[ \r\t]"
  private val scaladocDelim = Pattern.compile(s"$ws*(?:$$|\n$ws*\\**)")

  /** Parses a scaladoc comment */
  def parse(comment: String): Option[Scaladoc] = {
    val isScaladoc = comment.length >= 5 && comment.startsWith("/**") && comment.endsWith("*/")
    if (!isScaladoc) None
    else {
      val content = CharBuffer.wrap(comment, 3, comment.length - 2)
      // removes all trailing space, ensures newline at EOF
      val text = scaladocDelim.matcher(content).replaceAll("\n")
      fastparse.parse(text, parser(_)) match {
        case p: Parsed.Success[Scaladoc] => Some(p.value)
        case _ => None
      }
    }
  }

}
