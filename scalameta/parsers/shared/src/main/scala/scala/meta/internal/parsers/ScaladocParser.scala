package scala.meta.internal.parsers

import java.nio.CharBuffer
import java.util.regex.Pattern

import scala.meta.internal.Scaladoc
import scala.meta.internal.fastparse
import scala.meta.internal.fastparse._
import scala.meta.internal.fastparse.NoWhitespace._

/**
 * Represents a scaladoc line.
 */
object ScaladocParser {

  import Scaladoc._

  private val numberOfSupportedHeadingLevels = 6

  private def hspace[_: P] = CharIn("\t\r ")
  private def hspacesMin[_: P](min: Int) = P(CharsWhileIn("\t\r ", min))
  private def hspaces0[_: P] = hspacesMin(0)
  private def hspaces1[_: P] = hspacesMin(1)
  private def plusMinus[_: P] = CharsWhileIn("+\\-")
  private def hspacesMinWithLen[_: P](min: Int): P[Int] =
    (Index ~ hspacesMin(min) ~ Index).map { case (b, e) => e - b }

  private def nl[_: P]: P0 = P("\n")
  private def nlOrEndPeek[_: P] = &(End | nl)

  private def space[_: P] = CharIn("\t\r \n")
  private def spacesMin[_: P](min: Int) = CharsWhileIn("\t\r \n", min)
  private def spaces1[_: P] = spacesMin(1)
  private def nlHspaces0[_: P] = nl.? ~ hspaces0
  private def nlHspaces1[_: P] = space ~ hspaces0

  private def punctParser[_: P] = CharsWhileIn(".,:!?;)", 0)
  private def labelParser[_: P]: P[Unit] = (!space ~ AnyChar).rep(1)
  private def wordParser[_: P]: P[Word] = P(labelParser.!.map(Word.apply))

  private def listPrefix[_: P] = "-" | CharIn("1aiI") ~ "."

  private def escape[_: P] = P("\\")
  private def tableSep[_: P] = P("|")
  private def tableSpaceSep[_: P] = P(hspaces0 ~ tableSep)

  private def codePrefix[_: P] = P("{{{")
  private def codeSuffix[_: P] = P(hspaces0 ~ "}}}" ~~ !"}")

  private def linkPrefix[_: P] = P("[[" ~ hspaces0)
  private def linkSuffix[_: P] = P(hspaces0 ~ "]]")

  private def codeLineParser[_: P]: P[String] = P {
    def codeLineEnd = P(nl | codeSuffix)
    (!codeLineEnd ~ AnyChar).rep.!
  }

  private def codeExprParser[_: P]: P[CodeExpr] = P {
    def pattern = codePrefix ~ hspaces0 ~ codeLineParser ~ codeSuffix ~ punctParser.!
    pattern.map { case (x, y) => CodeExpr(x.trim, y) }
  }

  private def codeBlockParser[_: P]: P[CodeBlock] = P {
    def code = codeLineParser.rep(1, sep = nl)
    def pattern = hspaces0 ~ codePrefix ~ nl ~ code ~ codeSuffix
    pattern.map { x => CodeBlock(if (x.last.nonEmpty) x else x.dropRight(1)) }
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

  private def mdCodeBlockFence[_: P] = "`".rep(3) | "~".rep(3)

  private def mdCodeBlockParser[_: P](mdOffset: Int = 0): P[MdCodeBlock] = P {
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

  private def headingParser[_: P]: P[Heading] = P {
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

  private def linkParser[_: P]: P[Link] = P {
    def end = space | linkSuffix
    def anchor = P((!end ~ AnyChar).rep(1).!.rep(1, sep = spaces1))
    def pattern = linkPrefix ~ (anchor ~ linkSuffix ~ labelParser.?.!)
    pattern.map { case (x, y) => new Link(x, y) }
  }

  private def nextPartParser[_: P](mdOffset: Int = 0): P[Unit] = P {
    // used to terminate previous part, hence indent can be less
    def mdCodeBlockPrefix = hspace.rep(max = getMdOffsetMax(mdOffset)) ~ mdCodeBlockFence
    def anotherBeg = P(CharIn("@=") | (codePrefix ~ nl) | listPrefix | tableSep | "+-" | nl)
    nl | mdCodeBlockPrefix | hspaces0 ~/ anotherBeg
  }

  private def textParser[_: P](mdOffset: Int = 0): P[Text] = P {
    def end = P(nl ~/ nextPartParser(mdOffset))
    def part: P[TextPart] = P(codeExprParser | linkParser | wordParser)
    def sep = P(!end ~ nlHspaces0)
    hspaces0 ~ part.rep(1, sep = sep).map(x => Text(x))
  }

  // this is to be used at the start of a line
  private def leadTextParser[_: P](mdOffset: Int = 0) =
    !nextPartParser(mdOffset) ~ textParser(mdOffset)

  private def tagParser[_: P]: P[Tag] = P {
    def label = P((nl ~ !nextPartParser()).? ~ hspaces0 ~ wordParser)
    def desc = P {
      (textParser().? ~ embeddedTermsParser())
        .map { case (x, terms) => x.fold(terms)(_ +: terms) }
    }
    hspaces0 ~ ("@" ~ labelParser).!.flatMap { tag =>
      val tagType = TagType.getTag(tag)
      def labelOpt = if (tagType.hasLabel) label.map(x => Some(x)) else Pass(None)
      def descOpt = if (tagType.optDesc) desc else Pass(Nil)
      (labelOpt ~ descOpt).map { case (label, desc) => Tag(tagType, label, desc) }
    }
  }

  private def listBlockParser[_: P](indent: Int = 0): P[ListBlock] = P {
    /* https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html#other-formatting-notes
     * For list blocks, "you must have extra space in front", hence min `indent + 1`. */
    /* however, for markdown lists, the outer list does not need to have an extra space
     * but the nested list item must be offset by "marker + space after it" + [0, 3] */
    def listMarker = hspacesMinWithLen(indent + 1) ~ listPrefix.! ~ hspacesMinWithLen(1)
    listMarker.flatMap { case (listIndent, prefix, ws) =>
      val mdOffset = listIndent + prefix.length + ws
      def sep = nl ~ hspace.rep(exactly = listIndent) ~ prefix ~ hspaces1
      listItemParser(listIndent, mdOffset).rep(1, sep = sep).map(x => ListBlock(prefix, x))
    }
  }

  private def listItemParser[_: P](indent: Int, mdOffset: Int) = P {
    (textParser(mdOffset) ~ embeddedTermsParser(indent, mdOffset))
      .map { case (x, terms) => ListItem(x, terms) }
  }

  private def tableParser[_: P]: P[Table] = P {
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
      } yield {
        if (!isRight) Table.Left
        else if (!isLeft) Table.Right
        else Table.Center
      }
    }

    def cellChar = escape ~ AnyChar | !(nl | escape | tableSep) ~ AnyChar
    def row = (cellChar.rep.! ~ tableSpaceSep).rep(1)
    // non-standard but frequently used delimiter line, e.g.: +-----+-------+
    def delimLine = hspaces0 ~ plusMinus
    def sep = nl ~ (delimLine ~ nl).rep ~ tableSpaceSep
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
      } else
        Table(toRow(x.head.map(_.trim)), align, rest.tail.map(toRow))
    }

    (delimLine ~ nl).? ~ tableSpaceSep ~ table ~ (nl ~ delimLine).?
  }

  private def termParser[_: P] = P {
    def leadingParser = // might not consume full line
      tagParser
    def completeParser = // will consume full line
      listBlockParser() |
        mdCodeBlockParser() |
        codeBlockParser |
        headingParser |
        tableParser |
        textParser() // keep at the end, this is the fallback
    (nl | Start) ~ (leadingParser | (completeParser ~ nlOrEndPeek)) |
      textParser() // could be following an element leaving trailing text, e.g. tagParser
  }

  private def embeddedTermsParser[_: P](indent: Int = 0, mdOffset: Int = 0): P[Seq[Term]] = P {
    def completeParser = // will consume full line
      listBlockParser(indent) |
        mdCodeBlockParser(mdOffset) |
        codeBlockParser |
        tableParser |
        leadTextParser(mdOffset) // keep at the end, this is the fallback
    (nl ~ completeParser ~ nlOrEndPeek).rep
  }

  /** Contains all scaladoc parsers */
  private def parser[_: P]: P[Scaladoc] = P {
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
