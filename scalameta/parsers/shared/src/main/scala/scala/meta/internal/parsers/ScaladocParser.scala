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
  private def startOrNl[_: P] = nl | Start
  private def paraEnd[_: P] = nl.rep(exactly = 2)

  private def space[_: P] = CharIn("\t\r \n")
  private def spacesMin[_: P](min: Int) = CharsWhileIn("\t\r \n", min)
  private def spaces1[_: P] = spacesMin(1)
  private def nlHspaces0[_: P] = nl.? ~ hspaces0
  private def nlHspaces1[_: P] = space ~ hspaces0
  private def leadHspaces0[_: P] = startOrNl ~ hspaces0

  private def punctParser[_: P] = CharsWhileIn(".,:!?;)", 0)
  private def labelParser[_: P]: P[Unit] = (!space ~ AnyChar).rep(1)
  private def wordParser[_: P]: P[Word] = P(labelParser.!.map(Word.apply))

  private def listPrefix[_: P] = "-" | CharIn("1aiI") ~ "."

  private def escape[_: P] = P("\\")
  private def tableSep[_: P] = P("|")
  private def tableSpaceSep[_: P] = P(hspaces0 ~ tableSep)

  private def codePrefix[_: P] = P("{{{")
  private def codeSuffix[_: P] = P(hspaces0 ~ "}}}")

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
    def pattern = leadHspaces0 ~ codePrefix ~ nl ~ code ~ codeSuffix
    pattern.map { x => CodeBlock(if (x.last.nonEmpty) x else x.dropRight(1)) }
  }

  /*
   * Markdown fenced code blocks
   * https://spec.commonmark.org/0.29/#fenced-code-blocks
   */

  private def mdCodeBlockIndent[_: P] = hspace.rep(max = 3)
  private def mdCodeBlockFence[_: P] = "`".rep(3) | "~".rep(3)

  private def mdCodeBlockParser[_: P]: P[MdCodeBlock] = P {
    (startOrNl ~ mdCodeBlockIndent.! ~ mdCodeBlockFence.!).flatMap { case (indent, fence) =>
      def lineEnd = hspaces0 ~ nl
      def info = hspaces0 ~ labelParser.!.rep(0, sep = hspaces0) ~ lineEnd
      info.flatMap { infoSeq =>
        def codeEnd = mdCodeBlockIndent ~ fence.substring(0, 1).rep(fence.length)
        def line = hspace.rep(max = indent.length) ~ (!lineEnd ~ AnyChar).rep.!
        def lines = !codeEnd ~ line.rep(1, sep = lineEnd ~ !codeEnd)
        def block = (lines ~ lineEnd).? ~ codeEnd ~ hspaces0 ~ &(nl | End)
        block.map(x => MdCodeBlock(infoSeq, x.getOrElse(Nil), fence))
      }
    }
  }

  private def headingParser[_: P]: P[Heading] = P {
    def delimParser = leadHspaces0 ~ CharsWhileIn("=", 1).!
    // heading delimiter
    delimParser.flatMap { delim =>
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

  private def nextPartParser[_: P]: P[Unit] = P {
    def mdCodeBlockPrefix = mdCodeBlockIndent ~ mdCodeBlockFence
    def anotherBeg = P(CharIn("@=") | (codePrefix ~ nl) | listPrefix | tableSep | "+-" | nl)
    nl ~/ (nl | mdCodeBlockPrefix | hspaces0 ~/ anotherBeg)
  }

  private def textParser[_: P]: P[Text] = P {
    def end = P(End | nextPartParser)
    def part: P[TextPart] = P(!paraEnd ~ (codeExprParser | linkParser | wordParser))
    def sep = P(!end ~ nlHspaces0)
    def text = hspaces0 ~ part.rep(1, sep = sep)
    text.map(x => Text(x))
  }

  private def leadTextParser[_: P]: P[Text] = P((space | Start) ~ hspaces0 ~ textParser)

  private def tagLabelParser[_: P]: P[Word] = P(!nextPartParser ~ nlHspaces1 ~ wordParser)

  private def tagDescParser[_: P]: P[Option[Text]] = P {
    hspaces0 ~ (textParser | !nextPartParser ~ nl ~ textParser).?
  }

  private def tagParser[_: P]: P[Tag] = P {
    leadHspaces0 ~ ("@" ~ labelParser).!.flatMap { tag =>
      val tagType = TagType.getTag(tag)
      (tagType.hasLabel, tagType.optDesc) match {
        case (false, false) => Pass(Tag(tagType))
        case (false, true) => tagDescParser.map(x => Tag(tagType, desc = x))
        case (true, false) => tagLabelParser.map(x => Tag(tagType, label = Some(x)))
        case (true, true) =>
          (tagLabelParser ~ tagDescParser).map { case (label, desc) =>
            Tag(tagType, Some(label), desc)
          }
      }
    }
  }

  private def listBlockParser[_: P](indent: Int = 0): P[ListBlock] = P {
    /* https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html#other-formatting-notes
     * For list blocks, "you must have extra space in front", hence min `indent + 1`. */
    def listMarker = hspacesMinWithLen(indent + 1) ~ listPrefix.! ~ hspaces1
    def listParser = listMarker.flatMap { case (listIndent, prefix) =>
      def sep = nl ~ hspace.rep(exactly = listIndent) ~ prefix ~ hspaces1
      listItemParser(listIndent).rep(1, sep = sep).map(x => ListBlock(prefix, x))
    }
    startOrNl ~ listParser
  }

  private def listItemParser[_: P](indent: Int) = P {
    (textParser ~ listBlockParser(indent).?).map { case (desc, list) => ListItem(desc, list) }
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

    startOrNl ~ (delimLine ~ nl).? ~ tableSpaceSep ~ table ~ (nl ~ delimLine).?
  }

  private def termParser[_: P] =
    listBlockParser() |
      mdCodeBlockParser |
      codeBlockParser |
      headingParser |
      tagParser |
      tableParser |
      leadTextParser // keep at the end, this is the fallback

  private def notTermEnd[_: P] = P(!(End | paraEnd))
  private def termsParser[_: P] = P(notTermEnd ~ termParser)
  private def paraParser[_: P] = P(termsParser.rep(1).map(x => Scaladoc.Paragraph(x)))
  private def paraSep[_: P] = P((nl ~ &(nl)).rep(1))
  private def docParser[_: P] = P(paraParser.rep(sep = paraSep).map(x => Scaladoc(x)))

  /** Contains all scaladoc parsers */
  private def parser[_: P]: P[Scaladoc] = P {
    paraSep.? ~ docParser ~ spacesMin(0) ~ End
  }

  private val scaladocDelim = Pattern.compile("[ \t]*(?:$|\n[ \t]*\\**)")

  /** Parses a scaladoc comment */
  def parse(comment: String): Option[Scaladoc] = {
    val isScaladoc = comment.length >= 5 && comment.startsWith("/**") && comment.endsWith("*/")
    if (!isScaladoc) None
    else {
      val content = CharBuffer.wrap(comment, 3, comment.length - 2)
      val text = scaladocDelim.matcher(content).replaceAll("\n")
      fastparse.parse(text, parser(_)) match {
        case p: Parsed.Success[Scaladoc] => Some(p.value)
        case _ => None
      }
    }
  }

}
