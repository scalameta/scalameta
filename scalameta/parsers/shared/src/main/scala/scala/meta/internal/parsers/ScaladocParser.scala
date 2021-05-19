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

  private def punctParser[_: P] = CharsWhileIn(".,:!?;", 0)
  private def labelParser[_: P]: P[Unit] = (!space ~ AnyChar).rep(1)
  private def wordParser[_: P]: P[Word] = P(labelParser.!.map(Word.apply))
  private def trailWordParser[_: P] = nlHspaces1 ~ wordParser

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
    pattern.map { x => CodeBlock(if (x.last.nonEmpty) x.toSeq else x.view.dropRight(1).toSeq) }
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
    def pattern = linkPrefix ~ (anchor ~ linkSuffix ~ punctParser.!)
    pattern.map { case (x, y) => new Link(x, y) }
  }

  private def textParser[_: P]: P[Text] = P {
    def mdCodeBlockPrefix = mdCodeBlockIndent ~ mdCodeBlockFence
    def anotherBeg = P(CharIn("@=") | (codePrefix ~ nl) | listPrefix | tableSep | "+-" | nl)
    def end = P(End | nl ~/ (mdCodeBlockPrefix | hspaces0 ~/ anotherBeg))
    def part: P[TextPart] = P(!paraEnd ~ (codeExprParser | linkParser | wordParser))
    def sep = P(!end ~ nlHspaces0)
    def text = hspaces0 ~ part.rep(1, sep = sep)
    text.map(x => Text(x.toSeq))
  }

  private def trailTextParser[_: P]: P[Text] = P(nlHspaces1 ~ textParser)
  private def leadTextParser[_: P]: P[Text] = P((space | Start) ~ hspaces0 ~ textParser)

  private def tagParser[_: P]: P[Tag] = P {
    def tagTypeMap = TagType.predefined.map(x => x.tag -> x).toMap
    def getParserByTag(tag: String): P[Tag] = {
      val tagTypeOpt = tagTypeMap.get(tag)
      tagTypeOpt.fold[P[Tag]] {
        trailTextParser.map { desc => Tag(TagType.UnknownTag(tag), desc = desc) }
      } { tagType =>
        (tagType.hasLabel, tagType.hasDesc) match {
          case (false, false) => Pass(Tag(tagType))
          case (false, true) => trailTextParser.map(x => Tag(tagType, desc = x))
          case (true, false) => trailWordParser.map(x => Tag(tagType, label = x))
          case (true, true) =>
            (trailWordParser ~ trailTextParser).map { case (label, desc) =>
              Tag(tagType, label, desc)
            }
        }
      }
    }
    def tagLabelParser = P(("@" ~ labelParser).!)
    leadHspaces0 ~ tagLabelParser.flatMap(getParserByTag)
  }

  private def listBlockParser[_: P](minIndent: Int = 1): P[ListBlock] = P {
    def listParser = (hspacesMinWithLen(minIndent) ~ listPrefix.! ~ hspaces1).flatMap {
      case (indent, prefix) =>
        def sep = (nl ~ hspacesMinWithLen(indent) ~ prefix ~ hspaces1).flatMap { x =>
          if (x != indent) Fail else Pass
        }
        (textParser ~ listBlockParser(indent + 1).?)
          .map { case (desc, list) => ListItem(desc, list) }
          .rep(1, sep = sep)
          .map(x => ListBlock(prefix, x.toSeq))
    }
    startOrNl ~ listParser
  }

  private def tableParser[_: P]: P[Table] = P {
    def toRow(x: Iterable[String]): Table.Row = Table.Row(x.toSeq)
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
      val align = alignRow.flatMap(toAlign).toSeq
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
        Table(toRow(headBuilder.result()), alignBuilder.result(), rest.toSeq.map(toRow))
      } else
        Table(toRow(x.head.map(_.trim)), align, rest.tail.toSeq.map(toRow))
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
  private def paraParser[_: P] = P((termsParser.rep(1)).map(x => Scaladoc.Paragraph(x.toSeq)))
  private def paraSep[_: P] = P((nl ~ &(nl)).rep(1))
  private def docParser[_: P] = P(paraParser.rep(sep = paraSep).map(x => Scaladoc(x.toSeq)))

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
