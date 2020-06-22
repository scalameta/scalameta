package scala.meta.internal.parsers

import java.nio.CharBuffer
import java.util.regex.Pattern

import scala.meta.internal.Scaladoc
import scala.meta.internal.fastparse.all._

/**
 * Represents a scaladoc line.
 */
object ScaladocParser {

  import Scaladoc._

  private val numberOfSupportedHeadingLevels = 6

  private val hspaceChars = "\t\r "
  private def hspacesMin(min: Int) = CharsWhileIn(hspaceChars, min)
  private val hspaces0 = hspacesMin(0)
  private val hspaces1 = hspacesMin(1)
  private def hspacesMinWithLen(min: Int): Parser[Int] =
    (Index ~ hspacesMin(min) ~ Index).map { case (b, e) => e - b }

  private val nl: Parser[Unit] = "\n"
  private val startOrNl = nl | Start
  private val paraEnd = nl.rep(exactly = 2)

  private val spaceChars = hspaceChars + "\n"
  private val space = CharIn(spaceChars)
  private def spacesMin(min: Int) = CharsWhileIn(spaceChars, min)
  private val spaces1 = spacesMin(1)
  private val nlHspaces0 = nl.? ~ hspaces0
  private val nlHspaces1 = space ~ hspaces0
  private val leadHspaces0 = startOrNl ~ hspaces0

  private val punctParser = CharsWhileIn(".,:!?;", 0)
  private val labelParser: Parser[Unit] = (!space ~ AnyChar).rep(1)
  private val wordParser: Parser[Word] = P(labelParser.!.map(Word.apply))
  private val trailWordParser = nlHspaces1 ~ wordParser

  private val listPrefix = "-" | CharIn("1aiI") ~ "."

  private val escape = P("\\")
  private val tableSep = P("|")
  private val tableSpaceSep = P(hspaces0 ~ tableSep)

  private val codePrefix = P("{{{")
  private val codeSuffix = P(hspaces0 ~ "}}}")

  private val linkPrefix = P("[[" ~ hspaces0)
  private val linkSuffix = P(hspaces0 ~ "]]")

  private val codeLineParser: Parser[String] = {
    val codeLineEnd = P(nl | codeSuffix)
    P((!codeLineEnd ~ AnyChar).rep.!)
  }

  private val codeExprParser: Parser[CodeExpr] = {
    val pattern = codePrefix ~ hspaces0 ~ codeLineParser ~ codeSuffix ~ punctParser.!
    P(pattern.map {
      case (x, y) => CodeExpr(x.trim, y)
    })
  }

  private val codeBlockParser: Parser[CodeBlock] = {
    val code = codeLineParser.rep(1, sep = nl)
    val pattern = leadHspaces0 ~ codePrefix ~ nl ~ code ~ codeSuffix
    P(pattern.map { x => CodeBlock(if (x.last.nonEmpty) x.toSeq else x.view.dropRight(1).toSeq) })
  }

  private val headingParser: Parser[Heading] = {
    val delimParser = leadHspaces0 ~ CharsWhileIn("=", 1).!
    P(
      // heading delimiter
      delimParser.flatMap { delim =>
        val level = delim.length
        if (level > numberOfSupportedHeadingLevels) Fail
        else {
          val title = (!delim ~ AnyChar).rep(1)
          // Heading description and delimiter
          (title.! ~ delim ~ &(nl)).map(x => Heading(level, x.trim))
        }
      }
    )
  }

  private val linkParser: Parser[Link] = {
    val end = space | linkSuffix
    val anchor = P((!end ~ AnyChar).rep(1).!.rep(1, sep = spaces1))
    val pattern = linkPrefix ~ (anchor ~ linkSuffix ~ punctParser.!)
    P(pattern.map {
      case (x, y) => Link(x.head, x.tail.toSeq, y)
    })
  }

  private val textParser: Parser[Text] = {
    val anotherBeg = P(CharIn("@=") | (codePrefix ~ nl) | listPrefix | tableSep | "+-" | nl)
    val end = P(End | nl ~/ hspaces0 ~/ anotherBeg)
    val part: Parser[TextPart] = P(!paraEnd ~ (codeExprParser | linkParser | wordParser))
    val sep = P(!end ~ nlHspaces0)
    val text = hspaces0 ~ part.rep(1, sep = sep)
    P(text.map(x => Text(x.toSeq)))
  }

  private val trailTextParser: Parser[Text] = P(nlHspaces1 ~ textParser)
  private val leadTextParser: Parser[Text] = P((space | Start) ~ hspaces0 ~ textParser)

  private val tagParser: Parser[Tag] = {
    val tagTypeMap = TagType.predefined.map(x => x.tag -> x).toMap
    def getParserByTag(tag: String): Parser[Tag] = {
      val tagTypeOpt = tagTypeMap.get(tag)
      tagTypeOpt.fold[Parser[Tag]] {
        trailTextParser.map { desc => Tag(TagType.UnknownTag(tag), desc = desc) }
      } { tagType =>
        (tagType.hasLabel, tagType.hasDesc) match {
          case (false, false) => PassWith(Tag(tagType))
          case (false, true) => trailTextParser.map(x => Tag(tagType, desc = x))
          case (true, false) => trailWordParser.map(x => Tag(tagType, label = x))
          case (true, true) =>
            (trailWordParser ~ trailTextParser).map {
              case (label, desc) => Tag(tagType, label, desc)
            }
        }
      }
    }
    val tagLabelParser = P(("@" ~ labelParser).!)
    P(leadHspaces0 ~ tagLabelParser.flatMap(getParserByTag))
  }

  private def listBlockParser(minIndent: Int = 1): Parser[ListBlock] = {
    val listParser = (hspacesMinWithLen(minIndent) ~ listPrefix.! ~ hspaces1).flatMap {
      case (indent, prefix) =>
        val sep = (nl ~ hspacesMinWithLen(indent) ~ prefix ~ hspaces1).flatMap { x =>
          if (x != indent) Fail else Pass
        }
        (textParser ~ listBlockParser(indent + 1).?)
          .map { case (desc, list) => ListItem(desc, list) }
          .rep(1, sep = sep)
          .map(x => ListBlock(prefix, x.toSeq))
    }
    P(startOrNl ~ listParser)
  }

  private val tableParser: Parser[Table] = {
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

    val cellChar = escape ~ AnyChar | !(nl | escape | tableSep) ~ AnyChar
    val row = (cellChar.rep.! ~ tableSpaceSep).rep(1)
    // non-standard but frequently used delimiter line, e.g.: +-----+-------+
    val delimLine = hspaces0 ~ CharsWhileIn("+-")
    val sep = nl ~ (delimLine ~ nl).rep ~ tableSpaceSep
    // according to spec, the table must contain at least two rows
    val table = row.rep(2, sep = sep).map { x =>
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

    P(startOrNl ~ (delimLine ~ nl).? ~ tableSpaceSep ~ table ~ (nl ~ delimLine).?)
  }

  /** Contains all scaladoc parsers */
  private val parser: Parser[Scaladoc] = {
    val allParsers = Seq(
      listBlockParser(),
      codeBlockParser,
      headingParser,
      tagParser,
      tableParser,
      leadTextParser // keep at the end, this is the fallback
    )
    val termParser = P(allParsers.reduce(_ | _))
    val termEnd = End | paraEnd
    val termsParser = P((!termEnd ~ termParser).rep(1))
    val paraParser = termsParser.map(x => Scaladoc.Paragraph(x.toSeq))
    val paraSep = (nl ~ &(nl)).rep(1)
    val docParser = paraParser.rep(sep = paraSep).map(x => Scaladoc(x.toSeq))
    P(paraSep.? ~ docParser ~ spacesMin(0) ~ End)
  }

  private val scaladocDelim = Pattern.compile("[ \t]*(?:$|\n[ \t]*\\**)")

  /** Parses a scaladoc comment */
  def parse(comment: String): Option[Scaladoc] = {
    val isScaladoc = comment.length >= 5 && comment.startsWith("/**") && comment.endsWith("*/")
    if (!isScaladoc) None
    else {
      val content = CharBuffer.wrap(comment, 3, comment.length - 2)
      val text = scaladocDelim.matcher(content).replaceAll("\n")
      parser.parse(text) match {
        case p: Parsed.Success[Scaladoc] => Some(p.value)
        case _ => None
      }
    }
  }

}
