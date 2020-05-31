package scala.meta.internal.parsers

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
  private val startOrNl = Start | nl

  private val spaceChars = hspaceChars + "\n"
  private val space = CharIn(spaceChars)
  private def spacesMin(min: Int) = CharsWhileIn(spaceChars, min)
  private val spaces1 = spacesMin(1)
  private val nlHspaces1 = space ~ hspaces0
  private val leadHspaces0 = startOrNl ~ hspaces0

  private val labelParser: Parser[Unit] = (!space ~ AnyChar).rep(1)
  private val wordParser: Parser[Word] = P(labelParser.!.map(Word.apply))
  private val trailWordParser = nlHspaces1 ~ wordParser

  private val listPrefix = "-" | CharIn("1aiI") ~ "."

  private val codePrefix = P("{{{")
  private val codeSuffix = P(hspaces0 ~ "}}}")

  private val linkPrefix = P("[[" ~ hspaces0)
  private val linkSuffix = P(hspaces0 ~ "]]")

  private val codeLineParser: Parser[String] = {
    val codeLineEnd = P(nl | codeSuffix)
    P((!codeLineEnd ~ AnyChar).rep.!)
  }

  private val codeExprParser: Parser[CodeExpr] = {
    val pattern = codePrefix ~ hspaces0 ~ codeLineParser ~ codeSuffix
    P(pattern.map(x => CodeExpr(x.trim)))
  }

  private val codeBlockParser: Parser[CodeBlock] = {
    val code = codeLineParser.rep(1, sep = nl)
    val pattern = leadHspaces0 ~ codePrefix ~ nl ~ code ~ codeSuffix
    P(pattern.map { x =>
      val lines = if (x.last.nonEmpty) x.toSeq else x.view.dropRight(1).toSeq
      CodeBlock(lines: _*)
    })
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
    val pattern = linkPrefix ~ anchor ~ linkSuffix
    P(pattern.map(x => Link(x.head, x.tail.toSeq: _*)))
  }

  private val textParser: Parser[Text] = {
    val anotherBeg = P(hspaces0 ~/ (CharIn("@=") | (codePrefix ~ nl) | listPrefix))
    val end = P(End | nl ~/ anotherBeg)
    val part: Parser[TextPart] = P(codeExprParser | linkParser | wordParser)
    val sep = P(!end ~ nlHspaces1)
    val text = hspaces0 ~ part.rep(1, sep = sep)
    P(text.map(x => Text(x.toSeq: _*)))
  }

  private val trailTextParser: Parser[Text] = P(nlHspaces1 ~ textParser)
  private val leadTextParser: Parser[Text] = P((Start | space) ~ hspaces0 ~ textParser)

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
          .map(x => ListBlock(prefix, x.toSeq: _*))
    }
    P(startOrNl ~ listParser)
  }

  /** Contains all scaladoc parsers */
  private val parser: Parser[collection.Seq[Term]] = {
    val allParsers = Seq(
      listBlockParser(),
      codeBlockParser,
      headingParser,
      tagParser,
      leadTextParser // keep at the end, this is the fallback
    )
    P(allParsers.reduce(_ | _).rep(1) ~/ End)
  }

  private val scaladocLine = Pattern.compile("^[ \t]*\\**(.*?)[ \t]*$")

  /** Parses a scaladoc comment */
  def parse(comment: String): Option[Scaladoc] = {
    val isScaladoc = comment.startsWith("/**") && comment.endsWith("*/")
    if (!isScaladoc) None
    else Some(parseImpl(comment.substring(3, comment.length - 2)))
  }

  private def parseImpl(comment: String): Scaladoc = {
    val sb = new java.lang.StringBuilder
    val res = Seq.newBuilder[Scaladoc.Paragraph]
    def flush: Unit =
      if (sb.length() != 0) {
        val paragraph = sb.toString
        sb.setLength(0)
        parser.parse(paragraph) match {
          case p: Parsed.Success[collection.Seq[Scaladoc.Term]] =>
            if (p.value.nonEmpty)
              res += Scaladoc.Paragraph(p.value.toSeq: _*)
          case _ =>
            res += Scaladoc.Paragraph(Unknown(paragraph))
        }
      }

    comment.linesIterator.foreach { line =>
      val matcher = scaladocLine.matcher(line)
      matcher.matches() // shouldn always match
      val beg = matcher.start(1)
      val end = matcher.end(1)
      if (beg == end) flush
      else {
        if (sb.length() != 0) sb.append('\n')
        sb.append(line, beg, end)
      }
    }
    flush
    Scaladoc(res.result(): _*)
  }

}
