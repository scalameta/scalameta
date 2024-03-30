package scala.meta.contrib

import scala.meta.contrib.DocToken._
import scala.meta.tokens.Token.Comment

import fastparse.NoWhitespace._
import fastparse._

object ScaladocParser {

  private[this] val numberOfSupportedHeadingLevels = 6

  /**
   * Parses a scaladoc comment.
   */
  def parseScaladoc(comment: Comment): Option[List[DocToken]] = {

    def parseRec(toParse: String): List[DocToken] = parsers.iterator.map { p =>
      parse(toParse, p(_))
    }.collectFirst {
      case Parsed.Success(value, index) if index != 0 =>
        // Parse was successful, check the remaining Scaladoc
        val remainingScaladoc = toParse.substring(index, toParse.length).dropWhile(c => c == ' ')

        if (remainingScaladoc.trim.nonEmpty || remainingScaladoc.contains("\n\n"))
          // Adds the parsed token to the list of tokens and parse the rest of the string recursively.
          if (remainingScaladoc.take(2) == "\n\n") List(value, DocToken(Paragraph)) ++
            parseRec(remainingScaladoc.dropWhile(_ == '\n'))
          else List(value) ++ parseRec(remainingScaladoc.dropWhile(c => c == ' ' || c == '\n'))
        else
          // No more elements to parse, end recursion.
          List(value)
    }.getOrElse(Nil)

    comment.content.map(parseRec)
  }

  private[this] def generateHeadingParser[$: P](headingType: Heading): P[DocToken] = {
    val headingSymbols = "=" * headingType.level
    P(
      // Code block start
      headingSymbols
      // Heading description
      ~ ((AnyChar ~ !"=").rep ~ AnyChar).!.map(c => DocToken(headingType, c.trim))
      // Code block end
      ~ headingSymbols
    )
  }

  /**
   * Set containing all the scaladoc parsers.
   */
  private[this] def parsers: List[P[_] => P[DocToken]] = {

    def bodyParser[$: P] = ((AnyChar ~ !("\n@" | "{{{" | "\n\n" | End)).rep ~ AnyChar).!.map(_.trim)

    // Paragraph Parser
    def paragraphParser[$: P] = "\n\n".rep.!.map(_ => DocToken(Paragraph))

    // Parser for CodeBlock instances
    def codeBlockParser[$: P] = P(
      // Code block start
      "{{{"
      // Code within the code block.
      ~ ((AnyChar ~ !"}}}").rep ~ AnyChar).!.map(c => DocToken(CodeBlock, c.trim))
      // Code block end
      ~ "}}}"
    )

    // Parsers for headings/subheadings instances.
    val headingsParsers: List[P[_] => P[DocToken]] = DocToken.allHeadings.reverse.map { heading =>
      generateHeadingParser(heading)(_: P[_])
    }.toList

    // Parser for Inheritdoc instances
    def inheritDocParser[$: P] = P("@inheritdoc".!).map(_ => DocToken(InheritDoc))

    // Parsers for all labelled docs instances
    val labelledParsers: List[P[_] => P[DocToken]] = DocToken.tagTokenKinds.map { kind =>
      val label = kind.label
      if (kind.numberParameters == 1) {
        // Single parameter doc tokens
        def tagKindParser[$: P] = P(s"$label " ~ bodyParser.map(c => DocToken(kind, c.trim)))
        tagKindParser(_: P[_])
      } else {
        require(kind.numberParameters == 2)
        // Multiple parameter doc tokens
        def parser[$: P] = {
          def nameParser: P[String] = ((AnyChar ~ !" ").rep ~ AnyChar).!.map(_.trim)

          def nameAndBodyParsers: P[DocToken] = (nameParser ~ " ".rep.? ~ bodyParser.!).map {
            case (name, body) => DocToken(kind, name, body)
          }
          P(s"$label" ~ nameAndBodyParsers)
        }
        parser(_: P[_])
      }
    }

    // Fallback parser(Used when no label or description is provided)
    def descriptionParser[$: P] = bodyParser.map(DocToken(Description, _))

    List(paragraphParser(_: P[_]), inheritDocParser(_: P[_]), codeBlockParser(_: P[_])) ++
      headingsParsers ++ labelledParsers :+ (descriptionParser(_: P[_]))
  }
}
