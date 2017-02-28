package scala.meta.contrib

import fastparse.all._
import fastparse.core.Parsed

import scala.meta.contrib.DocToken._
import scala.meta.tokens.Token.Comment

object ScaladocParser {

  /**
    * Parses a scaladoc comment.
    */
  def parseScaladoc(comment: Comment): Seq[DocToken] = {

    def parseRec(toParse: String): Seq[DocToken] = {
      parsers
        .find(_.parse(toParse).index != 0)
        .map(_.parse(toParse)) match {
        case Some(p: Parsed.Success[DocToken, _, _]) =>
          // Parse was successful, check the remaining Scaladoc
          val remainingScaladoc =
            toParse
              .substring(p.index, toParse.length)
              .dropWhile(c => c == ' ')

          if (remainingScaladoc.trim.nonEmpty || remainingScaladoc.contains("\n\n")) {
            // Adds the parsed token to the list of tokens and parse the rest of the string recursively.
            if (remainingScaladoc.take(2) == "\n\n") {
              Seq(p.value, DocToken(Paragraph)) ++ parseRec(remainingScaladoc.dropWhile(_ == '\n'))
            } else {
              Seq(p.value) ++ parseRec(remainingScaladoc.dropWhile(c => c == ' ' || c == '\n'))
            }
          } else {
            // No more elements to parse, end recursion.
            Seq(p.value)
          }
        // Can't parse anymore, end recursion.
        case _ => Seq()
      }
    }

    parseRec(comment.content)
  }

  /**
    * Set containing all the scaladoc parsers.
    */
  private[this] val parsers: Seq[Parser[DocToken]] = {

    val bodyParser = ((AnyChar ~ !("\n@" | "{{{" | "\n\n" | End)).rep ~ AnyChar).!.map(_.trim)

    // Paragraph Parser
    val paragraphParser = "\n\n".rep.!.map(_ => DocToken(Paragraph))

    // Parser for CodeBlock instances
    val codeBlockParser =
      P(
        // Code block start
        "{{{"
        // Code within the code block.
          ~ ((AnyChar ~ !"}}}").rep ~ AnyChar).!.map(c => DocToken(CodeBlock, c.trim))
        // Code block end
          ~ "}}}"
      )
    // Parser for heading instances
    val headingParser =
      P(
        // Code block start
        "="
        // Heading description
          ~ ((AnyChar ~ !"=").rep ~ AnyChar).!.map(c => DocToken(Heading, c.trim))
        // Code block end
          ~ "="
      )
    val subHeadingParser =
      P(
        // Code block start
        "=="
        // Heading description
          ~ ((AnyChar ~ !"==").rep ~ AnyChar).!.map(c => DocToken(SubHeading, c.trim))
        // Code block end
          ~ "=="
      )

    // Parser for Inheritdoc instances
    val inheritDocParser = P("@inheritdoc".!).map(_ => DocToken(InheritDoc))

    // Parsers for all labelled docs instances
    val labelledParsers: Seq[Parser[DocToken]] = {

      DocToken.tagTokenKinds.map {
        // Single parameter doc tokens
        case kind @ DocToken.TagKind(label, 1) =>
          P(s"$label " ~ bodyParser.map(c => DocToken(kind, c.trim)))

        // Multiple parameter doc tokens
        case kind @ DocToken.TagKind(label, 2) =>
          val nameParser = ((AnyChar ~ !" ").rep ~ AnyChar).!.map(_.trim)

          val nameAndBodyParsers = {
            (nameParser ~ " ".rep.? ~ bodyParser.!).map {
              case (name, body) => DocToken(kind, name, body)
            }
          }
          P(s"$label" ~ nameAndBodyParsers)
      }
    }

    // Fallback parser(Used when no label or description is provided)
    val descriptionParser = bodyParser.map(DocToken(Description, _))

    // Merges all the parsers in a single list, with the description parser as the fallback,
    // in case no valid parser was found for an Scaladoc comment.
    (Seq(
      paragraphParser,
      subHeadingParser,
      headingParser,
      inheritDocParser,
      codeBlockParser
    ) ++ labelledParsers) :+ descriptionParser
  }
}
