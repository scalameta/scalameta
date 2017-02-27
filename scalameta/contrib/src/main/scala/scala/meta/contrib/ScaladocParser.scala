package scala.meta.contrib
import DocToken.{CodeBlock, Description, InheritDoc, _}
import fastparse.all._
import fastparse.core.Parsed

import scala.meta.Syntax
import scala.meta.tokens.Token.Comment

object ScaladocParser {

  /**
    * Parses a scaladoc [[Comment]].
    */
  def parseScaladoc(comment: Comment): Seq[DocToken] = {
    def parseRec(toParse: String): Seq[DocToken] = {
      parsers
        .find(_.parse(toParse).index != 0)
        .map(_.parse(toParse)) match {
        case Some(p: Parsed.Success[DocToken, _, _]) =>
          Seq(p.value) ++ parseRec(toParse.substring(p.index, toParse.length).trim)
        case _ => Seq()
      }
    }

    parseRec(prepareScaladoc(comment))
  }

  /**
    * List of reserved symbols used on scaladoc documentation.
    */
  private[this] val scaladocSymbols = Seq('/', '*', ' ')

  /**
    * Prepares the scaladoc removing all the notation symbols leaving
    * only its content, for making it easier to parse it.
    */
  private[this] def prepareScaladoc(scaladoc: Comment): String = {

    val trimmedScaladoc: String = scaladoc.show[Syntax].trim

    require(
      requirement = trimmedScaladoc.startsWith("/*") && trimmedScaladoc.endsWith("*/"),
      message = "Input comment is not a Scaladoc"
    )

    trimmedScaladoc.lines
      .map(_.dropWhile(scaladocSymbols.contains)) // Removes leading comments symbols
      .map(l => l.take(l.lastIndexWhere(!scaladocSymbols.contains(_)) + 1)) // Remove trailing comments symbols
      .filter(_.nonEmpty) // Removes empty scaladoc lines
      .toSeq
      .mkString("\n")
  }

  /**
    * Set containing all the scaladoc parsers.
    */
  private[this] val parsers: Seq[Parser[DocToken]] = {

    val bodyParser = ((AnyChar ~ !("\n@" | "{{{" | End)).rep ~ AnyChar).!.map(_.trim)

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

    // Parser for Inheritdoc instances
    val inheritDocParser = P("@inheritdoc".!).map(_ => DocToken(InheritDoc))

    // Parsers for all labelled docs instances
    val labelledParsers: Seq[Parser[DocToken]] = {

      DocToken.labelledTokenKinds.map {
        // Single parameter doc tokens
        case kind @ DocToken.LabelledKind(label, 1) =>
          P(s"$label " ~ bodyParser.map(c => DocToken(kind, c.trim)))

        // Multiple parameter doc tokens
        case kind @ DocToken.LabelledKind(label, 2) =>
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

    // Merges all the parsers in a single list, with the description parser as the fallback one.
    (Seq(inheritDocParser, codeBlockParser) ++ labelledParsers) :+ descriptionParser
  }
}
