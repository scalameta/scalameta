package scala.meta.contrib

import DocToken._
import scala.meta.Syntax
import scala.meta.tokens.Token.Comment

object ScaladocParser {

  /**
    * Parses a scaladoc [[Comment]].
    */
  def parseScaladoc(comment: Comment): Seq[DocToken] = {

    val trimmedComment: String = comment.show[Syntax].trim

    require(
      requirement = trimmedComment.startsWith("/*") && trimmedComment.endsWith("*/"),
      message = "Input comment is not a Scaladoc"
    )

    val scaladocSymbols = Seq('/', '*', ' ')

    val lineSeparedDocString: Seq[String] =
      trimmedComment
        .split(Array('\n', '\r')) // Splits the Scaladoc in lines
        .map(_.dropWhile(scaladocSymbols.contains)) // Removes leading comments symbols
        .map(l => l.take(l.lastIndexWhere(!scaladocSymbols.contains(_)) + 1)) // Remove trailing comments symbols
        .filter(_.nonEmpty) // Removes empty scaladoc lines

    mergeTokens(
      lineSeparedDocString.map(parseScaladocLine)
    )
  }

  /**
    * Once the Scaladoc parsing is done, merges multiline [[DocToken]].
    */
  private[this] def mergeTokens(docTokens: Seq[DocToken]): Seq[DocToken] = {
    docTokens.foldLeft(Seq[DocToken]()) {
      (acc: Seq[DocToken], nextToken: DocToken) => {
        acc.lastOption match {
          // If the next token is a DocText, append it to the previous token
          case Some(previousToken) if nextToken.kind.equals(Description) =>
            acc.dropRight(1) :+ previousToken.append(nextToken.body)
          // If the documentation is the first one allow everything
          case _ =>
            acc :+ nextToken
        }
      }
    }
  }

  /**
    * Parses a Scaladoc line [[String]] into a [[DocToken]]
    * with the line [[Kind]], if it is known.
    */
  private[this] def parseScaladocLine(docLine: String): DocToken = {

    DocToken.labelledTokenKinds.find(kind => docLine.startsWith(kind.label)) match {
        // Single lines tokens
      case Some(labelKind) if labelKind.numberParameters == 1 =>
        prepareSingleParameterToken(labelKind, docLine)
        // Multiple parameter token types
      case Some(labelKind) =>
        prepareMultipleParameterToken(labelKind, docLine)
      case _ if docLine.startsWith(OtherTag.label) =>
        val label = docLine.trim.split(" ").head
        val description = docLine.replaceFirst(label, "").trim
        DocToken(OtherTag, label, description)
      case _ => DocToken(Description, docLine)
    }
  }

  private[this] def prepareSingleParameterToken(docKind: Kind, docLine: String): DocToken =
    DocToken(docKind, docLine.replaceFirst(docKind.label, "").trim)

  private[this] def prepareMultipleParameterToken(docKind: Kind, docLine: String): DocToken = {
    val nameAndDescription = docLine.replaceFirst(s"${docKind.label} ", "")
    val name: String = nameAndDescription.split(' ').head
    val description: String = nameAndDescription.replaceFirst(s"$name ", "")
    DocToken(docKind, name, description)
  }
}
