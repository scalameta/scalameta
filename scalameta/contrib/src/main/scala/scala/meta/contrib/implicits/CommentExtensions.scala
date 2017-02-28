package scala.meta.contrib.implicits

import scala.meta._
import scala.meta.contrib.{DocToken, ScaladocParser}
import scala.meta.tokens.Token.Comment

trait CommentExtensions {

  private[this] val scaladocSymbols = Seq('/', '*', ' ')

  implicit class XtensionCommentOps(c: Comment) {

    private[this] val rawSyntax: String = c.show[Syntax].trim

    @inline
    def isScaladoc: Boolean = {
      rawSyntax.startsWith("/**") && rawSyntax.endsWith("*/")
    }

    @inline
    def content: String = {
      if (isScaladoc) {
        rawSyntax.lines
          .map(_.dropWhile(scaladocSymbols.contains)) // Removes leading comments symbols
          .map(l => l.take(l.lastIndexWhere(!scaladocSymbols.contains(_)) + 1)) // Remove trailing comments symbols
          .map(_.trim)
          .toSeq
          .mkString("\n")
          .trim
      } else {
        rawSyntax
          .take(rawSyntax.lastIndexWhere(!scaladocSymbols.contains(_)) + 1) // Remove trailing comments symbols
          .dropWhile(scaladocSymbols.contains) // Removes leading comments symbols
      }
    }

    @inline
    def structure: Seq[DocToken] = ScaladocParser.parseScaladoc(c)
  }

}

object CommentExtensions extends CommentExtensions
