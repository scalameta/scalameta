package scala.meta.contrib

import scala.meta.Syntax
import scala.meta.tokens.Token.Comment

object CommentOps {

  private[this] val scaladocSymbols = Seq('/', '*', ' ')

  @inline
  def isScaladoc(c: Comment): Boolean = {
    val rawSyntax: String = c.show[Syntax].trim
    rawSyntax.startsWith("/**") && rawSyntax.endsWith("*/")
  }

  def content(c: Comment): String = {
    val rawSyntax: String = c.show[Syntax].trim
    if (isScaladoc(c)) {
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
  def docTokens(c: Comment): Seq[DocToken] = ScaladocParser.parseScaladoc(c)
}
