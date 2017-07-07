package scala.meta.contrib

import scala.annotation.tailrec
import scala.meta.tokens.Token.Comment

object CommentOps {

  private[this] val scaladocSymbols: Char => Boolean = Set('*', ' ')
  private[this] val scaladocBorderSymbols: Char => Boolean = Set('/', '*', ' ')

  @tailrec
  private def dropRightWhile(str: String, predicate: (Char) => Boolean): String =
    if (str.isEmpty)
      ""
    else if (predicate(str.last))
      dropRightWhile(str.init, predicate)
    else
      str

  @inline
  def isScaladoc(c: Comment): Boolean = {
    val rawSyntax: String = c.syntax.trim
    rawSyntax.startsWith("/**") && rawSyntax.endsWith("*/")
  }

  def content(c: Comment): Option[String] = {
    val rawSyntax: String = c.syntax.trim
    if (isScaladoc(c)) {
      val content =
        dropRightWhile(rawSyntax, scaladocBorderSymbols)
          .dropWhile(scaladocBorderSymbols)

      Option(
        content.lines
          .map(_.dropWhile(scaladocSymbols)) // Removes leading comments symbols
          .map(_.trim)
          .mkString("\n")
          .trim)
    } else {
      Option.empty
    }
  }

  @inline
  def docTokens(c: Comment): Option[List[DocToken]] = ScaladocParser.parseScaladoc(c)
}
