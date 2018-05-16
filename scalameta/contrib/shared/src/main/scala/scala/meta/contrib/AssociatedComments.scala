package scala.meta.contrib

import scala.meta._
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Comment
import scala.collection.immutable.List
import org.scalameta.logger

sealed abstract class AssociatedComments(
    leadingMap: Map[Token, List[Comment]],
    trailingMap: Map[Token, List[Comment]]) {
  private def pretty(map: Map[Token, List[Comment]]): String =
    map
      .map {
        case (tok, comments) =>
          val commentStructure = comments.map(comment => logger.revealWhitespace(comment.syntax))
          s"    ${tok.structure} => $commentStructure"
      }
      .mkString("\n")
  def syntax: String =
    s"""|AssociatedComments(
        |  Leading =
        |${pretty(leadingMap)}
        |
        |  Trailing =
        |${pretty(trailingMap)}
        |)""".stripMargin

  override def toString: String = syntax
  def leading(tree: Tree): Set[Comment] =
    (for {
      token <- tree.tokens.headOption
      comments <- leadingMap.get(token)
    } yield comments).getOrElse(Nil).toSet

  def trailing(tree: Tree): Set[Comment] =
    (for {
      token <- tree.tokens.lastOption
      comments <- trailingMap.get(token)
    } yield comments).getOrElse(Nil).toSet

  def hasComment(tree: Tree): Boolean =
    trailing(tree).nonEmpty || leading(tree).nonEmpty
}

object AssociatedComments {

  def apply(tree: Tree): AssociatedComments = apply(tree.tokens)
  def apply(tokens: Tokens): AssociatedComments = {
    import scala.meta.tokens.Token._
    val leadingBuilder = Map.newBuilder[Token, List[Comment]]
    val trailingBuilder = Map.newBuilder[Token, List[Comment]]
    val leading = List.newBuilder[Comment]
    val trailing = List.newBuilder[Comment]
    var isLeading = true
    var lastToken: Token = tokens.head
    tokens.foreach {
      case c: Comment =>
        if (isLeading) leading += c
        else trailing += c
      case Token.LF() => isLeading = true
      case Token.EOF() =>
        val l = leading.result()
        val t = trailing.result()
        if (l.nonEmpty || t.nonEmpty) {
          trailingBuilder += lastToken -> (l ::: t)
        }
      case Trivia() =>
      case currentToken =>
        val t = trailing.result()
        if (t.nonEmpty) {
          trailingBuilder += lastToken -> t
          trailing.clear()
        }
        val l = leading.result()
        if (l.nonEmpty) {
          leadingBuilder += currentToken -> l
          leading.clear()
        }
        if (!currentToken.is[Comma]) {
          lastToken = currentToken
        }
        isLeading = false
    }
    new AssociatedComments(leadingBuilder.result(), trailingBuilder.result()) {}
  }
}
