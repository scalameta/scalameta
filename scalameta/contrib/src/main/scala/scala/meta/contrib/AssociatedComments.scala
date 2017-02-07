package scala.meta.contrib

import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens
import scala.meta.tokens.Token.Comment
import scala.collection.immutable.Seq

sealed abstract class AssociatedComments(leadingMap: Map[Token, Seq[Comment]],
                                         trailingMap: Map[Token, Seq[Comment]]) {
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
    val leadingBuilder = Map.newBuilder[Token, Seq[Comment]]
    val trailingBuilder = Map.newBuilder[Token, Seq[Comment]]
    val leading = Seq.newBuilder[Comment]
    val trailing = Seq.newBuilder[Comment]
    var isLeading = true
    var lastToken: Token = tokens.head
    tokens.foreach {
      case c: Comment =>
        if (isLeading) leading += c
        else trailing += c
      case Token.LF() => isLeading = true
      case Trivia() =>
      case currentToken =>
        val t = trailing.result()
        if (t.nonEmpty) {
          trailingBuilder += lastToken -> trailing.result()
          trailing.clear()
        }
        val l = leading.result()
        if (l.nonEmpty) {
          leadingBuilder += currentToken -> leading.result()
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
