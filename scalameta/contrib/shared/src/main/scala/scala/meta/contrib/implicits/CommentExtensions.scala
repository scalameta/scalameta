package scala.meta.contrib.implicits

import scala.meta.contrib.{CommentOps, DocToken}
import scala.meta.tokens.Token.Comment

trait CommentExtensions {

  implicit class XtensionCommentOps(c: Comment) {

    @inline
    def isScaladoc: Boolean = CommentOps.isScaladoc(c)

    @inline
    def content: Option[String] = CommentOps.content(c)

    @inline
    def docTokens: Option[List[DocToken]] = CommentOps.docTokens(c)
  }

}

object CommentExtensions extends CommentExtensions
