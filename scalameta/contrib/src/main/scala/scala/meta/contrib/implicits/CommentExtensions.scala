package scala.meta.contrib.implicits

import scala.meta.contrib.{CommentOps, DocToken}
import scala.meta.tokens.Token.Comment

trait CommentExtensions {

  implicit class XtensionCommentOps(c: Comment) {

    @inline
    def isScaladoc: Boolean = CommentOps.isScaladoc(c)

    @inline
    def content: String = CommentOps.content(c)

    @inline
    def docTokens: Seq[DocToken] = CommentOps.docTokens(c)
  }

}

object CommentExtensions extends CommentExtensions
