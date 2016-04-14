package scala.meta

package object tests {
  // Resets tokens in a given tree and possibly all its subtrees,
  // forcing the AST infrastructure to reinfer tokens on demand.
  implicit class XtensionResetTokens[T <: Tree](tree: T) {
    def resetTokens: T = {
      tree.withTokens(null).asInstanceOf[T]
    }
    def resetAllTokens: T = {
      tree.transform{ case tree: Tree => tree.resetTokens }.asInstanceOf[T]
    }
  }
}