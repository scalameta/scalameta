package scala.meta

package object tests {
  // Resets tokens in a given tree and possibly all its subtrees,
  // forcing the AST infrastructure to reinfer tokens on demand.
  implicit class XtensionResetTokens(tree: Tree) {
    def resetTokens: Tree = {
      tree.withTokens(null)
    }
    def resetAllTokens: Tree = {
      tree.transform { case tree: Tree => tree.resetTokens }
    }
  }
}