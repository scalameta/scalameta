package scala.meta
package internal
package ui

object show {
  def apply(tree: Tree) = {
    import scala.meta.ui.`package`.ShowOps
    import scala.meta.ui.Code
    import scala.meta.dialects.Scala211
    tree.show[Code] // we can't parameterize toString, so we default to Scala prettyprinting
  }
}
