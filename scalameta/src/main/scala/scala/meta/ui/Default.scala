package scala.meta
package internal
package ui

object show {
  def apply(tree: Tree) = {
    // TODO: I've no idea why it's necessary to explicitly write `package` here
    // otherwise I'm getting compilation errors and that merits an investigation
    import scala.meta.ui.`package`.ShowOps
    import scala.meta.ui.Code
    import scala.meta.dialects.`package`.Scala211
    tree.show[Code] // we can't parameterize toString, so we default to Scala prettyprinting
  }
}
