package scala.meta
package transversers

private[meta] class SimpleTraverser {
  def apply(tree: Tree): Unit = tree.children.foreach(apply)
}
