package scala.meta
package transversers

class SimpleTraverser {
  def apply(tree: Tree): Unit = tree.children.foreach(apply)
}
