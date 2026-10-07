package scala.meta
package transversers

class SimpleTraverser {
  def apply(tree: Tree): Unit = tree.foreachChild(apply)
}
