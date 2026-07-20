package scala.meta
package transversers

/**
 * Base of the generated [[Transformer]].
 *
 * Override `transformNode` to map a node; `transform` applies it across the tree and rebuilds each
 * node through the generated (protected) `apply`, which pulls each transformed child via
 * `transformChild`.
 */
private[meta] abstract class TransformerBase {

  protected def transformNode(node: Tree): Tree = node

  protected def apply(tree: Tree): Tree

  // Return the next pre-computed child -- matched by position, so `tree` is ignored.
  // Reached with no pending children only if `apply` was invoked outside `transform`
  // (e.g. a subclass widened `apply` to public and called it): fail loudly rather
  // than fall into the recursive, overflow-prone path.
  protected final def transformChild(tree: Tree): Tree = {
    if (pendingChildren eq null)
      throw new IllegalStateException("Transformer.apply must not be called directly; use transform")
    val child = pendingChildren(pendingIndex)
    pendingIndex += 1
    child
  }

  // Stack-safe: applies `transformNode` to each node pre-order, then rebuilds bottom-up with an
  // explicit stack, so tree height is bounded by the heap rather than the call stack.
  final def transform(root: Tree): Tree = {
    var stack: List[Frame] = frameOf(root) :: Nil
    var ret: Tree = null
    while (stack.nonEmpty) {
      val frame = stack.head
      if (ret ne null) {
        frame(ret)
        ret = null
      }
      val childFrame = frame.nextChildFrame()
      if (childFrame ne null) stack = childFrame :: stack
      else {
        pendingChildren = frame.children
        pendingIndex = 0
        ret = apply(frame.node) // generated dispatch; pulls children via transformChild
        pendingChildren = null
        stack = stack.tail
      }
    }
    ret
  }

  // A node being processed. The constructor applies `transformNode`, then fills
  // `children` from the result via `foreachChild` (reusing `apply`/`cursor`) and
  // resets `cursor`. During the walk the same `apply` overwrites each slot in place
  // with its transformed child; once `cursor` reaches the end the node is rebuilt.
  private class Frame(val node: Tree) extends (Tree => Unit) {
    private var cursor = 0
    val children: Array[Tree] = {
      val n = node.childrenCount
      if (n == 0) TransformerBase.NoChildren else new Array[Tree](n)
    }
    def apply(child: Tree): Unit = {
      children(cursor) = child
      cursor += 1
    }
    node.foreachChild(this)
    cursor = 0
    def nextChildFrame(): Frame = if (cursor < children.length) frameOf(children(cursor)) else null
  }
  private def frameOf(node: Tree): Frame = new Frame(transformNode(node))

  // While `transform` rebuilds one node, holds that node's transformed children in
  // field order; `apply` reads them once each, in order, via `transformChild`.
  private var pendingChildren: Array[Tree] = null
  private var pendingIndex = 0

}

private object TransformerBase {
  private val NoChildren = new Array[Tree](0)
}
