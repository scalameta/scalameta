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
  protected final def transformChild(tree: Tree): Tree =
    if (pendingChildren ne null) pendingChildren.next()
    else
      throw new IllegalStateException("Transformer.apply must not be called directly; use transform")

  // Stack-safe: applies `transformNode` to each node pre-order, then rebuilds bottom-up with an
  // explicit stack, so tree height is bounded by the heap rather than the call stack.
  final def transform(root: Tree): Tree = {
    // A node being processed: `transformNode` already applied; its children are
    // consumed left to right, their transformed results accumulated, then the node
    // is rebuilt once `remaining` is empty.
    final class Frame(val node: Tree) {
      var remaining: List[Tree] = node.children
      val results = List.newBuilder[Tree]
    }
    def frameOf(tree: Tree): Frame = new Frame(transformNode(tree))

    var stack: List[Frame] = frameOf(root) :: Nil
    var ret: Tree = null
    while (stack.nonEmpty) {
      val frame = stack.head
      if (ret ne null) {
        frame.results += ret
        ret = null
      }
      frame.remaining match {
        case child :: rest =>
          frame.remaining = rest
          stack = frameOf(child) :: stack
        case _ =>
          pendingChildren = frame.results.result().iterator
          ret = apply(frame.node) // generated dispatch; pulls children via transformChild
          pendingChildren = null
          stack = stack.tail
      }
    }
    ret
  }

  // While `transform` rebuilds one node, holds that node's transformed children in
  // field order; null otherwise. `apply` calls `transformChild` once per child in
  // that same order, so they line up by position.
  private var pendingChildren: Iterator[Tree] = null

}
