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

  protected final def transformChild(tree: Tree): Tree = apply(transformNode(tree))

  final def transform(root: Tree): Tree = transformChild(root)

}
