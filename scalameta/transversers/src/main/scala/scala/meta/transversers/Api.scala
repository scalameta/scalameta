package scala.meta
package transversers

private[meta] trait Api {
  implicit class XtensionCollectionLikeUI(tree: Tree) {
    def transform(fn: PartialFunction[Tree, Tree]): Tree = ???
    def traverse(fn: PartialFunction[Tree, Unit]): Tree = ???
    def collect[T](fn: PartialFunction[Tree, T]): List[T] = ???
  }
}

private[meta] trait Aliases {
  type Transformer = scala.meta.transversers.Transformer
  // val Transformer = scala.meta.transversers.Transformer

  type Traverser = scala.meta.transversers.Traverser
  // val Traverser = scala.meta.transversers.Traverser
}
