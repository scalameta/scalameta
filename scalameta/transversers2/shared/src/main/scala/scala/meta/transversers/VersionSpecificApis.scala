package scala.meta
package transversers

private[meta] trait VersionSpecificApis {

  implicit class XtensionTreeTransversers(tree: Tree) {
    def transform(fn: PartialFunction[Tree, Tree]): Tree = {
      object transformer extends Transformer {
        override def apply(tree: Tree): Tree = super.apply(fn.applyOrElse(tree, identity[Tree]))
      }
      transformer(tree)
    }
  }

}
