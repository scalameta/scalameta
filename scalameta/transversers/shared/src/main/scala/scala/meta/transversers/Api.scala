package scala.meta
package transversers

private[meta] trait Api {
  implicit class XtensionCollectionLikeUI(tree: Tree) {
    def transform(fn: PartialFunction[Tree, Tree]): Tree = {
      object transformer extends Transformer {
        override def apply(tree: Tree): Tree = super.apply(fn.applyOrElse(tree, identity[Tree]))
      }
      transformer(tree)
    }

    def traverse(fn: PartialFunction[Tree, Unit]): Unit = {
      object traverser extends SimpleTraverser {
        override def apply(tree: Tree): Unit = {
          fn.applyOrElse(tree, (_: Tree) => ())
          super.apply(tree)
        }
      }
      traverser(tree)
    }

    def collect[T](fn: PartialFunction[Tree, T]): List[T] = {
      val buf = scala.collection.mutable.ListBuffer[T]()
      object traverser extends SimpleTraverser {
        override def apply(tree: Tree): Unit = {
          fn.runWith(buf.+=)(tree)
          super.apply(tree)
        }
      }
      traverser(tree)
      buf.toList
    }
  }

  implicit class XtensionTreeLike[T <: Tree](tree: T) {
    private[meta] def withOriginRecursive(origin: trees.Origin): T = {
      tree.traverse { case t: Tree => t.privateSetOrigin(origin) }
      tree.privateSetOrigin(origin)
      tree
    }

    def withDialectIfRootAndNotSet(implicit dialect: Dialect): T =
      if (tree.parent.nonEmpty) tree // must set on root
      else if (tree.origin ne trees.Origin.None) tree // only if not set
      else withOriginRecursive(trees.Origin.DialectOnly(dialect))
  }
}

private[meta] trait Aliases {
  type Transformer = scala.meta.transversers.Transformer
  // there's no term Transformer, so we don't have a term alias here

  type Traverser = scala.meta.transversers.Traverser
  // there's no term Traverser, so we don't have a term alias here
}
