package scala.meta
package transversers

private[meta] trait Api {
  implicit class XtensionCollectionLikeUI(tree: Tree) {
    def transform(fn: PartialFunction[Tree, Tree]): Tree = {
      val liftedFn = fn.lift
      object transformer extends Transformer {
        override def apply(tree: Tree): Tree = super.apply(liftedFn(tree).getOrElse(tree))
      }
      transformer(tree)
    }

    def traverse(fn: PartialFunction[Tree, Unit]): Unit = {
      val liftedFn = fn.lift
      object traverser extends SimpleTraverser {
        override def apply(tree: Tree): Unit = {
          liftedFn(tree)
          super.apply(tree)
        }
      }
      traverser(tree)
    }

    def collect[T](fn: PartialFunction[Tree, T]): List[T] = {
      val liftedFn = fn.lift
      val buf = scala.collection.mutable.ListBuffer[T]()
      object traverser extends SimpleTraverser {
        override def apply(tree: Tree): Unit = {
          liftedFn(tree).foreach(buf += _)
          super.apply(tree)
        }
      }
      traverser(tree)
      buf.toList
    }
  }

  implicit class XtensionTreeLike[T <: Tree](tree: T) {
    private[meta] def withOriginRecursive(origin: trees.Origin): T = tree
      .transform { case t: Tree => t.withOrigin(origin) }.withOrigin(origin).asInstanceOf[T]

    def withDialectIfRootAndNotSet(implicit dialect: Dialect): T =
      if (tree.privateParent ne null) tree // must set on root
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
