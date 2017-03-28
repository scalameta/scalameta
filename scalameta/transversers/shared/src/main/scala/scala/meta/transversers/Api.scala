package scala.meta
package transversers

private[meta] trait Api {
  implicit class XtensionCollectionLikeUI(tree: Tree) {
    def transform(fn: PartialFunction[Tree, Tree]): Tree = {
      object transformer extends Transformer {
        override def apply(tree: Tree): Tree = {
          if (fn.isDefinedAt(tree)) super.apply(fn(tree))
          else super.apply(tree)
        }
      }
      transformer(tree)
    }

    def traverse(fn: PartialFunction[Tree, Unit]): Unit = {
      object traverser extends Traverser {
        override def apply(tree: Tree): Unit = {
          if (fn.isDefinedAt(tree)) fn(tree)
          super.apply(tree)
        }
      }
      traverser(tree)
    }

    def collect[T](fn: PartialFunction[Tree, T]): List[T] = {
      val buf = scala.collection.mutable.ListBuffer[T]()
      object traverser extends Traverser {
        override def apply(tree: Tree): Unit = {
          if (fn.isDefinedAt(tree)) buf += fn(tree)
          super.apply(tree)
        }
      }
      traverser(tree)
      buf.toList
    }
  }
}

private[meta] trait Aliases {
  type Transformer = scala.meta.transversers.Transformer
  // there's no term Transformer, so we don't have a term alias here

  type Traverser = scala.meta.transversers.Traverser
  // there's no term Traverser, so we don't have a term alias here
}
