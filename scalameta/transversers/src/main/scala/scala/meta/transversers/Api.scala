package scala.meta
package transversers

private[meta] trait Api {
  implicit class XtensionCollectionLikeUI(tree: Tree) {
    def transform(fn: PartialFunction[Tree, Tree]): Tree = {
      object transformer extends Transformer {
        override def apply(tree: Tree): Tree = {
          if (fn.isDefinedAt(tree)) {
            // TODO: Who should be tree2's prototype?
            // Is it `tree`, the untransformed original?
            // Or is it `tree1`, the a little bit transformed original?
            // Note that `super.apply(tree1)` may further transform the tree.
            // I've no idea what's the right choice, so I'm going with `tree`,
            // and later we can revise this decision.
            val tree1 = fn(tree)
            val tree2 = super.apply(tree1)
            if (tree eq tree2) tree
            else tree2.withTokens(scala.meta.internal.tokens.TransformedTokens(tree))
          } else {
            super.apply(tree)
          }
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
  // val Transformer = scala.meta.transversers.Transformer

  type Traverser = scala.meta.transversers.Traverser
  // val Traverser = scala.meta.transversers.Traverser
}
