package scala.meta
package transversers

private[meta] trait Api {
  implicit class XtensionCollectionLikeUI(tree: Tree) {
    def transform(fn: PartialFunction[Tree, Tree]): Tree = {
      object transformer extends Transformer {
        override def apply(tree: Tree): Tree = {
          if (fn.isDefinedAt(tree)) {
            // NOTE: I've been thinking hard about
            // who should be linked to who with TransformedTokens in this case.
            //
            // By the virtue of @transformer codegen, super.apply will link tree2 to tree1.
            // But should we link tree1 to tree?
            // On the one hand, yes, because we kinda want to link tree2 to tree, after all.
            // On the other hand, so far we've only installed TransformedTokens after
            // a copy-like transformation, i.e. in Tree.copy and super.apply.
            //
            // My current thinking is that we shouldn't link tree1 to tree, because
            // that'll actually do damage if the trees are unrelated, e.g. in a
            // ClassDef -[fn]-> ModuleDef transformation. If the user wants linking,
            // they can do it themselves. Not very friendly, but at least not harmful.
            //
            // This begs for a more principled way of transforming tokens.
            // I've summarized my further thoughts on the matter in #149.
            val tree1 = fn(tree)
            val tree2 = super.apply(tree1)
            tree2
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
