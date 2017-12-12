package scala.meta
package transversers

private[meta] trait Api {

  sealed trait TraversalOrder
  case object LeafFirst extends TraversalOrder
  case object RootFirst extends TraversalOrder

  implicit val order: TraversalOrder = RootFirst

  implicit class XtensionCollectionLikeUI(tree: Tree) {
    def transform(fn: PartialFunction[Tree, Tree])(implicit order: TraversalOrder): Tree = {
      object transformer extends Transformer {
        override def apply(tree: Tree): Tree = {
          order match {
            case RootFirst =>
              if (fn.isDefinedAt(tree)) super.apply(fn(tree))
              else super.apply(tree)
            case LeafFirst  =>
              if (fn.isDefinedAt(tree)) fn(super.apply(tree))
              else super.apply(tree)
          }
        }
      }
      transformer(tree)
    }

    def traverse(fn: PartialFunction[Tree, Unit])(implicit order: TraversalOrder): Unit = {
      object traverser extends Traverser {
        override def apply(tree: Tree): Unit = {
          order match {
            case RootFirst =>
              if (fn.isDefinedAt(tree)) fn(tree)
              super.apply(tree)
            case LeafFirst =>
              super.apply(tree)
              if (fn.isDefinedAt(tree)) fn(tree)
          }
        }
      }
      traverser(tree)
    }

    def collect[T](fn: PartialFunction[Tree, T])(implicit order: TraversalOrder): List[T] = {
      val buf = scala.collection.mutable.ListBuffer[T]()
      object traverser extends Traverser {
        override def apply(tree: Tree): Unit = {
          order match {
            case RootFirst =>
              if (fn.isDefinedAt(tree)) buf += fn(tree)
              super.apply(tree)
            case LeafFirst =>
              super.apply(tree)
              if (fn.isDefinedAt(tree)) buf += fn(tree)
          }
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
