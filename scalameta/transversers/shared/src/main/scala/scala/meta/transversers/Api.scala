package scala.meta
package transversers

private[meta] trait Api extends VersionSpecificApis {
  implicit class XtensionCollectionLikeUI(tree: Tree) {
    // legacy calls
    def traverse(f: PartialFunction[Tree, Unit]): Unit = tree.dfs(f.applyOrElse(_, (_: Tree) => ()))
    def collect[A](f: PartialFunction[Tree, A]): List[A] = tree.dfsCollect(f)
  }

  implicit class XtensionTreeLike[T <: Tree](tree: T) {
    private[meta] def withOriginRecursive(origin: trees.Origin): T = {
      tree.bfs(_.privateSetOrigin(origin))
      tree
    }

    def withDialectIfRootAndNotSet(implicit dialect: Dialect): T =
      if (tree.parent.nonEmpty) tree // must set on root
      else if (tree.origin ne trees.Origin.None) tree // only if not set
      else withOriginRecursive(trees.Origin.DialectOnly(dialect))
  }

  implicit class XtensionTreeTraversalOps(tree: Tree) {

    /**
     * Traverse the tree applying the visitor function.
     * @param f
     *   if function returns false, skip subtree
     */
    def bfsIf(f: Tree => Boolean): Unit = {
      val buf = new collection.mutable.Queue[Tree]
      buf += tree
      while (buf.nonEmpty) {
        val elem = buf.dequeue()
        if (f(elem)) buf ++= elem.children
      }
    }

    /**
     * Traverse the tree using breadth-first-search applying the predicate function.
     * @param f
     *   if predicate returns true, stop traversing and return the triggering tree
     */
    def bfsFind(f: Tree => Boolean): Option[Tree] = {
      val buf = new collection.mutable.Queue[Tree]
      buf += tree
      while (buf.nonEmpty) {
        val elem = buf.dequeue()
        if (f(elem)) return Some(elem)
        buf ++= elem.children
      }
      None
    }

    /**
     * Traverse the tree using depth-first-search applying the visitor function.
     * @param f
     *   if function returns false, skip subtree
     */
    def dfsIf(f: Tree => Boolean): Unit = {
      val buf = new collection.mutable.ListBuffer[Tree]
      buf += tree
      while (buf.nonEmpty) {
        val elem = buf.remove(0)
        if (f(elem)) buf.prependAll(elem.children)
      }
    }

    /**
     * Traverse the tree using depth-first-search applying the predicate function.
     * @param f
     *   if predicate returns true, stop traversing and return the triggering tree
     */
    def dfsFind(f: Tree => Boolean): Option[Tree] = {
      val buf = new collection.mutable.ListBuffer[Tree]
      buf += tree
      while (buf.nonEmpty) {
        val elem = buf.remove(0)
        if (f(elem)) return Some(elem)
        buf.prependAll(elem.children)
      }
      None
    }

    /**
     * Traverse the tree using breadth-first-search applying the visitor function.
     * @param f
     *   if function doesn't match a tree, skip subtree
     */
    def bfsIf(f: PartialFunction[Tree, Unit]): Unit = bfsIf(f.runWith(_ => ()))

    /**
     * Traverse the tree using depth-first-search applying the visitor function.
     * @param f
     *   if function doesn't match a tree, skip subtree
     */
    def dfsIf(f: PartialFunction[Tree, Unit]): Unit = dfsIf(f.runWith(_ => ()))

    /**
     * Traverse the tree using breadth-first-search applying the visitor function.
     */
    def bfs(f: Tree => Unit): Unit = bfsIf { x =>
      f(x)
      true
    }

    /**
     * Traverse the tree using depth-first-search applying the visitor function.
     */
    def dfs(f: Tree => Unit): Unit = dfsIf { x =>
      f(x)
      true
    }

    def bfsExists(f: Tree => Boolean): Boolean = bfsFind(f).isDefined
    def dfsExists(f: Tree => Boolean): Boolean = dfsFind(f).isDefined

    def bfsForall(f: Tree => Boolean): Boolean = !bfsExists(!f(_))
    def dfsForall(f: Tree => Boolean): Boolean = !dfsExists(!f(_))

    def bfsCollectFirst[A](f: PartialFunction[Tree, A]): Option[A] = {
      var res = Option.empty[A]
      bfsFind(f.runWith(x => res = Some(x)))
      res
    }
    def dfsCollectFirst[A](f: PartialFunction[Tree, A]): Option[A] = {
      var res = Option.empty[A]
      dfsFind(f.runWith(x => res = Some(x)))
      res
    }

    def bfsCollect[A](f: Tree => Option[A]): List[A] = {
      val buf = List.newBuilder[A]
      bfs(f(_).foreach(buf.+=))
      buf.result()
    }
    def dfsCollect[A](f: Tree => Option[A]): List[A] = {
      val buf = List.newBuilder[A]
      dfs(f(_).foreach(buf.+=))
      buf.result()
    }

    def bfsCollect[A](f: PartialFunction[Tree, A]): List[A] = bfsCollect(f.lift)
    def dfsCollect[A](f: PartialFunction[Tree, A]): List[A] = dfsCollect(f.lift)

    def bfsCollectEach[A](f: Tree => Iterable[A]): List[A] = {
      val buf = List.newBuilder[A]
      bfs(buf ++= f(_))
      buf.result()
    }
    def dfsCollectEach[A](f: Tree => Iterable[A]): List[A] = {
      val buf = List.newBuilder[A]
      dfs(buf ++= f(_))
      buf.result()
    }

    def bfsCollectEach[A](f: PartialFunction[Tree, Iterable[A]]): List[A] =
      bfsCollectEach(f.applyOrElse(_, (_: Tree) => Nil))
    def dfsCollectEach[A](f: PartialFunction[Tree, Iterable[A]]): List[A] =
      dfsCollectEach(f.applyOrElse(_, (_: Tree) => Nil))

  }

}
