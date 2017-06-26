package scala.meta.contrib

/**
  * The motivation for this typeclass is to extract values out of trees
  * that could have been fields on the classes. However, these values are
  * not included as fields on the tree nodes because of various reasons.
  */
trait Extract[A, B] {
  def extract(a: A): List[B]
}

object Extract {
  def apply[A, B](f: A => List[B]): Extract[A, B] = new Extract[A, B] {
    @inline override def extract(a: A): List[B] = f(a)
  }
}
