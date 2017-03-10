package scala.meta.contrib

/**
  * Logical extraction of B from A.
  *
  * Meaning that this is supposed to replicate what the use thinks should happen.
  * Not the actual class representation
  *
  * eg. Extract[Defn.Class, Seq[Stat]]
  *
  * is actually extracting the stats from the Template, which is a child of Defn.Class.
  */
trait Extract[A, B] {
  def extract(a: A): B
}

object Extract {
  def apply[A, B](f: A => B): Extract[A, B] = new Extract[A, B] {
    @inline override def extract(a: A): B = f(a)
  }
}
