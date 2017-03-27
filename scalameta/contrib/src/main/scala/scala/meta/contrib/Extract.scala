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
