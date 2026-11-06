package example

object ExistentialResult {
  def lib[U](u: U): Set[A1] forSome { type A1 <: U } = Set.empty
}
