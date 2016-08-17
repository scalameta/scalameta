import org.scalameta.roles._

class RoleSuite {
  trait Tree
  trait Subtree extends Tree

  val tree: Tree = ???
  val subtree: Subtree = ???
  val x11: Boolean = tree.is(TermLoc)
  val x12: org.scalameta.roles.internal.Dummy.type = tree.get(TermLoc)
  val x13: Tree = tree.set(TermLoc)
  val x21: Boolean = subtree.is(TermLoc)
  val x22: org.scalameta.roles.internal.Dummy.type = subtree.get(TermLoc)
  val x23: Subtree = subtree.set(TermLoc)

  trait Location {
    def test(tree: Tree): Boolean = ???
    def mark[U <: Tree](tree: U): U = ???
  }
  @role private object TermLoc extends Location with Role[Tree]

  val other: Tree = ???
  val subother: Subtree = ???
  val y11: Boolean = other.is(FooRole)
  private val y12: FooRole = other.get(FooRole)
  val y13: Tree = other.set(FooRole(42))
  val y21: Boolean = subother.is(FooRole)
  private val y22: FooRole = subother.get(FooRole)
  val y23: Subtree = subother.set(FooRole(42))

  @role private class FooRole(x: Int) extends Role[Tree]
  object FooRole {
    def get(x: Tree): Option[FooRole] = ???
    def set[U <: Tree](x: U, r: FooRole): U = ???
  }
}
