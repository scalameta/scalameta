package scala.meta
package internal

package object flags {
  implicit class XtensionFlags[T <: Tree](tree: T) {
    def isTypechecked: Boolean = (tree.flags & TYPECHECKED) == TYPECHECKED
    def setTypechecked: T = tree.withFlags(tree.flags | TYPECHECKED).asInstanceOf[T]
    def resetTypechecked: T = tree.withFlags(tree.flags & ~TYPECHECKED).asInstanceOf[T]
    def withTypechecked(value: Boolean): T = if (value) tree.setTypechecked else tree.resetTypechecked
  }

  type Flags = Int
  final val ZERO: Flags = 0x0
  final val TYPECHECKED: Flags = 0x1
}