package scala.reflect
package syntactic

import org.scalareflect.convert._
import scala.reflect.core._

package object show {
  trait Style[T] extends Convert[Tree, String]

  trait Code
  object Code {
    implicit val showCode: Style[Code] = new Style[Code] { def apply(tree: Tree): String = ShowCode.showTree(tree).toString }
  }

  trait Raw
  object Raw {
    implicit val showRaw: Style[Raw] = new Style[Raw] { def apply(tree: Tree): String = ShowRaw.showTree(tree).toString }
  }

  implicit class ShowOps(val tree: Tree) extends AnyVal {
    def show[T](implicit ev: Style[T]): String = ev(tree)
  }
}
