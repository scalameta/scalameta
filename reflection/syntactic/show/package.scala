package scala.reflect
package syntactic

import org.scalareflect.show._
import scala.reflect.core._
import scala.language.higherKinds

package object show {
  trait Code[T] extends Show[T]
  object Code {
    implicit def tree[T <: Tree]: Code[T] = new Code[T] { def apply(tree: T): Show.Result = ShowCode.showTree(tree) }
    // TODO: populate this with other implicits from ShowCode
  }

  trait Raw[T] extends Show[T]
  object Raw {
    implicit def tree[T <: Tree]: Raw[T] = new Raw[T] { def apply(tree: T): Show.Result = ShowRaw.showTree(tree) }
    // TODO: populate this with other implicits from ShowRaw
  }

  implicit class ShowOps[T](val x: T) extends AnyVal {
    def show[Style[T] <: Show[T]](implicit style: Style[T]): String = style(x).toString
  }
}
