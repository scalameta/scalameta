package scala.meta
package internal
package trees

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "${T} is not an ast class and can't be used here.")
trait AstInfo[T <: Metadata.Ast] {
  def quasi(rank: Int, tree: Tree): T with Quasi
}
