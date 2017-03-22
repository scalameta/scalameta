package scala.meta
package internal

import scala.meta.internal.ast.Metadata.Ast

package object ast {
  def astInfo[T <: Ast : AstInfo]: AstInfo[T] = implicitly[AstInfo[T]]
}