package example

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object AnonymousWithoutPosition {
  def apply(): AnyRef = macro impl

  // typecheck before scalac positions the expansion, so the anonymous class symbol has no position
  def impl(c: blackbox.Context)(): c.Tree = {
    import c.universe._
    c.typecheck(q"new AnyRef { override def toString: String = super.toString }")
  }
}
