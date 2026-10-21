package example

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object SelfTypeWithoutPosition {
  def apply(): AnyRef = macro impl

  // typecheck before scalac positions the expansion, so the original of the self type has no position
  def impl(c: blackbox.Context)(): c.Tree = {
    import c.universe._
    c.typecheck(q"new Serializable { self: Serializable => }")
  }
}
