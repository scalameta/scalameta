package example

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Expands to an anonymous class. scalac sets its position to the call. */
object AnonymousExpansion {
  def apply(): AnyRef = macro impl

  def impl(c: blackbox.Context)(): c.Tree = {
    import c.universe._
    q"new AnyRef { override def toString: String = super.toString }"
  }
}
