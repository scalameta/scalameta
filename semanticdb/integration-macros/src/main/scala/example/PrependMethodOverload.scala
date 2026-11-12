package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Keeps the annotated method and adds an overload `(s: Int): Int` before it. */
class PrependMethodOverload extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro PrependMethodOverload.impl
}

object PrependMethodOverload {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(d: DefDef) = annottees
    q"def ${d.name}(s: Int): Int = 2; $d"
  }
}
