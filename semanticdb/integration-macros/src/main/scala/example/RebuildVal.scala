package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Rebuilds the annotated `val` from its parts. */
class RebuildVal extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RebuildVal.impl
}

object RebuildVal {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(q"$mods val $name: $tpt = $rhs") = annottees
    q"$mods val $name: $tpt = $rhs"
  }
}
