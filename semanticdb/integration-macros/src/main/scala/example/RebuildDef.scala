package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Rebuilds the annotated method from its parts, as a tracing macro does. */
class RebuildDef extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RebuildDef.impl
}

object RebuildDef {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(q"$mods def $name[..$tparams](...$paramss): $tpt = $rhs") = annottees
    q"$mods def $name[..$tparams](...$paramss): $tpt = { println(${name.toString}); $rhs }"
  }
}
