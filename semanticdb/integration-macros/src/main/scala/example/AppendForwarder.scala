package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Keeps the annotated method `f` and adds a method `f$fwd` after it that calls `f`, with a copy of
 * the parameters of `f`.
 */
class AppendForwarder extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AppendForwarder.impl
}

object AppendForwarder {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(d: DefDef) = annottees
    val name = TermName(s"${d.name}$$fwd")
    val args = d.vparamss.map(_.map(p => q"${p.name}"))
    val fwd = q"def $name(...${d.vparamss.map(_.map(_.duplicate))}) = ${d.name}(...$args)"
    q"..${List(d, fwd)}"
  }
}
