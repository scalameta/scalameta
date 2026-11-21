package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Keeps the annotated method `f` and adds a method `f$fwd` before it that calls `f`, with a copy of
 * the parameters of `f`.
 */
class PrependForwarder extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro PrependForwarder.impl
}

object PrependForwarder {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(d: DefDef) = annottees
    val name = TermName(s"${d.name}$$fwd")
    val args = d.vparamss.map(_.map(p => q"${p.name}"))
    val fwd = q"def $name(...${d.vparamss.map(_.map(_.duplicate))}) = ${d.name}(...$args)"
    q"..${List(fwd, d)}"
  }
}
