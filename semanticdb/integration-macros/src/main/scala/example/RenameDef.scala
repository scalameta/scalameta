package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Renames the annotated method `f` to `f$impl` and adds a method `f` that calls it, as a memoizing
 * macro does.
 */
class RenameDef extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RenameDef.impl
}

object RenameDef {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(d: DefDef) = annottees
    val name = TermName(s"${d.name}$$impl")
    val args = d.vparamss.map(_.map(p => q"${p.name}"))
    val params = d.vparamss.map(_.map(p => q"val ${p.name}: ${p.tpt.duplicate}"))
    val wrapper = q"def ${d.name}(...$params) = $name(...$args)"
    q"..${List(wrapper, treeCopy.DefDef(d, d.mods, name, d.tparams, d.vparamss, d.tpt, d.rhs))}"
  }
}
