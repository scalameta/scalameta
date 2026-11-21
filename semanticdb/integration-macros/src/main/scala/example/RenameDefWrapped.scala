package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * As `RenameDef`, but the macro puts the written body in a block in `f$impl`, as a tracing macro
 * does.
 */
class RenameDefWrapped extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RenameDefWrapped.impl
}

object RenameDefWrapped {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(d: DefDef) = annottees
    val name = TermName(s"${d.name}$$impl")
    val args = d.vparamss.map(_.map(p => q"${p.name}"))
    val params = d.vparamss.map(_.map(p => q"val ${p.name}: ${p.tpt.duplicate}"))
    val wrapper = q"def ${d.name}(...$params) = $name(...$args)"
    val rhs = q"{ println(${d.name.toString}); ${d.rhs} }"
    q"..${List(wrapper, treeCopy.DefDef(d, d.mods, name, d.tparams, d.vparamss, d.tpt, rhs))}"
  }
}
