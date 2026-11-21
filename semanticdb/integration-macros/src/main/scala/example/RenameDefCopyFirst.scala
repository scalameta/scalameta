package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** As `RenameDef`, but puts `f$impl` before the method `f` that calls it. */
class RenameDefCopyFirst extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RenameDefCopyFirst.impl
}

object RenameDefCopyFirst {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(d: DefDef) = annottees
    val name = TermName(s"${d.name}$$impl")
    val args = d.vparamss.map(_.map(p => q"${p.name}"))
    val params = d.vparamss.map(_.map(p => q"val ${p.name}: ${p.tpt.duplicate}"))
    val wrapper = q"def ${d.name}(...$params) = $name(...$args)"
    q"..${List(treeCopy.DefDef(d, d.mods, name, d.tparams, d.vparamss, d.tpt, d.rhs), wrapper)}"
  }
}
