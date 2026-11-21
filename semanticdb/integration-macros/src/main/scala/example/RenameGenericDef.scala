package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** As `RenameDef`, for a method with type parameters: the macro copies them into the method `f`. */
class RenameGenericDef extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RenameGenericDef.impl
}

object RenameGenericDef {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(d: DefDef) = annottees
    val name = TermName(s"${d.name}$$impl")
    val targs = d.tparams.map(t => tq"${t.name}")
    val args = d.vparamss.map(_.map(p => q"${p.name}"))
    val params = d.vparamss.map(_.map(p => q"val ${p.name}: ${p.tpt.duplicate}"))
    val tparams = d.tparams.map(_.duplicate)
    val wrapper = q"def ${d.name}[..$tparams](...$params) = $name[..$targs](...$args)"
    q"..${List(wrapper, treeCopy.DefDef(d, d.mods, name, d.tparams, d.vparamss, d.tpt, d.rhs))}"
  }
}
