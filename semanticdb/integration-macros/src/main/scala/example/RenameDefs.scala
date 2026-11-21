package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Keeps the annotated class, renames each of its methods `f` to `f$impl` and adds a method `f` that
 * calls it, as a memoizing macro does.
 */
class RenameDefs extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RenameDefs.impl
}

object RenameDefs {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val (t: ClassDef) +: companion = annottees
    val body = t.impl.body.flatMap {
      case d: DefDef if d.name != termNames.CONSTRUCTOR =>
        val name = TermName(s"${d.name}$$impl")
        val args = d.vparamss.map(_.map(p => q"${p.name}"))
        val params = d.vparamss.map(_.map(p => q"val ${p.name}: ${p.tpt.duplicate}"))
        val wrapper = q"def ${d.name}(...$params) = $name(...$args)"
        List(wrapper, treeCopy.DefDef(d, d.mods, name, d.tparams, d.vparamss, d.tpt, d.rhs))
      case stat => List(stat)
    }
    val impl = treeCopy.Template(t.impl, t.impl.parents, t.impl.self, body)
    q"..${treeCopy.ClassDef(t, t.mods, t.name, t.tparams, impl) +: companion}"
  }
}
