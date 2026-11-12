package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Keeps the annotated class or object and rebuilds each of its methods from its parts. */
class RebuildDefs extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RebuildDefs.impl
}

object RebuildDefs {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    def rebuild(impl: Template) = treeCopy.Template(
      impl,
      impl.parents,
      impl.self,
      impl.body.map {
        case q"$mods def $name[..$tparams](...$paramss): $tpt = $rhs"
            if name != termNames.CONSTRUCTOR =>
          q"$mods def $name[..$tparams](...$paramss): $tpt = $rhs"
        case stat => stat
      },
    )
    val annottee = annottees.head match {
      case t: ClassDef => treeCopy.ClassDef(t, t.mods, t.name, t.tparams, rebuild(t.impl))
      case t: ModuleDef => treeCopy.ModuleDef(t, t.mods, t.name, rebuild(t.impl))
    }
    q"..${annottee +: annottees.tail}"
  }
}
