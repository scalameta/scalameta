package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Keeps the annotated class and adds an overload `(i: Int): Int` of each method before its members.
 */
class PrependOverloads extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro PrependOverloads.impl
}

object PrependOverloads {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val (t: ClassDef) +: companion = annottees
    val body = t.impl.body
    // scalac expects the constructor before the members
    val (ctor, members) = body.splitAt(body.indexWhere {
      case d: DefDef => d.name == termNames.CONSTRUCTOR
      case _ => false
    } + 1)
    val overloads = members.collect { case d: DefDef => q"def ${d.name}(i: Int): Int = 2" }
    val impl = treeCopy.Template(t.impl, t.impl.parents, t.impl.self, ctor ++ overloads ++ members)
    q"..${treeCopy.ClassDef(t, t.mods, t.name, t.tparams, impl) +: companion}"
  }
}
