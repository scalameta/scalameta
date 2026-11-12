package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Returns the class as written and adds `lens` to its companion, as circe and Monocle do. */
class KeepClass extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro KeepClass.impl
}

object KeepClass {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    annottees match {
      case Seq(cdef: ClassDef) => q"$cdef; object ${cdef.name.toTermName} { def lens: Int = 42 }"
      case Seq(cdef: ClassDef, mdef: ModuleDef) =>
        val q"$mmods object $mname extends { ..$mearly } with ..$mparents { $mself => ..$mstats }" =
          mdef
        val mstats1 = mstats :+ q"def lens: Int = 42"
        q"$cdef; $mmods object $mname extends { ..$mearly } with ..$mparents { $mself => ..$mstats1 }"
    }
  }
}
