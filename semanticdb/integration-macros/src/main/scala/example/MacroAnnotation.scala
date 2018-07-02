package example

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class MacroAnnotation extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroAnnotation.impl
}

object MacroAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(cdef, mdef) = annottees
    val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =
      cdef
    val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" =
      mdef
    val stats1 = stats :+ q"def classNumber: Int = 42"
    val mstats1 = stats :+ q"def objectNumber: Int = 42"
    val cdef1 =
      q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
    val mdef1 =
      q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
    q"{$cdef1; $mdef1}"
  }
}
