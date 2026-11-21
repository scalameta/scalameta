package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Keeps the annotated class and adds a companion with an `apply` that has a copy of the type
 * parameters and the constructor parameters of the class, as a builder macro does.
 */
class CompanionApply extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CompanionApply.impl
}

object CompanionApply {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(cdef: ClassDef) = annottees
    val ctor = cdef.impl.body
      .collectFirst { case d: DefDef if d.name == termNames.CONSTRUCTOR => d }.get
    val tparams = cdef.tparams.map(_.duplicate)
    val targs = cdef.tparams.map(t => tq"${t.name}")
    val params = ctor.vparamss
      .map(_.map(p => treeCopy.ValDef(p, Modifiers(Flag.PARAM), p.name, p.tpt.duplicate, EmptyTree)))
    val args = ctor.vparamss.map(_.map(p => q"${p.name}"))
    val apply = q"def apply[..$tparams](...$params) = new ${cdef.name}[..$targs](...$args)"
    q"$cdef; object ${cdef.name.toTermName} { $apply }"
  }
}
