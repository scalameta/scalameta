package scala.meta
package internal
package trees

import org.scalameta.internal.MacroHelpers

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

// Generates quasiquote definition boilerplate for a given interpolator
// and then injects it into the annottee.
// See usage examples in the `quasiquotes` project.
class quasiquote[T](qname: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro QuasiquoteMacros.impl
}

class QuasiquoteMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  val ReificationMacros = q"_root_.scala.meta.internal.quasiquotes.ReificationMacros"
  def impl(annottees: c.Tree*): c.Tree = annottees.transformAnnottees(new ImplTransformer {
    val q"new $_[..$qtypes](${qname: String}).macroTransform(..$_)" = c.macroApplication
    override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =
        cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" =
        mdef
      val mmods1 = Modifiers(mmods.flags, TypeName("meta"), mmods.annotations)
      if (stats.nonEmpty) c.abort(cdef.pos, "@quasiquote classes must have empty bodies")
      val qmodule = {
        val qtypesLub = lub(qtypes.map(_.duplicate).map { qtype =>
          try c.typecheck(qtype, c.TYPEmode).tpe
          catch {
            case c.TypecheckException(pos, msg) => c.abort(pos.asInstanceOf[c.Position], msg)
          }
        })
        q"""
          object ${TermName(qname)} {
            import scala.language.experimental.macros
            def apply[T >: Any](args: T*)(implicit dialect: _root_.scala.meta.Dialect): $qtypesLub = macro $ReificationMacros.apply
            def unapply(scrutinee: Any)(implicit dialect: _root_.scala.meta.Dialect): Any = macro $ReificationMacros.unapply
          }
        """
      }
      val qparser = {
        val qmonadicResults = qtypes.map(qtype =>
          q"""
          type Parse[T] = _root_.scala.meta.parsers.Parse[T]
          val parse = _root_.scala.Predef.implicitly[Parse[$qtype]]
          parse(input, dialect)
        """
        )
        val qmonadicResult = qmonadicResults.reduce((acc, curr) => q"$acc.orElse($curr)")
        val qresult = q"""
          $qmonadicResult match {
            case x: _root_.scala.meta.parsers.Parsed.Success[_] => x.tree
            case x: _root_.scala.meta.parsers.Parsed.Error => throw x.details
          }
        """
        q"private[meta] def parse(input: _root_.scala.meta.inputs.Input, dialect: _root_.scala.meta.Dialect) = $qresult"
      }
      val stats1 = stats :+ qmodule
      val mstats1 = mstats :+ qparser
      val cdef1 =
        q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      val mdef1 =
        q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
  })
}
