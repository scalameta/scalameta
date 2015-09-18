package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class quasiquote[T](qname: scala.Symbol) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro QuasiquoteMacros.impl
}

class QuasiquoteMacros(val c: Context) {
  import c.universe._
  import Flag._
  val ReificationMacros = q"_root_.scala.meta.internal.quasiquotes.ReificationMacros"
  def impl(annottees: c.Tree*): c.Tree = {
    val q"new $_[..$qtypes](scala.Symbol(${qname: String})).macroTransform(..$_)" = c.macroApplication
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      if (stats.nonEmpty) c.abort(cdef.pos, "@quasiquote classes must have empty bodies")
      val qmodule = {
        val qtypesLub = lub(qtypes.map(_.duplicate).map(qtype => {
          try c.typecheck(qtype, c.TYPEmode).tpe
          catch { case c.TypecheckException(pos, msg) => c.abort(pos.asInstanceOf[c.Position], msg) }
        }))
        q"""
          object ${TermName(qname)} {
            import scala.language.experimental.macros
            def apply[T >: Any](args: T*)(implicit dialect: _root_.scala.meta.Dialect): $qtypesLub = macro $ReificationMacros.apply
            def unapply(scrutinee: Any)(implicit dialect: _root_.scala.meta.Dialect): Any = macro $ReificationMacros.unapply
          }
        """
      }
      val qparser = {
        val qunsafeResults = qtypes.map(qtype => q"""
          // TODO: Explicitly spell out the desugaring
          import _root_.scala.meta.parsers._
          input.parse[$qtype]
        """)
        val qsafeResults = qunsafeResults.map(qunsafeParser => q"_root_.scala.util.Try($qunsafeParser)")
        val gsafeResultsWithLogging = qsafeResults.map(qsafeResult => q"""
          $qsafeResult.recover({
            case ex: _root_.scala.Exception =>
              if (_root_.scala.sys.`package`.props("quasiquote.debug") != null) ex.printStackTrace()
              throw ex
          })
        """)
        var qsafeResult = gsafeResultsWithLogging.reduce((acc, curr) => q"$acc.orElse($curr)")
        val qparseResult = if (qunsafeResults.length == 1) qunsafeResults.head else q"$qsafeResult.get"
        q"private[meta] def parse(input: _root_.scala.meta.inputs.Input)(implicit dialect: _root_.scala.meta.Dialect) = $qparseResult"
      }
      val stats1 = stats :+ qmodule
      val mstats1 = mstats :+ qparser
      val cdef1 = q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      val mdef1 = q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
    val expanded = annottees match {
      case (cdef: ClassDef) :: (mdef: ModuleDef) :: rest if cdef.mods.hasFlag(IMPLICIT) => transform(cdef, mdef) ++ rest
      case (cdef: ClassDef) :: rest if cdef.mods.hasFlag(IMPLICIT) => transform(cdef, q"object ${cdef.name.toTermName}") ++ rest
      case annottee :: rest => c.abort(annottee.pos, "only implicit classes can be @quasiquote")
    }
    q"{ ..$expanded; () }"
  }
}

