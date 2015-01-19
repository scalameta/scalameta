package org.scalameta.annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class quasiquote[T](qname: scala.Symbol) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro QuasiquoteMacros.impl
}

class QuasiquoteMacros(val c: Context) {
  import c.universe._
  import Flag._
  def impl(annottees: c.Tree*): c.Tree = {
    val q"new $_[..$qtypes](scala.Symbol(${qname: String})).macroTransform(..$_)" = c.macroApplication
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      if (stats.nonEmpty) c.abort(cdef.pos, "@quasiquote classes must have empty bodies")
      val qtypesLub = lub(qtypes.map(_.duplicate).map(qtype => c.typecheck(qtype, c.TYPEmode).tpe))
      val qmodule = q"""
        object ${TermName(qname)} {
          import scala.language.experimental.macros
          def apply[T](args: T*)(implicit dialect: _root_.scala.meta.Dialect): _root_.scala.meta.Tree = macro $mname.applyImpl
          def unapply(scrutinee: Any): $qtypesLub = macro ???
        }
      """
      val qunsafeResults = qtypes.map(qtype => q"_root_.scala.meta.syntactic.RichOrigin(s).parse[$qtype]")
      var qsafeResult = qunsafeResults.map(qunsafeParser => q"_root_.scala.util.Try($qunsafeParser)").reduce((acc, curr) => q"$acc.orElse($curr)")
      val qparseResult = if (qunsafeResults.length == 1) qunsafeResults.head else q"$qsafeResult.get"
      val qparser = q"(s: _root_.scala.Predef.String) => $qparseResult"
      val q"..$applyimpls" = q"""
        import scala.reflect.macros.whitebox.Context
        def applyImpl(c: Context)(args: c.Tree*)(dialect: c.Tree): c.Tree = {
          val helper = new _root_.scala.meta.syntactic.quasiquotes.Macros[c.type](c)
          implicit val dialectInstance: _root_.scala.meta.Dialect = {
            // We want to have a higher-order way to abstract over differences in dialects
            // and we're using implicits for that (implicits are values => values are higher-order => good).
            //
            // However, quasiquotes use macros, and macros are first-order, so we have a problem here.
            // Concretely, here we need to convert an implicit argument to a macro (the `dialect` tree)
            // into an instance of `Dialect` that we'll pass to the parser.
            //
            // TODO: For now I'll just prohibit quasiquotes for situations when `dialect` doesn't point to either Scala211 or Dotty.
            // A natural extension to this would be to allow any static value, not just predefined dialects.
            // Later on, we could further relax this restriction by doing parsing for a superset of all dialects and then
            // delaying validation of resulting ASTs until runtime.
            if (dialect.symbol == c.mirror.staticModule("_root_.scala.meta.dialects.Scala211")) _root_.scala.meta.dialects.Scala211
            else if (dialect.symbol == c.mirror.staticModule("_root_.scala.meta.dialects.Dotty")) _root_.scala.meta.dialects.Dotty
            else c.abort(c.enclosingPosition, "can't use the " + dialect + " dialect in quasiquotes")
          }
          helper.apply(c.macroApplication, $qparser)
        }
      """
      val cdef1 = q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..${qmodule +: stats} }"
      val mdef1 = q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..${mstats ++ applyimpls} }"
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

