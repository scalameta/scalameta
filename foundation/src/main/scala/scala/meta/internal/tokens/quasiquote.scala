package scala.meta
package internal
package tokens

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import org.scalameta.internal.MacroHelpers

// Generates quasiquote definition boilerplate for a given interpolator
// and then injects it into the annottee.
// See usage examples in the `tokenizers` project.
class quasiquote[T](qname: scala.Symbol) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro QuasiquoteMacros.impl
}

class QuasiquoteMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  import Flag._
  val Any = tq"_root_.scala.Any"
  val ReificationMacros = q"_root_.scala.meta.internal.tokenquasiquotes.ReificationMacros"
  val Dialect = tq"_root_.scala.meta.Dialect"
  val Tokens = tq"_root_.scala.meta.tokens.Tokens"
  def impl(annottees: c.Tree*): c.Tree = annottees.transformAnnottees(new ImplTransformer {
    val q"new $_[..$qtypes](scala.Symbol(${qname: String})).macroTransform(..$_)" = c.macroApplication
    override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val stats1 = stats :+ q"""
        object ${TermName(qname)} {
          import _root_.scala.language.experimental.macros
          def apply(args: $Any*)(implicit dialect: $Dialect): $Tokens = macro $ReificationMacros.apply
          def unapply(scrutinee: $Any)(implicit dialect: $Dialect): $Any = macro $ReificationMacros.unapply
        }
      """
      val cdef1 = q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      List(cdef1, mdef)
    }
  })
}

