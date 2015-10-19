package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.language.implicitConversions
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.hosts.scalac.reflect._
import scala.meta.internal.flags._

trait ToMname extends ReflectToolkit with MetaToolkit {
  self: Api =>

  protected implicit class XtensionGsymbolToMname[T <: g.Symbol, U <: m.Name](gsym: T)(implicit ev: GsymbolToMname[T, U]) {
    def toMname(gpre0: g.Type, value: String = gsym.displayName): U = {
      // TODO: account for ctornames?
      // TODO: what about anonymous names?
      val gpre = if (gpre0 == DefaultPrefix) gsym.prefix else gpre0
      val mname = {
        if (gsym.isTerm) m.Term.Name(value).withMattrs(gpre, gsym).asInstanceOf[U]
        else if (gsym.isType) m.Type.Name(value).withMattrs(gpre, gsym).asInstanceOf[U]
        else abort(gsym)
      }
      mname.forceTypechecked
    }
  }

  private lazy val dummySymbol = g.rootMirror.RootClass.newTermSymbol(g.TermName("<defaultPrefix>"))
  protected lazy val DefaultPrefix = g.TypeRef(g.NoPrefix, dummySymbol, Nil)
  protected implicit class XtensionDefaultPrefix(_g: g.type) { def DefaultPrefix = self.DefaultPrefix }

  protected trait GsymbolToMname[T, U]
  object GsymbolToMname {
    implicit def gsymbolToMname: GsymbolToMname[g.Symbol, m.Name] = null
    implicit def gtermSymbolToMtermName[T <: g.TermSymbol]: GsymbolToMname[T, m.Term.Name] = null
    implicit def gtypeSymbolToMtypeName[T <: g.TypeSymbol]: GsymbolToMname[T, m.Type.Name] = null
  }
}