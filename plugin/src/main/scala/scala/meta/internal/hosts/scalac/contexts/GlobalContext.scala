package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.contexts._
import org.scalameta.invariants._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.{ScalahostGlobalContext => GlobalContextApi}
import scala.meta.internal.hosts.scalac.converters.{Api => ConverterApi}
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.dialects.Scala211
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}

@context(translateExceptions = true)
class GlobalContext[G <: ScalaGlobal](val global: G) extends ConverterApi(global) with GlobalContextApi[G] {
  implicit val c: ScalametaSemanticContext = this

  def dialect: scala.meta.dialects.Scala211.type = {
    scala.meta.dialects.Scala211
  }

  private[meta] def desugar(term: mapi.Term): mapi.Term = {
    ???
  }

  private[meta] def tpe(term: mapi.Term): mapi.Type = {
    val mtpe = term.requireTyped()
    mtpe.require[m.Type]
  }

  private[meta] def tpe(param: mapi.Term.Param): mapi.Type.Arg = {
    val mtpe = param.name.requireTyped()
    mtpe.require[m.Type.Arg]
  }

  private[meta] def defns(ref: mapi.Ref): Seq[mapi.Member] = {
    ref.requireDenoted()
    ref match {
      case pname: m.Name => pname.toGsymbols.map(_.toMmember(pname.toGprefix))
      case m.Term.Select(_, pname) => defns(pname)
      case m.Type.Select(_, pname) => defns(pname)
      case m.Type.Project(_, pname) => defns(pname)
      case m.Type.Singleton(pref) => defns(pref)
      case m.Ctor.Ref.Select(_, pname) => defns(pname)
      case m.Ctor.Ref.Project(_, pname) => defns(pname)
      case m.Ctor.Ref.Function(pname) => defns(pname)
      case _: m.Import.Selector => ???
    }
  }

  private[meta] def members(tpe: mapi.Type): Seq[mapi.Member] = {
    val gtpe = tpe.require[m.Type].toGtype
    val gmembers = gtpe.members.filter(_ != g.rootMirror.RootPackage)
    val pmembers = gmembers.toLogical.map(_.toMmember(gtpe))
    val pfakectors = {
      val gpresym = gtpe.typeSymbol
      if (gpresym.isTrait) List(m.Ctor.Primary(Nil, m.Ctor.Name(gpresym.name.toString).withDenot(gpresym), List(List())))
      else Nil
    }
    pfakectors ++ pmembers
  }

  private[meta] def isSubType(tpe1: mapi.Type, tpe2: mapi.Type): Boolean = {
    val gtpe1 = tpe1.require[m.Type].toGtype
    val gtpe2 = tpe2.require[m.Type].toGtype
    gtpe1 <:< gtpe2
  }

  private[meta] def lub(tpes: Seq[mapi.Type]): mapi.Type = {
    val gtpes = tpes.map(_.require[m.Type].toGtype).toList
    g.lub(gtpes).toMtype
  }

  private[meta] def glb(tpes: Seq[mapi.Type]): mapi.Type = {
    val gtpes = tpes.map(_.require[m.Type].toGtype).toList
    g.glb(gtpes).toMtype
  }

  private[meta] def parents(tpe: mapi.Type): Seq[mapi.Type] = {
    val gtpe = tpe.require[m.Type].toGtype
    gtpe.directBaseTypes.map(_.toMtype)
  }

  private[meta] def widen(tpe: mapi.Type): mapi.Type = {
    val gtpe = tpe.require[m.Type].toGtype
    gtpe.widen.toMtype
  }

  private[meta] def dealias(tpe: mapi.Type): mapi.Type = {
    val gtpe = tpe.require[m.Type].toGtype
    gtpe.dealias.toMtype
  }

  private[meta] def parents(member: mapi.Member): Seq[mapi.Member] = {
    val gpre = member.require[m.Member].toGprefix
    val Seq(lsym) = member.require[m.Member].toGsymbols
    lsym.parents.map(_.toMmember(gpre)) // TODO: also instantiate type parameters when necessary
  }

  private[meta] def children(member: mapi.Member): Seq[mapi.Member] = {
    val gpre = member.require[m.Member].toGprefix
    val Seq(lsym) = member.require[m.Member].toGsymbols
    lsym.children.map(_.toMmember(gpre)) // TODO: also instantiate type parameters when necessary
  }
}