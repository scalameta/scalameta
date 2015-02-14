package scala.meta
package internal.hosts.scalac
package contexts

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.hosts.scalac.contexts.{SemanticContext => ScalahostSemanticContext}
import scala.meta.internal.hosts.scalac.converters.{Api => ConverterApi}
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.{meta => papi}
import scala.meta.internal.{ast => p}
import scala.meta.internal.{hygiene => h}
import scala.meta.ui.{Exception => SemanticException, Summary}

class SemanticContext[G <: ScalaGlobal](val global: G) extends ConverterApi(global) with ScalametaSemanticContext {
  implicit val c: ScalametaSemanticContext = this

  def dialect: scala.meta.dialects.Scala211.type = {
    scala.meta.dialects.Scala211
  }

  private[meta] def desugar(term: papi.Term): papi.Term = {
    ???
  }

  private[meta] def tpe(term: papi.Term): papi.Type = {
    val tree = term.asInstanceOf[p.Term]
    val gtpeFromOriginal = tree.originalTree.map(_.tpe)
    val gtpeFromDenotation = tree.originalPre.flatMap(gpre => tree.originalSym.map(_.gsymbol.infoIn(gpre).finalResultType))
    val gtpe = gtpeFromOriginal.orElse(gtpeFromDenotation) match {
      case Some(gtpe) => gtpe
      case _ => throw new SemanticException(s"implementation restriction: internal cache has no type associated with ${term.show[Summary]}")
    }
    gtpe.toPtype.asInstanceOf[papi.Type]
  }

  private[meta] def defns(ref: papi.Ref): Seq[papi.Member] = {
    def tryScratchpad(pref: p.Ref): Option[Seq[papi.Member]] = {
      for { gpre <- pref.originalPre; lsym <- pref.originalSym }
      yield List(lsym.toPmember(gpre))
    }
    def tryNative(pref: p.Ref): Seq[papi.Member] = {
      def resolveName(pname: p.Name): Seq[papi.Member] = {
        val gpre = pname.denot.prefix match {
          case h.Prefix.Zero => g.NoPrefix
          case h.Prefix.Type(ptpe) => ptpe.asInstanceOf[p.Type].toGtype
        }
        val lsym = symbolTable.convert(pname.denot.symbol)
        List(lsym.toPmember(gpre))
      }
      pref match {
        case pname: p.Name => resolveName(pname)
        case p.Term.Select(_, pname) => defns(pname)
        case p.Type.Select(_, pname) => defns(pname)
        case p.Type.Project(_, pname) => defns(pname)
        case p.Type.Singleton(pref) => defns(pref)
        case p.Ctor.Ref.Select(_, pname) => defns(pname)
        case p.Ctor.Ref.Project(_, pname) => defns(pname)
        case p.Ctor.Ref.Function(pname) => defns(pname)
        case _: p.Import.Selector => ???
      }
    }
    ref.requireAttributed()
    tryScratchpad(ref.asInstanceOf[p.Ref]).getOrElse(tryNative(ref.asInstanceOf[p.Ref]))
  }

  private[meta] def members(tpe: papi.Type): Seq[papi.Member] = {
    val gtpe = tpe.asInstanceOf[p.Type].toGtype
    val gmembers = gtpe.members.filter(_ != g.rootMirror.RootPackage)
    val pmembers = gmembers.toLogical.map(_.toPmember(gtpe))
    val pfakectors = {
      val gpresym = gtpe.typeSymbol
      if (gpresym.isTrait) List(p.Ctor.Primary(Nil, p.Ctor.Name(gpresym.name.toString).withDenot(gpresym), List(List())))
      else Nil
    }
    pfakectors ++ pmembers
  }

  private[meta] def isSubType(tpe1: papi.Type, tpe2: papi.Type): Boolean = {
    val gtpe1 = tpe1.asInstanceOf[p.Type].toGtype
    val gtpe2 = tpe2.asInstanceOf[p.Type].toGtype
    gtpe1 <:< gtpe2
  }

  private[meta] def lub(tpes: Seq[papi.Type]): papi.Type = {
    val gtpes = tpes.map(_.asInstanceOf[p.Type].toGtype).toList
    g.lub(gtpes).toPtype.asInstanceOf[papi.Type]
  }

  private[meta] def glb(tpes: Seq[papi.Type]): papi.Type = {
    val gtpes = tpes.map(_.asInstanceOf[p.Type].toGtype).toList
    g.glb(gtpes).toPtype.asInstanceOf[papi.Type]
  }

  private[meta] def parents(tpe: papi.Type): Seq[papi.Type] = {
    val gtpe = tpe.asInstanceOf[p.Type].toGtype
    gtpe.directBaseTypes.map(_.toPtype.asInstanceOf[papi.Type])
  }

  private[meta] def widen(tpe: papi.Type): papi.Type = {
    val gtpe = tpe.asInstanceOf[p.Type].toGtype
    gtpe.widen.toPtype.asInstanceOf[papi.Type]
  }

  private[meta] def dealias(tpe: papi.Type): papi.Type = {
    val gtpe = tpe.asInstanceOf[p.Type].toGtype
    gtpe.dealias.toPtype.asInstanceOf[papi.Type]
  }

  private[meta] def parents(member: papi.Member): Seq[papi.Member] = {
    ???
  }

  private[meta] def children(member: papi.Member): Seq[papi.Member] = {
    ???
  }
}