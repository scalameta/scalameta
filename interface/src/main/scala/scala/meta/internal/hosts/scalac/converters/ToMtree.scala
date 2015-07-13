package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.parsers.Helpers.{XtensionTermOps => _, _}

// This module exposes a method that can convert scala.reflect trees
// into equivalent scala.meta trees.
//
// Unlike in the previous implementation, we don't care
// about preserving syntactic details of the original code:
// we just produce scala.meta trees for everything that we see
// (desugared forms or non-desugared forms alike),
// so that the result of the conversion captures all the semantics of the original code.
//
// In order to obtain a scala.meta tree that combines syntactic and semantic precision,
// you will need to use a dedicated module called `mergeTrees`
// that is capable of merging syntactically precise trees (obtained from parsing)
// and semantically precise trees (obtain from converting).
trait ToMtree extends GlobalToolkit with MetaToolkit {
  self: Api =>

  def toMtree(gtree: g.Tree): m.Tree = mtree(gtree, g.EmptyTree)

  private def mtree(gtree: g.Tree, gparent: g.Tree): m.Tree = {
    val mresult = gtree match {
      case g.PackageDef(gpid, gstats) =>
        if (gpid.name == g.nme.EMPTY_PACKAGE_NAME) {
          require(gparent.isEmpty)
          m.Source(mtrees(gstats, gtree).require[Seq[m.Stat]])
        } else {
          val mroot = gstats match {
            case List(gstat: g.ModuleDef) if gstat.name == g.nme.PACKAGE =>
              mtree(gstat, gparent).require[m.Pkg.Object]
            case _ =>
              val mpid = mname(gpid, gtree).require[m.Term.Name]
              val mstats = mtrees(gstats, gparent).require[Seq[m.Stat]]
              m.Pkg(mpid, mstats)
          }
          m.Source(List(mroot))
        }
      case gtree @ g.ClassDef(gmods, gname, gtparams, gimpl) =>
        val mmods = this.mmods(gmods, gtree)
        val mname = this.mname(g.Ident(gname), gtree).require[m.Type.Name]
        val gctor = gimpl.body.collectFirst{ case gctor: g.DefDef if gctor.name == g.nme.CONSTRUCTOR => gctor }
        val gvparamss = gctor.map(_.vparamss).getOrElse(Nil)
        val (mtparams, mvparamss) = mparams(gtparams, gvparamss)
        val mctormods = gctor.map(gctor => this.mmods(gctor.mods, gtree)).getOrElse(Nil)
        val mctor = m.Ctor.Primary(mctormods, mctorname(gctor.getOrElse(g.EmptyTree), gtree), mvparamss)
        val mimpl = mtree(gimpl, gtree).require[m.Template]
        m.Defn.Class(mmods, mname, mtparams, mctor, mimpl)
      case _ =>
        unreachable(debug(gtree, g.showRaw(gtree)))
    }
    if (sys.props("convert.debug") != null) {
      println(gtree)
      println(mresult)
      println(g.showRaw(gtree, printIds = true))
      println(mresult.show[Semantics])
    }
    mresult
  }

  private def mtrees(gtrees: List[g.Tree], gparent: g.Tree): Seq[m.Tree] = {
    gtrees.map(gtree => mtree(gtree, gparent))
  }

  private def mtreess(gtreess: List[List[g.Tree]], gparent: g.Tree): Seq[Seq[m.Tree]] = {
    gtreess.map(gtrees => mtrees(gtrees, gparent))
  }

  private def mname(gtree: g.NameTree, gparent: g.Tree): m.Name = {
    val sname = gtree.displayName(gparent)
    val mname = {
      if (sname == "_") m.Name.Anonymous()
      else if (gtree.name.isTermName) m.Term.Name(sname)
      else if (gtree.name.isTypeName) m.Type.Name(sname)
      else unreachable(debug(gtree, g.showRaw(gtree)))
    }
    // TODO: attach denotations here
    mname
  }

  private def mctorname(gtree: g.Tree, gparent: g.ImplDef): m.Ctor.Name = {
    val mname = m.Ctor.Name(this.mname(gparent, g.EmptyTree).value)
    // TODO: attach denotations here
    // if gtree.nonEmpty, then it's a class ctor - should be quite simple
    // if gtree.isEmpty, then gparent is a trait or a module
    // check out how this is handled in the earlier version of scalahost:
    // https://github.com/scalameta/scalahost/blob/master/interface/src/main/scala/scala/meta/internal/hosts/scalac/converters/ToMtree.scala#L50
    mname
  }

  private def mmods(gmods: g.Modifiers, gparent: g.Tree): Seq[m.Mod] = {
    ???
  }

  private def mparams(gtparams: List[g.TypeDef], gvparamss: List[List[g.ValDef]]): (List[m.Type.Param], List[List[m.Term.Param]]) = {
    def referencedTparam(gtarg: g.Tree): Option[g.TypeDef] = gtparams.filter(gtparam => {
      if (gtparam.symbol != g.NoSymbol) gtparam.symbol == gtarg.symbol
      else gtarg match { case g.Ident(gname) => gname == gtparam.name; case _ => false }
    }).headOption
    object ViewBound {
      def unapply(gtree: g.ValDef): Option[(g.TypeDef, g.Tree)] = gtree match {
        case g.ValDef(_, _, gtpt @ g.TypeTree(), _) =>
          val gtycon = if (gtpt.tpe != null) gtpt.tpe.typeSymbolDirect else g.NoSymbol
          val gtargs = if (gtpt.tpe != null) gtpt.tpe.typeArgs else Nil
          val gfrom = gtargs.map(_.typeSymbol) match { case List(sym, _) => sym; case _ => g.NoSymbol }
          val tyconMatches = gtycon == g.definitions.FunctionClass(1)
          if (tyconMatches) referencedTparam(g.Ident(gfrom)).map(gtparam => (gtparam, g.Ident(gtycon)))
          else None
        case g.ValDef(_, _, g.AppliedTypeTree(gtycon, gfrom :: _ :: Nil), _) =>
          val tyconMatches = gtycon match {
            // NOTE: this doesn't handle every possible case (e.g. a g.Ident binding to renamed import),
            // but it should be good enough for 95% of the situations
            case g.Select(gpre, g.TermName("Function1")) => gpre.symbol == g.definitions.ScalaPackage
            case gtycon: g.RefTree => gtycon.symbol == g.definitions.FunctionClass(1)
            case _ => false
          }
          if (tyconMatches) referencedTparam(gfrom).map(gtparam => (gtparam, gtycon))
          else None
        case _ =>
          None
      }
    }
    object ContextBound {
      def unapply(gtree: g.ValDef): Option[(g.TypeDef, g.Tree)] = gtree match {
        case g.ValDef(_, _, gtpt @ g.TypeTree(), _) =>
          val gtycon = if (gtpt.tpe != null) gtpt.tpe.typeSymbolDirect else g.NoSymbol
          val gtargs = if (gtpt.tpe != null) gtpt.tpe.typeArgs else Nil
          val gtarg = gtargs.map(_.typeSymbol) match { case List(sym) => sym; case _ => g.NoSymbol }
          referencedTparam(g.Ident(gtarg)).map(gtparam => (gtparam, g.Ident(gtycon)))
        case g.ValDef(_, _, g.AppliedTypeTree(gtycon, gtarg :: Nil), _) =>
          referencedTparam(gtarg).map(gtparam => (gtparam, gtycon))
        case _ =>
          None
      }
    }
    val (gexplicitss, gimplicitss) = gvparamss.partition(_.exists(_.mods.hasFlag(IMPLICIT)))
    val (gbounds, gimplicits) = gimplicitss.flatten.partition(_.name.startsWith(g.nme.EVIDENCE_PARAM_PREFIX))
    val gtparams1 = gtparams.map(gtparam => {
      val gviewBounds = gbounds.flatMap(ViewBound.unapply).filter(_._1.name == gtparam.name)
      val gcontextBounds = gbounds.flatMap(ContextBound.unapply).filter(_._1.name == gtparam.name)
      gtparam.appendMetadata("viewBounds" -> gviewBounds, "contextBounds" -> gcontextBounds)
    })
    ???
  }
}