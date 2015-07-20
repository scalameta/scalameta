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
import scala.reflect.ClassTag
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

  protected def toMtree[T <: mapi.Tree : ClassTag](gtree: g.Tree): T = gtree.toMtree[T]

  protected implicit class RichTreeToMtree(gtree: g.Tree) {
    def toMtree[T <: mapi.Tree : ClassTag]: T = {
      // TODO: figure out a mechanism to automatically remove navigation links once we're done
      // in order to cut down memory consumption of the further compilation pipeline
      // TODO: another performance consideration is the fact that install/remove
      // are currently implemented as standalone tree traversal, and it would be faster
      // to integrate them into the transforming traversal
      gtree.installNavigationLinks()
      val denotedMtree = gtree match {
        case l.EmptyPackageDef(lstats) =>
          val mstats = lstats.toMtrees[m.Stat]
          m.Source(mstats)
        case l.ToplevelPackageDef(lname, lstats) =>
          val mname = lname.toMtree[m.Term.Name]
          val mstats = lstats.toMtrees[m.Stat]
          m.Source(List(m.Pkg(mname, mstats)))
        case l.NestedPackageDef(lname, lstats) =>
          val mname = lname.toMtree[m.Term.Name]
          val mstats = lstats.toMtrees[m.Stat]
          m.Pkg(mname, mstats)
        case l.ClassDef(lmods, lname, ltparams, lctor, limpl) =>
          val mmods = lmods.toMtrees[m.Mod]
          val mname = lname.toMtree[m.Type.Name]
          val mtparams = ltparams.toMtrees[m.Type.Param]
          val mctor = lctor.toMtree[m.Ctor.Primary]
          val mimpl = limpl.toMtree[m.Template]
          m.Defn.Class(mmods, mname, mtparams, mctor, mimpl)
        case l.PrimaryCtorDef(lmods, lname, lparamss) =>
          val mmods = lmods.toMtrees[m.Mod]
          val mname = lname.toMtree[m.Ctor.Name]
          val mparamss = lparamss.toMtreess[m.Term.Param]
          m.Ctor.Primary(mmods, mname, mparamss)
        case l.Template(learly, lparents, lself, lstats) =>
          val mearly = learly.toMtrees[m.Stat]
          val mparents = lparents.toMtrees[m.Term]
          val mself = lself.toMtree[m.Term.Param]
          val mstats = lstats.toMtrees[m.Stat]
          m.Template(mearly, mparents, mself, Some(mstats))
        case l.Parent(ltpt, lctor, largss) =>
          // TODO: how do we assign tpes to mctor and also to the successive results of the foldLeft below?
          val mtpt = ltpt.toMtree[m.Type]
          val mctor = mtpt.ctorRef(lctor.toMtree[m.Ctor.Name]).require[m.Term]
          val margss = largss.toMtreess[m.Term.Arg]
          margss.foldLeft(mctor)((mcurr, margs) => m.Term.Apply(mcurr, margs))
        case l.SelfDef(lname, ltpt) =>
          val mname = lname.toMtree[m.Term.Param.Name]
          val mtpt = if (ltpt.nonEmpty) Some(ltpt.toMtree[m.Type]) else None
          m.Term.Param(Nil, mname, mtpt, None)
        case l.DefDef(lmods, lname, ltparams, lparamss, ltpt, lrhs) =>
          val mmods = lmods.toMtrees[m.Mod]
          val mname = lname.toMtree[m.Term.Name]
          val mtparams = ltparams.toMtrees[m.Type.Param]
          val mparamss = lparamss.toMtreess[m.Term.Param]
          val mtpt = if (ltpt.nonEmpty) Some(ltpt.toMtree[m.Type]) else None
          val mrhs = lrhs.toMtree[m.Term]
          m.Defn.Def(mmods, mname, mtparams, mparamss, mtpt, mrhs)
        case l.ParamDef(lmods, lname, ltpt, ldefault) =>
          val mmods = lmods.toMtrees[m.Mod]
          val mname = lname.toMtree[m.Term.Param.Name]
          val mtpt = if (ltpt.nonEmpty) Some(ltpt.toMtree[m.Type]) else None
          val mdefault = if (ldefault.nonEmpty) Some(ldefault.toMtree[m.Term]) else None
          m.Term.Param(mmods, mname, mtpt, mdefault)
        case l.TermIdent(lname) =>
          lname.toMtree[m.Term.Name]
        case l.TermName(ldenot, lvalue) =>
          m.Term.Name(lvalue).tryDenot(ldenot)
        case l.TypeTree(gtpe) =>
          gtpe.toMtype
        case l.TypeIdent(lname) =>
          lname.toMtree[m.Type.Name]
        case l.TypeName(ldenot, lvalue) =>
          m.Type.Name(lvalue).tryDenot(ldenot)
        case l.TypeSelect(lpre, lname) =>
          val mpre = lpre.toMtree[m.Term.Ref]
          val mname = lname.toMtree[m.Type.Name]
          m.Type.Select(mpre, mname)
        case l.CtorIdent(lname) =>
          lname.toMtree[m.Ctor.Name]
        case l.CtorName(ldenot, lvalue) =>
          m.Ctor.Name(lvalue).tryDenot(ldenot)
        case l.AnonymousName(ldenot) =>
          m.Name.Anonymous().tryDenot(ldenot)
        case l.IndeterminateName(ldenot, lvalue) =>
          m.Name.Indeterminate(lvalue).tryDenot(ldenot)
        case _ =>
          unreachable(debug(gtree, g.showRaw(gtree)))
      }
      val typedMtree = denotedMtree match {
        case denotedMtree: m.Term => denotedMtree.tryTyping(gtree.tpe)
        case denotedMtree: m.Term.Param => denotedMtree.tryTyping(gtree.symbol.tpe)
        case denotedMtree => denotedMtree
      }
      val mtree = typedMtree // desugarings are taken care of in mergeTrees
      if (sys.props("convert.debug") != null && gtree.parent.isEmpty) {
        println("======= SCALA.REFLECT TREE =======")
        println(gtree)
        println(g.showRaw(gtree, printIds = true))
        println("======== SCALA.META TREE ========")
        println(mtree)
        println(mtree.show[Semantics])
        println("=================================")
      }
      mtree.require[T]
    }
  }

  protected implicit class RichTreesToMtrees(gtrees: List[g.Tree]) {
    def toMtrees[T <: m.Tree : ClassTag]: Seq[T] = gtrees.map(_.toMtree[T])
  }

  protected implicit class RichTreessToMtreess(gtreess: List[List[g.Tree]]) {
    def toMtreess[T <: m.Tree : ClassTag]: Seq[Seq[T]] = gtreess.map(_.toMtrees[T])
  }
}
