package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.{classTag, ClassTag}
import scala.reflect.internal.Flags._
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.flags._
import scala.meta.internal.parsers.Helpers.{XtensionTermOps => _, _}
import scala.meta.internal.hosts.scalac.reflect._

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

  protected implicit class XtensionGtreeToMtree(gtree: g.Tree) {
    def toMtree[T <: mapi.Tree : ClassTag]: T = {
      // TODO: figure out a mechanism to automatically remove navigation links once we're done
      // in order to cut down memory consumption of the further compilation pipeline
      // TODO: another performance consideration is the fact that install/remove
      // are currently implemented as standalone tree traversal, and it would be faster
      // to integrate them into the transforming traversal
      gtree.installNavigationLinks()
      val denotedMtree = gtree match {
        // ============ NAMES ============

        case l.AnonymousName(ldenot) =>
          m.Name.Anonymous().tryDenot(ldenot)
        case l.IndeterminateName(ldenot, lvalue) =>
          m.Name.Indeterminate(lvalue).tryDenot(ldenot)

        // ============ TERMS ============

        case l.TermThis(lname) =>
          val mname = lname.toMtree[m.Name.Qualifier]
          m.Term.This(mname)
        case l.TermName(ldenot, lvalue) =>
          m.Term.Name(lvalue).tryDenot(ldenot)
        case l.TermIdent(lname) =>
          lname.toMtree[m.Term.Name]
        case l.TermParamDef(lmods, lname, ltpt, ldefault) =>
          val mmods = lmods.toMtrees[m.Mod]
          val mname = lname.toMtree[m.Term.Param.Name]
          val mtpt = if (ltpt.nonEmpty) Some(ltpt.toMtree[m.Type]) else None
          val mdefault = if (ldefault.nonEmpty) Some(ldefault.toMtree[m.Term]) else None
          m.Term.Param(mmods, mname, mtpt, mdefault)

        // ============ TYPES ============

        case l.TypeTree(gtpe) =>
          gtpe.toMtype
        case l.TypeName(ldenot, lvalue) =>
          m.Type.Name(lvalue).tryDenot(ldenot)
        case l.TypeIdent(lname) =>
          lname.toMtree[m.Type.Name]
        case l.TypeSelect(lpre, lname) =>
          val mpre = lpre.toMtree[m.Term.Ref]
          val mname = lname.toMtree[m.Type.Name]
          m.Type.Select(mpre, mname)

        // ============ PATTERNS ============

        // ============ LITERALS ============

        // ============ DECLS ============

        // ============ DEFNS ============

        case l.DefDef(lmods, lname, ltparams, lparamss, ltpt, lrhs) =>
          val mmods = lmods.toMtrees[m.Mod]
          val mname = lname.toMtree[m.Term.Name]
          val mtparams = ltparams.toMtrees[m.Type.Param]
          val mparamss = lparamss.toMtreess[m.Term.Param]
          val mtpt = if (ltpt.nonEmpty) Some(ltpt.toMtree[m.Type]) else None
          val mrhs = lrhs.toMtree[m.Term]
          m.Defn.Def(mmods, mname, mtparams, mparamss, mtpt, mrhs)

        case l.ClassDef(lmods, lname, ltparams, lctor, limpl) =>
          val mmods = lmods.toMtrees[m.Mod]
          val mname = lname.toMtree[m.Type.Name]
          val mtparams = ltparams.toMtrees[m.Type.Param]
          val mctor = lctor.toMtree[m.Ctor.Primary]
          val mimpl = limpl.toMtree[m.Template]
          m.Defn.Class(mmods, mname, mtparams, mctor, mimpl)

        // ============ PKGS ============

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

        // ============ CTORS ============

        case l.PrimaryCtorDef(lmods, lname, lparamss) =>
          val mmods = lmods.toMtrees[m.Mod]
          val mname = lname.toMtree[m.Ctor.Name]
          val mparamss = lparamss.toMtreess[m.Term.Param]
          m.Ctor.Primary(mmods, mname, mparamss)
        case l.CtorName(ldenot, lvalue) =>
          m.Ctor.Name(lvalue).tryDenot(ldenot)
        case l.CtorIdent(lname) =>
          lname.toMtree[m.Ctor.Name]

        // ============ TEMPLATES ============

        case l.Template(learly, lparents, lself, lstats) =>
          val mearly = learly.toMtrees[m.Stat]
          val mparents = lparents.toMtrees[m.Ctor.Call]
          val mself = lself.toMtree[m.Term.Param]
          val mstats = lstats.toMtrees[m.Stat]
          m.Template(mearly, mparents, mself, Some(mstats))
        case l.Parent(ltpt, lctor, largss) =>
          val mtpt = ltpt.toMtree[m.Type]
          val mctor = mtpt.ctorRef(lctor.toMtree[m.Ctor.Name]).require[m.Term]
          val margss = largss.toMtreess[m.Term.Arg]
          margss.foldLeft(mctor)((mcurr, margs) => {
            val app = m.Term.Apply(mcurr, margs)
            app.withTyping(mcurr.typing.map{ case m.Type.Function(_, ret) => ret })
          })
        case l.SelfDef(lname, ltpt) =>
          val mname = lname.toMtree[m.Term.Param.Name]
          val mtpt = if (ltpt.nonEmpty) Some(ltpt.toMtree[m.Type]) else None
          m.Term.Param(Nil, mname, mtpt, None)

        // ============ MODIFIERS ============

        // ============ ODDS & ENDS ============

        case _ =>
          fail(gtree, s"encountered an unexpected tree during scala.reflect -> scala.meta conversion:$EOL${g.showRaw(gtree)}")
      }
      val typedMtree = denotedMtree match {
        case denotedMtree: m.Term => denotedMtree.tryTyping(gtree.tpe)
        case denotedMtree: m.Term.Param => denotedMtree.tryTyping(gtree.symbol.tpe)
        case denotedMtree => denotedMtree
      }
      // NOTE: Desugarings are handled in mergeTrees, but sometimes we don't merge,
      // e.g. when the source code is unavailable, and we still need to be TYPECHECKED even then.
      val expandedMtree = typedMtree match {
        case typedMtree: m.Term => typedMtree.withIdentityExpansion
        case typedMtree => typedMtree
      }
      val typecheckedMtree = expandedMtree.setTypechecked
      val mtree = indexOne(typecheckedMtree)
      if (sys.props("convert.debug") != null && gtree.parent.isEmpty) {
        println("======= SCALA.REFLECT TREE =======")
        println(gtree)
        println(g.showRaw(gtree, printIds = true, printTypes = true))
        println("======== SCALA.META TREE ========")
        println(mtree)
        println(mtree.show[Semantics])
        println("=================================")
      }
      // TODO: fix duplication wrt MergeTrees.scala
      if (classTag[T].runtimeClass.isAssignableFrom(mtree.getClass)) {
        mtree.asInstanceOf[T]
      } else {
        var expected = classTag[T].runtimeClass.getName
        expected = expected.stripPrefix("scala.meta.internal.ast.").stripPrefix("scala.meta.")
        expected = expected.stripSuffix("$Impl")
        expected = expected.replace("$", ".")
        val actual = mtree.productPrefix
        val summary = s"expected = $expected, actual = $actual"
        val details = s"${g.showRaw(gtree)}$EOL${mtree.show[Structure]}"
        fail(gtree, s"obtained an unexpected result during scala.reflect -> scala.meta conversion: $summary$EOL$details")
      }
    }

    private def fail(culprit: g.Tree, diagnostics: String): Nothing = {
      val traceback = culprit.parents.map(gtree => {
        val prefix = gtree.productPrefix
        var details = gtree.toString.replace("\n", " ")
        if (details.length > 60) details = details.take(60) + "..."
        s"($prefix) $details"
      }).mkString(EOL)
      throw new ConvertException(culprit, s"$diagnostics$EOL$traceback")
    }
  }

  protected implicit class RichTreesToMtrees(gtrees: List[g.Tree]) {
    def toMtrees[T <: m.Tree : ClassTag]: Seq[T] = gtrees.map(_.toMtree[T])
  }

  protected implicit class RichTreessToMtreess(gtreess: List[List[g.Tree]]) {
    def toMtreess[T <: m.Tree : ClassTag]: Seq[Seq[T]] = gtreess.map(_.toMtrees[T])
  }
}
