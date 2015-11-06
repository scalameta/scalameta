package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.debug._
import org.scalameta.default.Param
import org.scalameta.default.Param._
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
import scala.meta.internal.ast.Helpers.{XtensionTermOps => _, _}
import scala.meta.internal.hosts.scalac.reflect._
import scala.meta.internal.prettyprinters.Attributes
import scala.meta.internal.ast.XtensionConvertDebug

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
trait ToMtree extends ReflectToolkit with MetaToolkit {
  self: Api =>

  protected implicit class XtensionGtreeToMtree(gtree0: g.Tree) {
    private val (gtree, gexpansion) = gtree0 match {
      case UninferrableDesugaring(goriginal, gexpansion) => (goriginal, gexpansion)
      case _ => (gtree0, g.EmptyTree)
    }

    // TODO: figure out a mechanism to automatically remove navigation links once we're done
    // in order to cut down memory consumption of the further compilation pipeline
    // TODO: another performance consideration is the fact that install/remove
    // are currently implemented as standalone tree traversal, and it would be faster
    // to integrate them into the transforming traversal
    gtree.installNavigationLinks()
    gtree.setParent(gtree0.parent)

    def toMtree[T <: mapi.Tree : ClassTag]: T = {
      try {
        val maybeDenotedMtree = gtree match {
          // ============ NAMES ============

          case l.AnonymousName(ldenot) =>
            m.Name.Anonymous().tryMattrs(ldenot)
          case l.IndeterminateName(ldenot, lvalue) =>
            m.Name.Indeterminate(lvalue).tryMattrs(ldenot)

          // ============ TERMS ============

          case l.TermThis(lname) =>
            val mname = lname.toMtree[m.Name.Qualifier]
            m.Term.This(mname)
          case l.TermName(ldenot, lvalue) =>
            m.Term.Name(lvalue).tryMattrs(ldenot)
          case l.TermIdent(lname) =>
            lname.toMtree[m.Term.Name]
          case l.TermSelect(lpre, lname) =>
            val mpre = lpre.toMtree[m.Term]
            val mname = lname.toMtree[m.Term.Name]
            m.Term.Select(mpre, mname)
          case l.TermApply(lfun, largs) =>
            val mfun = lfun.toMtree[m.Term]
            val margs = largs.toMtrees[m.Term]
            m.Term.Apply(mfun, margs)
          case l.TermApplyType(lfun, ltargs) =>
            val mfun = lfun.toMtree[m.Term]
            val mtargs = ltargs.toMtrees[m.Type]
            m.Term.ApplyType(mfun, mtargs)
          case l.TermBlock(lstats) =>
            val mstats = lstats.toMtrees[m.Stat]
            m.Term.Block(mstats)
          case l.TermIf(lcond, lthen, lelse) =>
            val mcond = lcond.toMtree[m.Term]
            val mthen = lthen.toMtree[m.Term]
            val melse = lelse.toMtree[m.Term]
            m.Term.If(mcond, mthen, melse)
          case l.TermMatch(lscrut, lcases) =>
            val mscrut = lscrut.toMtree[m.Term]
            val mcases = lcases.toMtrees[m.Case]
            m.Term.Match(mscrut, mcases)
          case l.TermParamDef(lmods, lname, ltpt, ldefault) =>
            val mmods = lmods.toMtrees[m.Mod]
            val mname = lname.toMtree[m.Term.Param.Name]
            val mtpt = if (ltpt.nonEmpty) Some(ltpt.toMtree[m.Type]) else None
            val mdefault = if (ldefault.nonEmpty) Some(ldefault.toMtree[m.Term]) else None
            m.Term.Param(mmods, mname, mtpt, mdefault).tryMattrs(gtree.symbol.tpe)

          // ============ TYPES ============

          case l.TypeTree(gtpe) =>
            gtpe.toMtype
          case l.TypeName(ldenot, lvalue) =>
            m.Type.Name(lvalue).tryMattrs(ldenot)
          case l.TypeIdent(lname) =>
            lname.toMtree[m.Type.Name]
          case l.TypeSelect(lpre, lname) =>
            val mpre = lpre.toMtree[m.Term.Ref]
            val mname = lname.toMtree[m.Type.Name]
            m.Type.Select(mpre, mname)
          case l.TypeApply(ltpt, largs) =>
            val mtpt = ltpt.toMtree[m.Type]
            val margs = largs.toMtrees[m.Type]
            m.Type.Apply(mtpt, margs)
          case l.TypeBounds(llo, lhi) =>
            val mlo = llo.toMtreeopt[m.Type]
            val mhi = lhi.toMtreeopt[m.Type]
            m.Type.Bounds(mlo, mhi)
          case l.TypeParamDef(lmods, lname, ltparams, ltbounds, lvbounds, lcbounds) =>
            val mmods = lmods.toMtrees[m.Mod]
            val mname = lname.toMtree[m.Type.Param.Name]
            val mtparams = ltparams.toMtrees[m.Type.Param]
            val mtbounds = ltbounds.toMtree[m.Type.Bounds]
            val mvbounds = lvbounds.toMtrees[m.Type]
            val mcbounds = lcbounds.toMtrees[m.Type]
            m.Type.Param(mmods, mname, mtparams, mtbounds, mvbounds, mcbounds)

          // ============ PATTERNS ============

          case l.PatVarTerm(lname) =>
            val mname = lname.toMtree[m.Term.Name]
            m.Pat.Var.Term(mname)

          // ============ LITERALS ============

          case l.Literal(lvalue) =>
            m.Lit(lvalue)

          // ============ DECLS ============

          // ============ DEFNS ============

          case l.ValDef(lmods, lpats, ltpt, lrhs) =>
            val mmods = lmods.toMtrees[m.Mod]
            val mpats = lpats.toMtrees[m.Pat]
            val mtpt = if (ltpt.nonEmpty) Some(ltpt.toMtree[m.Type]) else None
            val mrhs = lrhs.toMtree[m.Term]
            m.Defn.Val(mmods, mpats, mtpt, mrhs)

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

          case l.TraitDef(lmods, lname, ltparams, limpl) =>
            val mmods = lmods.toMtrees[m.Mod]
            val mname = lname.toMtree[m.Type.Name]
            val mtparams = ltparams.toMtrees[m.Type.Param]
            val mctor = ???
            val mimpl = limpl.toMtree[m.Template]
            m.Defn.Trait(mmods, mname, mtparams, mctor, mimpl)

          case l.ObjectDef(lmods, lname, limpl) =>
            val mmods = lmods.toMtrees[m.Mod]
            val mname = lname.toMtree[m.Term.Name]
            val mimpl = limpl.toMtree[m.Template]
            m.Defn.Object(mmods, mname, mimpl)

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
            m.Ctor.Name(lvalue).tryMattrs(ldenot)
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
              app.tryMattrs(mcurr.typing.map{ case m.Type.Method(_, ret) => ret })
            })
          case l.SelfDef(lname, ltpt) =>
            val mname = lname.toMtree[m.Term.Param.Name]
            val mtpt = if (ltpt.nonEmpty) Some(ltpt.toMtree[m.Type]) else None
            val gtpe = lname.denot.sym match { case l.Self(owner) => owner.typeOfThis; case _ => g.NoType }
            m.Term.Param(Nil, mname, mtpt, None).tryMattrs(gtpe)

          // ============ MODIFIERS ============

          // ============ ODDS & ENDS ============

          case l.CaseDef(lpat, lguard, lbody) =>
            val mpat = lpat.toMtree[m.Pat]
            val mguard = lguard.toMtreeopt[m.Term]
            val mbody = lbody.toMtree[m.Term]
            m.Case(mpat, mguard, mbody)

          case _ =>
            fail(s"unsupported tree ${g.showRaw(gtree)}")
        }
        val maybeTypedMtree = {
          if (maybeDenotedMtree.isUnattributed) {
            maybeDenotedMtree match {
              case maybeDenotedMtree: m.Term.Name => maybeDenotedMtree // do nothing, typing already inferred from denotation
              case maybeDenotedMtree: m.Ctor.Name => maybeDenotedMtree // do nothing, typing already inferred from denotation
              case maybeDenotedMtree: m.Term => maybeDenotedMtree.tryMattrs(gtree.tpe)
              case maybeDenotedMtree: m.Term.Param => maybeDenotedMtree // do nothing, typing already assigned during conversion
              case maybeDenotedMtree => maybeDenotedMtree
            }
          } else {
            maybeDenotedMtree
          }
        }
        val maybeExpandedMtree = {
          if (gexpansion.isEmpty) maybeTypedMtree
          else maybeTypedMtree match {
            case maybeTypedMtree: m.Term =>
              val (obliviousGexpansion, memento) = gexpansion.forgetOriginal
              obliviousGexpansion.installNavigationLinks()
              try maybeTypedMtree.withExpansion(obliviousGexpansion.toMtree[m.Term])
              finally gexpansion.rememberOriginal(memento)
            case _ =>
              fail("unsupported original")
          }
        }
        val maybeTypecheckedMtree = {
          // TODO: Trying to force our way in is kinda lame.
          // In the future, we could remember whether any nested toMtree calls failed to attribute itself,
          // and then, based on that, decide whether we need to call setTypechecked or not.
          try maybeExpandedMtree.forceTypechecked
          catch { case ex: Exception => maybeExpandedMtree }
        }
        val maybeIndexedMtree = {
          if (maybeTypecheckedMtree.isTypechecked) indexOne(maybeTypecheckedMtree)
          else maybeTypecheckedMtree
        }
        if (gtree.parent.isEmpty) {
          Debug.logConvert {
            println("======= SCALA.REFLECT TREE =======")
            println(gtree)
            println(g.showRaw(gtree, printIds = true, printTypes = true))
            println("======== SCALA.META TREE ========")
            println(maybeIndexedMtree)
            println(maybeIndexedMtree.show[Attributes])
            println("=================================")
          }
        }
        if (classTag[T].runtimeClass.isAssignableFrom(maybeIndexedMtree.getClass)) {
          maybeIndexedMtree.asInstanceOf[T]
        } else {
          var expected = classTag[T].runtimeClass.getName
          expected = expected.stripPrefix("scala.meta.internal.ast.").stripPrefix("scala.meta.")
          expected = expected.stripSuffix("$Impl")
          expected = expected.replace("$", ".")
          val actual = maybeIndexedMtree.productPrefix
          val summary = s"expected = $expected, actual = $actual"
          val details = s"${g.showRaw(gtree)}$EOL${maybeIndexedMtree.show[Structure]}"
          fail(s"unexpected result: $summary$EOL$details")
        }
      } catch {
        case ex: ConvertException =>
          throw ex
        case ex: Exception =>
          fail(s"unexpected error (scroll down the stacktrace to see the cause):", ex)
        case ex: NotImplementedError =>
          fail(s"unexpected error (scroll down the stacktrace to see the cause):", ex)
      }
    }

    private def fail(diagnostics: String, ex: Param[Throwable] = Default): Nothing = {
      val culprit = gtree0 // NOTE: I tried making culprit a parameter of fail, but fail is always called with gtree0 anyway
      val culprits = culprit +: culprit.parents
      val traceback = culprits.map(gtree => {
        def briefPrettyprint(gtree: g.Tree): String = {
          var result = gtree.toString.replace("\n", " ")
          if (result.length > 60) result = result.take(60) + "..."
          result
        }
        val prefix = gtree.productPrefix
        var details = briefPrettyprint(gtree)
        gtree match {
          case UninferrableDesugaring(goriginal, _) =>
            details = details + " that desugars into "
            details = details + briefPrettyprint(goriginal)
          case _ =>
            // do nothing
        }
        s"($prefix) $details"
      }).mkString(EOL)
      throw new ConvertException(culprit, s"$diagnostics$EOL$traceback", ex.toOption)
    }
  }

  protected implicit class RichTreeoptToMtreeopt(gtreeopt: Option[g.Tree]) {
    def toMtreeopt[T <: m.Tree : ClassTag]: Option[T] = gtreeopt.map(_.toMtree[T])
  }

  protected implicit class RichTreesToMtrees(gtrees: List[g.Tree]) {
    def toMtrees[T <: m.Tree : ClassTag]: Seq[T] = gtrees.map(_.toMtree[T])
  }

  protected implicit class RichTreessToMtreess(gtreess: List[List[g.Tree]]) {
    def toMtreess[T <: m.Tree : ClassTag]: Seq[Seq[T]] = gtreess.map(_.toMtrees[T])
  }
}
