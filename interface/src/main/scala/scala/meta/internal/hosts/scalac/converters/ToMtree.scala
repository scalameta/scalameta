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

  def toMtree(gtree: g.Tree): m.Tree = mtree(gtree, Nil)

  private def mtree(gtree: g.Tree, gparents: List[g.Tree]): m.Tree = {
    val gparents1 = gtree :: gparents
    val mresult = gtree match {
      case g.PackageDef(gpid, gstats) =>
        if (gpid.name == g.nme.EMPTY_PACKAGE_NAME) {
          require(gparents.isEmpty)
          m.Source(mtrees(gstats, gparents1).require[Seq[m.Stat]])
        } else {
          val mroot = gstats match {
            case List(gstat: g.ModuleDef) if gstat.name == g.nme.PACKAGE =>
              mtree(gstat, gparents1).require[m.Pkg.Object]
            case _ =>
              val mpid = mname(gpid, gparents1).require[m.Term.Name]
              val mstats = mtrees(gstats, gparents1).require[Seq[m.Stat]]
              m.Pkg(mpid, mstats)
          }
          m.Source(List(mroot))
        }
      case gtree @ g.ClassDef(gmods, gname, gtparams, gimpl) =>
        val mmods = this.mmods(gmods, gparents1)
        val mname = this.mname(g.Ident(gname), gparents1).require[m.Type.Name]
        val gctor = gimpl.body.collectFirst{ case gctor: g.DefDef if gctor.name == g.nme.CONSTRUCTOR => gctor }
        val gvparamss = gctor.map(_.vparamss).getOrElse(Nil)
        val (mtparams, mvparamss) = mparams(gtparams, gvparamss, gparents1)
        val mctormods = gctor.map(gctor => this.mmods(gctor.mods, gparents1)).getOrElse(Nil)
        val mctor = m.Ctor.Primary(mctormods, mctorname(gctor.getOrElse(g.EmptyTree), gparents1), mvparamss)
        val mimpl = mtree(gimpl, gparents1).require[m.Template]
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

  private def mtrees(gtrees: List[g.Tree], gparents: List[g.Tree]): Seq[m.Tree] = {
    gtrees.map(gtree => mtree(gtree, gparents))
  }

  private def mtreess(gtreess: List[List[g.Tree]], gparents: List[g.Tree]): Seq[Seq[m.Tree]] = {
    gtreess.map(gtrees => mtrees(gtrees, gparents))
  }

  private def mname(gtree: g.NameTree, gparents: List[g.Tree]): m.Name = {
    val sname = gtree.displayName(gparents.headOption.getOrElse(g.EmptyTree))
    val mname = {
      if (sname == "_") m.Name.Anonymous()
      else if (gtree.name.isTermName) m.Term.Name(sname)
      else if (gtree.name.isTypeName) m.Type.Name(sname)
      else unreachable(debug(gtree, g.showRaw(gtree)))
    }
    // TODO: attach denotations here
    mname
  }

  private def mctorname(gtree: g.Tree, gparents: List[g.Tree]): m.Ctor.Name = {
    val (gmdef: g.NameTree) :: ggparents = gparents
    val mname = m.Ctor.Name(this.mname(gmdef, ggparents).value)
    // TODO: attach denotations here
    // if gtree.nonEmpty, then it's a class ctor - should be quite simple
    // if gtree.isEmpty, then gparent is a trait or a module
    // check out how this is handled in the earlier version of scalahost:
    // https://github.com/scalameta/scalahost/blob/master/interface/src/main/scala/scala/meta/internal/hosts/scalac/converters/ToMtree.scala#L50
    mname
  }

  private def mmods(gmods: g.Modifiers, gparents: List[g.Tree]): Seq[m.Mod] = {
    val gmdef = gparents.head.require[g.MemberDef]

    case class ClassParameter(gparam: g.ValDef, gfield: Option[g.ValDef], gctor: g.DefDef, gclass: g.ClassDef)
    val gclassparam: Option[ClassParameter] = {
      val syntactically = {
        gparents match {
          case
            (gparam @ g.ValDef(_, _, _, _)) ::
            (gctor @ g.DefDef(_, g.nme.CONSTRUCTOR, _, _, _, _)) ::
            (gclass @ g.ClassDef(_, _, _, g.Template(_, _, gsiblings))) :: _ =>
              import g.TermNameOps
              val gfield = gsiblings.collectFirst { case gfield: g.ValDef if gfield.name == gparam.name.localName => gfield }
              val gprim = gsiblings.collectFirst { case gprim: g.DefDef if gprim.name == g.nme.CONSTRUCTOR => gprim }
              val isClassParameter = gprim.map(gprim => gctor == gprim).getOrElse(false)
              if (isClassParameter) Some(ClassParameter(gparam, gfield, gctor, gclass))
              else None
          case _ =>
            None
        }
      }
      val semantically = {
        if (gmdef.symbol.owner.isPrimaryConstructor) {
          val gparam = gmdef.require[g.ValDef]
          val gctorSym = gmdef.symbol.owner
          val gctor = g.DefDef(gctorSym, g.EmptyTree)
          val gclassSym = gctorSym.owner
          val gclass = g.ClassDef(gclassSym, g.Template(Nil, g.noSelfType, Nil))
          val ggetterSym = gclassSym.info.member(gmdef.name)
          val gfieldSym = ggetterSym.map(_.owner.info.member(ggetterSym.localName))
          val gfield = if (gfieldSym != g.NoSymbol) Some(g.ValDef(gfieldSym)) else None
          Some(ClassParameter(gparam, gfield, gctor, gclass))
        } else None
      }
      syntactically.orElse(semantically)
    }

    val annotationMods: Seq[m.Mod] = {
      val gsyntacticAnnots = gmods.annotations
      val gsemanticAnnots = gparents.head.symbol.annotations
      mannots(gsyntacticAnnots, gsemanticAnnots, gparents)
    }
    val accessQualifierMods: Seq[m.Mod] = {
      if (gmods.hasFlag(SYNTHETIC) && gmods.hasFlag(ARTIFACT)) {
        // NOTE: some sick artifact vals produced by mkPatDef can be private to method (whatever that means)
        Nil
      } else if (gmods.hasFlag(LOCAL)) {
        // TODO: attach denotations here
        if (gmods.hasFlag(PROTECTED)) List(m.Mod.Protected(m.Term.This(m.Name.Anonymous())))
        else if (gmods.hasFlag(PRIVATE)) List(m.Mod.Private(m.Term.This(m.Name.Anonymous())))
        else unreachable(debug(gmods))
      } else if (gmods.hasAccessBoundary && gmods.privateWithin != g.tpnme.EMPTY) {
        // TODO: attach denotations here
        // TODO: `private[pkg] class C` doesn't have PRIVATE in its flags
        // so we need to account for that!
        val mqualifier = m.Name.Indeterminate(gmods.privateWithin.displayName)
        if (gmods.hasFlag(PROTECTED)) List(m.Mod.Protected(mqualifier))
        else List(m.Mod.Private(mqualifier))
      } else {
        // TODO: attach denotations here
        if (gmods.hasFlag(PROTECTED)) List(m.Mod.Protected(m.Name.Anonymous()))
        else if (gmods.hasFlag(PRIVATE)) List(m.Mod.Private(m.Name.Anonymous()))
        else Nil
      }
    }
    val otherMods: Seq[m.Mod] = {
      val mmods = scala.collection.mutable.ListBuffer[m.Mod]()
      val gmods = gmdef.mods
      if (gmods.hasFlag(IMPLICIT)) mmods += m.Mod.Implicit()
      if (gmods.hasFlag(FINAL)) mmods += m.Mod.Final()
      if (gmods.hasFlag(SEALED)) mmods += m.Mod.Sealed()
      if (gmods.hasFlag(OVERRIDE)) mmods += m.Mod.Override()
      if (gmods.hasFlag(CASE)) mmods += m.Mod.Case()
      if (gmods.hasFlag(ABSTRACT) && gmdef.isInstanceOf[g.ClassDef] && !gmods.hasFlag(TRAIT)) mmods += m.Mod.Abstract()
      if (gmods.hasFlag(ABSOVERRIDE)) { mmods += m.Mod.Abstract(); mmods += m.Mod.Override() }
      if (gmods.hasFlag(COVARIANT) && gmdef.isInstanceOf[g.TypeDef]) mmods += m.Mod.Covariant()
      if (gmods.hasFlag(CONTRAVARIANT) && gmdef.isInstanceOf[g.TypeDef]) mmods += m.Mod.Contravariant()
      if (gmods.hasFlag(LAZY)) mmods += m.Mod.Lazy()
      mmods.toList
    }
    val valVarParamMods: Seq[m.Mod] = {
      val mmods = scala.collection.mutable.ListBuffer[m.Mod]()
      val gfield = gclassparam.flatMap(_.gfield)
      val isImmutableField = gfield.map(!_.mods.hasFlag(MUTABLE)).getOrElse(false)
      val isMutableField = gfield.map(_.mods.hasFlag(MUTABLE)).getOrElse(false)
      val inCaseClass = gclassparam.map(_.gclass).map(_.mods.hasFlag(CASE)).getOrElse(false)
      if (isMutableField) mmods += m.Mod.VarParam()
      if (isImmutableField && !inCaseClass) mmods += m.Mod.ValParam()
      mmods.toList
    }
    val result = annotationMods ++ accessQualifierMods ++ otherMods ++ valVarParamMods

    // NOTE: we can't discern `class C(x: Int)` and `class C(private[this] val x: Int)`
    // so let's err on the side of the more popular option
    if (gclassparam.nonEmpty) result.filter({ case m.Mod.Private(m.Term.This(_)) => false; case _ => true })
    else result
  }

  private def mannots(gannots1: List[g.Tree], gannots2: List[g.AnnotationInfo], gparents: List[g.Tree]): Seq[m.Mod.Annot] = {
    ???
  }

  private def mparams(gtparams: List[g.TypeDef], gvparamss: List[List[g.ValDef]], gparents: List[g.Tree]): (Seq[m.Type.Param], Seq[Seq[m.Term.Param]]) = {
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
    val gvparamss1 = gexplicitss ++ (if (gimplicits.isEmpty) Nil else List(gimplicits))
    (mtrees(gtparams1, gparents).require[Seq[m.Type.Param]], mtreess(gvparamss1, gparents).require[Seq[Seq[m.Term.Param]]])
  }
}