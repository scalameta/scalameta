package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.collections._
import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.{meta => papi}
import scala.meta.internal.{ast => p}

// This module exposes a method that can convert scala.reflect symbols into equivalent scala.meta members.
// There are some peculiarities that you'll need to know about it:
//
// 1) The conversion always requires a prefix (i.e. a scala.reflect type), because
// our members track prefixes to avoid accidental mishaps and the inconvenience of typeSignatureIn/asSeenFrom.
// Consequently, the output p.Member might change its structural parts based on the prefix,
// e.g. `t"List".defs("head")` will look like `def head: A = ???`,
// while `t"List[Int]".defs("head")` will look like `def head: Int = ???`.
//
// 2) The conversion actually works not with g.Symbol, but with l.Symbol (aka logical symbol).
// That's because scala.reflect symbols and scala.meta members don't have a one-to-one correspondence
// (e.g. field + getter + setter collapse into a single p.Defn.Var and certain g.Symbols, e.g. $isInstanceOf,
// don't even have a representation in the scala.meta world).
//
// 3) The conversion not only supports lookup within scala.meta ASTs (i.e. AST persistence),
// but it can also operate in legacy mode, where it rebuilds scala.meta-compliant metadata from g.Symbols.
trait ToPmember extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class RichToPmember(lsym: l.Symbol) {
    private def pmods(lsym: l.Symbol): Seq[p.Mod] = {
      def annotationMods(lsym: l.Symbol): Seq[p.Mod] = {
        // TODO: collect annotations scattered over synthetic members
        lsym.gsymbol.annotations.toPannots
      }
      def accessQualifierMods(lsym: l.Symbol): Seq[p.Mod] = {
        val gsym = lsym.gsymbol
        val gpriv = gsym.privateWithin.orElse(gsym.owner)
        if (gsym.hasFlag(LOCAL)) {
          if (gsym.hasFlag(PROTECTED)) List(p.Mod.ProtectedThis().withDenot(gpriv))
          else if (gsym.hasFlag(PRIVATE)) List(p.Mod.PrivateThis().withDenot(gpriv))
          else unreachable
        } else if (gsym.hasAccessBoundary && gpriv != g.NoSymbol) {
          // TODO: `private[pkg] class C` doesn't have PRIVATE in its flags
          // so we need to account for that!
          if (gsym.hasFlag(PROTECTED)) List(p.Mod.ProtectedWithin(gpriv.name.toString).withDenot(gpriv))
          else List(p.Mod.PrivateWithin(gpriv.name.toString).withDenot(gpriv))
        } else {
          if (gsym.hasFlag(PROTECTED)) List(p.Mod.Protected())
          else if (gsym.hasFlag(PRIVATE)) List(p.Mod.Private())
          else Nil
        }
      }
      def otherMods(lsym: l.Symbol): Seq[p.Mod] = {
        val gsym = lsym.gsymbol
        val pmods = scala.collection.mutable.ListBuffer[p.Mod]()
        if (gsym.isImplicit) pmods += p.Mod.Implicit()
        if (gsym.isFinal) pmods += p.Mod.Final()
        if (gsym.isSealed) pmods += p.Mod.Sealed()
        if (gsym.isOverride) pmods += p.Mod.Override()
        if (gsym.isCase) pmods += p.Mod.Case()
        if (gsym.isAbstract && lsym.isInstanceOf[l.Clazz]) pmods += p.Mod.Abstract()
        if (gsym.isAbstractOverride) { pmods += p.Mod.Abstract(); pmods += p.Mod.Override() }
        if (gsym.isCovariant) pmods += p.Mod.Covariant()
        if (gsym.isContravariant) pmods += p.Mod.Contravariant()
        if (gsym.isLazy) pmods += p.Mod.Lazy()
        pmods.toList
      }
      def valVarParamMods(lsym: l.Symbol): Seq[p.Mod] = {
        val pmods = scala.collection.mutable.ListBuffer[p.Mod]()
        val ggetter = lsym.gsymbol.owner.filter(_.isPrimaryConstructor).map(_.owner.info.member(lsym.gsymbol.name))
        val gfield = ggetter.map(_.owner.info.member(ggetter.localName))
        val isApplicable = lsym.gsymbol.owner.isPrimaryConstructor && gfield != g.NoSymbol
        if (isApplicable && gfield.isMutable) pmods += p.Mod.VarParam()
        if (isApplicable && !gfield.isMutable && !gfield.owner.isCase) pmods += p.Mod.ValParam()
        pmods.toList
      }
      val result = annotationMods(lsym) ++ accessQualifierMods(lsym) ++ otherMods(lsym) ++ valVarParamMods(lsym)
      // TODO: we can't discern `class C(x: Int)` and `class C(private[this] val x: Int)`
      // so let's err on the side of the more popular option
      if (lsym.gsymbol.owner.isPrimaryConstructor) result.filter(!_.isInstanceOf[p.Mod.PrivateThis]) else result
    }
    def toPmember(gpre: g.Type): p.Member = lsymToPmemberCache.getOrElseUpdate((gpre, lsym), {
      if (sys.props("member.debug") != null) println((gpre, lsym))
      def approximateSymbol(lsym: l.Symbol): p.Member = {
        // NOTE: we don't need to clear the LOCAL_SUFFIX_STRING from the name of `lsym.gsymbol`
        // because it's always guaranteed not to end with LOCAL_SUFFIX_STRING
        // see LogicalSymbols.scala for more information
        lazy val gsym = lsym.gsymbol
        lazy val ginfo = gsym.moduleClass.orElse(gsym).infoIn(gpre)
        lazy val gtparams = ginfo.typeParams
        lazy val gvparamss = ginfo.paramss
        lazy val gtpe = {
          // NOTE: strips off only those vparams and tparams that are part of the definition
          // we don't want to, for example, damage type lambdas
          def loop(gtpe: g.Type): g.Type = gtpe match {
            case g.NullaryMethodType(gret) =>
              loop(gret)
            case g.MethodType(gvparams, gret) =>
              if (gvparams.forall(gsym => gvparamss.flatten.exists(_ == gsym))) loop(gret)
              else gtpe
            case g.PolyType(gtparams, gret) =>
              if (gtparams.forall(gsym => gtparams.exists(_ == gsym))) loop(gret)
              else gret
            case _ =>
              gtpe
          }
          loop(ginfo)
        }
        lazy val pmods = this.pmods(lsym)
        lazy val pname = lsym match {
          case l.PrimaryCtor(gsym) => p.Ctor.Name(gsym.owner.name.toString).withDenot(gpre, gsym).withOriginal(gsym)
          case l.SecondaryCtor(gsym) => p.Ctor.Name(gsym.owner.name.toString).withDenot(gpre, gsym).withOriginal(gsym)
          case l.TermParameter(gsym) => gsym.anoncvt(g.Ident(gsym))
          case l.TypeParameter(gsym) => gsym.anoncvt(g.Ident(gsym))
          case _ => gsym.precvt(gpre, g.Ident(gsym))
        }
        lazy val ptparams = gtparams.map(gtparam => l.TypeParameter(gtparam).toPmember(g.NoPrefix).asInstanceOf[p.Type.Param])
        lazy val pvparamss = gvparamss.map(_.map(gvparam => l.TermParameter(gvparam).toPmember(g.NoPrefix).asInstanceOf[p.Term.Param]))
        lazy val ptpe = gtpe.toPtype
        lazy val ptpeBounds = gtpe match {
          case gtpe @ g.TypeBounds(glo, ghi) =>
            val plo = if (glo =:= g.typeOf[Nothing]) None else Some(glo.toPtype.asInstanceOf[p.Type])
            val phi = if (ghi =:= g.typeOf[Any]) None else Some(ghi.toPtype.asInstanceOf[p.Type])
            p.Type.Bounds(plo, phi).withOriginal(gtpe)
        }
        lazy val punknownTerm = {
          // TODO: think of something both concise and distinctive
          // val gsys = g.definitions.SysPackage
          // val gsysError = gsys.info.decl(g.TermName("error"))
          // val psysError = p.Term.Select(p.Term.Name("sys").withDenot(gsys), p.Term.Name("error").withDenot(gsys))
          // p.Term.Apply(psysError, List(p.Lit.String("couldn't load tree"))).withOriginal(g.definitions.NothingClass.tpe)
          p.Term.Name("???").withDenot(g.definitions.Predef_???).withOriginal(g.definitions.Predef_???)
        }
        lazy val pbody = lsym match {
          case l.SecondaryCtor(gsym) =>
            val gctor = gsym.owner.primaryConstructor
            val pctorref = p.Ctor.Name(gsym.owner.name.toString).withDenot(gpre, gctor).withOriginal(gctor)
            p.Term.Apply(pctorref, List(punknownTerm))
          case _ =>
            punknownTerm
        }
        lazy val pmaybeBody = if (gsym.hasFlag(DEFAULTINIT)) None else Some(pbody)
        lazy val pfakector = {
          val pname = p.Ctor.Name(gsym.name.toString).withDenot(gpre, gsym)
          p.Ctor.Primary(Nil, pname, Nil)
        }
        lazy val pctor = {
          if (lsym.isInstanceOf[l.Clazz] || lsym.isInstanceOf[l.Object]) {
            val gctorsym = lsym.gsymbol.primaryConstructor
            val gctorinfo = gctorsym.infoIn(gpre)
            val pctorname = p.Ctor.Name(gsym.name.toString).withDenot(gpre, gctorsym).withOriginal(gctorsym)
            val pctorparamss = {
              if (lsym.isInstanceOf[l.Clazz]) gctorinfo.paramss.map(_.map(gvparam => l.TermParameter(gvparam).toPmember(g.NoPrefix).asInstanceOf[p.Term.Param]))
              else Nil // NOTE: synthetic constructors for modules have a fake List(List()) parameter list
            }
            p.Ctor.Primary(this.pmods(l.PrimaryCtor(gctorsym)), pctorname, pctorparamss)
          } else {
            pfakector
          }
        }
        lazy val ptemplate = {
          val (pearly, plate) = pstats.partition(_.originalSym match {
            case Some(l.Val(gfield, _)) => gfield.hasFlag(PRESUPER)
            case Some(l.Var(gfield, _, _)) => gfield.hasFlag(PRESUPER)
            case _ => false
          })
          val gparents = ginfo match {
            case g.ClassInfoType(gparents, _, _) => gparents
            case g.PolyType(_, g.ClassInfoType(gparents, _, _)) => gparents
          }
          val pparents = gparents.map(gparent => {
            val ptpe = gparent.toPtype.asInstanceOf[p.Type]
            val gctor = gparent.typeSymbol.primaryConstructor.orElse(gparent.typeSymbol)
            ptpe.ctorRef(gctor)
          })
          // TODO: apply gpre to pselftpe
          val pselftpe = if (gsym.thisSym != gsym) Some(gsym.thisSym.tpe.toPtype.asInstanceOf[p.Type]) else None
          val pself = p.Term.Param(Nil, p.Name.Anonymous(), pselftpe, None)
          p.Template(pearly, pparents, pself, Some(plate))
        }
        lazy val pstats = LazySeq({
          val gstatowner = gsym match { case gclass: g.ClassSymbol => gclass; case gmodule: g.ModuleSymbol => gmodule.moduleClass.asClass }
          val gstatpre = gstatowner.toTypeIn(gpre)
          val ldecls = gstatpre.decls.toLogical
          val lcensoredDecls = ldecls.filter(!_.isInstanceOf[l.PrimaryCtor])
          lcensoredDecls.map(_.toPmember(gstatpre)).map(_.stat)
        })
        lazy val pmaybeDefault = if (gsym.hasFlag(DEFAULTPARAM)) Some(pbody) else None
        lazy val pviewBounds = {
          val gevidences = gsym.owner.paramss.flatten.filter(_.name.startsWith(g.nme.EVIDENCE_PARAM_PREFIX))
          val gviewBounds = gevidences.map(gev => gev.tpe.typeArgs match {
            // TODO: hygiene!
            case List(gfrom, gto) if gfrom.typeSymbol.name == gsym.name => gto.typeSymbol
            case _ => g.NoSymbol
          }).filter(_ != g.NoSymbol)
          gviewBounds.map(gbound => gbound.asType.rawcvt(g.Ident(gbound)))
        }
        lazy val pcontextBounds = {
          val gevidences = gsym.owner.paramss.flatten.filter(_.name.startsWith(g.nme.EVIDENCE_PARAM_PREFIX))
          val gcontextBounds = gevidences.map(gev => gev.tpe.typeArgs match {
            // TODO: hygiene!
            case List(gtarg) if gtarg.typeSymbol.name == gsym.name => gev.tpe.typeSymbol
            case _ => g.NoSymbol
          }).filter(_ != g.NoSymbol)
          gcontextBounds.map(gbound => gbound.asType.rawcvt(g.Ident(gbound)))
        }
        val pmember: p.Member = lsym match {
          case l.None => unreachable
          case _: l.AbstractVal => p.Decl.Val(pmods, List(p.Pat.Var.Term(pname.asInstanceOf[p.Term.Name])), ptpe.asInstanceOf[p.Type]).member
          case _: l.AbstractVar => p.Decl.Var(pmods, List(p.Pat.Var.Term(pname.asInstanceOf[p.Term.Name])), ptpe.asInstanceOf[p.Type]).member
          case _: l.AbstractDef => p.Decl.Def(pmods, pname.asInstanceOf[p.Term.Name], ptparams, pvparamss, ptpe.asInstanceOf[p.Type])
          case _: l.AbstractType => p.Decl.Type(pmods, pname.asInstanceOf[p.Type.Name], ptparams, ptpeBounds)
          case _: l.Val => p.Defn.Val(pmods, List(p.Pat.Var.Term(pname.asInstanceOf[p.Term.Name])), Some(ptpe.asInstanceOf[p.Type]), pbody).member
          case _: l.Var => p.Defn.Var(pmods, List(p.Pat.Var.Term(pname.asInstanceOf[p.Term.Name])), Some(ptpe.asInstanceOf[p.Type]), pmaybeBody).member
          case _: l.Def => p.Defn.Def(pmods, pname.asInstanceOf[p.Term.Name], ptparams, pvparamss, Some(ptpe.asInstanceOf[p.Type]), pbody)
          case _: l.Macro => p.Defn.Def(pmods, pname.asInstanceOf[p.Term.Name], ptparams, pvparamss, Some(ptpe.asInstanceOf[p.Type]), pbody)
          case _: l.Type => p.Defn.Type(pmods, pname.asInstanceOf[p.Type.Name], ptparams, ptpe.asInstanceOf[p.Type])
          case _: l.Clazz => p.Defn.Class(pmods, pname.asInstanceOf[p.Type.Name], ptparams, pctor, ptemplate)
          case _: l.Trait => p.Defn.Trait(pmods, pname.asInstanceOf[p.Type.Name], ptparams, pctor, ptemplate)
          case _: l.Object => p.Defn.Object(pmods, pname.asInstanceOf[p.Term.Name], pctor, ptemplate)
          case _: l.Package => p.Pkg(pname.asInstanceOf[p.Term.Name], pstats)
          case _: l.PackageObject => p.Pkg.Object(pmods, pname.asInstanceOf[p.Term.Name], pctor, ptemplate)
          case _: l.PrimaryCtor => p.Ctor.Primary(pmods, pname.asInstanceOf[p.Ctor.Name], pvparamss)
          case _: l.SecondaryCtor => p.Ctor.Secondary(pmods, pname.asInstanceOf[p.Ctor.Name], pvparamss, pbody)
          case _: l.TermBind => p.Pat.Var.Term(pname.asInstanceOf[p.Term.Name])
          case _: l.TypeBind => p.Pat.Var.Type(pname.asInstanceOf[p.Type.Name])
          case _: l.TermParameter => p.Term.Param(pmods, pname.asInstanceOf[papi.Term.Name], Some(ptpe), pmaybeDefault)
          case _: l.TypeParameter => p.Type.Param(pmods, pname.asInstanceOf[papi.Type.Name], ptparams, ptpeBounds, pviewBounds, pcontextBounds)
          case _ => sys.error(s"unsupported symbol $lsym, designation = ${gsym.getClass}, flags = ${gsym.flags}")
        }
        pmember.withOriginal(lsym)
      }
      def applyPrefix(gpre: g.Type, pmem: p.Member): p.Member = {
        if (gpre == g.NoPrefix) pmem
        else {
          // TODO: implement me! it might not be that hard, to be honest
          // 1) Replace Type.Name(tparam) in pmem and its denotations with values obtained from gpre
          // 2) Replace Term.This(pmem.owner) in pmem and its denotations with apply(gpre)
          pmem
        }
      }
      val hsym = symbolTable.convert(lsym)
      val maybeSourceNativePmember = hsymToNativePmemberCache.get(hsym)
      val maybeNativePmember = maybeSourceNativePmember.map(applyPrefix(gpre, _))
      maybeNativePmember.getOrElse(approximateSymbol(lsym))
    })
  }
}