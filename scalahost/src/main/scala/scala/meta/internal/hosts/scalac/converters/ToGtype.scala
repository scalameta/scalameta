package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.ast.Helpers.XtensionTermOps
import scala.meta.internal.hosts.scalac.reflect._

// This module exposes a method to convert from scala.meta types to scala.reflect types.
// The logic is mostly straightforward except for when we need to create symbols for compound and existential types.
trait ToGtype extends ReflectToolkit with MetaToolkit {
  self: Api =>

  protected implicit class XtensionMtypeToGtype(mtpe: m.Type.Arg) {
    private def gannotinfo(mannot: m.Mod.Annot): g.AnnotationInfo = {
      val gtpe = mannot.body.tpe.require[m.Type].toGtype
      val gargss = mannot.body.ctorArgss.map(_.map(_.toGtree))
      if (gargss.length > 1) throw new ConvertException(mannot, s"implementation restriction: annotations with multiple argument lists are not supported by scalac")
      if (gtpe <:< g.definitions.StaticAnnotationClass.tpe) {
        g.AnnotationInfo(gtpe, gargss.head.toList, Nil)
      } else {
        require(gtpe <:< g.definitions.ClassfileAnnotationClass.tpe)
        ???
      }
    }
    private def gannotinfos(mannots: Seq[m.Mod.Annot]): List[g.AnnotationInfo] = {
      mannots.map(gannotinfo).toList
    }
    private def gtypeBounds(mbounds: m.Type.Bounds): g.TypeBounds = {
      val glo = mbounds.lo.map(_.toGtype).getOrElse(g.definitions.NothingClass.tpe)
      val ghi = mbounds.hi.map(_.toGtype).getOrElse(g.definitions.AnyClass.tpe)
      g.TypeBounds(glo, ghi)
    }
    private implicit class RichGlobalSymbol(gsym: g.Symbol) {
      private def mkGterm(name: String, flags: Long) = gsym.newTermSymbol(g.TermName(name), newFlags = flags)
      private def mkGtype(name: String, flags: Long) = gsym.newTypeSymbol(g.TypeName(name), newFlags = flags)
      def mkLabstractVal(name: String) = l.AbstractVal(mkGterm(name, DEFERRED | METHOD | STABLE | ACCESSOR))
      def mkLexistentialVal(name: String)  = l.AbstractType(mkGterm(name + g.nme.SINGLETON_SUFFIX, DEFERRED | EXISTENTIAL))
      def mkLabstractVar(name: String) = l.AbstractVar(mkGterm(name, DEFERRED | METHOD | ACCESSOR), mkGterm(name + g.nme.SETTER_SUFFIX, DEFERRED | METHOD | ACCESSOR))
      def mkLabstractDef(name: String) = l.AbstractDef(mkGterm(name, METHOD))
      def mkLabstractType(name: String) = l.AbstractType(mkGtype(name, DEFERRED))
      def mkLexistentialType(name: String) = l.AbstractType(mkGtype(name, DEFERRED | EXISTENTIAL))
      def mkLtype(name: String) = l.Type(mkGtype(name, 0))
      def mkLtermParameter(name: String) = l.TermParameter(mkGterm(name, PARAM))
      def mkLtypeParameter(name: String) = l.TypeParameter(mkGtype(name, PARAM | DEFERRED))
    }
    private implicit class RichLogicalSymbol(lsym: l.Symbol) {
      private def mimicMods(mmods: Seq[m.Mod], mtree: m.Tree): Unit = {
        val gsym = lsym.gsymbol // TODO: check that this is correct
        mmods.foreach({
          case mmod: m.Mod.Annot => gsym.withAnnotations(List(gannotinfo(mmod)))
          case m.Mod.Private(m.Name.Anonymous()) => gsym.setFlag(PRIVATE)
          case m.Mod.Private(m.Term.This(_)) => gsym.setFlag(PRIVATE | LOCAL)
          case m.Mod.Private(_) => ???
          case m.Mod.Protected(m.Name.Anonymous()) => gsym.setFlag(PROTECTED)
          case m.Mod.Protected(m.Term.This(_)) => gsym.setFlag(PROTECTED | LOCAL)
          case m.Mod.Protected(_) => ???
          case mmod: m.Mod.Implicit => gsym.setFlag(IMPLICIT)
          case mmod: m.Mod.Final => gsym.setFlag(FINAL)
          case mmod: m.Mod.Sealed => gsym.setFlag(SEALED)
          case mmod: m.Mod.Override => gsym.setFlag(OVERRIDE)
          case mmod: m.Mod.Case => gsym.setFlag(CASE)
          case mmod: m.Mod.Abstract => gsym.setFlag(ABSTRACT)
          case mmod: m.Mod.Covariant => gsym.setFlag(COVARIANT)
          case mmod: m.Mod.Contravariant => gsym.setFlag(CONTRAVARIANT)
          case mmod: m.Mod.Lazy => gsym.setFlag(LAZY)
          case mmod: m.Mod.ValParam => // do nothing
          case mmod: m.Mod.VarParam => // do nothing
        })
        // TODO: INTERFACE, MUTABLE, STATIC, PRESUPER, INCONSTRUCTOR, STABLE, *ACCESSOR, EXISTENTIAL
        mtree match { case m.Term.Param(_, _, Some(tpe), _) if tpe.toGtype.typeSymbol == g.definitions.ByNameParamClass => gsym.setFlag(BYNAMEPARAM); case _ => }
        if (gsym.hasFlag(ABSTRACT) && gsym.hasFlag(OVERRIDE)) { gsym.resetFlag(ABSTRACT | OVERRIDE); gsym.setFlag(ABSOVERRIDE) }
        if (mtree.isInstanceOf[m.Defn.Trait]) gsym.setFlag(TRAIT)
        mtree match { case mtree: m.Term.Param if mtree.default.nonEmpty => gsym.setFlag(DEFAULTPARAM); case _ => }
        mtree match { case mtree: m.Defn.Var if mtree.rhs.isEmpty => gsym.setFlag(DEFAULTINIT); case _ => }
      }
      private def gtparams(mtparams: Seq[m.Type.Param]): List[g.Symbol] = {
        mtparams.map(mtparam => {
          val htparam = mtparam.name.require[m.Name].denot.requireSymbol
          val gowner = { require(lsym.gsymbols.length == 1); lsym.gsymbol }
          val ltparam = symbolTable.lookupOrElseUpdate(htparam, gowner.mkLtypeParameter(mtparam.name.toString))
          ltparam.mimic(mtparam).gsymbol
        }).toList
      }
      private def gparams(mparams: Seq[m.Term.Param]): List[g.Symbol] = {
        mparams.map(mparam => {
          val hparam = mparam.name.require[m.Name].denot.requireSymbol
          val gowner = { require(lsym.gsymbols.length == 1); lsym.gsymbol }
          val lparam = symbolTable.lookupOrElseUpdate(hparam, gowner.mkLtermParameter(mparam.name.toString))
          lparam.mimic(mparam).gsymbol
        }).toList
      }
      private def mimicInfo(mtree: m.Tree): Unit = {
        (mtree, lsym) match {
          case (m.Decl.Val(_, _, mtpe), l.AbstractVal(gsym)) =>
            if (gsym.hasFlag(EXISTENTIAL)) {
              val upperBound = g.intersectionType(List(mtpe.toGtype, g.definitions.SingletonClass.tpe), gsym.owner)
              gsym.setInfo(g.TypeBounds.upper(upperBound))
            } else {
              gsym.setInfo(g.NullaryMethodType(mtpe.toGtype))
            }
          case (m.Decl.Var(_, _, mtpe), l.AbstractVar(ggetter, gsetter)) =>
            ggetter.setInfo(g.NullaryMethodType(mtpe.toGtype))
            val gsetterParams = List(gsetter.newTermSymbol(g.TermName("x$1"), newFlags = PARAM).setInfo(mtpe.toGtype))
            val gsetterRet = g.definitions.UnitClass.tpe
            gsetter.setInfo(g.MethodType(gsetterParams, gsetterRet))
          case (m.Decl.Def(_, _, mtparams, mparamss, mtpe), l.AbstractDef(gsym)) =>
            val gprecachedTparams = gtparams(mtparams)
            val gprecachedParamss = mparamss.map(gparams)
            val gmethodType = gprecachedParamss.foldLeft(mtpe.toGtype)((curr, gparams) => g.MethodType(gparams, curr))
            gsym.setInfo(g.genPolyType(gprecachedTparams, gmethodType))
          case (m.Decl.Type(_, _, mtparams, mtypebounds), l.AbstractType(gsym)) =>
            gsym.setInfo(g.genPolyType(gtparams(mtparams), gtypeBounds(mtypebounds)))
          case (m.Defn.Type(_, _, mtparams, mtpe), l.Type(gsym)) =>
            gsym.setInfo(g.genPolyType(gtparams(mtparams), mtpe.toGtype))
          case (m.Type.Param(_, _, mtparams, mtypebounds, mviewbounds, mcontextbounds), l.TypeParameter(gsym)) =>
            require(mcontextbounds.isEmpty && mviewbounds.isEmpty)
            gsym.setInfo(g.genPolyType(gtparams(mtparams), gtypeBounds(mtypebounds)))
          case (m.Term.Param(_, _, mdecltpe, mdefault), l.TermParameter(gsym)) =>
            require(mdecltpe.nonEmpty && mdefault.isEmpty)
            gsym.setInfo(mdecltpe.get.toGtype)
          case _ =>
            ???
        }
      }
      def mimic(mtree: m.Tree): l.Symbol = {
        if (!lsym.gsymbol.hasRawInfo) {
          import scala.language.reflectiveCalls
          mimicMods(mtree.require[{ def mods: Seq[m.Mod] }].mods, mtree)
          mimicInfo(mtree)
        }
        lsym
      }
    }
    private def gowner(mtree: m.Tree): g.Symbol = {
      // TODO: we probably need something other than NoSymbol for RefinedType.decls and ExistentialType.quants
      // I always had no idea about how this works in scala. I guess, it's time for find out :)
      g.NoSymbol
    }
    private def gprefix(hprefix: s.Prefix): g.Type = {
      hprefix match {
        case s.Prefix.Zero => g.NoPrefix
        case s.Prefix.Type(mtpe) => mtpe.require[m.Type].toGtype
      }
    }
    def toGtype: g.Type = tpeCache.getOrElseUpdate(mtpe, {
      def loop(mtpe: m.Type.Arg): g.Type = mtpe match {
        case mname: m.Type.Name =>
          g.TypeRef(gprefix(mname.denot.prefix), symbolTable.convert(mname.denot.requireSymbol).gsymbol, Nil)
        case m.Type.Select(mqual, mname) =>
          g.TypeRef(loop(m.Type.Singleton(mqual)), symbolTable.convert(mname.denot.requireSymbol).gsymbol, Nil)
        case m.Type.Project(mqual, mname) =>
          g.TypeRef(loop(mqual), symbolTable.convert(mname.denot.requireSymbol).gsymbol, Nil)
        case m.Type.Singleton(mref) =>
          def singleType(mname: m.Term.Name): g.Type = {
            val gsym = symbolTable.convert(mname.denot.requireSymbol).gsymbol
            if (gsym.isModuleClass) g.ThisType(gsym)
            else g.SingleType(gprefix(mname.denot.prefix), gsym)
          }
          def superType(msuper: m.Term.Super): g.Type = {
            val gpre = gprefix(msuper.thisp.require[m.Name].denot.prefix)
            val gmixsym = msuper.superp.require[m.Name].denot.requireSymbol match {
              case s.Symbol.Zero => g.intersectionType(gpre.typeSymbol.info.parents)
              case ssym => gpre.typeSymbol.info.baseType(symbolTable.convert(ssym).gsymbol)
            }
            g.SuperType(gpre, gmixsym)
          }
          mref match {
            case mname: m.Term.Name => singleType(mname)
            case m.Term.Select(_, mname) => singleType(mname)
            case mref: m.Term.This => g.ThisType(symbolTable.convert(mref.qual.require[m.Name].denot.requireSymbol).gsymbol)
            case mref: m.Term.Super => superType(mref)
          }
        case m.Type.Apply(mtpe, margs) =>
          g.appliedType(loop(mtpe), margs.map(loop).toList)
        case m.Type.ApplyInfix(mlhs, mop, mrhs) =>
          g.appliedType(loop(mop), List(loop(mlhs), loop(mrhs)))
        case m.Type.Function(mparams, mres) =>
          g.appliedType(g.definitions.FunctionClass(mparams.length + 1), (mparams :+ mres).map(loop).toList)
        case m.Type.Tuple(melements) =>
          g.appliedType(g.definitions.TupleClass(melements.length), melements.map(loop).toList)
        case m.Type.Compound(mtpes, mrefinement) =>
          val gscope = g.newScope
          val mexplodedRefinement = mrefinement.flatMap(mrefine => mrefine.binders.map(mname => mrefine -> mname))
          val refinement = mexplodedRefinement.map({ case (mrefine, mname) =>
            val lrefine = symbolTable.lookupOrElseUpdate(mname.denot.requireSymbol, mrefine match {
              case _: m.Decl.Val => gowner(mrefine).mkLabstractVal(mname.toString)
              case _: m.Decl.Var => gowner(mrefine).mkLabstractVar(mname.toString)
              case _: m.Decl.Def => gowner(mrefine).mkLabstractDef(mname.toString)
              case _: m.Decl.Type => gowner(mrefine).mkLabstractType(mname.toString)
              case _: m.Defn.Type => gowner(mrefine).mkLtype(mname.toString)
              case _ => unreachable(debug(mrefine, mrefine.show[Structure]))
            })
            lrefine.gsymbols.foreach(gscope.enter)
            mrefine -> lrefine
          })
          refinement.foreach({ case (mrefine, lrefine) => lrefine.mimic(mrefine) })
          g.refinedType(mtpes.map(loop).toList, gowner(mtpe), gscope, g.NoPosition)
        case m.Type.Existential(mtpe, mquants) =>
          val mexplodedQuants = mquants.flatMap(mquant => mquant.binders.map(mname => mquant -> mname))
          val quants = mexplodedQuants.map({ case (mquant, mname) =>
            val lquant = symbolTable.lookupOrElseUpdate(mname.denot.requireSymbol, mquant match {
              case _: m.Decl.Val => gowner(mquant).mkLexistentialVal(mname.toString)
              case _: m.Decl.Type => gowner(mquant).mkLexistentialType(mname.toString)
              case _ => unreachable(debug(mquant, mquant.show[Structure]))
            })
            mquant -> lquant
          })
          quants.foreach({ case (mquant, lquant) => lquant.mimic(mquant) })
          g.ExistentialType(quants.flatMap(_._2.gsymbols).toList, loop(mtpe))
        case m.Type.Annotate(mtpe, mannots) =>
          g.AnnotatedType(gannotinfos(mannots), loop(mtpe))
        case m.Type.Placeholder(mbounds) =>
          ???
        case m.Type.Arg.ByName(mtpe) =>
          g.appliedType(g.definitions.ByNameParamClass, List(loop(mtpe)))
        case m.Type.Arg.Repeated(mtpe) =>
          g.appliedType(g.definitions.RepeatedParamClass, List(loop(mtpe)))
        case mlit: m.Lit =>
          require(!mlit.value.isInstanceOf[scala.Symbol])
          g.ConstantType(g.Constant(mlit.value))
      }
      loop(mtpe)
    })
  }
}