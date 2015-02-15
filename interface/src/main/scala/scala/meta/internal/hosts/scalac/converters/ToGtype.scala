package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.meta.internal.{ast => p}
import scala.meta.internal.{hygiene => h}
import scala.meta.ui.{Exception => SemanticException}

// This module exposes a method to convert from scala.meta types to scala.reflect types.
// The logic is mostly straightforward except for when we need to create symbols for compound and existential types.
trait ToGtype extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class ToGtype(ptpe: p.Type.Arg) {
    private def gannotinfo(pannot: p.Mod.Annot): g.AnnotationInfo = {
      val gtpe = pannot.tree.ctorTpe.toGtype
      val gargss = pannot.tree.ctorArgss.map(_.map(_.toGtree))
      if (gargss.length > 1) throw new SemanticException(s"implementation restriction: annotations with multiple argument lists are not supported by scalac")
      if (gtpe <:< g.definitions.StaticAnnotationClass.tpe) {
        g.AnnotationInfo(gtpe, gargss.head.toList, Nil)
      } else {
        require(gtpe <:< g.definitions.ClassfileAnnotationClass.tpe)
        ???
      }
    }
    private def gannotinfos(pannots: Seq[p.Mod.Annot]): List[g.AnnotationInfo] = {
      pannots.map(gannotinfo).toList
    }
    private def gtypeBounds(pbounds: p.Type.Bounds): g.TypeBounds = {
      val glo = pbounds.lo.map(_.toGtype).getOrElse(g.definitions.NothingClass.tpe)
      val ghi = pbounds.hi.map(_.toGtype).getOrElse(g.definitions.AnyClass.tpe)
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
      private def mimicMods(pmods: Seq[p.Mod], ptree: p.Tree): Unit = {
        val gsym = lsym.gsymbol // TODO: check that this is correct
        pmods.foreach({
          case pmod: p.Mod.Annot => gsym.withAnnotations(List(gannotinfo(pmod)))
          case pmod: p.Mod.Private => gsym.setFlag(PRIVATE)
          case pmod: p.Mod.PrivateThis => gsym.setFlag(LOCAL)
          case pmod: p.Mod.PrivateWithin => ???
          case pmod: p.Mod.Protected => gsym.setFlag(PROTECTED)
          case pmod: p.Mod.ProtectedThis => gsym.setFlag(LOCAL)
          case pmod: p.Mod.ProtectedWithin => ???
          case pmod: p.Mod.Implicit => gsym.setFlag(IMPLICIT)
          case pmod: p.Mod.Final => gsym.setFlag(FINAL)
          case pmod: p.Mod.Sealed => gsym.setFlag(SEALED)
          case pmod: p.Mod.Override => gsym.setFlag(OVERRIDE)
          case pmod: p.Mod.Case => gsym.setFlag(CASE)
          case pmod: p.Mod.Abstract => gsym.setFlag(ABSTRACT)
          case pmod: p.Mod.Covariant => gsym.setFlag(COVARIANT)
          case pmod: p.Mod.Contravariant => gsym.setFlag(CONTRAVARIANT)
          case pmod: p.Mod.Lazy => gsym.setFlag(LAZY)
          case pmod: p.Mod.ValParam => // do nothing
          case pmod: p.Mod.VarParam => // do nothing
        })
        // TODO: INTERFACE, MUTABLE, STATIC, PRESUPER, INCONSTRUCTOR, STABLE, *ACCESSOR, EXISTENTIAL
        ptree match { case p.Term.Param(_, _, Some(tpe), _) if tpe.toGtype.typeSymbol == g.definitions.ByNameParamClass => gsym.setFlag(BYNAMEPARAM); case _ => }
        if (gsym.hasFlag(ABSTRACT) && gsym.hasFlag(OVERRIDE)) { gsym.resetFlag(ABSTRACT | OVERRIDE); gsym.setFlag(ABSOVERRIDE) }
        if (ptree.isInstanceOf[p.Defn.Trait]) gsym.setFlag(TRAIT)
        ptree match { case ptree: p.Term.Param if ptree.default.nonEmpty => gsym.setFlag(DEFAULTPARAM); case _ => }
        ptree match { case ptree: p.Defn.Var if ptree.rhs.isEmpty => gsym.setFlag(DEFAULTINIT); case _ => }
      }
      private def gtparams(ptparams: Seq[p.Type.Param]): List[g.Symbol] = {
        ptparams.map(ptparam => {
          val htparam = ptparam.name.require[p.Name].denot.symbol
          val gowner = { require(lsym.gsymbols.length == 1); lsym.gsymbol }
          val ltparam = symbolTable.lookupOrElseUpdate(htparam, gowner.mkLtypeParameter(ptparam.name.toString))
          ltparam.mimic(ptparam).gsymbol
        }).toList
      }
      private def gparams(pparams: Seq[p.Term.Param]): List[g.Symbol] = {
        pparams.map(pparam => {
          val hparam = pparam.name.require[p.Name].denot.symbol
          val gowner = { require(lsym.gsymbols.length == 1); lsym.gsymbol }
          val lparam = symbolTable.lookupOrElseUpdate(hparam, gowner.mkLtermParameter(pparam.name.toString))
          lparam.mimic(pparam).gsymbol
        }).toList
      }
      private def mimicInfo(ptree: p.Tree): Unit = {
        (ptree, lsym) match {
          case (p.Decl.Val(_, _, ptpe), l.AbstractVal(gsym)) =>
            if (gsym.hasFlag(EXISTENTIAL)) {
              val upperBound = g.intersectionType(List(ptpe.toGtype, g.definitions.SingletonClass.tpe), gsym.owner)
              gsym.setInfo(g.TypeBounds.upper(upperBound))
            } else {
              gsym.setInfo(g.NullaryMethodType(ptpe.toGtype))
            }
          case (p.Decl.Var(_, _, ptpe), l.AbstractVar(ggetter, gsetter)) =>
            ggetter.setInfo(g.NullaryMethodType(ptpe.toGtype))
            val gsetterParams = List(gsetter.newTermSymbol(g.TermName("x$1"), newFlags = PARAM).setInfo(ptpe.toGtype))
            val gsetterRet = g.definitions.UnitClass.tpe
            gsetter.setInfo(g.MethodType(gsetterParams, gsetterRet))
          case (p.Decl.Def(_, _, ptparams, pparamss, ptpe), l.AbstractDef(gsym)) =>
            val gprecachedTparams = gtparams(ptparams)
            val gprecachedParamss = pparamss.map(gparams)
            val gmethodType = gprecachedParamss.foldLeft(ptpe.toGtype)((curr, gparams) => g.MethodType(gparams, curr))
            gsym.setInfo(g.genPolyType(gprecachedTparams, gmethodType))
          case (p.Decl.Type(_, _, ptparams, ptypeBounds), l.AbstractType(gsym)) =>
            gsym.setInfo(g.genPolyType(gtparams(ptparams), gtypeBounds(ptypeBounds)))
          case (p.Defn.Type(_, _, ptparams, ptpe), l.Type(gsym)) =>
            gsym.setInfo(g.genPolyType(gtparams(ptparams), ptpe.toGtype))
          case (p.Type.Param(_, _, ptparams, ptypeBounds, pviewBounds, pcontextBounds), l.TypeParameter(gsym)) =>
            require(pcontextBounds.isEmpty && pviewBounds.isEmpty)
            gsym.setInfo(g.genPolyType(gtparams(ptparams), gtypeBounds(ptypeBounds)))
          case (p.Term.Param(_, _, pdecltpe, pdefault), l.TermParameter(gsym)) =>
            require(pdecltpe.nonEmpty && pdefault.isEmpty)
            gsym.setInfo(pdecltpe.get.toGtype)
          case _ =>
            ???
        }
      }
      def mimic(ptree: p.Tree): l.Symbol = {
        if (!lsym.gsymbol.hasRawInfo) {
          import scala.language.reflectiveCalls
          mimicMods(ptree.require[{ def mods: Seq[p.Mod] }].mods, ptree)
          mimicInfo(ptree)
        }
        lsym
      }
    }
    private def gowner(ptree: p.Tree): g.Symbol = {
      // TODO: we probably need something other than NoSymbol for RefinedType.decls and ExistentialType.quants
      // I always had no idea about how this works in scala. I guess, it's time for find out :)
      g.NoSymbol
    }
    private def gprefix(hprefix: h.Prefix): g.Type = {
      hprefix match {
        case h.Prefix.Zero => g.NoPrefix
        case h.Prefix.Type(ptpe) => ptpe.require[p.Type].toGtype
      }
    }
    def toGtype: g.Type = tpeCache.getOrElseUpdate(ptpe, {
      def loop(ptpe: p.Type.Arg): g.Type = ptpe match {
        case pname: p.Type.Name =>
          g.TypeRef(gprefix(pname.denot.prefix), symbolTable.convert(pname.denot.symbol).gsymbol, Nil)
        case p.Type.Select(pqual, pname) =>
          g.TypeRef(loop(p.Type.Singleton(pqual)), symbolTable.convert(pname.denot.symbol).gsymbol, Nil)
        case p.Type.Project(pqual, pname) =>
          g.TypeRef(loop(pqual), symbolTable.convert(pname.denot.symbol).gsymbol, Nil)
        case p.Type.Singleton(pref) =>
          def singleType(pname: p.Term.Name): g.Type = {
            val gsym = symbolTable.convert(pname.denot.symbol).gsymbol
            if (gsym.isModuleClass) g.ThisType(gsym)
            else g.SingleType(gprefix(pname.denot.prefix), gsym)
          }
          def superType(psuper: p.Term.Super): g.Type = {
            val gpre = gprefix(psuper.denot.prefix)
            val gmixsym = psuper.denot.symbol match {
              case h.Symbol.Zero => g.intersectionType(gpre.typeSymbol.info.parents)
              case hsym => gpre.typeSymbol.info.baseType(symbolTable.convert(hsym).gsymbol)
            }
            g.SuperType(gpre, gmixsym)
          }
          pref match {
            case pname: p.Term.Name => singleType(pname)
            case p.Term.Select(_, pname) => singleType(pname)
            case pref: p.Term.This => g.ThisType(symbolTable.convert(pref.denot.symbol).gsymbol)
            case pref: p.Term.Super => superType(pref)
          }
        case p.Type.Apply(ptpe, pargs) =>
          g.appliedType(loop(ptpe), pargs.map(loop).toList)
        case p.Type.ApplyInfix(plhs, pop, prhs) =>
          g.appliedType(loop(pop), List(loop(plhs), loop(prhs)))
        case p.Type.Function(pparams, pres) =>
          g.appliedType(g.definitions.FunctionClass(pparams.length + 1), (pparams :+ pres).map(loop).toList)
        case p.Type.Tuple(pelements) =>
          g.appliedType(g.definitions.TupleClass(pelements.length), pelements.map(loop).toList)
        case p.Type.Compound(ptpes, prefinement) =>
          val gscope = g.newScope
          val pexplodedRefinement = prefinement.flatMap(prefine => prefine.binders.map(pname => prefine -> pname))
          val refinement = pexplodedRefinement.map({ case (prefine, pname) =>
            val lrefine = symbolTable.lookupOrElseUpdate(pname.denot.symbol, prefine match {
              case _: p.Decl.Val => gowner(prefine).mkLabstractVal(pname.toString)
              case _: p.Decl.Var => gowner(prefine).mkLabstractVar(pname.toString)
              case _: p.Decl.Def => gowner(prefine).mkLabstractDef(pname.toString)
              case _: p.Decl.Type => gowner(prefine).mkLabstractType(pname.toString)
              case _: p.Defn.Type => gowner(prefine).mkLtype(pname.toString)
              case _ => unreachable
            })
            lrefine.gsymbols.foreach(gscope.enter)
            prefine -> lrefine
          })
          refinement.foreach({ case (prefine, lrefine) => lrefine.mimic(prefine) })
          g.refinedType(ptpes.map(loop).toList, gowner(ptpe), gscope, g.NoPosition)
        case p.Type.Existential(ptpe, pquants) =>
          val pexplodedQuants = pquants.flatMap(pquant => pquant.binders.map(pname => pquant -> pname))
          val quants = pexplodedQuants.map({ case (pquant, pname) =>
            val lquant = symbolTable.lookupOrElseUpdate(pname.denot.symbol, pquant match {
              case _: p.Decl.Val => gowner(pquant).mkLexistentialVal(pname.toString)
              case _: p.Decl.Type => gowner(pquant).mkLexistentialType(pname.toString)
              case _ => unreachable
            })
            pquant -> lquant
          })
          quants.foreach({ case (pquant, lquant) => lquant.mimic(pquant) })
          g.ExistentialType(quants.flatMap(_._2.gsymbols).toList, loop(ptpe))
        case p.Type.Annotate(ptpe, pannots) =>
          g.AnnotatedType(gannotinfos(pannots), loop(ptpe))
        case p.Type.Placeholder(pbounds) =>
          ???
        case p.Type.Arg.ByName(ptpe) =>
          g.appliedType(g.definitions.ByNameParamClass, List(loop(ptpe)))
        case p.Type.Arg.Repeated(ptpe) =>
          g.appliedType(g.definitions.RepeatedParamClass, List(loop(ptpe)))
        case plit: p.Lit =>
          plit match {
            case p.Lit.Bool(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Int(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Long(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Float(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Double(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Char(value) => g.ConstantType(g.Constant(value))
            case p.Lit.String(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Symbol(value) => unreachable
            case p.Lit.Null() => g.ConstantType(g.Constant(null))
            case p.Lit.Unit() => g.ConstantType(g.Constant(()))
          }
      }
      ptpe.requireAttributed()
      loop(ptpe)
    })
  }
}