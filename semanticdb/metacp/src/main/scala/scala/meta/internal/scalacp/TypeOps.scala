package scala.meta.internal.scalacp

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.tools.scalap.scalax.rules.scalasig._

trait TypeOps { self: Scalacp =>
  implicit class XtensionTypeSType(tpe: Type) {
    def toSemanticTpe: s.Type = {
      def loop(tpe: Type): s.Type = {
        tpe match {
          case ByNameType(tpe) =>
            val stpe = loop(tpe)
            s.ByNameType(stpe)
          case RepeatedType(tpe) =>
            val stpe = loop(tpe)
            s.RepeatedType(stpe)
          case TypeRefType(pre, sym, args) =>
            val spre = if (tpe.hasTrivialPrefix) s.NoType else loop(pre)
            val ssym = sym.ssym
            val sargs = args.map(loop)
            s.TypeRef(spre, ssym, sargs)
          case SingleType(pre, sym) =>
            val spre = if (tpe.hasTrivialPrefix) s.NoType else loop(pre)
            val ssym = {
              // NOTE: Due to some unclear reason, Scalac sometimes saves
              // (or Scalap sometimes loads) single types that point to
              // companion classes, not module classes (see #1392).
              // We assume that it's a mistake and work around accordingly.
              val raw = sym.ssym
              if (raw.isType) Symbols.Global(raw.owner, d.Term(raw.desc.value))
              else raw
            }
            s.SingleType(spre, ssym)
          case ThisType(sym) =>
            val ssym = sym.ssym
            s.ThisType(ssym)
          // // FIXME: https://github.com/scalameta/scalameta/issues/1291
          // case g.SuperType(gpre, gmix) =>
          //   ???
          // // FIXME: Could it be that this missing pattern matching case may have been
          // // the reason for mysterious Metacp crashes?
          // // https://github.com/scalameta/scalameta/issues/1494
          // case g.ConstantType(g.Constant(sym: g.TermSymbol)) if sym.hasFlag(gf.JAVA_ENUM) =>
          //   ???
          case ConstantType(underlying: Type) =>
            loop(underlying) match {
              case s.NoType => s.NoType
              case sarg =>
                val ssym = "java/lang/Class#"
                val sargs = sarg :: Nil
                s.TypeRef(s.NoType, ssym, sargs)
            }
          case ConstantType(const) =>
            val sconst = s.Constant(const)
            s.ConstantType(sconst)
          case RefinedType(sym, parents) =>
            val sparents = parents.map(loop)
            val stpe = s.WithType(sparents)
            val sdecls = Some(sym.children.sscope(HardlinkChildren))
            s.StructuralType(stpe, sdecls)
          case AnnotatedType(tpe, anns) =>
            val sanns = anns.reverse.map(_.toSemantic)
            val stpe = loop(tpe)
            s.AnnotatedType(sanns, stpe)
          case ExistentialType(tpe, tparams) =>
            val stpe = loop(tpe)
            val sdecls = Some(tparams.sscope(HardlinkChildren))
            s.ExistentialType(stpe, sdecls)
          case PolyType(tpe, tparams) =>
            loop(tpe) match {
              case s.NoType =>
                s.NoType
              case stpe =>
                val stparams = tparams.sscope(HardlinkChildren)
                s.UniversalType(Some(stparams), stpe)
            }
          case NoType =>
            s.NoType
          case NoPrefixType =>
            s.NoType
          case other =>
            sys.error(s"unsupported type $other")
        }
      }
      loop(tpe)
    }
    def toSemanticSig(linkMode: LinkMode): s.Signature = {
      def loop(tpe: Type): s.Signature = {
        tpe match {
          case ClassInfoType(sym, parents) =>
            val stparams = Some(s.Scope())
            val sparents = parents.map(_.toSemanticTpe)
            val sself = sym.self.toSemanticTpe
            val sdecls = Some(sym.semanticdbDecls.sscope(linkMode))
            s.ClassSignature(stparams, sparents, sself, sdecls)
          case _: NullaryMethodType | _: MethodType =>
            val stparams = Some(s.Scope())
            val sparamss = tpe.paramss.map(_.sscope(linkMode))
            val sret = tpe.ret.toSemanticTpe
            s.MethodSignature(stparams, sparamss, sret)
          case TypeBoundsType(lo, hi) =>
            val stparams = Some(s.Scope())
            val slo = lo.toSemanticTpe
            val shi = hi.toSemanticTpe
            s.TypeSignature(stparams, slo, shi)
          case PolyType(tpe, tparams) =>
            loop(tpe) match {
              case s.NoSignature =>
                s.NoSignature
              case t: s.ClassSignature =>
                val stparams = tparams.sscope(linkMode)
                t.copy(typeParameters = Some(stparams))
              case t: s.MethodSignature =>
                val stparams = tparams.sscope(linkMode)
                t.copy(typeParameters = Some(stparams))
              case t: s.TypeSignature =>
                val stparams = tparams.sscope(linkMode)
                t.copy(typeParameters = Some(stparams))
              case t: s.ValueSignature =>
                val stparams = tparams.sscope(HardlinkChildren)
                val stpe = t.tpe
                s.ValueSignature(s.UniversalType(Some(stparams), stpe))
            }
          case NoType =>
            s.NoSignature
          case other =>
            s.ValueSignature(other.toSemanticTpe)
        }
      }
      loop(tpe)
    }
  }

  implicit class XtensionType(tpe: Type) {
    def hasTrivialPrefix: Boolean = {
      def checkTrivialPrefix(pre: Type, sym: Symbol): Boolean = {
        pre match {
          case TypeRefType(prepre, presym, _) =>
            checkTrivialPrefix(prepre, presym) &&
              checkTrivialOwner(presym, sym) &&
              checkModule(presym)
          case SingleType(prepre, presym) =>
            checkTrivialPrefix(prepre, presym) &&
              checkTrivialOwner(presym, sym)
          case ThisType(presym) =>
            checkTrivialOwner(presym, sym)
          case NoPrefixType =>
            true
          case _ =>
            false
        }
      }
      def checkTrivialOwner(presym: Symbol, sym: Symbol): Boolean = {
        sym.parent match {
          case Some(owner) => presym.path == owner.path
          case None => true
        }
      }
      def checkModule(presym: Symbol): Boolean = {
        presym match {
          case presym: SymbolInfoSymbol => presym.isModule
          case presym: ExternalSymbol => presym.entry.entryType == 10
          case _ => false
        }
      }
      tpe match {
        case TypeRefType(pre, sym, _) => checkTrivialPrefix(pre, sym)
        case SingleType(pre, sym) => checkTrivialPrefix(pre, sym)
        case _ => false
      }
    }
    def paramss: List[List[SymbolInfoSymbol]] = {
      tpe match {
        case NullaryMethodType(_) =>
          Nil
        case MethodType(tpe, params) =>
          val symbolInfoParams = params.map(_.asInstanceOf[SymbolInfoSymbol])
          symbolInfoParams.toList +: tpe.paramss
        case _ =>
          Nil
      }
    }
    def ret: Type = {
      tpe match {
        case NullaryMethodType(tpe) => tpe.ret
        case MethodType(tpe, _) => tpe.ret
        case _ => tpe
      }
    }
  }

  private object ByNameType {
    def unapply(tpe: Type): Option[Type] = {
      tpe match {
        case TypeRefType(_, sym, List(tpe)) if sym.name == "<byname>" => Some(tpe)
        case _ => None
      }
    }
  }

  private object RepeatedType {
    def unapply(tpe: Type): Option[Type] = {
      tpe match {
        case TypeRefType(_, sym, List(tpe)) if sym.name == "<repeated>" => Some(tpe)
        case _ => None
      }
    }
  }
}
