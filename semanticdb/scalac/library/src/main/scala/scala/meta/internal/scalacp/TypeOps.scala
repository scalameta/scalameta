package scala.meta.internal.scalacp

import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.{KnownSymbols => ks}
import scala.meta.internal.{semanticdb => s}

import scala.tools.scalap.scalax.rules.scalasig._

trait TypeOps {
  self: Scalacp =>
  import TypeOps._
  implicit class XtensionTypeSType(tpe: Type) {
    def toSemanticTpe: s.Type = {
      // A repeated type (`T*`) is only valid in parameter position; scalac widens it to
      // `scala.Seq[T]` everywhere else (e.g. the synthetic `unapplySeq` return), but leaves it
      // un-widened in some pickled positions such as a case class's synthetic
      // `AbstractFunction1[T*, C]` parent. Widen any nested repeated so we never emit a
      // RepeatedType as a type argument. The RepeatedType extractor below matches only the Scala
      // `<repeated>` marker (Java varargs in classfiles are read by Javacp, not scalacp), so this
      // only ever widens a Scala repeated -> `Seq[T]`. See scalameta/scalameta#1497.
      def widenRepeated(tpe: Type): s.Type = tpe match {
        case RepeatedType(elem) => s.TypeRef(s.NoType, ks.Seq, loop(elem) :: Nil)
        case _ => loop(tpe)
      }
      def loop(tpe: Type): s.Type = tpe match {
        case ByNameType(tpe) =>
          val stpe = loop(tpe)
          s.ByNameType(stpe)
        case RepeatedType(tpe) =>
          val stpe = loop(tpe)
          s.RepeatedType(stpe)
        case tpe: TypeRefType =>
          val spre = if (tpe.hasTrivialPrefix) s.NoType else loop(tpe.prefix)
          val ssym = tpe.symbol.ssym
          val sargs = tpe.typeArgs.map(widenRepeated)
          s.TypeRef(spre, ssym, sargs)
        case tpe: SingleType =>
          val spre = if (tpe.hasTrivialPrefix) s.NoType else loop(tpe.typeRef)
          val ssym = {
            // NOTE: Due to some unclear reason, Scalac sometimes saves
            // (or Scalap sometimes loads) single types that point to
            // companion classes, not module classes (see #1392).
            // We assume that it's a mistake and work around accordingly.
            val raw = tpe.symbol.ssym
            if (raw.isType) Symbols.Global(raw.owner, d.Term(raw.desc.value)) else raw
          }
          s.SingleType(spre, ssym)
        case tpe: ThisType =>
          val ssym = tpe.symbol.ssym
          s.ThisType(ssym)
        // NOTE: There is deliberately no SuperType case. SuperType (`This.super[Mixin]`) is the
        // type of `super` references, an expression-level type that scalac never pickles (pickles
        // store declaration signatures, not method bodies), and scalap's pickle reader has no case
        // for the SUPERtpe pickle tag (scala.reflect.internal.pickling.PickleFormat) -- so metacp
        // can never receive one. The scalac/TextDocumentOps path handles SuperType because it walks
        // typed source trees. See https://github.com/scalameta/scalameta/issues/1291.
        case tpe: ConstantType => tpe.constant match {
            case c: ExternalSymbol =>
              // NOTE(olafur): manually construct a term symbol for external Java
              // enum symbols because `underlying.entry.entryType == 9` which by
              // default becomes a type symbol.
              val ssym = Symbols.Global(c.parent.get.ssym, d.Term(c.name))
              s.SingleType(s.NoType, ssym)
            case c: Type => loop(c) match {
                case s.NoType => s.NoType
                case sarg => s.TypeRef(s.NoType, ks.Class, sarg :: Nil)
              }
            case c =>
              val sconst = signedConstantType(tpe).getOrElse(s.Constant(c))
              s.ConstantType(sconst)
          }
        case tpe: RefinedType =>
          val sparents = tpe.typeRefs.map(loop)
          val stpe = s.WithType(sparents)
          val sdecls = Some(tpe.classSym.semanticdbDecls.sscope(HardlinkChildren))
          s.StructuralType(stpe, sdecls)
        case tpe: AnnotatedType =>
          val sanns = tpe.attribTreeRefs.reverse.map(_.toSemantic)
          val stpe = loop(tpe.typeRef)
          s.AnnotatedType(sanns, stpe)
        case tpe: ExistentialType =>
          val stpe = loop(tpe.typeRef)
          val sdecls = Some(tpe.symbols.sscope(HardlinkChildren))
          s.ExistentialType(stpe, sdecls)
        case tpe: PolyType => loop(tpe.typeRef) match {
            case s.NoType => s.NoType
            case stpe =>
              val stparams = tpe.symbols.sscope(HardlinkChildren)
              s.UniversalType(Some(stparams), stpe)
          }
        case NoType => s.NoType
        case NoPrefixType => s.NoType
        case other => sys.error(s"unsupported type $other")
      }
      loop(tpe)
    }
    def toSemanticSig(linkMode: LinkMode): s.Signature = {
      def loop(tpe: Type): s.Signature = tpe match {
        case tpe: ClassInfoType =>
          val stparams = Some(s.Scope())
          val sparents = tpe.typeRefs.map(_.toSemanticTpe)
          val sself = tpe.symbol.self.toSemanticTpe
          val sdecls = Some(tpe.symbol.semanticdbDecls.sscope(linkMode))
          s.ClassSignature(stparams, sparents, sself, sdecls)
        case _: NullaryMethodType | _: MethodType =>
          val stparams = Some(s.Scope())
          val sparamss = tpe.paramss.map(_.sscope(linkMode))
          val sret = tpe.ret.toSemanticTpe
          s.MethodSignature(stparams, sparamss, sret)
        case tpe: TypeBoundsType =>
          val stparams = Some(s.Scope())
          val slo = tpe.lower.toSemanticTpe
          val shi = tpe.upper.toSemanticTpe
          s.TypeSignature(stparams, slo, shi)
        case tpe: PolyType => loop(tpe.typeRef) match { // match only what loop returns
            case t: s.ClassSignature => t.withTypeParameters(tpe.symbols.sscope(linkMode))
            case t: s.MethodSignature => t.withTypeParameters(tpe.symbols.sscope(linkMode))
            case t: s.TypeSignature => t.withTypeParameters(tpe.symbols.sscope(linkMode))
            case t: s.ValueSignature =>
              val stparams = tpe.symbols.sscope(HardlinkChildren)
              s.ValueSignature(s.UniversalType(Some(stparams), t.tpe))
            case t => t
          }
        case NoType => s.NoSignature
        case other => s.ValueSignature(other.toSemanticTpe)
      }
      loop(tpe)
    }
  }

  private object ByNameType {
    def unapply(tpe: Type): Option[Type] = tpe match {
      case TypeRefType(_, sym, List(tpe)) if sym.name == "<byname>" => Some(tpe)
      case _ => None
    }
  }

  private object RepeatedType {
    def unapply(tpe: Type): Option[Type] = tpe match {
      case TypeRefType(_, sym, List(tpe)) if sym.name == "<repeated>" => Some(tpe)
      case _ => None
    }
  }
}

object TypeOps {
  import SymbolOps._

  implicit class XtensionTypeRefType(private val tpe: TypeRefType) extends AnyVal {
    def hasTrivialPrefix: Boolean = tpe.prefix.isTrivialPrefixOf(tpe.symbol)

    def isTrivialPrefixOf(sym: Symbol): Boolean = hasTrivialPrefix &&
      sym.isOwnedBy(tpe.symbol, module = true) && tpe.symbol.isModuleSymbol
  }

  implicit class XtensionSingleType(private val tpe: SingleType) extends AnyVal {
    def hasTrivialPrefix: Boolean = tpe.typeRef.isTrivialPrefixOf(tpe.symbol)

    def isTrivialPrefixOf(sym: Symbol): Boolean = hasTrivialPrefix &&
      sym.isOwnedBy(tpe.symbol, module = true)
  }

  implicit class XtensionPrefixType(private val tpe: Type) extends AnyVal {
    def isTrivialPrefixOf(sym: Symbol): Boolean = tpe match {
      case tpe: TypeRefType => tpe.isTrivialPrefixOf(sym)
      case tpe: SingleType => tpe.isTrivialPrefixOf(sym)
      case tpe: ThisType => sym.isOwnedBy(tpe.symbol, tpe.symbol.isModuleSymbol)
      case NoPrefixType => true
      case _ => false
    }
  }

  implicit class XtensionResultType(private val tpe: Type) extends AnyVal {
    def ret: Type = tpe match {
      case tpe: NullaryMethodType => tpe.resultType.ret
      case tpe: MethodType => tpe.resultType.ret
      case tpe: PolyType => tpe.typeRef.ret
      case _ => tpe
    }

    def paramss: List[List[SymbolInfoSymbol]] = tpe match {
      case _: NullaryMethodType => Nil
      case tpe: MethodType =>
        val symbolInfoParams = tpe.paramSymbols.map(_.asInstanceOf[SymbolInfoSymbol])
        symbolInfoParams.toList :: tpe.resultType.paramss
      case _ => Nil
    }
  }

}
