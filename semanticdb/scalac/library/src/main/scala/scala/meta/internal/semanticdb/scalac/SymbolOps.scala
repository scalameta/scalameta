package scala.meta.internal.semanticdb.scalac

import java.util.HashMap
import scala.{meta => m}
import scala.meta.internal.inputs._
import scala.meta.internal.scalacp._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.util.control.NonFatal

trait SymbolOps { self: SemanticdbOps =>

  lazy val symbolCache = new HashMap[g.Symbol, String]
  implicit class XtensionGSymbolMSymbol(sym: g.Symbol) {
    def toSemantic: String = {
      def uncached(sym: g.Symbol): String = {
        if (sym == null || sym == g.NoSymbol) return Symbols.None
        if (sym.isOverloaded) return Symbols.Multi(sym.alternatives.map(_.toSemantic))
        if (sym.isModuleClass) return sym.asClass.module.toSemantic
        if (sym.isTypeSkolem) return sym.deSkolemize.toSemantic
        if (sym.isSemanticdbLocal) return freshSymbol(sym.pos.toMeta.input)

        val owner = sym.owner.toSemantic
        val signature = {
          if (sym.isMethod || sym.isUsefulField) {
            d.Method(sym.name.toSemantic, sym.disambiguator)
          } else if (sym.isTypeParameter) {
            d.TypeParameter(sym.name.toSemantic)
          } else if (sym.isValueParameter) {
            d.Parameter(sym.name.toSemantic)
          } else if (sym.isType || sym.isJavaClass) {
            d.Type(sym.name.toSemantic)
          } else {
            d.Term(sym.name.toSemantic)
          }
        }
        Symbols.Global(owner, signature)
      }
      val msym = symbolCache.get(sym)
      if (msym != null) {
        msym
      } else {
        val msym = try {
          uncached(sym)
        } catch {
          case NonFatal(e) if isInteractiveCompiler =>
            // happens regularly for broken code with the pc, see
            // https://github.com/scalameta/scalameta/issues/1194
            Symbols.None
        }
        symbolCache.put(sym, msym)
        msym
      }
    }
  }

  implicit class XtensionGSymbolMSpec(sym: g.Symbol) {
    def isSemanticdbGlobal: Boolean = !isSemanticdbLocal
    def isSemanticdbLocal: Boolean = {
      def definitelyGlobal = sym.hasPackageFlag
      def definitelyLocal =
        sym == g.NoSymbol ||
          (sym.owner.isMethod && !sym.isParameter) ||
          ((sym.owner.isAliasType || sym.owner.isAbstractType) && !sym.isParameter) ||
          sym.isSelfParameter ||
          sym.isLocalDummy ||
          sym.isRefinementClass ||
          sym.isAnonymousClass ||
          sym.isAnonymousFunction ||
          sym.isExistential
      def ownerLocal = sym.owner.isSemanticdbLocal
      !definitelyGlobal && (definitelyLocal || ownerLocal)
    }
    def isSemanticdbMulti: Boolean = sym.isOverloaded
    def disambiguator: String = {
      val peers = sym.owner.semanticdbDecls.gsyms
      val overloads = peers.filter { peer =>
        peer.isMethod &&
        peer.name == sym.name
      }
      val suffix = {
        if (overloads.lengthCompare(1) == 0) ""
        else {
          val index = overloads.indexOf(sym)
          if (index <= 0) ""
          else "+" + index
        }
      }
      "(" + suffix + ")"
    }
    def semanticdbDecls: SemanticdbDecls = {
      if (sym.hasPackageFlag) {
        SemanticdbDecls(Nil)
      } else if (sym.isModule) {
        if (sym.isJavaDefined) sym.companionClass.semanticdbDecls
        else sym.moduleClass.semanticdbDecls
      } else {
        if (sym.isModuleClass && sym.isJavaDefined) {
          sym.companionClass.semanticdbDecls
        } else {
          def loop(info: g.Type): SemanticdbDecls = {
            info match {
              case g.PolyType(_, info) =>
                loop(info)
              case g.ClassInfoType(_, gdecls, _) =>
                val gbuf = List.newBuilder[g.Symbol]
                gdecls.sorted.filter(_.isUseful).foreach(gbuf.+=)
                if (sym.isJavaDefined) {
                  sym.companionModule.info.decls.filter(_.isUseful).foreach(gbuf.+=)
                }
                SemanticdbDecls(gbuf.result)
              case _ =>
                SemanticdbDecls(Nil)
            }
          }
          loop(sym.info)
        }
      }
    }
  }

  implicit class XtensionGScopeMSpec(scope: g.Scope) {
    def semanticdbDecls: SemanticdbDecls = {
      SemanticdbDecls(scope.sorted.filter(_.isUseful))
    }
  }

  implicit class XtensionGSymbolsMSpec(syms: List[g.Symbol]) {
    def sscope(linkMode: LinkMode): s.Scope = {
      linkMode match {
        case SymlinkChildren =>
          s.Scope(symlinks = syms.map(_.ssym))
        case HardlinkChildren =>
          s.Scope(hardlinks = syms.map(_.toSymbolInformation(HardlinkChildren)))
      }
    }
  }

  case class SemanticdbDecls(gsyms: List[g.Symbol]) {
    def sscope(linkMode: LinkMode): s.Scope = {
      linkMode match {
        case SymlinkChildren =>
          val sbuf = List.newBuilder[String]
          gsyms.foreach { gsym =>
            val ssym = gsym.ssym
            sbuf += ssym
            if (gsym.isUsefulField && gsym.isMutable) {
              if (ssym.isGlobal) {
                val setterName = ssym.desc.name + "_="
                val setterSym = Symbols.Global(ssym.owner, d.Method(setterName, "()"))
                sbuf += setterSym
              } else {
                val setterSym = ssym + "+1"
                sbuf += setterSym
              }
            }
          }
          s.Scope(symlinks = sbuf.result)
        case HardlinkChildren =>
          val sbuf = List.newBuilder[s.SymbolInformation]
          gsyms.foreach { gsym =>
            val sinfo = gsym.toSymbolInformation(HardlinkChildren)
            sbuf += sinfo
            if (gsym.isUsefulField && gsym.isMutable) {
              Synthetics.setterInfos(sinfo, HardlinkChildren).foreach(sbuf.+=)
            }
          }
          s.Scope(hardlinks = sbuf.result)
      }
    }
  }

  implicit class XtensionGSymbol(sym: g.Symbol) {
    def ssym: String = sym.toSemantic
    def self: g.Type = {
      sym.thisSym.info match {
        case g.RefinedType(List(_, self), _) if sym.thisSym != sym => self
        case _ => g.NoType
      }
    }
    def isSelfParameter: Boolean = {
      sym != g.NoSymbol && sym.owner.thisSym == sym
    }
    def isJavaClass: Boolean =
      sym.isJavaDefined &&
        !sym.hasPackageFlag &&
        (sym.isClass || sym.isModule)
    def isSyntheticConstructor: Boolean = {
      val isModuleConstructor = sym.isConstructor && sym.owner.isModuleClass
      val isTraitConstructor = sym.isMixinConstructor
      val isInterfaceConstructor = sym.isConstructor && sym.owner.isJavaDefined && sym.owner.isInterface
      val isEnumConstructor = sym.isConstructor && sym.owner.hasJavaEnumFlag
      val isStaticConstructor = sym.name == g.TermName("<clinit>")
      isModuleConstructor || isTraitConstructor || isInterfaceConstructor || isEnumConstructor || isStaticConstructor
    }
    def isLocalChild: Boolean =
      sym.name == g.tpnme.LOCAL_CHILD
    def isSyntheticValueClassCompanion: Boolean = {
      if (sym.isModule) {
        sym.moduleClass.isSyntheticValueClassCompanion
      } else {
        sym.isModuleClass &&
        sym.isSynthetic &&
        sym.semanticdbDecls.gsyms.isEmpty
      }
    }
    def isScalacField: Boolean = {
      val isFieldForPrivateThis = sym.isPrivateThis && sym.isTerm && !sym.isMethod && !sym.isModule
      val isFieldForOther = sym.name.endsWith(g.nme.LOCAL_SUFFIX_STRING)
      val isJavaDefined = sym.isJavaDefined || sym.hasJavaEnumFlag
      (isFieldForPrivateThis || isFieldForOther) && !isJavaDefined
    }
    def isUselessField: Boolean = {
      sym.isScalacField && sym.getterIn(sym.owner) != g.NoSymbol
    }
    def isUsefulField: Boolean = {
      sym.isScalacField && !sym.isUselessField
    }
    def isSyntheticCaseAccessor: Boolean = {
      sym.isCaseAccessor && sym.name.toString.contains("$")
    }
    def isSyntheticJavaModule: Boolean = {
      !sym.hasPackageFlag && sym.isJavaDefined && sym.isModule
    }
    def isAnonymousClassConstructor: Boolean = {
      sym.isConstructor && sym.owner.isAnonymousClass
    }
    def isSyntheticAbstractType: Boolean = {
      sym.isSynthetic && sym.isAbstractType // these are hardlinked to TypeOps
    }
    def isUseless: Boolean = {
      sym == g.NoSymbol ||
      sym.isAnonymousClass ||
      sym.isAnonymousClassConstructor ||
      sym.isSyntheticConstructor ||
      sym.isStaticConstructor ||
      sym.isLocalChild ||
      sym.isSyntheticValueClassCompanion ||
      sym.isUselessField ||
      sym.isSyntheticCaseAccessor ||
      sym.isRefinementClass ||
      sym.isSyntheticJavaModule ||
      sym.isSyntheticAbstractType
    }
    def isUselessOccurrence: Boolean = {
      sym.isUseless &&
      !sym.isSyntheticJavaModule // references to static Java inner classes should have occurrences
    }
    def isUseful: Boolean = !sym.isUseless
  }

  lazy val idCache = new HashMap[String, Int]
  private def freshSymbol(minput: m.Input): String = {
    if (minput == m.Input.None) Symbols.None
    else {
      val id = idCache.get(minput.syntax)
      idCache.put(minput.syntax, id + 1)
      Symbols.Local(id)
    }
  }
}
