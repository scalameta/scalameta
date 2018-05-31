package scala.meta.internal.semanticdb.scalac

import java.util.HashMap
import scala.{meta => m}
import scala.meta.internal.inputs._
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Scala._
import scala.meta.internal.semanticdb3.Scala.{Descriptor => d}
import scala.util.control.NonFatal

trait SymbolOps { self: SemanticdbOps =>

  lazy val idCache = new HashMap[String, Int]
  lazy val symbolCache = new HashMap[g.Symbol, m.Symbol]
  implicit class XtensionGSymbolMSymbol(sym: g.Symbol) {
    def toSemantic: m.Symbol = {
      def uncached(sym: g.Symbol): m.Symbol = {
        if (sym == null || sym == g.NoSymbol) return m.Symbol.None
        if (sym.isOverloaded) return m.Symbol.Multi(sym.alternatives.map(_.toSemantic))
        if (sym.isModuleClass) return sym.asClass.module.toSemantic
        if (sym.isTypeSkolem) return sym.deSkolemize.toSemantic

        if (sym.isSemanticdbLocal) {
          val mpos = sym.pos.toMeta
          return {
            if (mpos == m.Position.None) m.Symbol.None
            else {
              val id = idCache.get(mpos.input.syntax)
              idCache.put(mpos.input.syntax, id + 1)
              m.Symbol.Local("local" + id.toString)
            }
          }
        }

        val owner = sym.owner.toSemantic
        val signature = {
          if (sym.isMethod || sym.isUsefulField) {
            m.Signature.Method(sym.name.toSemantic, sym.disambiguator)
          } else if (sym.isTypeParameter) {
            m.Signature.TypeParameter(sym.name.toSemantic)
          } else if (sym.isValueParameter) {
            m.Signature.TermParameter(sym.name.toSemantic)
          } else if (sym.isType || sym.isJavaClass) {
            m.Signature.Type(sym.name.toSemantic)
          } else {
            m.Signature.Term(sym.name.toSemantic)
          }
        }
        m.Symbol.Global(owner, signature)
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
            m.Symbol.None
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
          sym.name.decoded.startsWith(g.nme.LOCALDUMMY_PREFIX) ||
          (sym.owner.isMethod && !sym.isParameter) ||
          ((sym.owner.isAliasType || sym.owner.isAbstractType) && !sym.isParameter) ||
          sym.isSelfParameter
      !definitelyGlobal && (definitelyLocal || sym.owner.isSemanticdbLocal)
    }
    def isSemanticdbMulti: Boolean = sym.isOverloaded
    def disambiguator: String = {
      val synonyms = sym.owner.semanticdbDecls.gsyms.filter(_.name == sym.name)
      val suffix = {
        if (synonyms.lengthCompare(1) == 0) ""
        else {
          val index = synonyms.indexOf(sym)
          if (index <= 0) ""
          else "+" + index
        }
      }
      "(" + suffix + ")"
    }
    def isSelfParameter: Boolean = {
      sym != g.NoSymbol && sym.owner.thisSym == sym
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

  case class SemanticdbDecls(gsyms: List[g.Symbol]) {
    lazy val ssyms: List[String] = {
      val sbuf = List.newBuilder[String]
      gsyms.foreach { gsym =>
        val ssym = gsym.toSemantic.syntax
        sbuf += ssym
      }
      sbuf.result
    }
  }

  implicit class XtensionGSymbol(sym: g.Symbol) {
    def isJavaClass: Boolean =
      sym.isJavaDefined &&
        !sym.hasPackageFlag &&
        (sym.isClass || sym.isModule)
    def isSyntheticConstructor: Boolean = {
      val isModuleConstructor = sym.isConstructor && sym.owner.isModuleClass
      val isTraitConstructor = sym.isMixinConstructor
      val isInterfaceConstructor = sym.isConstructor && sym.owner.isJavaDefined && sym.owner.isInterface
      val isEnumConstructor = sym.isConstructor && sym.owner.isJavaEnum
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
    def isUseless: Boolean = {
      sym.isSyntheticConstructor ||
      sym.isStaticConstructor ||
      sym.isLocalChild ||
      sym.isSyntheticValueClassCompanion ||
      sym.isUselessField
    }
    def isUseful: Boolean = !sym.isUseless
  }
}
