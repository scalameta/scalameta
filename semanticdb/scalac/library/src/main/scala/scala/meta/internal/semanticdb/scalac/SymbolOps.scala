package scala.meta.internal.semanticdb.scalac

import java.util.HashMap
import scala.{meta => m}
import scala.meta.internal.inputs._
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
          if (sym.isMethod) {
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
    def descriptor: String = {
      sym.info.descriptor.toString
    }
    def filterSiblings(syms: List[g.Symbol]): List[g.Symbol] = {
      syms.filter(_.name == sym.name)
    }
    def filterSiblings: List[g.Symbol] = {
      if (sym.owner.isJavaClass) {
        filterSiblings(sym.owner.companionClass.info.decls.sorted) ++
          filterSiblings(sym.owner.info.javaCompanionDecls)
      } else {
        filterSiblings(sym.owner.info.decls.sorted)
      }
    }
    def disambiguator: String = {
      val siblings = filterSiblings
      val synonyms = siblings.filter(_.descriptor == sym.descriptor)
      val suffix = {
        if (synonyms.lengthCompare(1) == 0) ""
        else {
          val index = synonyms.indexOf(sym)
          if (index == 0) ""
          else "+" + index
        }
      }
      "(" + descriptor + suffix + ")"
    }
    def isSelfParameter: Boolean = {
      sym != g.NoSymbol && sym.owner.thisSym == sym
    }
  }

  implicit class XtensionGSymbol(sym: g.Symbol) {
    def isJavaClass: Boolean =
      sym.isJavaDefined &&
        !sym.hasPackageFlag &&
        (sym.isClass || sym.isModule)
    def isSyntheticConstructor: Boolean = {
      (sym.isConstructor || sym.isMixinConstructor) &&
      (sym.owner.isModuleClass || sym.owner.isTrait)
    }
    def isLocalChild: Boolean =
      sym.name == g.tpnme.LOCAL_CHILD
    def isSyntheticValueClassCompanion: Boolean = {
      if (sym.isModule) {
        sym.moduleClass.isSyntheticValueClassCompanion
      } else {
        sym.isModuleClass &&
        sym.isSynthetic &&
        sym.info.decls.filtered.isEmpty
      }
    }
  }

  implicit class XtensionGScope(decls: g.Scope) {
    def filtered: List[g.Symbol] = {
      decls.sorted.filter { decl =>
        !decl.isSyntheticConstructor &&
        !decl.isLocalChild &&
        !decl.isSyntheticValueClassCompanion
      }
    }
  }
}
