package scala.meta.internal.semanticdb.scalac

import scala.{meta => m}
import scala.reflect.internal.{Flags => gf}
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}

trait SymbolInformationOps { self: DatabaseOps =>
  import g._

  implicit class XtensionGSymbolMSymbolInformation(gsym0: g.Symbol) {
    private val gsym: g.Symbol = {
      if (gsym0.isJavaClass) gsym0.companionClass
      else if (gsym0.isModuleClass) gsym0.asClass.module
      else if (gsym0.isTypeSkolem) gsym0.deSkolemize
      else gsym0
    }

    private val isObject = gsym.isModule && !gsym.hasFlag(gf.PACKAGE) && gsym.name != nme.PACKAGE

    private def kind: s.SymbolInformation.Kind = {
      gsym match {
        case _ if gsym.isSelfParameter => k.SELF_PARAMETER
        case _ if gsym.isSemanticdbLocal => k.LOCAL
        case gsym: MethodSymbol =>
          if (gsym.isConstructor) {
            k.CONSTRUCTOR
          } else {
            if (gsym.isGetter && gsym.isLazy && !gsym.isClass) {
              if (gsym.isLocalToBlock) k.LOCAL
              else k.FIELD
            } else if (gsym.isMacro) {
              k.MACRO
            } else {
              k.METHOD
            }
          }
        case gsym: ModuleSymbol =>
          if (gsym.hasPackageFlag) k.PACKAGE
          else if (gsym.isPackageObject) k.PACKAGE_OBJECT
          else k.OBJECT
        case gsym: TermSymbol =>
          if (gsym.isParameter) k.PARAMETER
          else if (gsym.isLocalToBlock) k.LOCAL
          else k.FIELD
        case gsym: ClassSymbol =>
          if (gsym.isTrait && gsym.hasFlag(gf.JAVA)) k.INTERFACE
          else if (gsym.isTrait) k.TRAIT
          else k.CLASS
        case gsym: TypeSymbol =>
          if (gsym.isParameter) k.TYPE_PARAMETER
          else k.TYPE
        case NoSymbol =>
          k.UNKNOWN_KIND
        case _ =>
          sys.error(s"unsupported symbol $gsym")
      }
    }

    def language: s.Language =
      if (gsym.hasPackageFlag) s.Language.SCALA
      else if (gsym.hasFlag(gf.JAVA)) s.Language.JAVA
      else s.Language.SCALA

    private[meta] def properties: Int = {
      val kind = this.kind
      var flags = 0
      def flip(prop: s.SymbolInformation.Property): Unit =
        flags |= prop.value
      def isAbstractClass =
        gsym.isClass && gsym.isAbstract && !gsym.isTrait && !gsym.hasFlag(gf.JAVA_ENUM)
      def isAbstractMethod = gsym.isMethod && gsym.isDeferred
      def isAbstractType = gsym.isType && !gsym.isParameter && gsym.isDeferred
      if (gsym.hasFlag(gf.PACKAGE)) {
        ()
      } else if (gsym.hasFlag(gf.JAVA)) {
        if (isAbstractClass || kind.isInterface || isAbstractMethod) flip(p.ABSTRACT)
        if (gsym.hasFlag(gf.FINAL) || gsym.hasFlag(gf.JAVA_ENUM)) flip(p.FINAL)
        if (gsym.hasFlag(gf.JAVA_ENUM)) flip(p.ENUM)
        if (gsym.hasFlag(gf.STATIC)) flip(p.STATIC)
      } else {
        if (isAbstractClass || isAbstractMethod || isAbstractType) flip(p.ABSTRACT)
        if (gsym.hasFlag(gf.FINAL) || isObject) flip(p.FINAL)
        if (gsym.hasFlag(gf.SEALED)) flip(p.SEALED)
        if (gsym.hasFlag(gf.IMPLICIT)) flip(p.IMPLICIT)
        if (gsym.hasFlag(gf.LAZY)) flip(p.LAZY)
        if (gsym.hasFlag(gf.CASE)) flip(p.CASE)
        if (gsym.isType && gsym.hasFlag(gf.CONTRAVARIANT)) flip(p.CONTRAVARIANT)
        if (gsym.isType && gsym.hasFlag(gf.COVARIANT)) flip(p.COVARIANT)
        if (kind.isLocal || kind.isField) {
          if (gsym.isMutable) flip(p.VAR)
          else flip(p.VAL)
        }
        if (gsym.isGetter || gsym.isSetter) {
          if (gsym.isStable) flip(p.VAL)
          else flip(p.VAR)
        }
        if (gsym.isParameter && gsym.owner.isPrimaryConstructor) {
          val ggetter = gsym.getterIn(gsym.owner.owner)
          if (ggetter != g.NoSymbol && !ggetter.isStable) flip(p.VAR)
          else if (ggetter != g.NoSymbol) flip(p.VAL)
          else ()
        }
        if (gsym.isPrimaryConstructor) flip(p.PRIMARY)
      }
      flags
    }

    private def name: String = {
      gsym.name.toSemantic
    }

    private def newInfo: (Option[s.Type], List[g.Symbol]) = {
      if (gsym.hasPackageFlag) (None, Nil)
      else {
        val ginfo = {
          if (gsym.isGetter && gsym.isLazy && !gsym.isClass) {
            gsym.info.finalResultType
          } else if (gsym.hasFlag(gf.JAVA_ENUM) && gsym.isStatic) {
            gsym.info.widen
          } else if (gsym.isAliasType) {
            def preprocess(info: g.Type): g.Type = {
              info match {
                case g.PolyType(tparams, tpe) => g.PolyType(tparams, preprocess(tpe))
                case tpe => g.TypeBounds(tpe, tpe)
              }
            }
            preprocess(gsym.info)
          } else {
            gsym.info
          }
        }
        ginfo.toSemantic
      }
    }

    private def anns: (List[s.Annotation], List[g.Symbol]) = {
      val buf = List.newBuilder[g.Symbol]
      val ganns = gsym.annotations.filter { gann =>
        gann.atp.typeSymbol != definitions.MacroImplAnnotation
      }
      val sanns = ganns.map { gann =>
        val (sann, todo) = gann.toSemantic
        todo.foreach(buf.+=)
        sann
      }
      (sanns, buf.result)
    }

    // TODO: I'm not completely happy with the implementation of this method.
    // See https://github.com/scalameta/scalameta/issues/1325 for details.
    private def acc: s.Accessibility = {
      if (gsym.hasFlag(gf.SYNTHETIC) && gsym.hasFlag(gf.ARTIFACT)) {
        // NOTE: some sick artifact vals produced by mkPatDef can be
        // private to method (whatever that means), so here we just ignore them.
        s.Accessibility(a.PUBLIC)
      } else {
        if (gsym.privateWithin == NoSymbol) {
          if (gsym.isPrivateThis) s.Accessibility(a.PRIVATE_THIS)
          else if (gsym.isPrivate) s.Accessibility(a.PRIVATE)
          else if (gsym.isProtectedThis) s.Accessibility(a.PROTECTED_THIS)
          else if (gsym.isProtected) s.Accessibility(a.PROTECTED)
          else s.Accessibility(a.PUBLIC)
        } else {
          val ssym = gsym.privateWithin.toSemantic.syntax
          if (gsym.isProtected) s.Accessibility(a.PROTECTED_WITHIN, ssym)
          else s.Accessibility(a.PRIVATE_WITHIN, ssym)
        }
      }
    }

    private def owner: m.Symbol = {
      if (config.owners.isAll && gsym.isSemanticdbGlobal) gsym.owner.toSemantic
      else m.Symbol.None
    }

    def toSymbolInformation(): SymbolInformationResult = {
      val (anns, todoAnns) = this.anns
      config.types match {
        case TypeMode.None =>
          val denot = s.SymbolInformation()
          SymbolInformationResult(denot, todoAnns)
        case TypeMode.All =>
          val (tpe, todoTpe) = newInfo
          val denot = s.SymbolInformation(
            symbol = gsym.toSemantic.syntax,
            language = language,
            kind = kind,
            accessibility = Some(acc),
            properties = properties,
            name = name,
            tpe = tpe,
            annotations = anns,
            owner = owner.syntax
          )
          SymbolInformationResult(denot, todoAnns ++ todoTpe)
      }
    }
  }

  // NOTE: Holds a symbol information along with todo lists of symbols to persist.
  case class SymbolInformationResult(denot: s.SymbolInformation, todoTpe: List[g.Symbol])
}
