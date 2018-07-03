package scala.meta.internal.scalacp

import scala.collection.mutable
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scala.tools.scalap.scalax.rules.ScalaSigParserError
import scala.tools.scalap.scalax.rules.scalasig._

trait SymbolInformationOps { self: Scalacp =>
  private val primaryCtors = mutable.Map[String, Int]()
  implicit class XtensionGSymbolSSymbolInformation(sym: Symbol) {
    private def language: s.Language = {
      // NOTE: We have no way to figure out whether an external symbol
      // comes from Java or Scala. Moreover, we can't even find out
      // whether it's a package or an object. Therefore, we default to l.SCALA.
      l.SCALA
    }

    private[meta] def kind: s.SymbolInformation.Kind = {
      sym match {
        // NOTE: Scalacp doesn't care about self parameters
        // because they are local, i.e. not saved in scala signatures.
        // case _ if sym.isSelfParameter =>
        //   k.SELF_PARAMETER
        case sym: MethodSymbol if sym.isMethod =>
          if (sym.isConstructor) k.CONSTRUCTOR
          else if (sym.isMacro) k.MACRO
          // NOTE: Scalap doesn't expose locals.
          // else if (sym.isGetter && sym.isLazy && sym.isLocalToBlock) k.LOCAL
          else k.METHOD
        case _: ObjectSymbol | _: ClassSymbol if sym.isModule =>
          if (sym.isPackage) k.PACKAGE
          else if (sym.isPackageObject) k.PACKAGE_OBJECT
          else k.OBJECT
        case sym: MethodSymbol =>
          // NOTE: This is craziness. In scalap, parameters, val and vars
          // are also modelled with method symbols.
          if (sym.isParam) k.PARAMETER
          // NOTE: Scalap doesn't expose locals.
          // else if (gsym.isLocalToBlock) k.LOCAL
          // NOTE: Scalap doesn't expose JAVA_ENUM.
          else if (sym.isJava /* || sym.isJavaEnum */ ) k.FIELD
          else k.METHOD
        case sym: ClassSymbol if !sym.isModule =>
          if (sym.isTrait && sym.isJava) k.INTERFACE
          else if (sym.isTrait) k.TRAIT
          else k.CLASS
        case _: TypeSymbol | _: AliasSymbol =>
          if (sym.isParam) k.TYPE_PARAMETER
          else k.TYPE
        case sym: ExternalSymbol =>
          // NOTE: Object and package external symbols
          // are indistinguishable from each other.
          // This means that metacp never sets k.PACKAGE.
          val hasTermName = {
            val idx = sym.entry.index + 1
            if (sym.entry.scalaSig.hasEntry(idx)) {
              val nameEntryType = sym.entry.scalaSig.table(idx)._1
              nameEntryType == 1
            } else {
              false
            }
          }
          val isModuleClass = sym.entry.entryType == 10
          def isScalaObject: Boolean = isModuleClass && !sym.isJavaDefined
          if (sym.isPackageAccordingToClasspath) k.PACKAGE
          else if (hasTermName || isScalaObject) k.OBJECT
          else k.CLASS
        case NoSymbol =>
          k.UNKNOWN_KIND
        case _ =>
          sys.error(s"unsupported symbol $sym")
      }
    }

    private[meta] def properties: Int = {
      sym match {
        case sym: SymbolInfoSymbol =>
          var flags = 0
          def flip(sprop: s.SymbolInformation.Property) = flags |= sprop.value
          def isAbstractClass = sym.isClass && sym.isAbstract && !sym.isTrait
          def isAbstractMethod = sym.isMethod && sym.isDeferred
          def isAbstractType = sym.isType && !sym.isParam && sym.isDeferred
          if (sym.isPackage) {
            ()
          } else if (sym.isJava) {
            if (isAbstractClass || kind.isInterface || isAbstractMethod) flip(p.ABSTRACT)
            // NOTE: Scalap doesn't expose JAVA_ENUM.
            if (sym.isFinal /* || sym.isJavaEnum */ ) flip(p.FINAL)
            // NOTE: Scalap doesn't expose JAVA_ENUM.
            // if (sym.isJavaEnum) flip(p.ENUM)
            if (sym.isStatic) flip(p.STATIC)
            ???
          } else {
            if (isAbstractClass || isAbstractMethod || isAbstractType) flip(p.ABSTRACT)
            if (sym.isFinal || sym.isModule) flip(p.FINAL)
            if (sym.isSealed) flip(p.SEALED)
            if (sym.isImplicit) flip(p.IMPLICIT)
            if (sym.isLazy) flip(p.LAZY)
            if (sym.isCase && (sym.isClass || sym.isModule)) flip(p.CASE)
            if (sym.isType && sym.isCovariant) flip(p.COVARIANT)
            if (sym.isType && sym.isContravariant) flip(p.CONTRAVARIANT)
            // NOTE: Scalap doesn't expose locals.
            if (/*kind.isLocal ||*/ sym.isUsefulField) {
              if (sym.isMutable) flip(p.VAR)
              else flip(p.VAL)
            }
            if (sym.isAccessor) {
              if (sym.isStable) flip(p.VAL)
              else flip(p.VAR)
            }
            if (sym.isParam) {
              sym.parent.foreach { parent =>
                if ((parent.properties & p.PRIMARY.value) != 0) {
                  parent.parent.foreach { grandParent =>
                    val classMembers = grandParent.children
                    val accessor = classMembers.find(m => m.isParamAccessor && m.name == sym.name)
                    accessor.foreach { accessor =>
                      val isStable = {
                        if (accessor.isMethod) accessor.isStable
                        else !accessor.isMutable
                      }
                      if (!isStable) flip(p.VAR)
                      else if (accessor.isMethod) flip(p.VAL)
                      else ()
                    }
                  }
                }
              }
            }
            if (sym.isConstructor) {
              val primaryIndex = primaryCtors.getOrElseUpdate(sym.path, sym.entry.index)
              if (sym.entry.index == primaryIndex) flip(p.PRIMARY)
            }
          }
          flags
        case _ =>
          0
      }
    }

    private def name: String = {
      if (sym.isPackageObject) {
        val sowner = sym.ssym.owner
        sowner.desc.name
      } else {
        sym.name.toSemantic
      }
    }

    private def sig(linkMode: LinkMode): s.Signature = {
      sym match {
        case sym: SymbolInfoSymbol =>
          try {
            if (sym.isPackage) {
              s.NoSignature
            } else {
              val sig = {
                // NOTE: Scalap doesn't expose JAVA_ENUM.
                // if (sym.isJavaEnum && gsym.isStatic) {
                //   ???
                // } else {
                if (sym.isAlias) {
                  def preprocess(tpe: Type): Type = {
                    tpe match {
                      case PolyType(tpe, tparams) => PolyType(preprocess(tpe), tparams)
                      case tpe => TypeBoundsType(tpe, tpe)
                    }
                  }
                  preprocess(sym.infoType)
                } else if (sym.isObject) {
                  sym.infoType match {
                    case TypeRefType(_, moduleClassSym: SymbolInfoSymbol, _) =>
                      moduleClassSym.infoType
                    case other =>
                      sys.error(s"unsupported type $other")
                  }
                } else {
                  sym.infoType
                }
              }
              val ssig = sig.toSemanticSig(linkMode)
              if (sym.isConstructor) {
                ssig match {
                  case ssig: s.MethodSignature => ssig.copy(returnType = s.NoType)
                  case _ => ssig
                }
              } else if (sym.isScalacField) {
                ssig match {
                  case ssig: s.ValueSignature =>
                    val stparams = Some(s.Scope())
                    val sparamss = Nil
                    val sret = ssig.tpe
                    s.MethodSignature(stparams, sparamss, sret)
                  case s.NoSignature =>
                    s.NoSignature
                  case _ =>
                    sys.error(s"unsupported signature: ${ssig.getClass} $ssig")
                }
              } else {
                ssig
              }
            }
          } catch {
            case ScalaSigParserError("Unexpected failure") =>
              // FIXME: https://github.com/scalameta/scalameta/issues/1494
              s.NoSignature
          }
        case _ =>
          s.NoSignature
      }
    }

    private def annotations: List[s.Annotation] = {
      // FIXME: https://github.com/scalameta/scalameta/issues/1315
      Nil
    }

    private def accessibility: Option[s.Accessibility] = {
      // FIXME: https://github.com/scalameta/scalameta/issues/1325
      sym match {
        case sym: SymbolInfoSymbol =>
          sym.symbolInfo.privateWithin match {
            case None =>
              if (sym.isPrivate && sym.isLocal) Some(s.Accessibility(a.PRIVATE_THIS))
              else if (sym.isPrivate) Some(s.Accessibility(a.PRIVATE))
              else if (sym.isProtected && sym.isLocal) Some(s.Accessibility(a.PROTECTED_THIS))
              else if (sym.isProtected) Some(s.Accessibility(a.PROTECTED))
              else Some(s.Accessibility(a.PUBLIC))
            case Some(privateWithin: Symbol) =>
              val ssym = privateWithin.ssym
              if (sym.isProtected) Some(s.Accessibility(a.PROTECTED_WITHIN, ssym))
              else Some(s.Accessibility(a.PRIVATE_WITHIN, ssym))
            case Some(other) =>
              sys.error(s"unsupported privateWithin: ${other.getClass} $other")
          }
        case _ =>
          None
      }
    }

    def toSymbolInformation(linkMode: LinkMode): s.SymbolInformation = {
      s.SymbolInformation(
        symbol = sym.ssym,
        language = language,
        kind = kind,
        properties = properties,
        name = name,
        signature = sig(linkMode),
        annotations = annotations,
        accessibility = accessibility
      )
    }
  }
}
