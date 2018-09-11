package scala.meta.internal.scalacp

import scala.collection.mutable
import scala.meta.internal.classpath._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{DisplayNames => dn}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scala.tools.scalap.scalax.rules.ScalaSigParserError
import scala.tools.scalap.scalax.rules.scalasig._

trait SymbolInformationOps { self: Scalacp =>
  private val primaryCtors = mutable.Map[String, Int]()
  implicit class XtensionGSymbolSSymbolInformation(sym: SymbolInfoSymbol) {
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
        case _ =>
          sys.error(s"unsupported symbol $sym")
      }
    }

    private[meta] def properties: Int = {
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
          sym.parent.foreach {
            case parent: SymbolInfoSymbol =>
              if ((parent.properties & p.PRIMARY.value) != 0) {
                parent.parent.foreach { grandParent =>
                  val classMembers = grandParent.children
                  val accessor =
                    classMembers.find(m => m.isParamAccessor && m.symbolName == sym.symbolName)
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
            case _ =>
              ()
          }
        }
        if (sym.isConstructor) {
          val primaryIndex = primaryCtors.getOrElseUpdate(sym.path, sym.entry.index)
          if (sym.entry.index == primaryIndex) flip(p.PRIMARY)
        }
        if (sym.isDefaultParameter) flip(p.DEFAULT)
      }
      flags
    }

    private def displayName: String = {
      if (sym.isRootPackage) dn.RootPackage
      else if (sym.isEmptyPackage) dn.EmptyPackage
      else if (sym.isConstructor) dn.Constructor
      else if (sym.name.startsWith("_$")) dn.Anonymous
      else if (sym.isPackageObject) sym.ssym.owner.desc.value
      else sym.symbolName
    }

    private def sig(linkMode: LinkMode): s.Signature = {
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
        case ex: MissingSymbolException =>
          if (settings.logBrokenSignatures) {
            reporter.err.println(s"broken signature for ${sym.ssym}: ${ex.getMessage}")
          }
          if (settings.stubBrokenSignatures) {
            s.NoSignature
          } else {
            throw ex
          }
        case ScalaSigParserError("Unexpected failure") =>
          // FIXME: https://github.com/scalameta/scalameta/issues/1494
          s.NoSignature
      }
    }

    private val syntheticAnnotationsSymbols = Set(
      "scala/reflect/macros/internal/macroImpl#"
    )
    private def syntheticAnnotations(annot: s.Annotation): Boolean = {
      annot.tpe match {
        case s.TypeRef(_, sym, _) => syntheticAnnotationsSymbols.contains(sym)
        case _ => false
      }
    }

    private def annotations: List[s.Annotation] = {
      val annots =
        sym.attributes
          .map(attribute => s.Annotation(attribute.typeRef.toSemanticTpe))
          .toList

      annots.filterNot(syntheticAnnotations)
    }

    private def access: s.Access = {
      kind match {
        case k.LOCAL | k.PARAMETER | k.SELF_PARAMETER | k.TYPE_PARAMETER | k.PACKAGE |
            k.PACKAGE_OBJECT =>
          s.NoAccess
        case _ =>
          sym.symbolInfo.privateWithin match {
            case None =>
              if (sym.isPrivate && sym.isLocal) s.PrivateThisAccess()
              else if (sym.isPrivate) s.PrivateAccess()
              else if (sym.isProtected && sym.isLocal) s.ProtectedThisAccess()
              else if (sym.isProtected) s.ProtectedAccess()
              else s.PublicAccess()
            case Some(privateWithin: Symbol) =>
              val ssym = privateWithin.ssym
              if (sym.isProtected) s.ProtectedWithinAccess(ssym)
              else s.PrivateWithinAccess(ssym)
            case Some(other) =>
              sys.error(s"unsupported privateWithin: ${other.getClass} $other")
          }
      }
    }

    def toSymbolInformation(linkMode: LinkMode): s.SymbolInformation = {
      s.SymbolInformation(
        symbol = sym.ssym,
        language = language,
        kind = kind,
        properties = properties,
        displayName = displayName,
        signature = sig(linkMode),
        annotations = annotations,
        access = access
      )
    }
  }
}
