package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.scalacp._
import scala.meta.internal.semanticdb.Scala.{DisplayNames => dn}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k, Property => p}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.{semanticdb => s}

import scala.collection.mutable
import scala.reflect.internal.{Flags => gf}
import scala.{meta => m}

trait SymbolInformationOps {
  self: SemanticdbOps =>
  import g._

  implicit class XtensionGSymbolMSymbolInformation(gsym0: g.Symbol) {

    private val gsym: g.Symbol =
      if (gsym0.isJavaClass) gsym0.companionClass
      else if (gsym0.isModuleClass) gsym0.asClass.module
      else if (gsym0.isTypeSkolem) gsym0.deSkolemize
      else gsym0

    private def language: s.Language =
      if (gsym.hasPackageFlag) l.SCALA
      else if (gsym.hasFlag(gf.JAVA)) l.JAVA
      else if (gsym.isParameter && gsym.owner.hasFlag(gf.JAVA)) l.JAVA
      else l.SCALA

    private[meta] def kind: s.SymbolInformation.Kind = gsym match {
      // term symbols
      case _ if gsym.isSelfParameter => k.SELF_PARAMETER
      // MIXEDIN shares the bit of EXISTENTIAL, but mixin runs after the phases we read
      case _ if gsym.isTerm && gsym.hasFlag(gf.EXISTENTIAL) => k.LOCAL
      case gsym: MethodSymbol =>
        if (gsym.isConstructor) k.CONSTRUCTOR
        else if (gsym.isMacro) k.MACRO
        else if (gsym.isGetter && gsym.isLazy && gsym.isLocalToBlock) k.LOCAL
        else k.METHOD
      case gsym: ModuleSymbol =>
        if (gsym.hasPackageFlag) k.PACKAGE
        else if (gsym.isPackageObject) k.PACKAGE_OBJECT
        else k.OBJECT
      case gsym: TermSymbol =>
        if (gsym.isParameter) k.PARAMETER
        else if (gsym.isLocalToBlock) k.LOCAL
        else if (gsym.hasFlag(gf.JAVA | gf.JAVA_ENUM)) k.FIELD
        else k.METHOD

      // type symbols
      case gsym: ClassSymbol =>
        if (gsym.isTrait) if (gsym.isJavaDefined) k.INTERFACE else k.TRAIT
        else if (gsym.isClassfileAnnotation) k.INTERFACE
        else k.CLASS
      case gsym: TypeSymbol => if (gsym.isParameter) k.TYPE_PARAMETER else k.TYPE

      case NoSymbol => k.UNKNOWN_KIND
      case _ => sys.error(s"unsupported symbol $gsym")
    }

    private[meta] def properties: Int = {
      if (gsym.hasFlag(gf.PACKAGE)) return 0
      val kind = this.kind
      var flags = 0
      def flip(prop: s.SymbolInformation.Property): Unit = flags |= prop.value
      def isAbstractClass = gsym.isClass && gsym.isAbstract && !gsym.isTrait &&
        !gsym.hasFlag(gf.JAVA_ENUM)
      def isAbstractMethod = gsym.isMethod && gsym.isDeferred && !kind.isLocal
      def isAbstractType = gsym.isType && !gsym.isParameter && gsym.isDeferred
      if (gsym.hasFlag(gf.JAVA)) {
        if (gsym.hasFlag(gf.ABSOVERRIDE)) {
          flip(p.ABSTRACT)
          flip(p.OVERRIDE)
        } else {
          if (isAbstractClass || kind.isInterface || isAbstractMethod) flip(p.ABSTRACT)
          if (gsym.hasFlag(gf.OVERRIDE)) flip(p.OVERRIDE)
        }
        if (gsym.hasFlag(gf.FINAL) || gsym.hasFlag(gf.JAVA_ENUM)) flip(p.FINAL)
        if (gsym.hasFlag(gf.JAVA_ENUM)) flip(p.ENUM)
        if (gsym.hasFlag(gf.STATIC) && !gsym.hasFlag(gf.INTERFACE)) flip(p.STATIC)
        if (gsym.isDefaultMethod) flip(p.DEFAULT)
      } else {
        // SYNTHETIC is emitted for Scala symbols only here; Java enum values/valueOf are
        // marked in javacp (metac emits no SymbolInformation for them). Self parameters
        // carry the synthetic flag even when written in source (`self =>`), but the spec
        // grants them no properties, so exclude them.
        if (!gsym.isSelfParameter && gsym.isSynthetic) flip(p.SYNTHETIC)
        if (gsym.hasFlag(gf.ABSOVERRIDE)) {
          flip(p.ABSTRACT)
          flip(p.OVERRIDE)
        } else {
          if (isAbstractClass || isAbstractMethod || isAbstractType) flip(p.ABSTRACT)
          if (gsym.hasFlag(gf.OVERRIDE)) flip(p.OVERRIDE)
        }
        if (gsym.hasFlag(gf.FINAL) || gsym.isModule) flip(p.FINAL)
        if (gsym.hasFlag(gf.SEALED)) flip(p.SEALED)
        if (gsym.hasFlag(gf.IMPLICIT)) flip(p.IMPLICIT)
        if (gsym.hasFlag(gf.LAZY)) flip(p.LAZY)
        if (gsym.hasFlag(gf.CASE) && (gsym.isClass || gsym.isModule)) flip(p.CASE)
        if (gsym.isType && gsym.hasFlag(gf.CONTRAVARIANT)) flip(p.CONTRAVARIANT)
        if (gsym.isType && gsym.hasFlag(gf.COVARIANT)) flip(p.COVARIANT)
        if (kind.isLocal || gsym.isUsefulField) if (gsym.isMutable) flip(p.VAR) else flip(p.VAL)
        if (gsym.isGetter || gsym.isSetter) if (gsym.isStable) flip(p.VAL) else flip(p.VAR)
        if (gsym.isParameter && gsym.owner.isPrimaryConstructor) {
          val gaccessor = gsym.owner.owner.info.decl(gsym.name)
          if (gaccessor != g.NoSymbol && !gaccessor.isStable) flip(p.VAR)
          else if (gaccessor != g.NoSymbol && gaccessor.isMethod) flip(p.VAL)
          else ()
        }
        if (gsym.isPrimaryConstructor) flip(p.PRIMARY)
        if (gsym.isDefaultParameter) flip(p.DEFAULT)
      }
      flags
    }

    private def displayName: String =
      if (gsym.isRootPackage) dn.RootPackage
      else if (gsym.isEmptyPackage) dn.EmptyPackage
      else if (gsym.isConstructor) dn.Constructor
      else if (gsym.decodedName.startsWith("_$")) dn.Anonymous
      else if (gsym.isPackageObject || gsym.isPackageObjectClass) gsym.owner.symbolName
      else gsym.symbolName

    private def sig(linkMode: LinkMode): s.Signature =
      if (gsym.hasPackageFlag) s.NoSignature
      else {
        val gsig =
          if (gsym.hasFlag(gf.JAVA_ENUM) && gsym.isStatic) gsym.info.widen
          else if (gsym.isAliasType) {
            def preprocess(tpe: g.Type): g.Type = tpe match {
              case tpe: g.PolyType => g.PolyType(tpe.typeParams, preprocess(tpe.resultType))
              case tpe => g.TypeBounds(tpe, tpe)
            }
            preprocess(gsym.info)
          } else if (gsym.isModule) gsym.moduleClass.info
          else gsym.info
        val ssig = gsig.toSemanticSig(linkMode)
        if (gsym.isConstructor) ssig match {
          case m: s.MethodSignature => m.copy(returnType = s.NoType)
          case m => m
        }
        // A local is a value, even a lazy getter or a private[this] pattern temporary
        else if (kind.isLocal) ssig match {
          case ssig: s.MethodSignature
              if ssig.parameterLists.isEmpty && ssig.typeParameters.forall(_.symlinks.isEmpty) =>
            s.ValueSignature(ssig.returnType)
          case _ => ssig
        }
        else if (gsym.isScalacField) ssig match {
          case ssig: s.ValueSignature =>
            val stparams = Some(s.Scope())
            val sparamss = Nil
            val sret = ssig.tpe
            s.MethodSignature(stparams, sparamss, sret)
          case s.Signature.Empty => s.NoSignature
          case _ => sys.error(s"unsupported signature: ${ssig.getClass} $ssig")
        }
        else if (gsym.isSelfParameter) gsym.owner.self.toSemanticTpe match {
          case s.NoType => s.NoSignature
          case stpe => s.ValueSignature(stpe)
        }
        else if (gsym.isClassfileAnnotation) ssig match {
          case ssig: s.ClassSignature =>
            val parents1 = ssig.parents.flatMap {
              case s.TypeRef(s.NoType, "scala/annotation/Annotation#", Nil) =>
                Some(s.TypeRef(s.NoType, "java/lang/Object#", Nil))
              case s.TypeRef(s.NoType, "scala/annotation/ClassfileAnnotation#", Nil) => None
              case sother => Some(sother)
            }
            ssig.copy(parents = parents1)
          case _ => sys.error(s"unsupported signature: ${ssig.getClass} $ssig")
        }
        else if (gsym.hasFlag(gf.JAVA) && kind == k.TYPE_PARAMETER) ssig match {
          case ssig: s.TypeSignature =>
            val upperBound1 = ssig.upperBound match {
              case s.StructuralType(s.WithType(tpes), _) => s.IntersectionType(tpes)
              case s.TypeRef(s.NoType, "scala/Any#", Nil) => s
                  .TypeRef(s.NoType, "java/lang/Object#", Nil)
              case sother => sother
            }
            ssig.copy(lowerBound = s.NoType, upperBound = upperBound1)
          case _ => sys.error(s"unsupported signature: ${ssig.getClass} $ssig")
        }
        else ssig
      }

    private def annotations: List[s.AnnotationTree] = {
      val buf = new mutable.ListBuffer[s.AnnotationTree]
      annotations(buf += _)(gsym)
      if (gsym.isGetter) {
        val len = buf.length
        annotations(x => if (!buf.iterator.take(len).contains(x)) buf += x)(
          gsym.owner.info.decl(gsym.localName).filter(!_.isMethod),
        )
      }
      buf.toList
    }

    private def annotations(f: s.AnnotationTree => Unit)(sym: g.Symbol): Unit = sym.annotations
      .foreach(x => if (x.symbol != definitions.MacroImplAnnotation) f(x.toSemantic))

    private def access: s.Access = kind match {
      case k.LOCAL | k.PARAMETER | k.SELF_PARAMETER | k.TYPE_PARAMETER | k.PACKAGE | k
            .PACKAGE_OBJECT => s.NoAccess
      case _ =>
        if (gsym.privateWithin == NoSymbol)
          if (gsym.isPrivateThis) s.PrivateThisAccess()
          else if (gsym.isPrivate) s.PrivateAccess()
          else if (gsym.isProtectedThis) s.ProtectedThisAccess()
          else if (gsym.isProtected) s.ProtectedAccess()
          else s.PublicAccess()
        else {
          val ssym = gsym.privateWithin.ssym
          if (gsym.isProtected) s.ProtectedWithinAccess(ssym) else s.PrivateWithinAccess(ssym)
        }
    }

    def overriddenSymbols: List[String] =
      if (self.config.overrides.isOn) gsym.allOverriddenSymbols.map(_.toSemantic) else Nil

    def toSymbolInfo(linkMode: LinkMode, ssym: String): s.SymbolInformation = s.SymbolInformation(
      symbol = ssym,
      language = language,
      kind = kind,
      properties = properties,
      displayName = displayName,
      overriddenSymbols = overriddenSymbols,
      signature = sig(linkMode),
      annotations = annotations,
      access = access,
    )
    def toSymbolInformation(linkMode: LinkMode): s.SymbolInformation =
      toSymbolInfo(linkMode, gsym.ssym)

  }
}
