package scala.meta.internal

import scala.meta.internal.io.{InputStreamIO, OutputStreamIO}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k, Property => p}
import scala.meta.internal.semanticdb.{Language => l}

import java.io.{InputStream, OutputStream}

package object semanticdb {

  val NoType = Type.Empty
  val NoConstant = Constant.Empty
  val NoSignature = Signature.Empty
  val NoTree = Tree.Empty
  val NoAccess = Access.Empty

  private[meta] val AnnotationTree = Annotation
  private[meta] type AnnotationTree = Annotation

  implicit class XtensionSemanticdbSymbolInformation(private val info: SymbolInformation)
      extends AnyVal {
    def isScala: Boolean = isLang(l.SCALA)
    def isJava: Boolean = isLang(l.JAVA)

    def isLocal: Boolean = isKind(k.LOCAL)
    def isField: Boolean = isKind(k.FIELD)
    def isMethod: Boolean = isKind(k.METHOD)
    def isConstructor: Boolean = isKind(k.CONSTRUCTOR)
    def isMacro: Boolean = isKind(k.MACRO)
    def isType: Boolean = isKind(k.TYPE)
    def isParameter: Boolean = isKind(k.PARAMETER)
    def isSelfParameter: Boolean = isKind(k.SELF_PARAMETER)
    def isTypeParameter: Boolean = isKind(k.TYPE_PARAMETER)
    def isObject: Boolean = isKind(k.OBJECT)
    def isPackage: Boolean = isKind(k.PACKAGE)
    def isPackageObject: Boolean = isKind(k.PACKAGE_OBJECT)
    def isClass: Boolean = isKind(k.CLASS)
    def isInterface: Boolean = isKind(k.INTERFACE)
    def isTrait: Boolean = isKind(k.TRAIT)

    def isAbstract: Boolean = hasProperty(p.ABSTRACT)
    def isOverride: Boolean = hasProperty(p.OVERRIDE)
    def isAbstractOverride: Boolean = hasProperties(in = p.ABSTRACT.value | p.OVERRIDE.value)
    def isAbstractNoOverride: Boolean = hasProperties(in = p.ABSTRACT.value, out = p.OVERRIDE.value)
    def isFinal: Boolean = hasProperty(p.FINAL)
    def isSealed: Boolean = hasProperty(p.SEALED)
    def isImplicit: Boolean = hasProperty(p.IMPLICIT)
    def isLazy: Boolean = hasProperty(p.LAZY)
    def isCase: Boolean = hasProperty(p.CASE)
    def isCovariant: Boolean = hasProperty(p.COVARIANT)
    def isContravariant: Boolean = hasProperty(p.CONTRAVARIANT)
    def isVal: Boolean = hasProperty(p.VAL)
    def isVar: Boolean = hasProperty(p.VAR)
    def isStatic: Boolean = hasProperty(p.STATIC)
    def isPrimary: Boolean = hasProperty(p.PRIMARY)
    def isEnum: Boolean = hasProperty(p.ENUM)
    def isDefault: Boolean = hasProperty(p.DEFAULT)
    def isGiven: Boolean = hasProperty(p.GIVEN)
    def isInline: Boolean = hasProperty(p.INLINE)
    def isOpen: Boolean = hasProperty(p.OPEN)
    def isTransparent: Boolean = hasProperty(p.TRANSPARENT)
    def isInfix: Boolean = hasProperty(p.INFIX)
    def isOpaque: Boolean = hasProperty(p.OPAQUE)

    def isPrivate: Boolean = info.access.isInstanceOf[PrivateAccess]
    def isPrivateThis: Boolean = info.access.isInstanceOf[PrivateThisAccess]
    def isPrivateWithin: Boolean = info.access.isInstanceOf[PrivateWithinAccess]
    def isProtected: Boolean = info.access.isInstanceOf[ProtectedAccess]
    def isProtectedThis: Boolean = info.access.isInstanceOf[ProtectedThisAccess]
    def isProtectedWithin: Boolean = info.access.isInstanceOf[ProtectedWithinAccess]
    def isPublic: Boolean = info.access.isInstanceOf[PublicAccess]
    def within: Option[String] = info.access match {
      case PrivateWithinAccess(symbol) => Some(symbol)
      case ProtectedWithinAccess(symbol) => Some(symbol)
      case _ => None
    }

    @inline
    private def isKind(kind: k): Boolean = info.kind eq kind
    @inline
    private def isLang(lang: l): Boolean = info.language eq lang
    @inline
    private def hasProperties(in: Int, out: Int = 0): Boolean = (info.properties & (in | out)) == in
    @inline
    private def hasAnyProperties(bitmask: Int): Boolean = (info.properties & bitmask) != 0
    @inline
    private def hasProperty(prop: p): Boolean = hasAnyProperties(prop.value)
  }

  implicit class XtensionSemanticdbScope(private val scope: Scope) extends AnyVal {
    def symbols: List[String] =
      if (scope.symlinks.nonEmpty) scope.symlinks.toList else scope.hardlinks.map(_.symbol).toList
    def infos: List[SymbolInformation] =
      if (scope.symlinks.nonEmpty) scope.symlinks.map(symbol => SymbolInformation(symbol = symbol))
        .toList
      else scope.hardlinks.toList
  }

  implicit class XtensionSemanticdbScopeOpt(private val scopeOpt: Option[Scope]) extends AnyVal {
    def symbols: List[String] = scopeOpt.map(_.symbols).getOrElse(Nil)
    def infos: List[SymbolInformation] = scopeOpt.map(_.infos).getOrElse(Nil)
  }

  implicit class XtensionSemanticdbScopes(private val scopes: Seq[Scope]) extends AnyVal {
    def symbols: List[List[String]] = scopes.map(_.symbols).toList
    def infos: List[List[SymbolInformation]] = scopes.map(_.infos).toList
  }

  implicit class XtensionSemanticdbType(private val tpe: Type) extends AnyVal {
    def nonEmpty: Boolean = tpe.isDefined
  }

  implicit class XtensionSemanticdbSignature(private val sig: Signature) extends AnyVal {
    def nonEmpty: Boolean = sig.isDefined
  }

  implicit class XtensionSemanticdbConstant(private val const: Constant) extends AnyVal {
    def nonEmpty: Boolean = const.isDefined

    def value: Option[Any] = const match {
      case NoConstant => None
      case UnitConstant() => Some(())
      case BooleanConstant(value) => Some(value)
      case ByteConstant(value) => Some(value.toByte)
      case ShortConstant(value) => Some(value.toShort)
      case CharConstant(value) => Some(value.toChar)
      case IntConstant(value) => Some(value)
      case LongConstant(value) => Some(value)
      case FloatConstant(value) => Some(value)
      case DoubleConstant(value) => Some(value)
      case StringConstant(value) => Some(value)
      case NullConstant() => Some(null)
    }
  }

  implicit class XtensionSemanticdbConstantCompanion(private val const: Constant.type)
      extends AnyVal {
    def apply(value: Any): Constant = value match {
      case () => UnitConstant()
      case value: Boolean => BooleanConstant(value)
      case value: Byte => ByteConstant(value.toInt)
      case value: Short => ShortConstant(value.toInt)
      case value: Char => CharConstant(value.toInt)
      case value: Int => IntConstant(value)
      case value: Long => LongConstant(value)
      case value: Float => FloatConstant(value)
      case value: Double => DoubleConstant(value)
      case value: String => StringConstant(value)
      case null => NullConstant()
      case _ => sys.error(s"unsupported value ${value.getClass} $value")
    }
  }

  implicit class XtensionSemanticdbTree(private val tree: Tree) extends AnyVal {
    def nonEmpty: Boolean = tree.isDefined
  }

  implicit class XtensionSemanticdbAccess(private val access: Access) extends AnyVal {
    def nonEmpty: Boolean = access.isDefined
  }

  implicit object ImplicitTextDocumentsInputStreamIO extends InputStreamIO[TextDocuments] {
    def read(is: Array[Byte]): TextDocuments = TextDocuments.parseFrom(is)
    def read(is: InputStream): TextDocuments = TextDocuments.parseFrom(is)
  }

  implicit object ImplicitTextDocumentsOutputStreamIO extends OutputStreamIO[TextDocuments] {
    def write(obj: TextDocuments, os: OutputStream): Unit = obj.writeTo(os)
  }

}
