package scala.meta.internal

import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}

package object semanticdb {

  val NoType = Type.Empty
  val NoConstant = Constant.Empty
  val NoSignature = Signature.Empty
  val NoTree = Tree.Empty
  val NoAccess = Access.Empty

  implicit class XtensionSemanticdbSymbolInformation(info: SymbolInformation) {
    def isScala: Boolean = info.language == l.SCALA
    def isJava: Boolean = info.language == l.JAVA
    def isLocal: Boolean = info.kind == k.LOCAL
    def isField: Boolean = info.kind == k.FIELD
    def isMethod: Boolean = info.kind == k.METHOD
    def isConstructor: Boolean = info.kind == k.CONSTRUCTOR
    def isMacro: Boolean = info.kind == k.MACRO
    def isType: Boolean = info.kind == k.TYPE
    def isParameter: Boolean = info.kind == k.PARAMETER
    def isSelfParameter: Boolean = info.kind == k.SELF_PARAMETER
    def isTypeParameter: Boolean = info.kind == k.TYPE_PARAMETER
    def isObject: Boolean = info.kind == k.OBJECT
    def isPackage: Boolean = info.kind == k.PACKAGE
    def isPackageObject: Boolean = info.kind == k.PACKAGE_OBJECT
    def isClass: Boolean = info.kind == k.CLASS
    def isInterface: Boolean = info.kind == k.INTERFACE
    def isTrait: Boolean = info.kind == k.TRAIT
    def isAbstract: Boolean = (info.properties & p.ABSTRACT.value) != 0
    def isFinal: Boolean = (info.properties & p.FINAL.value) != 0
    def isSealed: Boolean = (info.properties & p.SEALED.value) != 0
    def isImplicit: Boolean = (info.properties & p.IMPLICIT.value) != 0
    def isLazy: Boolean = (info.properties & p.LAZY.value) != 0
    def isCase: Boolean = (info.properties & p.CASE.value) != 0
    def isCovariant: Boolean = (info.properties & p.COVARIANT.value) != 0
    def isContravariant: Boolean = (info.properties & p.CONTRAVARIANT.value) != 0
    def isVal: Boolean = (info.properties & p.VAL.value) != 0
    def isVar: Boolean = (info.properties & p.VAR.value) != 0
    def isStatic: Boolean = (info.properties & p.STATIC.value) != 0
    def isPrimary: Boolean = (info.properties & p.PRIMARY.value) != 0
    def isEnum: Boolean = (info.properties & p.ENUM.value) != 0
    def isDefault: Boolean = (info.properties & p.DEFAULT.value) != 0
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
  }

  implicit class XtensionSemanticdbScope(scope: Scope) {
    def symbols: List[String] = {
      if (scope.symlinks.nonEmpty) scope.symlinks.toList
      else scope.hardlinks.map(_.symbol).toList
    }
    def infos: List[SymbolInformation] = {
      if (scope.symlinks.nonEmpty) {
        scope.symlinks.map(symbol => SymbolInformation(symbol = symbol)).toList
      } else {
        scope.hardlinks.toList
      }
    }
  }

  implicit class XtensionSemanticdbScopeOpt(scopeOpt: Option[Scope]) {
    def symbols: List[String] = scopeOpt.map(_.symbols).getOrElse(Nil)
    def infos: List[SymbolInformation] = scopeOpt.map(_.infos).getOrElse(Nil)
  }

  implicit class XtensionSemanticdbScopes(scopes: Seq[Scope]) {
    def symbols: List[List[String]] = scopes.map(_.symbols).toList
    def infos: List[List[SymbolInformation]] = scopes.map(_.infos).toList
  }

  implicit class XtensionSemanticdbType(tpe: Type) {
    def nonEmpty: Boolean = tpe.isDefined
  }

  implicit class XtensionSemanticdbSignature(sig: Signature) {
    def nonEmpty: Boolean = sig.isDefined
  }

  implicit class XtensionSemanticdbConstant(const: Constant) {
    def nonEmpty: Boolean = const.isDefined

    def value: Option[Any] = {
      const match {
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
  }

  implicit class XtensionSemanticdbConstantCompanion(const: Constant.type) {
    def apply(value: Any): Constant = {
      value match {
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
        case _ => sys.error("unsupported value ${value.getClass} $value")
      }
    }
  }

  implicit class XtensionSemanticdbTree(tree: Tree) {
    def nonEmpty: Boolean = tree.isDefined
  }

  implicit class XtensionSemanticdbAccess(access: Access) {
    def nonEmpty: Boolean = access.isDefined
  }
}
