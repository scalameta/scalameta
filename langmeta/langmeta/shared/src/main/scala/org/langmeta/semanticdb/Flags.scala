package org.langmeta
package semanticdb

import org.langmeta.internal.semanticdb.DeprecationMessage
private[semanticdb] trait Flags {
  @deprecated(DeprecationMessage, "3.8.0")
  final val VAL: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val VAR: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val DEF: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val METHOD: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val PRIMARYCTOR: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val SECONDARYCTOR: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val MACRO: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val TYPE: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val PARAM: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val TYPEPARAM: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val OBJECT: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val PACKAGE: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val PACKAGEOBJECT: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val CLASS: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val TRAIT: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val PRIVATE: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val PROTECTED: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val ABSTRACT: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val FINAL: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val SEALED: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val IMPLICIT: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val LAZY: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val CASE: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val COVARIANT: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val CONTRAVARIANT: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val INLINE: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val JAVADEFINED: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val GETTER: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val SETTER: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val SELFPARAM: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val INTERFACE: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val LOCAL: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val FIELD: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val CTOR: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val PRIMARY: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val ENUM: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val STATIC: Long = 0
}

private[semanticdb] trait HasFlags {
  @deprecated(DeprecationMessage, "3.8.0")
  def flags: Long
  @deprecated(DeprecationMessage, "3.8.0")
  def hasFlag(flag: Long): Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isVal: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isVar: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isDef: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isMethod: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isGetter: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isSetter: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isPrimaryCtor: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isSecondaryCtor: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isMacro: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isType: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isParam: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isSelfParam: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isTypeParam: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isObject: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isPackage: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isPackageObject: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isClass: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isTrait: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isInterface: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isPrivate: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isProtected: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isAbstract: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isFinal: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isSealed: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isImplicit: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isLazy: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isCase: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isCovariant: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isContravariant: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isInline: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isJavaDefined: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isLocal: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isField: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isCtor: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isPrimary: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isEnum: Boolean = throw new UnsupportedOperationException()
  @deprecated(DeprecationMessage, "3.8.0")
  def isStatic: Boolean = throw new UnsupportedOperationException()

  protected def flagSyntax: String = throw new UnsupportedOperationException()

  protected def flagStructure: String = throw new UnsupportedOperationException()
}
