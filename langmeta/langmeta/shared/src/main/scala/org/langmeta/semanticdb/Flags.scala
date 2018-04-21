package org.langmeta
package semanticdb

import org.langmeta.internal.semanticdb.DeprecationMessage
private[semanticdb] trait Flags {
  @deprecated(DeprecationMessage, "3.8.0")
  final val VAL: Long = 1L << 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val VAR: Long = 1L << 1
  @deprecated(DeprecationMessage, "3.8.0")
  final val DEF: Long = 0
  @deprecated(DeprecationMessage, "3.8.0")
  final val METHOD: Long = 1L << 2
  @deprecated(DeprecationMessage, "3.8.0")
  final val PRIMARYCTOR: Long = 1L << 3
  @deprecated(DeprecationMessage, "3.8.0")
  final val SECONDARYCTOR: Long = 1L << 4
  @deprecated(DeprecationMessage, "3.8.0")
  final val MACRO: Long = 1L << 5
  @deprecated(DeprecationMessage, "3.8.0")
  final val TYPE: Long = 1L << 6
  @deprecated(DeprecationMessage, "3.8.0")
  final val PARAM: Long = 1L << 7
  @deprecated(DeprecationMessage, "3.8.0")
  final val TYPEPARAM: Long = 1L << 8
  @deprecated(DeprecationMessage, "3.8.0")
  final val OBJECT: Long = 1L << 9
  @deprecated(DeprecationMessage, "3.8.0")
  final val PACKAGE: Long = 1L << 10
  @deprecated(DeprecationMessage, "3.8.0")
  final val PACKAGEOBJECT: Long = 1L << 11
  @deprecated(DeprecationMessage, "3.8.0")
  final val CLASS: Long = 1L << 12
  @deprecated(DeprecationMessage, "3.8.0")
  final val TRAIT: Long = 1L << 13
  @deprecated(DeprecationMessage, "3.8.0")
  final val PRIVATE: Long = 1L << 14
  @deprecated(DeprecationMessage, "3.8.0")
  final val PROTECTED: Long = 1L << 15
  @deprecated(DeprecationMessage, "3.8.0")
  final val ABSTRACT: Long = 1L << 16
  @deprecated(DeprecationMessage, "3.8.0")
  final val FINAL: Long = 1L << 17
  @deprecated(DeprecationMessage, "3.8.0")
  final val SEALED: Long = 1L << 18
  @deprecated(DeprecationMessage, "3.8.0")
  final val IMPLICIT: Long = 1L << 19
  @deprecated(DeprecationMessage, "3.8.0")
  final val LAZY: Long = 1L << 20
  @deprecated(DeprecationMessage, "3.8.0")
  final val CASE: Long = 1L << 21
  @deprecated(DeprecationMessage, "3.8.0")
  final val COVARIANT: Long = 1L << 22
  @deprecated(DeprecationMessage, "3.8.0")
  final val CONTRAVARIANT: Long = 1L << 23
  @deprecated(DeprecationMessage, "3.8.0")
  final val INLINE: Long = 1L << 24
  @deprecated(DeprecationMessage, "3.8.0")
  final val JAVADEFINED: Long = 1L << 25
  @deprecated(DeprecationMessage, "3.8.0")
  final val GETTER: Long = 1L << 26
  @deprecated(DeprecationMessage, "3.8.0")
  final val SETTER: Long = 1L << 27
  @deprecated(DeprecationMessage, "3.8.0")
  final val SELFPARAM: Long = 1L << 28
  @deprecated(DeprecationMessage, "3.8.0")
  final val INTERFACE: Long = 1L << 29
  @deprecated(DeprecationMessage, "3.8.0")
  final val LOCAL: Long = 1L << 30
  @deprecated(DeprecationMessage, "3.8.0")
  final val FIELD: Long = 1L << 31
  @deprecated(DeprecationMessage, "3.8.0")
  final val CTOR: Long = 1L << 32
  @deprecated(DeprecationMessage, "3.8.0")
  final val PRIMARY: Long = 1L << 33
  @deprecated(DeprecationMessage, "3.8.0")
  final val ENUM: Long = 1L << 34
  @deprecated(DeprecationMessage, "3.8.0")
  final val STATIC: Long = 1L << 35
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
  def isDef: Boolean = isMethod
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
