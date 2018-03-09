package org.langmeta
package semanticdb

private[semanticdb] trait Flags {
  final val VAL: Long = 1L << 0
  final val VAR: Long = 1L << 1
  @deprecated("Use `METHOD` instead.", "3.6.0")
  final val DEF: Long = METHOD
  final val METHOD: Long = 1L << 2
  @deprecated("Use `PRIMARY | CTOR` instead.", "3.6.0")
  final val PRIMARYCTOR: Long = 1L << 3
  @deprecated("Use `~PRIMARY` and `CTOR` instead.", "3.6.0")
  final val SECONDARYCTOR: Long = 1L << 4
  final val MACRO: Long = 1L << 5
  final val TYPE: Long = 1L << 6
  final val PARAM: Long = 1L << 7
  final val TYPEPARAM: Long = 1L << 8
  final val OBJECT: Long = 1L << 9
  final val PACKAGE: Long = 1L << 10
  final val PACKAGEOBJECT: Long = 1L << 11
  final val CLASS: Long = 1L << 12
  final val TRAIT: Long = 1L << 13
  final val PRIVATE: Long = 1L << 14
  final val PROTECTED: Long = 1L << 15
  final val ABSTRACT: Long = 1L << 16
  final val FINAL: Long = 1L << 17
  final val SEALED: Long = 1L << 18
  final val IMPLICIT: Long = 1L << 19
  final val LAZY: Long = 1L << 20
  final val CASE: Long = 1L << 21
  final val COVARIANT: Long = 1L << 22
  final val CONTRAVARIANT: Long = 1L << 23
  final val INLINE: Long = 1L << 24
  final val JAVADEFINED: Long = 1L << 25
  @deprecated("Use `METHOD | VAL` instead.", "3.6.0")
  final val GETTER: Long = 1L << 26
  @deprecated("Use `METHOD | VAR` instead.", "3.6.0")
  final val SETTER: Long = 1L << 27
  final val SELFPARAM: Long = 1L << 28
  final val INTERFACE: Long = 1L << 29
  final val LOCAL: Long = 1L << 30
  final val FIELD: Long = 1L << 31
  final val CTOR: Long = 1L << 32
  final val PRIMARY: Long = 1L << 33
}

private[semanticdb] trait HasFlags {
  def flags: Long
  def hasFlag(flag: Long): Boolean = (flags & flag) == flag

  def isVal: Boolean = hasFlag(VAL)
  def isVar: Boolean = hasFlag(VAR)
  @deprecated("Use `isMethod` instead.", "3.6.0")
  def isDef: Boolean = isMethod
  def isMethod: Boolean = hasFlag(METHOD)
  @deprecated("Use `isDef && isVal` instead.", "3.6.0")
  def isGetter: Boolean = hasFlag(GETTER)
  @deprecated("Use `isDef && isVar` instead.", "3.6.0")
  def isSetter: Boolean = hasFlag(SETTER)
  @deprecated("Use `isPrimary && isCtor` instead.", "3.6.0")
  def isPrimaryCtor: Boolean = hasFlag(PRIMARYCTOR)
  @deprecated("Use `!isPrimary && isCtor` instead.", "3.6.0")
  def isSecondaryCtor: Boolean = hasFlag(SECONDARYCTOR)
  def isMacro: Boolean = hasFlag(MACRO)
  def isType: Boolean = hasFlag(TYPE)
  def isParam: Boolean = hasFlag(PARAM)
  def isSelfParam: Boolean = hasFlag(SELFPARAM)
  def isTypeParam: Boolean = hasFlag(TYPEPARAM)
  def isObject: Boolean = hasFlag(OBJECT)
  def isPackage: Boolean = hasFlag(PACKAGE)
  def isPackageObject: Boolean = hasFlag(PACKAGEOBJECT)
  def isClass: Boolean = hasFlag(CLASS)
  def isTrait: Boolean = hasFlag(TRAIT)
  def isInterface: Boolean = hasFlag(INTERFACE)
  def isPrivate: Boolean = hasFlag(PRIVATE)
  def isProtected: Boolean = hasFlag(PROTECTED)
  def isAbstract: Boolean = hasFlag(ABSTRACT)
  def isFinal: Boolean = hasFlag(FINAL)
  def isSealed: Boolean = hasFlag(SEALED)
  def isImplicit: Boolean = hasFlag(IMPLICIT)
  def isLazy: Boolean = hasFlag(LAZY)
  def isCase: Boolean = hasFlag(CASE)
  def isCovariant: Boolean = hasFlag(COVARIANT)
  def isContravariant: Boolean = hasFlag(CONTRAVARIANT)
  def isInline: Boolean = hasFlag(INLINE)
  def isJavaDefined: Boolean = hasFlag(JAVADEFINED)
  def isLocal: Boolean = hasFlag(LOCAL)
  def isField: Boolean = hasFlag(FIELD)
  def isCtor: Boolean = hasFlag(CTOR)
  def isPrimary: Boolean = hasFlag(PRIMARY)

  protected def flagSyntax: String = {
    val buf = new StringBuilder
    def append(flag: String) = {
      if (buf.isEmpty) buf ++= flag
      else buf ++= (" " + flag)
    }
    def hasFlag(flag: Long) = (flags & flag) == flag
    if (hasFlag(PRIVATE)) append("PRIVATE")
    if (hasFlag(PROTECTED)) append("PROTECTED")
    if (hasFlag(ABSTRACT)) append("ABSTRACT")
    if (hasFlag(FINAL)) append("FINAL")
    if (hasFlag(SEALED)) append("SEALED")
    if (hasFlag(IMPLICIT)) append("IMPLICIT")
    if (hasFlag(LAZY)) append("LAZY")
    if (hasFlag(CASE)) append("CASE")
    if (hasFlag(COVARIANT)) append("COVARIANT")
    if (hasFlag(CONTRAVARIANT)) append("CONTRAVARIANT")
    if (hasFlag(INLINE)) append("INLINE")
    if (hasFlag(JAVADEFINED)) append("JAVADEFINED")
    if (hasFlag(PRIMARY)) append("PRIMARY")
    if (hasFlag(VAL)) append("VAL")
    if (hasFlag(VAR)) append("VAR")
    if (hasFlag(METHOD)) append("METHOD")
    if (hasFlag(MACRO)) append("MACRO")
    if (hasFlag(TYPE)) append("TYPE")
    if (hasFlag(PARAM)) append("PARAM")
    if (hasFlag(SELFPARAM)) append("SELFPARAM")
    if (hasFlag(TYPEPARAM)) append("TYPEPARAM")
    if (hasFlag(OBJECT)) append("OBJECT")
    if (hasFlag(PACKAGE)) append("PACKAGE")
    if (hasFlag(PACKAGEOBJECT)) append("PACKAGEOBJECT")
    if (hasFlag(CLASS)) append("CLASS")
    if (hasFlag(TRAIT)) append("TRAIT")
    if (hasFlag(INTERFACE)) append("INTERFACE")
    if (hasFlag(LOCAL)) append("LOCAL")
    if (hasFlag(FIELD)) append("FIELD")
    if (hasFlag(CTOR)) append("CTOR")
    buf.toString.toLowerCase
  }

  protected def flagStructure: String = {
    flagSyntax.replace(" ", " | ").toUpperCase
  }
}
