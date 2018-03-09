package org.langmeta
package semanticdb

private[semanticdb] trait Flags {
  final val VAL: Long = 1 << 0
  final val VAR: Long = 1 << 1
  @deprecated("Use `METHOD` instead.", "3.6.0") final val DEF: Long = METHOD
  final val METHOD: Long = 1 << 2
  final val PRIMARYCTOR: Long = 1 << 3
  final val SECONDARYCTOR: Long = 1 << 4
  final val MACRO: Long = 1 << 5
  final val TYPE: Long = 1 << 6
  final val PARAM: Long = 1 << 7
  final val TYPEPARAM: Long = 1 << 8
  final val OBJECT: Long = 1 << 9
  final val PACKAGE: Long = 1 << 10
  final val PACKAGEOBJECT: Long = 1 << 11
  final val CLASS: Long = 1 << 12
  final val TRAIT: Long = 1 << 13
  final val PRIVATE: Long = 1 << 14
  final val PROTECTED: Long = 1 << 15
  final val ABSTRACT: Long = 1 << 16
  final val FINAL: Long = 1 << 17
  final val SEALED: Long = 1 << 18
  final val IMPLICIT: Long = 1 << 19
  final val LAZY: Long = 1 << 20
  final val CASE: Long = 1 << 21
  final val COVARIANT: Long = 1 << 22
  final val CONTRAVARIANT: Long = 1 << 23
  final val INLINE: Long = 1 << 24
  final val JAVADEFINED: Long = 1 << 25
  @deprecated("Use `METHOD | VAL` instead.", "3.6.0") final val GETTER: Long = 1 << 26
  @deprecated("Use `METHOD | VAR` instead.", "3.6.0") final val SETTER: Long = 1 << 27
  final val SELFPARAM: Long = 1 << 28
  final val INTERFACE: Long = 1 << 29
  final val LOCAL: Long = 1 << 30
  final val FIELD: Long = 1 << 31
}

private[semanticdb] trait HasFlags {
  def flags: Long
  def hasFlag(flag: Long): Boolean = (flags & flag) == flag

  def isVal: Boolean = hasFlag(VAL)
  def isVar: Boolean = hasFlag(VAR)
  @deprecated("Use `isMethod` instead.", "3.6.0") def isDef: Boolean = isMethod
  def isMethod: Boolean = hasFlag(METHOD)
  @deprecated("Use `isDef && isVal` instead.", "3.6.0") def isGetter: Boolean = hasFlag(GETTER)
  @deprecated("Use `isDef && isVar` instead.", "3.6.0") def isSetter: Boolean = hasFlag(SETTER)
  def isPrimaryCtor: Boolean = hasFlag(PRIMARYCTOR)
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
    if (hasFlag(VAL)) append("VAL")
    if (hasFlag(VAR)) append("VAR")
    if (hasFlag(METHOD)) append("METHOD")
    if (hasFlag(PRIMARYCTOR)) append("PRIMARYCTOR")
    if (hasFlag(SECONDARYCTOR)) append("SECONDARYCTOR")
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
    buf.toString.toLowerCase
  }

  protected def flagStructure: String = {
    flagSyntax.replace(" ", " | ").toUpperCase
  }
}
