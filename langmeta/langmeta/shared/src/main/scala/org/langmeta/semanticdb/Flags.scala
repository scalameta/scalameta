package org.langmeta
package semanticdb

private[semanticdb] trait Flags {
  final val VAL: Long = 1 << 0
  final val VAR: Long = 1 << 1
  final val DEF: Long = 1 << 2
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
  final val SPECIALIZED: Long = 1 << 26
}

private[semanticdb] trait HasFlags {
  def flags: Long
  def hasFlag(flag: Long): Boolean = (flags & flag) == flag

  def isVal: Boolean = hasFlag(VAL)
  def isVar: Boolean = hasFlag(VAR)
  def isDef: Boolean = hasFlag(DEF)
  def isPrimaryCtor: Boolean = hasFlag(PRIMARYCTOR)
  def isSecondaryCtor: Boolean = hasFlag(SECONDARYCTOR)
  def isMacro: Boolean = hasFlag(MACRO)
  def isType: Boolean = hasFlag(TYPE)
  def isParam: Boolean = hasFlag(PARAM)
  def isTypeParam: Boolean = hasFlag(TYPEPARAM)
  def isObject: Boolean = hasFlag(OBJECT)
  def isPackage: Boolean = hasFlag(PACKAGE)
  def isPackageObject: Boolean = hasFlag(PACKAGEOBJECT)
  def isClass: Boolean = hasFlag(CLASS)
  def isTrait: Boolean = hasFlag(TRAIT)
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
  def isSpecialized: Boolean = hasFlag(SPECIALIZED)

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
    if (hasFlag(SPECIALIZED)) append("SPECIALIZED")
    if (hasFlag(VAL)) append("VAL")
    if (hasFlag(VAR)) append("VAR")
    if (hasFlag(DEF)) append("DEF")
    if (hasFlag(PRIMARYCTOR)) append("PRIMARYCTOR")
    if (hasFlag(SECONDARYCTOR)) append("SECONDARYCTOR")
    if (hasFlag(MACRO)) append("MACRO")
    if (hasFlag(TYPE)) append("TYPE")
    if (hasFlag(PARAM)) append("PARAM")
    if (hasFlag(TYPEPARAM)) append("TYPEPARAM")
    if (hasFlag(OBJECT)) append("OBJECT")
    if (hasFlag(PACKAGE)) append("PACKAGE")
    if (hasFlag(PACKAGEOBJECT)) append("PACKAGEOBJECT")
    if (hasFlag(CLASS)) append("CLASS")
    if (hasFlag(TRAIT)) append("TRAIT")
    buf.toString.toLowerCase
  }

  protected def flagStructure: String = {
    flagSyntax.replace(" ", " | ").toUpperCase
  }
}
