package scala.meta
package semantic

import org.scalameta.adt._
import org.scalameta.data._

@data class Denotation(flags: Long, name: String, info: String) extends HasFlags {
  def hasFlag(flag: Long) = (flags & flag) == flag

  def syntax = s"$flagSyntax $name" + (if (info != "") ": " + info else "")
  private def flagSyntax: String = flagStructure.replace(" | ", " ").toLowerCase

  def structure = s"""Denotation($flagStructure, "$name", "$info")"""
  private def flagStructure: String = {
    val buf = new StringBuilder
    def append(flag: String) = {
      if (buf.isEmpty) buf ++= flag
      else buf ++= (" | " + flag)
    }
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

    if (hasFlag(VAL)) append("VAL")
    if (hasFlag(VAR)) append("VAR")
    if (hasFlag(DEF)) append("DEF")
    if (hasFlag(PRIMARYCTOR)) append("PRIMARYCTOR")
    if (hasFlag(SECONDARYCTOR)) append("SECONDARYCTOR")
    if (hasFlag(MACRO)) append("MACRO")
    if (hasFlag(TYPE)) append("TYPE")
    if (hasFlag(TERMPARAM)) append("TERMPARAM")
    if (hasFlag(TYPEPARAM)) append("TYPEPARAM")
    if (hasFlag(OBJECT)) append("OBJECT")
    if (hasFlag(PACKAGE)) append("PACKAGE")
    if (hasFlag(PACKAGEOBJECT)) append("PACKAGEOBJECT")
    if (hasFlag(CLASS)) append("CLASS")
    if (hasFlag(TRAIT)) append("TRAIT")
    buf.toString
  }

  override def toString = syntax
}

private[semantic] trait Flags {
  final val VAL: Long = 1 << 0
  final val VAR: Long = 1 << 1
  final val DEF: Long = 1 << 2
  final val PRIMARYCTOR: Long = 1 << 3
  final val SECONDARYCTOR: Long = 1 << 4
  final val MACRO: Long = 1 << 5
  final val TYPE: Long = 1 << 6
  final val TERMPARAM: Long = 1 << 7
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
}

private[semantic] trait HasFlags {
  def hasFlag(flag: Long): Boolean

  def isVal: Boolean = hasFlag(VAL)
  def isVar: Boolean = hasFlag(VAR)
  def isDef: Boolean = hasFlag(DEF)
  def isPrimaryCtor: Boolean = hasFlag(PRIMARYCTOR)
  def isSecondaryCtor: Boolean = hasFlag(SECONDARYCTOR)
  def isMacro: Boolean = hasFlag(MACRO)
  def isType: Boolean = hasFlag(TYPE)
  def isTermParam: Boolean = hasFlag(TERMPARAM)
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
}