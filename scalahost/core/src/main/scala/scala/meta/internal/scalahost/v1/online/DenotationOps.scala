package scala.meta.internal
package scalahost
package v1
package online

import scala.{meta => m}
import scala.meta.semantic.v1._
import scala.reflect.internal.Flags._ // this import is here to prevent confusing unqualified flag references
import scala.meta.semantic.v1.{Flags => mf}
import scala.reflect.internal.{Flags => gf}

trait DenotationOps { self: Mirror =>
  import g._

  implicit class XtensionGSymbolMDenotation(gsym0: g.Symbol) {
    private val gsym: g.Symbol = {
      if (gsym0.isModuleClass) gsym0.asClass.module
      else if (gsym0.isTypeSkolem) gsym0.deSkolemize
      else gsym0.setterIn(gsym0.owner).orElse(gsym0.getterIn(gsym0.owner).orElse(gsym0))
    }

    private val isObject = gsym.isModule && !gsym.hasFlag(gf.PACKAGE) && gsym.name != nme.PACKAGE

    private def definitionFlags: Long = {
      var flags = 0l
      def maybeValOrVar = (gsym.isTerm && flags == 0l) || (gsym.hasFlag(gf.PARAMACCESSOR) && flags == mf.TERMPARAM)
      if (gsym.isMethod && !gsym.isConstructor && !gsym.hasFlag(gf.MACRO) && !gsym.hasFlag(gf.ACCESSOR) && !gsym.hasFlag(gf.PARAMACCESSOR)) flags |= mf.DEF
      if (gsym.isPrimaryConstructor) flags |= mf.PRIMARYCTOR
      if (gsym.isConstructor && !gsym.isPrimaryConstructor) flags |= mf.SECONDARYCTOR
      if (gsym.hasFlag(gf.MACRO)) flags |= mf.MACRO
      if (gsym.isType && !gsym.isClass && !gsym.hasFlag(gf.PARAM)) flags |= mf.TYPE
      if (gsym.isTerm && (gsym.hasFlag(gf.PARAM) || gsym.hasFlag(gf.PARAMACCESSOR))) flags |= mf.TERMPARAM
      if (gsym.isType && gsym.hasFlag(gf.PARAM)) flags |= mf.TYPEPARAM
      if (isObject) flags |= mf.OBJECT
      if (gsym.hasFlag(gf.PACKAGE)) flags |= mf.PACKAGE
      if (gsym.isModule && gsym.name == nme.PACKAGE) flags |= mf.PACKAGEOBJECT
      if (gsym.isClass && !gsym.hasFlag(gf.TRAIT)) flags |= mf.CLASS
      if (gsym.isClass && gsym.hasFlag(gf.TRAIT)) flags |= mf.TRAIT
      if (maybeValOrVar && (gsym.hasFlag(MUTABLE) || nme.isSetterName(gsym.name))) flags |= mf.VAR
      if (maybeValOrVar && !(gsym.hasFlag(LOCAL) && gsym.hasFlag(PARAMACCESSOR))) flags |= mf.VAL
      flags
    }

    private def accessQualifierFlags: Long = {
      var flags = 0l
      val gpriv = gsym.privateWithin.orElse(gsym.owner)
      if (gsym.hasFlag(gf.SYNTHETIC) && gsym.hasFlag(gf.ARTIFACT)) {
        // NOTE: some sick artifact vals produced by mkPatDef can be private to method (whatever that means)
        // so here we just ignore them
      } else {
        if (gsym.hasFlag(gf.PROTECTED)) flags |= mf.PROTECTED
        if (gsym.hasFlag(gf.PRIVATE) && !gsym.hasFlag(gf.PARAMACCESSOR)) flags |= mf.PRIVATE
        // TODO: `private[pkg] class C` doesn't have PRIVATE in its flags
        // so we need to account for that!
        if (gsym.hasAccessBoundary && gpriv != g.NoSymbol && !gsym.hasFlag(gf.PROTECTED)) flags |= mf.PRIVATE
      }
      flags
    }

    private def otherFlags: Long = {
      var flags = 0l
      val isDeclaredDeferred = gsym.hasFlag(gf.DEFERRED) && !gsym.hasFlag(gf.PARAM)
      val isDeclaredAbstract = (gsym.hasFlag(gf.ABSTRACT) && !gsym.hasFlag(gf.TRAIT)) || gsym.hasFlag(gf.ABSOVERRIDE)
      if (isDeclaredDeferred || isDeclaredAbstract) flags |= mf.ABSTRACT
      if ((gsym.hasFlag(gf.FINAL) && !gsym.hasFlag(gf.PACKAGE)) || isObject) flags |= mf.FINAL
      if (gsym.hasFlag(gf.SEALED)) flags |= mf.SEALED
      if (gsym.hasFlag(gf.IMPLICIT)) flags |= mf.IMPLICIT
      if (gsym.hasFlag(gf.LAZY)) flags |= mf.LAZY
      if (gsym.hasFlag(gf.CASE)) flags |= mf.CASE
      if (gsym.isType && gsym.hasFlag(gf.CONTRAVARIANT)) flags |= mf.CONTRAVARIANT
      if (gsym.isType && gsym.hasFlag(gf.COVARIANT)) flags |= mf.COVARIANT
      // TODO: mf.INLINE
      flags
    }

    def toDenotation: m.Denotation = {
      val flags = definitionFlags | accessQualifierFlags | otherFlags
      m.Denotation(flags)
    }
  }
}
