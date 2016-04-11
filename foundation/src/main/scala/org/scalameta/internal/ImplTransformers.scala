package org.scalameta
package internal

import scala.reflect.macros.blackbox.Context

trait ImplTransformers {
  val c: Context
  import c.universe._
  import Flag._

  implicit class XtensionAnnotteeTransformer(annottees: Seq[Tree]) {
    def transformAnnottees(transformer: ImplTransformer): Tree = {
      transformer.transform(annottees: _*)
    }
  }

  class ImplTransformer {
    def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = ???
    def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = ???
    def transformModule(mdef: ModuleDef): ModuleDef = ???

    def transform(annottees: Tree*): Tree = {
      // TODO: find out a smarter way to approach this
      def isImplemented(body: => Any): Boolean = try { body; true } catch { case _: NotImplementedError => false; case _: Throwable => true }
      val allowClasses = isImplemented(transformClass(null, null))
      val allowTraits = isImplemented(transformTrait(null, null))
      val allowModules = isImplemented(transformModule(null))
      if (!allowClasses && !allowTraits && !allowModules) sys.error("invalid ImplTransformer")

      def failUexpectedAnnottees() = {
        var allowed = List[String]()
        if (allowClasses) allowed :+= "classes"
        if (allowTraits) allowed :+= "traits"
        if (allowModules) allowed :+= "modules"
        val s_allowed = {
          if (allowed.length > 1) allowed.dropRight(1).mkString(", ") + " and " + allowed.last
          else allowed.mkString
        }
        val q"new $s_name(...$_).macroTransform(..$_)" = c.macroApplication
        c.abort(annottees.head.pos, s"only $s_allowed can be $s_name")
      }

      val expanded = annottees match {
        case (cdef @ ClassDef(mods, _, _, _)) :: (mdef: ModuleDef) :: rest =>
          if (!(mods hasFlag TRAIT)) {
            if (!allowClasses) failUexpectedAnnottees()
            transformClass(cdef, mdef) ++ rest
          } else {
            if (!allowTraits) failUexpectedAnnottees()
            transformTrait(cdef, mdef) ++ rest
          }
        case (cdef @ ClassDef(mods, name, _, _)) :: rest =>
          val syntheticMdef = q"object ${name.toTermName}"
          if (!(mods hasFlag TRAIT)) {
            if (!allowClasses) failUexpectedAnnottees()
            transformClass(cdef, syntheticMdef) ++ rest
          } else {
            if (!allowTraits) failUexpectedAnnottees()
            transformTrait(cdef, syntheticMdef) ++ rest
          }
        case (mdef @ ModuleDef(_, _, _)) :: rest =>
          if (!allowModules) failUexpectedAnnottees()
          transformModule(mdef) +: rest
        case annottee :: rest =>
          failUexpectedAnnottees()
      }
      q"{ ..$expanded; () }"
    }
  }
}