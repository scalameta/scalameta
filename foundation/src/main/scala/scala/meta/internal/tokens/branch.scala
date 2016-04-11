package scala.meta
package internal
package tokens

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import org.scalameta.internal.MacroHelpers

// @branch is a specialized version of @org.scalameta.adt.branch for scala.meta tokens.
class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BranchNamerMacros.impl
}

class BranchNamerMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, templ) = cdef
      val anns1 = q"new $TokenMetadataModule.branch" +: q"new $AdtPackage.branch" +: anns
      List(ClassDef(Modifiers(flags, privateWithin, anns1), name, tparams, templ), mdef)
    }
  })
}