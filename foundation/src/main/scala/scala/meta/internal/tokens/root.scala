package scala.meta
package internal
package tokens

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import org.scalameta.internal.MacroHelpers

// @root is a specialized version of @org.scalameta.adt.root for scala.meta tokens.
class root extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RootNamerMacros.impl
}

class RootNamerMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      val anns1 = q"new $TokenMetadataModule.root" +: q"new $AdtPackage.root" +: anns
      val parents1 = parents :+ tq"$TokenMetadataModule.Token" :+ tq"_root_.scala.Product" :+ tq"_root_.scala.Serializable"
      List(ClassDef(Modifiers(flags, privateWithin, anns1), name, tparams, Template(parents1, self, stats)), mdef)
    }
  })
}