package scala.meta
package internal
package tokens

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

// @root is a specialized version of @org.scalameta.adt.root for scala.meta tokens.
class root extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RootNamerMacros.impl
}

class RootNamerMacros(val c: Context) extends TokenNamerMacroHelpers {
  import c.universe._

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val ClassDef(mods, name, tparams, Template(parents, self, stats)) = cdef
      val mods1 = mods
        .mapAnnotations(_ :+ q"new $TokenMetadataModule.root" :+ q"new $AdtPackage.root")
      val parents1 = parents :+ tq"$TokenMetadataModule.Token" :+ tq"_root_.scala.Product" :+
        tq"_root_.scala.Serializable"
      val cdef1 = ClassDef(mods1, name, tparams, Template(parents1, self, stats))

      val ModuleDef(mmods, mname, Template(mparents, mself, mstats)) = mdef
      val mstats1 = mstats ++ getClassifierBoilerplateRoot()
      val mdef1 = ModuleDef(mmods, mname, Template(mparents, mself, mstats1))

      List(cdef1, mdef1)
    }
  })
}
