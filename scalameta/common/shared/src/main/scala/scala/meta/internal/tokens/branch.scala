package scala.meta.internal.tokens

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BranchNamerMacros.impl
}

class BranchNamerMacros(val c: Context) extends TokenNamerMacroHelpers {
  import c.universe._

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] =
      transformImpl(cdef, mdef)
    override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] =
      transformImpl(cdef, mdef)

    private def transformImpl(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val ClassDef(mods, name, tparams, template) = cdef
      val mods1 = mods
        .mapAnnotations(_ :+ q"new $TokenMetadataModule.branch" :+ q"new $AdtPackage.branch")
      val cdef1 = ClassDef(mods1, name, tparams, template)

      val ModuleDef(mmods, mname, Template(mparents, mself, mstats)) = mdef
      val mstats1 = mstats ++ getClassifierBoilerplate(cdef, true)
      val mdef1 = ModuleDef(mmods, mname, Template(mparents, mself, mstats1))

      List(cdef1, mdef1)
    }
  })
}
