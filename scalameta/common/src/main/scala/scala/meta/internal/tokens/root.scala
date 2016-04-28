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

  val Token = tq"_root_.scala.meta.tokens.Token"
  val Classifier = tq"_root_.scala.meta.classifiers.Classifier"

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val anns1 = q"new $TokenMetadataModule.root" +: q"new $AdtPackage.root" +: anns
      val mstats1 = scala.collection.mutable.ListBuffer[Tree]() ++ mstats

      val q"..$classifierBoilerplate" = q"""
        private object sharedClassifier extends $Classifier[$Token, $Token] {
          def apply(x: $Token): Boolean = true
        }
        implicit def classifier[T <: $Token]: $Classifier[T, $Token] = {
          sharedClassifier.asInstanceOf[$Classifier[T, $Token]]
        }
      """
      mstats1 ++= classifierBoilerplate

      val parents1 = parents :+ tq"$TokenMetadataModule.Token" :+ tq"_root_.scala.Product" :+ tq"_root_.scala.Serializable"
      val cdef1 = ClassDef(Modifiers(flags, privateWithin, anns1), name, tparams, Template(parents1, self, stats))
      val mdef1 = q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
  })
}