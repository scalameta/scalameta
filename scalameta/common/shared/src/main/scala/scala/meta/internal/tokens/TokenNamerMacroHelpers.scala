package scala.meta.internal.tokens

import org.scalameta.internal.MacroHelpers

import scala.language.experimental.macros

trait TokenNamerMacroHelpers extends MacroHelpers {
  import c.universe._

  protected val Token = tq"_root_.scala.meta.tokens.Token"
  protected val Classifier = tq"_root_.scala.meta.classifiers.Classifier"

  protected def getClassifierBoilerplateRoot() = {
    val q"..$classifierBoilerplate" = q"""
        private object sharedClassifier extends $Classifier[$Token, $Token] {
          def apply(x: $Token): _root_.scala.Boolean = true
        }
        implicit def classifier[T <: $Token]: $Classifier[T, $Token] = {
          sharedClassifier.asInstanceOf[$Classifier[T, $Token]]
        }
      """
    classifierBoilerplate
  }

  protected def getClassifierBoilerplate(cdef: ClassDef, unapplyToo: Boolean = false) = {
    val tpe = typeRef(cdef, false, true)
    val q"..$classifierBoilerplate" = q"""
        private object sharedClassifier extends $Classifier[$Token, $tpe] {
          def apply(x: $Token): _root_.scala.Boolean = x.isInstanceOf[$tpe]
        }
        implicit def classifier[T <: $Token]: $Classifier[T, $tpe] = {
          sharedClassifier.asInstanceOf[$Classifier[T, $tpe]]
        }
      """
    val unapplies = if (unapplyToo) List(q"def unapply(x: $tpe): Boolean = true") else Nil
    classifierBoilerplate ++ unapplies
  }

}
