package scala.meta

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.implicitNotFound
import scala.meta.internal.classifiers._

class classifier extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ClassifierMacros.classifier
}

package classifiers {
  @implicitNotFound("don't know how to classify ${T} as ${U}")
  trait Classifier[T, U] {
    def apply(x: T): Boolean
  }
}
