package scala.meta
package classifiers

import scala.reflect.ClassTag
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

private[meta] trait Api {
  implicit class XtensionClassifiable[T: Classifiable](x: T) {
    def is[U](implicit classifier: Classifier[T, U]): Boolean = {
      classifier.apply(x)
    }

    def isNot[U](implicit classifier: Classifier[T, U]): Boolean = {
      !this.is(classifier)
    }
  }
}

private[meta] trait Aliases {
}
