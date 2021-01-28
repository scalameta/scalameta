package scala.meta
package classifiers

import scala.util.control.NonFatal

private[meta] trait Api {
  implicit class XtensionClassifiable[T: Classifiable](x: T) {
    def is[U](implicit classifier: Classifier[T, U]): Boolean = {
      classifier.apply(x)
    }

    def isNot[U](implicit classifier: Classifier[T, U]): Boolean = {
      !this.is(classifier)
    }

    def ignoringErrorIs[U](implicit classifier: Classifier[T, U]): Boolean = {
      try {
        classifier.apply(x)
      } catch {
        case NonFatal(e) => false
      }
    }
  }
}

private[meta] trait Aliases {}
