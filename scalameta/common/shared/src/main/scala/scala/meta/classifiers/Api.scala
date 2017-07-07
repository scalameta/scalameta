package scala.meta
package classifiers


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
