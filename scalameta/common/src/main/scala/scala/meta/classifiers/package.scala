package scala.meta

package object classifiers extends classifiers.Api {
  // Note. This is an optimization. We inline this implicit class in this package
  // in order to make it extend AnyVal, and thus making it a value class.
  // JMH  benchmarks show this optimization yields a ~30% speedup in code that
  // uses `.is` a lot.
  final implicit class XtensionClassifiable[T](val x: T) extends AnyVal {
    def is[U](implicit classifier: classifiers.Classifier[T, U], classifiable: Classifiable[T]): Boolean = {
      classifier.apply(x)
    }
    def isNot[U](implicit classifier: classifiers.Classifier[T, U], classifiable: Classifiable[T]): Boolean = {
      !this.is(classifier, classifiable)
    }
  }
}
