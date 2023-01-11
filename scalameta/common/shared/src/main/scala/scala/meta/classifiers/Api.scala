package scala.meta
package classifiers

private[meta] trait Api {
  implicit class XtensionClassifiable[T: Classifiable](x: T) {
    type C[U] = Classifier[T, U]

    def is[U](implicit c: C[U]): Boolean = c(x)
    def isNot[U](implicit c: C[U]): Boolean = !c(x)

    def isAny[U1, U2](implicit c1: C[U1], c2: C[U2]): Boolean =
      c1(x) || c2(x)

    def isAny[U1, U2, U3](implicit c1: C[U1], c2: C[U2], c3: C[U3]): Boolean =
      c1(x) || c2(x) || c3(x)

    def isAny[U1, U2, U3, U4](implicit c1: C[U1], c2: C[U2], c3: C[U3], c4: C[U4]): Boolean =
      c1(x) || c2(x) || c3(x) || c4(x)
  }
}

private[meta] trait Aliases {}
