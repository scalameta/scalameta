package scala.meta
package classifiers

private[meta] trait Api {
  implicit class XtensionClassifiable[T: Classifiable](x: T) {
    type C[U] = Classifier[T, U]

    def is[U](implicit c: C[U]): Boolean = c(x)
    def isNot[U](implicit c: C[U]): Boolean = !c(x)

    def isAnyOf(cs: C[_]*): Boolean = cs.exists(_(x))

    def isAny[U1, U2](implicit c1: C[U1], c2: C[U2]): Boolean = c1(x) || c2(x)

    def isAny[U1, U2, U3](implicit c1: C[U1], c2: C[U2], c3: C[U3]): Boolean = c1(x) || c2(x) ||
      c3(x)

    def isAny[U1, U2, U3, U4](implicit c1: C[U1], c2: C[U2], c3: C[U3], c4: C[U4]): Boolean =
      c1(x) || c2(x) || c3(x) || c4(x)
  }

  implicit class XtensionOptionClassifiable[T: Classifiable](x: Option[T]) {
    type C[U] = Classifier[T, U]

    def is[U](implicit c: C[U]): Boolean = x.exists(c.apply)
    def isOpt[U](implicit c: C[U]): Boolean = x.forall(c.apply)

    def isAnyOf(cs: C[_]*): Boolean = x.exists(_.isAnyOf(cs: _*))
    def isAnyOfOpt(cs: C[_]*): Boolean = x.forall(_.isAnyOf(cs: _*))

    def isAny[U1: C, U2: C]: Boolean = x.exists(_.isAny[U1, U2])
    def isAnyOpt[U1: C, U2: C]: Boolean = x.forall(_.isAny[U1, U2])

    def isAny[U1: C, U2: C, U3: C]: Boolean = x.exists(_.isAny[U1, U2, U3])
    def isAnyOpt[U1: C, U2: C, U3: C]: Boolean = x.forall(_.isAny[U1, U2, U3])

    def isAny[U1: C, U2: C, U3: C, U4: C]: Boolean = x.exists(_.isAny[U1, U2, U3, U4])
    def isAnyOpt[U1: C, U2: C, U3: C, U4: C]: Boolean = x.forall(_.isAny[U1, U2, U3, U4])
  }
}

private[meta] trait Aliases {}
