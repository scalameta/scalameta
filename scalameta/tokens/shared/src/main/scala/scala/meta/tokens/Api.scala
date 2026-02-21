package scala.meta
package tokens

private[meta] trait Api {
  type TokenClassifier[T] = classifiers.Classifier[Token, T]
}
