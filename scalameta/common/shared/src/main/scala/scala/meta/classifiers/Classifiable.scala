package scala.meta
package classifiers

// Marker trait that signifies that the author of the data structure
// allows usage of `.is[T]` and `.isNot[T]` on it.
trait Classifiable[T]
