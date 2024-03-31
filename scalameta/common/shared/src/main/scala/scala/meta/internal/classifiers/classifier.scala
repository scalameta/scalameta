package scala.meta
package internal
package classifiers

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

class classifier extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ClassifierMacros.classifier
}
