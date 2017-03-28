package scala.meta
package internal
package classifiers

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class classifier extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ClassifierMacros.classifier
}
