package scala.meta.internal.trees

import scala.annotation.StaticAnnotation

object FieldAnnotations {
  class newField(after: String) extends StaticAnnotation
  class replacedField(until: String, pos: Int = -1) extends StaticAnnotation
  class replacesFields(after: String, ctor: Any) extends StaticAnnotation
}
