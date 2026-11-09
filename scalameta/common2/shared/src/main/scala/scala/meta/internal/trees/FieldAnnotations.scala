package scala.meta.internal.trees

import scala.annotation.{StaticAnnotation, nowarn}

// macros read the arguments of these annotations from the tree
@nowarn("cat=unused-params")
object FieldAnnotations {
  class newField(after: String) extends StaticAnnotation
  class replacedField(until: String, pos: Int = -1) extends StaticAnnotation
  class replacesFields(after: String, ctor: Any) extends StaticAnnotation
  // the elements of the field may sit in several trees: the tree does not adopt them
  class shared extends StaticAnnotation
}
