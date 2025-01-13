package scala.meta
package internal
package trees

import org.scalameta.adt.Metadata.Adt

import scala.annotation.StaticAnnotation
import scala.annotation.meta.getter

object Metadata {
  trait Ast extends Adt
  class root extends StaticAnnotation
  class branch extends StaticAnnotation
  class astClass extends StaticAnnotation
  class newField(after: String) extends StaticAnnotation
  class replacedField(until: String, pos: Int = -1) extends StaticAnnotation
  class replacesFields(after: String, ctor: Any) extends StaticAnnotation
  class astCompanion extends StaticAnnotation
  @getter
  class astField extends StaticAnnotation
  @getter
  class auxiliary extends StaticAnnotation
  class registry(paths: List[String]) extends StaticAnnotation
}
