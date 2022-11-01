package scala.meta
package internal
package trees

import scala.annotation.StaticAnnotation
import scala.annotation.meta.getter
import org.scalameta.adt.Metadata.Adt

object Metadata {
  trait Ast extends Adt
  class root extends StaticAnnotation
  class branch extends StaticAnnotation
  class astClass extends StaticAnnotation
  class newField(since: String) extends StaticAnnotation
  class replacedField(until: String, ctor: Any = null) extends StaticAnnotation
  class astCompanion extends StaticAnnotation
  @getter class astField extends StaticAnnotation
  @getter class auxiliary extends StaticAnnotation
  class registry(paths: List[String]) extends StaticAnnotation
}
