package scala.meta
package internal
package trees

import org.scalameta.adt.Metadata.Adt

import scala.annotation.meta.getter
import scala.annotation.{StaticAnnotation, nowarn}

object Metadata {
  trait Ast extends Adt
  class root extends StaticAnnotation
  class branch extends StaticAnnotation
  class astClass extends StaticAnnotation
  class astCompanion extends StaticAnnotation
  @getter
  class astField extends StaticAnnotation
  @getter
  class auxiliary extends StaticAnnotation
  @nowarn("cat=unused-params") // macros read the argument from the tree
  class registry(paths: List[String]) extends StaticAnnotation
}
