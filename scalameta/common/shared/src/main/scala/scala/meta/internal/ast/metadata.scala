package scala.meta
package internal
package ast

import scala.annotation.StaticAnnotation
import scala.annotation.meta.getter
import org.scalameta.adt.Metadata.Adt

object Metadata {
  trait Ast extends Adt
  class root extends StaticAnnotation
  class branch extends StaticAnnotation
  class astClass extends StaticAnnotation
  class astCompanion extends StaticAnnotation
  @getter class astField extends StaticAnnotation
  @getter class auxiliary extends StaticAnnotation
  class registry(paths: List[String]) extends StaticAnnotation
}
