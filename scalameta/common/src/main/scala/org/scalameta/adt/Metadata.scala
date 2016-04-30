package org.scalameta.adt

import scala.annotation.StaticAnnotation

object Metadata {
  trait Adt
  class root extends StaticAnnotation
  class branch extends StaticAnnotation
  class leafClass extends StaticAnnotation
  class leafCompanion extends StaticAnnotation
  class noneClass extends StaticAnnotation
  class byNeedField extends StaticAnnotation
}
