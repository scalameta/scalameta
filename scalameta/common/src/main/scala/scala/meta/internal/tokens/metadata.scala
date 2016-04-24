package scala.meta
package internal
package tokens

import scala.annotation.StaticAnnotation
import org.scalameta.adt.Metadata.Adt

object Metadata {
  trait Token extends Adt
  class root extends StaticAnnotation
  class tokenClass(name: String) extends StaticAnnotation
  class tokenCompanion extends StaticAnnotation
}
