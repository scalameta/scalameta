package scala.meta
package internal
package tokens

import org.scalameta.adt.Metadata.Adt

import scala.annotation.StaticAnnotation

object Metadata {
  trait Token extends Adt
  class root extends StaticAnnotation
  class branch extends StaticAnnotation
  class tokenClass(name: String, freeform: Boolean) extends StaticAnnotation
  class tokenCompanion extends StaticAnnotation
}
