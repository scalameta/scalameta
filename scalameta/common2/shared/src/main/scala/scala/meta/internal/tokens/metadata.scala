package scala.meta
package internal
package tokens

import org.scalameta.adt.Metadata.Adt

import scala.annotation.{StaticAnnotation, nowarn}

object Metadata {
  trait Token extends Adt
  class root extends StaticAnnotation
  class branch extends StaticAnnotation
  @nowarn("cat=unused-params") // macros read the arguments from the tree
  class tokenClass(name: String, freeform: Boolean) extends StaticAnnotation
  class tokenCompanion extends StaticAnnotation
}
