package scala.meta
package internal
package inputs

import scala.meta.inputs._
import org.scalameta.data._

@data class VirtualInput(s: scala.Predef.String) extends Input {
  lazy val chars = s.toArray
  override def toString = "VirtualInput(\"" + s + "\")"
}
