package scala.meta
package syntactic

import org.scalameta.data._
import org.scalameta.unreachable

@data class ParseException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") with ScalametaException {
  override def toString = super.toString
}
