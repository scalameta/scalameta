package scala.meta
package dialects

import org.scalameta.data._
import org.scalameta.unreachable

@data class DialectException(name: String, message: String)
extends Exception(message) with ScalametaException {
  override def toString = super.toString
}
