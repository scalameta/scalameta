package scala.meta
package internal
package ast

import org.scalameta.data._
import org.scalameta.unreachable

@data class MergeException(culprits: Seq[Tree], message: String, cause: Option[Throwable] = None)
extends Exception(message, cause.orNull) with ScalametaException {
  override def toString = super.toString
}
