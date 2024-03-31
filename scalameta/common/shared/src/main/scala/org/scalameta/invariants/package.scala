package org.scalameta

import scala.language.experimental.macros
import scala.reflect.ClassTag

package object invariants {
  // This macro behaves like `Predef.require` with an additional twist
  // of taking apart `requirement` and generating out an informative exception message
  // if things does wrong.
  //
  // Things that end up on an error message:
  // 1) Values of local variables.
  // 2) Calls to `org.scalameta.debug` as explained in documentation to that method.
  def require(requirement: Boolean): Unit = macro Macros.require
  def require(requirement: Boolean, clue: String): Unit = macro Macros.requireWithClue

  implicit class XtensionRequireCast[T](private val x: T) extends AnyVal {
    // Equivalent to requiring that `x.getClass` is assignable from `U`.
    // Implemented as a macro, because there's no other way to delegate to another macro.
    def require[U: ClassTag]: U = macro Macros.requireCast[U]
  }

  // Provides pretty notation for implications of different kinds.
  // This is surprisingly helpful when writing certain complex `require` calls.
  implicit class XtensionImplication(private val left: Boolean) extends AnyVal {
    def ==>(right: Boolean) = !left || right
    def <==(right: Boolean) = right ==> left
    def <==>(right: Boolean) = (left ==> right) && (right ==> left)
  }
}
