package org

package object scalameta {
  case object UnreachableError extends Error("this code path should've been unreachable")
  def unreachable = throw UnreachableError
}