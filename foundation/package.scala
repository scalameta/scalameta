package org

package object scalareflect {
  case object UnreachableException extends Exception("this code path should've been unreachable")
  def unreachable = throw UnreachableException
}