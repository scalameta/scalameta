package scala.meta.internal.inputs

import scala.collection.mutable

object Compat {
  def newMutableMap[A, B] = mutable.Map.empty[A, B]
}
