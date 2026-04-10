package scala.meta.internal.inputs

import scala.collection.concurrent.TrieMap

object Compat {
  def newMutableMap[A, B] = TrieMap.empty[A, B]
}
