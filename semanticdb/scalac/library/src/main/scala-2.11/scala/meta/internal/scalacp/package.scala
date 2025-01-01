package scala.meta.internal

import scala.collection.mutable

package object scalacp {

  implicit class ImplicitMap[K, V](private val obj: mutable.Map[K, V]) extends AnyVal {
    def updateWithRemap(key: K)(remappingFunction: Option[V] => V): V = {
      val vOldOpt = obj.get(key)
      val vNew = remappingFunction(vOldOpt)
      if (!vOldOpt.contains(vNew)) obj.update(key, vNew)
      vNew
    }
  }

}
