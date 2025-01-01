package scala.meta.internal

import scala.collection.mutable

package object scalacp {

  implicit class ImplicitMap[K, V](private val obj: mutable.Map[K, V]) extends AnyVal {
    def updateWithRemap(key: K)(remappingFunction: Option[V] => V): V = obj
      .updateWith(key)(vOpt => Some(remappingFunction(vOpt))).get
  }

}
