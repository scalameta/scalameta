package org.scalameta

package object reflection {
  def instanceOf[T: InstanceTag] = implicitly[InstanceTag[T]].instantiate
}
