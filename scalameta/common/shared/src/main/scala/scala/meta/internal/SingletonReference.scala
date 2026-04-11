package scala.meta.internal

import scala.util.DynamicVariable

class SingletonReference[A](init: A) {
  import SingletonReference.Ref

  private val global = new Ref(init)
  private val local: DynamicVariable[Ref[A]] = new DynamicVariable(global)

  def value: A = local.value.ref
  def value_=(value: A): Unit = global.ref = value

  def withValue[T](obj: A)(body: => T): T = local.withValue(new Ref(obj))(body)
}

object SingletonReference {
  private class Ref[A](
      @volatile
      var ref: A
  )
}
