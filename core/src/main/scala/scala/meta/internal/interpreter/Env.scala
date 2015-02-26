package scala.meta.internal.interpreter

import scala.collection.immutable.ListMap

import scala.meta._

object environment {
  type Heap = ListMap[Name, Object]
  type FrameStack = List[ListMap[Name, Object]]

  /**
   * Every object is a reference to the heap + additional fields.
   * NOTE: we do not support primitive values for now.
   */
  final case class Object(ref: Any, fields: Map[Name, Object] = Map())

  case class Env(stack: FrameStack, heap: Heap) {
    def push(nme: Name, value: Object): Env =
      copy(stack = (stack.head + ((nme -> value))) :: stack.tail)

    def lookup(nme: Name): Object = {
      // first look on the stack
      if (stack.head.contains(nme)) {
        stack.head(nme)
      } // TODO then in the objects
      else { // then look globally for the object
        ???
      }

    }
  }
}
