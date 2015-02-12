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
  final case class Object(ref: AnyRef, fields: Map[Name, Object])

  case class Env(stack: FrameStack, heap: Heap)
}

