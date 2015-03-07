package scala.meta.internal.interpreter

import scala.collection.immutable.ListMap
import scala.meta._
import scala.meta.semantic._
import scala.meta.internal.{ ast => i }

object environment {
  type TName = i.Term.Name
  type Heap = ListMap[TName, Object]
  type FrameStack = List[ListMap[TName, Object]]

  /**
   * Every object is a reference to the heap + additional fields.
   * NOTE: we do not support primitive values for now.
   */
  final case class Object(ref: Any, tpe: Type, fields: Map[TName, Object] = Map()) {
    def as[T]: T = ref.asInstanceOf[T]
  }

  case class Env(stack: FrameStack, heap: Heap) {
    def push(nme: TName, value: Object): Env =
      copy(stack = (stack.head + ((nme -> value))) :: stack.tail)

    def lookup(nme: TName)(implicit c: Context): Object = {
      // first look on the stack
      if (stack.head.contains(nme)) {
        stack.head(nme)
      } else if (nme.isObject) { // then look globally for the object
        val tmp = nme.tpe.scratchpad.head.toString.drop(9)
        val qualifiedName = tmp.take(tmp.length - 6)
        // fetch the object instance reflectively
        Object(toClass(nme.tpe).getField("MODULE$").get(toClass(nme.tpe)), nme.tpe)
      } else {
        ??? // TODO then in the scope of current objects
      }

    }
  }

  def toClass(tpe: Type)(implicit c: Context): Class[_] = {
    tpe.toString match { // TODO hack
      case "Int" => Class.forName("java.lang.Integer")
      case _ =>
        val tmp = tpe.scratchpad.head.toString.drop(9)
        val qualifiedName = tmp.take(tmp.length - 6)
        // ASK Eugene
        if (tpe.toString endsWith ".type") {
          Class.forName(qualifiedName + "$")
        } else {
          Class.forName(qualifiedName)
        }

    }
  }
}
