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
      } else if (nme.isPackage && !nme.members.isEmpty) {
        // fetch the object instance reflectively
        Object(toClass(nme).getField("MODULE$").get(toClass(nme)), nme.tpe)
      } else if (nme.isObject) { // then look globally for the object
        // fetch the object instance reflectively
        Object(toClass(nme).getField("MODULE$").get(toClass(nme)), nme.tpe)
      } else {
        ??? // TODO then in the scope of current objects
      }

    }
  }

  def toClass(nme: TName)(implicit c: Context): Class[_] = {
    val tpeString = nme.tpe.toString
    println(tpeString)
    tpeString match { // TODO hack
      case "Int" => Class.forName("java.lang.Integer")
      case "Ref.type" =>
        Class.forName("scala.meta.internal.ast.Ctor$Ref$")
      case "ApplyType.type" =>
        Class.forName("scala.meta.internal.ast.Term$ApplyType$")
      case "Name.type" =>
        Class.forName("scala.meta.internal.ast.Ctor$Ref$Name$")
      case "New.type" =>
        Class.forName("scala.meta.internal.ast.Term$New$")
      case "Param.type" =>
        Class.forName("scala.meta.internal.ast.Term$Param$")
      case "Anonymous.type" =>
        Class.forName("scala.meta.internal.ast.Name$Anonymous$")
      case "None.type" =>
        Class.forName("scala.None$")
      case "Some.type" =>
        Class.forName("scala.Some$")
      case "`package`.type" =>
        val qualifiedName = nme.toString + "." + tpeString.take(tpeString.length - ".type".length).replaceAll("`", "")
        // TODO owners are not working
        Class.forName("scala." + qualifiedName + "$")
      case _ =>
        val tmp = nme.tpe.scratchpad.head.toString.drop(9)
        val qualifiedName = tmp.take(tmp.length - 6)
        // ASK Eugene
        if (tpeString endsWith ".type") {
          Class.forName(qualifiedName + "$")
        } else {
          Class.forName(qualifiedName)
        }

    }
  }

  trait FunctionEnv {
    def functionEnv: Env
  }
}
