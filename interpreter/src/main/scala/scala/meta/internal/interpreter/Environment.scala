package scala.meta.internal.interpreter

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.collection.immutable.ListMap
import scala.meta._
import scala.meta.semantic._
import scala.meta.internal.{ ast => m }
import scala.meta.internal.{ hygiene => h }
import scala.meta.dialects.Scala211

object Environment {
  type TName = m.Term.Name
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
      val onstack = stack.head.collectFirst{ // TODO handle stack frames
        case (k, v) if (k == nme) => v
        case (k, v) if k.denot == h.Denotation.Zero && nme.denot == h.Denotation.Zero && k.value == nme.value => v // HACK!
      }
      if (onstack.nonEmpty) {
        onstack.get
      } else if (nme.isPackage && !nme.members.isEmpty) {
        // fetch the object instance reflectively
        lookup(nme.packageObject.name.asInstanceOf[m.Term.Name])
      } else if (nme.isObject || nme.isPackageObject) { // then look globally for the object
        nme.tpe.toString match {
          case "XtensionSemanticTermModule" => // TODO: Desugar
            val clazz = Class.forName("scala.meta.Term$")
            Object(XtensionSemanticTermModule(clazz.getField("MODULE$").get(clazz).asInstanceOf[scala.meta.Term.type]), nme.tpe)
          case "XtensionSemanticPatModule" =>
            val clazz = Class.forName("scala.meta.Pat$")
            Object(XtensionSemanticPatModule(clazz.getField("MODULE$").get(clazz).asInstanceOf[scala.meta.Pat.type]), nme.tpe)
          case _ =>
            val clazzName = nme.defn.ffi.get.stripPrefix("jvmErasure(").stripSuffix(")")
            val clazz = Utils.jvmTypeToClass(clazzName)
            Object(clazz.getField("MODULE$").get(clazz), nme.tpe)
        }
      } else {
        // TODO then in the scope of current objects
        unreachable(debug(nme, nme.defn))
      }

    }
  }

  trait FunctionEnv {
    def functionEnv: Env
  }
}
