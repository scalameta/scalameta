package scala.meta.internal.interpreter

import scala.collection.immutable.ListMap
import scala.meta._
import scala.meta.semantic._
import scala.meta.internal.{ ast => m }
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
      if (stack.head.contains(nme)) { // TODO handle stack frames
        stack.head(nme)
      } else if (nme.isPackage && !nme.members.isEmpty) {
        // fetch the object instance reflectively
        Object(toClass(nme).getField("MODULE$").get(toClass(nme)), nme.tpe)
      } else if (nme.isObject) { // then look globally for the object
        nme.tpe.toString match {
          case "XtensionSemanticTermModule" => // TODO: Desugar
            val clazz = Class.forName("scala.meta.Term$")
            Object(XtensionSemanticTermModule(clazz.getField("MODULE$").get(clazz).asInstanceOf[scala.meta.Term.type]), nme.tpe)
          case "XtensionSemanticPatModule" =>
            val clazz = Class.forName("scala.meta.Pat$")
            Object(XtensionSemanticPatModule(clazz.getField("MODULE$").get(clazz).asInstanceOf[scala.meta.Pat.type]), nme.tpe)
          case _ =>
            val clazz = toClass(nme)
            Object(clazz.getField("MODULE$").get(clazz), nme.tpe)
        }
      } else {
        ??? // TODO then in the scope of current objects
      }

    }
  }

  def toClass(nme: TName)(implicit c: Context): Class[_] = {
    val tpeString = nme.tpe.toString
    tpeString match { // TODO hack
      case "Int" => Class.forName("java.lang.Integer")
      case "Ref.type" => Class.forName("scala.meta.internal.ast.Ctor$Ref$")
      case "ApplyType.type" => Class.forName("scala.meta.internal.ast.Term$ApplyType$")
      case "Name.type" if nme.scratchpad.toString == "List(Denotation(meta.internal.ast.Term.type,Object(object Name,object Name)))" =>
        Class.forName("scala.meta.internal.ast.Term$Name$")
      case "Name.type" if nme.scratchpad.toString == "List(Denotation(meta.internal.ast.Type.type,Object(object Name,object Name)))" =>
        Class.forName("scala.meta.internal.ast.Type$Name$")
      case "Name.type" => Class.forName("scala.meta.internal.ast.Ctor$Ref$Name$")
      case "New.type" => Class.forName("scala.meta.internal.ast.Term$New$")
      case "Param.type" => Class.forName("scala.meta.internal.ast.Term$Param$")
      case "Anonymous.type" => Class.forName("scala.meta.internal.ast.Name$Anonymous$")
      case "None.type" => Class.forName("scala.None$")
      case "Some.type" => Class.forName("scala.Some$")
      case "Typed.type" => Class.forName("scala.meta.internal.ast.Pat$Typed$")
      case "Block.type" => Class.forName("scala.meta.internal.ast.Term$Block$")
      case "String.type" => Class.forName("scala.meta.internal.ast.Lit$String$")
      case "ApplyInfix.type" => Class.forName("scala.meta.internal.ast.Term$ApplyInfix$")
      case "Zero.type" if nme.scratchpad.toString == "List(Denotation(meta.internal.hygiene.Denotation.type,Object(object Zero,object Zero)))" => Class.forName("scala.meta.internal.hygiene.Denotation$Zero$")
      case "Zero.type" => Class.forName("scala.meta.internal.hygiene.Sigma$Zero$")
      case "Match.type" if nme.scratchpad.toString == "List(Denotation(meta.internal.ast.Term.type,Object(object Match,object Match)))" =>
        Class.forName("scala.meta.internal.ast.Term$Match$")
      case "Object.type" if nme.scratchpad.toString == "List(Denotation(meta.internal.ast.Defn.type,Object(object Object,object Object)))" =>
        Class.forName("scala.meta.internal.ast.Defn$Object$")
      case "Implicit.type" if nme.scratchpad.toString == "List(Denotation(meta.internal.ast.Mod.type,Object(object Implicit,object Implicit)))" =>
        Class.forName("scala.meta.internal.ast.Mod$Implicit$")
      case "Primary.type" if nme.scratchpad.toString == "List(Denotation(meta.internal.ast.Ctor.type,Object(object Primary,object Primary)))" =>
        Class.forName("scala.meta.internal.ast.Ctor$Primary$")
      case "Def.type" if nme.scratchpad.toString == "List(Denotation(meta.internal.ast.Defn.type,Object(object Def,object Def)))" =>
        Class.forName("scala.meta.internal.ast.Defn$Def$")
      case "Apply.type" =>
        Class.forName("scala.meta.internal.ast.Term$Apply$")
      case "Select.type" =>
        Class.forName("scala.meta.internal.ast.Term$Select$")
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
