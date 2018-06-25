package scala.meta.internal.metacp

import scala.tools.asm.Attribute
import scala.tools.asm.ClassVisitor
import scala.tools.asm.Opcodes

final class HasScalaSigVisitor extends ClassVisitor(Opcodes.ASM5) {
  var hasScalaSig = false
  override def visitAttribute(attr: Attribute): Unit = {
    attr.`type` match {
      case "ScalaSig" => hasScalaSig = true
      case _ =>
    }
  }
}
