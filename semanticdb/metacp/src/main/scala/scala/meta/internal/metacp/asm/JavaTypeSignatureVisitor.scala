package scala.meta.internal.metacp.asm

import scala.meta.internal.metacp.asm.JavaTypeSignature.BaseType
import scala.meta.internal.metacp.asm.JavaTypeSignature.ArrayTypeSignature
import scala.tools.asm.signature.SignatureVisitor

class JavaTypeSignatureVisitor(isArray: Boolean) extends TypedSignatureVisitor[JavaTypeSignature] {
  private var baseType: BaseType = _
  private val referenceTypeSignature: ReferenceTypeSignatureVisitor =
    new ReferenceTypeSignatureVisitor

  override def result(): JavaTypeSignature = {
    val obtained =
      if (baseType == null) referenceTypeSignature.result().get
      else baseType
    if (isArray) ArrayTypeSignature(obtained)
    else obtained
  }

  override def visitSuperclass: SignatureVisitor =
    // visitSuperclass can be called for field signatures and followed with
    // visitBaseType, which is not a reference.
    this

  override def visitArrayType: SignatureVisitor = {
    referenceTypeSignature.visitArrayType()
  }

  override def visitInnerClassType(name: String): Unit = {
    referenceTypeSignature.visitInnerClassType(name)
  }

  override def visitTypeVariable(name: String): Unit = {
    referenceTypeSignature.visitTypeVariable(name)
  }

  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    referenceTypeSignature.visitTypeArgument(wildcard)
  }

  override def visitTypeArgument(): Unit = {
    referenceTypeSignature.visitTypeArgument()
  }

  override def visitClassType(name: String): Unit = {
    referenceTypeSignature.visitClassType(name)
  }

  override def visitBaseType(descriptor: Char): Unit = {
    import BaseType._
    baseType = descriptor match {
      case 'V' => V
      case 'B' => B
      case 'J' => J
      case 'Z' => Z
      case 'I' => I
      case 'S' => S
      case 'C' => C
      case 'F' => F
      case 'D' => D
      case _ => throw new IllegalArgumentException(s"Invalid base descriptor '$descriptor'")
    }
  }

}
