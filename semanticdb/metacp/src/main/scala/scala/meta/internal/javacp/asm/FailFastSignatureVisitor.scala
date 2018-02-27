package scala.meta.internal.javacp.asm

import scala.tools.asm.Opcodes
import scala.tools.asm.signature.SignatureVisitor

/** A more sane default SignatureVisitor that throws exceptions on unhandled cases. */
abstract class FailFastSignatureVisitor extends SignatureVisitor(Opcodes.ASM5) {
  override def visitFormalTypeParameter(name: String): Unit = ???
  override def visitClassBound: SignatureVisitor = ???
  override def visitInterfaceBound: SignatureVisitor = ???
  override def visitSuperclass: SignatureVisitor = ???
  override def visitInterface: SignatureVisitor = ???
  override def visitParameterType: SignatureVisitor = ???
  override def visitReturnType: SignatureVisitor = ???
  override def visitExceptionType: SignatureVisitor = ???
  override def visitBaseType(descriptor: Char): Unit = ???
  override def visitTypeVariable(name: String): Unit = ???
  override def visitArrayType: SignatureVisitor = ???
  override def visitClassType(name: String): Unit = ???
  override def visitInnerClassType(name: String): Unit = ???
  override def visitTypeArgument(): Unit = ???
  override def visitTypeArgument(wildcard: Char): SignatureVisitor = ???
}
