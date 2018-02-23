package scala.meta.internal.metacp.asm

import scala.meta.internal.metacp.asm.JavaTypeSignature.SimpleClassTypeSignature
import scala.meta.internal.metacp.asm.JavaTypeSignature.TypeArguments
import scala.tools.asm.signature.SignatureVisitor

class SimpleClassTypeSignatureBuilder(identifier: String) {
  private val typeArguments = List.newBuilder[TypeArgumentVisitor]
  def result(): SimpleClassTypeSignature =
    typeArguments.result() match {
      case Nil => SimpleClassTypeSignature(identifier, None)
      case head :: tail =>
        SimpleClassTypeSignature(
          identifier,
          Some(TypeArguments(head.result(), tail.map(_.result()))))
    }

  def visitTypeArgument(): Unit = {
    val typeArgumentVisitor = new TypeArgumentVisitor
    typeArguments += typeArgumentVisitor
    typeArgumentVisitor.visitTypeArgument()
  }

  def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    val typeArgumentVisitor = new TypeArgumentVisitor
    typeArguments += typeArgumentVisitor
    typeArgumentVisitor.visitTypeArgument(wildcard)
  }
}
