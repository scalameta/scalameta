package scala.meta.internal.javacp.asm

import scala.meta.internal.javacp._
import scala.tools.asm.signature.SignatureVisitor

class TypeArgumentVisitor extends TypedSignatureVisitor[TypeArgument] {
  var wildcard = Option.empty[WildcardIndicator]
  val referenceTypeSignature = new ReferenceTypeSignatureVisitor
  override def result(): TypeArgument =
    wildcard match {
      case Some(WildcardIndicator.Star) => WildcardTypeArgument
      case _ => ReferenceTypeArgument(wildcard, referenceTypeSignature.result().get)
    }

  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    this.wildcard = wildcard match {
      case '+' => Some(WildcardIndicator.Plus)
      case '-' => Some(WildcardIndicator.Minus)
      case _ => None
    }
    referenceTypeSignature
  }
  override def visitTypeArgument(): Unit = {
    this.wildcard = Some(WildcardIndicator.Star)
  }
}
