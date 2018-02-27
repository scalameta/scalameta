package scala.meta.internal.javacp.asm

import scala.meta.internal.javacp._
import scala.tools.asm.signature.SignatureVisitor

class TypeParameterVisitor(identifier: String) extends TypedSignatureVisitor[TypeParameter] {
  val classBound = new ReferenceTypeSignatureVisitor
  val interfaceBounds = List.newBuilder[ReferenceTypeSignatureVisitor]
  override def result(): TypeParameter =
    TypeParameter(
      identifier,
      ClassBound(classBound.result()),
      interfaceBounds.result().map { ib =>
        InterfaceBound(ib.result().get)
      }
    )

  override def visitClassBound(): SignatureVisitor = {
    classBound
  }

  override def visitInterfaceBound(): SignatureVisitor = {
    val visitor = new ReferenceTypeSignatureVisitor
    interfaceBounds += visitor
    visitor
  }
}
