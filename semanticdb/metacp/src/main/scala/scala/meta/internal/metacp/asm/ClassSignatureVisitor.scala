package scala.meta.internal.metacp.asm

import scala.collection.JavaConverters._
import scala.meta.internal.metacp.asm.JavaTypeSignature._
import scala.tools.asm.signature.SignatureVisitor

class ClassSignatureVisitor
    extends TypedSignatureVisitor[ClassSignature]
    with TypeParametersVisitor {
  private val superclassSignature = new ReferenceTypeSignatureVisitor
  private val superinterfaceSignatures = List.newBuilder[ReferenceTypeSignatureVisitor]

  override def result(): ClassSignature = {
    val tparams = super.typeParametersResult()
    val superclass = superclassSignature.classTypeSignature()
    val interfaces = superinterfaceSignatures.result().map(_.classTypeSignature())
    ClassSignature(tparams, superclass, interfaces)
  }

  override def visitSuperclass(): SignatureVisitor = {
    superclassSignature.visitSuperclass()
  }

  override def visitInterface(): SignatureVisitor = {
    val visitor = new ReferenceTypeSignatureVisitor
    superinterfaceSignatures += visitor
    visitor
  }

}
