package scala.meta.internal.javacp.asm

import scala.meta.internal.javacp._
import scala.tools.asm.signature.SignatureVisitor

trait TypeParametersVisitor { this: SignatureVisitor =>
  private var typeParameters = List.newBuilder[TypeParameterVisitor]
  private var lastTypeParameterVisitor: TypeParameterVisitor = _

  def typeParametersResult(): Option[TypeParameters] = typeParameters.result() match {
    case Nil => None
    case head :: tail =>
      Some(TypeParameters(head.result(), tail.map(_.result())))
  }
  override def visitFormalTypeParameter(name: String): Unit = {
    val visitor = new TypeParameterVisitor(name)
    typeParameters += visitor
    lastTypeParameterVisitor = visitor
  }

  override def visitClassBound(): SignatureVisitor = {
    lastTypeParameterVisitor.visitClassBound()
  }

  override def visitInterfaceBound(): SignatureVisitor = {
    lastTypeParameterVisitor.visitInterfaceBound()
  }
}
