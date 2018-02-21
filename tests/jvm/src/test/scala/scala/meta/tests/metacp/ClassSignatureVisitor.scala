package scala.meta.tests.metacp

import java.util.NoSuchElementException
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.meta.internal.metacp.JavaTypeSignature.ClassBound
import scala.meta.internal.metacp.JavaTypeSignature.ClassSignature
import scala.meta.internal.metacp.JavaTypeSignature.InterfaceBound
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.ClassTypeSignature
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.ReferenceTypeArgument
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.SimpleClassTypeSignature
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.TypeArgument
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.TypeArguments
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.TypeVariableSignature
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.WildcardIndicator
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.WildcardTypeArgument
import scala.meta.internal.metacp.JavaTypeSignature.SuperclassSignature
import scala.meta.internal.metacp.JavaTypeSignature.SuperinterfaceSignature
import scala.meta.internal.metacp.JavaTypeSignature.TypeParameter
import scala.meta.internal.metacp.JavaTypeSignature.TypeParameters
//import scala.meta.internal.metacp.Javacp.SignatureMode
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.{semanticdb3 => s}
import scala.tools.asm.signature.SignatureVisitor
import scala.tools.asm.{Opcodes => o}
//import scala.meta.internal.metacp.Javacp.SignatureMode._
import scala.meta.internal.metacp.Javacp._

class TypeArgumentVisitor extends SignatureVisitor(o.ASM5) {
  var wildcard = Option.empty[WildcardIndicator]
  val referenceTypeSignature = new ReferenceTypeSignatureVisitor
  def result(): TypeArgument = wildcard match {
    case Some(WildcardIndicator.Star) => WildcardTypeArgument
    case _ => ReferenceTypeArgument(wildcard, referenceTypeSignature.result())
  }
  // TODO(olafur) handle WildcardTypeARgument

  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
//    pprint.log(wildcard)
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

class ReferenceTypeSignatureVisitor extends SignatureVisitor(o.ASM5) {
  private var referenceTypeSignature = Option.empty[ReferenceTypeSignature]
  private val typeArguments = List.newBuilder[TypeArgumentVisitor]
  def classTypeSignature(): ClassTypeSignature = result().asInstanceOf[ClassTypeSignature]
  def result(): ReferenceTypeSignature = resultOption().get
  def resultOption(): Option[ReferenceTypeSignature] = {
    referenceTypeSignature.map {
      case c: ClassTypeSignature =>
        val targs = typeArguments.result()
        targs match {
          case Nil => c
          case head :: tail =>
            val rest = tail.map(_.result())
//            pprint.log(rest)
            ClassTypeSignature(
              None,
              SimpleClassTypeSignature(
                c.simpleClassTypeSignature.identifier,
                Some(TypeArguments(head.result(), rest))
              )
            )
        }
      case t => t
    }
  }

  override def visitClassType(name: String): Unit = {
//    pprint.log(name)
    referenceTypeSignature = Some(
      ClassTypeSignature(
        None,
        SimpleClassTypeSignature(name, None)
      )
    )
  }

  override def visitTypeArgument(): Unit = {
    val typeArgumentVisitor = new TypeArgumentVisitor
    typeArguments += typeArgumentVisitor
    typeArgumentVisitor.visitTypeArgument()
  }

  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    val typeArgumentVisitor = new TypeArgumentVisitor
    typeArguments += typeArgumentVisitor
    typeArgumentVisitor.visitTypeArgument(wildcard)
  }

  override def visitTypeVariable(name: String): Unit = {
//    pprint.log(name)
    referenceTypeSignature = Some(TypeVariableSignature(name))
  }

}

class TypeParameterVisitor(identifier: String) extends SignatureVisitor(o.ASM5) {
  val classBound = new ReferenceTypeSignatureVisitor
  val interfaceBounds = List.newBuilder[ReferenceTypeSignatureVisitor]
  def result(): TypeParameter = {
    TypeParameter(
      identifier,
      ClassBound(classBound.resultOption()),
      interfaceBounds.result().map { ib =>
        InterfaceBound(ib.result())
      }
    )
  }

  override def visitClassBound(): SignatureVisitor = {
    classBound
  }

  override def visitInterfaceBound(): SignatureVisitor = {
    val visitor = new ReferenceTypeSignatureVisitor
    interfaceBounds += visitor
    visitor
  }

  override def visitTypeArgument(): Unit = {
    ???
  }

  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    ???
  }

}

class ClassSignatureVisitor extends SignatureVisitor(o.ASM5) {
  var typeParameters = List.newBuilder[TypeParameterVisitor]
  var lastTypeParameterVisitor: TypeParameterVisitor = _
  var superclassSignature = new ReferenceTypeSignatureVisitor
  val superinterfaceSignatures = List.newBuilder[ReferenceTypeSignatureVisitor]

  def result(): ClassSignature = {
    val tparams = typeParameters.result() match {
      case Nil => None
      case head :: tail =>
        Some(TypeParameters(head.result(), tail.map(_.result())))
    }
    val interfaces = superinterfaceSignatures.result().map(_.classTypeSignature())
    ClassSignature(tparams, superclassSignature.classTypeSignature(), interfaces)
  }

//  var mode: SignatureMode = Start

  override def visitFormalTypeParameter(name: String): Unit = {
//    pprint.log(name)
    val visitor = new TypeParameterVisitor(name)
    typeParameters += visitor
    lastTypeParameterVisitor = visitor
//    mode = FormalType
  }

  override def visitClassBound(): SignatureVisitor = {
    //    pprint.log("classbound")
    lastTypeParameterVisitor.visitClassBound()
  }

  override def visitInterfaceBound(): SignatureVisitor = {
//    pprint.log("interfaceBound")
    lastTypeParameterVisitor.visitInterfaceBound()
  }

  override def visitSuperclass(): SignatureVisitor = {
//    pprint.log("superclass")
    superclassSignature
  }

  override def visitInterface(): SignatureVisitor = {
//    pprint.log("interface")
    val visitor = new ReferenceTypeSignatureVisitor
    superinterfaceSignatures += visitor
    visitor
  }

  override def visitClassType(name: String): Unit = {
//    pprint.log(name)
    ???
  }

  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    ???
  }

  override def visitTypeVariable(name: String): Unit = {
    ???
  }

  override def visitTypeArgument(): Unit = {
    ???
  }

  override def visitEnd(): Unit = {
    ???
  }

}
