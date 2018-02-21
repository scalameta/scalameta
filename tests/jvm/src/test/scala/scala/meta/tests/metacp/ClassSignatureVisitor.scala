package scala.meta.tests.metacp

import java.util.NoSuchElementException
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.meta.internal.metacp.JavaTypeSignature
import scala.meta.internal.metacp.JavaTypeSignature.BaseType
import scala.meta.internal.metacp.JavaTypeSignature.ClassBound
import scala.meta.internal.metacp.JavaTypeSignature.ClassSignature
import scala.meta.internal.metacp.JavaTypeSignature.InterfaceBound
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.ArrayTypeSignature
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.ClassTypeSignature
import scala.meta.internal.metacp.JavaTypeSignature.ReferenceTypeSignature.ClassTypeSignatureSuffix
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
import scala.meta.internal.metacp.JavaTypeSignature.VoidDescriptor
//import scala.meta.internal.metacp.Javacp.SignatureMode
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.{semanticdb3 => s}
import scala.tools.asm.signature.SignatureVisitor
import scala.tools.asm.{Opcodes => o}
//import scala.meta.internal.metacp.Javacp.SignatureMode._
import scala.meta.internal.metacp.Javacp._

// Helper to catch unhandled cases. All default implementation do nothing
// which makes it difficult to detect if for example an inner class is not handled.
trait FailingSignatureVisitor { _: SignatureVisitor =>

  override def visitFormalTypeParameter(name: String): Unit = ???

  /**
    * Visits the class bound of the last visited formal type parameter.
    *
    * @return a non null visitor to visit the signature of the class bound.
    */
  override def visitClassBound: SignatureVisitor = ???

  /**
    * Visits an interface bound of the last visited formal type parameter.
    *
    * @return a non null visitor to visit the signature of the interface bound.
    */
  override def visitInterfaceBound: SignatureVisitor = ???

  /**
    * Visits the type of the super class.
    *
    * @return a non null visitor to visit the signature of the super class
    *         type.
    */
  override def visitSuperclass: SignatureVisitor = ???

  /**
    * Visits the type of an interface implemented by the class.
    *
    * @return a non null visitor to visit the signature of the interface type.
    */
  override def visitInterface: SignatureVisitor = ???

  /**
    * Visits the type of a method parameter.
    *
    * @return a non null visitor to visit the signature of the parameter type.
    */
  override def visitParameterType: SignatureVisitor = ???

  /**
    * Visits the return type of the method.
    *
    * @return a non null visitor to visit the signature of the return type.
    */
  override def visitReturnType: SignatureVisitor = ???

  /**
    * Visits the type of a method exception.
    *
    * @return a non null visitor to visit the signature of the exception type.
    */
  override def visitExceptionType: SignatureVisitor = ???

  /**
    * Visits a signature corresponding to a primitive type.
    *
    * @param descriptor
    * the descriptor of the primitive type, or 'V' for <tt>void</tt>
    * .
    */
  override def visitBaseType(descriptor: Char): Unit = ???

  /**
    * Visits a signature corresponding to a type variable.
    *
    * @param name
    * the name of the type variable.
    */
  override def visitTypeVariable(name: String): Unit = ???

  /**
    * Visits a signature corresponding to an array type.
    *
    * @return a non null visitor to visit the signature of the array element
    *         type.
    */
  override def visitArrayType: SignatureVisitor = ???

  /**
    * Starts the visit of a signature corresponding to a class or interface
    * type.
    *
    * @param name
    * the internal name of the class or interface.
    */
  override def visitClassType(name: String): Unit = ???

  /**
    * Visits an inner class.
    *
    * @param name
    * the local name of the inner class in its enclosing class.
    */
  override def visitInnerClassType(name: String): Unit = ???

  /**
    * Visits an unbounded type argument of the last visited class or inner
    * class type.
    */
  override def visitTypeArgument(): Unit = ???

  /**
    * Visits a type argument of the last visited class or inner class type.
    *
    * @param wildcard
    * '+', '-' or '='.
    * @return a non null visitor to visit the signature of the type argument.
    */
  override def visitTypeArgument(wildcard: Char): SignatureVisitor = ???

  /**
    * Ends the visit of a signature corresponding to a class or interface type.
    */
  override def visitEnd(): Unit = ()
}

class TypeArgumentVisitor extends SignatureVisitor(o.ASM5) with FailingSignatureVisitor {
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

class JavaTypeSignatureVisitor(isArray: Boolean)
    extends SignatureVisitor(o.ASM5)
    with FailingSignatureVisitor {
  private var baseType: BaseType = _
  private var referenceTypeSignature: ReferenceTypeSignatureVisitor = _

  def result(): JavaTypeSignature = {
    val obtained =
      if (baseType == null) referenceTypeSignature.result()
      else baseType
    if (isArray) ArrayTypeSignature(obtained)
    else obtained
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

class ReferenceTypeSignatureVisitor extends SignatureVisitor(o.ASM5) with FailingSignatureVisitor {
  private var arrayTypeSignatureVisitor: JavaTypeSignatureVisitor = _
  private var typeVariable: TypeVariableSignature = _
  private val simpleClassTypeSignatures = List.newBuilder[SimpleClassTypeSignatureBuilder]
  private var lastSimpleClassTypeSignatures: SimpleClassTypeSignatureBuilder = _
  def classTypeSignature(): ClassTypeSignature = result().asInstanceOf[ClassTypeSignature]
  def result(): ReferenceTypeSignature = resultOption().get
  def resultOption(): Option[ReferenceTypeSignature] = {
    if (arrayTypeSignatureVisitor != null) {
      arrayTypeSignatureVisitor.result() match {
        case r: ReferenceTypeSignature => Some(r)
        case baseType =>
          throw new IllegalArgumentException(s"Expected Reference Type, obtained $baseType")
      }
    } else if (typeVariable != null) {
      Some(typeVariable)
    } else {
      simpleClassTypeSignatures.result() match {
        case Nil => None
        case simpleClass :: suffix =>
          Some(
            ClassTypeSignature(
              None,
              simpleClass.result(),
              suffix.map(s => ClassTypeSignatureSuffix(s.result()))
            )
          )
      }
    }
  }

  override def visitArrayType(): SignatureVisitor = {
    val visitor = new JavaTypeSignatureVisitor(isArray = true)
    arrayTypeSignatureVisitor = visitor
    visitor
  }

  def startSimpleClass(name: String): Unit = {
    val builder = new SimpleClassTypeSignatureBuilder(name)
    simpleClassTypeSignatures += builder
    lastSimpleClassTypeSignatures = builder
  }

  override def visitClassType(name: String): Unit = {
//    pprint.log(name)
    name match {
      // ASM does not strip off the L for java.lang.Object for some reason.
      case "Ljava/lang/Object" => startSimpleClass("java/lang/Object")
      case _ => startSimpleClass(name)
    }
  }

  override def visitInnerClassType(name: String): Unit = {
    startSimpleClass(name)
  }

  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    lastSimpleClassTypeSignatures.visitTypeArgument(wildcard)
  }

  override def visitTypeArgument(): Unit = {
    lastSimpleClassTypeSignatures.visitTypeArgument()
  }

  override def visitTypeVariable(name: String): Unit = {
    typeVariable = TypeVariableSignature(name)
  }

}

class TypeParameterVisitor(identifier: String)
    extends SignatureVisitor(o.ASM5)
    with FailingSignatureVisitor {
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

}

class ClassSignatureVisitor extends SignatureVisitor(o.ASM5) with FailingSignatureVisitor {
  private var typeParameters = List.newBuilder[TypeParameterVisitor]
  private var lastTypeParameterVisitor: TypeParameterVisitor = _
  private var superclassSignature = new ReferenceTypeSignatureVisitor
  private val superinterfaceSignatures = List.newBuilder[ReferenceTypeSignatureVisitor]

  def result(): ClassSignature = {
    val tparams = typeParameters.result() match {
      case Nil => None
      case head :: tail =>
        Some(TypeParameters(head.result(), tail.map(_.result())))
    }
    val superclass = superclassSignature.classTypeSignature()
    val interfaces = superinterfaceSignatures.result().map(_.classTypeSignature())
//    pprint.log(superclass)
//    pprint.log(interfaces)
    ClassSignature(tparams, superclass, interfaces)
  }

//  var mode: SignatureMode = Start

  override def visitFormalTypeParameter(name: String): Unit = {
    val visitor = new TypeParameterVisitor(name)
    typeParameters += visitor
    lastTypeParameterVisitor = visitor
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

  override def visitEnd(): Unit = {
//    pprint.log("END")
  }

}
