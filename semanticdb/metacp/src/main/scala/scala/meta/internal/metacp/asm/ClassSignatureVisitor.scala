package scala.meta.internal.metacp.asm

import scala.collection.JavaConverters._
import scala.meta.internal.metacp.asm.JavaTypeSignature._
import scala.meta.internal.metacp.asm.JavaTypeSignature.ReferenceTypeSignature
import scala.meta.internal.metacp.asm.JavaTypeSignature.ReferenceTypeSignature._
import scala.tools.asm.signature.SignatureVisitor
import scala.tools.asm.{Opcodes => o}

/** A more sane default SignatureVisitor that throws exceptions on unhandled cases. */
abstract class FailFastSignatureVisitor extends SignatureVisitor(o.ASM5) {
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
  override def visitEnd(): Unit = () // OK to ignore.
}

abstract class TypedSignatureVisitor[T] extends FailFastSignatureVisitor {
  def result(): T
}

class TypeArgumentVisitor extends TypedSignatureVisitor[TypeArgument] {
  var wildcard = Option.empty[WildcardIndicator]
  val referenceTypeSignature = new ReferenceTypeSignatureVisitor
  override def result(): TypeArgument =
    wildcard match {
      case Some(WildcardIndicator.Star) => WildcardTypeArgument
      case _ => ReferenceTypeArgument(wildcard, referenceTypeSignature.result().get)
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

class ReferenceTypeSignatureVisitor extends TypedSignatureVisitor[Option[ReferenceTypeSignature]] {
  private var arrayTypeSignatureVisitor: JavaTypeSignatureVisitor = _
  private var typeVariable: TypeVariableSignature = _
  private val simpleClassTypeSignatures = List.newBuilder[SimpleClassTypeSignatureBuilder]
  private var lastSimpleClassTypeSignatures: SimpleClassTypeSignatureBuilder = _
  def classTypeSignature(): ClassTypeSignature = result().get.asInstanceOf[ClassTypeSignature]
  override def result(): Option[ReferenceTypeSignature] = {
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

class MethodSignatureVisitor
    extends TypedSignatureVisitor[MethodSignature]
    with TypeParametersVisitor {
  val params = List.newBuilder[JavaTypeSignatureVisitor]
  val returnType = new JavaTypeSignatureVisitor(false)
  val throws = List.newBuilder[ReferenceTypeSignatureVisitor]

  override def result(): MethodSignature = {
    val tparams = super.typeParametersResult()
    MethodSignature(
      tparams,
      params.result().map(_.result()),
      returnType.result(),
      throws.result().map(_.result().get).map {
        case cts: ClassTypeSignature => ThrowsSignature.ClassType(cts)
        case tvs: TypeVariableSignature => ThrowsSignature.TypeVariable(tvs)
        case els => throw new IllegalArgumentException(s"Expected ThrowsSignature, obtained $els")
      }
    )
  }

  override def visitParameterType: SignatureVisitor = {
    val visitor = new JavaTypeSignatureVisitor(false)
    params += visitor
    visitor
  }

  override def visitReturnType: SignatureVisitor = {
    returnType
  }

  override def visitExceptionType: SignatureVisitor = {
    val visitor = new ReferenceTypeSignatureVisitor
    throws += visitor
    visitor
  }

}

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
    //    pprint.log("classbound")
    lastTypeParameterVisitor.visitClassBound()
  }

  override def visitInterfaceBound(): SignatureVisitor = {
    //    pprint.log("interfaceBound")
    lastTypeParameterVisitor.visitInterfaceBound()
  }
}

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
    superclassSignature
  }

  override def visitInterface(): SignatureVisitor = {
    val visitor = new ReferenceTypeSignatureVisitor
    superinterfaceSignatures += visitor
    visitor
  }

}
