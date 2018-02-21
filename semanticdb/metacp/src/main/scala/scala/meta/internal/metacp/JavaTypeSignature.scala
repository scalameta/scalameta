package scala.meta.internal.metacp

// Translation of JVMS to Scala
// https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.9.1

trait Pretty {
  def print(sb: StringBuilder): Unit
  final def pretty: String = {
    val sb = new StringBuilder
    this.print(sb)
    sb.toString
  }
}
sealed trait JavaTypeSignature extends JavaTypeSignature.Result with Pretty
object JavaTypeSignature {

  abstract class BaseType(name: String) extends JavaTypeSignature with Product {
    final override def print(sb: StringBuilder): Unit =
      sb.append(this.productPrefix)
  }
  object BaseType {
    case object B extends BaseType("Byte")
    case object C extends BaseType("Char")
    case object D extends BaseType("Double")
    case object F extends BaseType("Float")
    case object I extends BaseType("Int")
    case object J extends BaseType("Long")
    case object S extends BaseType("Short")
    case object Z extends BaseType("Boolean")
  }

  sealed trait ReferenceTypeSignature extends JavaTypeSignature with FieldSignature
  object ReferenceTypeSignature {
    case class ClassTypeSignature(
        packageSpecifier: Option[PackageSpecifier],
        simpleClassTypeSignature: SimpleClassTypeSignature)
        extends ReferenceTypeSignature
        with SuperclassSignature
        with SuperinterfaceSignature
        with ThrowsSignature {
      override def print(sb: StringBuilder): Unit = {
        sb.append('L')
        simpleClassTypeSignature.print(sb)
        sb.append(';')
      }
    }
    case class TypeVariableSignature(identifier: String)
        extends ReferenceTypeSignature
        with SuperclassSignature
        with ThrowsSignature {
      override def print(sb: StringBuilder): Unit = {
        sb.append('T')
        sb.append(identifier)
        sb.append(';')
      }
    }
    case class ArrayTypeSignature(javaTypeSignature: JavaTypeSignature)
        extends ReferenceTypeSignature {
      override def print(sb: StringBuilder): Unit = {
        sb.append("[")
        javaTypeSignature.print(sb)
      }
    }
    // JVMS deviation. We don't use PackageSpecifier, the a/b/c path is encoded in the identifier.
    case class PackageSpecifier(identifier: String, specifier: List[PackageSpecifier])
    case class SimpleClassTypeSignature(identifier: String, typeArguments: Option[TypeArguments])
        extends Pretty {
      def print(sb: StringBuilder): Unit = {
        sb.append(identifier)
        typeArguments match {
          case Some(t: TypeArguments) =>
            sb.append('<')
            t.print(sb)
            sb.append('>')
          case None =>
        }
      }
    }

    case class TypeArguments(argument: TypeArgument, tail: List[TypeArgument]) extends Pretty {
      override def print(sb: StringBuilder): Unit = {
        argument.print(sb)
        tail.foreach(_.print(sb))
      }
    }
    trait TypeArgument extends Pretty
    case object WildcardTypeArgument extends TypeArgument {
      override def print(sb: StringBuilder): Unit = sb.append('*')
    }
    case class ReferenceTypeArgument(
        wildcard: Option[WildcardIndicator],
        referenceTypeSignature: ReferenceTypeSignature)
        extends TypeArgument {
      override def print(sb: StringBuilder): Unit = {
        wildcard match {
          case Some(w: WildcardIndicator) => w.print(sb)
          case _ =>
        }
        referenceTypeSignature.print(sb)
      }
    }
    sealed class WildcardIndicator(wildcard: Char) extends Pretty {
      override def print(sb: StringBuilder): Unit = sb.append(wildcard)
    }
    object WildcardIndicator {
      case object Plus extends WildcardIndicator('+')
      case object Minus extends WildcardIndicator('-')
      // NOTE(olafur) Star is strictly not a WildcardIndicator, however having
      // star here makes it easier to implement TypeArgumentVisitor.
      case object Star extends WildcardIndicator('*')
    }
    case class ClassTypeSignatureSuffix(simpleClassTypeSignature: SimpleClassTypeSignature)
  }

  import ReferenceTypeSignature._

  // class signature
  case class ClassSignature(
      typeParameters: Option[TypeParameters],
      superclassSignature: SuperclassSignature,
      superinterfaceSignature: List[SuperinterfaceSignature])
      extends Pretty {
    override def print(sb: StringBuilder): Unit = {
      typeParameters match {
        case Some(tp: TypeParameters) =>
          sb.append('<')
          tp.print(sb)
          sb.append('>')
        case _ =>
      }
      superclassSignature.print(sb)
      superinterfaceSignature.foreach(_.print(sb))
    }
  }
  case class TypeParameters(typeParameter: TypeParameter, tail: List[TypeParameter])
      extends Pretty {
    override def print(sb: StringBuilder): Unit = {
      typeParameter.print(sb)
      tail.foreach(_.print(sb))
    }
  }
  case class TypeParameter(
      identifier: String,
      classBound: ClassBound,
      interfaceBounds: List[InterfaceBound])
      extends Pretty {
    override def print(sb: StringBuilder): Unit = {
      sb.append(identifier)
      classBound.print(sb)
      interfaceBounds.foreach(_.print(sb))
    }
  }
  case class ClassBound(referenceTypeSignature: Option[ReferenceTypeSignature]) extends Pretty {
    override def print(sb: StringBuilder): Unit = {
      sb.append(':')
      referenceTypeSignature match {
        case Some(r: ReferenceTypeSignature) => r.print(sb)
        case _ =>
      }
    }
  }
  case class InterfaceBound(referenceTypeSignature: ReferenceTypeSignature) extends Pretty {
    override def print(sb: StringBuilder): Unit = {
      sb.append(':')
      referenceTypeSignature.print(sb)
    }
  }
  sealed trait SuperclassSignature extends Pretty
  sealed trait SuperinterfaceSignature extends Pretty

  // method signature
  case class MethodSignature(
      typeParameters: Option[TypeParameters],
      params: List[JavaTypeSignature],
      result: Result,
      throws: List[Either[ClassTypeSignature, TypeVariableSignature]])
  sealed trait Result
  sealed trait ThrowsSignature

  // void descriptor
  case object VoidDescriptor extends Result

  // field signature
  trait FieldSignature
}
