package scala.meta.internal.metacp

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.{semanticdb3 => s}
import scala.tools.asm.ClassReader
import scala.tools.asm.signature.SignatureReader
import scala.tools.asm.signature.SignatureVisitor
import scala.tools.asm.tree.ClassNode
import scala.tools.asm.{Opcodes => o}
import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath

object Javacp {

  def asmNodeFromBytes(bytes: Array[Byte]): ClassNode = {
    val node = new ClassNode()
    new ClassReader(bytes).accept(
      node,
      ClassReader.SKIP_DEBUG |
        ClassReader.SKIP_FRAMES |
        ClassReader.SKIP_CODE
    )
    node
  }

  def ref(symbol: String, args: List[s.Type] = Nil): s.Type = {
    s.Type(
      s.Type.Tag.TYPE_REF,
      typeRef = Some(s.TypeRef(prefix = None, symbol, args))
    )
  }

  class JType(
      var isArray: Boolean,
      var symbol: String,
      var name: String,
      val args: ListBuffer[JType]) {
    def setPrimitive(name: String): Unit = {
      this.symbol = s"_root_.scala.$name#"
      this.name = name
    }
    def setSymbol(newSymbol: String): Unit = {
      symbol = ssym(newSymbol)
      name = getName(newSymbol)
    }

    def toType: s.Type = {
      val tpe = ref(symbol, args.iterator.map(_.toType).toList)
      if (isArray) array(tpe)
      else tpe
    }

    override def toString: String = {
      val suffix = if (isArray) "[]" else ""
      if (args.isEmpty) symbol + suffix
      else symbol + args.mkString("<", ", ", ">") + suffix
    }
  }

  sealed trait SignatureMode
  object SignatureMode {
    case object ParameterType extends SignatureMode
    case object FormalType extends SignatureMode
    case object ReturnType extends SignatureMode
  }

  def getName(symbol: String): String = {
    val dollar = symbol.lastIndexOf('$')
    if (dollar < 0) {
      val slash = symbol.lastIndexOf('/')
      if (slash < 0) sys.error(s"Missing $$ or / from symbol '$symbol'")
      else symbol.substring(slash + 1)
    } else {
      symbol.substring(dollar + 1)
    }
  }

  class SemanticdbSignatureVisitor extends SignatureVisitor(o.ASM5) {
    import SignatureMode._
    def newTpe = new JType(false, "", "", ListBuffer.empty[JType])
    var owners = List.empty[JType]
    var tpe: JType = newTpe
    def returnType: Option[JType] =
      if (mode == ReturnType) Some(tpe)
      else None
    val parameterTypes: ListBuffer[JType] = ListBuffer.empty[JType]
    val formatTypeParameters: ListBuffer[String] = ListBuffer.empty[String]
    var mode: SignatureMode = FormalType

    override def visitParameterType(): SignatureVisitor = {
//      pprint.log("Parameter type")
      mode = ParameterType
      tpe = newTpe
      // require(owners == Nil)
      parameterTypes += tpe
      this
    }

    override def visitClassType(name: String): Unit = {
      // pprint.log(name)
      tpe.setSymbol(name)
    }

    override def visitFormalTypeParameter(name: String): Unit = {
//      pprint.log(name)
      formatTypeParameters += name
    }

    override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
      // pprint.log(wildcard)
      val arg = newTpe
      tpe.args += arg
      startType()
      tpe = arg
      this
    }

    override def visitArrayType(): SignatureVisitor = {
      tpe.isArray = true
      this
    }

    override def visitTypeArgument(): Unit = {
      // pprint.log("Type Argument")
    }

    override def visitEnd(): Unit = {
      mode match {
        case ParameterType =>
          endType()
        case ReturnType =>
          endType()
        case _ =>
      }
      // pprint.log("END")
    }

    def startType(): Unit = {
      owners = tpe :: owners
    }

    def endType(): Unit = {
      if (owners.nonEmpty) {
        tpe = owners.head
        owners = owners.tail
      }
    }

    override def visitTypeVariable(name: String): Unit = {
      tpe.symbol = name
      tpe.name = name
      // endType()
    }

    override def visitExceptionType(): SignatureVisitor = {
//      pprint.log("exceptionType")
      this
    }

    override def visitSuperclass(): SignatureVisitor = {
//      pprint.log("superClass")
      this
    }

    override def visitInterface(): SignatureVisitor = {
//      pprint.log("Interface")
      this
    }

    override def visitInterfaceBound(): SignatureVisitor = {
//      pprint.log("interface bound")
      this
    }

    override def visitInnerClassType(name: String): Unit = {
//      pprint.log(name)
    }

    override def visitClassBound(): SignatureVisitor = {
//      pprint.log("classBound")
      this
    }

    override def visitBaseType(descriptor: Char): Unit = {
      descriptor match {
        case 'V' => tpe.setPrimitive("Unit")
        case 'B' => tpe.setPrimitive("Byte")
        case 'J' => tpe.setPrimitive("Long")
        case 'Z' => tpe.setPrimitive("Boolean")
        case 'I' => tpe.setPrimitive("Int")
        case 'S' => tpe.setPrimitive("Short")
        case 'C' => tpe.setPrimitive("Char")
        case 'F' => tpe.setPrimitive("Float")
        case 'D' => tpe.setPrimitive("Double")
        case _ => sys.error(descriptor.toString)
      }
      // endType()
    }

    override def visitReturnType(): SignatureVisitor = {
      mode = ReturnType
//      pprint.log("Return type")
      tpe = newTpe
      // startType()
      this
    }

  }

  def array(tpe: s.Type) =
    ref("_root_.scala.Array#", tpe :: Nil)

  def ssym(string: String): String =
    "_root_." + string.replace('$', '#').replace('/', '.') + "#"

  implicit class XtensionAccess(n: Int) {
    def hasFlag(flag: Int): Boolean =
      (flag & n) != 0
  }

  def getSignature(signature: String, desc: String): SemanticdbSignatureVisitor = {
    val toParse =
      if (signature != null) signature
      else desc
    val signatureReader = new SignatureReader(toParse)
    val v = new SemanticdbSignatureVisitor
    signatureReader.accept(v)
    v
  }

  def process(root: Path, file: Path): s.TextDocument = {
    val bytes = Files.readAllBytes(file)
    val node = asmNodeFromBytes(bytes)
    val buf = ArrayBuffer.empty[s.SymbolInformation]
    val classSymbol = ssym(node.name)
    val className = getName(node.name)
    val classOwner =
      ssym(node.name.substring(0, node.name.length - className.length - 1))
    val classKind =
      if (node.access.hasFlag(o.ACC_INTERFACE)) k.TRAIT
      else k.CLASS
    val methods = node.methods.asScala
    val decriptorUsage =
      scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
    val descriptors = methods.map { method =>
      val v = getSignature(method.signature, method.desc)
      val names = v.parameterTypes.map(_.name)
      val descriptor = v.parameterTypes.map(_.name).mkString(",")
      (descriptor, method.desc, v.parameterTypes, v.returnType)
    }
    methods.zip(descriptors).foreach {
      case (
          method,
          descriptorPair @ (descriptor, desc, parameterTypes, Some(returnType))
          ) =>
        val finalDescriptor = {
          val conflicting = descriptors.filter(_._1 == descriptor)
          if (conflicting.lengthCompare(1) == 0) descriptor
          else {
            val index = conflicting.indexOf(descriptorPair) + 1
            descriptor + "+" + index
          }
        }
        val methodSymbol = classSymbol + method.name + "(" + finalDescriptor + ")."
        val methodKind = k.DEF
        val paramSymbols = ListBuffer.empty[String]
        parameterTypes.zipWithIndex.foreach {
          case (param, i) =>
            // TODO(olafur) use node.parameters.name if -parameters is set in javacOptions
            val paramName = "arg" + i
            val paramSymbol = methodSymbol + "(" + paramName + ")"
            paramSymbols += paramSymbol
            val paramKind = k.PARAMETER
            buf += s.SymbolInformation(
              symbol = paramSymbol,
              kind = paramKind,
              name = paramName,
              owner = methodSymbol,
              tpe = Some(param.toType)
            )
        }
        val methodType = s.Type(
          s.Type.Tag.METHOD_TYPE,
          methodType = Some(
            s.MethodType(
              parameters = s.MethodType.ParameterList(paramSymbols) :: Nil,
              returnType = Some(returnType.toType)
            )
          )
        )
        buf += s.SymbolInformation(
          symbol = methodSymbol,
          kind = methodKind,
          name = method.name,
          owner = classSymbol,
          tpe = Some(methodType)
        )
    }

    node.fields.asScala.foreach { field =>
      val fieldSymbol = classSymbol + field.name + "."
      val fieldKind = k.VAR
      buf += s.SymbolInformation(
        symbol = fieldSymbol,
        kind = fieldKind,
        name = field.name,
        owner = classOwner
      )
    }

    val decls = buf.map(_.symbol)
    val parents = (node.superName +: node.interfaces.asScala).map { parent =>
      val symbol = ssym(parent)
      s.Type(s.Type.Tag.TYPE_REF, typeRef = Some(s.TypeRef(symbol = symbol)))
    }

    val classTpe = s.Type(
      tag = s.Type.Tag.STRUCTURAL_TYPE,
      structuralType = Some(
        s.StructuralType(
          declarations = decls,
          parents = parents
        ))
    )

    buf += s.SymbolInformation(
      symbol = classSymbol,
      kind = classKind,
      name = className,
      owner = classOwner,
      tpe = Some(classTpe)
    )

    val uri = root.relativize(file).toString
    s.TextDocument(
      schema = s.Schema.SEMANTICDB3,
      uri = uri,
      symbols = buf
    )
  }

  def main(args: Array[String]): Unit = {
    run(args)
//    val signature =
//      "<T:Ljava/lang/Object;>(Ljava/util/ArrayList<Ljava/util/ArrayList<[TT;>;>;)Ljava/util/ArrayList<Ljava/util/ArrayList<[TT;>;>;"
//    val sr = new SignatureReader(signature)
//    val v = new SemanticdbSignatureVisitor
//    sr.accept(v)
  }

  def run(args: Array[String]): Unit = {
    val root = AbsolutePath("core/target/scala-2.12/classes/test")
    Files.walkFileTree(
      root.toNIO,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(file) == "class") {

            val db = process(root.toNIO, file)
            if (!file.toString.contains('$')) {
              // pprint.log(db.toProtoString, height = 1000)
              // Main.pprint(db)
            }
          }
          FileVisitResult.CONTINUE
        }
      }
    )

  }
}
