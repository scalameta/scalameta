package scala.meta.internal.metacp

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.Locale.LanguageRange
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
import scala.tools.asm.tree.FieldNode
import scala.tools.asm.tree.MethodNode
import scala.tools.asm.{Opcodes => o}
import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath

object Javacp {

  def array(tpe: s.Type): s.Type =
    ref("_root_.scala.Array#", tpe :: Nil)

  def ref(symbol: String, args: List[s.Type] = Nil): s.Type = {
    s.Type(
      s.Type.Tag.TYPE_REF,
      typeRef = Some(s.TypeRef(prefix = None, symbol, args))
    )
  }

  def enclosingPackage(name: String): String = {
    val slash = name.lastIndexOf('/')
    if (slash < 0) sys.error(name)
    else name.substring(0, slash)
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

  def process(root: Path, file: Path): s.TextDocument = {
    val bytes = Files.readAllBytes(file)
    val node = asmNodeFromBytes(bytes)
    val buf = ArrayBuffer.empty[s.SymbolInformation]

    def addPackage(name: String, owner: String): String = {
      val packageSymbol = owner + name + "."
      buf += s.SymbolInformation(
        symbol = packageSymbol,
        kind = k.PACKAGE,
        name = name,
        owner = owner
      )
      packageSymbol
    }

    val classSymbol = ssym(node.name)
    val className = getName(node.name)
    val isTopLevelClass = !node.name.contains("$")
    pprint.log(node.signature)
    pprint.log(node.outerMethodDesc)
    val classOwner: String = if (isTopLevelClass) {
      // Emit packages
      val packages = node.name.split("/")
      packages.iterator
        .take(packages.length - 1)
        .foldLeft(addPackage("_root_", "")) {
          case (owner, name) => addPackage(name, owner)
        }
    } else {
      ssym(node.name.substring(0, node.name.length - className.length - 1))
    }

    def saccessibility(access: Int): Option[s.Accessibility] = {
      val a = s.Accessibility.Tag
      if (access.hasFlag(o.ACC_PUBLIC)) accessibility(a.PUBLIC)
      else if (access.hasFlag(o.ACC_PROTECTED)) accessibility(a.PROTECTED)
      else if (access.hasFlag(o.ACC_PRIVATE)) accessibility(a.PRIVATE)
      else
        Some(
          s.Accessibility(a.PRIVATE_WITHIN, classOwner.substring(0, classOwner.lastIndexOf('.'))))
    }

    val classKind =
      if (node.access.hasFlag(o.ACC_INTERFACE)) k.TRAIT
      else k.CLASS
    val methods: Seq[MethodNode] = node.methods.asScala
    val descriptors: Seq[Declaration] = methods.map { method: MethodNode =>
      getSignature(method)
    }
    methods.zip(descriptors).foreach {
      case (method: MethodNode, descriptorPair: Declaration) =>
        import descriptorPair._

        val finalDescriptor = {
          val conflicting = descriptors.filter { d =>
            d.descriptor == descriptor &&
            d.name == method.name
          }
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
              returnType = returnType.map(_.toType)
            )
          )
        )
        buf += s.SymbolInformation(
          symbol = methodSymbol,
          kind = methodKind,
          name = method.name,
          owner = classSymbol,
          tpe = Some(methodType),
          accessibility = saccessibility(method.access)
        )
    }

    node.fields.asScala.foreach { field: FieldNode =>
      val fieldSymbol = classSymbol + field.name + "."
      val fieldKind =
        if (field.access.hasFlag(o.ACC_FINAL)) k.VAL
        else k.VAR
      val declaration = getSignature(field.name, field.signature, field.desc)
      buf += s.SymbolInformation(
        symbol = fieldSymbol,
        kind = fieldKind,
        name = field.name,
        owner = classSymbol,
        tpe = declaration.returnType.map(_.toType),
        accessibility = saccessibility(field.access)
      )
    }

    val decls = buf.map(_.symbol)

    val classDeclaration = getSignature(className, node.signature, null)
    val tparams = if (node.signature == null) {
      Nil
    } else {
      pprint.log(classDeclaration.formalTypeParameters)
      pprint.log(classDeclaration)
      classDeclaration.formalTypeParameters.map { tparamName =>
        val tparamSymbol = classSymbol + "[" + tparamName + "]"
        buf += s.SymbolInformation(
          tparamSymbol,
          kind = k.TYPE_PARAMETER,
          name = tparamName,
          owner = classSymbol
        )
        tparamSymbol
      }
    }
    val parents: Seq[s.Type] = if (node.signature == null) {
      (node.superName +: node.interfaces.asScala).map { parent =>
        val symbol = ssym(parent)
        s.Type(s.Type.Tag.TYPE_REF, typeRef = Some(s.TypeRef(symbol = symbol)))
      }
    } else {
      classDeclaration.superClass match {
        case Some(superClass) => superClass +: classDeclaration.interfaceTypes
        case None => classDeclaration.interfaceTypes
      }
    }
    val classTpe = s.Type(
      tag = s.Type.Tag.CLASS_INFO_TYPE,
      classInfoType = Some(
        s.ClassInfoType(
          typeParameters = tparams,
          parents = parents,
          declarations = decls
        )
      )
    )

    buf += s.SymbolInformation(
      symbol = classSymbol,
      kind = classKind,
      name = className,
      owner = classOwner,
      tpe = Some(classTpe),
      accessibility = saccessibility(node.access)
    )

    val uri = root.relativize(file).toString
    s.TextDocument(
      schema = s.Schema.SEMANTICDB3,
      uri = uri,
      symbols = buf
    )
  }

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

  def ssym(string: String): String =
    "_root_." + string.replace('$', '#').replace('/', '.') + "#"

  implicit class XtensionAccess(n: Int) {
    def hasFlag(flag: Int): Boolean =
      (flag & n) != 0
  }

  def getSignature(method: MethodNode): Declaration = {
    getSignature(method.name, method.signature, method.desc)
  }

  def getSignature(name: String, signature: String, desc: String): Declaration = {
    val toParse =
      if (signature != null) signature
      else desc
    val signatureReader = new SignatureReader(toParse)
    val v = new SemanticdbSignatureVisitor
    signatureReader.accept(v)
    Declaration(
      name,
      toParse,
      v.formatTypeParameters,
      v.parameterTypes,
      v.returnType,
      v.superClassType,
      v.interfaceTypes
    )
  }

  def accessibility(tag: s.Accessibility.Tag): Option[s.Accessibility] = Some(s.Accessibility(tag))

  sealed trait SignatureMode

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

  class SemanticdbSignatureVisitor extends SignatureVisitor(o.ASM5) {

    import SignatureMode._

    val parameterTypes: ListBuffer[JType] = ListBuffer.empty[JType]
    val formatTypeParameters: ListBuffer[String] = ListBuffer.empty[String]
    var owners = List.empty[JType]
    val interfaceTypes = ListBuffer.empty[JType]
    var superClassType = Option.empty[JType]
    var tpe: JType = newTpe
    var mode: SignatureMode = Start

    var returnType = Option.empty[JType]

    override def visitParameterType(): SignatureVisitor = {
      //      pprint.log("Parameter type")
      mode = ParameterType
      tpe = newTpe
//       require(owners == Nil)
      parameterTypes += tpe
      this
    }

    override def visitClassType(name: String): Unit = {
      //       pprint.log(name)
      tpe.setSymbol(name)
    }

    override def visitFormalTypeParameter(name: String): Unit = {
      //            pprint.log(name)
      mode = FormalType
      formatTypeParameters += name
    }

    override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
      //       pprint.log(wildcard)
      val arg = newTpe
      tpe.args += arg
      startType()
      tpe = arg
      this
    }

    def startType(): Unit = {
      owners = tpe :: owners
    }

    override def visitArrayType(): SignatureVisitor = {
      tpe.isArray = true
      this
    }

    override def visitTypeArgument(): Unit = {
      // pprint.log("Type Argument")
    }

    override def visitEnd(): Unit = {
      endType()
      mode match {
        case Interface =>
          interfaceTypes += tpe
        case SuperClass =>
          superClassType = Some(tpe)
        case ReturnType =>
          returnType = Some(tpe)
        case _ =>
      }
      pprint.log(tpe)
      // pprint.log("END")
    }

    def endType(): Unit = {
      if (owners.nonEmpty) {
        tpe = owners.head
        owners = owners.tail
      }
    }

    override def visitTypeVariable(name: String): Unit = {
      //      pprint.log(name)
      tpe.symbol = name
      tpe.name = name
      // endType()
    }

    override def visitExceptionType(): SignatureVisitor = {
      //      pprint.log("exceptionType")
      this
    }

    def setMode(mode: SignatureMode): Unit = {
      this.mode = mode
      tpe = newTpe
    }

    override def visitSuperclass(): SignatureVisitor = {
      pprint.log("superClass")
      setMode(SuperClass)
      this
    }

    override def visitInterface(): SignatureVisitor = {
      pprint.log("Interface")
      setMode(Interface)
      this
    }

    override def visitReturnType(): SignatureVisitor = {
      setMode(ReturnType)
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
      //      pprint.log(descriptor)
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


    def newTpe = new JType(false, "", "", ListBuffer.empty[JType])

  }

  case class Declaration(
      name: String,
      desc: String,
      formalTypeParameters: Seq[String],
      parameterTypes: Seq[JType],
      returnType: Option[JType],
      mySuperClass: Option[JType],
      myInterfaceTypes: Seq[JType]
  ) {
    def superClass: Option[s.Type] = mySuperClass.map(_.toType)
    def interfaceTypes: Seq[s.Type] = myInterfaceTypes.map(_.toType)
    val descriptor = parameterTypes.map(_.name).mkString(",")
  }

  object SignatureMode {

    case object Start extends SignatureMode
    case object ParameterType extends SignatureMode
    case object SuperClass extends SignatureMode
    case object Interface extends SignatureMode
    case object FormalType extends SignatureMode
    case object ReturnType extends SignatureMode

  }
}
