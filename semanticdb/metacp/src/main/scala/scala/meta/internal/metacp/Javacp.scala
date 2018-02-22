package scala.meta.internal.metacp

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.Locale.LanguageRange
import java.util.NoSuchElementException
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.meta.internal.metacp.asm.ClassSignatureVisitor
import scala.meta.internal.metacp.asm.FieldSignatureVisitor
import scala.meta.internal.metacp.asm.JavaTypeSignature
import scala.meta.internal.metacp.asm.JavaTypeSignature._
import scala.meta.internal.metacp.asm.JavaTypeSignature.ReferenceTypeSignature._
import scala.meta.internal.metacp.asm.MethodSignatureVisitor
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

object Javacp { self =>

  def process(root: Path, file: Path, scopes: Scopes): s.TextDocument = {
    val bytes = Files.readAllBytes(file)
    val node = asmNodeFromBytes(bytes)
    val symbols = process(node, scopes)
    val uri = root.relativize(file).toString
    s.TextDocument(
      schema = s.Schema.SEMANTICDB3,
      uri = uri,
      symbols = symbols
    )
  }

  def addPackages(nodeName: String, buf: ArrayBuffer[s.SymbolInformation]): String = {
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
    val packages = nodeName.split("/")
    packages.iterator
      .take(packages.length - 1)
      .foldLeft(addPackage("_root_", "")) {
        case (owner, name) => addPackage(name, owner)
      }
  }

  def language = Some(s.Language("Java"))

  implicit class XtensionTypeParameter(self: TypeArgument) {
    def toType(implicit scopes: Scopes): s.Type = self match {
      case ReferenceTypeArgument(wildcard, referenceTypeSignature) =>
        val tpe = referenceTypeSignature.toType
        wildcard match {
          case Some(WildcardIndicator.Plus) => tpe // TODO
          case Some(WildcardIndicator.Minus) => tpe // TODO
          case _ => tpe
        }
      case WildcardTypeArgument =>
        ref("local_wildcard") // TODO: handle wildcard type arguments
    }
  }

  implicit class XtensionSymbolInformationJavacp(self: s.SymbolInformation) {
    def toBinding: Binding = Binding(self.name, self.symbol)
  }

  implicit class XtensionJavaTypeSignature(self: JavaTypeSignature) {
    def toType(implicit scopes: Scopes): s.Type =
      fromJavaTypeSignature(self)(scopes)
  }

  implicit class XtensionTypeArgumentsOption(self: Option[TypeArguments]) {
    def toType(implicit scopes: Scopes): List[s.Type] = self match {
      case Some(targs: TypeArguments) => targs.all.map(_.toType)
      case _ => Nil
    }
  }

  def fromJavaTypeSignature(sig: JavaTypeSignature)(implicit scopes: Scopes): s.Type = sig match {
    case ClassTypeSignature(_, SimpleClassTypeSignature(identifier, targs), todo) =>
      ref(ssym(identifier), targs.toType)
    case TypeVariableSignature(name) =>
      ref(scopes.resolve(name))
    case t: BaseType =>
      ref("_root_.scala." + t.name + "#")
    case ArrayTypeSignature(tpe) =>
      array(tpe.toType)
  }

  case class TypeParameterInfo(value: TypeParameter, symbol: String)
  def addTypeParameters(
      typeParameters: TypeParameters,
      ownerSymbol: String,
      scopes: Scopes): List[s.SymbolInformation] = {
    val infos = typeParameters.all.map { typeParameter: TypeParameter =>
      val symbol = ownerSymbol + "[" + typeParameter.identifier + "]"
      // need to register all tparams before computing rhs types
      scopes.registerBinding(ownerSymbol, typeParameter.identifier, symbol)
      TypeParameterInfo(typeParameter, symbol)
    }
    infos.map(info => addTypeParameter(info, ownerSymbol, scopes))
  }
  def addTypeParameter(
      typeParameter: TypeParameterInfo,
      ownerSymbol: String,
      scopes: Scopes): s.SymbolInformation = {
    val typeParameters = typeParameter.value.upperBounds.map(fromJavaTypeSignature(_)(scopes))
    val upperBounds = typeParameters match {
      case upperBound :: Nil =>
        upperBound
      case _ =>
        s.Type(
          s.Type.Tag.STRUCTURAL_TYPE,
          structuralType = Some(s.StructuralType(parents = typeParameters))
        )
    }
    val tpe = s.Type(
      s.Type.Tag.TYPE_TYPE,
      typeType = Some(s.TypeType(upperBound = Some(upperBounds)))
    )

    s.SymbolInformation(
      symbol = typeParameter.symbol,
      language = language,
      kind = k.TYPE_PARAMETER,
      name = typeParameter.value.identifier,
      tpe = Some(tpe),
      owner = ownerSymbol
    )
  }

  def methodDescriptor(signature: MethodSignature): String =
    signature.params.iterator
      .map {
        case t: BaseType => t.name
        case t: ClassTypeSignature => getName(t.simpleClassTypeSignature.identifier)
        case t: TypeVariableSignature => t.identifier
        case _: ArrayTypeSignature => "Array"
      }
      .mkString(",")

  case class MethodInfo(node: MethodNode, descriptor: String, signature: MethodSignature)

  def process(node: ClassNode, scopes: Scopes): Seq[s.SymbolInformation] = {
    implicit val implicitScopes = scopes

    val buf = ArrayBuffer.empty[s.SymbolInformation]
    val decls = ListBuffer.empty[String]

    val classSymbol = ssym(node.name)
    val className = getName(node.name)
    val isTopLevelClass = !node.name.contains("$")

    if (node.outerClass != null) {
      
    }

    val classOwner: String = if (isTopLevelClass) {
      addPackages(node.name, buf)
    } else {
      ssym(node.name.substring(0, node.name.length - className.length - 1))
    }
    scopes.registerOwner(classSymbol, classOwner)

    val classKind =
      if (node.access.hasFlag(o.ACC_INTERFACE)) k.TRAIT
      else k.CLASS

    val classSignature =
      if (node.signature == null) {
        None
      } else {
        val classSignature =
          JavaTypeSignature.parse[ClassSignature](node.signature, new ClassSignatureVisitor)
        Some(classSignature)
      }

    val tparams: Seq[s.SymbolInformation] = classSignature match {
      case Some(ClassSignature(Some(typeParameters), _, _)) =>
        addTypeParameters(typeParameters, classSymbol, scopes)
      case _ => Nil
    }
    tparams.foreach(buf += _)

    val parents = classSignature match {
      case Some(c: ClassSignature) => c.parents.map(_.toType)
      case _ => Nil
    }

    node.fields.asScala.foreach { field: FieldNode =>
      val fieldSymbol = classSymbol + field.name + "."
      val fieldSignature = JavaTypeSignature.parse(
        if (field.signature == null) field.desc else field.signature,
        new FieldSignatureVisitor
      )

      buf += s.SymbolInformation(
        symbol = fieldSymbol,
        language = language,
        kind =
          if (field.access.hasFlag(o.ACC_FINAL)) k.VAL
          else k.VAR,
        name = field.name,
        accessibility = saccessibility(field.access, classSymbol),
        properties = sproperties(field.access),
        annotations = sannotations(field.access),
        tpe = Some(fieldSignature.toType)
      )

      decls += fieldSymbol
    }

    // NOTE: this logic will soon change https://github.com/scalameta/scalameta/issues/1358
    val methodSignatures = node.methods.asScala.map { method: MethodNode =>
      val signature = JavaTypeSignature.parse[MethodSignature](
        if (method.signature == null) method.desc else method.signature,
        new MethodSignatureVisitor
      )
      MethodInfo(method, methodDescriptor(signature), signature)
    }

    methodSignatures.foreach {
      case method: MethodInfo =>
        val synonyms = methodSignatures.filter { m =>
          m.node.name == method.node.name &&
          m.descriptor == method.descriptor
        }
        val suffix =
          if (synonyms.length == 1) ""
          else "+" + (1 + synonyms.indexWhere(_.signature eq method.signature))
        val methodSymbol = classSymbol + method.node.name + "(" + method.descriptor + suffix + ")"
        scopes.registerOwner(methodSymbol, classSymbol)

        val methodTypeParameters = method.signature.typeParameters match {
          case Some(tp: TypeParameters) => addTypeParameters(tp, methodSymbol, scopes)
          case _ => Nil
        }
        methodTypeParameters.foreach(buf += _)

        val parameterSymbols = method.signature.params.zipWithIndex.map {
          case (param: JavaTypeSignature, i) =>
            val name = "arg" + i // TODO(olafur) use node.parameters for JDK 8 with -parameters
            val paramSymbol = methodSymbol + "(" + name + ")"
            buf += s.SymbolInformation(
              symbol = paramSymbol,
              language = language,
              kind = k.PARAMETER,
              name = name,
              tpe = Some(param.toType)
            )
            paramSymbol
        }

        val methodType = s.Type(
          s.Type.Tag.METHOD_TYPE,
          methodType = Some(
            s.MethodType(
              typeParameters = methodTypeParameters.map(_.symbol),
              parameters = s.MethodType.ParameterList(parameterSymbols) :: Nil,
              returnType = Some(method.signature.result.toType)
            )
          )
        )

        buf += s.SymbolInformation(
          symbol = methodSymbol,
          language = language,
          kind = k.DEF,
          name = method.node.name,
          accessibility = saccessibility(method.node.access, classSymbol),
          properties = sproperties(method.node.access),
          annotations = sannotations(method.node.access),
          tpe = Some(methodType)
        )

        decls += methodSymbol
    }

    val classTpe = s.Type(
      tag = s.Type.Tag.CLASS_INFO_TYPE,
      classInfoType = Some(
        s.ClassInfoType(
          typeParameters = tparams.map(_.symbol),
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
      accessibility = saccessibility(node.access, classOwner),
      properties = sproperties(node.access),
      annotations = sannotations(node.access)
    )

    buf.result()
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
    var i = symbol.length - 1
    while (i >= 0) {
      symbol.charAt(i) match {
        case '$' | '/' => return symbol.substring(i + 1)
        case _ => i -= 1
      }
    }
    throw new IllegalArgumentException(s"Missing $$ or / from symbol '$symbol'")
  }

  def ssym(string: String): String =
    "_root_." + string.replace('$', '#').replace('/', '.') + "#"

  implicit class XtensionAccess(n: Int) {
    def hasFlag(flag: Int): Boolean =
      (flag & n) != 0
  }

  def accessibility(tag: s.Accessibility.Tag): Option[s.Accessibility] = Some(s.Accessibility(tag))
  def saccessibility(access: Int, owner: String): Option[s.Accessibility] = {
    val a = s.Accessibility.Tag
    if (access.hasFlag(o.ACC_PUBLIC)) accessibility(a.PUBLIC)
    else if (access.hasFlag(o.ACC_PROTECTED)) accessibility(a.PROTECTED)
    else if (access.hasFlag(o.ACC_PRIVATE)) accessibility(a.PRIVATE)
    else {
      Some(
        s.Accessibility(a.PRIVATE_WITHIN, owner.substring(0, owner.lastIndexOf('.')))
      )
    }
  }

  def sannotations(access: Int): Seq[s.Annotation] = {
    val buf = List.newBuilder[s.Annotation]

    def push(symbol: String): Unit =
      buf += s.Annotation(Some(ref(symbol)))

    if (access.hasFlag(o.ACC_DEPRECATED)) push("_root_.scala.deprecated#")
    if (access.hasFlag(o.ACC_STRICT)) push("_root_.scala.annotation.strictfp#")

    buf.result()
  }
  def sproperties(access: Int): Int = {
    val p = s.SymbolInformation.Property
    var bits = 0
    def sflip(sbit: Int) = bits ^= sbit
    if (access.hasFlag(o.ACC_ABSTRACT)) sflip(p.ABSTRACT.value)
    if (access.hasFlag(o.ACC_FINAL)) sflip(p.FINAL.value)
    bits
  }

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

}
