package scala.meta.internal.metacp

import java.nio.file.{Files, Path, Paths}

import org.langmeta.internal.io.PathIO

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.meta.internal.metacp.asm._
import scala.meta.internal.metacp.asm.JavaTypeSignature._
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.{semanticdb3 => s}
import scala.tools.asm.ClassReader
import scala.tools.asm.tree.{ClassNode, FieldNode, InnerClassNode, MethodNode}
import scala.tools.asm.{Opcodes => o}
import scala.util.control.{NoStackTrace, NonFatal}

object Javacp {

  def sinfos(
      root: Path,
      file: Path,
      scope: Scope,
      isVisited: mutable.Set[Path]): Seq[s.SymbolInformation] = {
    if (isVisited(file)) Nil
    else {
      isVisited += file
      val bytes = Files.readAllBytes(file)
      val node = parseClassNode(bytes)
      if (isAnonymousClass(node)) Nil
      else {
        sinfos(node, scope, root, isVisited)
      }
    }
  }

  // The logic behind this method is an implementation of the answer in this SO question:
  // https://stackoverflow.com/questions/42676404/how-do-i-know-if-i-am-visiting-an-anonymous-class-in-asm
  def isAnonymousClass(node: ClassNode): Boolean = {
    node.innerClasses.asScala.exists { ic: InnerClassNode =>
      ic.name == node.name &&
      ic.innerName == null
    }
  }

  val javaLanguage = Some(s.Language("Java"))

  def fromJavaTypeSignature(sig: JavaTypeSignature, scope: Scope): s.Type =
    sig match {
      case ClassTypeSignature(SimpleClassTypeSignature(identifier, targs), suffix) =>
        val prefix = styperef(ssym(identifier), targs.toType(scope))
        suffix.foldLeft(prefix) {
          case (accum, s: ClassTypeSignatureSuffix) =>
            styperef(
              prefix = Some(accum),
              symbol = ssym(s.simpleClassTypeSignature.identifier),
              args = s.simpleClassTypeSignature.typeArguments.toType(scope)
            )
        }
      case TypeVariableSignature(name) =>
        styperef(scope.resolve(name))
      case t: BaseType =>
        styperef("_root_.scala." + t.name + "#")
      case ArrayTypeSignature(tpe) =>
        sarray(tpe.toType(scope))
    }

  case class TypeParameterInfo(value: TypeParameter, symbol: String)
  def addTypeParameters(
      typeParameters: TypeParameters,
      ownerSymbol: String,
      scope: Scope): (Scope, List[s.SymbolInformation]) = {
    var nextScope = scope
    val infos = typeParameters.all.map { typeParameter: TypeParameter =>
      val symbol = ownerSymbol + "[" + typeParameter.identifier + "]"
      nextScope = nextScope.enter(typeParameter.identifier, symbol)
      TypeParameterInfo(typeParameter, symbol)
    }
    nextScope ->
      infos.map(info => addTypeParameter(info, ownerSymbol, nextScope))
  }
  def addTypeParameter(
      typeParameter: TypeParameterInfo,
      ownerSymbol: String,
      scope: Scope): s.SymbolInformation = {
    val typeParameters = typeParameter.value.upperBounds.map(fromJavaTypeSignature(_, scope))
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
      owner = ownerSymbol,
      language = javaLanguage,
      kind = k.TYPE_PARAMETER,
      name = typeParameter.value.identifier,
      tpe = Some(tpe)
    )
  }

  def methodDescriptor(signature: MethodSignature): String =
    signature.params.iterator
      .map {
        case t: BaseType => t.name
        case t: ClassTypeSignature => sname(t.simpleClassTypeSignature.identifier)
        case t: TypeVariableSignature => t.identifier
        case _: ArrayTypeSignature => "Array"
      }
      .mkString(",")

  case class MethodInfo(node: MethodNode, descriptor: String, signature: MethodSignature)

  def asmNameToPath(asmName: String, root: Path): Path = {
    (asmName + ".class").split("/").foldLeft(root) {
      case (accum, filename) => accum.resolve(filename)
    }
  }

  case class Scope(bindings: Map[String, String]) {
    def resolve(name: String): String = {
      bindings.getOrElse(name, {
        // TODO: fix https://github.com/scalameta/scalameta/issues/1365.
        // There are still a handful of cases in spark-sql where resolution fails for some reason.
        name
      })
    }
    def enter(name: String, symbol: String): Scope =
      Scope(bindings.updated(name, symbol))
  }
  object Scope {
    val empty = Scope(Map.empty)
  }

  def sinfos(
      node: ClassNode,
      scope: Scope,
      root: Path,
      isVisited: mutable.Set[Path]): Seq[s.SymbolInformation] = {

    val buf = ArrayBuffer.empty[s.SymbolInformation]
    val decls = ListBuffer.empty[String]

    val classSymbol = ssym(node.name)
    val className = sname(node.name)

    val isTopLevelClass = !node.name.contains("$")
    val classOwner: String = if (isTopLevelClass) {
      spackages(node.name, buf)
    } else {
      ssym(node.name.substring(0, node.name.length - className.length - 1))
    }

    val classKind =
      if (node.access.hasFlag(o.ACC_INTERFACE)) k.TRAIT
      else k.CLASS

    val classSignature: ClassSignature =
      if (node.signature == null) {
        ClassSignature.simple(node.superName, node.interfaces.asScala.toList)
      } else {
        JavaTypeSignature.parse[ClassSignature](node.signature, new ClassSignatureVisitor)
      }

    val (classScope: Scope, classTypeParameters) =
      classSignature.typeParameters match {
        case Some(tp) => addTypeParameters(tp, classSymbol, scope)
        case _ => scope -> Nil
      }
    classTypeParameters.foreach(buf += _)

    val classParents = classSignature.parents.map(_.toType(classScope))

    node.fields.asScala.foreach { field: FieldNode =>
      if (field.name.startsWith("this$")) {
        // Skip synthetic this$0 fields for inner classes that reference the enclosing class.
        ()
      } else {
        val fieldSymbol = classSymbol + field.name + "."
        val fieldSignature = JavaTypeSignature.parse(
          if (field.signature == null) field.desc else field.signature,
          new FieldSignatureVisitor
        )

        buf += s.SymbolInformation(
          symbol = fieldSymbol,
          owner = classSymbol,
          language = javaLanguage,
          kind =
            if (field.access.hasFlag(o.ACC_FINAL)) k.VAL
            else k.VAR,
          name = field.name,
          accessibility = saccessibility(field.access, classSymbol),
          properties = sproperties(field.access),
          annotations = sannotations(field.access),
          tpe = Some(fieldSignature.toType(classScope))
        )

        decls += fieldSymbol
      }
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
        val methodSymbol = classSymbol + method.node.name + "(" + method.descriptor + suffix + ")" + "."

        val (methodScope, methodTypeParameters) = method.signature.typeParameters match {
          case Some(tp: TypeParameters) => addTypeParameters(tp, methodSymbol, classScope)
          case _ => classScope -> Nil
        }
        methodTypeParameters.foreach(buf += _)

        val parameterSymbols = method.signature.params.zipWithIndex.map {
          case (param: JavaTypeSignature, i) =>
            // TODO(olafur) use node.parameters for JDK 8 with -parameters
            val name = "arg" + i
            val paramSymbol = methodSymbol + "(" + name + ")"
            buf += s.SymbolInformation(
              symbol = paramSymbol,
              owner = methodSymbol,
              language = javaLanguage,
              kind = k.PARAMETER,
              name = name,
              tpe = Some(param.toType(methodScope))
            )
            paramSymbol
        }

        val methodType = s.Type(
          s.Type.Tag.METHOD_TYPE,
          methodType = Some(
            s.MethodType(
              typeParameters = methodTypeParameters.map(_.symbol),
              parameters = s.MethodType.ParameterList(parameterSymbols) :: Nil,
              returnType = Some(method.signature.result.toType(methodScope))
            )
          )
        )

        buf += s.SymbolInformation(
          symbol = methodSymbol,
          owner = classSymbol,
          language = javaLanguage,
          kind = k.DEF,
          name = method.node.name,
          accessibility = saccessibility(method.node.access, classSymbol),
          properties = sproperties(method.node.access),
          annotations = sannotations(method.node.access),
          tpe = Some(methodType)
        )

        decls += methodSymbol
    }

    node.innerClasses.asScala.foreach { ic: InnerClassNode =>
      val innerClassPath = asmNameToPath(ic.name, root)
      if (Files.isRegularFile(innerClassPath)) {
        buf ++= sinfos(root, innerClassPath, classScope, isVisited)
      }
    }

    val classTpe = s.Type(
      tag = s.Type.Tag.CLASS_INFO_TYPE,
      classInfoType = Some(
        s.ClassInfoType(
          typeParameters = classTypeParameters.map(_.symbol),
          parents = classParents,
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

  def parseClassNode(bytes: Array[Byte]): ClassNode = {
    val node = new ClassNode()
    new ClassReader(bytes).accept(
      node,
      ClassReader.SKIP_DEBUG |
        ClassReader.SKIP_FRAMES |
        ClassReader.SKIP_CODE
    )
    node
  }

  def sname(asmName: String): String = {
    var i = asmName.length - 1
    while (i >= 0) {
      asmName.charAt(i) match {
        case '$' | '/' => return asmName.substring(i + 1)
        case _ => i -= 1
      }
    }
    throw new IllegalArgumentException(s"Missing $$ or / from symbol '$asmName'")
  }

  def ssym(asmName: String): String =
    "_root_." + asmName.replace('$', '#').replace('/', '.') + "#"

  def saccessibility(access: Int, owner: String): Option[s.Accessibility] = {
    def sacc(tag: s.Accessibility.Tag): Option[s.Accessibility] = Some(s.Accessibility(tag))
    val a = s.Accessibility.Tag
    if (access.hasFlag(o.ACC_PUBLIC)) sacc(a.PUBLIC)
    else if (access.hasFlag(o.ACC_PROTECTED)) sacc(a.PROTECTED)
    else if (access.hasFlag(o.ACC_PRIVATE)) sacc(a.PRIVATE)
    else {
      Some(
        s.Accessibility(a.PRIVATE_WITHIN, owner.substring(0, owner.lastIndexOf('.')))
      )
    }
  }

  def sannotations(access: Int): Seq[s.Annotation] = {
    val buf = List.newBuilder[s.Annotation]

    def push(symbol: String): Unit =
      buf += s.Annotation(Some(styperef(symbol)))

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

  def sarray(tpe: s.Type): s.Type =
    styperef("_root_.scala.Array#", tpe :: Nil)

  def styperef(symbol: String, args: List[s.Type] = Nil, prefix: Option[s.Type] = None): s.Type = {
    s.Type(
      s.Type.Tag.TYPE_REF,
      typeRef = Some(s.TypeRef(prefix, symbol, args))
    )
  }

  def spackages(nodeName: String, buf: ArrayBuffer[s.SymbolInformation]): String = {
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

  private implicit class XtensionTypeArgument(self: TypeArgument) {
    // TODO: implement wildcards after https://github.com/scalameta/scalameta/issues/1357
    def toType(scope: Scope): s.Type = self match {
      case ReferenceTypeArgument(_, referenceTypeSignature) =>
        referenceTypeSignature.toType(scope)
      case WildcardTypeArgument =>
        styperef("local_wildcard")
    }
  }

  private implicit class XtensionJavaTypeSignature(self: JavaTypeSignature) {
    def toType(scope: Scope): s.Type =
      fromJavaTypeSignature(self, scope)
  }

  private implicit class XtensionTypeArgumentsOption(self: Option[TypeArguments]) {
    def toType(scope: Scope): List[s.Type] = self match {
      case Some(targs: TypeArguments) => targs.all.map(_.toType(scope))
      case _ => Nil
    }
  }

  private implicit class XtensionAccess(asmAccess: Int) {
    def hasFlag(flag: Int): Boolean =
      (flag & asmAccess) != 0
  }

}
