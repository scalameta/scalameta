package scala.meta.internal.javacp

import org.langmeta.io.AbsolutePath
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.meta.internal.javacp.asm._
import scala.meta.internal.metacp._
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.{semanticdb3 => s}
import scala.tools.asm.tree.ClassNode
import scala.tools.asm.tree.FieldNode
import scala.tools.asm.tree.InnerClassNode
import scala.tools.asm.tree.MethodNode
import scala.tools.asm.{Opcodes => o}

object Javacp {
  def parse(classfile: ToplevelClassfile): Option[ToplevelInfos] = {
    sinfos(classfile, classfile.node, 0, Scope.empty) match {
      case others :+ toplevel =>
        Some(ToplevelInfos(classfile, List(toplevel), others.toList))
      case _ =>
        None
    }
  }

  private def sinfos(
      toplevel: ToplevelClassfile,
      node: ClassNode,
      access: Int,
      scope: Scope): Seq[s.SymbolInformation] = {

    val buf = ArrayBuffer.empty[s.SymbolInformation]
    val decls = ListBuffer.empty[String]

    def addInfo(
        symbol: String,
        kind: s.SymbolInformation.Kind,
        name: String,
        tpe: Option[s.Type],
        access: Int,
        owner: String): Unit = {
      buf += s.SymbolInformation(
        symbol = symbol,
        language = javaLanguage,
        kind = kind,
        properties = sproperties(access),
        name = name,
        tpe = tpe,
        annotations = sannotations(access),
        accessibility = saccessibility(access, owner),
        owner = owner
      )
    }

    if (isAnonymousClass(node)) return Nil
    val classSymbol = ssym(node.name)
    val className = sname(node.name)
    val classAccess = node.access | access
    val hasOuterClassReference = node.fields.asScala.exists(isOuterClassReference)

    val isTopLevelClass = !node.name.contains("$")
    val classOwner: String = if (isTopLevelClass) {
      val parts = node.name.substring(0, node.name.lastIndexOf("/")).split("/").toList
      ("_root_" :: parts).foldLeft("") {
        case (owner, pkgName) =>
          val pkgSymbol = owner + pkgName + "."
          addInfo(
            pkgSymbol,
            k.PACKAGE,
            pkgName,
            None,
            o.ACC_PUBLIC,
            owner
          )
          pkgSymbol
      }
    } else {
      ssym(node.name.substring(0, node.name.length - className.length - 1))
    }

    val classKind =
      if (classAccess.hasFlag(o.ACC_INTERFACE)) k.TRAIT
      else k.CLASS

    val isJavaLangObject = node.name == "java/lang/Object"
    val classSignature: ClassSignature =
      if (isJavaLangObject) {
        // java/lang/Object has no super class so node.superName == null.
        // ClassSignature requires a non-null superName so we special-handle java/lang/Object
        // when assigning classParents below.
        ClassSignature.simple("impossible", Nil)
      } else if (node.signature == null) {
        ClassSignature.simple(node.superName, node.interfaces.asScala.toList)
      } else {
        JavaTypeSignature.parse(node.signature, new ClassSignatureVisitor)
      }

    val (classScope: Scope, classTypeParameters) =
      classSignature.typeParameters match {
        case Some(tp) => addTypeParameters(tp, classSymbol, scope)
        case _ => scope -> Nil
      }
    classTypeParameters.foreach(buf += _)

    val classParents =
      if (isJavaLangObject) Nil
      else classSignature.parents.map(_.toType(classScope))

    node.fields.asScala.foreach { field: FieldNode =>
      if (isOuterClassReference(field)) {
        // Drop the constructor argument that holds the reference to the outer class.
        ()
      } else {
        val fieldSymbol = classSymbol + field.name + "."
        val fieldSignature = JavaTypeSignature.parse(
          if (field.signature == null) field.desc else field.signature,
          new FieldSignatureVisitor
        )

        val fieldKind =
          if (field.access.hasFlag(o.ACC_FINAL)) k.VAL
          else k.VAR

        addInfo(
          fieldSymbol,
          fieldKind,
          field.name,
          Some(fieldSignature.toType(classScope)),
          field.access,
          classSymbol
        )

        decls += fieldSymbol
      }
    }

    // NOTE: this logic will soon change https://github.com/scalameta/scalameta/issues/1358
    val methodSignatures = node.methods.asScala.map { method: MethodNode =>
      val signature = JavaTypeSignature.parse(
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

        val params =
          if (method.node.name == "<init>" &&
              hasOuterClassReference &&
              // Guard against an empty parameter list, which seems to only happen
              // in the JDK for java/util/regex/Pattern.class
              method.signature.params.nonEmpty) {
            // Drop the constructor argument that holds the reference to the outer class.
            method.signature.params.tail
          } else {
            method.signature.params
          }

        val parameterSymbols = params.zipWithIndex.map {
          case (param: JavaTypeSignature, i) =>
            val paramName = {
              if (method.node.parameters == null) "arg" + i
              else method.node.parameters.get(i).name
            }
            val paramSymbol = methodSymbol + "(" + paramName + ")"
            addInfo(
              paramSymbol,
              k.PARAMETER,
              paramName,
              Some(param.toType(methodScope)),
              o.ACC_PUBLIC,
              methodSymbol
            )
            paramSymbol
        }

        val methodType = s.Type(
          tag = s.Type.Tag.METHOD_TYPE,
          methodType = Some(
            s.MethodType(
              typeParameters = methodTypeParameters.map(_.symbol),
              parameters = s.MethodType.ParameterList(parameterSymbols) :: Nil,
              returnType = Some(method.signature.result.toType(methodScope))
            )
          )
        )

        addInfo(
          methodSymbol,
          k.DEF,
          method.node.name,
          Some(methodType),
          method.node.access,
          classSymbol
        )

        decls += methodSymbol
    }

    // node.innerClasses includes all inner classes, both direct and those nested inside other inner classes.
    val directInnerClasses = node.innerClasses.asScala.filter(_.outerName == node.name)
    directInnerClasses.foreach { ic =>
      val innerClassSymbol = ssym(ic.name)
      decls += innerClassSymbol
      val innerPath = asmNameToPath(ic.name, toplevel.base)
      val innerClassNode = innerPath.toClassNode
      buf ++= sinfos(toplevel, innerClassNode, ic.access, classScope)
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

    addInfo(
      classSymbol,
      classKind,
      className,
      Some(classTpe),
      classAccess,
      classOwner
    )
    buf.result()
  }

  // Returns true if this field holds a reference to an outer enclosing class.
  private def isOuterClassReference(field: FieldNode): Boolean =
    field.name.startsWith("this$")

  // The logic behind this method is an implementation of the answer in this SO question:
  // https://stackoverflow.com/questions/42676404/how-do-i-know-if-i-am-visiting-an-anonymous-class-in-asm
  // ClassNode.innerClasses includes all inner classes of a compilation unit, both nested inner classes as well
  // as enclosing outer classes. Anonymous classes are distinguished by InnerClassNode.innerName == null.
  private def isAnonymousClass(node: ClassNode): Boolean = {
    node.innerClasses.asScala.exists { ic: InnerClassNode =>
      ic.name == node.name &&
      ic.innerName == null
    }
  }

  private val javaLanguage = Some(s.Language("Java"))

  private def fromJavaTypeSignature(sig: JavaTypeSignature, scope: Scope): s.Type =
    sig match {
      case ClassTypeSignature(SimpleClassTypeSignature(identifier, targs), suffix) =>
        require(identifier != null, sig.toString)
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

  private case class TypeParameterInfo(value: TypeParameter, symbol: String)
  private def addTypeParameters(
      typeParameters: TypeParameters,
      ownerSymbol: String,
      scope: Scope): (Scope, List[s.SymbolInformation]) = {
    var nextScope = scope
    // Enter all type variables before computing types for right hand side type parameter bounds.
    // The bounds may forward reference type variables like here below:
    // public abstract class Recursive<
    //           A extends Recursive <A, B>,
    //           B extends Recursive.Inner <A , B>>
    val infos = typeParameters.all.map { typeParameter: TypeParameter =>
      val symbol = ownerSymbol + "[" + typeParameter.identifier + "]"
      nextScope = nextScope.enter(typeParameter.identifier, symbol)
      TypeParameterInfo(typeParameter, symbol)
    }
    nextScope ->
      infos.map(info => addTypeParameter(info, ownerSymbol, nextScope))
  }
  private def addTypeParameter(
      typeParameter: TypeParameterInfo,
      ownerSymbol: String,
      scope: Scope): s.SymbolInformation = {
    val typeParameters = typeParameter.value.upperBounds.map(fromJavaTypeSignature(_, scope))
    val upperBounds = typeParameters match {
      case upperBound :: Nil =>
        upperBound
      case _ =>
        s.Type(
          tag = s.Type.Tag.STRUCTURAL_TYPE,
          structuralType = Some(s.StructuralType(parents = typeParameters))
        )
    }
    val tpe = s.Type(
      tag = s.Type.Tag.TYPE_TYPE,
      typeType = Some(s.TypeType(upperBound = Some(upperBounds)))
    )

    s.SymbolInformation(
      symbol = typeParameter.symbol,
      language = javaLanguage,
      kind = k.TYPE_PARAMETER,
      name = typeParameter.value.identifier,
      tpe = Some(tpe),
      owner = ownerSymbol
    )
  }

  private def methodDescriptor(signature: MethodSignature): String =
    signature.params.iterator
      .map {
        case t: BaseType => t.name
        case t: ClassTypeSignature => sname(t.simpleClassTypeSignature.identifier)
        case t: TypeVariableSignature => t.identifier
        case _: ArrayTypeSignature => "Array"
      }
      .mkString(",")

  private case class MethodInfo(node: MethodNode, descriptor: String, signature: MethodSignature)

  private def asmNameToPath(asmName: String, base: AbsolutePath): AbsolutePath = {
    (asmName + ".class").split("/").foldLeft(base) {
      case (accum, filename) => accum.resolve(filename)
    }
  }

  private def sname(asmName: String): String = {
    var i = asmName.length - 1
    while (i >= 0) {
      asmName.charAt(i) match {
        case '$' | '/' => return asmName.substring(i + 1)
        case _ => i -= 1
      }
    }
    throw new IllegalArgumentException(s"Missing $$ or / from symbol '$asmName'")
  }

  private def ssym(asmName: String): String =
    "_root_." + asmName.replace('$', '#').replace('/', '.') + "#"

  private def saccessibility(access: Int, owner: String): Option[s.Accessibility] = {
    def sacc(tag: s.Accessibility.Tag): Option[s.Accessibility] = Some(s.Accessibility(tag))
    val a = s.Accessibility.Tag
    if (access.hasFlag(o.ACC_PUBLIC)) sacc(a.PUBLIC)
    else if (access.hasFlag(o.ACC_PROTECTED)) sacc(a.PROTECTED)
    else if (access.hasFlag(o.ACC_PRIVATE)) sacc(a.PRIVATE)
    else {
      Some(
        s.Accessibility(tag = a.PRIVATE_WITHIN, symbol = owner.substring(0, owner.lastIndexOf('.')))
      )
    }
  }

  private def sannotations(access: Int): Seq[s.Annotation] = {
    val buf = List.newBuilder[s.Annotation]

    def push(symbol: String): Unit =
      buf += s.Annotation(Some(styperef(symbol)))

    if (access.hasFlag(o.ACC_DEPRECATED)) push("_root_.scala.deprecated#")
    if (access.hasFlag(o.ACC_STRICT)) push("_root_.scala.annotation.strictfp#")

    buf.result()
  }

  private def sproperties(access: Int): Int = {
    val p = s.SymbolInformation.Property
    var bits = 0
    def sflip(sbit: Int) = bits ^= sbit
    if (access.hasFlag(o.ACC_ABSTRACT)) sflip(p.ABSTRACT.value)
    if (access.hasFlag(o.ACC_FINAL)) sflip(p.FINAL.value)
    if (access.hasFlag(o.ACC_STATIC)) sflip(p.STATIC.value)
    bits
  }

  private def sarray(tpe: s.Type): s.Type =
    styperef("_root_.scala.Array#", tpe :: Nil)

  private def styperef(symbol: String, args: List[s.Type] = Nil, prefix: Option[s.Type] = None): s.Type = {
    s.Type(
      tag = s.Type.Tag.TYPE_REF,
      typeRef = Some(s.TypeRef(prefix, symbol, args))
    )
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
