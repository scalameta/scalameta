package scala.meta.internal.javacp

import org.scalameta.collections._
import org.scalameta.internal.ScalaCompat._
import scala.meta.internal.classpath.ClasspathIndex
import scala.meta.internal.classpath.MissingSymbolException
import scala.meta.internal.javacp.asm._
import scala.meta.internal.metacp._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala.{Names => n}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.tools.asm.tree.ClassNode
import scala.tools.asm.tree.FieldNode
import scala.tools.asm.tree.InnerClassNode
import scala.tools.asm.tree.MethodNode
import scala.tools.asm.{Opcodes => o}

object Javacp {
  def parse(node: ClassNode, classpathIndex: ClasspathIndex): ClassfileInfos = {
    val infos = sinfos(node, classpathIndex, 0, Scope.empty)
    ClassfileInfos(node.name + ".class", s.Language.JAVA, infos.toList)
  }

  private def sinfos(
      node: ClassNode,
      classpathIndex: ClasspathIndex,
      access: Int,
      scope: Scope
  ): collection.Seq[s.SymbolInformation] = {

    val buf = ArrayBuffer.empty[s.SymbolInformation]
    val decls = ListBuffer.empty[String]

    def addInfo(
        symbol: String,
        kind: s.SymbolInformation.Kind,
        displayName: String,
        sig: s.Signature,
        access: Int
    ): s.SymbolInformation = {
      val info = s.SymbolInformation(
        symbol = symbol,
        language = l.JAVA,
        kind = kind,
        properties = sproperties(access, node),
        displayName = displayName,
        signature = sig,
        annotations = sannotations(access),
        access = saccess(access, symbol, kind)
      )
      buf += info
      info
    }

    if (isAnonymousClass(node)) return Nil
    val classSymbol = ssym(node.name)
    val classDisplayName = sdisplayName(node.name)
    val classAccess = node.access | access
    val hasOuterClassReference = node.fields.toScala.exists(isOuterClassReference)
    val isEnum = node.access.hasFlag(o.ACC_ENUM)

    val classKind = if (classAccess.hasFlag(o.ACC_INTERFACE)) k.INTERFACE else k.CLASS

    val isJavaLangObject = node.name == "java/lang/Object"
    val classSignature: ClassSignature =
      if (isJavaLangObject)
        // java/lang/Object has no super class so node.superName == null.
        // ClassSignature requires a non-null superName so we special-handle java/lang/Object
        // when assigning classParents below.
        ClassSignature.simple("impossible", Nil)
      else if (node.signature == null) ClassSignature
        .simple(node.superName, node.interfaces.toScala.toList)
      else JavaTypeSignature.parse(node.signature, new ClassSignatureVisitor)

    val (classScope: Scope, classTypeParameters) = classSignature.typeParameters match {
      case Some(tp) => addTypeParameters(tp, classSymbol, scope)
      case _ => scope -> Nil
    }
    classTypeParameters.foreach(buf += _)

    val classParents =
      if (isJavaLangObject) Nil else classSignature.parents.map(_.toSemanticTpe(classScope))

    node.fields.toScala.filterNot(_.access.hasFlag(o.ACC_SYNTHETIC)).foreach { field: FieldNode =>
      if (isOuterClassReference(field))
        // Drop the constructor argument that holds the reference to the outer class.
        ()
      else {
        val fieldSymbol = Symbols.Global(classSymbol, d.Term(field.name))
        val fieldDisplayName = field.name
        val fieldSignature = JavaTypeSignature.parse(
          if (field.signature == null) field.desc else field.signature,
          new FieldSignatureVisitor
        )
        val fieldInfo = addInfo(
          fieldSymbol,
          k.FIELD,
          fieldDisplayName,
          s.ValueSignature(fieldSignature.toSemanticTpe(classScope)),
          field.access
        )

        decls += fieldInfo.symbol
      }
    }

    val methodSignatures = node.methods.toScala.filterNot(_.access.hasFlag(o.ACC_SYNTHETIC))
      .map { method: MethodNode =>
        val signature = JavaTypeSignature.parse(
          if (method.signature == null) method.desc else method.signature,
          new MethodSignatureVisitor
        )
        MethodInfo(method, signature)
      }

    // NOTE: we sort methods by whether they're static or not in order to compute same method symbols as metac.
    // In scalac, static class members are separated from non-static members, which makes it impossible
    // to recover the original mixed static/non-static member order in the classfile.
    val byStaticAccess = methodSignatures.sortBy(_.node.access.hasFlag(o.ACC_STATIC))

    methodSignatures.foreach {
      case method: MethodInfo if method.node.name == "<clinit>" => ()
      case method: MethodInfo =>
        val isConstructor = method.node.name == "<init>"
        val methodDisambiguator = {
          val overloads = byStaticAccess.filter(_.node.name == method.node.name)
          if (overloads.lengthCompare(1) == 0) "()"
          else {
            val index = overloads.indexWhere(_.signature eq method.signature)
            if (index == 0) "()" else s"(+$index)"
          }
        }
        val methodDescriptor = d.Method(method.node.name, methodDisambiguator)
        val methodSymbol = Symbols.Global(classSymbol, methodDescriptor)

        val (methodScope, methodTypeParameters) = method.signature.typeParameters match {
          case Some(tp: TypeParameters) => addTypeParameters(tp, methodSymbol, classScope)
          case _ => classScope -> Nil
        }
        methodTypeParameters.foreach(buf += _)

        val collectNonMandatedParams
            : Tuple2[JavaTypeSignature, Int] => Option[(JavaTypeSignature, Option[String])] = {
          case (_, 0) if isConstructor && hasOuterClassReference =>
            // For JDK <21, implicit params holding the reference to the outer class
            // in constructors can only detected through flags if `-parameters` was passed
            // to `javac`, so we use the presence of a field as a heuristic proxy to
            // know when the first param is that reference and should therefore be skipped.
            //
            // Note that in JDK >=18, the heuristic is unreliable, as `javac` does not
            // synthetize the reference field if not needed, even though the constructor
            // still has an (unused) implicit param that we should skip.
            //
            // That means that inner classes not referencing their outer class will have
            // their implicit constructor param visible if compiled without `-parameters`
            // on JDK 18, 19 or 20.
            None
          case (sigParam, i) =>
            val nodeParamOpt = Option(method.node.parameters).flatMap(xs => Option(xs.get(i)))

            nodeParamOpt match {
              case Some(nodeParam)
                  if isConstructor && !isEnum &&
                    (nodeParam.access.hasFlag(o.ACC_MANDATED) ||
                      nodeParam.access.hasFlag(o.ACC_SYNTHETIC)) =>
                // Remove params holding the reference to the outer class in constructors
                None
              case Some(nodeParam) =>
                // For JDK >=21, null-named parameters may exist when `-parameters`
                // was not passed to `javac` and there is a mandated param.
                // See https://github.com/openjdk/jdk/pull/9862.
                Some((sigParam, Option(nodeParam.name)))
              case _ => Some((sigParam, None))
            }
        }

        val params = method.signature.params.zipWithIndex
          .collect(Function.unlift(collectNonMandatedParams))

        val parameters: List[s.SymbolInformation] = params.zipWithIndex
          .map { case ((param: JavaTypeSignature, paramDisplayNameOpt), i) =>
            val paramDisplayName = paramDisplayNameOpt.getOrElse("param" + i)
            val paramSymbol = Symbols.Global(methodSymbol, d.Parameter(paramDisplayName))
            val isRepeatedType = method.node.access.hasFlag(o.ACC_VARARGS) && i == params.length - 1
            val paramTpe =
              if (isRepeatedType) param.toSemanticTpe(methodScope) match {
                case s.TypeRef(s.NoType, "scala/Array#", targ :: Nil) => s.RepeatedType(targ)
                case tpe => sys.error(s"expected $paramDisplayName to be a scala/Array#, found $tpe")
              }
              else param.toSemanticTpe(methodScope)
            addInfo(
              paramSymbol,
              k.PARAMETER,
              paramDisplayName,
              s.ValueSignature(paramTpe),
              o.ACC_PUBLIC
            )
          }

        val returnType =
          if (isConstructor) s.NoType else method.signature.result.toSemanticTpe(methodScope)

        val methodKind = if (isConstructor) k.CONSTRUCTOR else k.METHOD

        val methodDisplayName = method.node.name

        val methodSig = s.MethodSignature(
          typeParameters = Some(s.Scope(methodTypeParameters.map(_.symbol))),
          parameterLists = List(s.Scope(parameters.map(_.symbol))),
          returnType = returnType
        )

        val methodInfo =
          addInfo(methodSymbol, methodKind, methodDisplayName, methodSig, method.node.access)

        decls += methodInfo.symbol
    }

    // node.innerClasses includes all inner classes, both direct and those nested inside other inner classes.
    val directInnerClasses = node.innerClasses.toScala.filter(_.outerName == node.name)
    directInnerClasses.foreach { ic =>
      val innerClassSymbol = ssym(ic.name)
      decls += innerClassSymbol
      val path = ic.name + ".class"
      val classfile = classpathIndex.getClassfile(path)
        .getOrElse(throw MissingSymbolException(ssym(ic.name)))
      val innerClassNode = classfile.toClassNode
      buf ++= sinfos(innerClassNode, classpathIndex, ic.access, classScope)
    }

    val classSig = s.ClassSignature(
      typeParameters = Some(s.Scope(classTypeParameters.map(_.symbol))),
      parents = classParents,
      self = s.NoType,
      declarations = Some(s.Scope(decls.toScalaSeq))
    )

    addInfo(classSymbol, classKind, classDisplayName, classSig, classAccess)
    buf
  }

  // Returns true if this field holds a reference to an outer enclosing class.
  private def isOuterClassReference(field: FieldNode): Boolean = field.name.startsWith("this$")

  // The logic behind this method is an implementation of the answer in this SO question:
  // https://stackoverflow.com/questions/42676404/how-do-i-know-if-i-am-visiting-an-anonymous-class-in-asm
  // ClassNode.innerClasses includes all inner classes of a compilation unit, both nested inner classes as well
  // as enclosing outer classes. Anonymous classes are distinguished by InnerClassNode.innerName == null.
  private def isAnonymousClass(node: ClassNode): Boolean = node.innerClasses.toScala
    .exists { ic: InnerClassNode => ic.name == node.name && ic.innerName == null }

  private def fromJavaTypeSignature(sig: JavaTypeSignature, scope: Scope): s.Type = sig match {
    case ClassTypeSignature(SimpleClassTypeSignature(identifier, targs), suffix) =>
      require(identifier != null, sig.toString)
      val sym = ssym(identifier)
      val prefix = styperef(sym, targs.toSemanticTpe(scope))
      val (result, _) = suffix
        .foldLeft(prefix -> sym) { case ((accum, owner), s: ClassTypeSignatureSuffix) =>
          val desc = Descriptor.Type(s.simpleClassTypeSignature.identifier)
          val symbol = Symbols.Global(owner, desc)
          styperef(
            prefix = accum,
            symbol = symbol,
            args = s.simpleClassTypeSignature.typeArguments.toSemanticTpe(scope)
          ) -> symbol
        }
      result
    case TypeVariableSignature(name) => styperef(scope.resolve(name))
    case t: BaseType => styperef("scala/" + t.name + "#")
    case ArrayTypeSignature(tpe) => sarray(tpe.toSemanticTpe(scope))
  }

  private case class TypeParameterInfo(value: TypeParameter, symbol: String)
  private def addTypeParameters(
      typeParameters: TypeParameters,
      ownerSymbol: String,
      scope: Scope
  ): (Scope, List[s.SymbolInformation]) = {
    var nextScope = scope
    // Enter all type variables before computing types for right hand side type parameter bounds.
    // The bounds may forward reference type variables like here below:
    // public abstract class Recursive<
    //           A extends Recursive <A, B>,
    //           B extends Recursive.Inner <A , B>>
    val infos = typeParameters.all.map { typeParameter: TypeParameter =>
      val symbol = Symbols.Global(ownerSymbol, d.TypeParameter(typeParameter.identifier))
      nextScope = nextScope.enter(typeParameter.identifier, symbol)
      TypeParameterInfo(typeParameter, symbol)
    }
    nextScope -> infos.map(info => addTypeParameter(info, ownerSymbol, nextScope))
  }
  private def addTypeParameter(
      typeParameter: TypeParameterInfo,
      ownerSymbol: String,
      scope: Scope
  ): s.SymbolInformation = {
    val typeParameters = typeParameter.value.upperBounds.map(fromJavaTypeSignature(_, scope))
    val upperBounds = typeParameters match {
      case upperBound :: Nil => upperBound
      case _ => s.IntersectionType(types = typeParameters)
    }
    val displayName = typeParameter.value.identifier
    val sig = s.TypeSignature(typeParameters = Some(s.Scope()), upperBound = upperBounds)

    s.SymbolInformation(
      symbol = typeParameter.symbol,
      language = l.JAVA,
      kind = k.TYPE_PARAMETER,
      displayName = displayName,
      signature = sig
    )
  }

  private case class MethodInfo(node: MethodNode, signature: MethodSignature)

  private def asmNameToPath(asmName: String, base: AbsolutePath): AbsolutePath =
    (asmName + ".class").split("/").foldLeft(base) { case (accum, filename) =>
      accum.resolve(filename)
    }

  private def sdisplayName(asmName: String): String = {
    var i = asmName.length - 1
    while (i >= 0) asmName.charAt(i) match {
      case '$' | '/' => return asmName.substring(i + 1)
      case _ => i -= 1
    }
    asmName
  }

  private def ssym(asmName: String): String = {
    var i = 0
    var slash = false
    val result = new StringBuilder
    val part = new StringBuilder
    def put(c: Char): Unit = part.append(c)
    def flush(): Unit = {
      result.append(n.encode(part.toString))
      part.clear()
    }
    while (i < asmName.length) {
      val c = asmName.charAt(i)
      if (c == '/') {
        slash = true
        flush()
        result.append('/')
      } else if (c == '$') {
        flush()
        result.append('#')
      } else put(c)
      i += 1
    }
    if (part.nonEmpty) flush()
    result.append('#')
    if (slash) result.toString else Symbols.EmptyPackage + result.toString
  }

  private def saccess(access: Int, symbol: String, kind: s.SymbolInformation.Kind): s.Access =
    kind match {
      case k.LOCAL | k.PARAMETER | k.TYPE_PARAMETER | k.PACKAGE => s.NoAccess
      case k.INTERFACE => s.PublicAccess()
      case _ =>
        if (access.hasFlag(o.ACC_PUBLIC)) s.PublicAccess()
        else if (access.hasFlag(o.ACC_PROTECTED)) s.ProtectedAccess()
        else if (access.hasFlag(o.ACC_PRIVATE)) s.PrivateAccess()
        else {
          val within = symbol.ownerChain.reverse.tail.find(_.desc.isPackage).get
          s.PrivateWithinAccess(within)
        }
    }

  private def sannotations(access: Int): Seq[s.Annotation] = {
    val buf = List.newBuilder[s.Annotation]

    def push(symbol: String): Unit = buf += s.Annotation(styperef(symbol))

    if (access.hasFlag(o.ACC_STRICT)) push("scala/annotation/strictfp#")

    buf.result()
  }

  private def sproperties(access: Int, ownerNode: ClassNode): Int = {
    val p = s.SymbolInformation.Property
    var bits = 0
    def sflip(p: s.SymbolInformation.Property) = bits ^= p.value
    if (access.hasFlag(o.ACC_ABSTRACT)) sflip(p.ABSTRACT)
    if (access.hasFlag(o.ACC_FINAL)) sflip(p.FINAL)
    if (access.hasFlag(o.ACC_STATIC)) sflip(p.STATIC)
    if (access.hasFlag(o.ACC_ENUM)) sflip(p.ENUM)
    if (ownerNode.access.hasFlag(o.ACC_INTERFACE) && !access.hasFlag(o.ACC_ABSTRACT) &&
      !access.hasFlag(o.ACC_STATIC)) sflip(p.DEFAULT)
    bits
  }

  private def sarray(tpe: s.Type): s.Type = styperef("scala/Array#", tpe :: Nil)

  private def styperef(
      symbol: String,
      args: List[s.Type] = Nil,
      prefix: s.Type = s.NoType
  ): s.Type = s.TypeRef(prefix, symbol, args)

  private implicit class XtensionTypeArgument(private val self: TypeArgument) extends AnyVal {
    // FIXME: https://github.com/scalameta/scalameta/issues/1563
    def toSemanticTpe(scope: Scope): s.Type = self match {
      case ReferenceTypeArgument(None, referenceTypeSignature) => referenceTypeSignature
          .toSemanticTpe(scope)
      case ReferenceTypeArgument(Some(_), _) | WildcardTypeArgument =>
        // FIXME: https://github.com/scalameta/scalameta/issues/1703
        styperef("local_wildcard")
    }
  }

  private implicit class XtensionJavaTypeSignature(private val self: JavaTypeSignature)
      extends AnyVal {
    def toSemanticTpe(scope: Scope): s.Type = fromJavaTypeSignature(self, scope)
  }

  private implicit class XtensionTypeArgumentsOption(private val self: Option[TypeArguments])
      extends AnyVal {
    def toSemanticTpe(scope: Scope): List[s.Type] = self match {
      case Some(targs: TypeArguments) => targs.all.map(_.toSemanticTpe(scope))
      case _ => Nil
    }
  }

  private implicit class XtensionAccess(private val asmAccess: Int) extends AnyVal {
    def hasFlag(flag: Int): Boolean = (flag & asmAccess) != 0
  }

}
