package scala.meta.internal.semanticdb.javac.semantics

import javax.lang.model.`type`.TypeKind
import javax.lang.model.element._
import scala.collection.mutable
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scala.collection.JavaConverters._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d, Names => n, _}

trait Elements {

  implicit class ElementOps(elem: Element) {

    def enclosingPackage: PackageElement = elem.getEnclosingElement match {
      case enclosing: PackageElement => enclosing
      case enclosing => enclosing.enclosingPackage
    }

    def enclosedElements: Seq[Element] = elem.getEnclosedElements.asScala

    def annotations: Seq[s.Annotation] = elem match {
      case elem: ExecutableElement if elem.getModifiers.contains(Modifier.STRICTFP) =>
        Seq(
          s.Annotation(
            tpe = s.TypeRef(symbol = "scala/annotation/strictfp#")
          ))
      case _ => Seq()
    }

    def isStatic(elem: ExecutableElement): Boolean = elem.getModifiers.contains(Modifier.STATIC)

    def owner: String = elem.getEnclosingElement.sym

    def sym: String = elem match {
      case elem: PackageElement =>
        val qualName = elem.getQualifiedName.toString
        if (qualName == "") Symbols.EmptyPackage
        else qualName.replace('.', '/') + "/"
      case elem: TypeElement =>
        Symbols.Global(owner, d.Type(symbolName))
      case elem: ExecutableElement =>
        val owner = elem.getEnclosingElement
        val disambig = {
          val siblings = owner.enclosedElements
          val siblingMethods = siblings.collect {
            case sibling: ExecutableElement if sibling.symbolName == symbolName => sibling
          }
          val (instance, static) = siblingMethods.partition(method => !isStatic(method))
          val methodPlace = (instance ++ static).indexOf(elem)
          if (methodPlace == 0) "()"
          else s"(+$methodPlace)"
        }
        Symbols.Global(owner.sym, d.Method(symbolName, disambig))
      case elem: VariableElement if elem.getKind == ElementKind.PARAMETER =>
        Symbols.Global(owner, d.Parameter(symbolName))
      case elem: VariableElement =>
        Symbols.Global(owner, d.Term(symbolName))
      case elem: TypeParameterElement =>
        Symbols.Global(owner, d.TypeParameter(symbolName))
    }

    def displayName: String = symbolName

    def symbolName: String = elem match {
      case elem: PackageElement =>
        if (elem.isUnnamed) "_empty_"
        else {
          val fullName = elem.getSimpleName.toString
          fullName.substring(fullName.lastIndexOf('.') + 1)
        }
      case elem => elem.getSimpleName.toString
    }

    def kind: s.SymbolInformation.Kind = elem match {
      case elem: PackageElement => k.PACKAGE
      case elem: TypeElement =>
        elem.getKind match {
          case ElementKind.CLASS => k.CLASS
          case ElementKind.ENUM => k.CLASS
          case ElementKind.INTERFACE => k.INTERFACE
          case ElementKind.ANNOTATION_TYPE => k.INTERFACE
          case _ => sys.error(elem.toString)
        }
      case elem: ExecutableElement =>
        elem.getKind match {
          case ElementKind.CONSTRUCTOR => k.CONSTRUCTOR
          case ElementKind.METHOD => k.METHOD
          case _ => sys.error(elem.toString)
        }
      case elem: VariableElement if elem.getKind == ElementKind.PARAMETER => k.PARAMETER
      case elem: VariableElement => k.FIELD
      case elem: TypeParameterElement => k.TYPE_PARAMETER
    }

    def access: s.Access = {
      kind match {
        case k.LOCAL | k.PARAMETER | k.TYPE_PARAMETER | k.PACKAGE =>
          s.NoAccess
        case k.INTERFACE =>
          s.PublicAccess()
        case _ =>
          val mods = elem.getModifiers
          if (mods.contains(Modifier.PUBLIC)) s.PublicAccess()
          else if (mods.contains(Modifier.PRIVATE)) s.PrivateAccess()
          else if (mods.contains(Modifier.PROTECTED)) s.ProtectedAccess()
          else s.PrivateWithinAccess(enclosingPackage.sym)
      }
    }

    def properties: Int = {
      var prop = 0
      elem.getModifiers.asScala.foreach {
        case Modifier.STATIC => prop |= p.STATIC.value
        case Modifier.FINAL => prop |= p.FINAL.value
        case Modifier.ABSTRACT => prop |= p.ABSTRACT.value
        case _ =>
      }
      elem.getKind match {
        case ElementKind.ENUM | ElementKind.ENUM_CONSTANT => prop |= p.ENUM.value
        case ElementKind.INTERFACE => prop |= p.ABSTRACT.value
        case _ =>
      }
      if (elem.getKind == ElementKind.METHOD &&
          (prop & p.ABSTRACT.value) == 0 &&
          (prop & p.STATIC.value) == 0 &&
          elem.getEnclosingElement.getKind == ElementKind.INTERFACE) {
        prop |= p.DEFAULT.value
      }
      prop
    }

    def isSynthetic: Boolean = elem match {
      case elem: ExecutableElement => Set("<init>", "<clinit>").contains(elem.symbolName)
      case _ => false
    }

    def signature: s.Signature = {
      elem match {
        case elem: PackageElement => s.NoSignature
        case elem: ExecutableElement =>
          val returnType =
            if (elem.getKind == ElementKind.CONSTRUCTOR) s.NoType
            else elem.getReturnType.tpe
          val params = elem.paramElements.map(_.sym)
          val tparams = elem.typeParamElements.map(_.sym)
          s.MethodSignature(
            typeParameters = Some(s.Scope(symlinks = tparams)),
            parameterLists = Seq(s.Scope(symlinks = params)),
            returnType = returnType
          )
        case elem: TypeElement =>
          val parents = {
            val superclass = elem.getSuperclass
            val extendParent =
              if (superclass.getKind == TypeKind.NONE) List(ObjectType)
              else List(superclass.tpe)
            val implementationParents = elem.getInterfaces.asScala.map(_.tpe)
            extendParent ++ implementationParents
          }
          val decls = {
            val (synthetics, others) = elem.enclosedElements.partition(_.isSynthetic)
            (synthetics ++ others).map(_.sym)
          }
          val tparams = elem.typeParamElements.map(_.sym)
          s.ClassSignature(
            typeParameters = Some(s.Scope(symlinks = tparams)),
            parents = parents,
            declarations = Some(s.Scope(symlinks = decls))
          )
        case elem: TypeParameterElement =>
          val bounds = {
            val elemBounds =
              elem.getBounds.asScala.map(_.tpe)
            elemBounds match {
              case Seq() => ObjectType
              case Seq(b) => b
              case elemBounds => s.IntersectionType(elemBounds)
            }
          }
          s.TypeSignature(
            typeParameters = Some(s.Scope()),
            upperBound = bounds
          )
        case elem: VariableElement if elem.getKind == ElementKind.PARAMETER =>
          val parent = elem.getEnclosingElement.asInstanceOf[ExecutableElement]
          val tpe = {
            val parentParams =
              parent.getParameters.asInstanceOf[java.util.List[VariableElement]].asScala
            if (parent.isVarArgs && elem == parentParams.last) {
              val containedType = elem.asType().tpe match {
                case s.TypeRef(_, _, Seq(contained)) => contained
              }
              s.RepeatedType(containedType)
            } else elem.asType().tpe
          }
          s.ValueSignature(
            tpe = tpe
          )
        case elem: VariableElement =>
          s.ValueSignature(
            tpe = elem.asType().tpe
          )
        case _ => s.NoSignature
      }
    }

    def info: s.SymbolInformation = s.SymbolInformation(
      symbol = sym,
      language = s.Language.JAVA,
      kind = kind,
      displayName = displayName,
      annotations = annotations,
      access = access,
      properties = properties,
      signature = signature
    )

    def populateInfos(infos: mutable.ListBuffer[s.SymbolInformation]): s.SymbolInformation = {
      val myInfo = info
      infos += myInfo
      elem match {
        case elem: TypeElement =>
          elem.typeParamElements.foreach { elem =>
            elem.populateInfos(infos)
          }
          elem.enclosedElements.foreach { elem =>
            elem.populateInfos(infos)
          }
        case elem: ExecutableElement =>
          elem.typeParamElements.foreach { elem =>
            elem.populateInfos(infos)
          }
          elem.paramElements.foreach { elem =>
            elem.populateInfos(infos)
          }
        case _ =>
      }
      myInfo
    }

  }

  implicit class ExecutableElementOps(elem: ExecutableElement) {
    def typeParamElements: Seq[TypeParameterElement] = elem.getTypeParameters.asScala
    def paramElements: Seq[VariableElement] = elem.getParameters.asScala
  }

  implicit class TypeElementOps(elem: TypeElement) {
    def typeParamElements: Seq[TypeParameterElement] = elem.getTypeParameters.asScala
  }

}
