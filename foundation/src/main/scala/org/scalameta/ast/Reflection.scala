package org.scalameta.ast

import scala.language.implicitConversions
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.adt.{Reflection => AdtReflection}
import org.scalameta.ast.{Reflection => AstReflection}

trait Reflection extends AdtReflection {
  import u._
  import internal._
  import decorators._
  import definitions._

  lazy val ApiTreeClass = mirror.staticClass("scala.meta.Tree")
  lazy val ImplTreeClass = mirror.staticClass("scala.meta.internal.ast.Tree")
  lazy val ImplQuasiClass = mirror.staticClass("scala.meta.internal.ast.Quasi")
  lazy val ApiNameQualifierClass = mirror.staticModule("scala.meta.Name").info.decl(TypeName("Qualifier")).asClass
  lazy val ApiStatClass = mirror.staticClass("scala.meta.Stat")
  lazy val ApiScopeClass = mirror.staticClass("scala.meta.Scope")
  lazy val PatClass = mirror.staticClass("scala.meta.Pat")
  lazy val PatTypeClass = mirror.staticModule("scala.meta.Pat").info.decl(TypeName("Type")).asClass
  lazy val PatTypeRefClass = mirror.staticModule("scala.meta.Pat").info.decl(TermName("Type")).info.decl(TypeName("Ref")).asClass
  lazy val RegistryModule = mirror.staticModule("scala.meta.internal.ast.Registry")
  lazy val RegistryAnnotation = mirror.staticModule("org.scalameta.ast.internal").info.member(TypeName("registry")).asClass

  private implicit class PrivateXtensionAstSymbol(sym: Symbol) {
    def isPublicTree = sym.isClass && (sym.asClass.toType <:< ApiTreeClass.toType) && !sym.isInternalTree
    def isInternalTree = sym.isClass && (sym.asClass.toType <:< ImplTreeClass.toType)
    def isBottomTree = sym.isClass && (sym.asClass.toType <:< ImplQuasiClass.toType)
    def weight = {
      val moveToRight = Set[Symbol](ApiNameQualifierClass, ApiStatClass, ApiScopeClass, PatTypeClass, PatTypeRefClass)
      val nudgeToRight = Set[Symbol](PatClass)
      if (moveToRight(sym)) 100
      else if (nudgeToRight(sym)) 1
      else 0
    }
  }
  
  override protected def figureOutDirectSubclasses(sym: ClassSymbol): List[Symbol] = {
    def fail = sys.error(s"failed to figure out direct subclasses for ${sym.fullName}")
    if (sym.isSealed) sym.knownDirectSubclasses.toList.sortBy(_.fullName)
    else if (sym.baseClasses.contains(ApiTreeClass)) scalaMetaRegistry.getOrElse(sym, fail)
    else fail
  }

  // NOTE: this is supposed to map root/branch/ast classes to their direct subclasses
  private lazy val scalaMetaRegistry: Map[Symbol, List[Symbol]] = {
    RegistryModule.initialize.annotations match {
      case List(ann) if ann.tree.tpe =:= RegistryAnnotation.toType =>
        val q"new $_($_.$_[..$_](..${astPaths: List[String]}))" = ann.tree
        val astClasses = astPaths.map(astPath => {
          def locateModule(root: ModuleSymbol, parts: List[String]): ModuleSymbol = parts match {
            case Nil => root
            case head :: rest => locateModule(root.info.member(TermName(head)).asModule, rest)
          }
          val modulePath :+ className = astPath.split('.').toList
          locateModule(mirror.RootPackage, modulePath).info.member(TypeName(className)).asClass
        })
        val entireHierarchy = {
          var result = astClasses.flatMap(_.baseClasses.map(_.asClass))
          result = result.filter(sym => sym.toType <:< ApiTreeClass.toType)
          result = result.flatMap(sym => List(sym, sym.companion.info.member(TypeName("Quasi")).asClass))
          result :+= ImplQuasiClass
          result.distinct
        }
        val registry = mutable.Map[Symbol, List[Symbol]]()
        entireHierarchy.foreach(sym => registry(sym) = Nil)
        entireHierarchy.foreach(sym => {
          val parents = sym.info.asInstanceOf[ClassInfoType].parents.map(_.typeSymbol)
          val relevantParents = parents.filter(p => p.isClass && p.asClass.baseClasses.contains(ApiTreeClass))
          relevantParents.foreach(parent => registry(parent) :+= sym)
        })
        registry.toMap
      case _ =>
        sys.error("failed to figure out meta trees")
    }
  }

  implicit class XtensionAstType(tpe: Type) {
    def publish: Type = tpe.map({
      case TypeRef(_, sym, Nil) if sym.isBottomTree =>
        // TODO: I've no idea what this thing was for, so I'm putting a crasher here to figure it out
        ???
      case TypeRef(_, sym, Nil) if sym.isInternalTree =>
        val publicParents = sym.asClass.baseClasses.filter(_.isPublicTree)
        val minimalParents = publicParents.filter(p1 => !publicParents.exists(p2 => p1 != p2 && p2.asClass.toType <:< p1.asClass.toType))
        intersectionType(minimalParents.sortBy(_.weight).map(_.asClass.toType))
      case tpe =>
        tpe
    })
  }
  
  implicit class XtensionAstTree(tree: Tree) {
    def detectAst: List[String] = {
      object astClassDetector extends Traverser {
        val result = scala.collection.mutable.ListBuffer[String]()
        var module = "_root_"
        var inner = false
        trait Path
        object Path {
          private def path(s: String): Path = new Path { override def toString = s }
          implicit def nameToPath(name: Name): Path = path(name.decodedName.toString)
          implicit def reftreeToPath(tree: RefTree): Path = path(tree.toString) // TODO: call decodedName on all components of tree
        }
        def drilldown[T](current: Path, inner: Boolean)(op: => T) = {
          val savedModule = this.module
          val savedInner = this.inner
          this.module = module + "." + current
          this.inner = inner
          val result = op
          this.inner = savedInner
          this.module = savedModule
          result
        }
        override def traverse(tree: Tree): Unit = tree match {
          case PackageDef(pid, stats) =>
            if (pid.name == termNames.EMPTY_PACKAGE_NAME) super.traverse(tree)
            else drilldown(pid, inner = false)(super.traverse(tree))
          case ModuleDef(_, name, _) =>
            drilldown(name, inner = false)(super.traverse(tree))
          case ClassDef(Modifiers(_, _, anns), name, _, impl) =>
            if (anns.exists(_.toString == "new ast()")) {
              if (inner) sys.error("@ast classes can't be inner: " + name)
              val q"$_ class $_[..$_] $_(...$paramss) extends { ..$_ } with ..$parents { $_ => ..$_ }" = tree
              drilldown(name, inner = true)(result += module)
            }
            drilldown(name, inner = true)(super.traverse(tree))
          case _ =>
            super.traverse(tree)
        }
      }
      astClassDetector.traverse(tree)
      astClassDetector.result.toList
    }
  }
}
