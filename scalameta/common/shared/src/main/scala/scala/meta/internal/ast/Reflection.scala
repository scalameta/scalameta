package scala.meta
package internal
package ast

import scala.language.implicitConversions
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.adt.{Reflection => AdtReflection}
import scala.meta.internal.ast.{Reflection => AstReflection}

trait Reflection extends AdtReflection {
  import u._
  import internal._
  import decorators._
  import definitions._
  import Flag._

  def Protected: Modifiers = Modifiers(PROTECTED)
  def PrivateMeta: Modifiers = PrivateMeta(NoFlags)
  def PrivateMeta(flags: FlagSet): Modifiers = Modifiers(flags, TypeName("meta"), Nil)

  lazy val TreeSymbol = mirror.staticClass("scala.meta.Tree")
  lazy val QuasiSymbol = mirror.staticClass("scala.meta.internal.ast.Quasi")
  lazy val AllModule = mirror.staticModule("scala.meta.internal.ast.All")
  lazy val RegistryAnnotation = mirror.staticModule("scala.meta.internal.ast.Metadata").info.member(TypeName("registry")).asClass

  override protected def figureOutDirectSubclasses(sym: ClassSymbol): List[Symbol] = {
    def fail = sys.error(s"failed to figure out direct subclasses for ${sym.fullName}")
    if (sym.isSealed) sym.knownDirectSubclasses.toList.sortBy(_.fullName)
    else if (sym.baseClasses.contains(TreeSymbol)) scalaMetaRegistry.getOrElse(sym, fail)
    else fail
  }

  // NOTE: this is supposed to map root/branch/ast classes to their direct subclasses
  private lazy val scalaMetaRegistry: Map[Symbol, List[Symbol]] = {
    AllModule.initialize.annotations match {
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
          result = result.filter(sym => sym.toType <:< TreeSymbol.toType)
          result = result.flatMap(sym => List(sym, sym.companion.info.member(TypeName("Quasi")).asClass))
          result :+= QuasiSymbol
          result.distinct
        }
        val registry = mutable.Map[Symbol, List[Symbol]]()
        entireHierarchy.foreach(sym => registry(sym) = Nil)
        entireHierarchy.foreach(sym => {
          val parents = sym.info.asInstanceOf[ClassInfoType].parents.map(_.typeSymbol)
          val relevantParents = parents.filter(p => p.isClass && p.asClass.baseClasses.contains(TreeSymbol))
          relevantParents.foreach(parent => registry(parent) :+= sym)
        })
        registry.toMap
      case _ =>
        sys.error("failed to figure out meta trees")
    }
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
