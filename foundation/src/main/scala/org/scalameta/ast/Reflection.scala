package org.scalameta.ast

import scala.language.implicitConversions
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
  lazy val ImplEllipsisClass = mirror.staticClass("scala.meta.internal.ast.Ellipsis")
  lazy val ImplUnquoteClass = mirror.staticClass("scala.meta.internal.ast.Unquote")
  lazy val ApiNameQualifierClass = mirror.staticModule("scala.meta.Name").info.decl(TypeName("Qualifier")).asClass
  lazy val ApiStatClass = mirror.staticClass("scala.meta.Stat")
  lazy val ApiScopeClass = mirror.staticClass("scala.meta.Scope")
  lazy val PatClass = mirror.staticClass("scala.meta.Pat")
  lazy val PatTypeClass = mirror.staticModule("scala.meta.Pat").info.decl(TypeName("Type")).asClass
  lazy val PatTypeRefClass = mirror.staticModule("scala.meta.Pat").info.decl(TermName("Type")).info.decl(TypeName("Ref")).asClass

  private implicit class PrivateXtensionAstSymbol(sym: Symbol) {
    def isPublicTree = sym.isClass && (sym.asClass.toType <:< ApiTreeClass.toType) && !sym.isInternalTree
    def isInternalTree = sym.isClass && (sym.asClass.toType <:< ImplTreeClass.toType)
    def isBottomTree = sym == ImplEllipsisClass || sym == ImplUnquoteClass
    def weight = {
      val moveToRight = Set[Symbol](ApiNameQualifierClass, ApiStatClass, ApiScopeClass, PatTypeClass, PatTypeRefClass)
      val nudgeToRight = Set[Symbol](PatClass)
      if (moveToRight(sym)) 100
      else if (nudgeToRight(sym)) 1
      else 0
    }
  }

  implicit class XtensionAstType(tpe: Type) {
    def publish: Type = tpe.map({
      case TypeRef(_, sym, Nil) if sym.isBottomTree =>
        NothingTpe
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
