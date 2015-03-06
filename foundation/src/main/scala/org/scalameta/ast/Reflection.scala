package org.scalameta.ast

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
}
