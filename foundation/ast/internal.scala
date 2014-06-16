package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context

object internal {
  class astClass extends StaticAnnotation
  class astCompanion extends StaticAnnotation

  def productPrefix[T]: String = macro Macros.productPrefix[T]
  def loadField[T](f: T): Unit = macro Macros.loadField
  def storeField[T](f: T, v: T): Unit = macro Macros.storeField
  def initField[T](f: T): T = macro Macros.initField
  def initParam[T](f: T): T = macro Macros.initField

  class Macros(val c: Context) {
    import c.universe._
    import definitions._
    def productPrefix[T](implicit T: c.WeakTypeTag[T]) = {
      def loop(sym: Symbol): String = {
        if (sym.owner.isPackageClass) sym.name.toString
        else loop(sym.owner) + "." + sym.name.toString
      }
      q"${loop(T.tpe.typeSymbol)}"
    }
    def loadField(f: c.Tree) = {
      val q"this.$finternalName" = f
      def uncapitalize(s: String) = if (s.length == 0) "" else { val chars = s.toCharArray; chars(0) = chars(0).toLower; new String(chars) }
      val fname = TermName(finternalName.toString.stripPrefix("_"))
      def lazyLoad(fn: c.Tree => c.Tree) = {
        q"""
          if ($f == null) {
            scala.Predef.require(this.internalPrototype != null)
            $f = ${fn(q"this.internalPrototype.$fname")}
          }
        """
      }
      f.tpe.finalResultType match {
        case Primitive(tpe) => q""
        case Tree(tpe) => lazyLoad(pf => q"$pf.internalCopy(prototype = $pf, parent = this)")
        case OptionTree(tpe) => lazyLoad(pf => q"$pf.map(el => el.internalCopy(prototype = el, parent = this))")
        case SeqTree(tpe) => lazyLoad(pf => q"$pf.map(el => el.internalCopy(prototype = el, parent = this))")
        case SeqSeqTree(tpe) => lazyLoad(pf => q"$pf.map(_.map(el => el.internalCopy(prototype = el, parent = this)))")
      }
    }
    def storeField(f: c.Tree, v: c.Tree) = {
      f.tpe.finalResultType match {
        case Primitive(tpe) => q""
        case Tree(tpe) => q"$f = $v.internalCopy(prototype = $v, parent = node)"
        case OptionTree(tpe) => q"$f = $v.map(el => el.internalCopy(prototype = el, parent = node))"
        case SeqTree(tpe) => q"$f = $v.map(el => el.internalCopy(prototype = el, parent = node))"
        case SeqSeqTree(tpe) => q"$f = $v.map(_.map(el => el.internalCopy(prototype = el, parent = node)))"
        case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
      }
    }
    def initField(f: c.Tree) = {
      f.tpe.finalResultType match {
        case Primitive(tpe) => q"$f"
        case Tree(tpe) => q"null"
        case OptionTree(tpe) => q"null"
        case SeqTree(tpe) => q"null"
        case SeqSeqTree(tpe) => q"null"
        case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
      }
    }
    private object Primitive {
      def unapply(tpe: Type): Option[Type] = {
        if (tpe =:= typeOf[String] ||
            tpe =:= typeOf[scala.Symbol] ||
            ScalaPrimitiveValueClasses.contains(tpe.typeSymbol)) Some(tpe)
        else None
      }
    }
    private object Tree {
      def unapply(tpe: Type): Option[Type] = {
        if (tpe <:< c.mirror.staticClass("scala.meta.Tree").asType.toType) Some(tpe)
        else None
      }
    }
    private object SeqTree {
      def unapply(tpe: Type): Option[Type] = {
        if (tpe.typeSymbol == c.mirror.staticClass("scala.collection.immutable.Seq")) {
          tpe.typeArgs match {
            case Tree(tpe) :: Nil => Some(tpe)
            case _ => None
          }
        } else None
      }
    }
    private object SeqSeqTree {
      def unapply(tpe: Type): Option[Type] = {
        if (tpe.typeSymbol == c.mirror.staticClass("scala.collection.immutable.Seq")) {
          tpe.typeArgs match {
            case SeqTree(tpe) :: Nil => Some(tpe)
            case _ => None
          }
        } else None
      }
    }
    private object OptionTree {
      def unapply(tpe: Type): Option[Type] = {
        if (tpe.typeSymbol == c.mirror.staticClass("scala.Option")) {
          tpe.typeArgs match {
            case Tree(tpe) :: Nil => Some(tpe)
            case _ => None
          }
        } else None
      }
    }
  }
}
