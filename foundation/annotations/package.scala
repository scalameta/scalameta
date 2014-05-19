package org.scalareflect

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context

package object annotations {
  object internal {
    class ast extends StaticAnnotation
    object ast {
      def productPrefix[T]: String = macro AstHelperMacros.productPrefix[T]
      def loadField[T](f: T): Unit = macro AstHelperMacros.loadField
      def storeField[T](f: T, v: T): Unit = macro AstHelperMacros.storeField
      def initField[T](f: T): T = macro AstHelperMacros.initField
      def initParam[T](f: T): T = macro AstHelperMacros.initField

      class AstHelperMacros(val c: Context) {
        import c.universe._
        import definitions._
        def parentIsImmutable(x: c.Tree) = {
          c.abort(c.enclosingPosition, s"parent can't be set directly. Insert a tree into another tree to have its parent updated automatically.")
        }
        def payloadIsImmutable(x: c.Tree) = {
          val q"$_.$setterName(...$args)" = c.macroApplication
          val fieldName = setterName.toString.stripSuffix("_$eq")
          val withName = "with" + fieldName.capitalize
          val mapName = "map" + fieldName.capitalize
          c.abort(c.enclosingPosition, s"trees are immutable, so $fieldName can't be mutated. Use $withName or $mapName to create a copy of the tree with $fieldName updated.")
        }
        def scratchpadIsImmutable(x: c.Tree)(h: c.Tree) = {
          c.abort(c.enclosingPosition, s"trees are immutable, so scratchpad can't be mutated. Use withScratchpad or mapScratchpad to create a copy of the tree with scratchpad updated.")
        }
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
          val fname = TermName(uncapitalize(finternalName.toString.stripPrefix("internal")))
          def lazyLoad(fn: c.Tree => c.Tree) = {
            q"""
              if ($f == null) {
                scala.Predef.require(this.prototype != null)
                $f = ${fn(q"this.prototype.$fname")}
              }
            """
          }
          f.tpe.finalResultType match {
            case Primitive(tpe) => q""
            case Tree(tpe) => lazyLoad(pf => q"$pf.internalWithParent(this)")
            case OptionTree(tpe) => lazyLoad(pf => q"$pf.map(_.internalWithParent(this))")
            case SeqTree(tpe) => lazyLoad(pf => q"$pf.map(_.internalWithParent(this))")
            case SeqSeqTree(tpe) => lazyLoad(pf => q"$pf.map(_.map(_.internalWithParent(this)))")
          }
        }
        def storeField(f: c.Tree, v: c.Tree) = {
          f.tpe.finalResultType match {
            case Primitive(tpe) => q""
            case Tree(tpe) => q"$f = $v.internalWithParent(node)"
            case OptionTree(tpe) => q"$f = $v.map(_.internalWithParent(node))"
            case SeqTree(tpe) => q"$f = $v.map(_.internalWithParent(node))"
            case SeqSeqTree(tpe) => q"$f = $v.map(_.map(_.internalWithParent(node)))"
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
            if (tpe <:< c.mirror.staticClass("scala.reflect.core.Tree").asType.toType) Some(tpe)
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
    class contextful[T] extends StaticAnnotation
    class hosted(macroApi: Boolean, mayFail: Boolean) extends StaticAnnotation
  }
}
