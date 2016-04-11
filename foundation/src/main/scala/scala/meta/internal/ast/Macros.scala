package scala.meta
package internal
package ast

import scala.language.experimental.macros
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.macros.blackbox.Context
import org.scalameta.adt.{Reflection => AdtReflection}
import org.scalameta.internal.MacroHelpers

object AstTyperMacros {
  def hierarchyCheck[T]: Unit = macro AstTyperMacrosBundle.hierarchyCheck[T]
  def productPrefix[T]: String = macro AstTyperMacrosBundle.productPrefix[T]
  def loadField[T](f: T): Unit = macro AstTyperMacrosBundle.loadField
  def storeField[T](f: T, v: T): Unit = macro AstTyperMacrosBundle.storeField
  def initField[T](f: T): T = macro AstTyperMacrosBundle.initField
  def initParam[T](f: T): T = macro AstTyperMacrosBundle.initField
  def children[T, U]: Seq[U] = macro AstTyperMacrosBundle.children[T]
}

class AstTyperMacrosBundle(val c: Context) extends AdtReflection with MacroHelpers {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._
  import c.internal._
  import decorators._
  import definitions._

  def hierarchyCheck[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    val sym = T.tpe.typeSymbol.asClass
    val designation = if (sym.isRoot) "root" else if (sym.isBranch) "branch" else if (sym.isLeaf) "leaf" else "unknown"
    val roots = sym.baseClasses.filter(_.isRoot)
    if (roots.length == 0 && sym.isLeaf) c.abort(c.enclosingPosition, s"rootless leaf is disallowed")
    else if (roots.length > 1) c.abort(c.enclosingPosition, s"multiple roots for a $designation: " + (roots.map(_.fullName).init.mkString(", ")) + " and " + roots.last.fullName)
    val root = roots.headOption.getOrElse(NoSymbol)
    sym.baseClasses.map(_.asClass).foreach{bsym =>
      val exempt =
        bsym.isModuleClass ||
        bsym == ObjectClass ||
        bsym == AnyClass ||
        bsym == symbolOf[scala.Serializable] ||
        bsym == symbolOf[java.io.Serializable] ||
        bsym == symbolOf[scala.Product] ||
        bsym == symbolOf[scala.Equals] ||
        root.info.baseClasses.contains(bsym)
      if (!exempt && !bsym.isRoot && !bsym.isBranch && !bsym.isLeaf) c.abort(c.enclosingPosition, s"outsider parent of a $designation: ${bsym.fullName}")
      // NOTE: sealedness is turned off because we can't have @ast hierarchy sealed anymore
      // hopefully, in the future we'll find a way to restore sealedness
      // if (!exempt && !bsym.isSealed && !bsym.isFinal) c.abort(c.enclosingPosition, s"unsealed parent of a $designation: ${bsym.fullName}")
    }
    q"()"
  }

  def interfaceToApi[I, A](interface: c.Tree)(implicit I: c.WeakTypeTag[I], A: c.WeakTypeTag[A]): c.Tree = {
    q"$interface.asInstanceOf[$A]"
  }

  def productPrefix[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    q"${T.tpe.typeSymbol.asLeaf.prefix}"
  }

  def loadField(f: c.Tree): c.Tree = {
    val q"this.$finternalName" = f
    val fname = TermName(finternalName.toString.stripPrefix("_"))
    def lazyLoad(fn: c.Tree => c.Tree) = {
      val assertionMessage = s"internal error when initializing ${c.internal.enclosingOwner.owner.name}.$fname"
      q"""
        if ($f == null) {
          // there's not much sense in using org.scalameta.invariants.require here
          // because when the assertion trips, the tree is most likely in inconsistent state
          // which will either lead to useless printouts or maybe even worse errors
          _root_.scala.Predef.require(this.privatePrototype != null, $assertionMessage)
          $f = ${fn(q"this.privatePrototype.$fname")}
        }
      """
    }
    def copySubtree(subtree: c.Tree) = {
      val tempName = c.freshName(TermName("copy" + fname.toString.capitalize))
      q"""
        val $tempName = $subtree.privateCopy(prototype = $subtree, parent = this)
        if (this.privatePrototype.isTypechecked != this.isTypechecked) $tempName.withTypechecked(this.isTypechecked)
        else $tempName
      """
    }
    f.tpe.finalResultType match {
      case Any(tpe) => q"()"
      case Primitive(tpe) => q"()"
      case Tree(tpe) => lazyLoad(pf => q"${copySubtree(pf)}")
      case OptionTree(tpe) => lazyLoad(pf => q"$pf.map(el => ${copySubtree(q"el")})")
      case OptionSeqTree(tpe) => lazyLoad(pf => q"$pf.map(_.map(el => ${copySubtree(q"el")}))")
      case SeqTree(tpe) => lazyLoad(pf => q"$pf.map(el => ${copySubtree(q"el")})")
      case SeqSeqTree(tpe) => lazyLoad(pf => q"$pf.map(_.map(el => ${copySubtree(q"el")}))")
    }
  }

  def storeField(f: c.Tree, v: c.Tree): c.Tree = {
    def copySubtree(subtree: c.Tree) = {
      q"$subtree.privateCopy(prototype = $subtree, parent = node)"
    }
    f.tpe.finalResultType match {
      case Any(tpe) => q"()"
      case Primitive(tpe) => q"()"
      case Tree(tpe) => q"$f = ${copySubtree(v)}"
      case OptionTree(tpe) => q"$f = $v.map(el => ${copySubtree(q"el")})"
      case OptionSeqTree(tpe) => q"$f = $v.map(_.map(el => ${copySubtree(q"el")}))"
      case SeqTree(tpe) => q"$f = $v.map(el => ${copySubtree(q"el")})"
      case SeqSeqTree(tpe) => q"$f = $v.map(_.map(el => ${copySubtree(q"el")}))"
      case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
    }
  }
  def initField(f: c.Tree): c.Tree = {
    f.tpe.finalResultType match {
      case Any(tpe) => q"$f"
      case Primitive(tpe) => q"$f"
      case Tree(tpe) => q"null"
      case OptionTree(tpe) => q"null"
      case OptionSeqTree(tpe) => q"null"
      case SeqTree(tpe) => q"null"
      case SeqSeqTree(tpe) => q"null"
      case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
    }
  }

  private object Any {
    def unapply(tpe: Type): Option[Type] = {
      if (tpe =:= AnyTpe) Some(tpe)
      else None
    }
  }
  private object Primitive {
    def unapply(tpe: Type): Option[Type] = {
      if (tpe =:= typeOf[String] ||
          tpe =:= typeOf[scala.Symbol] ||
          ScalaPrimitiveValueClasses.contains(tpe.typeSymbol)) Some(tpe)
      else if (tpe.typeSymbol == OptionClass && Primitive.unapply(tpe.typeArgs.head).nonEmpty) Some(tpe)
      else if (tpe.typeSymbol == ClassClass) Some(tpe)
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
  private object OptionSeqTree {
    def unapply(tpe: Type): Option[Type] = {
      if (tpe.typeSymbol == c.mirror.staticClass("scala.Option")) {
        tpe.typeArgs match {
          case SeqTree(tpe) :: Nil => Some(tpe)
          case _ => None
        }
      } else None
    }
  }

  def children[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    var streak = List[Tree]()
    def flushStreak(acc: Tree): Tree = {
      val result = if (acc.isEmpty) q"$streak" else q"$acc ++ $streak"
      streak = Nil
      result
    }
    val acc = T.tpe.typeSymbol.asLeaf.fields.foldLeft(q"": Tree)((acc, f) => f.tpe match {
      case Tree(_) =>
        streak :+= q"this.${f.sym}"
        acc
      case SeqTree(_) =>
        val acc1 = flushStreak(acc)
        q"$acc1 ++ this.${f.sym}"
      case SeqSeqTree(_) =>
        val acc1 = flushStreak(acc)
        q"$acc1 ++ this.${f.sym}.flatten"
      case OptionTree(_) =>
        val acc1 = flushStreak(acc)
        q"$acc1 ++ this.${f.sym}.toList"
      case OptionSeqTree(_) =>
        val acc1 = flushStreak(acc)
        q"$acc1 ++ this.${f.sym}.getOrElse(_root_.scala.collection.immutable.Nil)"
      case _ =>
        acc
    })
    flushStreak(acc)
  }
}
