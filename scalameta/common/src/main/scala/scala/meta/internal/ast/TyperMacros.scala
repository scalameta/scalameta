package scala.meta
package internal
package ast

import scala.language.experimental.macros
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.macros.blackbox.Context
import org.scalameta.adt.{Reflection => AdtReflection}
import org.scalameta.internal.MacroHelpers

// Parts of @root, @branch and @ast logic that need a typer context and can't be run in a macro annotation.
object CommonTyperMacros {
  def hierarchyCheck[T]: Unit = macro CommonTyperMacrosBundle.hierarchyCheck[T]
  def productPrefix[T]: String = macro CommonTyperMacrosBundle.productPrefix[T]
  def loadField[T](f: T): Unit = macro CommonTyperMacrosBundle.loadField
  def storeField[T](f: T, v: T): Unit = macro CommonTyperMacrosBundle.storeField
  def initField[T](f: T): T = macro CommonTyperMacrosBundle.initField
  def initParam[T](f: T): T = macro CommonTyperMacrosBundle.initField
  def children[T, U]: Seq[U] = macro CommonTyperMacrosBundle.children[T]
}

class CommonTyperMacrosBundle(val c: Context) extends AdtReflection with MacroHelpers {
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
        bsym == symbolOf[Object] ||
        bsym == symbolOf[Any] ||
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
    def copySubtree(subtree: c.Tree, subtp: c.Type) = {
      val tempName = c.freshName(TermName("copy" + fname.toString.capitalize))
      q"""
        val $tempName = $subtree.privateCopy(prototype = $subtree, parent = this).asInstanceOf[$subtp]
        if (this.privatePrototype.isTypechecked != this.isTypechecked) $tempName.withTypechecked(this.isTypechecked)
        else $tempName
      """
    }
    f.tpe.finalResultType match {
      case AnyTpe(tpe) => q"()"
      case PrimitiveTpe(tpe) => q"()"
      case TreeTpe(tpe) => lazyLoad(pf => q"${copySubtree(pf, tpe)}")
      case OptionTreeTpe(tpe) => lazyLoad(pf => q"$pf.map(el => ${copySubtree(q"el", tpe)})")
      case SeqTreeTpe(tpe) => lazyLoad(pf => q"$pf.map(el => ${copySubtree(q"el", tpe)})")
      case OptionSeqTreeTpe(tpe) => lazyLoad(pf => q"$pf.map(_.map(el => ${copySubtree(q"el", tpe)}))")
      case SeqSeqTreeTpe(tpe) => lazyLoad(pf => q"$pf.map(_.map(el => ${copySubtree(q"el", tpe)}))")
    }
  }

  def storeField(f: c.Tree, v: c.Tree): c.Tree = {
    def copySubtree(subtree: c.Tree, subtp: c.Type) = {
      q"$subtree.privateCopy(prototype = $subtree, parent = node).asInstanceOf[$subtp]"
    }
    f.tpe.finalResultType match {
      case AnyTpe(tpe) => q"()"
      case PrimitiveTpe(tpe) => q"()"
      case TreeTpe(tpe) => q"$f = ${copySubtree(v, tpe)}"
      case OptionTreeTpe(tpe) => q"$f = $v.map(el => ${copySubtree(q"el", tpe)})"
      case SeqTreeTpe(tpe) => q"$f = $v.map(el => ${copySubtree(q"el", tpe)})"
      case OptionSeqTreeTpe(tpe) => q"$f = $v.map(_.map(el => ${copySubtree(q"el", tpe)}))"
      case SeqSeqTreeTpe(tpe) => q"$f = $v.map(_.map(el => ${copySubtree(q"el", tpe)}))"
      case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
    }
  }
  def initField(f: c.Tree): c.Tree = {
    f.tpe.finalResultType match {
      case AnyTpe(tpe) => q"$f"
      case PrimitiveTpe(tpe) => q"$f"
      case TreeTpe(tpe) => q"null"
      case OptionTreeTpe(tpe) => q"null"
      case SeqTreeTpe(tpe) => q"null"
      case OptionSeqTreeTpe(tpe) => q"null"
      case SeqSeqTreeTpe(tpe) => q"null"
      case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
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
      case TreeTpe(_) =>
        streak :+= q"this.${f.sym}"
        acc
      case OptionTreeTpe(_) =>
        val acc1 = flushStreak(acc)
        q"$acc1 ++ this.${f.sym}.toList"
      case SeqTreeTpe(_) =>
        val acc1 = flushStreak(acc)
        q"$acc1 ++ this.${f.sym}"
      case OptionSeqTreeTpe(_) =>
        val acc1 = flushStreak(acc)
        q"$acc1 ++ this.${f.sym}.getOrElse(_root_.scala.collection.immutable.Nil)"
      case SeqSeqTreeTpe(_) =>
        val acc1 = flushStreak(acc)
        q"$acc1 ++ this.${f.sym}.flatten"
      case _ =>
        acc
    })
    flushStreak(acc)
  }
}
