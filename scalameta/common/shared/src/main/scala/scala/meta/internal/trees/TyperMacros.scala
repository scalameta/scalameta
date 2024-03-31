package scala.meta
package internal
package trees

import org.scalameta.adt.{Reflection => AdtReflection}
import org.scalameta.internal.MacroHelpers

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

// Parts of @root, @branch and @ast logic that need a typer context and can't be run in a macro annotation.
object CommonTyperMacros {
  def hierarchyCheck[T]: Unit = macro CommonTyperMacrosBundle.hierarchyCheck[T]
  def productPrefix[T]: String = macro CommonTyperMacrosBundle.productPrefix[T]
  def loadField[T](f: T, s: String): Unit = macro CommonTyperMacrosBundle.loadField
  def storeField[T](f: T, v: T, s: String): Unit = macro CommonTyperMacrosBundle.storeField
  def initField[T](f: T): T = macro CommonTyperMacrosBundle.initField
  def initParam[T](f: T): T = macro CommonTyperMacrosBundle.initField
  def children[T, U]: List[U] = macro CommonTyperMacrosBundle.children[T]
}

class CommonTyperMacrosBundle(val c: Context) extends AdtReflection with MacroHelpers {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._

  def hierarchyCheck[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    // NOTE: sealedness is turned off because we can't have @ast hierarchy sealed anymore
    // hopefully, in the future we'll find a way to restore sealedness
    checkHierarchy(T.tpe, c.abort(c.enclosingPosition, _), checkSealed = false)
    q"()"
  }

  def interfaceToApi[I, A](
      interface: c.Tree
  )(implicit I: c.WeakTypeTag[I], A: c.WeakTypeTag[A]): c.Tree = q"$interface.asInstanceOf[$A]"

  def productPrefix[T](implicit T: c.WeakTypeTag[T]): c.Tree = q"${T.tpe.typeSymbol.asLeaf.prefix}"

  def loadField(f: c.Tree, s: c.Tree): c.Tree = {
    val q"this.$finternalName" = f
    val fname = TermName(finternalName.toString.stripPrefix("_"))
    def lazyLoad(fn: c.Tree => c.Tree) = {
      val assertionMessage =
        s"internal error when initializing ${c.internal.enclosingOwner.owner.name}.$fname"
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
      q"$subtree.privateCopy(prototype = $subtree, parent = this, destination = null).asInstanceOf[$subtp]"
    }
    f.tpe.finalResultType match {
      case AnyTpe() => q"()"
      case PrimitiveTpe() => q"()"
      case tpe @ TreeTpe() => lazyLoad(pf => q"${copySubtree(pf, tpe)}")
      case OptionTreeTpe(tpe) => lazyLoad(pf => q"$pf.map(el => ${copySubtree(q"el", tpe)})")
      case ListTreeTpe(tpe) => lazyLoad(pf => q"$pf.map(el => ${copySubtree(q"el", tpe)})")
      case OptionListTreeTpe(tpe) =>
        lazyLoad(pf => q"$pf.map(_.map(el => ${copySubtree(q"el", tpe)}))")
      case ListListTreeTpe(tpe) =>
        lazyLoad(pf => q"$pf.map(_.map(el => ${copySubtree(q"el", tpe)}))")
      case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
    }
  }

  def storeField(f: c.Tree, v: c.Tree, s: c.Tree): c.Tree = {
    def copySubtree(subtree: c.Tree, subtp: c.Type) =
      q"$subtree.privateCopy(prototype = $subtree, parent = node, destination = $s).asInstanceOf[$subtp]"
    f.tpe.finalResultType match {
      case AnyTpe() => q"()"
      case PrimitiveTpe() => q"()"
      case tpe @ TreeTpe() => q"$f = ${copySubtree(v, tpe)}"
      case OptionTreeTpe(tpe) => q"$f = $v.map(el => ${copySubtree(q"el", tpe)})"
      case ListTreeTpe(tpe) => q"$f = $v.map(el => ${copySubtree(q"el", tpe)})"
      case OptionListTreeTpe(tpe) => q"$f = $v.map(_.map(el => ${copySubtree(q"el", tpe)}))"
      case ListListTreeTpe(tpe) => q"$f = $v.map(_.map(el => ${copySubtree(q"el", tpe)}))"
      case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
    }
  }
  def initField(f: c.Tree): c.Tree = f.tpe.finalResultType match {
    case AnyTpe() => q"$f"
    case PrimitiveTpe() => q"$f"
    case TreeTpe() => q"null"
    case OptionTreeTpe(tpe) => q"null"
    case ListTreeTpe(tpe) => q"null"
    case OptionListTreeTpe(tpe) => q"null"
    case ListListTreeTpe(tpe) => q"null"
    case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
  }

  def children[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    var streak = List[Tree]()
    def flushStreak(acc: Tree): Tree = {
      val result = if (acc.isEmpty) q"$streak" else q"$acc ++ $streak"
      streak = Nil
      result
    }
    val leaf = T.tpe.typeSymbol.asLeaf
    val allAnalyzedFields = leaf.fields
    val acc = allAnalyzedFields.foldLeft(q"": Tree)((acc, f) =>
      f.tpe match {
        case TreeTpe() =>
          streak :+= q"this.${f.sym}"
          acc
        case OptionTreeTpe(_) =>
          val acc1 = flushStreak(acc)
          q"$acc1 ++ this.${f.sym}.toList"
        case ListTreeTpe(_) =>
          val acc1 = flushStreak(acc)
          q"$acc1 ++ this.${f.sym}"
        case OptionListTreeTpe(_) =>
          val acc1 = flushStreak(acc)
          q"$acc1 ++ this.${f.sym}.getOrElse(_root_.scala.collection.immutable.Nil)"
        case ListListTreeTpe(_) =>
          val acc1 = flushStreak(acc)
          q"$acc1 ++ this.${f.sym}.flatten"
        case _ => acc
      }
    )
    flushStreak(acc)
  }
}
