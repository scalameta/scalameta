package scala.meta
package internal
package trees

import org.scalameta.adt.{Reflection => AdtReflection}
import org.scalameta.internal.{AdtHelpers, MacroHelpers}

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
  def childrenCount[T]: Int = macro CommonTyperMacrosBundle.childrenCount[T]
  def foreachChild[T, U](f: U => Unit): Unit = macro CommonTyperMacrosBundle.foreachChild[T]
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
      interface: c.Tree,
  )(implicit I: c.WeakTypeTag[I], A: c.WeakTypeTag[A]): c.Tree = q"$interface.asInstanceOf[$A]"

  def productPrefix[T](implicit T: c.WeakTypeTag[T]): c.Tree = q"${T.tpe.typeSymbol.asLeaf.prefix}"

  def loadField(f: c.Tree, s: c.Tree): c.Tree = {
    def lazyLoad(fn: c.Tree => c.Tree) = {
      val q"this.$finternalName" = f
      val ownerName = c.internal.enclosingOwner.owner.name
      val externalName = AdtHelpers.getterName(finternalName.toString)
      val assertionMessage = s"internal error when initializing $ownerName.$externalName"
      q"""
        if ($f == null) {
          // there's not much sense in using org.scalameta.invariants.require here
          // because when the assertion trips, the tree is most likely in inconsistent state
          // which will either lead to useless printouts or maybe even worse errors
          _root_.scala.Predef.require(this.privatePrototype != null, $assertionMessage)
          $f = ${fn(q"this.privatePrototype.${TermName(externalName)}")}
        }
      """
    }
    def copySubtree(subtree: c.Tree, subtp: c.Type) =
      q"$subtree.loadFieldForParent(parent = this).asInstanceOf[$subtp]"
    f.tpe.finalResultType match {
      case AnyTpe() => q"()"
      case PrimitiveTpe() => q"()"
      case tpe @ TreeTpe() => lazyLoad(pf => q"${copySubtree(pf, tpe)}")
      case OptionTreeTpe(tpe) => lazyLoad(pf => q"$pf.map(el => ${copySubtree(q"el", tpe)})")
      case ListTreeTpe(tpe) => lazyLoad(pf => q"$pf.map(el => ${copySubtree(q"el", tpe)})")
      case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
    }
  }

  def storeField(f: c.Tree, v: c.Tree, s: c.Tree): c.Tree = {
    def copySubtree(subtree: c.Tree, subtp: c.Type) =
      q"$subtree.storeFieldInParent(parent = node, destination = $s).asInstanceOf[$subtp]"
    f.tpe.finalResultType match {
      case AnyTpe() => q"()"
      case PrimitiveTpe() => q"()"
      case tpe @ TreeTpe() => q"$f = ${copySubtree(v, tpe)}"
      case OptionTreeTpe(tpe) => q"$f = $v.map(el => ${copySubtree(q"el", tpe)})"
      case ListTreeTpe(tpe) => q"$f = $v.map(el => ${copySubtree(q"el", tpe)})"
      case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
    }
  }
  def initField(f: c.Tree): c.Tree = f.tpe.finalResultType match {
    case AnyTpe() => q"$f"
    case PrimitiveTpe() => q"$f"
    case TreeTpe() => q"null"
    case OptionTreeTpe(tpe) => q"null"
    case ListTreeTpe(tpe) => q"null"
    case tpe => c.abort(c.enclosingPosition, s"unsupported field type $tpe")
  }

  // Sum of: one per single-Tree field (folded into a constant), plus `.size`
  // for each Option[Tree] field and `.length` for each List[Tree] field. No
  // collection is materialized -- lets a caller pre-size before foreachChild.
  def childrenCount[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    var base = 0
    val terms = T.tpe.typeSymbol.asLeaf.fields.flatMap(f =>
      f.tpe match {
        case TreeTpe() =>
          base += 1
          None
        case OptionTreeTpe(_) => Some(q"this.${f.sym}.size")
        case ListTreeTpe(_) => Some(q"this.${f.sym}.length")
        case _ => None
      },
    )
    terms.foldLeft(q"$base": Tree)((acc, t) => q"$acc + $t")
  }

  // Applies `f` to each child in field order, visiting Option/List fields
  // in place -- allocation-free (no intermediate List, unlike `children`).
  def foreachChild[T](f: c.Tree)(implicit T: c.WeakTypeTag[T]): c.Tree = {
    val stmts = T.tpe.typeSymbol.asLeaf.fields.flatMap(field =>
      field.tpe match {
        case TreeTpe() => Some(q"$f.apply(this.${field.sym})")
        case OptionTreeTpe(_) => Some(q"this.${field.sym}.foreach($f)")
        case ListTreeTpe(_) => Some(q"this.${field.sym}.foreach($f)")
        case _ => None
      },
    )
    q"{ ..$stmts; () }"
  }
}
