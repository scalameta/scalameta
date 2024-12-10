package scala.meta
package internal
package quasiquotes

import scala.compat.Platform.EOL
import scala.language.experimental.macros
import scala.quoted._
import scala.meta.internal.trees.{Reflection => AstReflection}

// Note copied from the Scala 2 counterpart file.
// NOTE: we don't have the signature as [O, I] to keep symmetry with Unlift
object Lift {
  transparent inline def apply[I](inline outside: Any): I = ${ConversionMacros.liftApplyImpl[I]('outside)}
  transparent inline def unapply[I](inline outside: Any): Option[I] = ${ConversionMacros.liftUnapplyImpl[I]('outside)}
}

// Note copied from the Scala 2 counterpart file.
// NOTE: here we can't have the signature be [I, O], because we never know I
// in the case of Unlift.apply, we've just assembled the reified result and don't know its type yet
// in the case of Unlift.unapply, we only know the expected type of the unquote, not its actual type
// it would be nice if Scala supported partially provided type argument lists
object Unlift {
  transparent inline def apply[O](inline inside: Any): O = ${ConversionMacros.unliftApplyImpl[O]('inside)}
  transparent inline def unapply[O](inline inside: Any): Option[O] = ${ConversionMacros.unliftUnapplyImpl[O]('inside)}
}

object ConversionMacros {
  def liftApplyImpl[I: Type](using Quotes)(outside: Expr[Any]) = new ConversionMacros(using quotes).liftApply[I](outside)
  def liftUnapplyImpl[I: Type](using Quotes)(outside: Expr[Any]) = new ConversionMacros(using quotes).liftUnapply[I](outside)

  def unliftApplyImpl[O: Type](using Quotes)(inside: Expr[Any]) = new ConversionMacros(using quotes).unliftApply[O](inside)
  def unliftUnapplyImpl[O: Type](using Quotes)(inside: Expr[Any]) = new ConversionMacros(using quotes).unliftUnapply[O](inside)
}

class ConversionMacros(using val topLevelQuotes: Quotes) {//extends AstReflection {
  import topLevelQuotes.reflect._
  import scala.meta.quasiquotes.{Lift, Unlift}

  private def typeMismatchMessage(found: TypeRepr, req: TypeRepr): String = {
    val foundReqMessage = found.show + ", required " + req.show
    "type mismatch when unquoting " + foundReqMessage
  }

  def liftApply[I: Type](outside: Expr[Any]): Expr[I] = {
    outside match
      case '{$outsideExpr: o} => 
        val outsideTpe = TypeRepr.of[o].widen
        val insideTpe = TypeRepr.of[I]
        if (outsideTpe <:< insideTpe) {
          outside.asExprOf[I]
        } else {
          outsideTpe.asType match
            case '[t] =>
              val liftableMaybe = Expr.summon[Lift[t, I]]
              liftableMaybe match
                case Some(liftable) => '{$liftable.apply(${outsideExpr.asExprOf[t]})}
                case None => report.errorAndAbort(typeMismatchMessage(outsideTpe, insideTpe))
        }
  }

  def liftUnapply[I: Type](outside: Expr[Any]): Expr[Option[I]] = {
    // Note copied from the Scala 2 counterpart file.
    // NOTE: Here's an interesting idea that I'd like to explore.
    // How about we allow things like `42 match { case q"$x" => x }`?
    // For that to work, we just need to wrap the reification result into `Lift.unapply`!
    ???
  }

  def unliftApply[O: Type](inside: Expr[Any]): Expr[O] = {
    // Note copied from the Scala 2 counterpart file.
    // NOTE: here we just disregard the expected outside type, because I can't find uses for it
    // duality is a fun thing, but it looks like here it just led me into a dead-end
    inside.asExprOf[O]
  }

  def unliftUnapply[O: Type](inside: Expr[Any]): Expr[Option[O]] = {
    inside match
      case '{$insideExpr: i} =>
        val insideTpe = TypeRepr.of[i].widen
        val outsideTpe = TypeRepr.of[O]
        if (insideTpe <:< outsideTpe) {
          '{Some(${inside.asExprOf[O]})}
        } else {
          val unliftableMaybe = Expr.summon[Unlift[i, O]]
          unliftableMaybe match
            case Some(unliftable) => '{$unliftable.apply($insideExpr)}
            case None => report.errorAndAbort(typeMismatchMessage(insideTpe, outsideTpe))
        }
  }
}
