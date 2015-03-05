package scala.meta
package internal
package quasiquotes

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.compat.Platform.EOL
import org.scalameta.ast.{Reflection => AstReflection}
import org.scalameta.invariants._
import org.scalameta.unreachable

// NOTE: we don't have the signature as [O, I] to keep symmetry with Unlift
object Lift {
  def apply[I](outside: Any): I = macro ConversionMacros.liftApply[I]
  def unapply[I](outside: Any): Option[I] = macro ConversionMacros.liftUnapply[I]
}

// NOTE: here we can't have the signature be [I, O], because we never know I
// in the case of Unlift.apply, we've just assembled the reified result and don't know its type yet
// in the case of Unlift.unapply, we only know the expected type of the unquote, not its actual type
// it would be nice if Scala supported partially provided type argument lists
object Unlift {
  def apply[O](inside: Any): O = macro ConversionMacros.unliftApply[O]
  def unapply[O](inside: Any): Option[O] = macro ConversionMacros.unliftUnapply[O]
}

private[meta] class ConversionMacros(val c: Context) extends AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
  import u._
  import definitions._

  val MetaLiftable = symbolOf[scala.meta.Liftable[_, _]]

  def liftApply[I](outside: c.Tree)(implicit I: c.WeakTypeTag[I]): c.Tree = {
    val insideTpe = I.tpe
    if (outside.tpe <:< insideTpe.publish) {
      val needsCast = !(outside.tpe <:< insideTpe)
      if (needsCast) q"$outside.asInstanceOf[$insideTpe]"
      else outside
    } else {
      val liftable = c.inferImplicitValue(appliedType(MetaLiftable, outside.tpe, insideTpe.publish), silent = true)
      if (liftable.nonEmpty) {
        val lifted = q"$liftable.apply($outside)"
        val needsCast = !(insideTpe.publish =:= insideTpe)
        if (needsCast) q"$lifted.asInstanceOf[$insideTpe]"
        else lifted
      } else {
        val errorMessage = s"type mismatch when unquoting;$EOL found   : ${outside.tpe}$EOL required: ${insideTpe.publish}"
        c.abort(c.enclosingPosition, errorMessage)
      }
    }
  }

  def liftUnapply[I](outside: c.Tree)(implicit I: c.WeakTypeTag[I]): c.Tree = {
    // TODO: Here's an interesting idea that I'd like to explore.
    // How about we allow things like `42 match { case q"$x" => x }`?
    // For that to work, we just need to wrap the reification result into `Lift.unapply`!
    // NOTE: also see the TODO in the last lines in ReificationMacros.reifySkeleton
    ???
  }

  def unliftApply[O](inside: c.Tree)(implicit O: c.WeakTypeTag[O]): c.Tree = {
    // TODO: here we just disregard the expected outside type, because I can't find uses for it
    // duality is a fun thing, but it looks like here it just led me into a dead-end
    q"$inside: ${inside.tpe.publish}"
  }

  def unliftUnapply[O](inside: c.Tree)(implicit O: c.WeakTypeTag[O]): c.Tree = {
    // TODO: this macro is implemented differently from other extractor macros
    // because it's called in a very unusual hacky way, which needs to be fixed
    inside match {
      case pq"$name: $tpt" =>
        val insideTpe = {
          try c.typecheck(tpt, c.TYPEmode, silent = false)
          catch { case c.TypecheckException(pos, msg) => c.abort(pos.asInstanceOf[c.Position], msg) }
        }
        val outsideTpe = O.tpe
        sys.error(s"not yet implemented: Unlift.unapply[$insideTpe, $outsideTpe]")
      case other =>
        other
    }
  }
}

