package scala.meta
package internal
package quasiquotes

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.meta.internal.ast.{Reflection => AstReflection}

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

class ConversionMacros(val c: Context) extends AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
  import u._
  import definitions._

  val MetaLift = symbolOf[scala.meta.quasiquotes.Lift[_, _]]
  val MetaUnlift = symbolOf[scala.meta.quasiquotes.Unlift[_, _]]

  private def foundReqMsg(found: c.Type, req: c.Type): String = {
    val g = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val msg = g.analyzer.foundReqMsg(found.asInstanceOf[g.Type], req.asInstanceOf[g.Type])
    // TODO: somehow, error reporting facilities are now printing
    // some types as `meta.XXX` and some types as `scala.meta.XXX`
    // find out why and fix this ugliness
    msg.replace("meta.", "scala.meta.").replace("scala.scala.", "scala.")
  }

  def liftApply[I](outside: c.Tree)(implicit I: c.WeakTypeTag[I]): c.Tree = {
    val outsideTpe = outside.tpe
    val insideTpe = I.tpe
    if (outsideTpe <:< insideTpe) {
      val needsCast = !(outsideTpe <:< insideTpe)
      if (needsCast) q"$outside.asInstanceOf[$insideTpe]"
      else outside
    } else {
      val liftable = c.inferImplicitValue(appliedType(MetaLift, outsideTpe, insideTpe), silent = true)
      if (liftable.nonEmpty) {
        q"$liftable.apply($outside)"
      } else {
        val errorMessage = "type mismatch when unquoting" + foundReqMsg(outsideTpe, insideTpe)
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
    q"$inside"
  }

  def unliftUnapply[O](inside: c.Tree)(implicit O: c.WeakTypeTag[O]): c.Tree = {
    val insideTpe = inside.tpe
    val outsideTpe = O.tpe
    if (insideTpe <:< outsideTpe) {
      q"_root_.scala.Some($inside: $insideTpe)"
    } else {
      val unliftable = c.inferImplicitValue(appliedType(MetaUnlift, insideTpe, outsideTpe), silent = true)
      if (unliftable.nonEmpty) {
        q"$unliftable.apply($inside)"
      } else {
        val errorMessage = "type mismatch when unquoting" + foundReqMsg(insideTpe, outsideTpe)
        c.abort(c.enclosingPosition, errorMessage)
      }
    }
  }
}

