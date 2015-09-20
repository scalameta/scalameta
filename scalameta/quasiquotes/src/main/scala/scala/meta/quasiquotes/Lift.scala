package scala.meta
package quasiquotes

import org.scalameta.convert._
import scala.annotation.implicitNotFound
import scala.meta.internal.{ast => impl}

@implicitNotFound(msg = "don't know how to lift ${O} into ${I}")
trait Lift[O, I] extends Convert[O, I]

object Lift {
  def apply[O, I](f: O => I): Lift[O, I] = new Lift[O, I] { def apply(x: O): I = f(x) }

  implicit def liftBool[O <: Boolean, I >: Lit]: Lift[O, I]  = Lift{ x => impl.Lit.Bool(x) }
  implicit def liftByte[O <: Byte, I >: Lit]: Lift[O, I]     = Lift{ x => impl.Lit.Byte(x) }
  implicit def liftShort[O <: Short, I >: Lit]: Lift[O, I]   = Lift{ x => impl.Lit.Short(x) }
  implicit def liftInt[O <: Int, I >: Lit]: Lift[O, I]       = Lift{ x => impl.Lit.Int(x) }
  implicit def liftLong[O <: Long, I >: Lit]: Lift[O, I]     = Lift{ x => impl.Lit.Long(x) }
  implicit def liftFloat[O <: Float, I >: Lit]: Lift[O, I]   = Lift{ x => impl.Lit.Float(x) }
  implicit def liftDouble[O <: Double, I >: Lit]: Lift[O, I] = Lift{ x => impl.Lit.Double(x) }
  implicit def liftChar[O <: Char, I >: Lit]: Lift[O, I]     = Lift{ x => impl.Lit.Char(x) }
  implicit def liftString[O <: String, I >: Lit]: Lift[O, I] = Lift{ x => impl.Lit.String(x) }
  implicit def liftSymbol[I >: Lit]: Lift[Symbol, I]         = Lift{ x => impl.Lit.Symbol(x) }
  implicit def liftNull[I >: Lit]: Lift[Null, I]             = Lift{ x => impl.Lit.Null() }
  implicit def liftUnit[I >: Lit]: Lift[Unit, I]             = Lift{ x => impl.Lit.Unit() }

  implicit def liftIdentity[O, I >: O]: Lift[O, I] = Lift { x => x }
  implicit def liftOption[O, I](implicit lift: Lift[O, I]): Lift[O, Option[I]] = Lift { x => Some(lift(x)) }
}
