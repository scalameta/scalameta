package scala.meta
package quasiquotes

import scala.meta.common._
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to lift ${O} into ${I}")
trait Lift[O, I] extends Convert[O, I]

object Lift {
  def apply[O, I](f: O => I): Lift[O, I] = new Lift[O, I] { def apply(x: O): I = f(x) }

  implicit def liftBoolean[O <: Boolean, I >: Lit]: Lift[O, I] = Lift{ x => Lit.Boolean(x) }
  implicit def liftByte[O <: Byte, I >: Lit]: Lift[O, I]       = Lift{ x => Lit.Byte(x) }
  implicit def liftShort[O <: Short, I >: Lit]: Lift[O, I]     = Lift{ x => Lit.Short(x) }
  implicit def liftInt[O <: Int, I >: Lit]: Lift[O, I]         = Lift{ x => Lit.Int(x) }
  implicit def liftLong[O <: Long, I >: Lit]: Lift[O, I]       = Lift{ x => Lit.Long(x) }
  implicit def liftFloat[O <: Float, I >: Lit]: Lift[O, I]     = Lift{ x => Lit.Float(x) }
  implicit def liftDouble[O <: Double, I >: Lit]: Lift[O, I]   = Lift{ x => Lit.Double(x) }
  implicit def liftChar[O <: Char, I >: Lit]: Lift[O, I]       = Lift{ x => Lit.Char(x) }
  implicit def liftString[O <: String, I >: Lit]: Lift[O, I]   = Lift{ x => Lit.String(x) }
  implicit def liftSymbol[I >: Lit]: Lift[Symbol, I]           = Lift{ x => Lit.Symbol(x) }
  implicit def liftNull[I >: Lit]: Lift[Null, I]               = Lift{ x => Lit.Null(x) }
  implicit def liftUnit[I >: Lit]: Lift[Unit, I]               = Lift{ x => Lit.Unit(x) }

  implicit def liftIdentity[O, I >: O]: Lift[O, I] = Lift { x => x }
  implicit def liftAnyToOption[O, I](implicit lift: Lift[O, I]): Lift[O, Option[I]] = Lift { x => Some(lift(x)) }
  implicit def liftSomeToList[O, I](implicit lift: Lift[O, I]): Lift[Some[O], List[I]] = Lift { _.toList.map(x => lift(x)) }
  implicit def liftNoneToList[O, I](implicit lift: Lift[O, I]): Lift[None.type, List[I]] = Lift { _ => Nil }
  implicit def liftOptionToList[O, I](implicit lift: Lift[O, I]): Lift[Option[O], List[I]] = Lift { _.toList.map(x => lift(x)) }
}
