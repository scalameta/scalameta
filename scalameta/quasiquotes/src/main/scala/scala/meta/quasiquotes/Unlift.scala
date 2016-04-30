package scala.meta
package quasiquotes

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scala.meta.common._

@implicitNotFound(msg = "don't know how to unlift ${I} into ${O}")
trait Unlift[I, O] extends Convert[I, Option[O]]

object Unlift {
  def apply[I, O](pf: PartialFunction[I, O]): Unlift[I, O] = new Unlift[I, O] { def apply(x: I): Option[O] = pf.lift(x) }

  implicit def unliftBool[I >: Lit]: Unlift[I, Boolean]  = Unlift{ case Lit(x: Boolean) => x }
  implicit def unliftByte[I >: Lit]: Unlift[I, Byte]     = Unlift{ case Lit(x: Byte) => x }
  implicit def unliftShort[I >: Lit]: Unlift[I, Short]   = Unlift{ case Lit(x: Short) => x }
  implicit def unliftInt[I >: Lit]: Unlift[I, Int]       = Unlift{ case Lit(x: Int) => x }
  implicit def unliftLong[I >: Lit]: Unlift[I, Long]     = Unlift{ case Lit(x: Long) => x }
  implicit def unliftFloat[I >: Lit]: Unlift[I, Float]   = Unlift{ case Lit(x: Float) => x }
  implicit def unliftDouble[I >: Lit]: Unlift[I, Double] = Unlift{ case Lit(x: Double) => x }
  implicit def unliftChar[I >: Lit]: Unlift[I, Char]     = Unlift{ case Lit(x: Char) => x }
  implicit def unliftString[I >: Lit]: Unlift[I, String] = Unlift{ case Lit(x: String) => x }
  implicit def unliftSymbol[I >: Lit]: Unlift[I, Symbol] = Unlift{ case Lit(x: Symbol) => x }
  implicit def unliftNull[I >: Lit]: Unlift[I, Null]     = Unlift{ case Lit(null) => null }
  implicit def unliftUnit[I >: Lit]: Unlift[I, Unit]     = Unlift{ case Lit(()) => () }

  // TODO: what are the dual of liftOption?
  implicit def unliftIdentity[I, O <: I : ClassTag]: Unlift[I, O] = Unlift { case x: O => x }
}
