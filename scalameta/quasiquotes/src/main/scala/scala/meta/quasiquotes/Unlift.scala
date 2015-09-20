package scala.meta
package quasiquotes

import org.scalameta.convert._
import scala.annotation.implicitNotFound
import scala.meta.internal.{ast => impl}

@implicitNotFound(msg = "don't know how to unlift ${I} into ${O}")
trait Unlift[I, O] extends Convert[I, Option[O]]

object Unlift {
  def apply[I, O](pf: PartialFunction[I, O]): Unlift[I, O] = new Unlift[I, O] { def apply(x: I): Option[O] = pf.lift(x) }

  implicit def unliftBool[I >: Lit]: Unlift[I, Boolean]  = Unlift{ case impl.Lit.Bool(x) => x }
  implicit def unliftByte[I >: Lit]: Unlift[I, Byte]     = Unlift{ case impl.Lit.Byte(x) => x }
  implicit def unliftShort[I >: Lit]: Unlift[I, Short]   = Unlift{ case impl.Lit.Short(x) => x }
  implicit def unliftInt[I >: Lit]: Unlift[I, Int]       = Unlift{ case impl.Lit.Int(x) => x }
  implicit def unliftLong[I >: Lit]: Unlift[I, Long]     = Unlift{ case impl.Lit.Long(x) => x }
  implicit def unliftFloat[I >: Lit]: Unlift[I, Float]   = Unlift{ case impl.Lit.Float(x) => x }
  implicit def unliftDouble[I >: Lit]: Unlift[I, Double] = Unlift{ case impl.Lit.Double(x) => x }
  implicit def unliftChar[I >: Lit]: Unlift[I, Char]     = Unlift{ case impl.Lit.Char(x) => x }
  implicit def unliftString[I >: Lit]: Unlift[I, String] = Unlift{ case impl.Lit.String(x) => x }
  implicit def unliftSymbol[I >: Lit]: Unlift[I, Symbol] = Unlift{ case impl.Lit.Symbol(x) => x }
  implicit def unliftNull[I >: Lit]: Unlift[I, Null]     = Unlift{ case impl.Lit.Null() => null }
  implicit def unliftUnit[I >: Lit]: Unlift[I, Unit]     = Unlift{ case impl.Lit.Unit() => () }

  // TODO: what are the duals of liftIdentity and liftOption?
}
