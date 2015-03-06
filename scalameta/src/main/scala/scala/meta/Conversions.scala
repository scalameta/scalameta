package scala.meta

import org.scalameta.convert._
import scala.annotation.implicitNotFound
import scala.meta.internal.{ast => impl}

@implicitNotFound(msg = "don't know how to lift ${O} into ${I}")
trait Liftable[O, I] extends Convert[O, I]

object Liftable {
  def apply[O, I](f: O => I): Liftable[O, I] = new Liftable[O, I] { def apply(x: O): I = f(x) }

  implicit def liftBool[O <: Boolean, I >: Lit]: Liftable[O, I]  = Liftable{ x => impl.Lit.Bool(x) }
  implicit def liftByte[O <: Byte, I >: Lit]: Liftable[O, I]     = Liftable{ x => impl.Lit.Byte(x) }
  implicit def liftShort[O <: Short, I >: Lit]: Liftable[O, I]   = Liftable{ x => impl.Lit.Short(x) }
  implicit def liftInt[O <: Int, I >: Lit]: Liftable[O, I]       = Liftable{ x => impl.Lit.Int(x) }
  implicit def liftLong[O <: Long, I >: Lit]: Liftable[O, I]     = Liftable{ x => impl.Lit.Long(x) }
  implicit def liftFloat[O <: Float, I >: Lit]: Liftable[O, I]   = Liftable{ x => impl.Lit.Float(x) }
  implicit def liftDouble[O <: Double, I >: Lit]: Liftable[O, I] = Liftable{ x => impl.Lit.Double(x) }
  implicit def liftChar[O <: Char, I >: Lit]: Liftable[O, I]     = Liftable{ x => impl.Lit.Char(x) }
  implicit def liftString[O <: String, I >: Lit]: Liftable[O, I] = Liftable{ x => impl.Lit.String(x) }
  implicit def liftSymbol[I >: Lit]: Liftable[Symbol, I]         = Liftable{ x => impl.Lit.Symbol(x) }
  implicit def liftNull[I >: Lit]: Liftable[Null, I]             = Liftable{ x => impl.Lit.Null() }
  implicit def liftUnit[I >: Lit]: Liftable[Unit, I]             = Liftable{ x => impl.Lit.Unit() }
}

@implicitNotFound(msg = "don't know how to unlift ${I} into ${O}")
trait Unliftable[I, O] extends Convert[I, Option[O]]

object Unliftable {
  def apply[I, O](pf: PartialFunction[I, O]): Unliftable[I, O] = new Unliftable[I, O] { def apply(x: I): Option[O] = pf.lift(x) }

  implicit def unliftBool[I >: Lit]: Unliftable[I, Boolean]  = Unliftable{ case impl.Lit.Bool(x) => x }
  implicit def unliftByte[I >: Lit]: Unliftable[I, Byte]     = Unliftable{ case impl.Lit.Byte(x) => x }
  implicit def unliftShort[I >: Lit]: Unliftable[I, Short]   = Unliftable{ case impl.Lit.Short(x) => x }
  implicit def unliftInt[I >: Lit]: Unliftable[I, Int]       = Unliftable{ case impl.Lit.Int(x) => x }
  implicit def unliftLong[I >: Lit]: Unliftable[I, Long]     = Unliftable{ case impl.Lit.Long(x) => x }
  implicit def unliftFloat[I >: Lit]: Unliftable[I, Float]   = Unliftable{ case impl.Lit.Float(x) => x }
  implicit def unliftDouble[I >: Lit]: Unliftable[I, Double] = Unliftable{ case impl.Lit.Double(x) => x }
  implicit def unliftChar[I >: Lit]: Unliftable[I, Char]     = Unliftable{ case impl.Lit.Char(x) => x }
  implicit def unliftString[I >: Lit]: Unliftable[I, String] = Unliftable{ case impl.Lit.String(x) => x }
  implicit def unliftSymbol[I >: Lit]: Unliftable[I, Symbol] = Unliftable{ case impl.Lit.Symbol(x) => x }
  implicit def unliftNull[I >: Lit]: Unliftable[I, Null]     = Unliftable{ case impl.Lit.Null() => null }
  implicit def unliftUnit[I >: Lit]: Unliftable[I, Unit]     = Unliftable{ case impl.Lit.Unit() => () }
}
