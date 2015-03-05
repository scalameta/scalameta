package scala.meta

import org.scalameta.convert._
import scala.annotation.implicitNotFound
import scala.meta.internal.{ast => impl}

@implicitNotFound(msg = "don't know how to lift ${T} into ${U}")
trait Liftable[T, U] extends Convert[T, U]

object Liftable extends StandardLiftables {
  def apply[T, U](f: T => U): Liftable[T, U] = new Liftable[T, U] { def apply(x: T): U = f(x) }
}

private[meta] trait StandardLiftables {
  implicit def liftBool[T <: Boolean, U >: Lit]: Liftable[T, U]  = Liftable(x => impl.Lit.Bool(x))
  implicit def liftByte[T <: Byte, U >: Lit]: Liftable[T, U]     = Liftable(x => impl.Lit.Byte(x))
  implicit def liftShort[T <: Short, U >: Lit]: Liftable[T, U]   = Liftable(x => impl.Lit.Short(x))
  implicit def liftInt[T <: Int, U >: Lit]: Liftable[T, U]       = Liftable(x => impl.Lit.Int(x))
  implicit def liftLong[T <: Long, U >: Lit]: Liftable[T, U]     = Liftable(x => impl.Lit.Long(x))
  implicit def liftFloat[T <: Float, U >: Lit]: Liftable[T, U]   = Liftable(x => impl.Lit.Float(x))
  implicit def liftDouble[T <: Double, U >: Lit]: Liftable[T, U] = Liftable(x => impl.Lit.Double(x))
  implicit def liftChar[T <: Char, U >: Lit]: Liftable[T, U]     = Liftable(x => impl.Lit.Char(x))
  implicit def liftString[T <: String, U >: Lit]: Liftable[T, U] = Liftable(x => impl.Lit.String(x))
  implicit def liftSymbol[U >: Lit]: Liftable[Symbol, U]         = Liftable(x => impl.Lit.Symbol(x))
  implicit def liftNull[U >: Lit]: Liftable[Null, U]             = Liftable(x => impl.Lit.Null())
  implicit def liftUnit[U >: Lit]: Liftable[Unit, U]             = Liftable(x => impl.Lit.Unit())
}
