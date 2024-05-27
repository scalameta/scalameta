package scala.meta.internal.trees

import scala.meta.tokens.Token

private[meta] sealed trait Unary {
  def op: String
}

private[meta] object Unary {

  private val numericOpMap = Seq[Numeric](Plus, Minus, Tilde).map(unaryWithKey).toMap
  val opMap = numericOpMap ++ Seq[Unary](Not).map(unaryWithKey)

  private def unaryWithKey[T <: Unary](unary: T) = unary.op -> unary

  def unapply(token: Token.Ident): Option[(String, Unary)] = {
    val op = token.text
    opMap.get(op).map(op -> _)
  }

  sealed trait Numeric extends Unary {
    def apply(value: BigInt): BigInt
    // could return None if not applicable (such as `~`)
    def apply(value: BigDecimal): Option[BigDecimal]
  }

  object Numeric {
    def unapply(token: Token.Ident): Option[Numeric] = numericOpMap.get(token.text)
  }

  sealed trait Logical extends Unary {
    def apply(value: Boolean): Boolean
  }

  case object Noop extends Numeric {
    val op = ""
    def apply(value: BigInt): BigInt = value
    def apply(value: BigDecimal): Option[BigDecimal] = Some(value)
  }

  case object Plus extends Numeric {
    val op = "+"
    def apply(value: BigInt): BigInt = value
    def apply(value: BigDecimal): Option[BigDecimal] = Some(value)
  }

  case object Minus extends Numeric {
    val op = "-"
    def apply(value: BigInt): BigInt = -value
    def apply(value: BigDecimal): Option[BigDecimal] = Some(-value)
  }

  case object Tilde extends Numeric {
    val op = "~"
    def apply(value: BigInt): BigInt = ~value
    def apply(value: BigDecimal): Option[BigDecimal] = None
  }

  case object Not extends Logical {
    val op = "!"
    def apply(value: Boolean): Boolean = !value
  }

}
