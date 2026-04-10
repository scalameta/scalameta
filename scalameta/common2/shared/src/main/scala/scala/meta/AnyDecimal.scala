package scala.meta

import java.math.{BigDecimal => JBigDec, MathContext}

case class AnyDecimal(digits: String, exponent: String) {

  override def toString: String = if (exponent.isEmpty) digits else digits + 'e' + exponent

  def toBigDecimalOpt: Option[BigDecimal] = {
    val expOpt =
      if (exponent.isEmpty) Some(0)
      else if (exponent.length > 11) None // Int.MaxValue contains 10 decimal digits, and sign
      else {
        val lexp = exponent.toLong
        val iexp = lexp.toInt
        if (iexp != lexp) None else Some(iexp)
      }
    expOpt.flatMap { exp =>
      val mc = MathContext.UNLIMITED
      val partial = new JBigDec(digits, mc)
      def res(bd: JBigDec) = Some(new BigDecimal(bd, mc))
      if (exp == 0) {
        val normalized = if (partial.scale().abs <= 1) partial else partial.stripTrailingZeros()
        res(normalized)
      } else {
        val totalScale = partial.scale() - exp.toLong
        if (totalScale.toInt != totalScale) None // overflows
        else res(partial.movePointRight(exp).stripTrailingZeros())
      }
    }
  }

}
