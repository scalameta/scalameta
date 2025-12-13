package scala.meta

import java.math.MathContext

import scala.math.ScalaNumber

case class FineDecimal(significand: BigInt, exponent: BigInt) extends ScalaNumber {

  override def isWhole: Boolean = exponent.signum >= 0
  override def underlying(): AnyRef = this
  override def intValue(): Int = toBigDecimal.intValue
  override def longValue(): Long = toBigDecimal.longValue
  override def floatValue(): Float = toBigDecimal.floatValue
  override def doubleValue(): Double = toBigDecimal.doubleValue

  // BigDecimal scale is the opposite of exponent
  def toBigDecimal: BigDecimal = BigDecimal(significand, -exponent.intValue, MathContext.UNLIMITED)

  def toBigDecimalOpt: Option[BigDecimal] = if (exponent.isValidInt) Some(toBigDecimal) else None

  override def toString: String =
    if (exponent.isValidInt) toBigDecimal.toString()
    else significand.toString() + "e" + exponent.toString()

}

object FineDecimal {

  def parse(str: String, radix: Int = 10): BigInt =
    if (str.isEmpty) BigInt(0) else BigInt(str, radix)

  def toBase36(obj: BigInt): String = if (obj.signum == 0) "" else obj.toString(Character.MAX_RADIX)
  def fromBase36(str: String): BigInt = parse(str, Character.MAX_RADIX)

}
