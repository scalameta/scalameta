package scala.meta
package internal
package tokenizers

import scala.meta.tokens.Token

import java.math.{MathContext, RoundingMode}

class LegacyTokenData {

  import LegacyToken._

  /** the next token */
  var token: LegacyToken = EMPTY

  /** the offset of the first character of the current token */
  var offset: Offset = 0

  /** the offset past the last character of the current token */
  var endOffset: Offset = 0

  /** the string value of a literal or name of identifier */
  var strVal: String = _

  /** the base of a number */
  var base: Int = 0

  override def toString =
    s"{token = $token, position = [$offset,$endOffset), strVal = $strVal, base = $base}"

  /**
   * Convert current strVal to char value
   */
  def charVal: Char = if (strVal.isEmpty) 0 else strVal.charAt(0)

  private def toBigInt(prevToken: Token, maxBitLength: Int, what: String): Either[String, BigInt] =
    try {
      val value = BigInt(strVal, base)
      val isNegativeSign = prevToken match {
        case t: Token.Ident => t.text == "-"
        case _ => false
      }
      val adjustedValue = if (isNegativeSign) value.underlying().negate() else value.underlying()
      val ok = adjustedValue.bitLength() < maxBitLength + (if (base == 10) 0 else 1)
      if (ok) Right(value) else Left("integer number out of range for " + what)
    } catch { case _: Exception => Left(s"malformed integer $what number") }

  private def toBigDec(max: BigDecimal, what: String): Either[String, BigDecimal] =
    try {
      val value = BigDecimal(strVal)
      if (value <= max) Right(value) else Left("floating-point value out of range for " + what)
    } catch { case _: Exception => Left(s"malformed floating-point $what number") }

  // these values are always non-negative, since we don't include any unary operators
  def intVal(prevToken: Token): Either[String, BigInt] =
    toBigInt(prevToken, java.lang.Integer.SIZE, "Int")
  def longVal(prevToken: Token): Either[String, BigInt] =
    toBigInt(prevToken, java.lang.Long.SIZE, "Long")
  def floatVal: Either[String, BigDecimal] = toBigDec(LegacyTokenData.bigDecimalMaxFloat, "Float")
  def doubleVal: Either[String, BigDecimal] = toBigDec(LegacyTokenData.bigDecimalMaxDouble, "Double")

  def setIdentifier(ident: String, dialect: Dialect, check: Boolean = true)(
      fCheck: LegacyTokenData => Unit
  ): Unit = {
    strVal = ident
    token = IDENTIFIER
    if (check) kw2legacytoken.get(ident).foreach {
      case ENUM if !dialect.allowEnums =>
      case GIVEN if !dialect.allowGivenUsing =>
      case EXPORT if !dialect.allowExportClause =>
      case THEN if !dialect.allowQuietSyntax =>
      case TYPELAMBDAARROW if !dialect.allowTypeLambdas =>
      case CTXARROW if !dialect.allowGivenUsing =>
      case x =>
        token = x
        fCheck(this)
    }
  }

  private[tokenizers] def setInvalidToken(message: String): Unit = {
    token = INVALID
    strVal = message
  }

  @inline
  def ok: Boolean = token >= 0

}

object LegacyTokenData {
  // add a bit more, JS doesn't handle it well
  private val bigDecimalMaxFloat = BigDecimal
    .binary(Float.MaxValue, new MathContext(8, RoundingMode.UP))
  private val bigDecimalMaxDouble = BigDecimal
    .binary(Double.MaxValue, new MathContext(32, RoundingMode.UP))
}
