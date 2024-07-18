package scala.meta
package internal
package tokenizers

import java.math.MathContext
import java.math.RoundingMode

import scala.util.Try

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

  /**
   * Convert current strVal, base to an integer value This is tricky because of max negative value.
   */
  private def integerVal: Either[String, BigInt] =
    try Right(BigInt(strVal, base))
    catch { case e: Exception => Left(s"malformed integer number: ${e.getMessage}") }

  /**
   * Convert current strVal, base to double value
   */
  private def floatingVal: Either[String, BigDecimal] =
    try Right(BigDecimal(strVal))
    catch { case e: Exception => Left(s"malformed floating-point number: ${e.getMessage}") }

  // these values are always non-negative, since we don't include any unary operators
  def intVal: Either[String, BigInt] = integerVal
  def longVal: Either[String, BigInt] = integerVal
  def floatVal: Either[String, BigDecimal] = floatingVal.right.flatMap { value =>
    if (value <= LegacyTokenData.bigDecimalMaxFloat) Right(value)
    else Left("floating-point value out of range for Float")
  }
  def doubleVal: Either[String, BigDecimal] = floatingVal.right.flatMap { value =>
    if (value <= LegacyTokenData.bigDecimalMaxDouble) Right(value)
    else Left("floating-point value out of range for Double")
  }

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
