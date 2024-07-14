package scala.meta
package internal
package tokenizers

import scala.meta.inputs._

import java.math.MathContext
import java.math.RoundingMode

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
  private def integerVal(implicit reporter: Reporter): BigInt =
    try BigInt(strVal, base)
    catch {
      case e: Exception => reporter
          .syntaxError(s"malformed integer number: ${e.getMessage}", at = offset)
    }

  /**
   * Convert current strVal, base to double value
   */
  private def floatingVal(implicit reporter: Reporter): BigDecimal =
    try BigDecimal(strVal)
    catch {
      case e: Exception => reporter
          .syntaxError(s"malformed floating-point number: ${e.getMessage}", at = offset)
    }

  // these values are always non-negative, since we don't include any unary operators
  def intVal(implicit reporter: Reporter): BigInt = integerVal
  def longVal(implicit reporter: Reporter): BigInt = integerVal
  def floatVal(implicit reporter: Reporter): BigDecimal = {
    val value = floatingVal
    if (value > LegacyTokenData.bigDecimalMaxFloat) reporter
      .syntaxError("floating-point value out of range for Float", offset)
    value
  }
  def doubleVal(implicit reporter: Reporter): BigDecimal = {
    val value = floatingVal
    if (value > LegacyTokenData.bigDecimalMaxDouble) reporter
      .syntaxError("floating-point value out of range for Double", offset)
    value
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
}

object LegacyTokenData {
  // add a bit more, JS doesn't handle it well
  private val bigDecimalMaxFloat = BigDecimal
    .binary(Float.MaxValue, new MathContext(8, RoundingMode.UP))
  private val bigDecimalMaxDouble = BigDecimal
    .binary(Double.MaxValue, new MathContext(32, RoundingMode.UP))
}
