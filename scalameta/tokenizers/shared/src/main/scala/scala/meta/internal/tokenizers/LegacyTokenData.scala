package scala.meta
package internal
package tokenizers

import java.math.MathContext
import java.math.RoundingMode

import scala.meta.inputs._

trait LegacyTokenData {

  import LegacyToken._

  /** the input that is currently being tokenized */
  var input: Input = null

  /** the next token */
  var token: LegacyToken = EMPTY

  /** the offset of the first character of the current token */
  var offset: Offset = 0

  /** the offset of the character following the token preceding this one */
  var lastOffset: Offset = 0

  /** the offset of the last character of the current token */
  var endOffset: Offset = 0

  /** the name of an identifier */
  var name: String = _

  /** the string value of a literal */
  var strVal: String = _

  /** the base of a number */
  var base: Int = 0

  def copyFrom(td: LegacyTokenData): this.type = {
    this.input = td.input
    this.token = td.token
    this.offset = td.offset
    this.lastOffset = td.lastOffset
    this.endOffset = td.endOffset
    this.name = td.name
    this.strVal = td.strVal
    this.base = td.base
    this
  }

  override def toString =
    s"{token = $token, position = $offset..$endOffset, lastOffset = $lastOffset, name = $name, strVal = $strVal, base = $base}"

  lazy val reporter: Reporter = Reporter(input)
  import reporter._

  /**
   * Convert current strVal to char value
   */
  def charVal: Char = if (strVal.isEmpty) 0 else strVal.charAt(0)

  /**
   * Convert current strVal, base to an integer value This is tricky because of max negative value.
   */
  private def integerVal: BigInt = {
    try BigInt(strVal, base)
    catch {
      case e: Exception => syntaxError(s"malformed integer number: ${e.getMessage}", at = offset)
    }
  }

  /**
   * Convert current strVal, base to double value
   */
  private def floatingVal: BigDecimal = {
    try BigDecimal(strVal)
    catch {
      case e: Exception =>
        syntaxError(s"malformed floating-point number: ${e.getMessage}", at = offset)
    }
  }

  // these values are always non-negative, since we don't include any unary operators
  def intVal: BigInt = integerVal
  def longVal: BigInt = integerVal
  def floatVal: BigDecimal = {
    val value = floatingVal
    if (value > LegacyTokenData.bigDecimalMaxFloat)
      syntaxError("floating-point value out of range for Float", offset)
    value
  }
  def doubleVal: BigDecimal = {
    val value = floatingVal
    if (value > LegacyTokenData.bigDecimalMaxDouble)
      syntaxError("floating-point value out of range for Double", offset)
    value
  }

  def setIdentifier(ident: String, dialect: Dialect, check: Boolean = true)(
      fCheck: LegacyTokenData => Unit
  ): Unit = {
    name = ident
    token = IDENTIFIER
    if (check) kw2legacytoken.get(name).foreach {
      case ENUM if !dialect.allowEnums =>
      case GIVEN if !dialect.allowGivenUsing =>
      case EXPORT if !dialect.allowExportClause =>
      case THEN if !dialect.allowSignificantIndentation =>
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
