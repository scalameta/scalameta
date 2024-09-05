package scala.meta.internal.parsers

import scala.meta.classifiers._
import scala.meta.tokens.Token

sealed trait SepRegion {
  def indent = -1
  def isIndented: Boolean
}

sealed trait CanProduceLF

sealed trait SepRegionIndented extends SepRegion with CanProduceLF {
  override final def isIndented = true
}
sealed trait SepRegionNonIndented extends SepRegion {
  override final def isIndented = false
}

// this describes delimiters (indent, parens, braces, brackets)
sealed trait RegionDelim extends SepRegion
// this describes delimiters which are non-indented
sealed trait RegionDelimNonIndented extends SepRegionNonIndented with RegionDelim
// this describes non-delimiters which are also non-indented (likely all of them)
sealed trait RegionNonDelimNonIndented extends SepRegionNonIndented

case class RegionIndent(override val indent: Int) extends SepRegionIndented with RegionDelim

case class RegionLine(override val indent: Int) extends RegionNonDelimNonIndented with CanProduceLF

case object RegionParen extends RegionDelimNonIndented
case object RegionBracket extends RegionDelimNonIndented
case class RegionBrace(override val indent: Int) extends RegionDelimNonIndented with CanProduceLF

case object RegionCaseMark extends RegionNonDelimNonIndented
final class RegionCaseExpr(override val indent: Int) extends SepRegionNonIndented
final class RegionCaseBody(override val indent: Int, val arrow: Token)
    extends SepRegionNonIndented with CanProduceLF

sealed trait RegionDefDecl extends RegionNonDelimNonIndented
// initial mark of a definition (before colon)
case object RegionDefMark extends RegionDefDecl
// the type part of a definition (between colon and optional equals)
case object RegionDefType extends RegionDefDecl

/** region hierarchy to mark packages, classes, etc which can use `colon-eol` before template */
sealed trait RegionTemplateDecl extends RegionNonDelimNonIndented

/** the initial part of declaration, before the template */
case object RegionTemplateMark extends RegionTemplateDecl with CanProduceLF

/** the initial part of the template, containing any inherit clauses */
case object RegionTemplateInherit extends RegionTemplateDecl

/**
 * this marks the template body (or constructs which look like a template body, such as extensions).
 * for instance, helps override handling of `case` designed for catch/match/partial function but
 * inappropriate for enum.
 */
case object RegionTemplateBody extends RegionNonDelimNonIndented

/** this marks the initial part of extension */
case object RegionExtensionMark extends RegionNonDelimNonIndented

/**
 * All control statements
 */
sealed trait RegionControl extends RegionNonDelimNonIndented with CanProduceLF {
  def isControlKeyword(token: Token): Boolean
  def isTerminatingToken(token: Token): Boolean
  def isTerminatingTokenRequired(): Boolean

  /**
   * In cases when we encounter a terminating token, the control region is completed.
   *
   * However, in cases when it's optional, some other token could also possibly terminate the
   * control region, and this method allows dealing with that special case.
   */
  final def isNotTerminatingTokenIfOptional(token: Token): Boolean =
    !isTerminatingTokenRequired() && !isTerminatingToken(token)
}

/*
 * The next three regions describe an initial part of an `if` or `while` control statement.
 * These statements can have the following cases:
 * - scala3-style condition: one which doesn't start with an opening parenthesis and therefore
 *   requires a terminating `then` or `do`
 * - scala2-style condition: starts with an opening parenthesis, doesn't use a terminating token
 * - scala3-style condition which _looks_ like scala2: starts with an opening parenthesis but
 *   continues the condition after the closing and requires a terminating token
 */

/**
 * Describes a scala3-style condition of an `if` or `while` which requires a terminating token.
 */
sealed trait RegionControlCond extends RegionControl {
  final def isTerminatingTokenRequired(): Boolean = true
}

/**
 * Describes the initial part of an `if` or `while` condition that started with an opening
 * parenthesis, before the matching closing parenthesis has been encountered.
 */
sealed trait RegionControlMaybeCond extends RegionControl {
  final def isTerminatingTokenRequired(): Boolean = false
  def asCond(): RegionControlCond
  def asCondOrBody(): RegionControlMaybeBody
  def asBody(): Option[RegionControl]
}

/**
 * Describes the part of an `if` or `while` whose condition started with an opening parenthesis,
 * after encountering the matching closing parenthesis; it might still continue a scala3-style
 * condition or describe the body instead.
 */
sealed trait RegionControlMaybeBody extends RegionControl {
  final def isTerminatingTokenRequired(): Boolean = false
}

sealed trait RegionWhile extends RegionControl {
  def isControlKeyword(token: Token): Boolean = token.is[Token.KwWhile]
  def isTerminatingToken(token: Token): Boolean = token.is[Token.KwDo]
}
object RegionWhile {
  def apply(next: Token): RegionWhile =
    if (next.is[Token.LeftParen]) RegionWhileMaybeCond else RegionWhileCond
}
case object RegionWhileCond extends RegionControlCond with RegionWhile
case object RegionWhileMaybeCond extends RegionControlMaybeCond with RegionWhile {
  def asCond(): RegionControlCond = RegionWhileCond
  def asCondOrBody(): RegionControlMaybeBody = RegionWhileMaybeBody
  def asBody(): Option[RegionControl] = None
}
case object RegionWhileMaybeBody extends RegionControlMaybeBody with RegionWhile

sealed trait RegionIf extends RegionControl {
  def isControlKeyword(token: Token): Boolean = token.is[Token.KwIf]
  def isTerminatingToken(token: Token): Boolean = token.isAny[Token.KwThen, Token.KwElse]
}
object RegionIf {
  def apply(next: Token): RegionIf =
    if (next.is[Token.LeftParen]) RegionIfMaybeCond else RegionIfCond
}
case object RegionIfCond extends RegionControlCond with RegionIf {
  override def isTerminatingToken(token: Token): Boolean = token.is[Token.KwThen]
}
case object RegionIfMaybeCond extends RegionControlMaybeCond with RegionIf {
  def asCond(): RegionControlCond = RegionIfCond
  def asCondOrBody(): RegionControlMaybeBody = RegionIfMaybeBody
  def asBody(): Option[RegionControl] = Some(RegionThen)
}
case object RegionIfMaybeBody extends RegionControlMaybeBody with RegionIf

/**
 * Describes the body of an `if` statement, with a possible `else` following.
 */
case object RegionThen extends RegionControl {
  def isControlKeyword(token: Token): Boolean = token.is[Token.KwThen]
  def isTerminatingToken(token: Token): Boolean = token.is[Token.KwElse]
  def isTerminatingTokenRequired(): Boolean = false
}

object RegionFor {
  def apply(next: Token): RegionFor = next match {
    case _: Token.LeftParen => RegionForMaybeParens
    case _: Token.LeftBrace => RegionForBraces
    case _ => RegionForOther
  }
}
sealed trait RegionFor extends RegionControl {
  def isControlKeyword(token: Token): Boolean = token.is[Token.KwFor]
  def isTerminatingToken(token: Token): Boolean = token.isAny[Token.KwDo, Token.KwYield]
  def isClosingConditionToken(token: Token): Boolean
}
case object RegionForMaybeParens extends RegionFor {
  def isTerminatingTokenRequired(): Boolean = false
  def isClosingConditionToken(token: Token): Boolean = false
}
case object RegionForParens extends RegionFor {
  def isTerminatingTokenRequired(): Boolean = false
  def isClosingConditionToken(token: Token): Boolean = token.is[Token.RightParen]
}
case object RegionForBraces extends RegionFor {
  def isTerminatingTokenRequired(): Boolean = false
  def isClosingConditionToken(token: Token): Boolean = token.is[Token.RightBrace]
}
case object RegionForOther extends RegionFor {
  def isTerminatingTokenRequired(): Boolean = true
  def isClosingConditionToken(token: Token): Boolean = false
}

case object RegionTry extends SepRegionNonIndented with CanProduceLF
