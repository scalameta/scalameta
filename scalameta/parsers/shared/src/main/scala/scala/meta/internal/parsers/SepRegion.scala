package scala.meta.internal.parsers

import scala.meta.classifiers._
import scala.meta.tokens.Token

sealed trait SepRegion {
  def indent = -1
  def width: Int = indent
  def isIndented: Boolean
}

sealed trait CanProduceLF

sealed trait SepRegionIndented extends SepRegion with CanProduceLF {
  override final def isIndented = true
}
sealed trait SepRegionNonIndented extends SepRegion {
  override final def isIndented = false
}

/** Records an open construct but its end is determined by the enclosing region */
sealed trait RegionDeferring extends SepRegion

// this describes delimiters (indent, parens, braces, brackets)
sealed trait RegionDelim extends SepRegion
// this describes delimiters which are non-indented
sealed trait RegionDelimNonIndented extends SepRegionNonIndented with RegionDelim
sealed trait RegionDelimIndented extends SepRegionIndented with RegionDelim
// this describes non-delimiters which are also non-indented (likely all of them)
sealed trait RegionNonDelimNonIndented extends SepRegionNonIndented

class RegionIndent(override val indent: Int) extends RegionDelimIndented

/** The indented region of a `case` on the line of its `catch`. As in dotty, it holds one case. */
final class RegionCatchSameLine(override val indent: Int) extends RegionDelimIndented

/**
 * A lambda after a colon that ends on its line, with the indentation of its context. As dotty's
 * `ENDlambda`, it ends at the first line end outside the delimiters, cases and conditions in it.
 */
final class RegionColonLambda(override val indent: Int) extends SepRegionIndented

case class RegionLine(override val indent: Int)
    extends RegionNonDelimNonIndented with CanProduceLF with RegionDeferring

case class RegionParen(override val indent: Int) extends RegionDelimNonIndented
case class RegionBracket(override val indent: Int) extends RegionDelimNonIndented
case class RegionBrace(override val indent: Int) extends RegionDelimNonIndented with CanProduceLF {
  private[parsers] var widthValue: Int = indent
  override def width: Int = widthValue
}

case object RegionCaseMark extends RegionNonDelimNonIndented
sealed trait RegionCaseExpr extends RegionNonDelimNonIndented
final class RegionCaseExprPat(override val indent: Int) extends RegionCaseExpr
final class RegionCaseGuard(override val indent: Int) extends RegionCaseExpr
final class RegionCaseBody(override val indent: Int, val arrow: Token)
    extends SepRegionNonIndented with CanProduceLF with RegionDeferring
// indented region within a case guard, such as a colon argument
final class RegionCaseGuardIndent(indent: Int) extends RegionIndent(indent)

case object RegionColonMaybeFewerBraces extends RegionNonDelimNonIndented with CanProduceLF

sealed trait RegionHasParamMark extends RegionNonDelimNonIndented

sealed trait RegionDefDecl extends RegionNonDelimNonIndented
// initial mark of a definition (before colon)
case object RegionDefMark extends RegionDefDecl with RegionHasParamMark
// the type part of a definition (between colon and optional equals)
case object RegionDefType extends RegionDefDecl

/** region hierarchy to mark packages, classes, etc which can use `colon-eol` before template */
sealed trait RegionTemplateDecl extends RegionNonDelimNonIndented {
  val body: RegionTemplateBody = RegionTemplateBody
}

/** the initial part of declaration, before the template */
sealed trait RegionTemplateMark extends RegionTemplateDecl with CanProduceLF {
  val inherit: RegionTemplateInherit = RegionTemplateInherit
}

/** the initial part of declaration, before the template */
case object RegionTemplateMark extends RegionTemplateMark with RegionHasParamMark

/** the initial part of an enum declaration, before the template */
case object RegionEnumMark extends RegionTemplateMark with RegionHasParamMark {
  override val body: RegionTemplateBody = RegionEnumBody
  override val inherit: RegionTemplateInherit = RegionEnumInherit
}

/** the initial part of declaration, before the template */
case object RegionNewMark extends RegionTemplateMark

/** the initial part of the template, containing any inherit clauses */
sealed trait RegionTemplateInherit extends RegionTemplateDecl

case object RegionTemplateInherit extends RegionTemplateInherit

/** the initial part of the template of an enum, containing any inherit clauses */
case object RegionEnumInherit extends RegionTemplateInherit {
  override val body: RegionTemplateBody = RegionEnumBody
}

// the initial part of a given declaration or definition, before `=` or template
final class RegionGivenDecl(val kw: Token) extends RegionHasParamMark with CanProduceLF

/**
 * this marks the template body (or constructs which look like a template body, such as extensions).
 * for instance, helps override handling of `case` designed for catch/match/partial function but
 * inappropriate for enum.
 */
sealed trait RegionTemplateBody extends RegionNonDelimNonIndented

case object RegionTemplateBody extends RegionTemplateBody

/** the template body of an enum, where a `case` can start an enum case */
case object RegionEnumBody extends RegionTemplateBody

/** this marks the initial part of extension */
case object RegionExtensionMark extends RegionHasParamMark

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

  /** the indent of the condition's first line, but -1 if it starts it */
  def lineIndent: Int
}

/**
 * Describes the initial part of an `if` or `while` condition that started with an opening
 * parenthesis, before the matching closing parenthesis has been encountered.
 */
sealed trait RegionControlMaybeCond extends RegionControl {
  final def isTerminatingTokenRequired(): Boolean = false
  def asCond(): RegionControlCond
  def asBody(): RegionControl

  /** Returns true if `token` terminates the region that [[asCond]] returns */
  def isCondTerminatingToken(token: Token): Boolean
}

sealed trait RegionWhile extends RegionControl {
  def isControlKeyword(token: Token): Boolean = token.is[Token.KwWhile]
  def isTerminatingToken(token: Token): Boolean = token.is[Token.KwDo]
}
object RegionWhile {
  def apply(next: Token, lineIndent: Int): RegionWhile =
    if (next.is[Token.LeftParen]) new RegionWhileMaybeCond(lineIndent)
    else new RegionWhileCond(lineIndent)
}
final class RegionWhileCond(val lineIndent: Int) extends RegionControlCond with RegionWhile
final class RegionWhileMaybeCond(lineIndent: Int) extends RegionControlMaybeCond with RegionWhile {
  def asCond(): RegionControlCond = new RegionWhileCond(lineIndent)
  def asBody(): RegionControl = null
  def isCondTerminatingToken(token: Token): Boolean = isTerminatingToken(token)
}

sealed trait RegionIf extends RegionControl {
  def isControlKeyword(token: Token): Boolean = token.is[Token.KwIf]
  def isTerminatingToken(token: Token): Boolean = token.isAny[Token.KwThen, Token.KwElse]
}
object RegionIf {
  def apply(next: Token, lineIndent: Int): RegionIf =
    if (next.is[Token.LeftParen]) new RegionIfMaybeCond(lineIndent) else new RegionIfCond(lineIndent)
}
final class RegionIfCond(val lineIndent: Int) extends RegionControlCond with RegionIf {
  override def isTerminatingToken(token: Token): Boolean = token.is[Token.KwThen]
}
final class RegionIfMaybeCond(lineIndent: Int) extends RegionControlMaybeCond with RegionIf {
  def asCond(): RegionControlCond = new RegionIfCond(lineIndent)
  def asBody(): RegionControl = RegionThen
  def isCondTerminatingToken(token: Token): Boolean = token.is[Token.KwThen]
}

/**
 * Describes the body of an `if` statement, with a possible `else` following.
 */
case object RegionThen extends RegionControl with RegionDeferring {
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

case object RegionTry extends RegionControl with RegionDeferring {
  def isControlKeyword(token: Token): Boolean = token.is[Token.KwTry]
  def isTerminatingToken(token: Token): Boolean = token.isAny[Token.KwCatch, Token.KwFinally]
  def isTerminatingTokenRequired(): Boolean = false
}
