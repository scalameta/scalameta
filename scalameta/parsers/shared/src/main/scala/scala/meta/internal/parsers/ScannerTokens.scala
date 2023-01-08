package scala.meta.internal.parsers

import scala.annotation.tailrec
import scala.meta.Dialect
import scala.meta.classifiers._
import scala.meta.inputs.Input
import scala.meta.internal.classifiers.classifier
import scala.meta.internal.tokenizers._
import scala.meta.prettyprinters._
import scala.meta.tokenizers._
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens

final class ScannerTokens(val tokens: Tokens)(implicit dialect: Dialect) {

  @tailrec
  final def getPrevIndex(index: Int): Int = {
    val prev = getPrevSafeIndex(index)
    if (tokens(prev).is[Trivia]) getPrevIndex(prev) else prev
  }

  def getPrevToken(index: Int): Token = tokens(getPrevIndex(index))

  @tailrec
  final def getNextIndex(index: Int): Int = {
    val next = getNextSafeIndex(index)
    if (tokens(next).is[Trivia]) getNextIndex(next) else next
  }

  def getNextToken(index: Int): Token = tokens(getNextIndex(index))

  @tailrec
  final def getStrictAfterSafe(index: Int): Int = {
    val token = tokens(index)
    if (token.isAny[HSpace, Comment]) getStrictAfterSafe(getNextSafeIndex(index))
    else index
  }

  def getStrictNext(index: Int): Int = getStrictAfterSafe(getNextSafeIndex(index))

  @inline def getPrevSafeIndex(index: Int): Int = Math.max(index - 1, 0)
  @inline def getNextSafeIndex(index: Int): Int = Math.min(index + 1, tokens.length - 1)

  // NOTE: Scala's parser isn't ready to accept whitespace and comment tokens,
  // so we have to filter them out, because otherwise we'll get errors like `expected blah, got whitespace`
  // However, in certain tricky cases some whitespace tokens (namely, newlines) do have to be emitted.
  // This leads to extremely dirty and seriously crazy code.
  implicit class XtensionTokenClass(token: Token) {

    def isClassOrObject = token.isAny[KwClass, KwObject]
    def isClassOrObjectOrEnum = isClassOrObject || (token.is[Ident] && dialect.allowEnums)

    def asString: String =
      s"[${token.getClass.getSimpleName}@${token.end}]${token.syntax.replace("\n", "")}"

    def isBackquoted: Boolean = {
      val text = token.text
      text.startsWith("`") && text.endsWith("`")
    }

    def isIdentSymbolicInfixOperator: Boolean = isBackquoted || {
      val text = token.text

      @tailrec
      def iter(idx: Int, nonEmpty: Boolean): Boolean = {
        val ch = text(idx)
        if (ch == '_') nonEmpty || idx > 0 && iter(idx - 1, false)
        else Chars.isOperatorPart(ch) && (idx == 0 || iter(idx - 1, true))
      }

      val len = text.length
      len == 0 || (text(0) != '@' && iter(len - 1, false))
    }
  }

  def isLeadingInfixOperator(index: Int): Boolean = {
    val token = tokens(index)
    token.is[Ident] && token.isIdentSymbolicInfixOperator && {
      val nextSafeIndex = getNextSafeIndex(index)
      tokens(nextSafeIndex).is[Whitespace] &&
      (tokens(getStrictAfterSafe(nextSafeIndex)) match {
        case _: Ident | _: Interpolation.Id | _: LeftParen | _: LeftBrace | _: Literal => true
        case _ => false
      })
    }
  }

  def getTokenAtLineStart(idx: Int): Token =
    getTokenAtLineStart(idx, tokens(idx))

  @tailrec
  private def getTokenAtLineStart(idx: Int, nextNonSpace: Token): Token =
    if (idx == 0) nextNonSpace
    else
      tokens(idx) match {
        case _: AtEOL => nextNonSpace
        case _: HSpace => getTokenAtLineStart(idx - 1, nextNonSpace)
        case t => getTokenAtLineStart(idx - 1, t)
      }

  val soft = new SoftKeywords(dialect)

  @classifier
  trait TypeIntro {
    def unapply(token: Token): Boolean = token match {
      case _: Ident | _: KwSuper | _: KwThis | _: LeftParen | _: At | _: Underscore | _: Unquote =>
        true
      case _: Literal => dialect.allowLiteralTypes
      case _ => false
    }
  }

  def isEndMarkerIntro(index: Int): Boolean = tokens(index).is[soft.KwEnd] && {
    val nextIndex = getStrictNext(index)
    tokens(nextIndex) match {
      case _: Ident | _: KwIf | _: KwWhile | _: KwFor | _: KwMatch | _: KwTry | _: KwNew |
          _: KwThis | _: KwGiven | _: KwVal =>
        tokens(getStrictNext(nextIndex)) match {
          case _: EOF | _: AtEOL => true
          case _ => false
        }
      case _ => false
    }
  }

  // then  else  do  catch  finally  yield  match
  @classifier
  trait CanContinueOnNextLine {
    def unapply(token: Token): Boolean = token match {
      case _: KwThen | _: KwElse | _: KwDo | _: KwCatch | _: KwFinally | _: KwYield | _: KwMatch =>
        true
      case _ => false
    }
  }

  def isExprIntro(token: Token, index: => Int): Boolean = token match {
    case _: Ident => !isSoftModifier(index) && !isEndMarkerIntro(index)
    case _: Literal | _: Interpolation.Id | _: Xml.Start | _: KwDo | _: KwFor | _: KwIf | _: KwNew |
        _: KwReturn | _: KwSuper | _: KwThis | _: KwThrow | _: KwTry | _: KwWhile | _: LeftParen |
        _: LeftBrace | _: Underscore | _: Unquote | _: MacroSplice | _: MacroQuote |
        _: Indentation.Indent =>
      true
    case _: LeftBracket => dialect.allowPolymorphicFunctions
    case _ => false
  }

  def isSoftModifier(index: Int): Boolean = {
    @inline def nextIsDclIntroOrModifierOr(f: Token => Boolean): Boolean = {
      val next = getNextIndex(index)
      isDclIntro(next) || isModifier(next) || f(tokens(next))
    }

    tokens(index).toString match {
      case soft.KwTransparent() => nextIsDclIntroOrModifierOr(_.is[KwTrait])
      case soft.KwOpaque() => nextIsDclIntroOrModifierOr(_ => false)
      case soft.KwInline() => nextIsDclIntroOrModifierOr(matchesAfterInlineMatchMod)
      case soft.KwOpen() | soft.KwInfix() => isDefIntro(getNextIndex(index))
      case _ => false
    }
  }

  @inline def isInlineMatchMod(index: Int): Boolean =
    tokens(index).is[soft.KwInline] && matchesAfterInlineMatchMod(getNextToken(index))

  private def matchesAfterInlineMatchMod(token: Token): Boolean = token match {
    case _: LeftParen | _: LeftBrace | _: KwNew | _: Ident | _: Literal | _: Interpolation.Id |
        _: Xml.Start | _: KwSuper | _: KwThis | _: MacroSplice | _: MacroQuote =>
      true
    case _ => false
  }

  def isCaseIntro(index: Int): Boolean =
    tokens(index).is[KwCase] && !getNextToken(index).isClassOrObject

  @tailrec
  final def isDefIntro(index: Int): Boolean = tokens(index) match {
    case _: At => true
    case _: Unquote | _: Ellipsis => isDefIntro(getNextIndex(index))
    case _: KwCase => getNextToken(index).isClassOrObjectOrEnum
    case _ => isDclIntro(index) || isModifier(index) || isTemplateIntro(index)
  }

  @tailrec
  final def isTemplateIntro(index: Int): Boolean = tokens(index) match {
    case _: At | _: KwClass | _: KwObject | _: KwTrait => true
    case _: Unquote => isTemplateIntro(getNextIndex(index))
    case _: KwCase => getNextToken(index).isClassOrObjectOrEnum
    case _ => isModifier(index)
  }

  @tailrec
  final def isDclIntro(index: Int): Boolean = tokens(index) match {
    case _: KwDef | _: KwType | _: KwEnum | _: KwVal | _: KwVar | _: KwGiven => true
    case _: Unquote => isDclIntro(getNextIndex(index))
    case _ => isKwExtension(index)
  }

  // Logic taken from the Scala 3 parser
  def isKwExtension(index: Int): Boolean =
    tokens(index).is[soft.KwExtension] && (getNextToken(index) match {
      case _: LeftParen | _: LeftBracket => true
      case _ => false
    })

  def isModifier(index: Int): Boolean =
    tokens(index).is[ModifierKeyword] || isSoftModifier(index)

  @classifier
  trait NonParamsModifier {
    def unapply(token: Token): Boolean = {
      token.toString match {
        case soft.KwOpen() | soft.KwOpaque() | soft.KwTransparent() | soft.KwInfix() => true
        case _ => false
      }
    }
  }

  def isNonlocalModifier(token: Token): Boolean = token match {
    case _: KwPrivate | _: KwProtected | _: KwOverride | soft.KwOpen() => true
    case _ => false
  }

  @classifier
  trait StatSeqEnd {
    def unapply(token: Token): Boolean = token match {
      case _: RightBrace | _: EOF | _: Indentation.Outdent => true
      case _ => false
    }
  }

  def isCaseDefEnd(token: Token, index: => Int): Boolean = token match {
    case _: RightBrace | _: RightParen | _: EOF | _: Indentation.Outdent => true
    case _: KwCase => !getNextToken(index).isClassOrObject
    case _: Ellipsis => getNextToken(index).is[KwCase]
    case _ => false
  }

  def canStartIndent(index: Int): Boolean = tokens(index) match {
    case _: KwYield | _: KwTry | _: KwCatch | _: KwFinally | _: KwMatch | _: KwDo | _: KwFor |
        _: KwThen | _: KwElse | _: KwWhile | _: KwIf | _: RightArrow | _: KwReturn | _: LeftArrow |
        _: ContextArrow =>
      true
    case _: Equals =>
      getNextToken(index) match {
        case _: KwMacro => false
        case _ => true
      }
    case _: KwWith =>
      val nextIndex = getNextIndex(index)
      isDefIntro(nextIndex) || (tokens(nextIndex) match {
        case _: KwImport | _: KwExport => true
        case _ => false
      })
    case _ => false
  }

  @classifier
  trait CantStartStat {
    def unapply(token: Token): Boolean = token match {
      case _: KwCatch | _: KwElse | _: KwExtends | _: KwFinally | _: KwForsome | _: KwMatch |
          _: KwWith | _: KwYield | _: RightParen | _: LeftBracket | _: RightBracket |
          _: RightBrace | _: Comma | _: Colon | _: Dot | _: Equals | _: Semicolon | _: Hash |
          _: RightArrow | _: LeftArrow | _: Subtype | _: Supertype | _: Viewbound | _: LF |
          _: LFLF | _: EOF =>
        true
      case _ => false
    }
  }

  def canEndStat(index: Int): Boolean = tokens(index) match {
    case _: Ident | _: KwGiven | _: Literal | _: Interpolation.End | _: Xml.End | _: KwReturn |
        _: KwThis | _: KwType | _: RightParen | _: RightBracket | _: RightBrace | _: Underscore |
        _: Ellipsis | _: Unquote =>
      true
    case _ => isEndMarkerIntro(getPrevIndex(index))
  }

  @classifier
  trait StatSep {
    def unapply(token: Token): Boolean = token match {
      case _: Semicolon | _: LF | _: LFLF | _: EOF => true
      case _ => false
    }
  }

  object Wildcard {
    def unapply(token: Token): Boolean =
      token.is[Underscore] || isStar(token)

    def isStar(token: Token): Boolean =
      dialect.allowStarWildcardImport && token.syntax == "*"
  }

  /**
   * When token on `tokenPosition` is not a whitespace and is a first non-whitespace character in a
   * current line then a result is a number of whitespace characters counted. Otherwise
   * {{{(-1, -1)}}} is returned.
   *
   * Returns a tuple2 where:
   *   - first value is indentation level
   *   - second is `LF` token index
   */
  private[parsers] def countIndentAndNewlineIndex(tokenPosition: Int): (Int, Int) = {
    @tailrec
    def countIndentInternal(pos: Int, acc: Int = 0): (Int, Int) = {
      if (pos < 0) (acc, pos)
      else {
        val token = tokens(pos)
        token match {
          case _: EOL | _: BOF => (acc, pos)
          case AsMultilineComment(c) => (ScannerTokens.multilineCommentIndent(c), pos)
          case _: Comment => countIndentInternal(pos - 1)
          case _: HSpace => countIndentInternal(pos - 1, acc + 1)
          case _ => (-1, -1)
        }
      }
    }

    if (tokens(tokenPosition).is[Whitespace]) (-1, -1)
    else countIndentInternal(tokenPosition - 1)
  }

  private[parsers] def countIndent(tokenPosition: Int): Int =
    countIndentAndNewlineIndex(tokenPosition)._1

  private[parsers] def mkIndentToken(pointPos: Int): Token = {
    val token = tokens(pointPos)
    new Indentation.Indent(token.input, token.dialect, token.start, token.start)
  }

  private[parsers] def mkOutdentToken(pointPos: Int): Token = {
    val token = tokens(pointPos)
    new Indentation.Outdent(token.input, token.dialect, token.start, token.start)
  }

  private[parsers] def findOutdentPos(prevPos: Int, currPos: Int, region: SepRegion): Int = {
    val outdent = region.indent

    @tailrec
    def iter(i: Int, pos: Int, indent: Int): Int = {
      if (i >= currPos) {
        if (pos < currPos) pos else currPos - 1
      } else {
        tokens(i) match {
          case _: EOL =>
            iter(i + 1, i, 0)
          case _: HSpace if indent >= 0 =>
            iter(i + 1, pos, indent + 1)
          case _: Whitespace =>
            iter(i + 1, pos, indent)
          case _: Comment if indent < 0 || outdent <= indent =>
            iter(i + 1, i + 1, -1)
          case _ => pos
        }
      }
    }

    val iterPos = 1 + prevPos
    if (iterPos < currPos) iter(iterPos, prevPos, if (tokens(prevPos).is[EOL]) 0 else -1)
    else if (tokens(currPos).is[EOF]) currPos
    else prevPos
  }

  @tailrec
  private[parsers] def isAheadNewLine(currentPosition: Int): Boolean = {
    val nextPos = currentPosition + 1
    nextPos < tokens.length && {
      val nextToken = tokens(nextPos)
      nextToken.is[LF] || nextToken.is[Trivia] && isAheadNewLine(nextPos)
    }
  }

}

object ScannerTokens {

  def apply(input: Input)(implicit dialect: Dialect): ScannerTokens = {
    new ScannerTokens(input.tokenize.get)
  }

  private def multilineCommentIndent(t: Comment): Int = {
    @tailrec
    def loop(idx: Int, indent: Int, isAfterNewline: Boolean): Int = {
      if (idx == t.value.length) indent
      else {
        t.value.charAt(idx) match {
          case '\n' => loop(idx + 1, 0, isAfterNewline = true)
          case ' ' | '\t' if isAfterNewline => loop(idx + 1, indent + 1, isAfterNewline)
          case _ => loop(idx + 1, indent, isAfterNewline = false)
        }
      }
    }

    loop(0, 0, false)
  }

}
