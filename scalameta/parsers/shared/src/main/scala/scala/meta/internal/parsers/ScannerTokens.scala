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

import org.scalameta.debug

class ScannerTokens(tokens: Tokens, input: Input)(implicit dialect: Dialect) {

  // NOTE: This is a cache that's necessary for reasonable performance of prev/next for tokens.
  // It maps character positions in input's content into indices in the scannerTokens vector.
  // One complication here is that there can be multiple tokens that sort of occupy a given position,
  // so the clients of this cache need to be wary of that!
  private val cache: Array[Int] = {
    val result = new Array[Int](input.chars.length + 1)
    var i = 0
    while (i < tokens.length) {
      val token = tokens(i)
      var j = token.start
      do {
        result(j) = i
        j += 1
      } while (j < token.end)
      i += 1
    }
    result
  }

  @inline def apply(idx: Int): Token = tokens(idx)
  @inline def length: Int = tokens.length
  @inline def head: Token = tokens.head

  import Implicits._

  @tailrec
  private final def getPrev(token: Token): Token = {
    val prev = token.prevSafe
    if (prev.is[Trivia]) getPrev(prev) else prev
  }

  @tailrec
  private final def getNext(token: Token): Token = {
    val next = token.nextSafe
    if (next.is[Trivia]) getNext(next) else next
  }

  @tailrec
  private final def getStrictAfterSafe(token: Token): Token = {
    if (token.is[HSpace] || token.is[Comment]) getStrictAfterSafe(token.nextSafe)
    else token
  }

  object Implicits {
    // NOTE: Scala's parser isn't ready to accept whitespace and comment tokens,
    // so we have to filter them out, because otherwise we'll get errors like `expected blah, got whitespace`
    // However, in certain tricky cases some whitespace tokens (namely, newlines) do have to be emitted.
    // This leads to extremely dirty and seriously crazy code.
    implicit class XtensionTokenClass(token: Token) {

      def isClassOrObject = token.is[KwClass] || token.is[KwObject]
      def isClassOrObjectOrEnum = isClassOrObject || (token.is[Ident] && dialect.allowEnums)

      def asString: String =
        s"[${token.getClass.getSimpleName}@${token.end}]${token.syntax.replace("\n", "")}"

      def index: Int = {
        @tailrec
        def lurk(roughIndex: Int): Int = {
          require(roughIndex >= 0 && debug(token), s"tok=$asString")
          val scannerToken = tokens(roughIndex)

          def exactMatch = scannerToken eq token

          def originMatch =
            (token.is[LFLF] || token.is[Indentation.Outdent] || token.is[Indentation.Indent]) &&
              scannerToken.start == token.start && scannerToken.end == token.end

          if (exactMatch || originMatch) roughIndex
          else lurk(roughIndex - 1)
        }

        lurk(cache(token.start))
      }

      @inline
      def prev: Token = getPrev(token)

      @inline
      def prevSafe: Token = tokens(Math.max(index - 1, 0))

      @inline
      def next: Token = getNext(token)

      @inline
      def nextSafe: Token = tokens(Math.min(index + 1, tokens.length - 1))

      @inline
      def strictNext: Token = getStrictAfterSafe(nextSafe)

      def isBackquoted: Boolean = {
        val text = token.text
        text.startsWith("`") && text.endsWith("`")
      }

      def isSymbolicInfixOperator: Boolean = token.is[Ident] && isIdentSymbolicInfixOperator

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

      def isLeadingInfixOperator: Boolean =
        token.is[Ident] && isIdentLeadingInfixOperator

      def isIdentLeadingInfixOperator: Boolean =
        dialect.allowInfixOperatorAfterNL && isSymbolicInfixOperator && (nextSafe match {
          case nt: Whitespace =>
            getStrictAfterSafe(nt) match {
              case _: Ident | _: Interpolation.Id | _: LeftParen | _: LeftBrace | _: Literal => true
              case _ => false
            }
          case _ => false
        })

    }
  }

  object Classifiers {

    val soft = new SoftKeywords(dialect)

    @classifier
    trait TypeIntro {
      def unapply(token: Token): Boolean = token match {
        case _: Ident | _: KwSuper | _: KwThis | _: LeftParen | _: At | _: Underscore |
            _: Unquote =>
          true
        case _: Literal => dialect.allowLiteralTypes
        case _ => false
      }
    }

    @classifier
    trait EndMarkerWord {
      def unapply(token: Token): Boolean = token match {
        case _: Ident | _: KwIf | _: KwWhile | _: KwFor | _: KwMatch | _: KwTry | _: KwNew |
            _: KwThis | _: KwGiven | _: KwVal =>
          true
        case _ => false
      }
    }

    @classifier
    trait EndMarkerIntro {
      def unapply(token: Token): Boolean = token match {
        case Ident("end") if dialect.allowEndMarker =>
          val nt = token.strictNext
          nt.is[EndMarkerWord] && {
            val nt2 = nt.strictNext
            nt2.is[EOF] || nt2.is[AtEOL]
          }
        case _ => false
      }
    }
    // then  else  do  catch  finally  yield  match
    @classifier
    trait CanContinueOnNextLine {
      def unapply(token: Token): Boolean = token match {
        case _: KwThen | _: KwElse | _: KwDo | _: KwCatch | _: KwFinally | _: KwYield |
            _: KwMatch =>
          true
        case _ => false
      }
    }

    @classifier
    trait ExprIntro {
      def unapply(token: Token): Boolean = token match {
        case _: Ident => !token.is[SoftModifier] && !token.is[EndMarkerIntro]
        case _: Literal | _: Interpolation.Id | _: Xml.Start | _: KwDo | _: KwFor | _: KwIf |
            _: KwNew | _: KwReturn | _: KwSuper | _: KwThis | _: KwThrow | _: KwTry | _: KwWhile |
            _: LeftParen | _: LeftBrace | _: Underscore | _: Unquote | _: MacroSplice |
            _: MacroQuote | _: Indentation.Indent =>
          true
        case _: LeftBracket => dialect.allowPolymorphicFunctions
        case _ => false
      }
    }

    @classifier
    trait SoftModifier {
      def unapply(token: Token): Boolean = {
        @inline def nextIsDclIntroOrModifierOr(f: Token => Boolean): Boolean =
          token.next match {
            case DclIntro() | Modifier() => true
            case t => f(t)
          }
        token.toString match {
          case soft.KwTransparent() => nextIsDclIntroOrModifierOr(_.is[KwTrait])
          case soft.KwOpaque() => nextIsDclIntroOrModifierOr(_ => false)
          case soft.KwInline() => nextIsDclIntroOrModifierOr(InlineMatchMod.matchesNext)
          case soft.KwOpen() | soft.KwInfix() => token.next.is[DefIntro]
          case _ => false
        }
      }
    }

    @classifier
    trait InlineMatchMod {
      @inline def unapply(token: Token): Boolean =
        token.is[soft.KwInline] && matchesNext(token.next)
      private[Classifiers] def matchesNext(token: Token): Boolean = token match {
        case _: LeftParen | _: LeftBrace | _: KwNew | _: Ident | _: Literal | _: Interpolation.Id |
            _: Xml.Start | _: KwSuper | _: KwThis | _: MacroSplice | _: MacroQuote =>
          true
        case _ => false
      }
    }

    @classifier
    trait CaseIntro {
      def unapply(token: Token): Boolean = {
        token.is[KwCase] && !token.next.isClassOrObject
      }
    }

    @classifier
    trait DefIntro {
      @tailrec
      final def unapply(token: Token): Boolean = token match {
        case _: At | Modifier() | TemplateIntro() | DclIntro() => true
        case _: Unquote | _: Ellipsis => unapply(token.next)
        case _: KwCase => token.next.isClassOrObjectOrEnum
        case _ => false
      }
    }

    @classifier
    trait TemplateIntro {
      @tailrec
      final def unapply(token: Token): Boolean = token match {
        case _: At | _: KwClass | _: KwObject | _: KwTrait | Modifier() => true
        case _: Unquote => unapply(token.next)
        case _: KwCase => token.next.isClassOrObjectOrEnum
        case _ => false
      }
    }

    @classifier
    trait DclIntro {
      @tailrec
      final def unapply(token: Token): Boolean = token match {
        case _: KwDef | _: KwType | _: KwEnum | _: KwVal | _: KwVar | _: KwGiven | KwExtension() =>
          true
        case _: Unquote => unapply(token.next)
        case _ => false
      }
    }

    @classifier
    trait KwExtension {
      def unapply(token: Token) = {
        // Logic taken from the Scala 3 parser
        token.is[soft.KwExtension] && (token.next match {
          case _: LeftParen | _: LeftBracket => true
          case _ => false
        })
      }
    }

    @classifier
    trait Modifier {
      def unapply(token: Token): Boolean = token match {
        case _: ModifierKeyword | SoftModifier() => true
        case _ => false
      }
    }

    @classifier
    trait NonParamsModifier {
      def unapply(token: Token): Boolean = {
        token.toString match {
          case soft.KwOpen() | soft.KwOpaque() | soft.KwTransparent() | soft.KwInfix() => true
          case _ => false
        }
      }
    }

    @classifier
    trait NonlocalModifier {
      def unapply(token: Token): Boolean = token match {
        case _: KwPrivate | _: KwProtected | _: KwOverride | soft.KwOpen() => true
        case _ => false
      }
    }

    @classifier
    trait StatSeqEnd {
      def unapply(token: Token): Boolean = token match {
        case _: RightBrace | _: EOF | _: Indentation.Outdent => true
        case _ => false
      }
    }

    @classifier
    trait CaseDefEnd {
      def unapply(token: Token): Boolean = token match {
        case _: RightBrace | _: RightParen | _: EOF | _: Indentation.Outdent => true
        case _: KwCase => !token.next.isClassOrObject
        case _: Ellipsis => token.next.is[KwCase]
        case _ => false
      }
    }

    @classifier
    trait CanStartIndent {
      def unapply(token: Token): Boolean = token match {
        case _: KwYield | _: KwTry | _: KwCatch | _: KwFinally | _: KwMatch | _: KwDo | _: KwFor |
            _: KwThen | _: KwElse | _: KwWhile | _: KwIf | _: RightArrow | _: KwReturn |
            _: LeftArrow | _: ContextArrow =>
          true
        case _: Equals =>
          token.next match {
            case _: KwMacro => false
            case _ => true
          }
        case _: KwWith =>
          token.next match {
            case _: KwImport | _: KwExport | DefIntro() => true
            case _ => false
          }
        case _ => false
      }
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

    @classifier
    trait CanEndStat {
      def unapply(token: Token): Boolean = token match {
        case _: Ident | _: KwGiven | _: Literal | _: Interpolation.End | _: Xml.End | _: KwReturn |
            _: KwThis | _: KwType | _: RightParen | _: RightBracket | _: RightBrace |
            _: Underscore | _: Ellipsis | _: Unquote =>
          true
        case _ => token.prev.is[EndMarkerIntro]
      }
    }

    @classifier
    trait StatSep {
      def unapply(token: Token): Boolean = token match {
        case _: Semicolon | _: LF | _: LFLF | _: EOF => true
        case _ => false
      }
    }

    @classifier
    trait CanStartColonEol {
      def unapply(token: Token): Boolean = token match {
        case _: KwTrait | _: KwClass | _: KwObject | _: KwEnum | _: KwType | _: KwPackage |
            _: KwGiven | _: KwNew =>
          true
        case _ => false
      }
    }

    object Wildcard {
      def unapply(token: Token): Boolean =
        token.is[Underscore] || isStar(token)

      def isStar(token: Token): Boolean =
        dialect.allowStarWildcardImport && token.syntax == "*"
    }

  }

}

object ScannerTokens {

  def apply(input: Input)(implicit dialect: Dialect): ScannerTokens = {
    val tokens = input.tokenize.get
    new ScannerTokens(tokens, input)
  }

}
