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

      def isLeadingInfixOperator: Boolean = dialect.allowSignificantIndentation && {
        val parts = token.text.split("_")
        parts.nonEmpty &&
        parts.last.forall(Chars.isOperatorPart) &&
        !token.text.startsWith("@") && {
          val nextSafeToken = nextSafe
          nextSafeToken.is[Whitespace] && {
            val nextToken = getStrictAfterSafe(nextSafeToken)
            nextToken.is[Ident] || nextToken.is[Interpolation.Id] ||
            nextToken.is[Literal] || nextToken.is[LeftParen] ||
            nextToken.is[LeftBrace]
          }
        }
      }

    }
  }

  object Classifiers {

    val soft = new SoftKeywords(dialect)

    @classifier
    trait TypeIntro {
      def unapply(token: Token): Boolean = {
        token.is[Ident] || token.is[KwSuper] || token.is[KwThis] ||
        token.is[LeftParen] || token.is[At] || token.is[Underscore] ||
        token.is[Unquote] || (token.is[Literal] && dialect.allowLiteralTypes)
      }
    }

    @classifier
    trait EndMarkerWord {
      def unapply(token: Token): Boolean = {
        token.is[Ident] || token.is[KwIf] || token.is[KwWhile] || token.is[KwFor] ||
        token.is[KwMatch] || token.is[KwTry] || token.is[KwNew] || token.is[KwThis] ||
        token.is[KwGiven] || token.is[KwVal]
      }
    }

    @classifier
    trait StopScanToken {
      def unapply(token: Token): Boolean = {
        token.is[KwPackage] || token.is[KwExport] || token.is[KwImport] ||
        token.is[KwIf] || token.is[KwElse] || token.is[KwWhile] || token.is[KwDo] ||
        token.is[KwFor] || token.is[KwYield] || token.is[KwNew] || token.is[KwTry] ||
        token.is[KwCatch] || token.is[KwFinally] || token.is[KwThrow] || token.is[KwReturn] ||
        token.is[KwMatch] || token.is[EOF] || token.is[Semicolon] || token.is[Modifier] || token
          .is[DefIntro]
      }
    }

    @classifier
    trait EndMarkerIntro {
      def unapply(token: Token): Boolean = {
        dialect.allowEndMarker &&
        token.is[Ident] &&
        token.text == "end" &&
        token.strictNext.is[EndMarkerWord] &&
        (token.next.strictNext.is[AtEOL] || token.next.strictNext.is[EOF])
      }
    }
    // then  else  do  catch  finally  yield  match
    @classifier
    trait CanContinueOnNextLine {
      def unapply(token: Token): Boolean = {
        token.is[KwThen] || token.is[KwElse] || token.is[KwDo] ||
        token.is[KwCatch] || token.is[KwFinally] || token.is[KwYield] ||
        token.is[KwMatch]
      }
    }

    @classifier
    trait ExprIntro {
      def unapply(token: Token): Boolean = {
        (token.is[Ident] && !token.is[SoftModifier] && !token.is[EndMarkerIntro]) ||
        token.is[Literal] || token.is[Interpolation.Id] || token.is[Xml.Start] ||
        token.is[KwDo] || token.is[KwFor] || token.is[KwIf] ||
        token.is[KwNew] || token.is[KwReturn] || token.is[KwSuper] ||
        token.is[KwThis] || token.is[KwThrow] || token.is[KwTry] || token.is[KwWhile] ||
        token.is[LeftParen] || token.is[LeftBrace] || token.is[Underscore] ||
        token.is[Unquote] || token.is[MacroSplice] || token.is[MacroQuote] ||
        token.is[Indentation.Indent] || (token.is[LeftBracket] && dialect.allowPolymorphicFunctions)
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
      private[Classifiers] def matchesNext(nextToken: Token): Boolean = {
        nextToken.is[LeftParen] || nextToken.is[LeftBrace] || nextToken.is[KwNew] ||
        nextToken.is[Ident] || nextToken.is[Literal] || nextToken.is[Interpolation.Id] ||
        nextToken.is[Xml.Start] || nextToken.is[KwSuper] || nextToken.is[KwThis] ||
        nextToken.is[MacroSplice] || nextToken.is[MacroQuote]
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
      def unapply(token: Token): Boolean = {
        token.is[Modifier] || token.is[At] ||
        token.is[TemplateIntro] || token.is[DclIntro] ||
        (token.is[Unquote] && token.next.is[DefIntro]) ||
        (token.is[Ellipsis] && token.next.is[DefIntro]) ||
        (token.is[KwCase] && token.next.isClassOrObjectOrEnum)
      }
    }

    @classifier
    trait TemplateIntro {
      def unapply(token: Token): Boolean = {
        token.is[Modifier] || token.is[At] ||
        token.is[KwClass] || token.is[KwObject] || token.is[KwTrait] ||
        (token.is[Unquote] && token.next.is[TemplateIntro]) ||
        (token.is[KwCase] && token.next.isClassOrObjectOrEnum)
      }
    }

    @classifier
    trait DclIntro {
      def unapply(token: Token): Boolean = {
        token.is[KwDef] || token.is[KwType] || token.is[KwEnum] ||
        token.is[KwVal] || token.is[KwVar] || token.is[KwGiven] ||
        token.is[KwExtension] || (token.is[Unquote] && token.next.is[DclIntro])
      }
    }

    @classifier
    trait KwExtension {
      def unapply(token: Token) = {
        // Logic taken from the Scala 3 parser
        token.is[soft.KwExtension] && (token.next.is[LeftParen] || token.next.is[LeftBracket])

      }
    }

    @classifier
    trait Modifier {
      def unapply(token: Token): Boolean = {
        token.is[ModifierKeyword] || token.is[SoftModifier]
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
      def unapply(token: Token): Boolean = {
        token.is[KwPrivate] || token.is[KwProtected] ||
        token.is[KwOverride] || token.is[soft.KwOpen]
      }
    }

    @classifier
    trait LocalModifier {
      def unapply(token: Token): Boolean = {
        token.is[Modifier] && !token.is[NonlocalModifier]
      }
    }

    @classifier
    trait StatSeqEnd {
      def unapply(token: Token): Boolean = {
        token.is[RightBrace] || token.is[EOF] || token.is[Indentation.Outdent]
      }
    }

    @classifier
    trait CaseDefEnd {
      def unapply(token: Token): Boolean = {
        token.is[RightBrace] || token.is[EOF] || token.is[Indentation.Outdent] ||
        (token.is[KwCase] && !token.next.isClassOrObject) || token.is[RightParen] ||
        (token.is[Ellipsis] && token.next.is[KwCase])
      }
    }

    @classifier
    trait CanStartIndent {
      def unapply(token: Token): Boolean = {
        token.is[KwYield] || token.is[KwTry] || token.is[KwCatch] || token.is[KwFinally] ||
        token.is[KwMatch] || token.is[KwDo] || token.is[KwFor] || token.is[KwThen] ||
        token.is[KwElse] || token.is[Equals] || token.is[KwWhile] || token.is[KwIf] ||
        token.is[RightArrow] || token.is[KwReturn] || token.is[LeftArrow] ||
        token.is[ContextArrow] || (
          token.is[KwWith] && {
            val next = token.next
            next.is[DefIntro] || next.is[KwImport] || next.is[KwExport]
          }
        )
      }
    }

    @classifier
    trait CantStartStat {
      def unapply(token: Token): Boolean = {
        token.is[KwCatch] || token.is[KwElse] || token.is[KwExtends] ||
        token.is[KwFinally] || token.is[KwForsome] || token.is[KwMatch] ||
        token.is[KwWith] || token.is[KwYield] ||
        token.is[RightParen] || token.is[LeftBracket] || token.is[RightBracket] ||
        token.is[RightBrace] || token.is[Comma] || token.is[Colon] ||
        token.is[Dot] || token.is[Equals] || token.is[Semicolon] ||
        token.is[Hash] || token.is[RightArrow] || token.is[LeftArrow] ||
        token.is[Subtype] || token.is[Supertype] || token.is[Viewbound] ||
        token.is[LF] || token.is[LFLF] || token.is[EOF]

      }
    }

    @classifier
    trait CanEndStat {
      def unapply(token: Token): Boolean = {
        token.is[Ident] || token.is[KwGiven] || token.is[Literal] ||
        token.is[Interpolation.End] || token.is[Xml.End] ||
        token.is[KwReturn] || token.is[KwThis] || token.is[KwType] ||
        token.is[RightParen] || token.is[RightBracket] || token.is[RightBrace] ||
        token.is[Underscore] || token.is[Ellipsis] || token.is[Unquote] ||
        token.prev.is[EndMarkerIntro]
      }
    }

    @classifier
    trait StatSep {
      def unapply(token: Token): Boolean = {
        token.is[Semicolon] || token.is[LF] || token.is[LFLF] || token.is[EOF]
      }
    }

    @classifier
    trait CanStartColonEol {
      def unapply(token: Token): Boolean = {
        token.is[KwTrait] || token.is[KwClass] ||
        token.is[KwObject] || token.is[KwEnum] ||
        token.is[KwType] || token.is[KwPackage] ||
        token.is[KwGiven] || token.is[KwNew]
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
