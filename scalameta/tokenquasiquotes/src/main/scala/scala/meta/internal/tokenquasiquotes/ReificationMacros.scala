package scala.meta
package internal
package tokenquasiquotes

import org.scalameta.convert._
import scala.reflect.macros.whitebox.Context
import scala.meta.internal.dialects.InstantiateDialect
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.tokenizers._

/**
 * Object used to extract the underlying code for each token.
 * Two tokens are considered equal (in pattern matching) if they both come from the same code.
 */
object TokenExtractor {
  def unapply(t: Token) = Some(t.code)
}

class ReificationMacros(val c: Context) extends TokenLiftables
                                           with InstantiateDialect {

  import c.universe._
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  def apply(args: c.Tree*)(dialect: c.Tree): c.Tree = expand
  def unapply(scrutinee: c.Tree)(dialect: c.Tree): c.Tree = expand

  // Extract the interesting parts of toks"..."
  private lazy val q"$_($_.apply(..${parts: List[String]})).$_.$method[..$_](..$args)($dialectTree)" = c.macroApplication

  private val quasiquotePrefix = c.freshName("quasiquote")
  private def bindingName(i: Int) = TermName(quasiquotePrefix + "$x$" + i)

  private def arg(i: Int): c.Tree = method match {
    case TermName("apply") =>
      args(i)

    case TermName("unapply") =>
      val name = bindingName(i)
      pq"$name @ _"
  }

  /**
   * Gets the tokens for each part of the input, and glue them together in a single `Tokens`
   * by inserting special `Token.Unquote` tokens between them.
   * The tree of the Unquote token is a scala.meta tree that represents the index of the
   * argument.
   */
  private lazy val toks: Tokens = {
    implicit val dialect: Dialect = scala.meta.dialects.Quasiquote(instantiateDialect(dialectTree))

    /** Removes the heading BOF and trailing EOF from a sequence of tokens */
    def trim(toks: Tokens): Tokens = toks match {
      case (_: Token.BOF) +: toks :+ (_: Token.EOF) => Tokens(toks: _*)
      case _                                                => toks
    }

    val result =
      parts.init.zipWithIndex.flatMap {
        case (part, i) =>
          // TODO: Ugh, creating synthetic inputs with semi-meaningless text just to satisfy the framework.
          // This really shows the need in virtual tokens...
          val argAsString = arg(i).toString
          trim(part.tokens) :+ Token.Unquote(Input.String(argAsString), dialect, 0, argAsString.length - 1, arg(i))
      } ++ trim(parts.last.tokens)

    Tokens(result: _*)
  }

  def expand: c.Tree = {
    method match {
      case TermName("apply") =>
        q"$toks"

      case TermName("unapply") =>
        def countEllipsisUnquote(toks: Seq[Token]): Int = toks match {
          case (_: Token.Ellipsis) +: (_: Token.Unquote) +: rest =>
            1 + countEllipsisUnquote(rest)
          case other +: rest =>
            countEllipsisUnquote(rest)
          case _ =>
            0
        }

        if (countEllipsisUnquote(toks) > 1) {
          c.abort(c.macroApplication.pos, "cannot use splicing more than once per quasiquote")
        }

        def patternForToken(t: Token) = t match {
          case t: Token.Unquote => pq"${t.tree.asInstanceOf[c.Tree]}"
          case t                    => pq"_root_.scala.meta.internal.tokenquasiquotes.TokenExtractor(${t.code})"
        }

        // Split the input in three parts: (before, ..unquote, after).
        // Since `Tokens` is a`Seq`, we will be able to deconstruct an instance of
        // `Tokens` using before +: ..unquote :+ after.
        val splitted = {
          def split(toks: Seq[Token]): (Tokens, Option[Token], Tokens) = toks match {
            case (_: Token.Ellipsis) +: (u: Token.Unquote) +: rest =>
              (Tokens(), Some(u), Tokens(rest: _*))

            case t +: rest =>
              val (before, middle, after) = split(rest)
              (t +: before, middle, after)

            case Seq() =>
              (Tokens(), None, Tokens())
          }

          split(toks)
        }

        val pattern =
          splitted match {
            // corresponds to `case toks"..$foo" => ...`
            case (Tokens(), Some(middle), Tokens()) =>
              patternForToken(middle)

            // corresponds to `case toks"foo $bar baz" => ...`
            case (before, None, _) =>
              val subPatterns = before map patternForToken
              q"_root_.scala.meta.tokens.Tokens(..$subPatterns)"

            // corresponds to `case toks"foo $bar ..$baz" => ...`
            case (before, Some(middle), Tokens()) =>
              val beforePatterns = before map patternForToken
              (beforePatterns foldRight patternForToken(middle)) {
                case (pat, acc) => pq"$pat +: $acc"
              }

            // corresponds to `case toks"foo $bar ..$baz $quux blah"
            case (before, Some(middle), after) =>
              val beforePatterns = before map patternForToken
              val afterPatterns  = after map patternForToken

              val withBeforePatterns = (beforePatterns foldRight patternForToken(middle)) {
                case (pat, acc) => pq"$pat +: $acc"
              }

              (afterPatterns foldLeft withBeforePatterns) {
                case (acc, pat) => pq"$acc :+ $pat"
              }
          }

        // Find the number of the (unique) Unquote token that is preceded by an Ellipsis
        // We use this information to wrap the value matched in `Tokens()` (otherwise we
        // would get a Token.Projected[Token])
        val dottedUnquote =
          splitted match {
            case (before, Some(_), _) => before count (_.isInstanceOf[Token.Unquote])
            case _ => -1
          }

        val (thenp, elsep) =
          if (parts.size == 1) (q"true", q"false")
          else {
            val bindings = parts.init.zipWithIndex map {
              case (_, i) =>
                val name = bindingName(i)
                if (i == dottedUnquote) q"_root_.scala.meta.tokens.Tokens($name: _*)"
                else q"$name"
            }
            (q"_root_.scala.Some(..$bindings)", q"_root_.scala.None")
          }

        // If our pattern is simply toks"..$foo", we don't emit a default case
        // to avoid generating a warning during the compilation of the expansion.
        val cases =
          if (parts.size == 2 && dottedUnquote == 0)
            q"""in match { case $pattern => $thenp }"""
          else
            q"""in match {
                  case $pattern => $thenp
                  case _        => $elsep
                }"""

        q"""
          new {
            def unapply(in: _root_.scala.meta.tokens.Tokens) = $cases
          }.unapply(..$args)
        """
    }
  }
}
