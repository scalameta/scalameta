package scala.meta.internal.tokenizers

import scala.meta.Dialect
import scala.meta.inputs._
import scala.meta.tokenizers._
import scala.meta.tokens._

import scala.collection.mutable.ListBuffer

trait WhitespaceTokenizer {
  def pushHS(token: Token.HSpace): Unit
  def pushVS(token: Token.EOL): Unit

  def flush(): Unit
}

object WhitespaceTokenizer {

  def apply(input: Input, dialect: Dialect)(implicit
      options: TokenizerOptions,
      tokens: java.util.Collection[Token]
  ): WhitespaceTokenizer =
    if (options.groupWhitespace) new Grouping(input, dialect) else new Granular

  class Granular(implicit tokens: java.util.Collection[Token]) extends WhitespaceTokenizer {
    override def pushHS(token: Token.HSpace): Unit = tokens.add(token)
    override def pushVS(token: Token.EOL): Unit = tokens.add(token)
    override def flush(): Unit = {}
  }

  class Grouping(input: Input, dialect: Dialect)(implicit tokens: java.util.Collection[Token])
      extends WhitespaceTokenizer {
    private val bufHS = new ListBuffer[Token.HSpace] // just horizontal tokens
    private val bufVS = new ListBuffer[Token.EOL] // consecutive EOL, without embedded horizontal space

    private def flushWS[A <: Token](
        buf: ListBuffer[A]
    )(f: (Int, Int, List[A]) => Token.Whitespace): Unit = buf.lastOption.foreach { last =>
      if (buf.length == 1) tokens.add(last)
      else {
        val res = buf.result()
        tokens.add(f(res.head.start, last.end, res))
      }
      buf.clear()
    }

    override def flush(): Unit = {
      flushWS(bufVS) { case (beg, end, res) => Token.MultiNL(input, dialect, beg, end, res) }
      flushWS(bufHS) { case (beg, end, res) => Token.MultiHS(input, dialect, beg, end, res) }
    }

    override def pushHS(token: Token.HSpace): Unit = bufHS += token
    override def pushVS(token: Token.EOL): Unit = {
      bufVS += token
      bufHS.clear() // simply ignore trailing space
    }
  }

}
