package scala.meta
package internal
package trees

import org.scalameta.adt
import scala.meta.common._
import scala.meta.inputs._
import scala.meta.internal.tokens._
import scala.meta.tokens._
import scala.meta.tokenizers._

@adt.root
trait Origin extends Optional
object Origin {
  @adt.none
  object None extends Origin

  @adt.leaf
  class Parsed(input: Input, dialect: Dialect, pos: TokenStreamPosition) extends Origin {
    @inline private def tokenize() = dialect(input).tokenize.get

    def position: Position = {
      val tokens = tokenize()
      val start = tokens(pos.start).start
      val end = tokens(pos.end - 1).end
      Position.Range(input, start, end)
    }

    def tokens: Tokens = {
      tokenize().slice(pos.start, pos.end)
    }
  }

  @adt.leaf
  class Generated(dialect: Dialect) extends Origin

}
