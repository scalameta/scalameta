package scala.meta.syntactic
package parsers

import scala.collection.immutable

class TokIterator(tokens: immutable.IndexedSeq[Tok],
                  private var pos: Int = -1,
                  private var _tok: Tok = null) extends Iterator[Tok] {
  require(tokens.nonEmpty)
  if (pos == -1) {
    next()
  }
  def tok: Tok = _tok
  def hasNext: Boolean = pos + 1 < tokens.size
  def next: Tok = {
    if (!hasNext) throw new NoSuchElementException()
    pos += 1
    _tok = tokens(pos)
    _tok
  }
  def fork: TokIterator = new TokIterator(tokens, pos, _tok)
}
