package scala.meta
package syntactic.parsers

import scala.collection.immutable

class TokIterator(tokens: immutable.IndexedSeq[(Tok, Offset)], 
                  private var pos: Int = -1,
                  private var _tok: Tok = null,
                  private var _offset: Offset = 0) extends Iterator[(Tok, Offset)] {
  require(tokens.nonEmpty)
  if (pos == -1) {
    next()
  }  
  def tok: Tok = _tok
  def offset: Offset = _offset
  def hasNext: Boolean = pos + 1 < tokens.size
  def next: (Tok, Offset) = {
    if (!hasNext) throw new NoSuchElementException()
    pos += 1
    val t @ (tok, offset) = tokens(pos)
    _tok = tok
    _offset = offset
    t
  } 
  def fork: TokIterator = new TokIterator(tokens, pos, _tok, _offset)
}
