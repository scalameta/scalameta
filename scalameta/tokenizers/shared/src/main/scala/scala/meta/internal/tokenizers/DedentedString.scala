package scala.meta.internal.tokenizers

/* SIP-72 dedented string literals: strip the first newline after the opening
 * quotes, the final newline with the closing line's whitespace, and the
 * closing line's indentation from every other line; normalize line ends to
 * LF. Trimming is lenient: a piece that does not match the rules is kept as
 * is, so invalid code still gets a best-effort value. */
private[meta] object DedentedString {

  @inline
  private def isBlank(c: Char) = c == ' ' || c == '\t'
  @inline
  private def isLineEnd(c: Char) = c == '\n' || c == '\r'

  /** The whitespace before the closing quotes, taken off the last part. */
  def closingIndent(lastPart: String): String = {
    var i = lastPart.length - 1
    while (i >= 0 && isBlank(lastPart(i))) i -= 1
    lastPart.substring(i + 1)
  }

  def trim(content: String): String =
    trimPart(content, closingIndent(content), isFirst = true, isLast = true)

  /* Trim one literal segment of an interpolation; a plain literal is a single
   * segment which is both first and last. Non-first segments begin mid-line,
   * right after a splice. */
  def trimPart(content: String, indent: String, isFirst: Boolean, isLast: Boolean): String = {
    val length = content.length

    var bodyStart = 0
    if (isFirst) {
      while (bodyStart < length && isBlank(content(bodyStart))) bodyStart += 1
      if (bodyStart < length && isLineEnd(content(bodyStart))) {
        if (content(bodyStart) == '\r' && bodyStart + 1 < length && content(bodyStart + 1) == '\n')
          bodyStart += 1
        bodyStart += 1
      } else bodyStart = 0 // no newline after the opening quotes: keep the line
    }

    var bodyEnd = length
    if (isLast) {
      var lastNonBlank = length - 1
      while (lastNonBlank >= 0 && isBlank(content(lastNonBlank))) lastNonBlank -= 1
      if (lastNonBlank >= 0 && isLineEnd(content(lastNonBlank))) {
        bodyEnd = lastNonBlank
        if (content(bodyEnd) == '\n' && bodyEnd > 0 && content(bodyEnd - 1) == '\r') bodyEnd -= 1
        if (bodyEnd < bodyStart) return "" // empty literal: both cuts share the newline
      } // otherwise text touches the closing quotes: keep the line
    }

    val sb = new java.lang.StringBuilder(bodyEnd - bodyStart)
    var i = bodyStart
    var lineStart = isFirst
    while (i < bodyEnd)
      if (lineStart) {
        lineStart = false
        // a line that does not start with the indent is kept as is
        if (content.startsWith(indent, i)) i += indent.length
      } else {
        val c = content(i)
        i += 1
        if (c == '\n') {
          sb.append('\n')
          lineStart = true
        } else if (c == '\r') {
          if (i < bodyEnd && content(i) == '\n') i += 1
          sb.append('\n')
          lineStart = true
        } else sb.append(c)
      }
    sb.toString
  }

}
