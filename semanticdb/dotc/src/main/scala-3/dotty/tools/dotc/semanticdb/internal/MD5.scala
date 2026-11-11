package dotty.tools.dotc.semanticdb.internal

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object MD5 {

  private val HexDigits = "0123456789ABCDEF"

  /** Returns the MD5 digest of the UTF-8 bytes of a string, in upper-case hex. */
  def compute(string: String): String = {
    val bytes = string.getBytes(StandardCharsets.UTF_8)
    val digest = MessageDigest.getInstance("MD5").nn.digest(bytes).nn
    val chars = new Array[Char](2 * digest.length)
    var i = 0
    while (i < digest.length) {
      chars(2 * i) = HexDigits.charAt(digest(i) >> 4 & 0xf)
      chars(2 * i + 1) = HexDigits.charAt(digest(i) & 0xf)
      i += 1
    }
    new String(chars)
  }

}
