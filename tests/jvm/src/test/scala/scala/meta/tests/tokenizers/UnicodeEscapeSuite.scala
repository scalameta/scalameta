package scala.meta.tests.tokenizers

import org.scalameta.logger
import scala.meta.internal.io.InputStreamIO

import java.nio.charset.StandardCharsets

class UnicodeEscapeSuite extends BaseTokenizerSuite {
  // Read tests from external file because scalac processes string literals in source
  // @ """ s"${x}\uef17" """.length
  // res0: Int = 10
  // by reading from external file escapes like `\uef17` are represented
  // as 6 characters instead of one.
  val tests = new String(
    InputStreamIO.readBytes(this.getClass.getClassLoader.getResourceAsStream("unicode.txt")),
    StandardCharsets.UTF_8
  )

  // asserts that tokenize(code).syntax == code
  def checkRoundtrip(original: String): Unit = test(logger.revealWhitespace(original)) {
    val tokens = tokenize(original)
    val obtained = tokens.toString
    assertNoDiff(obtained, original)
  }

  tests.linesIterator.foreach(line => checkRoundtrip(line))

}
