package scala.meta.tests.parsers

import scala.meta._
import scala.meta.internal.parsers.ScannerTokens

import MoreHelpers._

class ScannerTokensSuite extends ParseSuite {

  private def tokensOf(code: String): ScannerTokens = {
    implicit val dialect: Dialect = dialects.Scala213
    ScannerTokens(code.asInput)
  }

  // the text of the token at `idx`, or a marker past either end
  private def at(st: ScannerTokens, idx: Int): String =
    if (idx < 0) "<before>" else if (idx >= st.tokens.length) "<after>" else st.tokens(idx).text

  test("next and previous tokens past trivia") {
    val st = tokensOf(
      """|a /* c */
         |  b
         |""".stripMargin,
    )
    val a = st.tokens.indexWhere(_.text == "a")
    val b = st.tokens.indexWhere(_.text == "b")
    assertEquals(at(st, st.nextNonHTriviaAt(a)), "\n")
    assertEquals(st.nextNonHTrivia(a).text, "\n")
    assertEquals(at(st, st.nextNonTriviaAt(a)), "b")
    assertEquals(at(st, st.nextNonWhitespaceAt(a)), "/* c */")
    assertEquals(at(st, st.prevNonHTriviaAt(b)), "\n")
    assertEquals(at(st, st.prevNonTriviaAt(b)), "a")
    assertEquals(at(st, st.prevNonWhitespaceAt(b)), "/* c */")
  }

  test("next and previous tokens at the ends of the input") {
    val st = tokensOf("a")
    assertEquals(at(st, st.prevNonTriviaAt(0)), "<before>")
    assertEquals(st.nextNonTriviaAt(st.tokens.length - 1), st.tokens.length - 1)
  }

}
