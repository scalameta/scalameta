package scala.meta.tests.parsers

import scala.meta._
import scala.meta.internal.parsers.{LazyTokenIterator, ScannerTokens}

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

  test("outdents and statement ends") {
    implicit val dialect: Dialect = dialects.Scala3
    val st = ScannerTokens(
      """|object A:
         |  val x = 1
         |val y = 2
         |""".stripMargin.asInput,
    )
    val iter = LazyTokenIterator(st)
    while (iter.hasCurr) iter.next()
    def found(f: Int => Boolean) = st.tokens.indices.filter(f).toList
    // an outdent before `val y`, and a statement that ends at `1` before a newline
    assertEquals(
      found(st.isOutdentAt).map { i =>
        val next = st.nextNonTriviaAt(i)
        s"${at(st, next)} ${at(st, st.nextNonTriviaAt(next))}"
      },
      List("val y"),
    )
    assertEquals(found(st.isStatEolAfter).map(at(st, _)), List("1"))
  }

}
