package scala.meta.tests
package semanticdb

import org.scalameta.logger
import org.scalatest.FunSuite
import scala.meta.interactive.InteractiveSemanticdb._
import scala.meta.testkit.DiffAssertions
import scala.tools.nsc.interactive.Global

class InteractiveSuite extends FunSuite with DiffAssertions {
  val compiler: Global = newCompiler(scalacOptions = "-Ywarn-unused-import" :: Nil)
  def check(
      original: String,
      expected: String
  ): Unit = {
    test(logger.revealWhitespace(original)) {
      val options = List("-P:semanticdb:denotations:all", "-P:semanticdb:signatures:all")
      val document = toDocument(compiler, original, options).copy(language = "Interactive")
      assertNoDiff(document.syntax, expected)
    }
  }

  check(
    """package b
      |import scala.concurrent.Future
      |object a {
      |  val x = _root_.scala.List()
      |  x + "string"
      |}
    """.stripMargin,
    // Note that scala don't resolve to a symbol, this is a sign that the
    // typer hijacking is not working as expected with interactive.Global.
    """
      |Language:
      |Interactive
      |
      |Names:
      |[8..9): b <= b.
      |[17..22): scala => scala.
      |[23..33): concurrent => scala.concurrent.
      |[34..40): Future => scala.concurrent.Future#;scala.concurrent.Future.
      |[48..49): a <= b.a.
      |[58..59): x <= b.a.x().
      |[75..79): List => scala.collection.immutable.List.
      |[84..85): x => b.a.x().
      |[86..87): + => scala.Predef.any2stringadd#`+`(String).
      |
      |Messages:
      |[34..40): [warning] Unused import
      |
      |Symbols:
      |b. => package b
      |b.a. => final object a
      |b.a.x(). => val method x: List[Nothing]
      |  [0..4): List => scala.collection.immutable.List#
      |  [5..12): Nothing => scala.Nothing#
      |b.a.x. => private val field x: List[Nothing]
      |  [0..4): List => scala.collection.immutable.List#
      |  [5..12): Nothing => scala.Nothing#
      |scala. => package scala
      |scala.Predef.any2stringadd#`+`(String). => method +: (other: String): String
      |  [8..14): String => scala.Predef.String#
      |  [17..23): String => scala.Predef.String#
      |scala.collection.immutable.List. => final object List
      |scala.concurrent. => package concurrent
      |
      |Synthetics:
      |[79..79): *.apply[Nothing]
      |  [0..1): * => _star_.
      |  [2..7): apply => scala.collection.immutable.List.apply(?).
      |  [8..15): Nothing => scala.Nothing#
      |[84..85): scala.Predef.any2stringadd[List[Nothing]](*)
      |  [27..31): List => scala.collection.immutable.List#
      |  [32..39): Nothing => scala.Nothing#
      |  [13..26): any2stringadd => scala.Predef.any2stringadd(A).
      |  [42..43): * => _star_.
    """.stripMargin
  )

  // This tests a case where SymbolOps.toSemantic crashes
  check(
    """
      |object b {
      |  def add(a: In) = 1
      |}""".stripMargin,
    """
      |Language:
      |Interactive
      |
      |Names:
      |[8..9): b <= _empty_.b.
      |[18..21): add <= _empty_.b.add(?).
      |[22..23): a <= _empty_.b.add(?).(a)
      |[25..27): In => _empty_.b.add(?).(a)`<error: <none>>`#
      |
      |Messages:
      |[25..27): [error] not found: type In
      |
      |Symbols:
      |_empty_.b. => final object b
      |_empty_.b.add(?). => method add: (a: <error>): Int
      |  [14..17): Int => scala.Int#
      |_empty_.b.add(?).(a) => param a: <error>
      |_empty_.b.add(?).(a)`<error: <none>>`# => class `<error: <none>>`
    """.stripMargin
  )
}
