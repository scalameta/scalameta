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
    // Note that _root_.scala don't resolve to a symbol, this is a sign that the
    // typer hijacking is not working as expected with interactive.Global.
    """
      |Language:
      |Interactive
      |
      |Names:
      |[8..9): b <= _root_.b.
      |[17..22): scala => _root_.scala.
      |[23..33): concurrent => _root_.scala.concurrent.
      |[34..40): Future => _root_.scala.concurrent.Future#;_root_.scala.concurrent.Future.
      |[48..49): a <= _root_.b.a.
      |[58..59): x <= _root_.b.a.x().
      |[75..79): List => _root_.scala.collection.immutable.List.
      |[84..85): x => _root_.b.a.x().
      |[86..87): + => _root_.scala.Predef.any2stringadd#`+`(String).
      |
      |Messages:
      |[34..40): [warning] Unused import
      |
      |Symbols:
      |_root_.b. => package b
      |_root_.b.a. => final object a
      |_root_.b.a.x(). => val method x: List[Nothing]
      |  [0..4): List => _root_.scala.collection.immutable.List#
      |  [5..12): Nothing => _root_.scala.Nothing#
      |_root_.b.a.x. => private val field x: List[Nothing]
      |  [0..4): List => _root_.scala.collection.immutable.List#
      |  [5..12): Nothing => _root_.scala.Nothing#
      |_root_.scala. => package scala
      |_root_.scala.Predef.any2stringadd#`+`(String). => method +: (other: String): String
      |  [8..14): String => _root_.scala.Predef.String#
      |  [17..23): String => _root_.scala.Predef.String#
      |_root_.scala.collection.immutable.List. => final object List
      |_root_.scala.concurrent. => package concurrent
      |
      |Synthetics:
      |[79..79): *.apply[Nothing]
      |  [0..1): * => _star_.
      |  [2..7): apply => _root_.scala.collection.immutable.List.apply(A*).
      |  [8..15): Nothing => _root_.scala.Nothing#
      |[84..85): scala.Predef.any2stringadd[List[Nothing]](*)
      |  [27..31): List => _root_.scala.collection.immutable.List#
      |  [32..39): Nothing => _root_.scala.Nothing#
      |  [13..26): any2stringadd => _root_.scala.Predef.any2stringadd(A).
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
      |[22..23): a <= (a)
      |[25..27): In => (a)`<error: <none>>`#
      |
      |Messages:
      |[25..27): [error] not found: type In
      |
      |Symbols:
      |(a) => param a: <error>
      |(a)`<error: <none>>`# => class `<error: <none>>`
      |_empty_.b. => final object b
    """.stripMargin
  )
}
