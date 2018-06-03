package scala.meta.tests
package semanticdb

import org.scalameta.logger
import org.scalatest.FunSuite
import scala.meta.interactive.InteractiveSemanticdb._
import scala.meta.testkit.DiffAssertions
import scala.meta.internal.semanticdb3.Print
import scala.tools.nsc.interactive.Global

class InteractiveSuite extends FunSuite with DiffAssertions {
  val compiler: Global = newCompiler(scalacOptions = "-Ywarn-unused-import" :: Nil)
  def check(
      original: String,
      expected: String
  ): Unit = {
    test(logger.revealWhitespace(original)) {
      val options = List("-P:semanticdb:experimental:synthetics:on")
      val document = toTextDocument(compiler, original, options)
      val syntax = Print.document(document)
      assertNoDiff(syntax, expected)
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
      |interactive.scala
      |-----------------
      |
      |Summary:
      |Schema => SemanticDB v3
      |Uri => interactive.scala
      |Text => non-empty
      |Language => Scala
      |Symbols => 3 entries
      |Occurrences => 10 entries
      |Diagnostics => 1 entries
      |Synthetics => 2 entries
      |
      |Symbols:
      |b. => package b
      |b.a. => final object a.{+1 decls}
      |  extends AnyRef
      |b.a.x(). => val method x: : List[Nothing]
      |  List => scala.collection.immutable.List#
      |  Nothing => scala.Nothing#
      |
      |Occurrences:
      |[0:8..0:9): b <= b.
      |[1:7..1:12): scala => scala.
      |[1:13..1:23): concurrent => scala.concurrent.
      |[1:24..1:30): Future => scala.concurrent.Future#
      |[1:24..1:30): Future => scala.concurrent.Future.
      |[2:7..2:8): a <= b.a.
      |[3:6..3:7): x <= b.a.x().
      |[3:23..3:27): List => scala.collection.immutable.List.
      |[4:2..4:3): x => b.a.x().
      |[4:4..4:5): + => scala.Predef.any2stringadd#`+`().
      |
      |Diagnostics:
      |[1:24..1:30) [warning] Unused import
      |
      |Synthetics:
      |[3:27..3:27):  => *.apply[Nothing]
      |  [0:0..0:1): * => _star_.
      |  [0:2..0:7): apply => scala.collection.immutable.List.apply().
      |  [0:8..0:15): Nothing => scala.Nothing#
      |[4:2..4:3): x => scala.Predef.any2stringadd[List[Nothing]](*)
      |  [0:13..0:26): any2stringadd => scala.Predef.any2stringadd().
      |  [0:27..0:31): List => scala.collection.immutable.List#
      |  [0:32..0:39): Nothing => scala.Nothing#
      |  [0:42..0:43): * => _star_.
    """.stripMargin
  )

  // This tests a case where SymbolOps.toSemantic crashes
  check(
    """
      |object b {
      |  def add(a: In) = 1
      |}""".stripMargin,
    """
      |interactive.scala
      |-----------------
      |
      |Summary:
      |Schema => SemanticDB v3
      |Uri => interactive.scala
      |Text => non-empty
      |Language => Scala
      |Symbols => 3 entries
      |Occurrences => 4 entries
      |Diagnostics => 1 entries
      |
      |Symbols:
      |_empty_.b. => final object b.{+1 decls}
      |  extends AnyRef
      |_empty_.b.add(). => method add: (a: <?>): Int
      |  a => _empty_.b.add().(a)
      |  Int => scala.Int#
      |_empty_.b.add().(a) => param a
      |
      |Occurrences:
      |[1:7..1:8): b <= _empty_.b.
      |[2:6..2:9): add <= _empty_.b.add().
      |[2:10..2:11): a <= _empty_.b.add().(a)
      |[2:13..2:15): In => _empty_.b.add().(a)`<error: <none>>`#
      |
      |Diagnostics:
      |[2:13..2:15) [error] not found: type In
    """.stripMargin
  )
}
