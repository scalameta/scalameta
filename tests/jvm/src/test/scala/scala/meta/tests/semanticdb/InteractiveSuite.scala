package scala.meta.tests
package semanticdb

import org.scalameta.logger
import scala.meta.interactive.InteractiveSemanticdb._
import scala.meta.internal.semanticdb.Print

import scala.collection.SortedMap
import scala.tools.nsc.interactive.Global
import scala.util.Properties

import munit.FunSuite

class InteractiveSuite extends FunSuite {
  val option =
    if (ScalaVersion.version.startsWith("2.11")) "-Ywarn-unused-import" else "-Ywarn-unused:imports"
  val compiler: Global = newCompiler(scalacOptions = option :: Nil)
  def check(
      original: String,
      expected: String,
      compat: List[(ScalaVersion.Version, String)] = List.empty
  ): Unit = test(logger.revealWhitespace(original)) {
    val options = List("-P:semanticdb:synthetics:on", "-P:semanticdb:text:on")
    val document = toTextDocument(compiler, original, options)
    val format = scala.meta.metap.Format.Detailed
    val syntax = Print.document(format, document)
    val expectedCompat = ScalaVersion.getExpected(compat, expected)
    assertNoDiff(syntax, expectedCompat)
  }

  def expected(occurrences: Int, listOccurences: String, applySynth: String) =
    s"""|interactive.scala
        |-----------------
        |
        |Summary:
        |Schema => SemanticDB v4
        |Uri => interactive.scala
        |Text => non-empty
        |Language => Scala
        |Symbols => 2 entries
        |Occurrences => $occurrences entries
        |Diagnostics => 1 entries
        |Synthetics => 2 entries
        |
        |Symbols:
        |b/a. => final object a extends AnyRef { +1 decls }
        |  AnyRef => scala/AnyRef#
        |b/a.x. => val method x: List[Nothing]
        |  List => scala/collection/immutable/List#
        |  Nothing => scala/Nothing#
        |
        |Occurrences:
        |[0:8..0:9): b <= b/
        |[1:7..1:12): scala => scala/
        |[1:13..1:23): concurrent => scala/concurrent/
        |[1:24..1:30): Future => scala/concurrent/Future#
        |[1:24..1:30): Future => scala/concurrent/Future.
        |[2:7..2:8): a <= b/a.
        |[3:6..3:7): x <= b/a.x.
        |$listOccurences
        |[4:2..4:3): x => b/a.x.
        |[4:4..4:5): + => scala/Predef.any2stringadd#`+`().
        |
        |Diagnostics:
        |[1:24..1:30) [warning] Unused import
        |
        |Synthetics:
        |[3:10..3:27): _root_.scala.List => *.apply[Nothing]
        |  $applySynth
        |  Nothing => scala/Nothing#
        |[4:2..4:3): x => Predef.any2stringadd[List[Nothing]](*)
        |  Predef => scala/Predef.
        |  any2stringadd => scala/Predef.any2stringadd().
        |  List => scala/collection/immutable/List#
        |  Nothing => scala/Nothing#
    """.stripMargin

  val expectedLatest = expected(
    11,
    """|[3:17..3:22): scala => scala/
       |[3:23..3:27): List => scala/package.List.""".stripMargin,
    "apply => scala/collection/IterableFactory#apply()."
  )

  val expectedPrevious = expected(
    10,
    "[3:23..3:27): List => scala/collection/immutable/List.",
    "apply => scala/collection/immutable/List.apply()."
  )

  val expectedPrevious213 = expected(
    10,
    "[3:23..3:27): List => scala/collection/immutable/List.",
    "apply => scala/collection/IterableFactory#apply()."
  )

  check(
    """|package b
       |import scala.concurrent.Future
       |object a {
       |  val x = _root_.scala.List()
       |  x + "string"
       |}
       |""".stripMargin,
    // Note that scala don't resolve to a symbol, this is a sign that the
    // typer hijacking is not working as expected with interactive.Global.
    expectedPrevious,
    compat = List(
      ScalaVersion.Full("2.13.3") -> expectedPrevious213,
      ScalaVersion.Full("2.13.2") -> expectedPrevious213,
      ScalaVersion.Full("2.13.1") -> expectedPrevious213,
      ScalaVersion.Scala213 -> expectedLatest
    )
  )

  // This tests a case where SymbolOps.toSemantic crashes
  check(
    """|
       |object b {
       |  def add(a: In) = 1
       |}""".stripMargin,
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 3 entries
       |Occurrences => 4 entries
       |Diagnostics => 1 entries
       |
       |Symbols:
       |_empty_/b. => final object b extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/b.add(). => method add(a): Int
       |  a => _empty_/b.add().(a)
       |  Int => scala/Int#
       |_empty_/b.add().(a) => param a
       |
       |Occurrences:
       |[1:7..1:8): b <= _empty_/b.
       |[2:6..2:9): add <= _empty_/b.add().
       |[2:10..2:11): a <= _empty_/b.add().(a)
       |[2:13..2:15): In => local0
       |
       |Diagnostics:
       |[2:13..2:15) [error] not found: type In
       |""".stripMargin
  )
}
