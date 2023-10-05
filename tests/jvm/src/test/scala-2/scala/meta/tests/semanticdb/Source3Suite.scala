package scala.meta.tests
package semanticdb

import org.scalameta.logger
import scala.meta.interactive.InteractiveSemanticdb._
import scala.meta.internal.semanticdb.Print

import scala.collection.SortedMap
import scala.tools.nsc.interactive.Global
import scala.util.Properties

import munit.FunSuite

class Source3Suite extends FunSuite {

  override def munitIgnore: Boolean = !ScalaVersion.isSupported(minimal212 = 14, minimal213 = 6)

  val compiler: Global = newCompiler(scalacOptions = "-Xsource:3" :: Nil)
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

  val expected = """|interactive.scala
                    |-----------------
                    |
                    |Summary:
                    |Schema => SemanticDB v4
                    |Uri => interactive.scala
                    |Text => non-empty
                    |Language => Scala
                    |Symbols => 4 entries
                    |Occurrences => 16 entries
                    |
                    |Symbols:
                    |b/a. => final object a extends AnyRef { +2 decls }
                    |  AnyRef => scala/AnyRef#
                    |b/a.args. => val method args: List[String]
                    |  List => scala/collection/immutable/List#
                    |  String => scala/Predef.String#
                    |b/a.func(). => method func(args: String*): Nothing
                    |  args => b/a.func().(args)
                    |  String => scala/Predef.String#
                    |  Nothing => scala/Nothing#
                    |b/a.func().(args) => param args: String*
                    |  String => scala/Predef.String#
                    |
                    |Occurrences:
                    |[0:8..0:9): b <= b/
                    |[1:7..1:12): scala => scala/
                    |[1:13..1:23): concurrent => scala/concurrent/
                    |[1:24..1:30): Future => scala/concurrent/Future#
                    |[1:24..1:30): Future => scala/concurrent/Future.
                    |[2:7..2:8): a <= b/a.
                    |[3:6..3:10): func <= b/a.func().
                    |[3:11..3:15): args <= b/a.func().(args)
                    |[3:17..3:23): String => java/lang/String#
                    |[3:28..3:31): ??? => scala/Predef.`???`().
                    |[4:6..4:10): args <= b/a.args.
                    |[4:13..4:17): List => scala/package.List.
                    |[4:18..4:23): empty => scala/collection/immutable/List.empty().
                    |[4:24..4:30): String => scala/Predef.String#
                    |[5:2..5:6): func => b/a.func().
                    |[5:7..5:11): args => b/a.args.""".stripMargin

  check(
    """|package b
       |import scala.concurrent.Future as F
       |object a {
       |  def func(args: String*) = ???
       |  val args = List.empty[String]
       |  func(args*) 
       |}
       |""".stripMargin,
    expected,
    compat = List(
      ScalaVersion.Scala212 ->
        expected.replace("scala/package.List.", "scala/collection/immutable/List.")
    )
  )

}
