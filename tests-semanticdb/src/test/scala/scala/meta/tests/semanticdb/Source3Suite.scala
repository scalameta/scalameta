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
      compat: List[(ScalaVersion.Version, String)] = List.empty,
  )(implicit loc: munit.Location): Unit = test(logger.revealWhitespace(original)) {
    val options = List("-P:semanticdb:synthetics:on", "-P:semanticdb:text:on")
    val document = toTextDocument(compiler, original, options)
    val format = scala.meta.metap.Format.Detailed
    val syntax = Print.document(format, document)
    val expectedCompat = ScalaVersion.getExpected(compat, expected)
    assertNoDiff(syntax, expectedCompat)
  }

  val expected =
    """|interactive.scala
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
        expected.replace("scala/package.List.", "scala/collection/immutable/List."),
    ),
  )

  check(
    """|package b
       |object a {
       |  for { (x, y) <- Option((1, 2)) } yield x + y
       |}
       |""".stripMargin,
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 4 entries
       |Occurrences => 8 entries
       |Synthetics => 2 entries
       |
       |Symbols:
       |b/a. => final object a extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local1 => val local x: Int
       |  Int => scala/Int#
       |local2 => val local y: Int
       |  Int => scala/Int#
       |
       |Occurrences:
       |[0:8..0:9): b <= b/
       |[1:7..1:8): a <= b/a.
       |[2:9..2:10): x <= local1
       |[2:12..2:13): y <= local2
       |[2:18..2:24): Option => scala/Option.
       |[2:41..2:42): x => local1
       |[2:43..2:44): + => scala/Int#`+`(+4).
       |[2:45..2:46): y => local2
       |
       |Synthetics:
       |[2:2..2:46): for { (x, y) <- Option((1, 2)) } yield x + y => orig(Option((1, 2))).map[Int]({(x$1) => orig(x + y)})
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |  x$1 => local0
       |[2:18..2:24): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#""".stripMargin,
  )

  check(
    """|package b
       |object c {
       |  for { case (x, y) <- Option((1, 2)) } yield x + y
       |  for { case Some(x) <- Option(Option(1)) } yield x
       |}
       |""".stripMargin,
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 6 entries
       |Occurrences => 13 entries
       |Synthetics => 5 entries
       |
       |Symbols:
       |b/c. => final object c extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local1 => val local x: Int
       |  Int => scala/Int#
       |local2 => val local y: Int
       |  Int => scala/Int#
       |local3 => synthetic param x$2: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local4 => val local x: Int
       |  Int => scala/Int#
       |
       |Occurrences:
       |[0:8..0:9): b <= b/
       |[1:7..1:8): c <= b/c.
       |[2:14..2:15): x <= local1
       |[2:17..2:18): y <= local2
       |[2:23..2:29): Option => scala/Option.
       |[2:46..2:47): x => local1
       |[2:48..2:49): + => scala/Int#`+`(+4).
       |[2:50..2:51): y => local2
       |[3:13..3:17): Some => scala/Some.
       |[3:18..3:19): x <= local4
       |[3:24..3:30): Option => scala/Option.
       |[3:31..3:37): Option => scala/Option.
       |[3:50..3:51): x => local4
       |
       |Synthetics:
       |[2:2..2:51): for { case (x, y) <- Option((1, 2)) } yield x + y => orig(Option((1, 2))).map[Int]({(x$1) => orig(x + y)})
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |  x$1 => local0
       |[2:23..2:29): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |[3:2..3:51): for { case Some(x) <- Option(Option(1)) } yield x => orig(Option(Option(1))).map[Int]({(x$2) => orig(x)})
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |  x$2 => local3
       |[3:24..3:30): Option => *.apply[Option[Int]]
       |  apply => scala/Option.apply().
       |  Option => scala/Option#
       |  Int => scala/Int#
       |[3:31..3:37): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |""".stripMargin,
  )

}
