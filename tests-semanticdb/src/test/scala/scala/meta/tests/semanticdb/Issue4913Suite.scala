package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/pull/4913
class Issue4913Suite extends SemanticdbSuite {

  // kinds of definitions before a comment
  occurrences(
    """|package a
       |class C1 // c
       |case class C2() // c
       |trait T1 // c
       |object O1 // c
       |class C3() // c
       |class C4 { } // c
       |""".stripMargin,
    """|[0:8..0:9): a <= a/
       |[1:6..1:8): C1 <= a/C1#
       |[2:11..2:13): C2 <= a/C2#
       |[2:13..2:13):  <= a/C2#`<init>`().
       |[3:6..3:8): T1 <= a/T1#
       |[4:7..4:9): O1 <= a/O1.
       |[5:6..5:8): C3 <= a/C3#
       |[5:8..5:8):  <= a/C3#`<init>`().
       |[6:6..6:8): C4 <= a/C4#
       |[6:9..6:9):  <= a/C4#`<init>`().
       |""".stripMargin,
  )

  // classes with type parameters, modifiers and annotations
  occurrences(
    """|package b
       |class C1[T] // c
       |final class C2 // c
       |@deprecated class C3 // c
       |class C4 private ; // c
       |class C5 private[b] // c
       |class C6 @deprecated() // c
       |abstract class C7 // c
       |sealed abstract class C8[+T] // c
       |""".stripMargin,
    """|[0:8..0:9): b <= b/
       |[1:6..1:8): C1 <= b/C1#
       |[1:9..1:10): T <= b/C1#[T]
       |[2:12..2:14): C2 <= b/C2#
       |[3:1..3:11): deprecated => scala/deprecated#
       |[3:11..3:11):  => scala/deprecated#`<init>`().
       |[3:18..3:20): C3 <= b/C3#
       |[4:6..4:8): C4 <= b/C4#
       |[4:17..4:17):  <= b/C4#`<init>`().
       |[5:6..5:8): C5 <= b/C5#
       |[5:17..5:18): b => b/
       |[5:24..5:24):  <= b/C5#`<init>`().
       |[6:6..6:8): C6 <= b/C6#
       |[6:10..6:20): deprecated => scala/deprecated#
       |[6:20..6:20):  => scala/deprecated#`<init>`().
       |[6:27..6:27):  <= b/C6#`<init>`().
       |[7:15..7:17): C7 <= b/C7#
       |[8:22..8:24): C8 <= b/C8#
       |[8:26..8:27): T <= b/C8#[T]
       |""".stripMargin,
  )

  // what follows a class
  occurrences(
    """|package c
       |class C1
       |
       |class C2
       |class C3 /* c */
       |class C4
       |// c
       |class C5
       |/** c */
       |class C6
       |""".stripMargin,
    """|[0:8..0:9): c <= c/
       |[1:6..1:8): C1 <= c/C1#
       |[1:8..1:8):  <= c/C1#`<init>`().
       |[3:6..3:8): C2 <= c/C2#
       |[3:8..3:8):  <= c/C2#`<init>`().
       |[4:6..4:8): C3 <= c/C3#
       |[5:6..5:8): C4 <= c/C4#
       |[5:8..5:8):  <= c/C4#`<init>`().
       |[7:6..7:8): C5 <= c/C5#
       |[7:8..7:8):  <= c/C5#`<init>`().
       |[9:6..9:8): C6 <= c/C6#
       |""".stripMargin,
  )

  // classes before a semicolon or a brace
  occurrences(
    """|package d
       |class C1 /* c */ ; class C2
       |class C3 ; class C4 ;
       |object O { class C5 }
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:8..0:9): d <= d/
         |[1:6..1:8): C1 <= d/C1#
         |[1:17..1:17):  <= d/C1#`<init>`().
         |[1:25..1:27): C2 <= d/C2#
         |[1:27..1:27):  <= d/C2#`<init>`().
         |[2:6..2:8): C3 <= d/C3#
         |[2:9..2:9):  <= d/C3#`<init>`().
         |[2:17..2:19): C4 <= d/C4#
         |[2:20..2:20):  <= d/C4#`<init>`().
         |[3:7..3:8): O <= d/O.
         |[3:17..3:19): C5 <= d/O.C5#
         |[3:20..3:20):  <= d/O.C5#`<init>`().
         |""".stripMargin
    else
      """|[0:8..0:9): d <= d/
         |[1:6..1:8): C1 <= d/C1#
         |[1:25..1:27): C2 <= d/C2#
         |[1:27..1:27):  <= d/C2#`<init>`().
         |[2:6..2:8): C3 <= d/C3#
         |[2:17..2:19): C4 <= d/C4#
         |[3:7..3:8): O <= d/O.
         |[3:17..3:19): C5 <= d/O.C5#
         |""".stripMargin,
  )

  // a class at the end of a file without a newline
  occurrences(
    """|package e
       |class C1""".stripMargin,
    """|[0:8..0:9): e <= e/
       |[1:6..1:8): C1 <= e/C1#
       |[1:8..1:8):  <= e/C1#`<init>`().
       |""".stripMargin,
  )

  // a class before a comment at the end of a file
  occurrences(
    """|package f
       |class C1 // c""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:8..0:9): f <= f/
         |[1:6..1:8): C1 <= f/C1#
         |[1:13..1:13):  <= f/C1#`<init>`().
         |""".stripMargin
    else
      """|[0:8..0:9): f <= f/
         |[1:6..1:8): C1 <= f/C1#
         |""".stripMargin,
  )

  // nested classes
  occurrences(
    """|package g
       |object O {
       |  class C1 // c
       |  class C2
       |}
       |class K {
       |  class C3 // c
       |  def m = { class C4 // c
       |    new C4 }
       |}
       |""".stripMargin,
    """|[0:8..0:9): g <= g/
       |[1:7..1:8): O <= g/O.
       |[2:8..2:10): C1 <= g/O.C1#
       |[3:8..3:10): C2 <= g/O.C2#
       |[3:10..3:10):  <= g/O.C2#`<init>`().
       |[5:6..5:7): K <= g/K#
       |[5:8..5:8):  <= g/K#`<init>`().
       |[6:8..6:10): C3 <= g/K#C3#
       |[7:6..7:7): m <= g/K#m().
       |[7:18..7:20): C4 <= local0
       |[8:8..8:10): C4 => local0
       |[8:10..8:10):  => local1
       |""".stripMargin,
  )

  // a class in a package object
  occurrences(
    """|package h
       |package object p {
       |  class C1 // c
       |}
       |""".stripMargin,
    """|[0:8..0:9): h <= h/
       |[1:15..1:16): p <= h/p/package.
       |[2:8..2:10): C1 <= h/p/package.C1#
       |""".stripMargin,
  )

  // templates on the next line
  occurrences(
    """|package b
       |class C1 // c
       |  extends AnyRef
       |class C2 // c
       |{ }
       |class C3 private // c
       |  extends AnyRef
       |""".stripMargin,
    """|[0:8..0:9): b <= b/
       |[1:6..1:8): C1 <= b/C1#
       |[2:2..2:2):  <= b/C1#`<init>`().
       |[2:10..2:16): AnyRef => scala/AnyRef#
       |[2:16..2:16):  => java/lang/Object#`<init>`().
       |[3:6..3:8): C2 <= b/C2#
       |[5:6..5:8): C3 <= b/C3#
       |[6:2..6:2):  <= b/C3#`<init>`().
       |[6:10..6:16): AnyRef => scala/AnyRef#
       |[6:16..6:16):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

}
