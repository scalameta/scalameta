package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1789
class Issue1789Suite extends SemanticdbSuite {

  occurrences(
    """|package a
       |@example.MacroAnnotation
       |class A
       |object A
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:8..0:9): a <= a/
         |[1:1..1:8): example => a/A#
         |""".stripMargin
    else
      """|[0:8..0:9): a <= a/
         |[1:1..1:8): example => a/A#
         |[2:7..2:7):  <= a/A#`<init>`().
         |""".stripMargin,
  )

  occurrences(
    """|package b
       |import example.MacroAnnotation
       |@MacroAnnotation() class B[T](x: T)
       |object B
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:8..0:9): b <= b/
         |[1:7..1:14): example => example/
         |[1:15..1:30): MacroAnnotation => example/MacroAnnotation#
         |[1:15..1:30): MacroAnnotation => example/MacroAnnotation.
         |[2:1..2:16): MacroAnnotation => b/B#
         |[2:27..2:28): T <= b/B#[T]
         |[2:30..2:31): x <= b/B#`<init>`().(x)
         |[2:30..2:31): x <= b/B#x.
         |[2:33..2:34): T => b/B#[T]
         |""".stripMargin
    else
      """|[0:8..0:9): b <= b/
         |[1:7..1:14): example => example/
         |[1:15..1:30): MacroAnnotation => example/MacroAnnotation#
         |[1:15..1:30): MacroAnnotation => example/MacroAnnotation.
         |[2:1..2:16): MacroAnnotation => b/B#
         |[2:27..2:28): T <= b/B#[T]
         |[2:29..2:29):  <= b/B#`<init>`().
         |[2:30..2:31): x <= b/B#`<init>`().(x)
         |[2:30..2:31): x <= b/B#x.
         |[2:33..2:34): T => b/B#[T]
         |""".stripMargin,
  )

  occurrences(
    """|package c
       |@org.scalameta.data.data
       |class C(a: Int)
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:8..0:9): c <= c/
         |[1:1..1:4): org => c/C#
         |[1:24..1:24):  => java/lang/IndexOutOfBoundsException#`<init>`(+1).
         |[2:11..2:14): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:8..0:9): c <= c/
         |[1:1..1:4): org => c/C#
         |[1:24..1:24):  => java/lang/IndexOutOfBoundsException#`<init>`(+1).
         |[2:7..2:7):  <= c/C#`<init>`().
         |[2:11..2:14): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    """|package d
       |@deprecated @example.MacroAnnotation class D
       |object D
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:8..0:9): d <= d/
         |[1:1..1:11): deprecated => scala/deprecated#
         |[1:11..1:11):  => scala/deprecated#`<init>`().
         |[1:13..1:20): example => d/D#
         |""".stripMargin
    else
      """|[0:8..0:9): d <= d/
         |[1:1..1:11): deprecated => scala/deprecated#
         |[1:11..1:11):  => scala/deprecated#`<init>`().
         |[1:13..1:20): example => d/D#
         |[1:44..1:44):  <= d/D#`<init>`().
         |""".stripMargin,
  )

  occurrences(
    """|package e
       |@deprecated class E
       |object E
       |""".stripMargin,
    """|[0:8..0:9): e <= e/
       |[1:1..1:11): deprecated => scala/deprecated#
       |[1:11..1:11):  => scala/deprecated#`<init>`().
       |[1:18..1:19): E <= e/E#
       |[1:19..1:19):  <= e/E#`<init>`().
       |[2:7..2:8): E <= e/E.
       |""".stripMargin,
  )

  occurrences(
    """|package f
       |object F {
       |  @example.MacroAnnotation class G
       |  object G
       |  val g = new G().classNumber + G.objectNumber
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:8..0:9): f <= f/
         |[1:7..1:8): F <= f/F.
         |[2:3..2:10): example => f/F.G#
         |[4:6..4:7): g <= f/F.g.
         |[4:14..4:15): G => f/F.G#
         |[4:15..4:15):  => f/F.G#`<init>`().
         |[4:18..4:29): classNumber => f/F.G#classNumber().
         |[4:30..4:31): + => scala/Int#`+`(+4).
         |[4:32..4:33): G => f/F.G.
         |[4:34..4:46): objectNumber => f/F.G.objectNumber().
         |""".stripMargin
    else
      """|[0:8..0:9): f <= f/
         |[1:7..1:8): F <= f/F.
         |[2:3..2:10): example => f/F.G#
         |[2:34..2:34):  <= f/F.G#`<init>`().
         |[4:6..4:7): g <= f/F.g.
         |[4:14..4:15): G => f/F.G#
         |[4:15..4:15):  => f/F.G#`<init>`().
         |[4:18..4:29): classNumber => f/F.G#classNumber().
         |[4:30..4:31): + => scala/Int#`+`(+4).
         |[4:32..4:33): G => f/F.G.
         |[4:34..4:46): objectNumber => f/F.G.objectNumber().
         |""".stripMargin,
  )

  occurrences(
    """|package r
       |@org.scalameta.adt.root trait R
       |""".stripMargin,
    """|[0:8..0:9): r <= r/
       |[1:1..1:4): org => org/scalameta/adt/Metadata.root#
       |[1:23..1:23):  => org/scalameta/adt/Metadata.root#`<init>`().
       |""".stripMargin,
  )

}
