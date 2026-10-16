package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1916
class Issue1916Suite extends SemanticdbSuite {

  occurrences(
    """|package a
       |trait E
       |class C { this: E => }
       |""".stripMargin,
    """|[0:8..0:9): a <= a/
       |[1:6..1:7): E <= a/E#
       |[2:6..2:7): C <= a/C#
       |[2:8..2:8):  <= a/C#`<init>`().
       |[2:16..2:17): E => a/E#
       |""".stripMargin,
  )

  occurrences(
    """|package b
       |class C { this: String => }
       |""".stripMargin,
    """|[0:8..0:9): b <= b/
       |[1:6..1:7): C <= b/C#
       |[1:8..1:8):  <= b/C#`<init>`().
       |[1:16..1:22): String => scala/Predef.String#
       |""".stripMargin,
  )

  occurrences(
    """|package c
       |class C { this: Map[Int, String] => }
       |""".stripMargin,
    """|[0:8..0:9): c <= c/
       |[1:6..1:7): C <= c/C#
       |[1:8..1:8):  <= c/C#`<init>`().
       |[1:16..1:19): Map => scala/Predef.Map#
       |[1:20..1:23): Int => scala/Int#
       |[1:25..1:31): String => scala/Predef.String#
       |""".stripMargin,
  )

  occurrences(
    """|package c2
       |class C { this: Map[_, Int] => }
       |""".stripMargin,
    """|[0:8..0:10): c2 <= c2/
       |[1:6..1:7): C <= c2/C#
       |[1:8..1:8):  <= c2/C#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    """|package d
       |trait A
       |trait B
       |class C { self: A with B => }
       |""".stripMargin,
    """|[0:8..0:9): d <= d/
       |[1:6..1:7): A <= d/A#
       |[2:6..2:7): B <= d/B#
       |[3:6..3:7): C <= d/C#
       |[3:8..3:8):  <= d/C#`<init>`().
       |[3:10..3:14): self <= local0
       |[3:16..3:17): A => d/A#
       |[3:23..3:24): B => d/B#
       |""".stripMargin,
  )

  occurrences(
    """|package d2
       |trait A
       |class C { self: A with Map[Int, String] => }
       |""".stripMargin,
    """|[0:8..0:10): d2 <= d2/
       |[1:6..1:7): A <= d2/A#
       |[2:6..2:7): C <= d2/C#
       |[2:8..2:8):  <= d2/C#`<init>`().
       |[2:10..2:14): self <= local0
       |[2:16..2:17): A => d2/A#
       |[2:23..2:26): Map => scala/Predef.Map#
       |[2:27..2:30): Int => scala/Int#
       |[2:32..2:38): String => scala/Predef.String#
       |""".stripMargin,
  )

  occurrences(
    """|package e
       |trait A { type T }
       |class C { self: A { type T = Int } => }
       |""".stripMargin,
    """|[0:8..0:9): e <= e/
       |[1:6..1:7): A <= e/A#
       |[1:15..1:16): T <= e/A#T#
       |[2:6..2:7): C <= e/C#
       |[2:8..2:8):  <= e/C#`<init>`().
       |[2:10..2:14): self <= local1
       |[2:16..2:17): A => e/A#
       |[2:25..2:26): T <= local0
       |[2:29..2:32): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    """|package f
       |class C { this: scala.collection.immutable.Seq[Int] => }
       |""".stripMargin,
    """|[0:8..0:9): f <= f/
       |[1:6..1:7): C <= f/C#
       |[1:8..1:8):  <= f/C#`<init>`().
       |[1:16..1:21): scala => scala/
       |[1:22..1:32): collection => scala/collection/
       |[1:33..1:42): immutable => scala/collection/immutable/
       |[1:43..1:46): Seq => scala/collection/immutable/Seq#
       |[1:47..1:50): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    """|package g
       |class O { trait T }
       |class C { this: O#T => }
       |""".stripMargin,
    """|[0:8..0:9): g <= g/
       |[1:6..1:7): O <= g/O#
       |[1:8..1:8):  <= g/O#`<init>`().
       |[1:16..1:17): T <= g/O#T#
       |[2:6..2:7): C <= g/C#
       |[2:8..2:8):  <= g/C#`<init>`().
       |[2:16..2:17): O => g/O#
       |[2:18..2:19): T => g/O#T#
       |""".stripMargin,
  )

  occurrences(
    """|package h
       |trait T[X] { this: List[X] => }
       |object O { this: Product => }
       |""".stripMargin,
    """|[0:8..0:9): h <= h/
       |[1:6..1:7): T <= h/T#
       |[1:8..1:9): X <= h/T#[X]
       |[1:19..1:23): List => scala/package.List#
       |[1:24..1:25): X => h/T#[X]
       |[2:7..2:8): O <= h/O.
       |[2:17..2:24): Product => scala/Product#
       |""".stripMargin,
  )

  occurrences(
    """|package i
       |object O {
       |  classOf[Int]
       |  classOf[scala.Int]
       |}
       |""".stripMargin,
    """|[0:8..0:9): i <= i/
       |[1:7..1:8): O <= i/O.
       |[2:2..2:9): classOf => scala/Predef.classOf().
       |[2:10..2:13): Int => scala/Int#
       |[3:2..3:9): classOf => scala/Predef.classOf().
       |[3:10..3:19): scala.Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    """|package j
       |object O {
       |  classOf[List[Int]]
       |  classOf[Array[Int]]
       |  classOf[Map[String, List[Int]]]
       |}
       |""".stripMargin,
    """|[0:8..0:9): j <= j/
       |[1:7..1:8): O <= j/O.
       |[2:2..2:9): classOf => scala/Predef.classOf().
       |[3:2..3:9): classOf => scala/Predef.classOf().
       |[4:2..4:9): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  occurrences(
    """|package k
       |class O { trait T }
       |object O {
       |  classOf[scala.collection.immutable.List[Int]]
       |  classOf[O#T]
       |}
       |""".stripMargin,
    """|[0:8..0:9): k <= k/
       |[1:6..1:7): O <= k/O#
       |[1:8..1:8):  <= k/O#`<init>`().
       |[1:16..1:17): T <= k/O#T#
       |[2:7..2:8): O <= k/O.
       |[3:2..3:9): classOf => scala/Predef.classOf().
       |[4:2..4:9): classOf => scala/Predef.classOf().
       |[4:10..4:13): O#T => k/O#T#
       |""".stripMargin,
  )

  occurrences(
    """|package l
       |object O {
       |  classOf[(Int, String)]
       |  classOf[Int => String]
       |  classOf[List[_]]
       |}
       |""".stripMargin,
    """|[0:8..0:9): l <= l/
       |[1:7..1:8): O <= l/O.
       |[2:2..2:9): classOf => scala/Predef.classOf().
       |[3:2..3:9): classOf => scala/Predef.classOf().
       |[4:2..4:9): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

}
