package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/4902
class Issue4902Suite extends SemanticdbSuite {

  // parents without arguments
  occurrences(
    """|package a
       |trait U; trait V; class K
       |class C1 extends U
       |class C2 extends U // c
       |class C3 extends U {}
       |class C4 extends U with V
       |class C5 extends K
       |class C6 extends K // c
       |class C7 extends K {}
       |class C8 extends K with V
       |class C9 extends K()
       |class C10 // c
       |object O1 extends U // c
       |""".stripMargin,
    """|[0:8..0:9): a <= a/
       |[1:6..1:7): U <= a/U#
       |[1:15..1:16): V <= a/V#
       |[1:24..1:25): K <= a/K#
       |[1:25..1:25):  <= a/K#`<init>`().
       |[2:6..2:8): C1 <= a/C1#
       |[2:9..2:9):  <= a/C1#`<init>`().
       |[2:17..2:18): U => a/U#
       |[2:18..2:18):  => java/lang/Object#`<init>`().
       |[3:6..3:8): C2 <= a/C2#
       |[3:9..3:9):  <= a/C2#`<init>`().
       |[3:17..3:18): U => a/U#
       |[3:18..3:18):  => java/lang/Object#`<init>`().
       |[4:6..4:8): C3 <= a/C3#
       |[4:9..4:9):  <= a/C3#`<init>`().
       |[4:17..4:18): U => a/U#
       |[4:18..4:18):  => java/lang/Object#`<init>`().
       |[5:6..5:8): C4 <= a/C4#
       |[5:9..5:9):  <= a/C4#`<init>`().
       |[5:17..5:18): U => a/U#
       |[5:18..5:18):  => java/lang/Object#`<init>`().
       |[5:24..5:25): V => a/V#
       |[6:6..6:8): C5 <= a/C5#
       |[6:9..6:9):  <= a/C5#`<init>`().
       |[6:17..6:18): K => a/K#
       |[6:18..6:18):  => a/K#`<init>`().
       |[7:6..7:8): C6 <= a/C6#
       |[7:9..7:9):  <= a/C6#`<init>`().
       |[7:17..7:18): K => a/K#
       |[7:18..7:18):  => a/K#`<init>`().
       |[8:6..8:8): C7 <= a/C7#
       |[8:9..8:9):  <= a/C7#`<init>`().
       |[8:17..8:18): K => a/K#
       |[8:18..8:18):  => a/K#`<init>`().
       |[9:6..9:8): C8 <= a/C8#
       |[9:9..9:9):  <= a/C8#`<init>`().
       |[9:17..9:18): K => a/K#
       |[9:18..9:18):  => a/K#`<init>`().
       |[9:24..9:25): V => a/V#
       |[10:6..10:8): C9 <= a/C9#
       |[10:9..10:9):  <= a/C9#`<init>`().
       |[10:17..10:18): K => a/K#
       |[10:18..10:18):  => a/K#`<init>`().
       |[11:6..11:9): C10 <= a/C10#
       |[11:9..11:9):  <= a/C10#`<init>`().
       |[12:7..12:9): O1 <= a/O1.
       |[12:18..12:19): U => a/U#
       |[12:19..12:19):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  // a trait parent on the last line
  occurrences(
    """|package b
       |trait U
       |object O1 extends U
       |""".stripMargin,
    """|[0:8..0:9): b <= b/
       |[1:6..1:7): U <= b/U#
       |[2:7..2:9): O1 <= b/O1.
       |[2:18..2:19): U => b/U#
       |[2:19..2:19):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  // a class parent on the last line
  occurrences(
    """|package c
       |class K
       |class C1 extends K
       |""".stripMargin,
    """|[0:8..0:9): c <= c/
       |[1:6..1:7): K <= c/K#
       |[1:7..1:7):  <= c/K#`<init>`().
       |[2:6..2:8): C1 <= c/C1#
       |[2:9..2:9):  <= c/C1#`<init>`().
       |[2:17..2:18): K => c/K#
       |[2:18..2:18):  => c/K#`<init>`().
       |""".stripMargin,
  )

  // a class on the last line
  occurrences(
    """|package d
       |class C1
       |""".stripMargin,
    """|[0:8..0:9): d <= d/
       |[1:6..1:8): C1 <= d/C1#
       |[1:8..1:8):  <= d/C1#`<init>`().
       |""".stripMargin,
  )

  // instances and annotations
  occurrences(
    """|package e
       |trait U; class K
       |class ann extends scala.annotation.StaticAnnotation
       |object O {
       |  val a = new K // c
       |  val b = new K {}
       |  val c = new U {}
       |  val d = new K with U
       |  @ann val e = 1
       |  val f: Int @ann @ann = 1
       |  @ann // c
       |  val g = 1
       |}
       |""".stripMargin,
    """|[0:8..0:9): e <= e/
       |[1:6..1:7): U <= e/U#
       |[1:15..1:16): K <= e/K#
       |[1:16..1:16):  <= e/K#`<init>`().
       |[2:6..2:9): ann <= e/ann#
       |[2:10..2:10):  <= e/ann#`<init>`().
       |[2:18..2:23): scala => scala/
       |[2:24..2:34): annotation => scala/annotation/
       |[2:35..2:51): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:51..2:51):  => scala/annotation/Annotation#`<init>`().
       |[3:7..3:8): O <= e/O.
       |[4:6..4:7): a <= e/O.a.
       |[4:14..4:15): K => e/K#
       |[4:15..4:15):  => e/K#`<init>`().
       |[5:6..5:7): b <= e/O.b.
       |[5:14..5:14):  <= local0
       |[5:14..5:15): K => e/K#
       |[5:15..5:15):  => e/K#`<init>`().
       |[6:6..6:7): c <= e/O.c.
       |[6:14..6:14):  <= local1
       |[6:14..6:15): U => e/U#
       |[6:15..6:15):  => java/lang/Object#`<init>`().
       |[7:6..7:7): d <= e/O.d.
       |[7:14..7:14):  <= local2
       |[7:14..7:15): K => e/K#
       |[7:15..7:15):  => e/K#`<init>`().
       |[7:21..7:22): U => e/U#
       |[8:3..8:6): ann => e/ann#
       |[8:6..8:6):  => e/ann#`<init>`().
       |[8:11..8:12): e <= e/O.e.
       |[9:6..9:7): f <= e/O.f.
       |[9:9..9:12): Int => scala/Int#
       |[9:14..9:17): ann => e/ann#
       |[9:17..9:17):  => e/ann#`<init>`().
       |[9:19..9:22): ann => e/ann#
       |[9:22..9:22):  => e/ann#`<init>`().
       |[10:3..10:6): ann => e/ann#
       |[10:6..10:6):  => e/ann#`<init>`().
       |[11:6..11:7): g <= e/O.g.
       |""".stripMargin,
  )

  // an instance at the end of a line
  occurrences(
    """|package f
       |class K
       |object O { val a = new K }
       |object P { val a = new K
       |}
       |""".stripMargin,
    """|[0:8..0:9): f <= f/
       |[1:6..1:7): K <= f/K#
       |[1:7..1:7):  <= f/K#`<init>`().
       |[2:7..2:8): O <= f/O.
       |[2:15..2:16): a <= f/O.a.
       |[2:23..2:24): K => f/K#
       |[2:24..2:24):  => f/K#`<init>`().
       |[3:7..3:8): P <= f/P.
       |[3:15..3:16): a <= f/P.a.
       |[3:23..3:24): K => f/K#
       |[3:24..3:24):  => f/K#`<init>`().
       |""".stripMargin,
  )

}
