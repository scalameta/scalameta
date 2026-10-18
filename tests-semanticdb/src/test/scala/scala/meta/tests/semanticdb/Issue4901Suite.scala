package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/4901
class Issue4901Suite extends SemanticdbSuite {

  // super in a trait
  occurrences(
    """|package a
       |trait T0 { def z: Int = 1 }
       |trait T1 { def x: Int = 1 }
       |trait T2 extends T1 { def y: Int = super.x }
       |trait T3 extends T1 { def y: Int = super[T1].x }
       |trait T4 extends T0 with T1 { def y: Int = super.x + super.z }
       |trait T5 extends T1 { override def x: Int = super.x }
       |trait T6 extends T1 { def y: Int = super.hashCode }
       |""".stripMargin,
    """|[0:8..0:9): a <= a/
       |[1:6..1:8): T0 <= a/T0#
       |[1:9..1:9):  <= a/T0#`<init>`().
       |[1:15..1:16): z <= a/T0#z().
       |[1:18..1:21): Int => scala/Int#
       |[2:6..2:8): T1 <= a/T1#
       |[2:9..2:9):  <= a/T1#`<init>`().
       |[2:15..2:16): x <= a/T1#x().
       |[2:18..2:21): Int => scala/Int#
       |[3:6..3:8): T2 <= a/T2#
       |[3:9..3:9):  <= a/T2#`<init>`().
       |[3:17..3:19): T1 => a/T1#
       |[3:26..3:27): y <= a/T2#y().
       |[3:29..3:32): Int => scala/Int#
       |[3:35..3:35):  => a/T2#
       |[3:40..3:40):  => a/T1#
       |[3:41..3:42): x => a/T1#x().
       |[4:6..4:8): T3 <= a/T3#
       |[4:9..4:9):  <= a/T3#`<init>`().
       |[4:17..4:19): T1 => a/T1#
       |[4:26..4:27): y <= a/T3#y().
       |[4:29..4:32): Int => scala/Int#
       |[4:35..4:35):  => a/T3#
       |[4:41..4:43): T1 => a/T1#
       |[4:45..4:46): x => a/T1#x().
       |[5:6..5:8): T4 <= a/T4#
       |[5:9..5:9):  <= a/T4#`<init>`().
       |[5:17..5:19): T0 => a/T0#
       |[5:25..5:27): T1 => a/T1#
       |[5:34..5:35): y <= a/T4#y().
       |[5:37..5:40): Int => scala/Int#
       |[5:43..5:43):  => a/T4#
       |[5:48..5:48):  => a/T1#
       |[5:49..5:50): x => a/T1#x().
       |[5:51..5:52): + => scala/Int#`+`(+4).
       |[5:53..5:53):  => a/T4#
       |[5:58..5:58):  => a/T0#
       |[5:59..5:60): z => a/T0#z().
       |[6:6..6:8): T5 <= a/T5#
       |[6:9..6:9):  <= a/T5#`<init>`().
       |[6:17..6:19): T1 => a/T1#
       |[6:35..6:36): x <= a/T5#x().
       |[6:38..6:41): Int => scala/Int#
       |[6:44..6:44):  => a/T5#
       |[6:49..6:49):  => a/T1#
       |[6:50..6:51): x => a/T1#x().
       |[7:6..7:8): T6 <= a/T6#
       |[7:9..7:9):  <= a/T6#`<init>`().
       |[7:17..7:19): T1 => a/T1#
       |[7:26..7:27): y <= a/T6#y().
       |[7:29..7:32): Int => scala/Int#
       |[7:35..7:35):  => a/T6#
       |[7:40..7:40):  => java/lang/Object#
       |[7:41..7:49): hashCode => java/lang/Object#hashCode().
       |""".stripMargin,
  )

  // super in a class
  occurrences(
    """|package b
       |class K { def w: Int = 1 }
       |trait T1 { def x: Int = 1 }
       |trait T2 extends T1
       |class C1 extends T1 { def y: Int = super.x }
       |class C2 extends K with T1 { def y: Int = super.x + super.w }
       |class C3 extends T1 { def y: Int = C3.super.x }
       |class C4 extends T1 { def y: Int = super.hashCode }
       |class C5 { def y: Int = super.hashCode }
       |class C6 extends T2 { def y: Int = super.x }
       |class C7 extends K with T1 { def y: Int = super[T1].x + super[K].w }
       |object O { val o = new K with T1 { def y: Int = super.x } }
       |""".stripMargin,
    """|[0:8..0:9): b <= b/
       |[1:6..1:7): K <= b/K#
       |[1:8..1:8):  <= b/K#`<init>`().
       |[1:14..1:15): w <= b/K#w().
       |[1:17..1:20): Int => scala/Int#
       |[2:6..2:8): T1 <= b/T1#
       |[2:9..2:9):  <= b/T1#`<init>`().
       |[2:15..2:16): x <= b/T1#x().
       |[2:18..2:21): Int => scala/Int#
       |[3:6..3:8): T2 <= b/T2#
       |[3:17..3:19): T1 => b/T1#
       |[4:6..4:8): C1 <= b/C1#
       |[4:9..4:9):  <= b/C1#`<init>`().
       |[4:17..4:19): T1 => b/T1#
       |[4:19..4:19):  => java/lang/Object#`<init>`().
       |[4:26..4:27): y <= b/C1#y().
       |[4:29..4:32): Int => scala/Int#
       |[4:35..4:35):  => b/C1#
       |[4:40..4:40):  => b/T1#
       |[4:41..4:42): x => b/T1#x().
       |[5:6..5:8): C2 <= b/C2#
       |[5:9..5:9):  <= b/C2#`<init>`().
       |[5:17..5:18): K => b/K#
       |[5:18..5:18):  => b/K#`<init>`().
       |[5:24..5:26): T1 => b/T1#
       |[5:33..5:34): y <= b/C2#y().
       |[5:36..5:39): Int => scala/Int#
       |[5:42..5:42):  => b/C2#
       |[5:47..5:47):  => b/T1#
       |[5:48..5:49): x => b/T1#x().
       |[5:50..5:51): + => scala/Int#`+`(+4).
       |[5:52..5:52):  => b/C2#
       |[5:57..5:57):  => b/K#
       |[5:58..5:59): w => b/K#w().
       |[6:6..6:8): C3 <= b/C3#
       |[6:9..6:9):  <= b/C3#`<init>`().
       |[6:17..6:19): T1 => b/T1#
       |[6:19..6:19):  => java/lang/Object#`<init>`().
       |[6:26..6:27): y <= b/C3#y().
       |[6:29..6:32): Int => scala/Int#
       |[6:35..6:37): C3 => b/C3#
       |[6:43..6:43):  => b/T1#
       |[6:44..6:45): x => b/T1#x().
       |[7:6..7:8): C4 <= b/C4#
       |[7:9..7:9):  <= b/C4#`<init>`().
       |[7:17..7:19): T1 => b/T1#
       |[7:19..7:19):  => java/lang/Object#`<init>`().
       |[7:26..7:27): y <= b/C4#y().
       |[7:29..7:32): Int => scala/Int#
       |[7:35..7:35):  => b/C4#
       |[7:40..7:40):  => java/lang/Object#
       |[7:41..7:49): hashCode => java/lang/Object#hashCode().
       |[8:6..8:8): C5 <= b/C5#
       |[8:9..8:9):  <= b/C5#`<init>`().
       |[8:15..8:16): y <= b/C5#y().
       |[8:18..8:21): Int => scala/Int#
       |[8:24..8:24):  => b/C5#
       |[8:29..8:29):  => java/lang/Object#
       |[8:30..8:38): hashCode => java/lang/Object#hashCode().
       |[9:6..9:8): C6 <= b/C6#
       |[9:9..9:9):  <= b/C6#`<init>`().
       |[9:17..9:19): T2 => b/T2#
       |[9:19..9:19):  => java/lang/Object#`<init>`().
       |[9:26..9:27): y <= b/C6#y().
       |[9:29..9:32): Int => scala/Int#
       |[9:35..9:35):  => b/C6#
       |[9:40..9:40):  => b/T2#
       |[9:41..9:42): x => b/T1#x().
       |[10:6..10:8): C7 <= b/C7#
       |[10:9..10:9):  <= b/C7#`<init>`().
       |[10:17..10:18): K => b/K#
       |[10:18..10:18):  => b/K#`<init>`().
       |[10:24..10:26): T1 => b/T1#
       |[10:33..10:34): y <= b/C7#y().
       |[10:36..10:39): Int => scala/Int#
       |[10:42..10:42):  => b/C7#
       |[10:48..10:50): T1 => b/T1#
       |[10:52..10:53): x => b/T1#x().
       |[10:54..10:55): + => scala/Int#`+`(+4).
       |[10:56..10:56):  => b/C7#
       |[10:62..10:63): K => b/K#
       |[10:65..10:66): w => b/K#w().
       |[11:7..11:8): O <= b/O.
       |[11:15..11:16): o <= b/O.o.
       |[11:23..11:23):  <= local0
       |[11:23..11:24): K => b/K#
       |[11:24..11:24):  => b/K#`<init>`().
       |[11:30..11:32): T1 => b/T1#
       |[11:39..11:40): y <= local1
       |[11:42..11:45): Int => scala/Int#
       |[11:53..11:53):  => b/T1#
       |[11:54..11:55): x => b/T1#x().
       |""".stripMargin,
  )

}
