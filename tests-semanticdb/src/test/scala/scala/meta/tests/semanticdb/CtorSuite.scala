package scala.meta.tests.semanticdb

class CtorSuite extends SemanticdbSuite {

  // #1840: `new` with and without parentheses
  occurrences(
    """|object noparens {
       |  class C
       |  val c = new C
       |}
       |object parens {
       |  class C
       |  val c = (new C)
       |}
       |""".stripMargin,
    """|[0:7..0:15): noparens <= _empty_/noparens.
       |[1:8..1:9): C <= _empty_/noparens.C#
       |[1:9..1:9):  <= _empty_/noparens.C#`<init>`().
       |[2:6..2:7): c <= _empty_/noparens.c.
       |[2:14..2:15): C => _empty_/noparens.C#
       |[2:15..2:15):  => _empty_/noparens.C#`<init>`().
       |[4:7..4:13): parens <= _empty_/parens.
       |[5:8..5:9): C <= _empty_/parens.C#
       |[5:9..5:9):  <= _empty_/parens.C#`<init>`().
       |[6:6..6:7): c <= _empty_/parens.c.
       |[6:15..6:16): C => _empty_/parens.C#
       |[6:16..6:16):  => _empty_/parens.C#`<init>`().
       |""".stripMargin,
  )

  // #4902: parents without arguments
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

  // #4902: an object with a trait parent on the last line
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

  // #4902: a class parent on the last line
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

  // #4902: a class on the last line
  occurrences(
    """|package d
       |class C1
       |""".stripMargin,
    """|[0:8..0:9): d <= d/
       |[1:6..1:8): C1 <= d/C1#
       |[1:8..1:8):  <= d/C1#`<init>`().
       |""".stripMargin,
  )

  // #4902: instances and annotations
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

  // #4902: an instance at the end of a line
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

  // #4910: parents with inferred type arguments
  occurrences(
    """|package q
       |object n { class P[T](x: T); type A[T] = P[T]; type B = P[Int]; class Q[T]() }
       |import n._
       |class C1 extends n.A(1)
       |class C2 extends A(1)
       |class C3 extends n.B(1)
       |class C4 extends B(1)
       |class C5 extends P(1)
       |class C6 extends n.Q
       |class C7 extends Q
       |class C8 extends java.util.ArrayList
       |class C9 extends java.util.ArrayList(1)
       |class O { class I[T](x: T); class C10 extends this.I(1) }
       |object o extends O
       |class C11 extends o.I(1)
       |""".stripMargin,
    """|[0:8..0:9): q <= q/
       |[1:7..1:8): n <= q/n.
       |[1:17..1:18): P <= q/n.P#
       |[1:19..1:20): T <= q/n.P#[T]
       |[1:21..1:21):  <= q/n.P#`<init>`().
       |[1:22..1:23): x <= q/n.P#`<init>`().(x)
       |[1:22..1:23): x <= q/n.P#x.
       |[1:25..1:26): T => q/n.P#[T]
       |[1:34..1:35): A <= q/n.A#
       |[1:36..1:37): T <= q/n.A#[T]
       |[1:41..1:42): P => q/n.P#
       |[1:43..1:44): T => q/n.A#[T]
       |[1:52..1:53): B <= q/n.B#
       |[1:56..1:57): P => q/n.P#
       |[1:58..1:61): Int => scala/Int#
       |[1:70..1:71): Q <= q/n.Q#
       |[1:72..1:73): T <= q/n.Q#[T]
       |[1:74..1:74):  <= q/n.Q#`<init>`().
       |[2:7..2:8): n => q/n.
       |[3:6..3:8): C1 <= q/C1#
       |[3:9..3:9):  <= q/C1#`<init>`().
       |[3:17..3:18): n => q/n.
       |[3:19..3:20): A => q/n.A#
       |[3:20..3:20):  => q/n.P#`<init>`().
       |[4:6..4:8): C2 <= q/C2#
       |[4:9..4:9):  <= q/C2#`<init>`().
       |[4:17..4:18): A => q/n.A#
       |[4:18..4:18):  => q/n.P#`<init>`().
       |[5:6..5:8): C3 <= q/C3#
       |[5:9..5:9):  <= q/C3#`<init>`().
       |[5:17..5:18): n => q/n.
       |[5:19..5:20): B => q/n.B#
       |[5:20..5:20):  => q/n.P#`<init>`().
       |[6:6..6:8): C4 <= q/C4#
       |[6:9..6:9):  <= q/C4#`<init>`().
       |[6:17..6:18): B => q/n.B#
       |[6:18..6:18):  => q/n.P#`<init>`().
       |[7:6..7:8): C5 <= q/C5#
       |[7:9..7:9):  <= q/C5#`<init>`().
       |[7:17..7:18): P => q/n.P#
       |[7:18..7:18):  => q/n.P#`<init>`().
       |[8:6..8:8): C6 <= q/C6#
       |[8:9..8:9):  <= q/C6#`<init>`().
       |[8:17..8:18): n => q/n.
       |[8:19..8:20): Q => q/n.Q#
       |[8:20..8:20):  => q/n.Q#`<init>`().
       |[9:6..9:8): C7 <= q/C7#
       |[9:9..9:9):  <= q/C7#`<init>`().
       |[9:17..9:18): Q => q/n.Q#
       |[9:18..9:18):  => q/n.Q#`<init>`().
       |[10:6..10:8): C8 <= q/C8#
       |[10:9..10:9):  <= q/C8#`<init>`().
       |[10:17..10:21): java => java/
       |[10:22..10:26): util => java/util/
       |[10:27..10:36): ArrayList => java/util/ArrayList#
       |[10:36..10:36):  => java/util/ArrayList#`<init>`(+1).
       |[11:6..11:8): C9 <= q/C9#
       |[11:9..11:9):  <= q/C9#`<init>`().
       |[11:17..11:21): java => java/
       |[11:22..11:26): util => java/util/
       |[11:27..11:36): ArrayList => java/util/ArrayList#
       |[11:36..11:36):  => java/util/ArrayList#`<init>`().
       |[12:6..12:7): O <= q/O#
       |[12:8..12:8):  <= q/O#`<init>`().
       |[12:16..12:17): I <= q/O#I#
       |[12:18..12:19): T <= q/O#I#[T]
       |[12:20..12:20):  <= q/O#I#`<init>`().
       |[12:21..12:22): x <= q/O#I#`<init>`().(x)
       |[12:21..12:22): x <= q/O#I#x.
       |[12:24..12:25): T => q/O#I#[T]
       |[12:34..12:37): C10 <= q/O#C10#
       |[12:38..12:38):  <= q/O#C10#`<init>`().
       |[12:51..12:52): I => q/O#I#
       |[12:52..12:52):  => q/O#I#`<init>`().
       |[13:7..13:8): o <= q/o.
       |[13:17..13:18): O => q/O#
       |[13:18..13:18):  => q/O#`<init>`().
       |[14:6..14:9): C11 <= q/C11#
       |[14:10..14:10):  <= q/C11#`<init>`().
       |[14:18..14:19): o => q/o.
       |[14:20..14:21): I => q/O#I#
       |[14:21..14:21):  => q/O#I#`<init>`().
       |""".stripMargin,
  )

  // #4910: parents and constructors through type aliases
  occurrences(
    """|package q
       |object n {
       |  class P[T](x: T); type A[T] = P[T]; type A2[T] = A[T]
       |  type L[T] = java.util.ArrayList[T]
       |  trait R[T]; type RA[T] = R[T]; type RB = R[Int]
       |}
       |object m { type A[T] = n.P[T]; type P[T] = n.P[T] }
       |import n.{A => Z}
       |class C1 extends n.A[Int](1)
       |class C2 extends n.A2(1)
       |class C3 extends m.A(1)
       |class C4 extends Z(1)
       |class C5 extends n.L
       |class C6 extends n.L(1)
       |class C7 extends n.RB
       |class C8 extends n.RA[Int]
       |class C9 extends n.A[Int](1) with n.RB
       |class C10 extends n.A[Int](1) with n.RA[Int]
       |object o {
       |  val v1 = new n.A(1)
       |  val v2 = new n.A(1) {}
       |  val v3 = new n.A[Int](1)
       |  val v4 = new n.L
       |  val v5 = new n.RB {}
       |  val v6 = new n.A2(1)
       |}
       |class C11 extends m.P(1)
       |class O2 { type A[T] = n.P[T]; type P[T] = n.P[T] }
       |class C12 extends O2#A(1)
       |class C13 extends O2#P(1)
       |object o2 { val v1 = new m.P(1) {} }
       |""".stripMargin,
    """|[0:8..0:9): q <= q/
       |[1:7..1:8): n <= q/n.
       |[2:8..2:9): P <= q/n.P#
       |[2:10..2:11): T <= q/n.P#[T]
       |[2:12..2:12):  <= q/n.P#`<init>`().
       |[2:13..2:14): x <= q/n.P#`<init>`().(x)
       |[2:13..2:14): x <= q/n.P#x.
       |[2:16..2:17): T => q/n.P#[T]
       |[2:25..2:26): A <= q/n.A#
       |[2:27..2:28): T <= q/n.A#[T]
       |[2:32..2:33): P => q/n.P#
       |[2:34..2:35): T => q/n.A#[T]
       |[2:43..2:45): A2 <= q/n.A2#
       |[2:46..2:47): T <= q/n.A2#[T]
       |[2:51..2:52): A => q/n.A#
       |[2:53..2:54): T => q/n.A2#[T]
       |[3:7..3:8): L <= q/n.L#
       |[3:9..3:10): T <= q/n.L#[T]
       |[3:14..3:18): java => java/
       |[3:19..3:23): util => java/util/
       |[3:24..3:33): ArrayList => java/util/ArrayList#
       |[3:34..3:35): T => q/n.L#[T]
       |[4:8..4:9): R <= q/n.R#
       |[4:10..4:11): T <= q/n.R#[T]
       |[4:19..4:21): RA <= q/n.RA#
       |[4:22..4:23): T <= q/n.RA#[T]
       |[4:27..4:28): R => q/n.R#
       |[4:29..4:30): T => q/n.RA#[T]
       |[4:38..4:40): RB <= q/n.RB#
       |[4:43..4:44): R => q/n.R#
       |[4:45..4:48): Int => scala/Int#
       |[6:7..6:8): m <= q/m.
       |[6:16..6:17): A <= q/m.A#
       |[6:18..6:19): T <= q/m.A#[T]
       |[6:23..6:24): n => q/n.
       |[6:25..6:26): P => q/n.P#
       |[6:27..6:28): T => q/m.A#[T]
       |[6:36..6:37): P <= q/m.P#
       |[6:38..6:39): T <= q/m.P#[T]
       |[6:43..6:44): n => q/n.
       |[6:45..6:46): P => q/n.P#
       |[6:47..6:48): T => q/m.P#[T]
       |[7:7..7:8): n => q/n.
       |[7:10..7:11): A => q/n.A#
       |[8:6..8:8): C1 <= q/C1#
       |[8:9..8:9):  <= q/C1#`<init>`().
       |[8:17..8:18): n => q/n.
       |[8:19..8:20): A => q/n.A#
       |[8:21..8:24): Int => scala/Int#
       |[8:25..8:25):  => q/n.P#`<init>`().
       |[9:6..9:8): C2 <= q/C2#
       |[9:9..9:9):  <= q/C2#`<init>`().
       |[9:17..9:18): n => q/n.
       |[9:19..9:21): A2 => q/n.A2#
       |[9:21..9:21):  => q/n.P#`<init>`().
       |[10:6..10:8): C3 <= q/C3#
       |[10:9..10:9):  <= q/C3#`<init>`().
       |[10:17..10:18): m => q/m.
       |[10:19..10:20): A => q/m.A#
       |[10:20..10:20):  => q/n.P#`<init>`().
       |[11:6..11:8): C4 <= q/C4#
       |[11:9..11:9):  <= q/C4#`<init>`().
       |[11:17..11:18): Z => q/n.A#
       |[11:18..11:18):  => q/n.P#`<init>`().
       |[12:6..12:8): C5 <= q/C5#
       |[12:9..12:9):  <= q/C5#`<init>`().
       |[12:17..12:18): n => q/n.
       |[12:19..12:20): L => q/n.L#
       |[12:20..12:20):  => java/util/ArrayList#`<init>`(+1).
       |[13:6..13:8): C6 <= q/C6#
       |[13:9..13:9):  <= q/C6#`<init>`().
       |[13:17..13:18): n => q/n.
       |[13:19..13:20): L => q/n.L#
       |[13:20..13:20):  => java/util/ArrayList#`<init>`().
       |[14:6..14:8): C7 <= q/C7#
       |[14:9..14:9):  <= q/C7#`<init>`().
       |[14:17..14:18): n => q/n.
       |[14:19..14:21): RB => q/n.RB#
       |[14:21..14:21):  => java/lang/Object#`<init>`().
       |[15:6..15:8): C8 <= q/C8#
       |[15:9..15:9):  <= q/C8#`<init>`().
       |[15:17..15:18): n => q/n.
       |[15:19..15:21): RA => q/n.RA#
       |[15:22..15:25): Int => scala/Int#
       |[15:26..15:26):  => java/lang/Object#`<init>`().
       |[16:6..16:8): C9 <= q/C9#
       |[16:9..16:9):  <= q/C9#`<init>`().
       |[16:17..16:18): n => q/n.
       |[16:19..16:20): A => q/n.A#
       |[16:21..16:24): Int => scala/Int#
       |[16:25..16:25):  => q/n.P#`<init>`().
       |[16:34..16:35): n => q/n.
       |[16:36..16:38): RB => q/n.RB#
       |[17:6..17:9): C10 <= q/C10#
       |[17:10..17:10):  <= q/C10#`<init>`().
       |[17:18..17:19): n => q/n.
       |[17:20..17:21): A => q/n.A#
       |[17:22..17:25): Int => scala/Int#
       |[17:26..17:26):  => q/n.P#`<init>`().
       |[17:35..17:36): n => q/n.
       |[17:37..17:39): RA => q/n.RA#
       |[17:40..17:43): Int => scala/Int#
       |[18:7..18:8): o <= q/o.
       |[19:6..19:8): v1 <= q/o.v1.
       |[19:15..19:16): n => q/n.
       |[19:17..19:18): A => q/n.A#
       |[19:18..19:18):  => q/n.P#`<init>`().
       |[20:6..20:8): v2 <= q/o.v2.
       |[20:15..20:15):  <= local0
       |[20:15..20:16): n => q/n.
       |[20:17..20:18): A => q/n.A#
       |[20:18..20:18):  => q/n.P#`<init>`().
       |[21:6..21:8): v3 <= q/o.v3.
       |[21:15..21:16): n => q/n.
       |[21:17..21:18): A => q/n.A#
       |[21:19..21:22): Int => scala/Int#
       |[21:23..21:23):  => q/n.P#`<init>`().
       |[22:6..22:8): v4 <= q/o.v4.
       |[22:15..22:16): n => q/n.
       |[22:17..22:18): L => q/n.L#
       |[22:18..22:18):  => java/util/ArrayList#`<init>`(+1).
       |[23:6..23:8): v5 <= q/o.v5.
       |[23:15..23:15):  <= local1
       |[23:15..23:16): n => q/n.
       |[23:17..23:19): RB => q/n.RB#
       |[23:19..23:19):  => java/lang/Object#`<init>`().
       |[24:6..24:8): v6 <= q/o.v6.
       |[24:15..24:16): n => q/n.
       |[24:17..24:19): A2 => q/n.A2#
       |[24:19..24:19):  => q/n.P#`<init>`().
       |[26:6..26:9): C11 <= q/C11#
       |[26:10..26:10):  <= q/C11#`<init>`().
       |[26:18..26:19): m => q/m.
       |[26:20..26:21): P => q/m.P#
       |[26:21..26:21):  => q/n.P#`<init>`().
       |[27:6..27:8): O2 <= q/O2#
       |[27:9..27:9):  <= q/O2#`<init>`().
       |[27:16..27:17): A <= q/O2#A#
       |[27:18..27:19): T <= q/O2#A#[T]
       |[27:23..27:24): n => q/n.
       |[27:25..27:26): P => q/n.P#
       |[27:27..27:28): T => q/O2#A#[T]
       |[27:36..27:37): P <= q/O2#P#
       |[27:38..27:39): T <= q/O2#P#[T]
       |[27:43..27:44): n => q/n.
       |[27:45..27:46): P => q/n.P#
       |[27:47..27:48): T => q/O2#P#[T]
       |[28:6..28:9): C12 <= q/C12#
       |[28:10..28:10):  <= q/C12#`<init>`().
       |[28:18..28:20): O2 => q/O2#
       |[28:21..28:22): A => q/O2#A#
       |[28:22..28:22):  => q/n.P#`<init>`().
       |[29:6..29:9): C13 <= q/C13#
       |[29:10..29:10):  <= q/C13#`<init>`().
       |[29:18..29:20): O2 => q/O2#
       |[29:21..29:22): P => q/O2#P#
       |[29:22..29:22):  => q/n.P#`<init>`().
       |[30:7..30:9): o2 <= q/o2.
       |[30:16..30:18): v1 <= q/o2.v1.
       |[30:25..30:25):  <= local2
       |[30:25..30:26): m => q/m.
       |[30:27..30:28): P => q/m.P#
       |[30:28..30:28):  => q/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4910: parents through aliases in objects of the same name
  occurrences(
    """|package q
       |object a { object n { class P[T](x: T); type A[T] = P[T] } }
       |object b { object n { type P[T] = a.n.P[T] } }
       |import a.{n => k}
       |class D1 extends b.n.P(1)
       |class D2 extends a.n.A(1)
       |class D3 extends _root_.q.a.n.A(1)
       |class D4 extends _root_.q.b.n.P(1)
       |class D5 extends k.A(1)
       |class D6 extends k.P(1)
       |object o { val v1 = new b.n.P(1) {}; val v2 = new a.n.A(1) {} }
       |""".stripMargin,
    """|[0:8..0:9): q <= q/
       |[1:7..1:8): a <= q/a.
       |[1:18..1:19): n <= q/a.n.
       |[1:28..1:29): P <= q/a.n.P#
       |[1:30..1:31): T <= q/a.n.P#[T]
       |[1:32..1:32):  <= q/a.n.P#`<init>`().
       |[1:33..1:34): x <= q/a.n.P#`<init>`().(x)
       |[1:33..1:34): x <= q/a.n.P#x.
       |[1:36..1:37): T => q/a.n.P#[T]
       |[1:45..1:46): A <= q/a.n.A#
       |[1:47..1:48): T <= q/a.n.A#[T]
       |[1:52..1:53): P => q/a.n.P#
       |[1:54..1:55): T => q/a.n.A#[T]
       |[2:7..2:8): b <= q/b.
       |[2:18..2:19): n <= q/b.n.
       |[2:27..2:28): P <= q/b.n.P#
       |[2:29..2:30): T <= q/b.n.P#[T]
       |[2:34..2:35): a => q/a.
       |[2:36..2:37): n => q/a.n.
       |[2:38..2:39): P => q/a.n.P#
       |[2:40..2:41): T => q/b.n.P#[T]
       |[3:7..3:8): a => q/a.
       |[3:10..3:11): n => q/a.n.
       |[4:6..4:8): D1 <= q/D1#
       |[4:9..4:9):  <= q/D1#`<init>`().
       |[4:17..4:18): b => q/b.
       |[4:19..4:20): n => q/b.n.
       |[4:21..4:22): P => q/b.n.P#
       |[4:22..4:22):  => q/a.n.P#`<init>`().
       |[5:6..5:8): D2 <= q/D2#
       |[5:9..5:9):  <= q/D2#`<init>`().
       |[5:17..5:18): a => q/a.
       |[5:19..5:20): n => q/a.n.
       |[5:21..5:22): A => q/a.n.A#
       |[5:22..5:22):  => q/a.n.P#`<init>`().
       |[6:6..6:8): D3 <= q/D3#
       |[6:9..6:9):  <= q/D3#`<init>`().
       |[6:17..6:23): _root_ => _root_/
       |[6:24..6:25): q => q/
       |[6:26..6:27): a => q/a.
       |[6:28..6:29): n => q/a.n.
       |[6:30..6:31): A => q/a.n.A#
       |[6:31..6:31):  => q/a.n.P#`<init>`().
       |[7:6..7:8): D4 <= q/D4#
       |[7:9..7:9):  <= q/D4#`<init>`().
       |[7:17..7:23): _root_ => _root_/
       |[7:24..7:25): q => q/
       |[7:26..7:27): b => q/b.
       |[7:28..7:29): n => q/b.n.
       |[7:30..7:31): P => q/b.n.P#
       |[7:31..7:31):  => q/a.n.P#`<init>`().
       |[8:6..8:8): D5 <= q/D5#
       |[8:9..8:9):  <= q/D5#`<init>`().
       |[8:17..8:18): k => q/a.n.
       |[8:19..8:20): A => q/a.n.A#
       |[8:20..8:20):  => q/a.n.P#`<init>`().
       |[9:6..9:8): D6 <= q/D6#
       |[9:9..9:9):  <= q/D6#`<init>`().
       |[9:17..9:18): k => q/a.n.
       |[9:19..9:20): P => q/a.n.P#
       |[9:20..9:20):  => q/a.n.P#`<init>`().
       |[10:7..10:8): o <= q/o.
       |[10:15..10:17): v1 <= q/o.v1.
       |[10:24..10:24):  <= local0
       |[10:24..10:25): b => q/b.
       |[10:26..10:27): n => q/b.n.
       |[10:28..10:29): P => q/b.n.P#
       |[10:29..10:29):  => q/a.n.P#`<init>`().
       |[10:41..10:43): v2 <= q/o.v2.
       |[10:50..10:50):  <= local1
       |[10:50..10:51): a => q/a.
       |[10:52..10:53): n => q/a.n.
       |[10:54..10:55): A => q/a.n.A#
       |[10:55..10:55):  => q/a.n.P#`<init>`().
       |""".stripMargin,
  )

  // #4910: parent qualifiers resolved in the scope around the class
  occurrences(
    """|package q
       |object a { object n { class P[T](x: T); type A[T] = P[T] } }
       |object b { object n { type P[T] = a.n.P[T] } }
       |class E1 extends q.b.n.P(1)
       |object e2 { import a.{n => k}; class E2 extends k.A(1) }
       |object e3 { def f = { import a.{n => k}; new k.A(1) {} } }
       |object e4 { import a._; class E4 extends n.A(1) }
       |object e5 { import b.n; class E5 extends n.P(1) }
       |object e6 { object q { val x = 1 }; class E6 extends _root_.q.b.n.P(1) }
       |object e7 { object a { object n { type A[T] = q.b.n.P[T] } }; class E7 extends a.n.A(1) }
       |trait W { object w { type P[T] = q.a.n.P[T] } }
       |object e8 extends W { class E8 extends w.P(1) }
       |class E9 extends W { class E9 extends this.w.P(1) }
       |object e10 { def f(o: b.n.type) = new o.P(1) {} }
       |object e11 { val f = (o: b.n.type) => new o.P(1) {} }
       |object e12 { (b.n: Any) match { case o: b.n.type => new o.P(1) {} } }
       |""".stripMargin,
    """|[0:8..0:9): q <= q/
       |[1:7..1:8): a <= q/a.
       |[1:18..1:19): n <= q/a.n.
       |[1:28..1:29): P <= q/a.n.P#
       |[1:30..1:31): T <= q/a.n.P#[T]
       |[1:32..1:32):  <= q/a.n.P#`<init>`().
       |[1:33..1:34): x <= q/a.n.P#`<init>`().(x)
       |[1:33..1:34): x <= q/a.n.P#x.
       |[1:36..1:37): T => q/a.n.P#[T]
       |[1:45..1:46): A <= q/a.n.A#
       |[1:47..1:48): T <= q/a.n.A#[T]
       |[1:52..1:53): P => q/a.n.P#
       |[1:54..1:55): T => q/a.n.A#[T]
       |[2:7..2:8): b <= q/b.
       |[2:18..2:19): n <= q/b.n.
       |[2:27..2:28): P <= q/b.n.P#
       |[2:29..2:30): T <= q/b.n.P#[T]
       |[2:34..2:35): a => q/a.
       |[2:36..2:37): n => q/a.n.
       |[2:38..2:39): P => q/a.n.P#
       |[2:40..2:41): T => q/b.n.P#[T]
       |[3:6..3:8): E1 <= q/E1#
       |[3:9..3:9):  <= q/E1#`<init>`().
       |[3:17..3:18): q => q/
       |[3:19..3:20): b => q/b.
       |[3:21..3:22): n => q/b.n.
       |[3:23..3:24): P => q/b.n.P#
       |[3:24..3:24):  => q/a.n.P#`<init>`().
       |[4:7..4:9): e2 <= q/e2.
       |[4:19..4:20): a => q/a.
       |[4:22..4:23): n => q/a.n.
       |[4:37..4:39): E2 <= q/e2.E2#
       |[4:40..4:40):  <= q/e2.E2#`<init>`().
       |[4:48..4:49): k => q/a.n.
       |[4:50..4:51): A => q/a.n.A#
       |[4:51..4:51):  => q/a.n.P#`<init>`().
       |[5:7..5:9): e3 <= q/e3.
       |[5:16..5:17): f <= q/e3.f().
       |[5:29..5:30): a => q/a.
       |[5:32..5:33): n => q/a.n.
       |[5:45..5:45):  <= local0
       |[5:45..5:46): k => q/a.n.
       |[5:47..5:48): A => q/a.n.A#
       |[5:48..5:48):  => q/a.n.P#`<init>`().
       |[6:7..6:9): e4 <= q/e4.
       |[6:19..6:20): a => q/a.
       |[6:30..6:32): E4 <= q/e4.E4#
       |[6:33..6:33):  <= q/e4.E4#`<init>`().
       |[6:41..6:42): n => q/a.n.
       |[6:43..6:44): A => q/a.n.A#
       |[6:44..6:44):  => q/a.n.P#`<init>`().
       |[7:7..7:9): e5 <= q/e5.
       |[7:19..7:20): b => q/b.
       |[7:21..7:22): n => q/b.n.
       |[7:30..7:32): E5 <= q/e5.E5#
       |[7:33..7:33):  <= q/e5.E5#`<init>`().
       |[7:41..7:42): n => q/b.n.
       |[7:43..7:44): P => q/b.n.P#
       |[7:44..7:44):  => q/a.n.P#`<init>`().
       |[8:7..8:9): e6 <= q/e6.
       |[8:19..8:20): q <= q/e6.q.
       |[8:27..8:28): x <= q/e6.q.x.
       |[8:42..8:44): E6 <= q/e6.E6#
       |[8:45..8:45):  <= q/e6.E6#`<init>`().
       |[8:53..8:59): _root_ => _root_/
       |[8:60..8:61): q => q/
       |[8:62..8:63): b => q/b.
       |[8:64..8:65): n => q/b.n.
       |[8:66..8:67): P => q/b.n.P#
       |[8:67..8:67):  => q/a.n.P#`<init>`().
       |[9:7..9:9): e7 <= q/e7.
       |[9:19..9:20): a <= q/e7.a.
       |[9:30..9:31): n <= q/e7.a.n.
       |[9:39..9:40): A <= q/e7.a.n.A#
       |[9:41..9:42): T <= q/e7.a.n.A#[T]
       |[9:46..9:47): q => q/
       |[9:48..9:49): b => q/b.
       |[9:50..9:51): n => q/b.n.
       |[9:52..9:53): P => q/b.n.P#
       |[9:54..9:55): T => q/e7.a.n.A#[T]
       |[9:68..9:70): E7 <= q/e7.E7#
       |[9:71..9:71):  <= q/e7.E7#`<init>`().
       |[9:79..9:80): a => q/e7.a.
       |[9:81..9:82): n => q/e7.a.n.
       |[9:83..9:84): A => q/e7.a.n.A#
       |[9:84..9:84):  => q/a.n.P#`<init>`().
       |[10:6..10:7): W <= q/W#
       |[10:8..10:8):  <= q/W#`<init>`().
       |[10:17..10:18): w <= q/W#w.
       |[10:26..10:27): P <= q/W#w.P#
       |[10:28..10:29): T <= q/W#w.P#[T]
       |[10:33..10:34): q => q/
       |[10:35..10:36): a => q/a.
       |[10:37..10:38): n => q/a.n.
       |[10:39..10:40): P => q/a.n.P#
       |[10:41..10:42): T => q/W#w.P#[T]
       |[11:7..11:9): e8 <= q/e8.
       |[11:18..11:19): W => q/W#
       |[11:19..11:19):  => java/lang/Object#`<init>`().
       |[11:28..11:30): E8 <= q/e8.E8#
       |[11:31..11:31):  <= q/e8.E8#`<init>`().
       |[11:39..11:40): w => q/W#w.
       |[11:41..11:42): P => q/W#w.P#
       |[11:42..11:42):  => q/a.n.P#`<init>`().
       |[12:6..12:8): E9 <= q/E9#
       |[12:9..12:9):  <= q/E9#`<init>`().
       |[12:17..12:18): W => q/W#
       |[12:18..12:18):  => java/lang/Object#`<init>`().
       |[12:27..12:29): E9 <= q/E9#E9#
       |[12:30..12:30):  <= q/E9#E9#`<init>`().
       |[12:43..12:44): w => q/W#w.
       |[12:45..12:46): P => q/W#w.P#
       |[12:46..12:46):  => q/a.n.P#`<init>`().
       |[13:7..13:10): e10 <= q/e10.
       |[13:17..13:18): f <= q/e10.f().
       |[13:19..13:20): o <= q/e10.f().(o)
       |[13:22..13:23): b => q/b.
       |[13:24..13:25): n => q/b.n.
       |[13:38..13:38):  <= local1
       |[13:38..13:39): o => q/e10.f().(o)
       |[13:40..13:41): P => q/b.n.P#
       |[13:41..13:41):  => q/a.n.P#`<init>`().
       |[14:7..14:10): e11 <= q/e11.
       |[14:17..14:18): f <= q/e11.f.
       |[14:22..14:23): o <= local2
       |[14:25..14:26): b => q/b.
       |[14:27..14:28): n => q/b.n.
       |[14:42..14:42):  <= local3
       |[14:42..14:43): o => local2
       |[14:44..14:45): P => q/b.n.P#
       |[14:45..14:45):  => q/a.n.P#`<init>`().
       |[15:7..15:10): e12 <= q/e12.
       |[15:14..15:15): b => q/b.
       |[15:16..15:17): n => q/b.n.
       |[15:19..15:22): Any => scala/Any#
       |[15:37..15:38): o <= local4
       |[15:40..15:41): b => q/b.
       |[15:42..15:43): n => q/b.n.
       |[15:56..15:56):  <= local5
       |[15:56..15:57): o => local4
       |[15:58..15:59): P => q/b.n.P#
       |[15:59..15:59):  => q/a.n.P#`<init>`().
       |""".stripMargin,
  )

  // #4910: parent qualifiers through super
  occurrences(
    """|package q
       |object a { object n { class P[T](x: T) } }
       |class S { object x { class I[T](y: T); type J[T] = I[T] }; object z { type I[T] = a.n.P[T] } }
       |trait U { object u { class I[T](y: T) } }
       |class F1 extends S { class G1 extends super.x.I(1) }
       |class F2 extends S { class G2 extends F2.super.x.I(1) }
       |class F3 extends S with U { class G3 extends super[U].u.I(1) }
       |class F4 extends S { class G4 extends super.z.I(1) }
       |class F5 extends S { class G5 extends super.x.J(1) }
       |""".stripMargin,
    """|[0:8..0:9): q <= q/
       |[1:7..1:8): a <= q/a.
       |[1:18..1:19): n <= q/a.n.
       |[1:28..1:29): P <= q/a.n.P#
       |[1:30..1:31): T <= q/a.n.P#[T]
       |[1:32..1:32):  <= q/a.n.P#`<init>`().
       |[1:33..1:34): x <= q/a.n.P#`<init>`().(x)
       |[1:33..1:34): x <= q/a.n.P#x.
       |[1:36..1:37): T => q/a.n.P#[T]
       |[2:6..2:7): S <= q/S#
       |[2:8..2:8):  <= q/S#`<init>`().
       |[2:17..2:18): x <= q/S#x.
       |[2:27..2:28): I <= q/S#x.I#
       |[2:29..2:30): T <= q/S#x.I#[T]
       |[2:31..2:31):  <= q/S#x.I#`<init>`().
       |[2:32..2:33): y <= q/S#x.I#`<init>`().(y)
       |[2:32..2:33): y <= q/S#x.I#y.
       |[2:35..2:36): T => q/S#x.I#[T]
       |[2:44..2:45): J <= q/S#x.J#
       |[2:46..2:47): T <= q/S#x.J#[T]
       |[2:51..2:52): I => q/S#x.I#
       |[2:53..2:54): T => q/S#x.J#[T]
       |[2:66..2:67): z <= q/S#z.
       |[2:75..2:76): I <= q/S#z.I#
       |[2:77..2:78): T <= q/S#z.I#[T]
       |[2:82..2:83): a => q/a.
       |[2:84..2:85): n => q/a.n.
       |[2:86..2:87): P => q/a.n.P#
       |[2:88..2:89): T => q/S#z.I#[T]
       |[3:6..3:7): U <= q/U#
       |[3:8..3:8):  <= q/U#`<init>`().
       |[3:17..3:18): u <= q/U#u.
       |[3:27..3:28): I <= q/U#u.I#
       |[3:29..3:30): T <= q/U#u.I#[T]
       |[3:31..3:31):  <= q/U#u.I#`<init>`().
       |[3:32..3:33): y <= q/U#u.I#`<init>`().(y)
       |[3:32..3:33): y <= q/U#u.I#y.
       |[3:35..3:36): T => q/U#u.I#[T]
       |[4:6..4:8): F1 <= q/F1#
       |[4:9..4:9):  <= q/F1#`<init>`().
       |[4:17..4:18): S => q/S#
       |[4:18..4:18):  => q/S#`<init>`().
       |[4:27..4:29): G1 <= q/F1#G1#
       |[4:30..4:30):  <= q/F1#G1#`<init>`().
       |[4:44..4:45): x => q/S#x.
       |[4:46..4:47): I => q/S#x.I#
       |[4:47..4:47):  => q/S#x.I#`<init>`().
       |[5:6..5:8): F2 <= q/F2#
       |[5:9..5:9):  <= q/F2#`<init>`().
       |[5:17..5:18): S => q/S#
       |[5:18..5:18):  => q/S#`<init>`().
       |[5:27..5:29): G2 <= q/F2#G2#
       |[5:30..5:30):  <= q/F2#G2#`<init>`().
       |[5:47..5:48): x => q/S#x.
       |[5:49..5:50): I => q/S#x.I#
       |[5:50..5:50):  => q/S#x.I#`<init>`().
       |[6:6..6:8): F3 <= q/F3#
       |[6:9..6:9):  <= q/F3#`<init>`().
       |[6:17..6:18): S => q/S#
       |[6:18..6:18):  => q/S#`<init>`().
       |[6:24..6:25): U => q/U#
       |[6:34..6:36): G3 <= q/F3#G3#
       |[6:37..6:37):  <= q/F3#G3#`<init>`().
       |[6:54..6:55): u => q/U#u.
       |[6:56..6:57): I => q/U#u.I#
       |[6:57..6:57):  => q/U#u.I#`<init>`().
       |[7:6..7:8): F4 <= q/F4#
       |[7:9..7:9):  <= q/F4#`<init>`().
       |[7:17..7:18): S => q/S#
       |[7:18..7:18):  => q/S#`<init>`().
       |[7:27..7:29): G4 <= q/F4#G4#
       |[7:30..7:30):  <= q/F4#G4#`<init>`().
       |[7:44..7:45): z => q/S#z.
       |[7:46..7:47): I => q/S#z.I#
       |[7:47..7:47):  => q/a.n.P#`<init>`().
       |[8:6..8:8): F5 <= q/F5#
       |[8:9..8:9):  <= q/F5#`<init>`().
       |[8:17..8:18): S => q/S#
       |[8:18..8:18):  => q/S#`<init>`().
       |[8:27..8:29): G5 <= q/F5#G5#
       |[8:30..8:30):  <= q/F5#G5#`<init>`().
       |[8:44..8:45): x => q/S#x.
       |[8:46..8:47): J => q/S#x.J#
       |[8:47..8:47):  => q/S#x.I#`<init>`().
       |""".stripMargin,
  )

  // #4913: kinds of definitions before a comment
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
       |[1:8..1:8):  <= a/C1#`<init>`().
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

  // #4913: classes with type parameters, modifiers and annotations
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
       |[1:11..1:11):  <= b/C1#`<init>`().
       |[2:12..2:14): C2 <= b/C2#
       |[2:14..2:14):  <= b/C2#`<init>`().
       |[3:1..3:11): deprecated => scala/deprecated#
       |[3:11..3:11):  => scala/deprecated#`<init>`().
       |[3:18..3:20): C3 <= b/C3#
       |[3:20..3:20):  <= b/C3#`<init>`().
       |[4:6..4:8): C4 <= b/C4#
       |[4:16..4:16):  <= b/C4#`<init>`().
       |[5:6..5:8): C5 <= b/C5#
       |[5:17..5:18): b => b/
       |[5:19..5:19):  <= b/C5#`<init>`().
       |[6:6..6:8): C6 <= b/C6#
       |[6:10..6:20): deprecated => scala/deprecated#
       |[6:20..6:20):  => scala/deprecated#`<init>`().
       |[6:22..6:22):  <= b/C6#`<init>`().
       |[7:15..7:17): C7 <= b/C7#
       |[7:17..7:17):  <= b/C7#`<init>`().
       |[8:22..8:24): C8 <= b/C8#
       |[8:26..8:27): T <= b/C8#[T]
       |[8:28..8:28):  <= b/C8#`<init>`().
       |""".stripMargin,
  )

  // #4913: what follows a class
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
       |[4:8..4:8):  <= c/C3#`<init>`().
       |[5:6..5:8): C4 <= c/C4#
       |[5:8..5:8):  <= c/C4#`<init>`().
       |[7:6..7:8): C5 <= c/C5#
       |[7:8..7:8):  <= c/C5#`<init>`().
       |[9:6..9:8): C6 <= c/C6#
       |[9:8..9:8):  <= c/C6#`<init>`().
       |""".stripMargin,
  )

  // #4913: classes before a semicolon or a brace
  occurrences(
    """|package d
       |class C1 /* c */ ; class C2
       |class C3 ; class C4 ;
       |object O { class C5 }
       |""".stripMargin,
    """|[0:8..0:9): d <= d/
       |[1:6..1:8): C1 <= d/C1#
       |[1:8..1:8):  <= d/C1#`<init>`().
       |[1:25..1:27): C2 <= d/C2#
       |[1:27..1:27):  <= d/C2#`<init>`().
       |[2:6..2:8): C3 <= d/C3#
       |[2:8..2:8):  <= d/C3#`<init>`().
       |[2:17..2:19): C4 <= d/C4#
       |[2:19..2:19):  <= d/C4#`<init>`().
       |[3:7..3:8): O <= d/O.
       |[3:17..3:19): C5 <= d/O.C5#
       |[3:19..3:19):  <= d/O.C5#`<init>`().
       |""".stripMargin,
  )

  // #4913: a class at the end of a file without a newline
  occurrences(
    """|package e
       |class C1""".stripMargin,
    """|[0:8..0:9): e <= e/
       |[1:6..1:8): C1 <= e/C1#
       |[1:8..1:8):  <= e/C1#`<init>`().
       |""".stripMargin,
  )

  // #4913: a class before a comment at the end of a file
  occurrences(
    """|package f
       |class C1 // c""".stripMargin,
    """|[0:8..0:9): f <= f/
       |[1:6..1:8): C1 <= f/C1#
       |[1:8..1:8):  <= f/C1#`<init>`().
       |""".stripMargin,
  )

  // #4913: nested classes
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
       |[2:10..2:10):  <= g/O.C1#`<init>`().
       |[3:8..3:10): C2 <= g/O.C2#
       |[3:10..3:10):  <= g/O.C2#`<init>`().
       |[5:6..5:7): K <= g/K#
       |[5:8..5:8):  <= g/K#`<init>`().
       |[6:8..6:10): C3 <= g/K#C3#
       |[6:10..6:10):  <= g/K#C3#`<init>`().
       |[7:6..7:7): m <= g/K#m().
       |[7:18..7:20): C4 <= local0
       |[7:20..7:20):  <= local1
       |[8:8..8:10): C4 => local0
       |[8:10..8:10):  => local1
       |""".stripMargin,
  )

  // #4913: a class in a package object
  occurrences(
    """|package h
       |package object p {
       |  class C1 // c
       |}
       |""".stripMargin,
    """|[0:8..0:9): h <= h/
       |[1:15..1:16): p <= h/p/package.
       |[2:8..2:10): C1 <= h/p/package.C1#
       |[2:10..2:10):  <= h/p/package.C1#`<init>`().
       |""".stripMargin,
  )

  // #4913: templates on the next line
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
       |[4:0..4:0):  <= b/C2#`<init>`().
       |[5:6..5:8): C3 <= b/C3#
       |[6:2..6:2):  <= b/C3#`<init>`().
       |[6:10..6:16): AnyRef => scala/AnyRef#
       |[6:16..6:16):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through aliases in an object
  occurrences(
    """|package q
       |object n {
       |  class P[T](x: T)
       |  type A[T] = P[T]
       |  type A2[T] = A[T]
       |  type F[T, U] = P[T]
       |  type L[T] = java.util.ArrayList[T]
       |}
       |class C1 extends n.A(1)
       |class C2 extends n.A2(1)
       |class C3 extends n.F(1)
       |class C4 extends n.L
       |class C5 extends n.P(1)
       |""".stripMargin,
    """|[0:8..0:9): q <= q/
       |[1:7..1:8): n <= q/n.
       |[2:8..2:9): P <= q/n.P#
       |[2:10..2:11): T <= q/n.P#[T]
       |[2:12..2:12):  <= q/n.P#`<init>`().
       |[2:13..2:14): x <= q/n.P#`<init>`().(x)
       |[2:13..2:14): x <= q/n.P#x.
       |[2:16..2:17): T => q/n.P#[T]
       |[3:7..3:8): A <= q/n.A#
       |[3:9..3:10): T <= q/n.A#[T]
       |[3:14..3:15): P => q/n.P#
       |[3:16..3:17): T => q/n.A#[T]
       |[4:7..4:9): A2 <= q/n.A2#
       |[4:10..4:11): T <= q/n.A2#[T]
       |[4:15..4:16): A => q/n.A#
       |[4:17..4:18): T => q/n.A2#[T]
       |[5:7..5:8): F <= q/n.F#
       |[5:9..5:10): T <= q/n.F#[T]
       |[5:12..5:13): U <= q/n.F#[U]
       |[5:17..5:18): P => q/n.P#
       |[5:19..5:20): T => q/n.F#[T]
       |[6:7..6:8): L <= q/n.L#
       |[6:9..6:10): T <= q/n.L#[T]
       |[6:14..6:18): java => java/
       |[6:19..6:23): util => java/util/
       |[6:24..6:33): ArrayList => java/util/ArrayList#
       |[6:34..6:35): T => q/n.L#[T]
       |[8:6..8:8): C1 <= q/C1#
       |[8:9..8:9):  <= q/C1#`<init>`().
       |[8:17..8:18): n => q/n.
       |[8:19..8:20): A => q/n.A#
       |[8:20..8:20):  => q/n.P#`<init>`().
       |[9:6..9:8): C2 <= q/C2#
       |[9:9..9:9):  <= q/C2#`<init>`().
       |[9:17..9:18): n => q/n.
       |[9:19..9:21): A2 => q/n.A2#
       |[9:21..9:21):  => q/n.P#`<init>`().
       |[10:6..10:8): C3 <= q/C3#
       |[10:9..10:9):  <= q/C3#`<init>`().
       |[10:17..10:18): n => q/n.
       |[10:19..10:20): F => q/n.F#
       |[10:20..10:20):  => q/n.P#`<init>`().
       |[11:6..11:8): C4 <= q/C4#
       |[11:9..11:9):  <= q/C4#`<init>`().
       |[11:17..11:18): n => q/n.
       |[11:19..11:20): L => q/n.L#
       |[11:20..11:20):  => java/util/ArrayList#`<init>`(+1).
       |[12:6..12:8): C5 <= q/C5#
       |[12:9..12:9):  <= q/C5#`<init>`().
       |[12:17..12:18): n => q/n.
       |[12:19..12:20): P => q/n.P#
       |[12:20..12:20):  => q/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through aliases in scope
  occurrences(
    """|package r
       |object n { class P[T](x: T); type A[T] = P[T] }
       |object m { type A[T] = n.P[T]; type P[T] = n.P[T] }
       |import n.{A => RA}
       |class D1 extends m.A(1)
       |class D2 extends m.P(1)
       |class D3 extends RA(1)
       |object o { import n._; class D4 extends A(1); val v = new A(1) {} }
       |object nest { object k { type A[T] = n.P[T] } }
       |class D5 extends nest.k.A(1)
       |class D6 extends _root_.r.n.A(1)
       |""".stripMargin,
    """|[0:8..0:9): r <= r/
       |[1:7..1:8): n <= r/n.
       |[1:17..1:18): P <= r/n.P#
       |[1:19..1:20): T <= r/n.P#[T]
       |[1:21..1:21):  <= r/n.P#`<init>`().
       |[1:22..1:23): x <= r/n.P#`<init>`().(x)
       |[1:22..1:23): x <= r/n.P#x.
       |[1:25..1:26): T => r/n.P#[T]
       |[1:34..1:35): A <= r/n.A#
       |[1:36..1:37): T <= r/n.A#[T]
       |[1:41..1:42): P => r/n.P#
       |[1:43..1:44): T => r/n.A#[T]
       |[2:7..2:8): m <= r/m.
       |[2:16..2:17): A <= r/m.A#
       |[2:18..2:19): T <= r/m.A#[T]
       |[2:23..2:24): n => r/n.
       |[2:25..2:26): P => r/n.P#
       |[2:27..2:28): T => r/m.A#[T]
       |[2:36..2:37): P <= r/m.P#
       |[2:38..2:39): T <= r/m.P#[T]
       |[2:43..2:44): n => r/n.
       |[2:45..2:46): P => r/n.P#
       |[2:47..2:48): T => r/m.P#[T]
       |[3:7..3:8): n => r/n.
       |[3:10..3:11): A => r/n.A#
       |[4:6..4:8): D1 <= r/D1#
       |[4:9..4:9):  <= r/D1#`<init>`().
       |[4:17..4:18): m => r/m.
       |[4:19..4:20): A => r/m.A#
       |[4:20..4:20):  => r/n.P#`<init>`().
       |[5:6..5:8): D2 <= r/D2#
       |[5:9..5:9):  <= r/D2#`<init>`().
       |[5:17..5:18): m => r/m.
       |[5:19..5:20): P => r/m.P#
       |[5:20..5:20):  => r/n.P#`<init>`().
       |[6:6..6:8): D3 <= r/D3#
       |[6:9..6:9):  <= r/D3#`<init>`().
       |[6:17..6:19): RA => r/n.A#
       |[6:19..6:19):  => r/n.P#`<init>`().
       |[7:7..7:8): o <= r/o.
       |[7:18..7:19): n => r/n.
       |[7:29..7:31): D4 <= r/o.D4#
       |[7:32..7:32):  <= r/o.D4#`<init>`().
       |[7:40..7:41): A => r/n.A#
       |[7:41..7:41):  => r/n.P#`<init>`().
       |[7:50..7:51): v <= r/o.v.
       |[7:58..7:58):  <= local0
       |[7:58..7:59): A => r/n.A#
       |[7:59..7:59):  => r/n.P#`<init>`().
       |[8:7..8:11): nest <= r/nest.
       |[8:21..8:22): k <= r/nest.k.
       |[8:30..8:31): A <= r/nest.k.A#
       |[8:32..8:33): T <= r/nest.k.A#[T]
       |[8:37..8:38): n => r/n.
       |[8:39..8:40): P => r/n.P#
       |[8:41..8:42): T => r/nest.k.A#[T]
       |[9:6..9:8): D5 <= r/D5#
       |[9:9..9:9):  <= r/D5#`<init>`().
       |[9:17..9:21): nest => r/nest.
       |[9:22..9:23): k => r/nest.k.
       |[9:24..9:25): A => r/nest.k.A#
       |[9:25..9:25):  => r/n.P#`<init>`().
       |[10:6..10:8): D6 <= r/D6#
       |[10:9..10:9):  <= r/D6#`<init>`().
       |[10:17..10:23): _root_ => _root_/
       |[10:24..10:25): r => r/
       |[10:26..10:27): n => r/n.
       |[10:28..10:29): A => r/n.A#
       |[10:29..10:29):  => r/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through type projections
  occurrences(
    """|package s
       |object n { class P[T](x: T) }
       |class O2 { type A[T] = n.P[T]; type P[T] = n.P[T] }
       |class E1 extends O2#A(1)
       |class E2 extends O2#P(1)
       |object w { class W { type A[T] = n.P[T] } }
       |class E3 extends w.W#A(1)
       |""".stripMargin,
    """|[0:8..0:9): s <= s/
       |[1:7..1:8): n <= s/n.
       |[1:17..1:18): P <= s/n.P#
       |[1:19..1:20): T <= s/n.P#[T]
       |[1:21..1:21):  <= s/n.P#`<init>`().
       |[1:22..1:23): x <= s/n.P#`<init>`().(x)
       |[1:22..1:23): x <= s/n.P#x.
       |[1:25..1:26): T => s/n.P#[T]
       |[2:6..2:8): O2 <= s/O2#
       |[2:9..2:9):  <= s/O2#`<init>`().
       |[2:16..2:17): A <= s/O2#A#
       |[2:18..2:19): T <= s/O2#A#[T]
       |[2:23..2:24): n => s/n.
       |[2:25..2:26): P => s/n.P#
       |[2:27..2:28): T => s/O2#A#[T]
       |[2:36..2:37): P <= s/O2#P#
       |[2:38..2:39): T <= s/O2#P#[T]
       |[2:43..2:44): n => s/n.
       |[2:45..2:46): P => s/n.P#
       |[2:47..2:48): T => s/O2#P#[T]
       |[3:6..3:8): E1 <= s/E1#
       |[3:9..3:9):  <= s/E1#`<init>`().
       |[3:17..3:19): O2 => s/O2#
       |[3:20..3:21): A => s/O2#A#
       |[3:21..3:21):  => s/n.P#`<init>`().
       |[4:6..4:8): E2 <= s/E2#
       |[4:9..4:9):  <= s/E2#`<init>`().
       |[4:17..4:19): O2 => s/O2#
       |[4:20..4:21): P => s/O2#P#
       |[4:21..4:21):  => s/n.P#`<init>`().
       |[5:7..5:8): w <= s/w.
       |[5:17..5:18): W <= s/w.W#
       |[5:19..5:19):  <= s/w.W#`<init>`().
       |[5:26..5:27): A <= s/w.W#A#
       |[5:28..5:29): T <= s/w.W#A#[T]
       |[5:33..5:34): n => s/n.
       |[5:35..5:36): P => s/n.P#
       |[5:37..5:38): T => s/w.W#A#[T]
       |[6:6..6:8): E3 <= s/E3#
       |[6:9..6:9):  <= s/E3#`<init>`().
       |[6:17..6:18): w => s/w.
       |[6:19..6:20): W => s/w.W#
       |[6:21..6:22): A => s/w.W#A#
       |[6:22..6:22):  => s/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through an alias of the owner's type parameter
  occurrences(
    """|package t
       |object n { class P[T](x: T) }
       |class W[X[_]] { type A[T] = X[T] }
       |object w extends W[n.P]
       |class G1 extends w.A(1)
       |""".stripMargin,
    """|[0:8..0:9): t <= t/
       |[1:7..1:8): n <= t/n.
       |[1:17..1:18): P <= t/n.P#
       |[1:19..1:20): T <= t/n.P#[T]
       |[1:21..1:21):  <= t/n.P#`<init>`().
       |[1:22..1:23): x <= t/n.P#`<init>`().(x)
       |[1:22..1:23): x <= t/n.P#x.
       |[1:25..1:26): T => t/n.P#[T]
       |[2:6..2:7): W <= t/W#
       |[2:8..2:9): X <= t/W#[X]
       |[2:10..2:11): _ <= t/W#[X][_$$1]
       |[2:14..2:14):  <= t/W#`<init>`().
       |[2:21..2:22): A <= t/W#A#
       |[2:23..2:24): T <= t/W#A#[T]
       |[2:28..2:29): X => t/W#[X]
       |[2:30..2:31): T => t/W#A#[T]
       |[3:7..3:8): w <= t/w.
       |[3:17..3:18): W => t/W#
       |[3:19..3:20): n => t/n.
       |[3:21..3:22): P => t/n.P#
       |[3:23..3:23):  => t/W#`<init>`().
       |[4:6..4:8): G1 <= t/G1#
       |[4:9..4:9):  <= t/G1#`<init>`().
       |[4:17..4:18): w => t/w.
       |[4:19..4:20): A => t/W#A#
       |[4:20..4:20):  => t/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through an alias of a type parameter, through an import or the enclosing object
  occurrences(
    """|package u
       |object n { class P[T](x: T) }
       |class W[X[_]] { type A[T] = X[T] }
       |object w extends W[n.P]
       |object g {
       |  import w._
       |  class G2 extends A(1)
       |}
       |object o extends W[n.P] { class G3 extends A(1) }
       |""".stripMargin,
    """|[0:8..0:9): u <= u/
       |[1:7..1:8): n <= u/n.
       |[1:17..1:18): P <= u/n.P#
       |[1:19..1:20): T <= u/n.P#[T]
       |[1:21..1:21):  <= u/n.P#`<init>`().
       |[1:22..1:23): x <= u/n.P#`<init>`().(x)
       |[1:22..1:23): x <= u/n.P#x.
       |[1:25..1:26): T => u/n.P#[T]
       |[2:6..2:7): W <= u/W#
       |[2:8..2:9): X <= u/W#[X]
       |[2:10..2:11): _ <= u/W#[X][_$$1]
       |[2:14..2:14):  <= u/W#`<init>`().
       |[2:21..2:22): A <= u/W#A#
       |[2:23..2:24): T <= u/W#A#[T]
       |[2:28..2:29): X => u/W#[X]
       |[2:30..2:31): T => u/W#A#[T]
       |[3:7..3:8): w <= u/w.
       |[3:17..3:18): W => u/W#
       |[3:19..3:20): n => u/n.
       |[3:21..3:22): P => u/n.P#
       |[3:23..3:23):  => u/W#`<init>`().
       |[4:7..4:8): g <= u/g.
       |[5:9..5:10): w => u/w.
       |[6:8..6:10): G2 <= u/g.G2#
       |[6:11..6:11):  <= u/g.G2#`<init>`().
       |[6:19..6:20): A => u/W#A#
       |[6:20..6:20):  => u/n.P#`<init>`().
       |[8:7..8:8): o <= u/o.
       |[8:17..8:18): W => u/W#
       |[8:19..8:20): n => u/n.
       |[8:21..8:22): P => u/n.P#
       |[8:23..8:23):  => u/W#`<init>`().
       |[8:32..8:34): G3 <= u/o.G3#
       |[8:35..8:35):  <= u/o.G3#`<init>`().
       |[8:43..8:44): A => u/W#A#
       |[8:44..8:44):  => u/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through a projection from a member class of a trait with a type parameter
  occurrences(
    """|package v
       |object n { class P[T](x: T) }
       |trait T[X[_]] { class V { type A[U] = X[U] } }
       |object o extends T[n.P]
       |class E4 extends o.V#A(1)
       |""".stripMargin,
    """|[0:8..0:9): v <= v/
       |[1:7..1:8): n <= v/n.
       |[1:17..1:18): P <= v/n.P#
       |[1:19..1:20): T <= v/n.P#[T]
       |[1:21..1:21):  <= v/n.P#`<init>`().
       |[1:22..1:23): x <= v/n.P#`<init>`().(x)
       |[1:22..1:23): x <= v/n.P#x.
       |[1:25..1:26): T => v/n.P#[T]
       |[2:6..2:7): T <= v/T#
       |[2:8..2:9): X <= v/T#[X]
       |[2:10..2:11): _ <= v/T#[X][_$$1]
       |[2:14..2:14):  <= v/T#`<init>`().
       |[2:22..2:23): V <= v/T#V#
       |[2:24..2:24):  <= v/T#V#`<init>`().
       |[2:31..2:32): A <= v/T#V#A#
       |[2:33..2:34): U <= v/T#V#A#[U]
       |[2:38..2:39): X => v/T#[X]
       |[2:40..2:41): U => v/T#V#A#[U]
       |[3:7..3:8): o <= v/o.
       |[3:17..3:18): T => v/T#
       |[3:19..3:20): n => v/n.
       |[3:21..3:22): P => v/n.P#
       |[3:23..3:23):  => java/lang/Object#`<init>`().
       |[4:6..4:8): E4 <= v/E4#
       |[4:9..4:9):  <= v/E4#`<init>`().
       |[4:17..4:18): o => v/o.
       |[4:19..4:20): V => v/T#V#
       |[4:21..4:22): A => v/T#V#A#
       |[4:22..4:22):  => v/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through aliases local to a block or inherited by the enclosing object
  occurrences(
    """|package x
       |object n { class P[T](x: T) }
       |trait HasA { type A[T] = n.P[T] }
       |object o extends HasA { class C1 extends A(1) }
       |object f { def f = { type A[T] = n.P[T]; new A(1) {} } }
       |""".stripMargin,
    """|[0:8..0:9): x <= x/
       |[1:7..1:8): n <= x/n.
       |[1:17..1:18): P <= x/n.P#
       |[1:19..1:20): T <= x/n.P#[T]
       |[1:21..1:21):  <= x/n.P#`<init>`().
       |[1:22..1:23): x <= x/n.P#`<init>`().(x)
       |[1:22..1:23): x <= x/n.P#x.
       |[1:25..1:26): T => x/n.P#[T]
       |[2:6..2:10): HasA <= x/HasA#
       |[2:18..2:19): A <= x/HasA#A#
       |[2:20..2:21): T <= x/HasA#A#[T]
       |[2:25..2:26): n => x/n.
       |[2:27..2:28): P => x/n.P#
       |[2:29..2:30): T => x/HasA#A#[T]
       |[3:7..3:8): o <= x/o.
       |[3:17..3:21): HasA => x/HasA#
       |[3:21..3:21):  => java/lang/Object#`<init>`().
       |[3:30..3:32): C1 <= x/o.C1#
       |[3:33..3:33):  <= x/o.C1#`<init>`().
       |[3:41..3:42): A => x/HasA#A#
       |[3:42..3:42):  => x/n.P#`<init>`().
       |[4:7..4:8): f <= x/f.
       |[4:15..4:16): f <= x/f.f().
       |[4:26..4:27): A <= local0
       |[4:28..4:29): T <= local1
       |[4:33..4:34): n => x/n.
       |[4:35..4:36): P => x/n.P#
       |[4:37..4:38): T => local1
       |[4:45..4:45):  <= local2
       |[4:45..4:46): A => local0
       |[4:46..4:46):  => x/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through an alias in a package object
  occurrences(
    """|package z
       |object n { class P[T](x: T) }
       |package object p { type A[T] = z.n.P[T] }
       |package p { class C extends A(1) }
       |""".stripMargin,
    """|[0:8..0:9): z <= z/
       |[1:7..1:8): n <= z/n.
       |[1:17..1:18): P <= z/n.P#
       |[1:19..1:20): T <= z/n.P#[T]
       |[1:21..1:21):  <= z/n.P#`<init>`().
       |[1:22..1:23): x <= z/n.P#`<init>`().(x)
       |[1:22..1:23): x <= z/n.P#x.
       |[1:25..1:26): T => z/n.P#[T]
       |[2:15..2:16): p <= z/p/package.
       |[2:24..2:25): A <= z/p/package.A#
       |[2:26..2:27): T <= z/p/package.A#[T]
       |[2:31..2:32): z => z/
       |[2:33..2:34): n => z/n.
       |[2:35..2:36): P => z/n.P#
       |[2:37..2:38): T => z/p/package.A#[T]
       |[3:8..3:9): p <= z/p/
       |[3:18..3:19): C <= z/p/C#
       |[3:20..3:20):  <= z/p/C#`<init>`().
       |[3:28..3:29): A => z/p/package.A#
       |[3:29..3:29):  => z/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through several parents
  occurrences(
    """|package s1
       |object n { class P[T](x: T); type A[T] = P[T]; trait T }
       |class C extends n.A(1) with n.T
       |""".stripMargin,
    """|[0:8..0:10): s1 <= s1/
       |[1:7..1:8): n <= s1/n.
       |[1:17..1:18): P <= s1/n.P#
       |[1:19..1:20): T <= s1/n.P#[T]
       |[1:21..1:21):  <= s1/n.P#`<init>`().
       |[1:22..1:23): x <= s1/n.P#`<init>`().(x)
       |[1:22..1:23): x <= s1/n.P#x.
       |[1:25..1:26): T => s1/n.P#[T]
       |[1:34..1:35): A <= s1/n.A#
       |[1:36..1:37): T <= s1/n.A#[T]
       |[1:41..1:42): P => s1/n.P#
       |[1:43..1:44): T => s1/n.A#[T]
       |[1:53..1:54): T <= s1/n.T#
       |[2:6..2:7): C <= s1/C#
       |[2:8..2:8):  <= s1/C#`<init>`().
       |[2:16..2:17): n => s1/n.
       |[2:18..2:19): A => s1/n.A#
       |[2:19..2:19):  => s1/n.P#`<init>`().
       |[2:28..2:29): n => s1/n.
       |[2:30..2:31): T => s1/n.T#
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through a projection from an applied type
  occurrences(
    """|package s2
       |object n { class P[T](x: T) }
       |class O3[X] { type A[T] = n.P[T] }
       |class C extends O3[Int]#A(1)
       |""".stripMargin,
    """|[0:8..0:10): s2 <= s2/
       |[1:7..1:8): n <= s2/n.
       |[1:17..1:18): P <= s2/n.P#
       |[1:19..1:20): T <= s2/n.P#[T]
       |[1:21..1:21):  <= s2/n.P#`<init>`().
       |[1:22..1:23): x <= s2/n.P#`<init>`().(x)
       |[1:22..1:23): x <= s2/n.P#x.
       |[1:25..1:26): T => s2/n.P#[T]
       |[2:6..2:8): O3 <= s2/O3#
       |[2:9..2:10): X <= s2/O3#[X]
       |[2:12..2:12):  <= s2/O3#`<init>`().
       |[2:19..2:20): A <= s2/O3#A#
       |[2:21..2:22): T <= s2/O3#A#[T]
       |[2:26..2:27): n => s2/n.
       |[2:28..2:29): P => s2/n.P#
       |[2:30..2:31): T => s2/O3#A#[T]
       |[3:6..3:7): C <= s2/C#
       |[3:8..3:8):  <= s2/C#`<init>`().
       |[3:24..3:25): A => s2/n.P#
       |[3:25..3:25):  => s2/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through a local alias that shadows an imported one
  occurrences(
    """|package s3
       |object n { class P[T](x: T); type A[T] = P[T] }
       |object m { class Q[T](x: T) }
       |object k {
       |  import n._
       |  def f = { type A[T] = m.Q[T]; new A(1) {} }
       |}
       |""".stripMargin,
    """|[0:8..0:10): s3 <= s3/
       |[1:7..1:8): n <= s3/n.
       |[1:17..1:18): P <= s3/n.P#
       |[1:19..1:20): T <= s3/n.P#[T]
       |[1:21..1:21):  <= s3/n.P#`<init>`().
       |[1:22..1:23): x <= s3/n.P#`<init>`().(x)
       |[1:22..1:23): x <= s3/n.P#x.
       |[1:25..1:26): T => s3/n.P#[T]
       |[1:34..1:35): A <= s3/n.A#
       |[1:36..1:37): T <= s3/n.A#[T]
       |[1:41..1:42): P => s3/n.P#
       |[1:43..1:44): T => s3/n.A#[T]
       |[2:7..2:8): m <= s3/m.
       |[2:17..2:18): Q <= s3/m.Q#
       |[2:19..2:20): T <= s3/m.Q#[T]
       |[2:21..2:21):  <= s3/m.Q#`<init>`().
       |[2:22..2:23): x <= s3/m.Q#`<init>`().(x)
       |[2:22..2:23): x <= s3/m.Q#x.
       |[2:25..2:26): T => s3/m.Q#[T]
       |[3:7..3:8): k <= s3/k.
       |[4:9..4:10): n => s3/n.
       |[5:6..5:7): f <= s3/k.f().
       |[5:17..5:18): A <= local0
       |[5:19..5:20): T <= local1
       |[5:24..5:25): m => s3/m.
       |[5:26..5:27): Q => s3/m.Q#
       |[5:28..5:29): T => local1
       |[5:36..5:36):  <= local2
       |[5:36..5:37): A => local0
       |[5:37..5:37):  => s3/m.Q#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through a projection from a type parameter of the enclosing class
  occurrences(
    """|package b1
       |object n { class P[T](x: T) }
       |class O2 { type A[T] = n.P[T] }
       |class O[Z] { type A[T] = n.P[T] }
       |class K[O <: O2] { class C extends O#A(1) }
       |class K2[R <: O2] { class C extends R#A(1) }
       |""".stripMargin,
    """|[0:8..0:10): b1 <= b1/
       |[1:7..1:8): n <= b1/n.
       |[1:17..1:18): P <= b1/n.P#
       |[1:19..1:20): T <= b1/n.P#[T]
       |[1:21..1:21):  <= b1/n.P#`<init>`().
       |[1:22..1:23): x <= b1/n.P#`<init>`().(x)
       |[1:22..1:23): x <= b1/n.P#x.
       |[1:25..1:26): T => b1/n.P#[T]
       |[2:6..2:8): O2 <= b1/O2#
       |[2:9..2:9):  <= b1/O2#`<init>`().
       |[2:16..2:17): A <= b1/O2#A#
       |[2:18..2:19): T <= b1/O2#A#[T]
       |[2:23..2:24): n => b1/n.
       |[2:25..2:26): P => b1/n.P#
       |[2:27..2:28): T => b1/O2#A#[T]
       |[3:6..3:7): O <= b1/O#
       |[3:8..3:9): Z <= b1/O#[Z]
       |[3:11..3:11):  <= b1/O#`<init>`().
       |[3:18..3:19): A <= b1/O#A#
       |[3:20..3:21): T <= b1/O#A#[T]
       |[3:25..3:26): n => b1/n.
       |[3:27..3:28): P => b1/n.P#
       |[3:29..3:30): T => b1/O#A#[T]
       |[4:6..4:7): K <= b1/K#
       |[4:8..4:9): O <= b1/K#[O]
       |[4:13..4:15): O2 => b1/O2#
       |[4:17..4:17):  <= b1/K#`<init>`().
       |[4:25..4:26): C <= b1/K#C#
       |[4:27..4:27):  <= b1/K#C#`<init>`().
       |[4:35..4:36): O => b1/K#[O]
       |[4:37..4:38): A => b1/O2#A#
       |[4:38..4:38):  => b1/n.P#`<init>`().
       |[5:6..5:8): K2 <= b1/K2#
       |[5:9..5:10): R <= b1/K2#[R]
       |[5:14..5:16): O2 => b1/O2#
       |[5:18..5:18):  <= b1/K2#`<init>`().
       |[5:26..5:27): C <= b1/K2#C#
       |[5:28..5:28):  <= b1/K2#C#`<init>`().
       |[5:36..5:37): R => b1/K2#[R]
       |[5:38..5:39): A => b1/O2#A#
       |[5:39..5:39):  => b1/n.P#`<init>`().
       |""".stripMargin,
  )

  /* #4919: parents with inferred type arguments, through a projection from a type parameter of the
   * class
   */
  occurrences(
    """|package b2
       |object n { class P[T](x: T) }
       |class O2 { type A[T] = n.P[T] }
       |class T { type A[U] = n.P[U] }
       |class C[T <: O2] extends T#A(1)
       |class D[S <: O2] extends S#A(1)
       |""".stripMargin,
    """|[0:8..0:10): b2 <= b2/
       |[1:7..1:8): n <= b2/n.
       |[1:17..1:18): P <= b2/n.P#
       |[1:19..1:20): T <= b2/n.P#[T]
       |[1:21..1:21):  <= b2/n.P#`<init>`().
       |[1:22..1:23): x <= b2/n.P#`<init>`().(x)
       |[1:22..1:23): x <= b2/n.P#x.
       |[1:25..1:26): T => b2/n.P#[T]
       |[2:6..2:8): O2 <= b2/O2#
       |[2:9..2:9):  <= b2/O2#`<init>`().
       |[2:16..2:17): A <= b2/O2#A#
       |[2:18..2:19): T <= b2/O2#A#[T]
       |[2:23..2:24): n => b2/n.
       |[2:25..2:26): P => b2/n.P#
       |[2:27..2:28): T => b2/O2#A#[T]
       |[3:6..3:7): T <= b2/T#
       |[3:8..3:8):  <= b2/T#`<init>`().
       |[3:15..3:16): A <= b2/T#A#
       |[3:17..3:18): U <= b2/T#A#[U]
       |[3:22..3:23): n => b2/n.
       |[3:24..3:25): P => b2/n.P#
       |[3:26..3:27): U => b2/T#A#[U]
       |[4:6..4:7): C <= b2/C#
       |[4:8..4:9): T <= b2/C#[T]
       |[4:13..4:15): O2 => b2/O2#
       |[4:17..4:17):  <= b2/C#`<init>`().
       |[4:25..4:26): T => b2/C#[T]
       |[4:27..4:28): A => b2/O2#A#
       |[4:28..4:28):  => b2/n.P#`<init>`().
       |[5:6..5:7): D <= b2/D#
       |[5:8..5:9): S <= b2/D#[S]
       |[5:13..5:15): O2 => b2/O2#
       |[5:17..5:17):  <= b2/D#`<init>`().
       |[5:25..5:26): S => b2/D#[S]
       |[5:27..5:28): A => b2/O2#A#
       |[5:28..5:28):  => b2/n.P#`<init>`().
       |""".stripMargin,
  )

  /* #4919: parents with inferred type arguments, through a projection from a type parameter of a
   * method
   */
  occurrences(
    """|package b3
       |object n { class P[T](x: T) }
       |class O2 { type A[T] = n.P[T] }
       |class O { type A[T] = n.P[T] }
       |object k {
       |  def f[O <: O2] = new O#A(1) {}
       |  def g[R <: O2] = new R#A(1) {}
       |}
       |""".stripMargin,
    """|[0:8..0:10): b3 <= b3/
       |[1:7..1:8): n <= b3/n.
       |[1:17..1:18): P <= b3/n.P#
       |[1:19..1:20): T <= b3/n.P#[T]
       |[1:21..1:21):  <= b3/n.P#`<init>`().
       |[1:22..1:23): x <= b3/n.P#`<init>`().(x)
       |[1:22..1:23): x <= b3/n.P#x.
       |[1:25..1:26): T => b3/n.P#[T]
       |[2:6..2:8): O2 <= b3/O2#
       |[2:9..2:9):  <= b3/O2#`<init>`().
       |[2:16..2:17): A <= b3/O2#A#
       |[2:18..2:19): T <= b3/O2#A#[T]
       |[2:23..2:24): n => b3/n.
       |[2:25..2:26): P => b3/n.P#
       |[2:27..2:28): T => b3/O2#A#[T]
       |[3:6..3:7): O <= b3/O#
       |[3:8..3:8):  <= b3/O#`<init>`().
       |[3:15..3:16): A <= b3/O#A#
       |[3:17..3:18): T <= b3/O#A#[T]
       |[3:22..3:23): n => b3/n.
       |[3:24..3:25): P => b3/n.P#
       |[3:26..3:27): T => b3/O#A#[T]
       |[4:7..4:8): k <= b3/k.
       |[5:6..5:7): f <= b3/k.f().
       |[5:8..5:9): O <= b3/k.f().[O]
       |[5:13..5:15): O2 => b3/O2#
       |[5:23..5:23):  <= local0
       |[5:23..5:24): O => b3/k.f().[O]
       |[5:25..5:26): A => b3/O2#A#
       |[5:26..5:26):  => b3/n.P#`<init>`().
       |[6:6..6:7): g <= b3/k.g().
       |[6:8..6:9): R <= b3/k.g().[R]
       |[6:13..6:15): O2 => b3/O2#
       |[6:23..6:23):  <= local1
       |[6:23..6:24): R => b3/k.g().[R]
       |[6:25..6:26): A => b3/O2#A#
       |[6:26..6:26):  => b3/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through an alias through the self type
  occurrences(
    """|package b4
       |object n { class P[T](x: T) }
       |trait HasA { type A[T] = n.P[T] }
       |trait T { self: HasA => class C extends A(1) }
       |""".stripMargin,
    """|[0:8..0:10): b4 <= b4/
       |[1:7..1:8): n <= b4/n.
       |[1:17..1:18): P <= b4/n.P#
       |[1:19..1:20): T <= b4/n.P#[T]
       |[1:21..1:21):  <= b4/n.P#`<init>`().
       |[1:22..1:23): x <= b4/n.P#`<init>`().(x)
       |[1:22..1:23): x <= b4/n.P#x.
       |[1:25..1:26): T => b4/n.P#[T]
       |[2:6..2:10): HasA <= b4/HasA#
       |[2:18..2:19): A <= b4/HasA#A#
       |[2:20..2:21): T <= b4/HasA#A#[T]
       |[2:25..2:26): n => b4/n.
       |[2:27..2:28): P => b4/n.P#
       |[2:29..2:30): T => b4/HasA#A#[T]
       |[3:6..3:7): T <= b4/T#
       |[3:8..3:8):  <= b4/T#`<init>`().
       |[3:10..3:14): self <= local0
       |[3:16..3:20): HasA => b4/HasA#
       |[3:30..3:31): C <= b4/T#C#
       |[3:32..3:32):  <= b4/T#C#`<init>`().
       |[3:40..3:41): A => b4/HasA#A#
       |[3:41..3:41):  => b4/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through an alias inherited by an outer object
  occurrences(
    """|package b5
       |object n { class P[T](x: T) }
       |trait HasA { type A[T] = n.P[T] }
       |object o extends HasA { object i { class C extends A(1) } }
       |""".stripMargin,
    """|[0:8..0:10): b5 <= b5/
       |[1:7..1:8): n <= b5/n.
       |[1:17..1:18): P <= b5/n.P#
       |[1:19..1:20): T <= b5/n.P#[T]
       |[1:21..1:21):  <= b5/n.P#`<init>`().
       |[1:22..1:23): x <= b5/n.P#`<init>`().(x)
       |[1:22..1:23): x <= b5/n.P#x.
       |[1:25..1:26): T => b5/n.P#[T]
       |[2:6..2:10): HasA <= b5/HasA#
       |[2:18..2:19): A <= b5/HasA#A#
       |[2:20..2:21): T <= b5/HasA#A#[T]
       |[2:25..2:26): n => b5/n.
       |[2:27..2:28): P => b5/n.P#
       |[2:29..2:30): T => b5/HasA#A#[T]
       |[3:7..3:8): o <= b5/o.
       |[3:17..3:21): HasA => b5/HasA#
       |[3:21..3:21):  => java/lang/Object#`<init>`().
       |[3:31..3:32): i <= b5/o.i.
       |[3:41..3:42): C <= b5/o.i.C#
       |[3:43..3:43):  <= b5/o.i.C#`<init>`().
       |[3:51..3:52): A => b5/HasA#A#
       |[3:52..3:52):  => b5/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through an alias in the package object of an outer package clause
  occurrences(
    """|package b6
       |object n { class P[T](x: T) }
       |package object p { type A[T] = n.P[T] }
       |package p {
       |  package q { class C extends A(1) }
       |}
       |""".stripMargin,
    """|[0:8..0:10): b6 <= b6/
       |[1:7..1:8): n <= b6/n.
       |[1:17..1:18): P <= b6/n.P#
       |[1:19..1:20): T <= b6/n.P#[T]
       |[1:21..1:21):  <= b6/n.P#`<init>`().
       |[1:22..1:23): x <= b6/n.P#`<init>`().(x)
       |[1:22..1:23): x <= b6/n.P#x.
       |[1:25..1:26): T => b6/n.P#[T]
       |[2:15..2:16): p <= b6/p/package.
       |[2:24..2:25): A <= b6/p/package.A#
       |[2:26..2:27): T <= b6/p/package.A#[T]
       |[2:31..2:32): n => b6/n.
       |[2:33..2:34): P => b6/n.P#
       |[2:35..2:36): T => b6/p/package.A#[T]
       |[3:8..3:9): p <= b6/p/
       |[4:10..4:11): q <= b6/p/q/
       |[4:20..4:21): C <= b6/p/q/C#
       |[4:22..4:22):  <= b6/p/q/C#`<init>`().
       |[4:30..4:31): A => b6/p/package.A#
       |[4:31..4:31):  => b6/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through an alias imported from a value that is not an object
  occurrences(
    """|package b7
       |object n { class P[T](x: T) }
       |class HasA { type A[T] = n.P[T] }
       |object k {
       |  val v = new HasA
       |  import v._
       |  class C extends A(1)
       |  class D extends v.A(1)
       |}
       |""".stripMargin,
    """|[0:8..0:10): b7 <= b7/
       |[1:7..1:8): n <= b7/n.
       |[1:17..1:18): P <= b7/n.P#
       |[1:19..1:20): T <= b7/n.P#[T]
       |[1:21..1:21):  <= b7/n.P#`<init>`().
       |[1:22..1:23): x <= b7/n.P#`<init>`().(x)
       |[1:22..1:23): x <= b7/n.P#x.
       |[1:25..1:26): T => b7/n.P#[T]
       |[2:6..2:10): HasA <= b7/HasA#
       |[2:11..2:11):  <= b7/HasA#`<init>`().
       |[2:18..2:19): A <= b7/HasA#A#
       |[2:20..2:21): T <= b7/HasA#A#[T]
       |[2:25..2:26): n => b7/n.
       |[2:27..2:28): P => b7/n.P#
       |[2:29..2:30): T => b7/HasA#A#[T]
       |[3:7..3:8): k <= b7/k.
       |[4:6..4:7): v <= b7/k.v.
       |[4:14..4:18): HasA => b7/HasA#
       |[4:18..4:18):  => b7/HasA#`<init>`().
       |[5:9..5:10): v => b7/k.v.
       |[6:8..6:9): C <= b7/k.C#
       |[6:10..6:10):  <= b7/k.C#`<init>`().
       |[6:18..6:19): A => b7/HasA#A#
       |[6:19..6:19):  => b7/n.P#`<init>`().
       |[7:8..7:9): D <= b7/k.D#
       |[7:10..7:10):  <= b7/k.D#`<init>`().
       |[7:18..7:19): v => b7/k.v.
       |[7:20..7:21): A => b7/HasA#A#
       |[7:21..7:21):  => b7/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through a projection of a projection
  occurrences(
    """|package b8
       |object n { class P[T](x: T) }
       |class O { class I { type A[T] = n.P[T] } }
       |class C extends O#I#A(1)
       |""".stripMargin,
    """|[0:8..0:10): b8 <= b8/
       |[1:7..1:8): n <= b8/n.
       |[1:17..1:18): P <= b8/n.P#
       |[1:19..1:20): T <= b8/n.P#[T]
       |[1:21..1:21):  <= b8/n.P#`<init>`().
       |[1:22..1:23): x <= b8/n.P#`<init>`().(x)
       |[1:22..1:23): x <= b8/n.P#x.
       |[1:25..1:26): T => b8/n.P#[T]
       |[2:6..2:7): O <= b8/O#
       |[2:8..2:8):  <= b8/O#`<init>`().
       |[2:16..2:17): I <= b8/O#I#
       |[2:18..2:18):  <= b8/O#I#`<init>`().
       |[2:25..2:26): A <= b8/O#I#A#
       |[2:27..2:28): T <= b8/O#I#A#[T]
       |[2:32..2:33): n => b8/n.
       |[2:34..2:35): P => b8/n.P#
       |[2:36..2:37): T => b8/O#I#A#[T]
       |[3:6..3:7): C <= b8/C#
       |[3:8..3:8):  <= b8/C#`<init>`().
       |[3:16..3:17): O => b8/O#
       |[3:18..3:19): I => b8/O#I#
       |[3:20..3:21): A => b8/O#I#A#
       |[3:21..3:21):  => b8/n.P#`<init>`().
       |""".stripMargin,
  )

  // #4919: parents with inferred type arguments, through a projection from a local class
  occurrences(
    """|package b9
       |object n { class P[T](x: T) }
       |object k {
       |  def f = {
       |    class L { type A[T] = n.P[T] }
       |    new L#A(1) {}
       |  }
       |}
       |""".stripMargin,
    """|[0:8..0:10): b9 <= b9/
       |[1:7..1:8): n <= b9/n.
       |[1:17..1:18): P <= b9/n.P#
       |[1:19..1:20): T <= b9/n.P#[T]
       |[1:21..1:21):  <= b9/n.P#`<init>`().
       |[1:22..1:23): x <= b9/n.P#`<init>`().(x)
       |[1:22..1:23): x <= b9/n.P#x.
       |[1:25..1:26): T => b9/n.P#[T]
       |[2:7..2:8): k <= b9/k.
       |[3:6..3:7): f <= b9/k.f().
       |[4:10..4:11): L <= local0
       |[4:12..4:12):  <= local1
       |[4:19..4:20): A <= local2
       |[4:21..4:22): T <= local3
       |[4:26..4:27): n => b9/n.
       |[4:28..4:29): P => b9/n.P#
       |[4:30..4:31): T => local3
       |[5:8..5:8):  <= local4
       |[5:8..5:9): L => local0
       |[5:10..5:11): A => local2
       |[5:11..5:11):  => b9/n.P#`<init>`().
       |""".stripMargin,
  )

}
