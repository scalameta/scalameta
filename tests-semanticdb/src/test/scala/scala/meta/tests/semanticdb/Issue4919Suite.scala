package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/pull/4919
class Issue4919Suite extends SemanticdbSuite {

  // parents with inferred type arguments, through aliases in an object
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

  // parents with inferred type arguments, through aliases in scope
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

  // parents with inferred type arguments, through type projections
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

  // parents with inferred type arguments, through an alias of the owner's type parameter
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

  // parents with inferred type arguments, through an alias of a type parameter, through an import or the enclosing object
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

  // parents with inferred type arguments, through a projection from a member class of a trait with a type parameter
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

  // parents with inferred type arguments, through aliases local to a block or inherited by the enclosing object
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

  // parents with inferred type arguments, through an alias in a package object
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

  // parents with inferred type arguments, through several parents
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

  // parents with inferred type arguments, through a projection from an applied type
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

  // parents with inferred type arguments, through a local alias that shadows an imported one
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

  // parents with inferred type arguments, through a projection from a type parameter of the enclosing class
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

  // parents with inferred type arguments, through a projection from a type parameter of the class
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

  // parents with inferred type arguments, through a projection from a type parameter of a method
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

  // parents with inferred type arguments, through an alias through the self type
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

  // parents with inferred type arguments, through an alias inherited by an outer object
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

  // parents with inferred type arguments, through an alias in the package object of an outer package clause
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

  // parents with inferred type arguments, through an alias imported from a value that is not an object
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

  // parents with inferred type arguments, through a projection of a projection
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

  // parents with inferred type arguments, through a projection from a local class
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
