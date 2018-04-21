package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac.SemanticdbMode

class SyntheticsSuite extends DatabaseSuite(SemanticdbMode.Slim) {

  synthetics(
    """
      |package g
      |import scala.language.higherKinds
      |trait CommandeerDSL[Host]
      |object CommandeerDSL {
      |  def apply[Host, DSL <: CommandeerDSL[Host]](host: Host)(implicit dsl: DSL): DSL = dsl
      |}
      |trait Foo
      |object Foo {
      |  implicit val fooDSL: FooDSL = new FooDSL {}
      |}
      |trait FooDSL extends CommandeerDSL[Foo]
      |object RunMe {
      |  CommandeerDSL(null.asInstanceOf[Foo])
      |}
    """.trim.stripMargin,
    """|[324..324): *.apply[Foo, FooDSL]
       |  [0..1): * => _star_.
       |  [2..7): apply => g.CommandeerDSL.apply(Host,DSL).
       |  [8..11): Foo => g.Foo#
       |  [13..19): FooDSL => g.FooDSL#
       |[348..348): *(g.Foo.fooDSL)
       |  [0..1): * => _star_.
       |  [8..14): fooDSL => g.Foo.fooDSL().
    """.trim.stripMargin
  )

  synthetics(
    """
      |package h
      |class C[T]
      |object C {
      |  implicit def int: C[Int] = new C[Int]
      |  implicit def list[T: C]: C[List[T]] = ???
      |}
      |
      |class X
      |object X {
      |  implicit def cvt[T: C](x: T): X = ???
      |}
      |
      |object M {
      |  Nil.map(x => 2)
      |
      |  def c[T: C] = ???
      |  M.c[List[Int]]
      |
      |  def x(x: X) = ???
      |  x(42)
      |
      |  def i[T](t: T) = ???
      |  i(new C[Int])
      |}
    """.trim.stripMargin,
    """|[201..201): *[Int, List[Int]]
       |  [0..1): * => _star_.
       |  [2..5): Int => scala.Int#
       |  [12..15): Int => scala.Int#
       |  [7..11): List => scala.collection.immutable.List#
       |[209..209): *(scala.collection.immutable.List.canBuildFrom[Int])
       |  [0..1): * => _star_.
       |  [47..50): Int => scala.Int#
       |  [34..46): canBuildFrom => scala.collection.immutable.List.canBuildFrom().
       |[247..247): *(h.C.list[Int](h.C.int))
       |  [0..1): * => _star_.
       |  [11..14): Int => scala.Int#
       |  [6..10): list => h.C.list(C).
       |  [20..23): int => h.C.int().
       |[273..275): h.X.cvt[Int](*)(h.C.int)
       |  [8..11): Int => scala.Int#
       |  [4..7): cvt => h.X.cvt(T,C).
       |  [13..14): * => _star_.
       |  [20..23): int => h.C.int().
       |[304..304): *[C[Int]]
       |  [0..1): * => _star_.
       |  [4..7): Int => scala.Int#
       |  [2..3): C => h.C#
    """.trim.stripMargin
  )

  synthetics(
    "class J[T: Manifest] { val arr = Array.empty[T] }",
    """|[47..47): *(J.this.evidence$1)
       |  [0..1): * => _star_.
       |  [9..19): evidence$1 => _empty_.J#evidence$1.
       |""".trim.stripMargin
  )
  synthetics(
    """|object q {
       |  List(1) + "blaH"
       |}
    """.stripMargin,
    """|[13..20): scala.Predef.any2stringadd[List[Int]](*)
       |  [27..31): List => scala.collection.immutable.List#
       |  [13..26): any2stringadd => scala.Predef.any2stringadd(A).
       |  [32..35): Int => scala.Int#
       |  [38..39): * => _star_.
       |[17..17): *.apply[Int]
       |  [0..1): * => _star_.
       |  [2..7): apply => scala.collection.immutable.List.apply(A*).
       |  [8..11): Int => scala.Int#
       |""".stripMargin
  )

  synthetics(
    """|object r {
       |  class F
       |  implicit val ordering: Ordering[F] = ???
       |  val x: Ordered[F] = new F
       |}
    """.stripMargin,
    """|[86..91): scala.math.Ordered.orderingToOrdered[F](*)(r.this.ordering)
       |  [19..36): orderingToOrdered => scala.math.Ordered.orderingToOrdered(T,Ordering).
       |  [37..38): F => _empty_.r.F#
       |  [40..41): * => _star_.
       |  [50..58): ordering => _empty_.r.ordering().
       |""".stripMargin
  )

  synthetics(
    """|object s {
       |  def apply() = 2
       |  s()
       |  s.apply()
       |  case class Bar()
       |  Bar()
       |  1.asInstanceOf[Int => Int](2)
       |}
    """.stripMargin,
    """|[32..32): *.apply
       |  [0..1): * => _star_.
       |  [2..7): apply => _empty_.s.apply().
       |[71..71): *.apply
       |  [0..1): * => _star_.
       |  [2..7): apply => _empty_.s.Bar.apply().
       |[102..102): *.apply
       |  [0..1): * => _star_.
       |  [2..7): apply => scala.Function1#apply(T1).
       |""".stripMargin
  )

  synthetics(
    """
      |object ah {
      | for (x <- 1 to 10; y <- 0 until 10) println(x -> x)
      | for (i <- 1 to 10; j <- 0 until 10) yield (i, j)
      | for (i <- 1 to 10; j <- 0 until 10 if i % 2 == 0) yield (i, j)
      |}
    """.trim.stripMargin,
    """
      |[23..24): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
      |  [24..25): * => _star_.
      |[30..30): *.foreach[Unit]
      |  [0..1): * => _star_.
      |  [2..9): foreach => scala.collection.immutable.Range#foreach(Function1).
      |  [10..14): Unit => scala.Unit#
      |[37..38): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
      |  [24..25): * => _star_.
      |[47..47): *.foreach[Unit]
      |  [0..1): * => _star_.
      |  [2..9): foreach => scala.collection.immutable.Range#foreach(Function1).
      |  [10..14): Unit => scala.Unit#
      |[57..58): scala.Predef.ArrowAssoc[Int](*)
      |  [13..23): ArrowAssoc => scala.Predef.ArrowAssoc(A).
      |  [24..27): Int => scala.Int#
      |  [29..30): * => _star_.
      |[61..61): *[Int]
      |  [0..1): * => _star_.
      |  [2..5): Int => scala.Int#
      |[76..77): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
      |  [24..25): * => _star_.
      |[83..83): *.flatMap[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
      |  [0..1): * => _star_.
      |  [2..9): flatMap => scala.collection.TraversableLike#flatMap(Function1,CanBuildFrom).
      |  [22..25): Int => scala.Int#
      |  [10..16): Tuple2 => scala.Tuple2#
      |  [17..20): Int => scala.Int#
      |  [28..38): IndexedSeq => scala.collection.immutable.IndexedSeq#
      |  [39..45): Tuple2 => scala.Tuple2#
      |  [46..49): Int => scala.Int#
      |  [51..54): Int => scala.Int#
      |  [58..59): * => _star_.
      |  [112..118): Tuple2 => scala.Tuple2#
      |  [119..122): Int => scala.Int#
      |  [124..127): Int => scala.Int#
      |  [99..111): canBuildFrom => scala.collection.immutable.IndexedSeq.canBuildFrom().
      |[90..91): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
      |  [24..25): * => _star_.
      |[100..100): *.map[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
      |  [0..1): * => _star_.
      |  [2..5): map => scala.collection.TraversableLike#map(Function1,CanBuildFrom).
      |  [18..21): Int => scala.Int#
      |  [6..12): Tuple2 => scala.Tuple2#
      |  [13..16): Int => scala.Int#
      |  [24..34): IndexedSeq => scala.collection.immutable.IndexedSeq#
      |  [35..41): Tuple2 => scala.Tuple2#
      |  [42..45): Int => scala.Int#
      |  [47..50): Int => scala.Int#
      |  [54..55): * => _star_.
      |  [108..114): Tuple2 => scala.Tuple2#
      |  [115..118): Int => scala.Int#
      |  [120..123): Int => scala.Int#
      |  [95..107): canBuildFrom => scala.collection.immutable.IndexedSeq.canBuildFrom().
      |[126..127): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
      |  [24..25): * => _star_.
      |[133..133): *.flatMap[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
      |  [0..1): * => _star_.
      |  [2..9): flatMap => scala.collection.TraversableLike#flatMap(Function1,CanBuildFrom).
      |  [22..25): Int => scala.Int#
      |  [10..16): Tuple2 => scala.Tuple2#
      |  [17..20): Int => scala.Int#
      |  [28..38): IndexedSeq => scala.collection.immutable.IndexedSeq#
      |  [39..45): Tuple2 => scala.Tuple2#
      |  [46..49): Int => scala.Int#
      |  [51..54): Int => scala.Int#
      |  [58..59): * => _star_.
      |  [112..118): Tuple2 => scala.Tuple2#
      |  [119..122): Int => scala.Int#
      |  [124..127): Int => scala.Int#
      |  [99..111): canBuildFrom => scala.collection.immutable.IndexedSeq.canBuildFrom().
      |[140..141): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
      |  [24..25): * => _star_.
      |[150..150): *.withFilter
      |  [0..1): * => _star_.
      |  [2..12): withFilter => scala.collection.TraversableLike#withFilter(Function1).
      |[164..164): *.map[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
      |  [0..1): * => _star_.
      |  [2..5): map => scala.collection.generic.FilterMonadic#map(Function1,CanBuildFrom).
      |  [18..21): Int => scala.Int#
      |  [6..12): Tuple2 => scala.Tuple2#
      |  [13..16): Int => scala.Int#
      |  [24..34): IndexedSeq => scala.collection.immutable.IndexedSeq#
      |  [35..41): Tuple2 => scala.Tuple2#
      |  [42..45): Int => scala.Int#
      |  [47..50): Int => scala.Int#
      |  [54..55): * => _star_.
      |  [108..114): Tuple2 => scala.Tuple2#
      |  [115..118): Int => scala.Int#
      |  [120..123): Int => scala.Int#
      |  [95..107): canBuildFrom => scala.collection.immutable.IndexedSeq.canBuildFrom().
    """.trim.stripMargin
  )

  synthetics(
    """
      |object ai {
      |  import scala.concurrent.ExecutionContext.Implicits.global
      |  for {
      |    a <- scala.concurrent.Future.successful(1)
      |    b <- scala.concurrent.Future.successful(2)
      |  } println(a)
      |  for {
      |    a <- scala.concurrent.Future.successful(1)
      |    b <- scala.concurrent.Future.successful(2)
      |    if a < b
      |  } yield a
      |}
    """.trim.stripMargin,
    """
      |[123..123): *[Int]
      |  [0..1): * => _star_.
      |  [2..5): Int => scala.Int#
      |[126..126): *.foreach[Unit](*)(scala.concurrent.ExecutionContext.Implicits.global)
      |  [0..1): * => _star_.
      |  [2..9): foreach => scala.concurrent.Future#foreach(Function1,ExecutionContext).
      |  [10..14): Unit => scala.Unit#
      |  [16..17): * => _star_.
      |  [63..69): global => scala.concurrent.ExecutionContext.Implicits.global().
      |[170..170): *[Int]
      |  [0..1): * => _star_.
      |  [2..5): Int => scala.Int#
      |[173..173): *.foreach[Unit](*)(scala.concurrent.ExecutionContext.Implicits.global)
      |  [0..1): * => _star_.
      |  [2..9): foreach => scala.concurrent.Future#foreach(Function1,ExecutionContext).
      |  [10..14): Unit => scala.Unit#
      |  [16..17): * => _star_.
      |  [63..69): global => scala.concurrent.ExecutionContext.Implicits.global().
      |[240..240): *[Int]
      |  [0..1): * => _star_.
      |  [2..5): Int => scala.Int#
      |[243..243): *.flatMap[Int](*)(scala.concurrent.ExecutionContext.Implicits.global)
      |  [0..1): * => _star_.
      |  [2..9): flatMap => scala.concurrent.Future#flatMap(Function1,ExecutionContext).
      |  [10..13): Int => scala.Int#
      |  [15..16): * => _star_.
      |  [62..68): global => scala.concurrent.ExecutionContext.Implicits.global().
      |[287..287): *[Int]
      |  [0..1): * => _star_.
      |  [2..5): Int => scala.Int#
      |[290..290): *.withFilter(*)(scala.concurrent.ExecutionContext.Implicits.global)
      |  [0..1): * => _star_.
      |  [2..12): withFilter => scala.concurrent.Future#withFilter(Function1,ExecutionContext).
      |  [13..14): * => _star_.
      |  [60..66): global => scala.concurrent.ExecutionContext.Implicits.global().
      |[303..303): *.map[Int](*)(scala.concurrent.ExecutionContext.Implicits.global)
      |  [0..1): * => _star_.
      |  [2..5): map => scala.concurrent.Future#map(Function1,ExecutionContext).
      |  [6..9): Int => scala.Int#
      |  [11..12): * => _star_.
      |  [58..64): global => scala.concurrent.ExecutionContext.Implicits.global().
    """.trim.stripMargin
  )
}
