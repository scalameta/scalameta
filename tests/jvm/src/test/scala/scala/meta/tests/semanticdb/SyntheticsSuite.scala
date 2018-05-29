package scala.meta.tests.semanticdb

class SyntheticsSuite extends SemanticdbSuite() {

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
    """|[12:15..12:15):  => *.apply[Foo, FooDSL]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:7): apply => g.CommandeerDSL.apply(Host,DSL).
       |  [0:8..0:11): Foo => g.Foo#
       |  [0:13..0:19): FooDSL => g.FooDSL#
       |[12:39..12:39):  => *(g.Foo.fooDSL)
       |  [0:0..0:1): * => _star_.
       |  [0:8..0:14): fooDSL => g.Foo.fooDSL().
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
    """|[13:9..13:9):  => *[Int, List[Int]]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:5): Int => scala.Int#
       |  [0:7..0:11): List => scala.collection.immutable.List#
       |  [0:12..0:15): Int => scala.Int#
       |[13:17..13:17):  => *(scala.collection.immutable.List.canBuildFrom[Int])
       |  [0:0..0:1): * => _star_.
       |  [0:34..0:46): canBuildFrom => scala.collection.immutable.List.canBuildFrom().
       |  [0:47..0:50): Int => scala.Int#
       |[16:16..16:16):  => *(h.C.list[Int](h.C.int))
       |  [0:0..0:1): * => _star_.
       |  [0:6..0:10): list => h.C.list(C).
       |  [0:11..0:14): Int => scala.Int#
       |  [0:20..0:23): int => h.C.int().
       |[19:4..19:6): 42 => h.X.cvt[Int](*)(h.C.int)
       |  [0:4..0:7): cvt => h.X.cvt(T,C).
       |  [0:8..0:11): Int => scala.Int#
       |  [0:13..0:14): * => _star_.
       |  [0:20..0:23): int => h.C.int().
       |[22:3..22:3):  => *[C[Int]]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:3): C => h.C#
       |  [0:4..0:7): Int => scala.Int#
    """.trim.stripMargin
  )

  synthetics(
    "class J[T: Manifest] { val arr = Array.empty[T] }",
    """|[0:47..0:47):  => *(J.this.evidence$1)
       |  [0:0..0:1): * => _star_.
       |  [0:9..0:19): evidence$1 => _empty_.J#evidence$1().
       |""".trim.stripMargin
  )
  synthetics(
    """|object q {
       |  List(1) + "blaH"
       |}
    """.stripMargin,
    """|[1:2..1:9): List(1) => scala.Predef.any2stringadd[List[Int]](*)
       |  [0:13..0:26): any2stringadd => scala.Predef.any2stringadd(A).
       |  [0:27..0:31): List => scala.collection.immutable.List#
       |  [0:32..0:35): Int => scala.Int#
       |  [0:38..0:39): * => _star_.
       |[1:6..1:6):  => *.apply[Int]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:7): apply => scala.collection.immutable.List.apply(A*).
       |  [0:8..0:11): Int => scala.Int#
       |""".stripMargin
  )

  synthetics(
    """|object r {
       |  class F
       |  implicit val ordering: Ordering[F] = ???
       |  val x: Ordered[F] = new F
       |}
    """.stripMargin,
    """|[3:22..3:27): new F => scala.math.Ordered.orderingToOrdered[F](*)(r.this.ordering)
       |  [0:19..0:36): orderingToOrdered => scala.math.Ordered.orderingToOrdered(T,Ordering).
       |  [0:37..0:38): F => _empty_.r.F#
       |  [0:40..0:41): * => _star_.
       |  [0:50..0:58): ordering => _empty_.r.ordering().
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
    """|[2:3..2:3):  => *.apply
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:7): apply => _empty_.s.apply().
       |[5:5..5:5):  => *.apply
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:7): apply => _empty_.s.Bar.apply().
       |[6:28..6:28):  => *.apply
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:7): apply => scala.Function1#apply(T1).
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
    """|[1:11..1:12): 1 => scala.Predef.intWrapper(*)
       |  [0:13..0:23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
       |  [0:24..0:25): * => _star_.
       |[1:18..1:18):  => *.foreach[Unit]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:9): foreach => scala.collection.immutable.Range#foreach(Function1).
       |  [0:10..0:14): Unit => scala.Unit#
       |[1:25..1:26): 0 => scala.Predef.intWrapper(*)
       |  [0:13..0:23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
       |  [0:24..0:25): * => _star_.
       |[1:35..1:35):  => *.foreach[Unit]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:9): foreach => scala.collection.immutable.Range#foreach(Function1).
       |  [0:10..0:14): Unit => scala.Unit#
       |[1:45..1:46): x => scala.Predef.ArrowAssoc[Int](*)
       |  [0:13..0:23): ArrowAssoc => scala.Predef.ArrowAssoc(A).
       |  [0:24..0:27): Int => scala.Int#
       |  [0:29..0:30): * => _star_.
       |[1:49..1:49):  => *[Int]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:5): Int => scala.Int#
       |[2:11..2:12): 1 => scala.Predef.intWrapper(*)
       |  [0:13..0:23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
       |  [0:24..0:25): * => _star_.
       |[2:18..2:18):  => *.flatMap[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:9): flatMap => scala.collection.TraversableLike#flatMap(Function1,CanBuildFrom).
       |  [0:10..0:16): Tuple2 => scala.Tuple2#
       |  [0:17..0:20): Int => scala.Int#
       |  [0:22..0:25): Int => scala.Int#
       |  [0:28..0:38): IndexedSeq => scala.collection.immutable.IndexedSeq#
       |  [0:39..0:45): Tuple2 => scala.Tuple2#
       |  [0:46..0:49): Int => scala.Int#
       |  [0:51..0:54): Int => scala.Int#
       |  [0:58..0:59): * => _star_.
       |  [0:99..0:111): canBuildFrom => scala.collection.immutable.IndexedSeq.canBuildFrom().
       |  [0:112..0:118): Tuple2 => scala.Tuple2#
       |  [0:119..0:122): Int => scala.Int#
       |  [0:124..0:127): Int => scala.Int#
       |[2:25..2:26): 0 => scala.Predef.intWrapper(*)
       |  [0:13..0:23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
       |  [0:24..0:25): * => _star_.
       |[2:35..2:35):  => *.map[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:5): map => scala.collection.TraversableLike#map(Function1,CanBuildFrom).
       |  [0:6..0:12): Tuple2 => scala.Tuple2#
       |  [0:13..0:16): Int => scala.Int#
       |  [0:18..0:21): Int => scala.Int#
       |  [0:24..0:34): IndexedSeq => scala.collection.immutable.IndexedSeq#
       |  [0:35..0:41): Tuple2 => scala.Tuple2#
       |  [0:42..0:45): Int => scala.Int#
       |  [0:47..0:50): Int => scala.Int#
       |  [0:54..0:55): * => _star_.
       |  [0:95..0:107): canBuildFrom => scala.collection.immutable.IndexedSeq.canBuildFrom().
       |  [0:108..0:114): Tuple2 => scala.Tuple2#
       |  [0:115..0:118): Int => scala.Int#
       |  [0:120..0:123): Int => scala.Int#
       |[3:11..3:12): 1 => scala.Predef.intWrapper(*)
       |  [0:13..0:23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
       |  [0:24..0:25): * => _star_.
       |[3:18..3:18):  => *.flatMap[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:9): flatMap => scala.collection.TraversableLike#flatMap(Function1,CanBuildFrom).
       |  [0:10..0:16): Tuple2 => scala.Tuple2#
       |  [0:17..0:20): Int => scala.Int#
       |  [0:22..0:25): Int => scala.Int#
       |  [0:28..0:38): IndexedSeq => scala.collection.immutable.IndexedSeq#
       |  [0:39..0:45): Tuple2 => scala.Tuple2#
       |  [0:46..0:49): Int => scala.Int#
       |  [0:51..0:54): Int => scala.Int#
       |  [0:58..0:59): * => _star_.
       |  [0:99..0:111): canBuildFrom => scala.collection.immutable.IndexedSeq.canBuildFrom().
       |  [0:112..0:118): Tuple2 => scala.Tuple2#
       |  [0:119..0:122): Int => scala.Int#
       |  [0:124..0:127): Int => scala.Int#
       |[3:25..3:26): 0 => scala.Predef.intWrapper(*)
       |  [0:13..0:23): intWrapper => scala.LowPriorityImplicits#intWrapper(Int).
       |  [0:24..0:25): * => _star_.
       |[3:35..3:35):  => *.withFilter
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:12): withFilter => scala.collection.TraversableLike#withFilter(Function1).
       |[3:49..3:49):  => *.map[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:5): map => scala.collection.generic.FilterMonadic#map(Function1,CanBuildFrom).
       |  [0:6..0:12): Tuple2 => scala.Tuple2#
       |  [0:13..0:16): Int => scala.Int#
       |  [0:18..0:21): Int => scala.Int#
       |  [0:24..0:34): IndexedSeq => scala.collection.immutable.IndexedSeq#
       |  [0:35..0:41): Tuple2 => scala.Tuple2#
       |  [0:42..0:45): Int => scala.Int#
       |  [0:47..0:50): Int => scala.Int#
       |  [0:54..0:55): * => _star_.
       |  [0:95..0:107): canBuildFrom => scala.collection.immutable.IndexedSeq.canBuildFrom().
       |  [0:108..0:114): Tuple2 => scala.Tuple2#
       |  [0:115..0:118): Int => scala.Int#
       |  [0:120..0:123): Int => scala.Int#
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
    """|[3:43..3:43):  => *[Int]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:5): Int => scala.Int#
       |[3:46..3:46):  => *.foreach[Unit](*)(scala.concurrent.ExecutionContext.Implicits.global)
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:9): foreach => scala.concurrent.Future#foreach(Function1,ExecutionContext).
       |  [0:10..0:14): Unit => scala.Unit#
       |  [0:16..0:17): * => _star_.
       |  [0:63..0:69): global => scala.concurrent.ExecutionContext.Implicits.global().
       |[4:43..4:43):  => *[Int]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:5): Int => scala.Int#
       |[4:46..4:46):  => *.foreach[Unit](*)(scala.concurrent.ExecutionContext.Implicits.global)
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:9): foreach => scala.concurrent.Future#foreach(Function1,ExecutionContext).
       |  [0:10..0:14): Unit => scala.Unit#
       |  [0:16..0:17): * => _star_.
       |  [0:63..0:69): global => scala.concurrent.ExecutionContext.Implicits.global().
       |[7:43..7:43):  => *[Int]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:5): Int => scala.Int#
       |[7:46..7:46):  => *.flatMap[Int](*)(scala.concurrent.ExecutionContext.Implicits.global)
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:9): flatMap => scala.concurrent.Future#flatMap(Function1,ExecutionContext).
       |  [0:10..0:13): Int => scala.Int#
       |  [0:15..0:16): * => _star_.
       |  [0:62..0:68): global => scala.concurrent.ExecutionContext.Implicits.global().
       |[8:43..8:43):  => *[Int]
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:5): Int => scala.Int#
       |[8:46..8:46):  => *.withFilter(*)(scala.concurrent.ExecutionContext.Implicits.global)
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:12): withFilter => scala.concurrent.Future#withFilter(Function1,ExecutionContext).
       |  [0:13..0:14): * => _star_.
       |  [0:60..0:66): global => scala.concurrent.ExecutionContext.Implicits.global().
       |[9:12..9:12):  => *.map[Int](*)(scala.concurrent.ExecutionContext.Implicits.global)
       |  [0:0..0:1): * => _star_.
       |  [0:2..0:5): map => scala.concurrent.Future#map(Function1,ExecutionContext).
       |  [0:6..0:9): Int => scala.Int#
       |  [0:11..0:12): * => _star_.
       |  [0:58..0:64): global => scala.concurrent.ExecutionContext.Implicits.global().
    """.trim.stripMargin
  )
}
