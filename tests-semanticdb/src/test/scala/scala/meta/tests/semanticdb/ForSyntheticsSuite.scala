package scala.meta.tests.semanticdb

class ForSyntheticsSuite extends SemanticdbSuite {

  // for comprehensions (#4135)
  synthetics(
    """|package x
       |class C {
       |  for { a <- Option(1); b <- Option(2); c = a + b } yield (a, b, c)
       |  for { (a, b) <- Option((1, 2)) } yield a + b
       |  for { a <- Option(1); (b, c) <- Option((a, a)) } yield b + c
       |}
       |""".stripMargin,
    """|[2:2..2:67): for { a <- Option(1); b <- Option(2); c = a + b } yield (a, b, c) => orig(Option(1)).flatMap[Tuple3[Int, Int, Int]]({(a) => orig(Option(2)).map[Tuple2[Int, Int]]({(b) => orig(c = a + b)}).map[Tuple3[Int, Int, Int]]({(x$1) => orig((a, b, c))})})
       |  flatMap => scala/Option#flatMap().
       |  Tuple3 => scala/Tuple3#
       |  Int => scala/Int#
       |  a => local0
       |  map => scala/Option#map().
       |  Tuple2 => scala/Tuple2#
       |  b => local1
       |  x$1 => local2
       |[2:13..2:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[2:29..2:35): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[3:2..3:46): for { (a, b) <- Option((1, 2)) } yield a + b => orig(Option((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$2) => orig(a + b)})
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$1 => local4
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |  x$2 => local5
       |[3:18..3:24): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |[4:2..4:62): for { a <- Option(1); (b, c) <- Option((a, a)) } yield b + c => orig(Option(1)).flatMap[Int]({(a) => orig(Option((a, a))).withFilter({(check$ifrefutable$2) => orig()}).map[Int]({(x$3) => orig(b + c)})})
       |  flatMap => scala/Option#flatMap().
       |  Int => scala/Int#
       |  a => local8
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$2 => local9
       |  map => scala/Option#WithFilter#map().
       |  x$3 => local10
       |[4:13..4:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[4:34..4:40): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { (a, b) <- Option((1, 2)) if a > 0 } yield b
       |}
       |""".stripMargin,
    """|[1:2..1:51): for { (a, b) <- Option((1, 2)) if a > 0 } yield b => orig(Option((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).withFilter({(x$1) => orig(a > 0)}).map[Int]({(x$2) => orig(b)})
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$1 => local0
       |  withFilter => scala/Option#WithFilter#withFilter().
       |  x$1 => local1
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |  x$2 => local2
       |[1:18..1:24): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { (a, b) <- List((1, 2)) } println(a + b)
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:47): for { (a, b) <- List((1, 2)) } println(a + b) => orig(List((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).foreach[Unit]({(x$1) => orig(println(a + b))})
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  foreach => scala/collection/generic/FilterMonadic#foreach().
         |  Unit => scala/Unit#
         |  x$1 => local1
         |[1:18..1:22): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/immutable/List.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:47): for { (a, b) <- List((1, 2)) } println(a + b) => orig(List((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).foreach[Unit]({(x$1) => orig(println(a + b))})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  foreach => scala/collection/WithFilter#foreach().
         |  Unit => scala/Unit#
         |  x$1 => local1
         |[1:18..1:22): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/IterableFactory#apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { (a, b) <- Option((1, 2)); c <- Option(a) } yield b + c
       |}
       |""".stripMargin,
    """|[1:2..1:62): for { (a, b) <- Option((1, 2)); c <- Option(a) } yield b + c => orig(Option((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).flatMap[Int]({(x$1) => orig(Option(a)).map[Int]({(c) => orig(b + c)})})
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$1 => local0
       |  flatMap => scala/Option#WithFilter#flatMap().
       |  Int => scala/Int#
       |  x$1 => local1
       |  map => scala/Option#map().
       |  c => local2
       |[1:18..1:24): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |[1:39..1:45): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { a <- Option(1); b = a; (c, d) <- Option((a, b)) } yield c + d
       |}
       |""".stripMargin,
    """|[1:2..1:69): for { a <- Option(1); b = a; (c, d) <- Option((a, b)) } yield c + d => orig(Option(1)).map[Tuple2[Int, Int]]({(a) => orig(b = a)}).flatMap[Int]({(x$2) => orig(Option((a, b))).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(c + d)})})
       |  map => scala/Option#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  a => local0
       |  flatMap => scala/Option#flatMap().
       |  x$2 => local1
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$1 => local2
       |  map => scala/Option#WithFilter#map().
       |  x$1 => local3
       |[1:13..1:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[1:41..1:47): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { a <- Option(1); (b, c) = (a, a) } yield b + c
       |}
       |""".stripMargin,
    """|[1:2..1:53): for { a <- Option(1); (b, c) = (a, a) } yield b + c => orig(Option(1)).map[Tuple2[Int, Tuple2[Int, Int]]]({(a) => orig((b, c) = (a, a))}).map[Int]({(x$3) => orig(b + c)})
       |  map => scala/Option#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  a => local0
       |  x$3 => local1
       |[1:13..1:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[1:24..1:30): (b, c) => *.apply[Tuple2[Int, Int], Int, Int]
       |  apply => scala/Tuple3.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { (a, b) <- Option((1, 2)) } yield for { (c, d) <- Option((a, b)) } yield c + d
       |}
       |""".stripMargin,
    """|[1:2..1:85): for { (a, b) <- Option((1, 2)) } yield for { (c, d) <- Option((a, b)) } yield c + d => orig(Option((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).map[Option[Int]]({(x$2) => orig(Option((a, b))).withFilter({(check$ifrefutable$2) => orig()}).map[Int]({(x$1) => orig(c + d)})})
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$1 => local0
       |  map => scala/Option#WithFilter#map().
       |  Option => scala/Option#
       |  Int => scala/Int#
       |  x$2 => local1
       |  check$ifrefutable$2 => local2
       |  x$1 => local3
       |[1:18..1:24): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |[1:57..1:63): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { a: Int <- List(1) } yield a
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:35): for { a: Int <- List(1) } yield a => orig(List(1)).withFilter({(check$ifrefutable$1) => orig()}).map[Int, List[Int]]({(a) => orig(a)})(List.canBuildFrom[Int])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/generic/FilterMonadic#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  a => local1
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[1:18..1:22): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:35): for { a: Int <- List(1) } yield a => orig(List(1)).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(a) => orig(a)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/WithFilter#map().
         |  Int => scala/Int#
         |  a => local1
         |[1:18..1:22): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { t @ (a, b) <- Option((1, 2)) } yield t
       |}
       |""".stripMargin,
    """|[1:2..1:46): for { t @ (a, b) <- Option((1, 2)) } yield t => orig(Option((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).map[Tuple2[Int, Int]]({(x$1) => orig(t)})
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$1 => local0
       |  map => scala/Option#WithFilter#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  x$1 => local1
       |[1:22..1:28): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { Some(a) <- List(Option(1)) } yield a
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:44): for { Some(a) <- List(Option(1)) } yield a => orig(List(Option(1))).withFilter({(check$ifrefutable$1) => orig()}).map[Int, List[Int]]({(x$1) => orig(a)})(List.canBuildFrom[Int])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/generic/FilterMonadic#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x$1 => local1
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[1:19..1:23): List => *.apply[Option[Int]]
         |  apply => scala/collection/immutable/List.apply().
         |  Option => scala/Option#
         |  Int => scala/Int#
         |[1:24..1:30): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:44): for { Some(a) <- List(Option(1)) } yield a => orig(List(Option(1))).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(a)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/WithFilter#map().
         |  Int => scala/Int#
         |  x$1 => local1
         |[1:19..1:23): List => *.apply[Option[Int]]
         |  apply => scala/collection/IterableFactory#apply().
         |  Option => scala/Option#
         |  Int => scala/Int#
         |[1:24..1:30): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { a @ b <- Option(1) } yield a + b
       |}
       |""".stripMargin,
    """|[1:2..1:40): for { a @ b <- Option(1) } yield a + b => orig(Option(1)).map[Int]({(x$1) => orig(a + b)})
       |  map => scala/Option#map().
       |  Int => scala/Int#
       |  x$1 => local0
       |[1:17..1:23): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |""".stripMargin,
  )

  synthetics(
    """|class W[A](val a: A) {
       |  def withFilter[B](p: A => Boolean): W[A] = this
       |  def map[B](f: A => B): W[B] = new W(f(a))
       |}
       |class C {
       |  for { (b, c) <- new W((1, 2)) } yield b + c
       |}
       |""".stripMargin,
    """|[2:38..2:39): f => *.apply
       |  apply => scala/Function1#apply().
       |[5:2..5:45): for { (b, c) <- new W((1, 2)) } yield b + c => orig(new W((1, 2))).withFilter[Nothing]({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(b + c)})
       |  withFilter => _empty_/W#withFilter().
       |  Nothing => scala/Nothing#
       |  check$ifrefutable$1 => local0
       |  map => _empty_/W#map().
       |  Int => scala/Int#
       |  x$1 => local1
       |""".stripMargin,
  )

  synthetics(
    """|class W[A](val a: A) {
       |  def withFilter(p: A => Boolean)(implicit d: DummyImplicit): W[A] = this
       |  def map[B](f: A => B): W[B] = new W(f(a))
       |}
       |class C {
       |  for { (b, c) <- new W((1, 2)) } yield b + c
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[2:38..2:39): f => *.apply
         |  apply => scala/Function1#apply().
         |[5:2..5:45): for { (b, c) <- new W((1, 2)) } yield b + c => orig(new W((1, 2))).withFilter({(check$ifrefutable$1) => orig()})(dummyImplicit).map[Int]({(x$1) => orig(b + c)})
         |  withFilter => _empty_/W#withFilter().
         |  check$ifrefutable$1 => local0
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  map => _empty_/W#map().
         |  Int => scala/Int#
         |  x$1 => local1
         |""".stripMargin
    else
      """|[2:38..2:39): f => *.apply
         |  apply => scala/Function1#apply().
         |[5:2..5:45): for { (b, c) <- new W((1, 2)) } yield b + c => orig(new W((1, 2))).withFilter({(check$ifrefutable$1) => orig()})(dummyImplicit).map[Int]({(x$1) => orig(b + c)})
         |  withFilter => _empty_/W#withFilter().
         |  check$ifrefutable$1 => local0
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  map => _empty_/W#map().
         |  Int => scala/Int#
         |  x$1 => local1
         |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class F(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean): F = this
       |}
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def toG(f: F): G = new G(f.a)
       |}
       |class C {
       |  for { (b, c) <- new F((1, 2)) } yield b + c
       |}
       |""".stripMargin,
    """|[5:40..5:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[11:2..11:45): for { (b, c) <- new F((1, 2)) } yield b + c => orig(new F((1, 2))).map[Int]({(x$1) => orig(b + c)})
       |  map => _empty_/G#map().
       |  Int => scala/Int#
       |  x$1 => local0
       |[11:18..11:31): new F((1, 2)) => *.withFilter
       |  withFilter => _empty_/F#withFilter().
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class F(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean)(implicit d: DummyImplicit): F = this
       |}
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def toG(f: F): G = new G(f.a)
       |}
       |class C {
       |  for { (b, c) <- new F((1, 2)) } yield b + c
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[5:40..5:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[11:2..11:45): for { (b, c) <- new F((1, 2)) } yield b + c => orig(new F((1, 2))).map[Int]({(x$1) => orig(b + c)})
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[11:18..11:31): new F((1, 2)) => *.withFilter({(check$ifrefutable$1) => orig()})(dummyImplicit)
         |  withFilter => _empty_/F#withFilter().
         |  check$ifrefutable$1 => local1
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |""".stripMargin
    else
      """|[5:40..5:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[11:2..11:45): for { (b, c) <- new F((1, 2)) } yield b + c => orig(new F((1, 2))).map[Int]({(x$1) => orig(b + c)})
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[11:18..11:31): new F((1, 2)) => *.withFilter({(check$ifrefutable$1) => orig()})(dummyImplicit)
         |  withFilter => _empty_/F#withFilter().
         |  check$ifrefutable$1 => local1
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |""".stripMargin,
  )

  synthetics(
    """|class W[A](val a: A) {
       |  def withFilter[B](p: A => Boolean)(implicit d: DummyImplicit): W[A] = this
       |  def map[B](f: A => B): W[B] = new W(f(a))
       |}
       |class C {
       |  for { (b, c) <- new W((1, 2)) } yield b + c
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[2:38..2:39): f => *.apply
         |  apply => scala/Function1#apply().
         |[5:2..5:45): for { (b, c) <- new W((1, 2)) } yield b + c => orig(new W((1, 2))).withFilter[Nothing]({(check$ifrefutable$1) => orig()})(dummyImplicit).map[Int]({(x$1) => orig(b + c)})
         |  withFilter => _empty_/W#withFilter().
         |  Nothing => scala/Nothing#
         |  check$ifrefutable$1 => local0
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  map => _empty_/W#map().
         |  Int => scala/Int#
         |  x$1 => local1
         |""".stripMargin
    else
      """|[2:38..2:39): f => *.apply
         |  apply => scala/Function1#apply().
         |[5:2..5:45): for { (b, c) <- new W((1, 2)) } yield b + c => orig(new W((1, 2))).withFilter[Nothing]({(check$ifrefutable$1) => orig()})(dummyImplicit).map[Int]({(x$1) => orig(b + c)})
         |  withFilter => _empty_/W#withFilter().
         |  Nothing => scala/Nothing#
         |  check$ifrefutable$1 => local0
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  map => _empty_/W#map().
         |  Int => scala/Int#
         |  x$1 => local1
         |""".stripMargin,
  )

  synthetics(
    """|class M {
       |  def withFilter(p: ((Int, Int)) => Boolean): M = this
       |  def foreach(f: ((Int, Int)) => Unit): Unit = ()
       |  def map(f: ((Int, Int)) => Int): Int = 0
       |}
       |class C {
       |  for { (a, b) <- new M } println(a + b)
       |  for { (a, b) <- new M } yield a + b
       |}
       |""".stripMargin,
    """|[6:18..6:23): new M => *.withFilter({(check$ifrefutable$1) => orig()}).foreach
       |  withFilter => _empty_/M#withFilter().
       |  check$ifrefutable$1 => local0
       |  foreach => _empty_/M#foreach().
       |[7:18..7:23): new M => *.withFilter({(check$ifrefutable$2) => orig()}).map
       |  withFilter => _empty_/M#withFilter().
       |  check$ifrefutable$2 => local3
       |  map => _empty_/M#map().
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { c <- "ab" } println(c)
       |  for { c <- "ab" } yield c
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:30): for { c <- "ab" } println(c) => orig("ab").foreach[Unit]({(c) => orig(println(c))})
         |  foreach => scala/collection/IndexedSeqOptimized#foreach().
         |  Unit => scala/Unit#
         |  c => local0
         |[2:2..2:27): for { c <- "ab" } yield c => orig("ab").map[Char, String]({(c) => orig(c)})(StringCanBuildFrom)
         |  map => scala/collection/TraversableLike#map().
         |  Char => scala/Char#
         |  String => scala/Predef.String#
         |  c => local1
         |  StringCanBuildFrom => scala/Predef.StringCanBuildFrom.
         |""".stripMargin
    else
      """|[1:2..1:30): for { c <- "ab" } println(c) => orig("ab").foreach[Unit]({(c) => orig(println(c))})
         |  foreach => scala/collection/StringOps#foreach().
         |  Unit => scala/Unit#
         |  c => local0
         |[2:13..2:17): "ab" => *.map
         |  map => scala/collection/StringOps#map(+1).
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { a <- Array(1) } yield a
       |  for { (a, b) <- Array((1, 2)) } yield a + b
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:31): for { a <- Array(1) } yield a => orig(Array(1)).map[Int, Array[Int]]({(a) => orig(a)})(canBuildFrom((`macro-expandee` : ClassTag[Int])))
         |  map => scala/collection/TraversableLike#map().
         |  Int => scala/Int#
         |  Array => scala/Array#
         |  a => local0
         |  canBuildFrom => scala/Array.canBuildFrom().
         |  ClassTag => scala/reflect/ClassTag#
         |[1:13..1:18): Array => *.apply
         |  apply => scala/Array.apply(+5).
         |[2:2..2:45): for { (a, b) <- Array((1, 2)) } yield a + b => orig(Array((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).map[Int, Array[Int]]({(x$1) => orig(a + b)})(canBuildFrom((`macro-expandee` : ClassTag[Int])))
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local1
         |  map => scala/collection/generic/FilterMonadic#map().
         |  Int => scala/Int#
         |  Array => scala/Array#
         |  x$1 => local2
         |  canBuildFrom => scala/Array.canBuildFrom().
         |  ClassTag => scala/reflect/ClassTag#
         |[2:18..2:23): Array => *.apply[Tuple2[Int, Int]]
         |  apply => scala/Array.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[2:18..2:31): Array((1, 2)) => *((`macro-expandee` : ClassTag[Tuple2[Int, Int]]))
         |  ClassTag => scala/reflect/ClassTag#
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:31): for { a <- Array(1) } yield a => orig(Array(1)).map[Int]({(a) => orig(a)})((`macro-expandee` : ClassTag[Int]))
         |  map => scala/collection/ArrayOps#map().
         |  Int => scala/Int#
         |  a => local0
         |  ClassTag => scala/reflect/ClassTag#
         |[1:13..1:18): Array => *.apply
         |  apply => scala/Array.apply(+5).
         |[2:2..2:45): for { (a, b) <- Array((1, 2)) } yield a + b => orig(Array((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(a + b)})((`macro-expandee` : ClassTag[Int]))
         |  withFilter => scala/collection/ArrayOps#withFilter().
         |  check$ifrefutable$1 => local1
         |  map => scala/collection/ArrayOps.WithFilter#map().
         |  Int => scala/Int#
         |  x$1 => local2
         |  ClassTag => scala/reflect/ClassTag#
         |[2:18..2:23): Array => *.apply[Tuple2[Int, Int]]
         |  apply => scala/Array.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[2:18..2:31): Array((1, 2)) => *((`macro-expandee` : ClassTag[Tuple2[Int, Int]]))
         |  ClassTag => scala/reflect/ClassTag#
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class K(val a: Int)
       |class H(val a: Int) {
       |  def foreach(f: Int => Unit): Unit = f(a)
       |}
       |object K {
       |  implicit def toH(k: K)(implicit d: DummyImplicit): H = new H(k.a)
       |}
       |class C {
       |  for { c <- new K(1) } println(c)
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[3:38..3:39): f => *.apply
         |  apply => scala/Function1#apply().
         |[9:13..9:21): new K(1) => *(dummyImplicit).foreach
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  foreach => _empty_/H#foreach().
         |""".stripMargin
    else
      """|[3:38..3:39): f => *.apply
         |  apply => scala/Function1#apply().
         |[9:13..9:21): new K(1) => *(dummyImplicit).foreach
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  foreach => _empty_/H#foreach().
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  def g(implicit d: DummyImplicit): Option[Int] = Some(1)
       |  for { a <- g } yield a
       |  for { a <- g if a > 0 } yield a
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:50..1:54): Some => *.apply[Int]
         |  apply => scala/Some.apply().
         |  Int => scala/Int#
         |[2:2..2:24): for { a <- g } yield a => orig(g).map[Int]({(a) => orig(a)})
         |  map => scala/Option#map().
         |  Int => scala/Int#
         |  a => local0
         |[2:13..2:14): g => *(dummyImplicit)
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |[3:2..3:33): for { a <- g if a > 0 } yield a => orig(g).withFilter({(a) => orig(a > 0)}).map[Int]({(a) => orig(a)})
         |  withFilter => scala/Option#withFilter().
         |  a => local1
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |[3:13..3:14): g => *(dummyImplicit)
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |""".stripMargin
    else
      """|[1:50..1:54): Some => *.apply[Int]
         |  apply => scala/Some.apply().
         |  Int => scala/Int#
         |[2:2..2:24): for { a <- g } yield a => orig(g).map[Int]({(a) => orig(a)})
         |  map => scala/Option#map().
         |  Int => scala/Int#
         |  a => local0
         |[2:13..2:14): g => *(dummyImplicit)
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |[3:2..3:33): for { a <- g if a > 0 } yield a => orig(g).withFilter({(a) => orig(a > 0)}).map[Int]({(a) => orig(a)})
         |  withFilter => scala/Option#withFilter().
         |  a => local1
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |[3:13..3:14): g => *(dummyImplicit)
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class K(val a: Int)
       |class H(val a: Int) {
       |  def map[B](f: Int => B): B = f(a)
       |}
       |object K {
       |  implicit def toH(k: K)(implicit d: DummyImplicit): H = new H(k.a)
       |}
       |class C {
       |  for { c <- new K(1) } yield c
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[3:31..3:32): f => *.apply
         |  apply => scala/Function1#apply().
         |[9:2..9:31): for { c <- new K(1) } yield c => orig(new K(1))(dummyImplicit).map[Int]({(c) => orig(c)})
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  map => _empty_/H#map().
         |  Int => scala/Int#
         |  c => local0
         |""".stripMargin
    else
      """|[3:31..3:32): f => *.apply
         |  apply => scala/Function1#apply().
         |[9:2..9:31): for { c <- new K(1) } yield c => orig(new K(1))(dummyImplicit).map[Int]({(c) => orig(c)})
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  map => _empty_/H#map().
         |  Int => scala/Int#
         |  c => local0
         |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class F(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean)(implicit d: DummyImplicit): F = this
       |}
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def toG(f: F)(implicit d: DummyImplicit): G = new G(f.a)
       |}
       |class C {
       |  for { (b, c) <- new F((1, 2)) } yield b + c
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[5:40..5:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[11:2..11:45): for { (b, c) <- new F((1, 2)) } yield b + c => orig(new F((1, 2)))(dummyImplicit).map[Int]({(x$1) => orig(b + c)})
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[11:18..11:31): new F((1, 2)) => *.withFilter({(check$ifrefutable$1) => orig()})(dummyImplicit)
         |  withFilter => _empty_/F#withFilter().
         |  check$ifrefutable$1 => local1
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |""".stripMargin
    else
      """|[5:40..5:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[11:2..11:45): for { (b, c) <- new F((1, 2)) } yield b + c => orig(new F((1, 2)))(dummyImplicit).map[Int]({(x$1) => orig(b + c)})
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[11:18..11:31): new F((1, 2)) => *.withFilter({(check$ifrefutable$1) => orig()})(dummyImplicit)
         |  withFilter => _empty_/F#withFilter().
         |  check$ifrefutable$1 => local1
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class W(val a: Int) {
       |  def map[B](f: Int => B): F[B] = new F(f(a))
       |}
       |class F[B](val b: B)
       |class G[B](val b: B) {
       |  def map[C](f: B => C): C = f(b)
       |}
       |object F {
       |  implicit def toG[B](f: F[B]): G[B] = new G(f.b)
       |}
       |class C {
       |  for { a <- new W(1); b = a } yield a + b
       |}
       |""".stripMargin,
    """|[2:40..2:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[6:29..6:30): f => *.apply
       |  apply => scala/Function1#apply().
       |[12:2..12:42): for { a <- new W(1); b = a } yield a + b => orig(a <- new W(1); b = a).map[Int]({(x$1) => orig(a + b)})
       |  map => _empty_/G#map().
       |  Int => scala/Int#
       |  x$1 => local0
       |[12:8..12:28): a <- new W(1); b = a => orig(new W(1)).map[Tuple2[Int, Int]]({(a) => orig(b = a)})
       |  map => _empty_/W#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  a => local1
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class W(val a: Int) {
       |  def map[B](f: Int => B): F[B] = new F(f(a))
       |}
       |class F[B](val b: B)
       |class G[B](val b: B) {
       |  def map[C](f: B => C): C = f(b)
       |}
       |object F {
       |  implicit def toG[B](f: F[B])(implicit d: DummyImplicit): G[B] = new G(f.b)
       |}
       |class C {
       |  for { a <- new W(1); b = a } yield a + b
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[2:40..2:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[6:29..6:30): f => *.apply
         |  apply => scala/Function1#apply().
         |[12:2..12:42): for { a <- new W(1); b = a } yield a + b => orig(a <- new W(1); b = a)(dummyImplicit).map[Int]({(x$1) => orig(a + b)})
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[12:8..12:28): a <- new W(1); b = a => orig(new W(1)).map[Tuple2[Int, Int]]({(a) => orig(b = a)})
         |  map => _empty_/W#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  a => local1
         |""".stripMargin
    else
      """|[2:40..2:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[6:29..6:30): f => *.apply
         |  apply => scala/Function1#apply().
         |[12:2..12:42): for { a <- new W(1); b = a } yield a + b => orig(a <- new W(1); b = a)(dummyImplicit).map[Int]({(x$1) => orig(a + b)})
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[12:8..12:28): a <- new W(1); b = a => orig(new W(1)).map[Tuple2[Int, Int]]({(a) => orig(b = a)})
         |  map => _empty_/W#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  a => local1
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  def e[A]: List[Int] = Nil
       |  e.foreach(println)
       |  for { a <- e } println(a)
       |}
       |""".stripMargin,
    """|[2:2..2:3): e => *[Nothing]
       |  Nothing => scala/Nothing#
       |[2:2..2:11): e.foreach => *[Unit]
       |  Unit => scala/Unit#
       |[3:2..3:27): for { a <- e } println(a) => orig(e).foreach[Unit]({(a) => orig(println(a))})
       |  foreach => scala/collection/immutable/List#foreach().
       |  Unit => scala/Unit#
       |  a => local0
       |[3:13..3:14): e => *[Nothing]
       |  Nothing => scala/Nothing#
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { c <- "ab" if c > 'a' } yield c
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:38): for { c <- "ab" if c > 'a' } yield c => orig("ab").withFilter({(c) => orig(c > 'a')}).map[Char, String]({(c) => orig(c)})(StringCanBuildFrom)
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  c => local0
         |  map => scala/collection/generic/FilterMonadic#map().
         |  Char => scala/Char#
         |  String => scala/Predef.String#
         |  StringCanBuildFrom => scala/Predef.StringCanBuildFrom.
         |""".stripMargin
    else
      """|[1:13..1:28): "ab" if c > 'a' => orig("ab").withFilter({(c) => orig(c > 'a')}).map
         |  withFilter => scala/collection/StringOps#withFilter().
         |  c => local0
         |  map => scala/collection/StringOps.WithFilter#map(+1).
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { a <- List(1).map(_ + 1) } yield a
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:41): for { a <- List(1).map(_ + 1) } yield a => orig(List(1).map(_ + 1))(List.canBuildFrom[Int]).map[Int, List[Int]]({(a) => orig(a)})(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |  map => scala/collection/immutable/List#map().
         |  List => scala/collection/immutable/List#
         |  a => local0
         |[1:13..1:17): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:13..1:24): List(1).map => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |""".stripMargin
    else
      """|[1:2..1:41): for { a <- List(1).map(_ + 1) } yield a => orig(List(1).map(_ + 1)).map[Int]({(a) => orig(a)})
         |  map => scala/collection/immutable/List#map().
         |  Int => scala/Int#
         |  a => local0
         |[1:13..1:17): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:13..1:24): List(1).map => *[Int]
         |  Int => scala/Int#
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  def g(implicit d: DummyImplicit): Option[(Int, Int)] = Some((1, 2))
       |  for { (a, b) <- g } yield a + b
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:57..1:61): Some => *.apply[Tuple2[Int, Int]]
         |  apply => scala/Some.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[2:2..2:33): for { (a, b) <- g } yield a + b => orig(g).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(a + b)})
         |  withFilter => scala/Option#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |  x$1 => local1
         |[2:18..2:19): g => *(dummyImplicit)
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |""".stripMargin
    else
      """|[1:57..1:61): Some => *.apply[Tuple2[Int, Int]]
         |  apply => scala/Some.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[2:2..2:33): for { (a, b) <- g } yield a + b => orig(g).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(a + b)})
         |  withFilter => scala/Option#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |  x$1 => local1
         |[2:18..2:19): g => *(dummyImplicit)
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  def g(implicit d: DummyImplicit): Int = 1
       |  def h(implicit d: DummyImplicit): Boolean = true
       |  def e[A]: List[Int] = Nil
       |  for { a <- Option(1) if h } yield g
       |  for { a <- Option(1); b = g } yield e
       |  for { a <- List(1) } yield List(a).map(_ + 1)
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[4:2..4:37): for { a <- Option(1) if h } yield g => orig(Option(1)).withFilter({(a) => orig(h)}).map[Int]({(a) => orig(g)})
         |  withFilter => scala/Option#withFilter().
         |  a => local0
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |[4:13..4:19): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[4:26..4:27): h => *(dummyImplicit)
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |[4:36..4:37): g => *(dummyImplicit)
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |[5:2..5:39): for { a <- Option(1); b = g } yield e => orig(Option(1)).map[Tuple2[Int, Int]]({(a) => orig(b = g)}).map[List[Int]]({(x$1) => orig(e)})
         |  map => scala/Option#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  a => local1
         |  List => scala/package.List#
         |  x$1 => local2
         |[5:13..5:19): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[5:28..5:29): g => *(dummyImplicit)
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |[5:38..5:39): e => *[Nothing]
         |  Nothing => scala/Nothing#
         |[6:2..6:47): for { a <- List(1) } yield List(a).map(_ + 1) => orig(List(1)).map[List[Int], List[List[Int]]]({(a) => orig(List(a).map(_ + 1))(List.canBuildFrom[Int])})(List.canBuildFrom[List[Int]])
         |  map => scala/collection/immutable/List#map().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |  a => local4
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[6:13..6:17): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[6:29..6:33): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[6:29..6:40): List(a).map => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |""".stripMargin
    else
      """|[4:2..4:37): for { a <- Option(1) if h } yield g => orig(Option(1)).withFilter({(a) => orig(h)}).map[Int]({(a) => orig(g)})
         |  withFilter => scala/Option#withFilter().
         |  a => local0
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |[4:13..4:19): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[4:26..4:27): h => *(dummyImplicit)
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |[4:36..4:37): g => *(dummyImplicit)
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |[5:2..5:39): for { a <- Option(1); b = g } yield e => orig(Option(1)).map[Tuple2[Int, Int]]({(a) => orig(b = g)}).map[List[Int]]({(x$1) => orig(e)})
         |  map => scala/Option#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  a => local1
         |  List => scala/package.List#
         |  x$1 => local2
         |[5:13..5:19): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[5:28..5:29): g => *(dummyImplicit)
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |[5:38..5:39): e => *[Nothing]
         |  Nothing => scala/Nothing#
         |[6:2..6:47): for { a <- List(1) } yield List(a).map(_ + 1) => orig(List(1)).map[List[Int]]({(a) => orig(List(a).map(_ + 1))})
         |  map => scala/collection/immutable/List#map().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |  a => local4
         |[6:13..6:17): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[6:29..6:33): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[6:29..6:40): List(a).map => *[Int]
         |  Int => scala/Int#
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  val f: Int => Option[Int] = Some(_)
       |  for { a <- f(1) } yield a
       |  for { (a, b) <- f(1).map(x => (x, x)) } yield a + b
       |}
       |""".stripMargin,
    """|[1:30..1:34): Some => *.apply[Int]
       |  apply => scala/Some.apply().
       |  Int => scala/Int#
       |[2:2..2:27): for { a <- f(1) } yield a => orig(f(1)).map[Int]({(a) => orig(a)})
       |  map => scala/Option#map().
       |  Int => scala/Int#
       |  a => local1
       |[2:13..2:14): f => *.apply
       |  apply => scala/Function1#apply().
       |[3:2..3:53): for { (a, b) <- f(1).map(x => (x, x)) } yield a + b => orig(f(1).map(x => (x, x))).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$2) => orig(a + b)})
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$1 => local2
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |  x$2 => local3
       |[3:18..3:19): f => *.apply
       |  apply => scala/Function1#apply().
       |[3:18..3:26): f(1).map => *[Tuple2[Int, Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class V(val a: Int) {
       |  def map(f: Int => (Int, Int)): F = new F(f(a))
       |}
       |class F(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean): F = this
       |}
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def toG(f: F)(implicit d: DummyImplicit): G = new G(f.a)
       |}
       |class C {
       |  for { (b, c) <- for (x <- new V(1)) yield (x, x) } yield b + c
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[2:43..2:44): f => *.apply
         |  apply => scala/Function1#apply().
         |[8:40..8:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[14:2..14:64): for { (b, c) <- for (x <- new V(1)) yield (x, x) } yield b + c => orig(for (x <- new V(1)) yield (x, x))(dummyImplicit).map[Int]({(x$1) => orig(b + c)})
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[14:18..14:50): for (x <- new V(1)) yield (x, x) => orig(new V(1)).map({(x) => orig((x, x))}).withFilter
         |  map => _empty_/V#map().
         |  x => local1
         |  withFilter => _empty_/F#withFilter().
         |""".stripMargin
    else
      """|[2:43..2:44): f => *.apply
         |  apply => scala/Function1#apply().
         |[8:40..8:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[14:2..14:64): for { (b, c) <- for (x <- new V(1)) yield (x, x) } yield b + c => orig(for (x <- new V(1)) yield (x, x))(dummyImplicit).map[Int]({(x$1) => orig(b + c)})
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[14:18..14:50): for (x <- new V(1)) yield (x, x) => orig(new V(1)).map({(x) => orig((x, x))}).withFilter
         |  map => _empty_/V#map().
         |  x => local1
         |  withFilter => _empty_/F#withFilter().
         |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class W(val a: Int) {
       |  def map(f: Int => (Int, Int))(implicit d: DummyImplicit): F = new F(f(a))
       |}
       |class F(val a: (Int, Int))
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def toG(f: F): G = new G(f.a)
       |}
       |class C {
       |  for { a <- new W(1); b = a } yield a + b
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[2:70..2:71): f => *.apply
         |  apply => scala/Function1#apply().
         |[6:40..6:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[12:2..12:42): for { a <- new W(1); b = a } yield a + b => orig(a <- new W(1); b = a).map[Int]({(x$1) => orig(a + b)})
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[12:8..12:28): a <- new W(1); b = a => orig(new W(1)).map({(a) => orig(b = a)})(dummyImplicit)
         |  map => _empty_/W#map().
         |  a => local1
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |""".stripMargin
    else
      """|[2:70..2:71): f => *.apply
         |  apply => scala/Function1#apply().
         |[6:40..6:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[12:2..12:42): for { a <- new W(1); b = a } yield a + b => orig(a <- new W(1); b = a).map[Int]({(x$1) => orig(a + b)})
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[12:8..12:28): a <- new W(1); b = a => orig(new W(1)).map({(a) => orig(b = a)})(dummyImplicit)
         |  map => _empty_/W#map().
         |  a => local1
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class V(val a: Int) {
       |  def map(f: Int => (Int, Int))(implicit d: DummyImplicit): F = new F(f(a))
       |}
       |class F(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean): F = this
       |}
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def toG(f: F)(implicit d: DummyImplicit): G = new G(f.a)
       |}
       |class C {
       |  for { (b, c) <- for (x <- new V(1)) yield (x, x) } yield b + c
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[2:70..2:71): f => *.apply
         |  apply => scala/Function1#apply().
         |[8:40..8:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[14:2..14:64): for { (b, c) <- for (x <- new V(1)) yield (x, x) } yield b + c => orig(for (x <- new V(1)) yield (x, x))(dummyImplicit).map[Int]({(x$1) => orig(b + c)})
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[14:18..14:50): for (x <- new V(1)) yield (x, x) => orig(new V(1)).map({(x) => orig((x, x))})(dummyImplicit).withFilter
         |  map => _empty_/V#map().
         |  x => local1
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  withFilter => _empty_/F#withFilter().
         |""".stripMargin
    else
      """|[2:70..2:71): f => *.apply
         |  apply => scala/Function1#apply().
         |[8:40..8:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[14:2..14:64): for { (b, c) <- for (x <- new V(1)) yield (x, x) } yield b + c => orig(for (x <- new V(1)) yield (x, x))(dummyImplicit).map[Int]({(x$1) => orig(b + c)})
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  map => _empty_/G#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[14:18..14:50): for (x <- new V(1)) yield (x, x) => orig(new V(1)).map({(x) => orig((x, x))})(dummyImplicit).withFilter
         |  map => _empty_/V#map().
         |  x => local1
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  withFilter => _empty_/F#withFilter().
         |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  val k: (Int => Int) => Int = f => f(1)
       |  for { a <- Option(1) } yield k(x => x + a)
       |  for { f <- Some((x: Int) => x) } yield f(1)
       |}
       |""".stripMargin,
    """|[1:36..1:37): f => *.apply
       |  apply => scala/Function1#apply().
       |[2:2..2:44): for { a <- Option(1) } yield k(x => x + a) => orig(Option(1)).map[Int]({(a) => orig(k).apply({(x) => orig(x + a)})})
       |  map => scala/Option#map().
       |  Int => scala/Int#
       |  a => local1
       |  apply => scala/Function1#apply().
       |  x => local2
       |[2:13..2:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[3:2..3:45): for { f <- Some((x: Int) => x) } yield f(1) => orig(Some).apply[Function1[Int, Int]]({(x) => orig(x)}).map[Int]({(f) => orig(f(1))})
       |  apply => scala/Some.apply().
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  x => local3
       |  map => scala/Option#map().
       |  f => local4
       |[3:41..3:42): f => *.apply
       |  apply => scala/Function1#apply().
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  val a = Array((x: Int) => x)
       |}
       |""".stripMargin,
    """|[1:10..1:30): Array((x: Int) => x) => orig(Array).apply[Function1[Int, Int]]({(x) => orig(x)})((`macro-expandee` : ClassTag[Function1[Int, Int]]))
       |  apply => scala/Array.apply().
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  x => local0
       |  ClassTag => scala/reflect/ClassTag#
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class V(val a: Int) {
       |  def map(f: Int => (Int, Int)): F = new F(f(a))
       |}
       |class F(val a: (Int, Int))
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def toG(f: F): G = new G(f.a)
       |}
       |class C {
       |  for { p <- for (x <- new V(1)) yield (x, x) } yield p
       |}
       |""".stripMargin,
    """|[2:43..2:44): f => *.apply
       |  apply => scala/Function1#apply().
       |[6:40..6:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[12:2..12:55): for { p <- for (x <- new V(1)) yield (x, x) } yield p => orig(for (x <- new V(1)) yield (x, x)).map[Tuple2[Int, Int]]({(p) => orig(p)})
       |  map => _empty_/G#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  p => local0
       |[12:23..12:31): new V(1) => *.map
       |  map => _empty_/V#map().
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class V(val a: Int) {
       |  def map(f: Int => (Int, Int)): F = new F(f(a))
       |}
       |class F(val a: (Int, Int))
       |class G(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean): G = this
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |  def foreach(f: ((Int, Int)) => Unit): Unit = f(a)
       |}
       |class O(val a: Int) {
       |  def flatMap(f: Int => G): G = f(a)
       |  def map(f: Int => G): G = f(a)
       |}
       |object F {
       |  implicit def toG(f: F): G = new G(f.a)
       |  implicit def toB(f: F): Boolean = true
       |}
       |class C {
       |  for { (b, c) <- for (x <- new V(1)) yield (x, x) } yield b + c
       |  for { p <- (for (x <- new V(1)) yield (x, x)) } yield p
       |  for { a <- new O(1); p <- for (x <- new V(a)) yield (x, x) } yield new G(p)
       |  for { a <- new O(1) } yield for (x <- new V(a)) yield (x, x)
       |  for { a <- Option(1) if (for (x <- new V(a)) yield (x, x)) } yield a
       |  for { a <- Option(1); g: G = for (x <- new V(a)) yield (x, x) } yield g.a
       |  for { a <- new O(1); x <- new V(a) } yield (x, x)
       |  for { p <- for (x <- new V(1)) yield (x, x) } println(p)
       |}
       |""".stripMargin,
    """|[2:43..2:44): f => *.apply
       |  apply => scala/Function1#apply().
       |[7:40..7:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[8:47..8:48): f => *.apply
       |  apply => scala/Function1#apply().
       |[11:32..11:33): f => *.apply
       |  apply => scala/Function1#apply().
       |[12:28..12:29): f => *.apply
       |  apply => scala/Function1#apply().
       |[19:2..19:64): for { (b, c) <- for (x <- new V(1)) yield (x, x) } yield b + c => orig(for (x <- new V(1)) yield (x, x)).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(b + c)})
       |  withFilter => _empty_/G#withFilter().
       |  check$ifrefutable$1 => local0
       |  map => _empty_/G#map().
       |  Int => scala/Int#
       |  x$1 => local1
       |[19:28..19:36): new V(1) => *.map
       |  map => _empty_/V#map().
       |[20:2..20:57): for { p <- (for (x <- new V(1)) yield (x, x)) } yield p => orig(for (x <- new V(1)) yield (x, x)).map[Tuple2[Int, Int]]({(p) => orig(p)})
       |  map => _empty_/G#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  p => local5
       |[20:24..20:32): new V(1) => *.map
       |  map => _empty_/V#map().
       |[21:13..21:21): new O(1) => *.flatMap
       |  flatMap => _empty_/O#flatMap().
       |[21:23..21:77): p <- for (x <- new V(a)) yield (x, x) } yield new G(p) => orig(for (x <- new V(a)) yield (x, x)).map[G]({(p) => orig(new G(p))})
       |  map => _empty_/G#map().
       |  G => _empty_/G#
       |  p => local8
       |[21:38..21:46): new V(a) => *.map
       |  map => _empty_/V#map().
       |[22:13..22:21): new O(1) => *.map
       |  map => _empty_/O#map().
       |[22:30..22:62): for (x <- new V(a)) yield (x, x) => toG(*)
       |  toG => _empty_/F.toG().
       |[22:40..22:48): new V(a) => *.map
       |  map => _empty_/V#map().
       |[23:2..23:70): for { a <- Option(1) if (for (x <- new V(a)) yield (x, x)) } yield a => orig(Option(1)).withFilter({(a) => orig(for (x <- new V(a)) yield (x, x))}).map[Int]({(a) => orig(a)})
       |  withFilter => scala/Option#withFilter().
       |  a => local12
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |[23:13..23:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[23:37..23:45): new V(a) => *.map
       |  map => _empty_/V#map().
       |[24:2..24:75): for { a <- Option(1); g: G = for (x <- new V(a)) yield (x, x) } yield g.a => orig(Option(1)).map[Tuple2[Int, G]]({(a) => orig(g: G = for (x <- new V(a)) yield (x, x))}).map[Tuple2[Int, Int]]({(x$2) => orig(g.a)})
       |  map => scala/Option#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  G => _empty_/G#
       |  a => local14
       |  x$2 => local15
       |[24:13..24:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[24:31..24:63): for (x <- new V(a)) yield (x, x) => toG(*)
       |  toG => _empty_/F.toG().
       |[24:41..24:49): new V(a) => *.map
       |  map => _empty_/V#map().
       |[25:13..25:21): new O(1) => *.flatMap
       |  flatMap => _empty_/O#flatMap().
       |[25:23..25:51): x <- new V(a) } yield (x, x) => toG(*)
       |  toG => _empty_/F.toG().
       |[25:28..25:36): new V(a) => *.map
       |  map => _empty_/V#map().
       |[26:13..26:45): for (x <- new V(1)) yield (x, x) => *.foreach
       |  foreach => _empty_/G#foreach().
       |[26:23..26:31): new V(1) => *.map
       |  map => _empty_/V#map().
       |""".stripMargin,
  )

  synthetics(
    """|class B(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean): B = this
       |  def map[T](f: ((Int, Int)) => T): Boolean = true
       |}
       |class C {
       |  for { p <- for (x <- Option(1)) yield (x, x) } yield p
       |  for { (a, b) <- for (x <- Option(1)) yield (x, x) } yield a + b
       |  for { p <- for ((x, 1) <- Option((1, 1))) yield x } yield p
       |  for { a <- Option(1); p <- for (x <- Option(a)) yield x } yield p
       |  for { a <- Option(1) } yield for (x <- Option(a)) yield x
       |  for { a <- Option(1) if (for (x <- new B((a, a))) yield x) } yield a
       |  for { a <- Option(1) if (for ((x, 1) <- new B((a, a))) yield x) } yield a
       |  for { a <- Option(1); b = for (x <- Option(a)) yield x } yield b
       |  for { a <- Option(1); b = for ((x, 1) <- Option((a, a))) yield x } yield b
       |}
       |""".stripMargin,
    """|[5:2..5:56): for { p <- for (x <- Option(1)) yield (x, x) } yield p => orig(Option(1)).map[Tuple2[Int, Int]]({(x) => orig((x, x))}).map[Tuple2[Int, Int]]({(p) => orig(p)})
       |  map => scala/Option#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  x => local0
       |  p => local1
       |[5:23..5:29): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[6:2..6:65): for { (a, b) <- for (x <- Option(1)) yield (x, x) } yield a + b => orig(Option(1)).map[Tuple2[Int, Int]]({(x) => orig((x, x))}).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(a + b)})
       |  map => scala/Option#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  x => local2
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$1 => local3
       |  map => scala/Option#WithFilter#map().
       |  x$1 => local4
       |[6:28..6:34): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[7:2..7:61): for { p <- for ((x, 1) <- Option((1, 1))) yield x } yield p => orig(Option((1, 1))).withFilter({(check$ifrefutable$2) => orig()}).map[Int]({(x$2) => orig(x)}).map[Int]({(p) => orig(p)})
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$2 => local7
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |  x$2 => local8
       |  map => scala/Option#map().
       |  p => local9
       |[7:28..7:34): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |[8:2..8:67): for { a <- Option(1); p <- for (x <- Option(a)) yield x } yield p => orig(Option(1)).flatMap[Int]({(a) => orig(Option(a)).map[Int]({(x) => orig(x)}).map[Int]({(p) => orig(p)})})
       |  flatMap => scala/Option#flatMap().
       |  Int => scala/Int#
       |  a => local11
       |  map => scala/Option#map().
       |  x => local12
       |  p => local13
       |[8:13..8:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[8:39..8:45): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[9:2..9:59): for { a <- Option(1) } yield for (x <- Option(a)) yield x => orig(Option(1)).map[Option[Int]]({(a) => orig(Option(a)).map[Int]({(x) => orig(x)})})
       |  map => scala/Option#map().
       |  Option => scala/Option#
       |  Int => scala/Int#
       |  a => local14
       |  x => local15
       |[9:13..9:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[9:41..9:47): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[10:2..10:70): for { a <- Option(1) if (for (x <- new B((a, a))) yield x) } yield a => orig(Option(1)).withFilter({(a) => orig(new B((a, a))).map[Tuple2[Int, Int]]({(x) => orig(x)})}).map[Int]({(a) => orig(a)})
       |  withFilter => scala/Option#withFilter().
       |  a => local16
       |  map => _empty_/B#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  x => local17
       |  map => scala/Option#WithFilter#map().
       |[10:13..10:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[11:2..11:75): for { a <- Option(1) if (for ((x, 1) <- new B((a, a))) yield x) } yield a => orig(Option(1)).withFilter({(a) => orig(new B((a, a))).withFilter({(check$ifrefutable$3) => orig()}).map[Int]({(x$3) => orig(x)})}).map[Int]({(a) => orig(a)})
       |  withFilter => scala/Option#withFilter().
       |  a => local18
       |  withFilter => _empty_/B#withFilter().
       |  check$ifrefutable$3 => local19
       |  map => _empty_/B#map().
       |  Int => scala/Int#
       |  x$3 => local20
       |  map => scala/Option#WithFilter#map().
       |[11:13..11:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[12:2..12:66): for { a <- Option(1); b = for (x <- Option(a)) yield x } yield b => orig(Option(1)).map[Tuple2[Int, Option[Int]]]({(a) => orig(b = for (x <- Option(a)) yield x)}).map[Option[Int]]({(x$4) => orig(b)})
       |  map => scala/Option#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Option => scala/Option#
       |  a => local22
       |  x$4 => local23
       |[12:13..12:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[12:28..12:56): for (x <- Option(a)) yield x => orig(Option(a)).map[Int]({(x) => orig(x)})
       |  map => scala/Option#map().
       |  Int => scala/Int#
       |  x => local24
       |[12:38..12:44): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[13:2..13:76): for { a <- Option(1); b = for ((x, 1) <- Option((a, a))) yield x } yield b => orig(Option(1)).map[Tuple2[Int, Option[Int]]]({(a) => orig(b = for ((x, 1) <- Option((a, a))) yield x)}).map[Option[Int]]({(x$6) => orig(b)})
       |  map => scala/Option#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Option => scala/Option#
       |  a => local26
       |  x$6 => local27
       |[13:13..13:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[13:28..13:66): for ((x, 1) <- Option((a, a))) yield x => orig(Option((a, a))).withFilter({(check$ifrefutable$4) => orig()}).map[Int]({(x$5) => orig(x)})
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$4 => local28
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |  x$5 => local29
       |[13:43..13:49): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  synthetics(
    """|class C {
       |  for { p <- for (x <- List(1)) yield (x, x) } yield p
       |  for { (a, b) <- for (x <- List(1)) yield (x, x) } yield a + b
       |  for { p <- for ((x, 1) <- List((1, 1))) yield x } yield p
       |  for { a <- List(1); p <- for (x <- List(a)) yield x } yield p
       |  for { a <- List(1) } yield for (x <- List(a)) yield x
       |  for { a <- List(1) if (for (x <- List(a)) yield x).isEmpty } yield a
       |  for { a <- List(1); b = for (x <- List(a)) yield x } yield b
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:54): for { p <- for (x <- List(1)) yield (x, x) } yield p => orig(List(1)).map[Tuple2[Int, Int], List[Tuple2[Int, Int]]]({(x) => orig((x, x))})(List.canBuildFrom[Tuple2[Int, Int]]).map[Tuple2[Int, Int], List[Tuple2[Int, Int]]]({(p) => orig(p)})(List.canBuildFrom[Tuple2[Int, Int]])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  p => local1
         |[1:23..1:27): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[2:2..2:63): for { (a, b) <- for (x <- List(1)) yield (x, x) } yield a + b => orig(List(1)).map[Tuple2[Int, Int], List[Tuple2[Int, Int]]]({(x) => orig((x, x))})(List.canBuildFrom[Tuple2[Int, Int]]).withFilter({(check$ifrefutable$1) => orig()}).map[Int, List[Int]]({(x$1) => orig(a + b)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x => local2
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local3
         |  map => scala/collection/generic/FilterMonadic#map().
         |  x$1 => local4
         |[2:28..2:32): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[3:2..3:59): for { p <- for ((x, 1) <- List((1, 1))) yield x } yield p => orig(List((1, 1))).withFilter({(check$ifrefutable$2) => orig()}).map[Int, List[Int]]({(x$2) => orig(x)})(List.canBuildFrom[Int]).map[Int, List[Int]]({(p) => orig(p)})(List.canBuildFrom[Int])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$2 => local7
         |  map => scala/collection/generic/FilterMonadic#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x$2 => local8
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  map => scala/collection/immutable/List#map().
         |  p => local9
         |[3:28..3:32): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/immutable/List.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[4:2..4:63): for { a <- List(1); p <- for (x <- List(a)) yield x } yield p => orig(List(1)).flatMap[Int, List[Int]]({(a) => orig(List(a)).map[Int, List[Int]]({(x) => orig(x)})(List.canBuildFrom[Int]).map[Int, List[Int]]({(p) => orig(p)})(List.canBuildFrom[Int])})(List.canBuildFrom[Int])
         |  flatMap => scala/collection/immutable/List#flatMap().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  a => local11
         |  map => scala/collection/immutable/List#map().
         |  x => local12
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  p => local13
         |[4:13..4:17): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[4:37..4:41): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[5:2..5:55): for { a <- List(1) } yield for (x <- List(a)) yield x => orig(List(1)).map[List[Int], List[List[Int]]]({(a) => orig(List(a)).map[Int, List[Int]]({(x) => orig(x)})(List.canBuildFrom[Int])})(List.canBuildFrom[List[Int]])
         |  map => scala/collection/immutable/List#map().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |  a => local14
         |  x => local15
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[5:13..5:17): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[5:39..5:43): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[6:2..6:70): for { a <- List(1) if (for (x <- List(a)) yield x).isEmpty } yield a => orig(List(1)).withFilter({(a) => orig((for (x <- List(a)) yield x).isEmpty)}).map[Int, List[Int]]({(a) => orig(a)})(List.canBuildFrom[Int])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  a => local16
         |  map => scala/collection/generic/FilterMonadic#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[6:13..6:17): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[6:25..6:51): for (x <- List(a)) yield x => orig(List(a)).map[Int, List[Int]]({(x) => orig(x)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x => local17
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[6:35..6:39): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[7:2..7:62): for { a <- List(1); b = for (x <- List(a)) yield x } yield b => orig(List(1)).map[Tuple2[Int, List[Int]], List[Tuple2[Int, List[Int]]]]({(a) => orig(b = for (x <- List(a)) yield x)})(List.canBuildFrom[Tuple2[Int, List[Int]]]).map[List[Int], List[List[Int]]]({(x$3) => orig(b)})(List.canBuildFrom[List[Int]])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  a => local18
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$3 => local19
         |[7:13..7:17): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[7:26..7:52): for (x <- List(a)) yield x => orig(List(a)).map[Int, List[Int]]({(x) => orig(x)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x => local20
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[7:36..7:40): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:54): for { p <- for (x <- List(1)) yield (x, x) } yield p => orig(List(1)).map[Tuple2[Int, Int]]({(x) => orig((x, x))}).map[Tuple2[Int, Int]]({(p) => orig(p)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  x => local0
         |  p => local1
         |[1:23..1:27): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[2:2..2:63): for { (a, b) <- for (x <- List(1)) yield (x, x) } yield a + b => orig(List(1)).map[Tuple2[Int, Int]]({(x) => orig((x, x))}).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(a + b)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  x => local2
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local3
         |  map => scala/collection/WithFilter#map().
         |  x$1 => local4
         |[2:28..2:32): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[3:2..3:59): for { p <- for ((x, 1) <- List((1, 1))) yield x } yield p => orig(List((1, 1))).withFilter({(check$ifrefutable$2) => orig()}).map[Int]({(x$2) => orig(x)}).map[Int]({(p) => orig(p)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$2 => local7
         |  map => scala/collection/WithFilter#map().
         |  Int => scala/Int#
         |  x$2 => local8
         |  map => scala/collection/immutable/List#map().
         |  p => local9
         |[3:28..3:32): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/IterableFactory#apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[4:2..4:63): for { a <- List(1); p <- for (x <- List(a)) yield x } yield p => orig(List(1)).flatMap[Int]({(a) => orig(List(a)).map[Int]({(x) => orig(x)}).map[Int]({(p) => orig(p)})})
         |  flatMap => scala/collection/immutable/List#flatMap().
         |  Int => scala/Int#
         |  a => local11
         |  map => scala/collection/immutable/List#map().
         |  x => local12
         |  p => local13
         |[4:13..4:17): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[4:37..4:41): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[5:2..5:55): for { a <- List(1) } yield for (x <- List(a)) yield x => orig(List(1)).map[List[Int]]({(a) => orig(List(a)).map[Int]({(x) => orig(x)})})
         |  map => scala/collection/immutable/List#map().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |  a => local14
         |  x => local15
         |[5:13..5:17): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[5:39..5:43): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[6:2..6:70): for { a <- List(1) if (for (x <- List(a)) yield x).isEmpty } yield a => orig(List(1)).withFilter({(a) => orig((for (x <- List(a)) yield x).isEmpty)}).map[Int]({(a) => orig(a)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  a => local16
         |  map => scala/collection/WithFilter#map().
         |  Int => scala/Int#
         |[6:13..6:17): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[6:25..6:51): for (x <- List(a)) yield x => orig(List(a)).map[Int]({(x) => orig(x)})
         |  map => scala/collection/immutable/List#map().
         |  Int => scala/Int#
         |  x => local17
         |[6:35..6:39): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[7:2..7:62): for { a <- List(1); b = for (x <- List(a)) yield x } yield b => orig(List(1)).map[Tuple2[Int, List[Int]]]({(a) => orig(b = for (x <- List(a)) yield x)}).map[List[Int]]({(x$3) => orig(b)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  a => local18
         |  x$3 => local19
         |[7:13..7:17): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[7:26..7:52): for (x <- List(a)) yield x => orig(List(a)).map[Int]({(x) => orig(x)})
         |  map => scala/collection/immutable/List#map().
         |  Int => scala/Int#
         |  x => local20
         |[7:36..7:40): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class D
       |object D {
       |  implicit val d: D = new D
       |}
       |class V(val a: Int) {
       |  def map(f: Int => (Int, Int)): F = new F(f(a))
       |}
       |class F(val a: (Int, Int))
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |class O(val a: Int) {
       |  def flatMap(f: Int => G)(implicit d: D): G = f(a)
       |  def map(f: Int => G)(implicit d: D): G = f(a)
       |}
       |object F {
       |  implicit def toG(f: F): G = new G(f.a)
       |}
       |class C {
       |  for { a <- new O(1); p <- for (x <- new V(a)) yield (x, x) } yield new G(p)
       |  for { a <- new O(1) } yield for (x <- new V(a)) yield (x, x)
       |  for { a <- new O(1); x <- new V(a) } yield (x, x)
       |}
       |""".stripMargin,
    """|[6:43..6:44): f => *.apply
       |  apply => scala/Function1#apply().
       |[10:40..10:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[13:47..13:48): f => *.apply
       |  apply => scala/Function1#apply().
       |[14:43..14:44): f => *.apply
       |  apply => scala/Function1#apply().
       |[20:2..20:77): for { a <- new O(1); p <- for (x <- new V(a)) yield (x, x) } yield new G(p) => orig(new O(1)).flatMap({(a) => orig(for (x <- new V(a)) yield (x, x)).map[G]({(p) => orig(new G(p))})})(d)
       |  flatMap => _empty_/O#flatMap().
       |  a => local0
       |  map => _empty_/G#map().
       |  G => _empty_/G#
       |  p => local1
       |  d => _empty_/D.d.
       |[20:38..20:46): new V(a) => *.map
       |  map => _empty_/V#map().
       |[21:2..21:62): for { a <- new O(1) } yield for (x <- new V(a)) yield (x, x) => orig(new O(1)).map({(a) => orig(for (x <- new V(a)) yield (x, x))})(d)
       |  map => _empty_/O#map().
       |  a => local3
       |  d => _empty_/D.d.
       |[21:40..21:48): new V(a) => *.map
       |  map => _empty_/V#map().
       |[22:2..22:51): for { a <- new O(1); x <- new V(a) } yield (x, x) => orig(new O(1)).flatMap({(a) => orig(x <- new V(a) } yield (x, x))})(d)
       |  flatMap => _empty_/O#flatMap().
       |  a => local5
       |  d => _empty_/D.d.
       |[22:28..22:36): new V(a) => *.map
       |  map => _empty_/V#map().
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class V(val a: Int) {
       |  def map(f: Int => (Int, Int)): F = new F(f(a))
       |}
       |class W(val a: Int) {
       |  def map(f: Int => (Int, Int))(implicit d: DummyImplicit): F = new F(f(a))
       |}
       |class F(val a: (Int, Int))
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def toG(f: F)(implicit d: DummyImplicit): G = new G(f.a)
       |}
       |class C {
       |  for { p <- for (x <- new V(1)) yield (x, x) } yield p
       |  for { p <- for (x <- new W(1)) yield (x, x) } yield p
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[2:43..2:44): f => *.apply
         |  apply => scala/Function1#apply().
         |[5:70..5:71): f => *.apply
         |  apply => scala/Function1#apply().
         |[9:40..9:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[15:2..15:55): for { p <- for (x <- new V(1)) yield (x, x) } yield p => orig(for (x <- new V(1)) yield (x, x))(dummyImplicit).map[Tuple2[Int, Int]]({(p) => orig(p)})
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  map => _empty_/G#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  p => local0
         |[15:23..15:31): new V(1) => *.map
         |  map => _empty_/V#map().
         |[16:2..16:55): for { p <- for (x <- new W(1)) yield (x, x) } yield p => orig(for (x <- new W(1)) yield (x, x))(dummyImplicit).map[Tuple2[Int, Int]]({(p) => orig(p)})
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |  map => _empty_/G#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  p => local2
         |[16:13..16:45): for (x <- new W(1)) yield (x, x) => orig(new W(1)).map({(x) => orig((x, x))})(dummyImplicit)
         |  map => _empty_/W#map().
         |  x => local3
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |""".stripMargin
    else
      """|[2:43..2:44): f => *.apply
         |  apply => scala/Function1#apply().
         |[5:70..5:71): f => *.apply
         |  apply => scala/Function1#apply().
         |[9:40..9:41): f => *.apply
         |  apply => scala/Function1#apply().
         |[15:2..15:55): for { p <- for (x <- new V(1)) yield (x, x) } yield p => orig(for (x <- new V(1)) yield (x, x))(dummyImplicit).map[Tuple2[Int, Int]]({(p) => orig(p)})
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  map => _empty_/G#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  p => local0
         |[15:23..15:31): new V(1) => *.map
         |  map => _empty_/V#map().
         |[16:2..16:55): for { p <- for (x <- new W(1)) yield (x, x) } yield p => orig(for (x <- new W(1)) yield (x, x))(dummyImplicit).map[Tuple2[Int, Int]]({(p) => orig(p)})
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |  map => _empty_/G#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  p => local2
         |[16:13..16:45): for (x <- new W(1)) yield (x, x) => orig(new W(1)).map({(x) => orig((x, x))})(dummyImplicit)
         |  map => _empty_/W#map().
         |  x => local3
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class V(val a: Int) {
       |  def map(f: Int => (Int, Int)): F = new F(f(a))
       |}
       |class F(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean): H = new H(a)
       |}
       |class H(val a: (Int, Int))
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object H {
       |  implicit def toG(h: H): G = new G(h.a)
       |}
       |class C {
       |  for { p <- for (x <- new V(1)) yield (x, x) if p._1 > 0 } yield p
       |}
       |""".stripMargin,
    """|[2:43..2:44): f => *.apply
       |  apply => scala/Function1#apply().
       |[9:40..9:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[15:2..15:67): for { p <- for (x <- new V(1)) yield (x, x) if p._1 > 0 } yield p => orig(for (x <- new V(1)) yield (x, x) if p._1 > 0).map[Tuple2[Int, Int]]({(p) => orig(p)})
       |  map => _empty_/G#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  p => local0
       |[15:13..15:45): for (x <- new V(1)) yield (x, x) => orig(new V(1)).map({(x) => orig((x, x))}).withFilter
       |  map => _empty_/V#map().
       |  x => local1
       |  withFilter => _empty_/F#withFilter().
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class V(val a: Int) {
       |  def map(f: Int => Int): X = new X(f(a))
       |}
       |class X(val a: Int) {
       |  def map(f: Int => (Int, Int)): F = new F(f(a))
       |}
       |class F(val a: (Int, Int))
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def toG(f: F): G = new G(f.a)
       |}
       |class C {
       |  for { p <- for (x <- new V(1)) yield x; q = p } yield p + q
       |}
       |""".stripMargin,
    """|[2:36..2:37): f => *.apply
       |  apply => scala/Function1#apply().
       |[5:43..5:44): f => *.apply
       |  apply => scala/Function1#apply().
       |[9:40..9:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[15:2..15:61): for { p <- for (x <- new V(1)) yield x; q = p } yield p + q => orig(p <- for (x <- new V(1)) yield x; q = p).map[Int]({(x$1) => orig(p + q)})
       |  map => _empty_/G#map().
       |  Int => scala/Int#
       |  x$1 => local0
       |[15:13..15:40): for (x <- new V(1)) yield x => orig(new V(1)).map({(x) => orig(x)}).map
       |  map => _empty_/V#map().
       |  x => local1
       |  map => _empty_/X#map().
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class E(val a: (Int, Int))
       |class F(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean): H = new H(a)
       |}
       |class H(val a: (Int, Int))
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object E {
       |  implicit def toF(e: E): F = new F(e.a)
       |}
       |object H {
       |  implicit def toG(h: H): G = new G(h.a)
       |}
       |class C {
       |  for { (b, c) <- new E((1, 2)) } yield b + c
       |}
       |""".stripMargin,
    """|[7:40..7:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[16:2..16:45): for { (b, c) <- new E((1, 2)) } yield b + c => orig(new E((1, 2))).map[Int]({(x$1) => orig(b + c)})
       |  map => _empty_/G#map().
       |  Int => scala/Int#
       |  x$1 => local0
       |[16:18..16:31): new E((1, 2)) => *.withFilter
       |  withFilter => _empty_/F#withFilter().
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class F(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean): F = this
       |}
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def toG(f: F): G = new G(f.a)
       |}
       |class C {
       |  for { (b, c) <- new F((1, 2)) if b > 0 } yield b + c
       |}
       |""".stripMargin,
    """|[5:40..5:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[11:2..11:54): for { (b, c) <- new F((1, 2)) if b > 0 } yield b + c => orig(new F((1, 2)) if b > 0).map[Int]({(x$2) => orig(b + c)})
       |  map => _empty_/G#map().
       |  Int => scala/Int#
       |  x$2 => local0
       |[11:18..11:31): new F((1, 2)) => *.withFilter({(check$ifrefutable$1) => orig()}).withFilter
       |  withFilter => _empty_/F#withFilter().
       |  check$ifrefutable$1 => local1
       |""".stripMargin,
  )

  synthetics(
    """|class M {
       |  def withFilter(p: ((Int, Int)) => Boolean): M = this
       |  def map(f: ((Int, Int)) => Int)(implicit d: DummyImplicit): Int = 0
       |}
       |class C {
       |  for { (a, b) <- new M } yield a + b
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[5:2..5:37): for { (a, b) <- new M } yield a + b => orig(new M).withFilter({(check$ifrefutable$1) => orig()}).map({(x$1) => orig(a + b)})(dummyImplicit)
         |  withFilter => _empty_/M#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => _empty_/M#map().
         |  x$1 => local1
         |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
         |""".stripMargin
    else
      """|[5:2..5:37): for { (a, b) <- new M } yield a + b => orig(new M).withFilter({(check$ifrefutable$1) => orig()}).map({(x$1) => orig(a + b)})(dummyImplicit)
         |  withFilter => _empty_/M#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => _empty_/M#map().
         |  x$1 => local1
         |  dummyImplicit => scala/DummyImplicit.dummyImplicit.
         |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class D
       |object D {
       |  implicit val d: D = new D
       |}
       |class F(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean): F = this
       |}
       |class G(val a: (Int, Int)) {
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |object F {
       |  implicit def map(f: F)(implicit d: D): G = new G(f.a)
       |}
       |class C {
       |  for { b <- new F((1, 2)) } yield b
       |  for { (b, c) <- new F((1, 2)) } yield b + c
       |}
       |""".stripMargin,
    """|[9:40..9:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[15:2..15:36): for { b <- new F((1, 2)) } yield b => orig(new F((1, 2)))(d).map[Tuple2[Int, Int]]({(b) => orig(b)})
       |  d => _empty_/D.d.
       |  map => _empty_/G#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  b => local0
       |[16:2..16:45): for { (b, c) <- new F((1, 2)) } yield b + c => orig(new F((1, 2)))(d).map[Int]({(x$1) => orig(b + c)})
       |  d => _empty_/D.d.
       |  map => _empty_/G#map().
       |  Int => scala/Int#
       |  x$1 => local1
       |[16:18..16:31): new F((1, 2)) => *.withFilter
       |  withFilter => _empty_/F#withFilter().
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class O(val a: Int) {
       |  def map(f: Int => G): G = f(a)
       |}
       |class F(val a: (Int, Int))
       |class G(val a: (Int, Int))
       |object F {
       |  implicit def toG(f: F): G = new G(f.a)
       |}
       |class C {
       |  for { a <- new O(1) } yield new F((a, a))
       |}
       |""".stripMargin,
    """|[2:28..2:29): f => *.apply
       |  apply => scala/Function1#apply().
       |[10:13..10:21): new O(1) => *.map
       |  map => _empty_/O#map().
       |[10:30..10:43): new F((a, a)) => toG(*)
       |  toG => _empty_/F.toG().
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class O(val a: Int) {
       |  def map[B](f: Int => B): B = f(a)
       |}
       |class F(val a: (Int, Int))
       |class G(val a: (Int, Int))
       |object F {
       |  implicit def toG(f: F): G = new G(f.a)
       |}
       |class C {
       |  val g: G = for { a <- new O(1) } yield new F((a, a))
       |}
       |""".stripMargin,
    """|[2:31..2:32): f => *.apply
       |  apply => scala/Function1#apply().
       |[10:13..10:54): for { a <- new O(1) } yield new F((a, a)) => orig(new O(1)).map[G]({(a) => orig(new F((a, a)))})
       |  map => _empty_/O#map().
       |  G => _empty_/G#
       |  a => local0
       |""".stripMargin,
  )

  synthetics(
    """|import scala.language.implicitConversions
       |class D
       |object D {
       |  implicit val d: D = new D
       |}
       |class F(val a: (Int, Int))
       |class Gw(val a: (Int, Int)) {
       |  def withFilter(p: ((Int, Int)) => Boolean): Gw = this
       |  def map[B](f: ((Int, Int)) => B): B = f(a)
       |}
       |class Gf(val a: (Int, Int)) {
       |  def flatMap[B](f: ((Int, Int)) => Option[B]): Option[B] = f(a)
       |}
       |class Ge(val a: (Int, Int)) {
       |  def foreach[U](f: ((Int, Int)) => U): Unit = f(a)
       |}
       |object F {
       |  implicit def withFilter(f: F)(implicit d: D): Gw = new Gw(f.a)
       |  implicit def flatMap(f: F)(implicit d: D): Gf = new Gf(f.a)
       |  implicit def foreach(f: F)(implicit d: D): Ge = new Ge(f.a)
       |}
       |class C {
       |  for { (b, c) <- new F((1, 2)) } yield b + c
       |  for { b <- new F((1, 2)); c <- Option(1) } yield c
       |  for { b <- new F((1, 2)) } println(b)
       |}
       |""".stripMargin,
    """|[8:40..8:41): f => *.apply
       |  apply => scala/Function1#apply().
       |[11:60..11:61): f => *.apply
       |  apply => scala/Function1#apply().
       |[14:47..14:48): f => *.apply
       |  apply => scala/Function1#apply().
       |[22:2..22:45): for { (b, c) <- new F((1, 2)) } yield b + c => orig(new F((1, 2)))(d).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(b + c)})
       |  d => _empty_/D.d.
       |  withFilter => _empty_/Gw#withFilter().
       |  check$ifrefutable$1 => local0
       |  map => _empty_/Gw#map().
       |  Int => scala/Int#
       |  x$1 => local1
       |[23:2..23:52): for { b <- new F((1, 2)); c <- Option(1) } yield c => orig(new F((1, 2)))(d).flatMap[Int]({(b) => orig(Option(1)).map[Int]({(c) => orig(c)})})
       |  d => _empty_/D.d.
       |  flatMap => _empty_/Gf#flatMap().
       |  Int => scala/Int#
       |  b => local4
       |  map => scala/Option#map().
       |  c => local5
       |[23:33..23:39): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[24:2..24:39): for { b <- new F((1, 2)) } println(b) => orig(new F((1, 2)))(d).foreach[Unit]({(b) => orig(println(b))})
       |  d => _empty_/D.d.
       |  foreach => _empty_/Ge#foreach().
       |  Unit => scala/Unit#
       |  b => local6
       |""".stripMargin,
  )

}
