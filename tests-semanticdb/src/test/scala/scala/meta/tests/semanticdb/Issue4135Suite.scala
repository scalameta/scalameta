package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/4135
class Issue4135Suite extends SemanticdbSuite {

  // for comprehensions (#4135)
  synthetics(
    """|package x
       |class C {
       |  for { a <- Option(1); b <- Option(2); c = a + b } yield (a, b, c)
       |  for { (a, b) <- Option((1, 2)) } yield a + b
       |  for { a <- Option(1); (b, c) <- Option((a, a)) } yield b + c
       |}
       |""".stripMargin,
    """|[2:2..2:67): for { a <- Option(1); b <- Option(2); c = a + b } yield (a, b, c) => orig(Option(1)).flatMap[Tuple3[Int, Int, Int]]({(a) => orig(Option(2)).map[Tuple2[Int, Int]]({(b) => orig(c = a + b)}).map[Tuple3[Int, Int, Int]]({(local2) => orig((a, b, c))})})
       |  flatMap => scala/Option#flatMap().
       |  Tuple3 => scala/Tuple3#
       |  Int => scala/Int#
       |  a => local0
       |  map => scala/Option#map().
       |  Tuple2 => scala/Tuple2#
       |  b => local1
       |  local2 => local2
       |[2:13..2:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[2:29..2:35): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[3:2..3:46): for { (a, b) <- Option((1, 2)) } yield a + b => orig(Option((1, 2))).withFilter({(local4) => orig()}).map[Int]({(local5) => orig((a, b) <- Option((1, 2)) } yield a + b)})
       |  withFilter => scala/Option#withFilter().
       |  local4 => local4
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |  local5 => local5
       |[3:18..3:24): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |[4:2..4:62): for { a <- Option(1); (b, c) <- Option((a, a)) } yield b + c => orig(Option(1)).flatMap[Int]({(a) => orig(Option((a, a))).withFilter({(local9) => orig()}).map[Int]({(local10) => orig((b, c) <- Option((a, a)) } yield b + c)})})
       |  flatMap => scala/Option#flatMap().
       |  Int => scala/Int#
       |  a => local8
       |  withFilter => scala/Option#withFilter().
       |  local9 => local9
       |  map => scala/Option#WithFilter#map().
       |  local10 => local10
       |[4:13..4:19): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[4:34..4:40): Option => *.apply[Tuple2[Int, Int]]
       |  apply => scala/Option.apply().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin.replace(
      "orig((a, b, c))",
      if (ScalaVersion.is213) "orig(b <- Option(2); c = a + b } yield (a, b, c))"
      else "orig((a, b, c))",
    ),
  )

}
