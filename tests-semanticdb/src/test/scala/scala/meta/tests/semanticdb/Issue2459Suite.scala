package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/2459
class Issue2459Suite extends SemanticdbSuite {

  occurrences(
    """|object a {
       |  class A
       |  trait B
       |  val x1 = new A {}
       |  val x2 = new A with B {}
       |  val x3 = new A with B
       |  val x4 = new { val y = 1 }
       |  val x5 = new A
       |  val x6 = new B {}
       |  val x7 = new A with B { def z = 1 }
       |  val x8 = new A()
       |  val x9 = new A() {}
       |}
       |""".stripMargin,
    """|[0:7..0:8): a <= _empty_/a.
       |[1:8..1:9): A <= _empty_/a.A#
       |[1:9..1:9):  <= _empty_/a.A#`<init>`().
       |[2:8..2:9): B <= _empty_/a.B#
       |[3:6..3:8): x1 <= _empty_/a.x1.
       |[3:15..3:15):  <= local0
       |[3:15..3:16): A => _empty_/a.A#
       |[3:16..3:16):  => _empty_/a.A#`<init>`().
       |[4:6..4:8): x2 <= _empty_/a.x2.
       |[4:15..4:15):  <= local1
       |[4:15..4:16): A => _empty_/a.A#
       |[4:16..4:16):  => _empty_/a.A#`<init>`().
       |[4:22..4:23): B => _empty_/a.B#
       |[5:6..5:8): x3 <= _empty_/a.x3.
       |[5:15..5:15):  <= local2
       |[5:15..5:16): A => _empty_/a.A#
       |[5:16..5:16):  => _empty_/a.A#`<init>`().
       |[5:22..5:23): B => _empty_/a.B#
       |[6:6..6:8): x4 <= _empty_/a.x4.
       |[6:15..6:15):  <= local3
       |[6:21..6:22): y <= local4
       |[7:6..7:8): x5 <= _empty_/a.x5.
       |[7:15..7:16): A => _empty_/a.A#
       |[7:16..7:16):  => _empty_/a.A#`<init>`().
       |[8:6..8:8): x6 <= _empty_/a.x6.
       |[8:15..8:15):  <= local5
       |[8:15..8:16): B => _empty_/a.B#
       |[8:16..8:16):  => java/lang/Object#`<init>`().
       |[9:6..9:8): x7 <= _empty_/a.x7.
       |[9:15..9:15):  <= local6
       |[9:15..9:16): A => _empty_/a.A#
       |[9:16..9:16):  => _empty_/a.A#`<init>`().
       |[9:22..9:23): B => _empty_/a.B#
       |[9:30..9:31): z <= local7
       |[10:6..10:8): x8 <= _empty_/a.x8.
       |[10:15..10:16): A => _empty_/a.A#
       |[10:16..10:16):  => _empty_/a.A#`<init>`().
       |[11:6..11:8): x9 <= _empty_/a.x9.
       |[11:15..11:15):  <= local8
       |[11:15..11:16): A => _empty_/a.A#
       |[11:16..11:16):  => _empty_/a.A#`<init>`().
       |""".stripMargin,
  )

}
