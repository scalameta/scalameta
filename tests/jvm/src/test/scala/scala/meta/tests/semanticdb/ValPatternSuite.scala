package scala.meta.tests.semanticdb

class ValPatternSuite extends SemanticdbSuite {

  occurrences(
    """
      |object A {
      | val x #:: xs = Stream(1, 2)
      |}
    """.stripMargin,
    """|[1:7..1:8): A <= _empty_/A.
       |[2:5..2:6): x <= _empty_/A.x.
       |[2:7..2:10): #:: => scala/package.`#::`.
       |[2:11..2:13): xs <= _empty_/A.xs.
       |[2:16..2:22): Stream => scala/package.Stream.
       |""".stripMargin
  )

  occurrences(
    """
      |object B {
      | val (left, right) = (1, 2)
      | println((left, right))
      |}
    """.stripMargin,
    """|[1:7..1:8): B <= _empty_/B.
       |[2:6..2:10): left <= _empty_/B.left.
       |[2:12..2:17): right <= _empty_/B.right.
       |[3:1..3:8): println => scala/Predef.println(+1).
       |[3:10..3:14): left => _empty_/B.left.
       |[3:16..3:21): right => _empty_/B.right.
       |""".stripMargin
  )

  occurrences(
    """
      |object D {
      |  val Some(number1) =
      |    Some(1)
      |  println(number1)
      |}
    """.stripMargin,
    """|[1:7..1:8): D <= _empty_/D.
       |[2:6..2:10): Some => scala/Some.
       |[2:11..2:18): number1 => _empty_/D.number1.
       |[3:4..3:8): Some => scala/Some.
       |[4:2..4:9): println => scala/Predef.println(+1).
       |[4:10..4:17): number1 => _empty_/D.number1.
       |""".stripMargin
  )

  occurrences(
    """
      |class E {
      |  var (leftVar, rightVar) = (1, 2)
      |  var Some(number1Var) =
      |    Some(1)
      |  println((leftVar, rightVar, number1Var))
      |}
    """.stripMargin,
    """|[1:6..1:7): E <= _empty_/E#
       |[1:8..1:8):  <= _empty_/E#`<init>`().
       |[2:7..2:14): leftVar <= _empty_/E#leftVar().
       |[2:16..2:24): rightVar <= _empty_/E#rightVar().
       |[3:6..3:10): Some => scala/Some.
       |[3:11..3:21): number1Var => _empty_/E#number1Var.
       |[4:4..4:8): Some => scala/Some.
       |[5:2..5:9): println => scala/Predef.println(+1).
       |[5:11..5:18): leftVar => _empty_/E#leftVar().
       |[5:20..5:28): rightVar => _empty_/E#rightVar().
       |[5:30..5:40): number1Var => _empty_/E#number1Var().
       |""".stripMargin
  )

  occurrences(
    """
      |class F {
      |  locally {
      |    val (left, right) = (1, 2)
      |    val Some(number1) =
      |      Some(1)
      |    println((left, right, number1))
      |  }
      |}
    """.stripMargin,
    """|[1:6..1:7): F <= _empty_/F#
       |[1:8..1:8):  <= _empty_/F#`<init>`().
       |[2:2..2:9): locally => scala/Predef.locally().
       |[3:9..3:13): left <= local1
       |[3:15..3:20): right <= local2
       |[4:8..4:12): Some => scala/Some.
       |[4:13..4:20): number1 => local3
       |[5:6..5:10): Some => scala/Some.
       |[6:4..6:11): println => scala/Predef.println(+1).
       |[6:13..6:17): left => local1
       |[6:19..6:24): right => local2
       |[6:26..6:33): number1 => local3
       |""".stripMargin
  )

  occurrences(
    """
      |class G {
      |  locally {
      |    var (left, right) = (1, 2)
      |    var Some(number1) =
      |      Some(1)
      |    println((left, right, number1))
      |  }
      |}
    """.stripMargin,
    """|[1:6..1:7): G <= _empty_/G#
       |[1:8..1:8):  <= _empty_/G#`<init>`().
       |[2:2..2:9): locally => scala/Predef.locally().
       |[3:9..3:13): left <= local1
       |[3:15..3:20): right <= local2
       |[4:8..4:12): Some => scala/Some.
       |[4:13..4:20): number1 => local3
       |[5:6..5:10): Some => scala/Some.
       |[6:4..6:11): println => scala/Predef.println(+1).
       |[6:13..6:17): left => local1
       |[6:19..6:24): right => local2
       |[6:26..6:33): number1 => local3
       |""".stripMargin
  )

}
