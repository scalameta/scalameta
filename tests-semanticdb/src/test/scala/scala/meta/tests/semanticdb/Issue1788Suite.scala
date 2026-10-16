package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1788
class Issue1788Suite extends SemanticdbSuite {

  occurrences(
    """|object Main {
       |  @deprecated(
       |    message = "a",
       |    since = "b"
       |  ) def b = 1
       |}
       |""".stripMargin,
    """|[0:7..0:11): Main <= _empty_/Main.
       |[1:3..1:13): deprecated => scala/deprecated#
       |[1:13..1:13):  => scala/deprecated#`<init>`().
       |[2:4..2:11): message => scala/deprecated#`<init>`().(message)
       |[3:4..3:9): since => scala/deprecated#`<init>`().(since)
       |[4:8..4:9): b <= _empty_/Main.b().
       |""".stripMargin,
  )

}
