package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac._

// https://github.com/scalameta/scalameta/issues/1780
class Issue1780Suite extends SemanticdbSuite {

  override def customizeConfig(config: SemanticdbConfig): SemanticdbConfig = config
    .copy(synthetics = BinaryMode.Off)

  occurrences(
    """|object a {
       |  implicit class RichInt(x: Int) {
       |    def soRich(other: Int) = x + other
       |  }
       |
       |  val op = 1
       |  op soRich 2
       |}
       |""".stripMargin,
    """|[0:7..0:8): a <= _empty_/a.
       |[1:17..1:24): RichInt <= _empty_/a.RichInt#
       |[1:24..1:24):  <= _empty_/a.RichInt#`<init>`().
       |[1:25..1:26): x <= _empty_/a.RichInt#`<init>`().(x)
       |[1:25..1:26): x <= _empty_/a.RichInt#x.
       |[1:28..1:31): Int => scala/Int#
       |[2:8..2:14): soRich <= _empty_/a.RichInt#soRich().
       |[2:15..2:20): other <= _empty_/a.RichInt#soRich().(other)
       |[2:22..2:25): Int => scala/Int#
       |[2:29..2:30): x => _empty_/a.RichInt#x.
       |[2:31..2:32): + => scala/Int#`+`(+4).
       |[2:33..2:38): other => _empty_/a.RichInt#soRich().(other)
       |[5:6..5:8): op <= _empty_/a.op.
       |[6:2..6:4): op => _empty_/a.op.
       |[6:5..6:11): soRich => _empty_/a.RichInt#soRich().
       |""".stripMargin,
  )

}
