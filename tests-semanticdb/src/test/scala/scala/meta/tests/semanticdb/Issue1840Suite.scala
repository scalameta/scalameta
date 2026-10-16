package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1840
class Issue1840Suite extends SemanticdbSuite {

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

}
