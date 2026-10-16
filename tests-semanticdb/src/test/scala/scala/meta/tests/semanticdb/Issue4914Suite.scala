package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/pull/4914
class Issue4914Suite extends SemanticdbSuite {

  private val code =
    """|object IdenticalSnippet {
       |  def f = { val x = 1; x }
       |}
       |""".stripMargin

  checkSection(
    "identical snippet, first compile",
    code,
    """|[0:7..0:23): IdenticalSnippet <= _empty_/IdenticalSnippet.
       |[1:6..1:7): f <= _empty_/IdenticalSnippet.f().
       |[1:16..1:17): x <= local0
       |[1:23..1:24): x => local0
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "identical snippet, second compile",
    code,
    """|[0:7..0:23): IdenticalSnippet <= _empty_/IdenticalSnippet.
       |[1:6..1:7): f <= _empty_/IdenticalSnippet.f().
       |[1:16..1:17): x <= local0
       |[1:23..1:24): x => local0
       |""".stripMargin,
    "Occurrences",
  )

}
