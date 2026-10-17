package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1968
class Issue1968Suite extends SemanticdbSuite {

  occurrences(
    """|object a {
       |  implicit def addByNameNullaryApply(directive: Int): (=> Int) => Int = ???
       |
       |  def bar(fn: (=> Int) => Int): Int = 42
       |  val get = 42
       |  get {
       |    42
       |  }}
       |""".stripMargin,
    """|[0:7..0:8): a <= _empty_/a.
       |[1:15..1:36): addByNameNullaryApply <= _empty_/a.addByNameNullaryApply().
       |[1:37..1:46): directive <= _empty_/a.addByNameNullaryApply().(directive)
       |[1:48..1:51): Int => scala/Int#
       |[1:58..1:61): Int => scala/Int#
       |[1:66..1:69): Int => scala/Int#
       |[1:72..1:75): ??? => scala/Predef.`???`().
       |[3:6..3:9): bar <= _empty_/a.bar().
       |[3:10..3:12): fn <= _empty_/a.bar().(fn)
       |[3:18..3:21): Int => scala/Int#
       |[3:26..3:29): Int => scala/Int#
       |[3:32..3:35): Int => scala/Int#
       |[4:6..4:9): get <= _empty_/a.get.
       |[5:2..5:5): get => _empty_/a.get.
       |""".stripMargin,
  )

}
