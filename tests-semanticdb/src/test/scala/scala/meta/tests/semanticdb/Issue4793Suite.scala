package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/4793
class Issue4793Suite extends SemanticdbSuite {

  targeted(
    """|object AnonymousWithoutPosition {
       |  val x = example.AnonymousWithoutPosition()
       |}
       |""".stripMargin,
    { doc =>
      val noRange = doc.occurrences.filter(_.range.forall(_.startLine < 0))
      assertEquals(noRange.map(_.symbol), Nil)
    },
  )

  targeted(
    """|object ExpansionSynthetics {
       |  val a = example.ExpansionSynthetics.typeApply()
       |  val b = example.ExpansionSynthetics.implicitArg()
       |  val c = example.ExpansionSynthetics.implicitView()
       |  val d = example.ExpansionSynthetics.untyped()
       |}
       |""".stripMargin,
    doc => assertEquals(doc.synthetics.map(_.range), Nil),
  )

  test("self type without position") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object SelfTypeWithoutPosition {
         |  val x = example.SelfTypeWithoutPosition()
         |}
         |""".stripMargin,
      "Occurrences",
    )
    assertNoDiff(
      load(),
      """|[0:7..0:30): SelfTypeWithoutPosition <= _empty_/SelfTypeWithoutPosition.
         |[1:6..1:7): x <= _empty_/SelfTypeWithoutPosition.x.
         |[1:10..1:17): example => example/
         |[1:18..1:41): SelfTypeWithoutPosition => example/SelfTypeWithoutPosition.
         |""".stripMargin,
    )
  }

}
