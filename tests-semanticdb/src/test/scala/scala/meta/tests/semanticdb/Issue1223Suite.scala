package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1223
// Wrapping an infix application in parentheses must not shift the synthetic off
// the application. In scalameta 2.1.2 the implicit-argument synthetic was attached
// at the second operand instead of spanning the whole `a zip b` application.
//
// Snippets are wrapped in `object Wrapped { ... }` by PrintSuiteBase, so source
// lines start at line 2. Synthetic ranges are printed by Print.synthetic (Compact).
class Issue1223Suite extends PrintSuiteBase {

  // Cross-version: the `*[Int]` type-application tracks `:: Nil` (cols 3..9),
  // shifted by exactly the leading `(` relative to an un-parenthesized `2 :: Nil`.
  checkSynthetics(
    "(2 :: Nil)",
    """|[2:3..2:9): :: Nil => *[Int]
       |""".stripMargin,
  )

  private val zip = "class C(nums: Seq[Int], strs: Seq[String]) { (nums zip strs) }"

  if (ScalaVersion.is212)
    // 2.12: zip takes an implicit CanBuildFrom. The synthetic spans the full
    // parenthesized application `nums zip strs`, not just the second operand.
    checkSynthetics(
      zip,
      """|[2:46..2:59): nums zip strs => *(Seq.canBuildFrom[Tuple2[Int, String]])
         |[2:46..2:54): nums zip => *[Int, String, Seq[Tuple2[Int, String]]]
         |""".stripMargin,
    )
  else
    // 2.13: zip is a plain type-application (no CanBuildFrom). Same invariant:
    // the synthetic tracks the infix application, not the `(`.
    checkSynthetics(
      zip,
      """|[2:46..2:54): nums zip => *[String]
         |""".stripMargin,
    )

}
