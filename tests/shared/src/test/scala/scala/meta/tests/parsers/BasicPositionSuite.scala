package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.prettyprinters._

class BasicPositionSuite extends BasePositionSuite(dialects.Scala213) {
  checkPositions[Term](
    "1 + (2 / 3) * 4",
    """|Term.ApplyInfix (2 / 3) * 4
       |Term.ApplyInfix 2 / 3
       |""".stripMargin
  )

  checkPositions[Term](
    "1 + (()) * 4",
    """|Term.ApplyInfix (()) * 4
       |""".stripMargin
  )

  checkPositions[Term](
    "1 + ((1, 2, 3)) * 4",
    """|Term.ApplyInfix ((1, 2, 3)) * 4
       |Term.Tuple (1, 2, 3)
       |""".stripMargin
  )

  checkPositions[Term](
    "a f (b)"
  )

  checkPositions[Term](
    "a f (123)"
  )

  checkPositions[Term](
    "a f ()"
  )

  checkPositions[Term](
    "a f (())"
  )

  checkPositions[Term](
    "a f ((1, 2, 3))",
    """|Term.Tuple (1, 2, 3)
       |""".stripMargin
  )

  checkPositions[Term](
    "a f (()).foo",
    """|Term.Select (()).foo
       |""".stripMargin
  )

  checkPositions[Term](
    "a f (())(b)",
    """|Term.Apply (())(b)
       |""".stripMargin
  )

  checkPositions[Term](
    "1 + { 2 / 3 } * 4",
    """|Term.ApplyInfix { 2 / 3 } * 4
       |Term.Block { 2 / 3 }
       |Term.ApplyInfix 2 / 3
       |""".stripMargin
  )

  checkPositions[Term](
    "{ 2 / 3 } + 4",
    """|Term.Block { 2 / 3 }
       |Term.ApplyInfix 2 / 3
       |""".stripMargin
  )

  checkPositions[Term](
    "(1 + 2).foo",
    """|Term.ApplyInfix (1 + 2)
       |""".stripMargin
  )
  checkPositions[Term](
    "foo == (a + b).c(d)",
    """|Term.Apply (a + b).c(d)
       |Term.Select a + b).c
       |Term.ApplyInfix a + b
       |""".stripMargin
  )
  checkPositions[Stat](
    // Issue #333
    """def shortInfo: String = s"created=$x"""",
    """|Term.Interpolate s"created=$x"
       |""".stripMargin
  )
  checkPositions[Case](
    // Issue #331
    "case foo if bar || baz =>",
    """|Term.ApplyInfix bar || baz
       |Term.Block case foo if bar || baz =>@@
       |""".stripMargin
  )
  checkPositions[Stat](
    """a + b + c""",
    """|Term.ApplyInfix a + b
       |""".stripMargin
  )

  test("apply(startLine,startColumn,endLine,endColumn)") {
    val input = Input.String(
      """|  val x = 2 // line 0
         |
         |            // line 2""".stripMargin
    )
    val x = Position.Range(input, 0, 2, 0, 11)
    assert(x.text == "val x = 2")
    val x2 = Position.Range(input, 0, 2, 0, Int.MaxValue)
    assert(x2.text == "val x = 2 // line 0")
    val empty = Position.Range(input, 1, 0, 1, Int.MaxValue)
    assert(empty.text == "")
    val last = Position.Range(input, 2, 0, 2, Int.MaxValue)
    assert(last.text == "            // line 2")
  }

}
