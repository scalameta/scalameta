package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.prettyprinters._

class BasicPositionSuite extends BasePositionSuite {
  check[Term](
    "1 + (2 / 3) * 4",
    """|Term.ApplyInfix (2 / 3) * 4
       |Term.ApplyInfix (2 / 3)
       |""".stripMargin
  )
  check[Term](
    "(1 + 2).foo",
    """|Term.ApplyInfix (1 + 2)
       |""".stripMargin
  )
  check[Stat](
    // Issue #333
    """def shortInfo: String = s"created=$x"""",
    """|Term.Interpolate s"created=$x"
       |""".stripMargin
  )
  check[Case](
    // Issue #331
    "case foo if bar || baz =>",
    """|Term.ApplyInfix bar || baz
       |Term.Block case foo if bar || baz =>→←
       |""".stripMargin
  )
  check[Stat](
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
