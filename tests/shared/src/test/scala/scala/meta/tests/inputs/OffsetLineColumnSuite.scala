package scala.meta.tests
package inputs

import scala.meta._

import munit._

class OffsetLineColumnSuite extends FunSuite {
  private def test(s: String)(expected: String): Unit = {
    val testName = if (s != "") s.replace("\n", "\\n") else "empty string"
    super.test(testName) {
      val content = Input.String(s)
      val points = 0.to(content.chars.length).map(i => Position.Range(content, i, i))
      val actual = points.map(p => s"${p.start} ${p.startLine} ${p.startColumn}")
        .mkString("", "\n", "\n")
      assertEquals(actual, expected)
      points.foreach { p1 =>
        val p2 = Position.Range(content, p1.startLine, p1.startColumn, p1.endLine, p1.endColumn)
        assertEquals(p1, p2)
      }
    }
  }

  test("")(
    """|0 0 0
       |""".stripMargin
  )

  test("\n")(
    """|0 0 0
       |1 1 0
       |""".stripMargin
  )

  test("foo")(
    """|0 0 0
       |1 0 1
       |2 0 2
       |3 0 3
       |""".stripMargin
  )

  test("foo\n")(
    """|0 0 0
       |1 0 1
       |2 0 2
       |3 0 3
       |4 1 0
       |""".stripMargin
  )

  test("foo\nbar")(
    """|0 0 0
       |1 0 1
       |2 0 2
       |3 0 3
       |4 1 0
       |5 1 1
       |6 1 2
       |7 1 3
       |""".stripMargin
  )
}
