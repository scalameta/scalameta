package scala.meta.tests
package inputs

import scala.meta._

import munit._

class OffsetLineColumnSuite extends FunSuite {

  private def test(s: String)(expected: String)(implicit loc: munit.Location): Unit = {
    val testName = if (s != "") s.replace("\n", "\\n").replace("\r", "\\r") else "empty string"
    super.test(testName) {
      val content = Input.String(s)
      val len = content.chars.length
      val points = (0 until len).flatMap(i =>
        Seq(Position.Range.exclusive(content, i, i), Position.Range.inclusive(content, i, i)),
      ) :+ Position.Range.exclusive(content, len, len)
      val sb = new StringBuilder
      points.foreach { p =>
        sb.append('[').append(p.start).append(", ").append(p.end).append(") -> [")
        sb.append(p.startLine).append(':').append(p.startColumn).append(", ")
        sb.append(p.lastLine).append(':').append(p.lastColumn).append("] end=(")
        sb.append(p.endLine).append(':').append(p.endColumn).append(")\n")
      }
      assertEquals(sb.result(), expected)
      points.foreach { p1 =>
        val p2 = Position.Range(content, p1.startLine, p1.startColumn, p1.endLine, p1.endColumn)
        assertEquals(p1, p2)
      }
    }
  }

  test("")(
    """|[0, 0) -> [0:0, -1:0] end=(0:0)
       |""".stripMargin,
  )

  test("\n")(
    """|[0, 0) -> [0:0, -1:0] end=(0:0)
       |[0, 1) -> [0:0, 0:0] end=(1:0)
       |[1, 1) -> [1:0, 0:0] end=(1:0)
       |""".stripMargin,
  )

  test("foo")(
    """|[0, 0) -> [0:0, -1:0] end=(0:0)
       |[0, 1) -> [0:0, 0:0] end=(0:1)
       |[1, 1) -> [0:1, 0:0] end=(0:1)
       |[1, 2) -> [0:1, 0:1] end=(0:2)
       |[2, 2) -> [0:2, 0:1] end=(0:2)
       |[2, 3) -> [0:2, 0:2] end=(0:3)
       |[3, 3) -> [0:3, 0:2] end=(0:3)
       |""".stripMargin,
  )

  test("foo\n") {
    """|[0, 0) -> [0:0, -1:0] end=(0:0)
       |[0, 1) -> [0:0, 0:0] end=(0:1)
       |[1, 1) -> [0:1, 0:0] end=(0:1)
       |[1, 2) -> [0:1, 0:1] end=(0:2)
       |[2, 2) -> [0:2, 0:1] end=(0:2)
       |[2, 3) -> [0:2, 0:2] end=(0:3)
       |[3, 3) -> [0:3, 0:2] end=(0:3)
       |[3, 4) -> [0:3, 0:3] end=(1:0)
       |[4, 4) -> [1:0, 0:3] end=(1:0)
       |""".stripMargin
  }

  test("foo\nbar") {
    """|[0, 0) -> [0:0, -1:0] end=(0:0)
       |[0, 1) -> [0:0, 0:0] end=(0:1)
       |[1, 1) -> [0:1, 0:0] end=(0:1)
       |[1, 2) -> [0:1, 0:1] end=(0:2)
       |[2, 2) -> [0:2, 0:1] end=(0:2)
       |[2, 3) -> [0:2, 0:2] end=(0:3)
       |[3, 3) -> [0:3, 0:2] end=(0:3)
       |[3, 4) -> [0:3, 0:3] end=(1:0)
       |[4, 4) -> [1:0, 0:3] end=(1:0)
       |[4, 5) -> [1:0, 1:0] end=(1:1)
       |[5, 5) -> [1:1, 1:0] end=(1:1)
       |[5, 6) -> [1:1, 1:1] end=(1:2)
       |[6, 6) -> [1:2, 1:1] end=(1:2)
       |[6, 7) -> [1:2, 1:2] end=(1:3)
       |[7, 7) -> [1:3, 1:2] end=(1:3)
       |""".stripMargin
  }

  test("foo\r") {
    """|[0, 0) -> [0:0, -1:0] end=(0:0)
       |[0, 1) -> [0:0, 0:0] end=(0:1)
       |[1, 1) -> [0:1, 0:0] end=(0:1)
       |[1, 2) -> [0:1, 0:1] end=(0:2)
       |[2, 2) -> [0:2, 0:1] end=(0:2)
       |[2, 3) -> [0:2, 0:2] end=(0:3)
       |[3, 3) -> [0:3, 0:2] end=(0:3)
       |[3, 4) -> [0:3, 0:3] end=(1:0)
       |[4, 4) -> [1:0, 0:3] end=(1:0)
       |""".stripMargin
  }

  test("foo\rbar") {
    """|[0, 0) -> [0:0, -1:0] end=(0:0)
       |[0, 1) -> [0:0, 0:0] end=(0:1)
       |[1, 1) -> [0:1, 0:0] end=(0:1)
       |[1, 2) -> [0:1, 0:1] end=(0:2)
       |[2, 2) -> [0:2, 0:1] end=(0:2)
       |[2, 3) -> [0:2, 0:2] end=(0:3)
       |[3, 3) -> [0:3, 0:2] end=(0:3)
       |[3, 4) -> [0:3, 0:3] end=(1:0)
       |[4, 4) -> [1:0, 0:3] end=(1:0)
       |[4, 5) -> [1:0, 1:0] end=(1:1)
       |[5, 5) -> [1:1, 1:0] end=(1:1)
       |[5, 6) -> [1:1, 1:1] end=(1:2)
       |[6, 6) -> [1:2, 1:1] end=(1:2)
       |[6, 7) -> [1:2, 1:2] end=(1:3)
       |[7, 7) -> [1:3, 1:2] end=(1:3)
       |""".stripMargin
  }

  super.test("hasNewlines") {
    val input = Input.String("foo\nbar")
    val ranges = List(0 -> 3, 0 -> 4, 4 -> 4, 2 -> 5, 0 -> 7)
    val obtained = ranges.map { case (start, end) => Position.Range(input, start, end).hasNewlines }
    assertEquals(obtained, List(false, true, false, true, true))
  }

  super.test("hasNewlinesBeforeEnd") {
    val input = Input.String("foo\nbar")
    val ranges = List(0 -> 3, 0 -> 4, 4 -> 4, 2 -> 5, 0 -> 7)
    val obtained = ranges.map { case (start, end) =>
      Position.Range(input, start, end).hasNewlinesBeforeEnd
    }
    assertEquals(obtained, List(false, false, false, true, true))
  }

}
