package scala.meta.tests
package inputs

import org.scalameta.internal.ScalaCompat.EOL
import scala.meta._
import scala.meta.internal.inputs._

import munit._

class FormatMessageSuite extends TreeSuiteBase {
  private def test(s: String)(expected: String): Unit = {
    val testName = if (s != "") s.replace("\n", "\\n") else "empty string"
    super.test(testName) {
      val content = Input.String(s)
      val points = 0.to(content.chars.length).map(i => Position.Range(content, i, i))
      val actual = points.map(p => s"${p.formatMessage("error", "foo")}").mkString(EOL)
      assertNoDiff(actual, expected)
    }
  }

  test("")(
    """|
       |<input>:1: error: foo
       |
       |^
       |""".stripMargin.lf2nl
  )

  test("\n")(
    """|
       |<input>:1: error: foo
       |
       |^
       |<input>:2: error: foo
       |
       |^
       |""".stripMargin.lf2nl
  )

  test("foo")(
    """|
       |<input>:1: error: foo
       |foo
       |^
       |<input>:1: error: foo
       |foo
       | ^
       |<input>:1: error: foo
       |foo
       |  ^
       |<input>:1: error: foo
       |foo
       |   ^
       |""".stripMargin.lf2nl
  )

  test("foo\n")(
    """|
       |<input>:1: error: foo
       |foo
       |^
       |<input>:1: error: foo
       |foo
       | ^
       |<input>:1: error: foo
       |foo
       |  ^
       |<input>:1: error: foo
       |foo
       |   ^
       |<input>:2: error: foo
       |
       |^
       |""".stripMargin.lf2nl
  )

  test("foo\nbar")(
    """|
       |<input>:1: error: foo
       |foo
       |^
       |<input>:1: error: foo
       |foo
       | ^
       |<input>:1: error: foo
       |foo
       |  ^
       |<input>:1: error: foo
       |foo
       |   ^
       |<input>:2: error: foo
       |bar
       |^
       |<input>:2: error: foo
       |bar
       | ^
       |<input>:2: error: foo
       |bar
       |  ^
       |<input>:2: error: foo
       |bar
       |   ^
       |""".stripMargin.lf2nl
  )
}
