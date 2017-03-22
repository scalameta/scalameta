package scala.meta.tests
package inputs

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.internal.inputs._

class FormatMessageSuite extends FunSuite {
  private def test(s: String)(expected: String): Unit = {
    val testName = (if (s != "") s.replace("\n", "\\n") else "empty string")
    super.test(testName) {
      val content = Input.String(s)
      val points = 0.to(content.chars.length).map(i => Point.Offset(content, i))
      val actual = points.map(p => s"${p.formatMessage("error", "foo")}").mkString(EOL)
      if (actual != expected) Console.err.println(actual)
      assert(actual === expected)
    }
  }

  test("")("""
    |<input>:1: error: foo
    |
    |^
  """.trim.stripMargin)

  test("\n")("""
    |<input>:1: error: foo
    |
    |^
    |<input>:2: error: foo
    |
    |^
  """.trim.stripMargin)

  test("foo")("""
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
  """.trim.stripMargin)

  test("foo\n")("""
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
  """.trim.stripMargin)

  test("foo\nbar")("""
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
  """.trim.stripMargin)
}