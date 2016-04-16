package scala.meta.tests
package inputs

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._

class PositionSuite extends FunSuite {
  private def testOffsets(s: String)(expected: String): Unit = {
    val testName = "offsets for " + (if (s != "") s.replace("\n", "\\n") else "empty string")
    super.test(testName) {
      val content = Input.String(s)
      val points = 0.to(content.chars.length).map(i => Point.Offset(content, i))
      intercept[IllegalArgumentException](Point.Offset(content, -1))
      intercept[IllegalArgumentException](Point.Offset(content, content.chars.length + 1))
      val actual = points.map(p => s"${p.offset} ${p.line} ${p.column}").mkString(EOL)
      if (actual != expected) Console.err.println(actual)
      assert(actual === expected)
    }
  }

  testOffsets("")("""
    |0 0 0
  """.trim.stripMargin)

  testOffsets("\n")("""
    |0 0 0
    |1 1 0
  """.trim.stripMargin)

  testOffsets("foo")("""
    |0 0 0
    |1 0 1
    |2 0 2
    |3 0 3
  """.trim.stripMargin)

  testOffsets("foo\n")("""
    |0 0 0
    |1 0 1
    |2 0 2
    |3 0 3
    |4 1 0
  """.trim.stripMargin)

  testOffsets("foo\nbar")("""
    |0 0 0
    |1 0 1
    |2 0 2
    |3 0 3
    |4 1 0
    |5 1 1
    |6 1 2
    |7 1 3
  """.trim.stripMargin)
}