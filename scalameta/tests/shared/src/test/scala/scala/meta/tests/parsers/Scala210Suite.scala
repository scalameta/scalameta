package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala210

class Scala210Suite extends ParseSuite {
  test("$_") {
    intercept[TokenizeException](term(""" val q"x + $_" = tree """))
  }

  test("case classes without a parameter list are allowed") {
    templStat("case class A")
  }
}
