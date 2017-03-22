package scala.meta.tests
package parsers

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala210

class Scala210Suite extends ParseSuite {
  test("$_") {
    intercept[TokenizeException](term(""" val q"x + $_" = tree """))
  }
}