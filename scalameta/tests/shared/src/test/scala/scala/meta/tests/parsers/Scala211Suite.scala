package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala211

class Scala211Suite extends ParseSuite {
  test("case classes without a parameter list are not allowed") {
    def failWithMessage(code: String) = {
      val error = intercept[ParseException](templStat(code))
      assert(error.getMessage.contains(
        "case classes must have a parameter list; try 'case class A()' or 'case object A'"))
    }

    failWithMessage("case class A")
    failWithMessage("case class A[T]")
    failWithMessage("case class A[T] private")
  }

  test("case classes with an empty parameter list are allowed") {
    templStat("case class A()")
    templStat("case class A @Inject() ()")
    templStat("case class A private ()")
  }
}
