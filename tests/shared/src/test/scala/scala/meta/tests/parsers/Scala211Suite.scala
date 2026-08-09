package scala.meta.tests
package parsers

import scala.meta._

class Scala211Suite extends ParseSuite {

  implicit val dialect: Dialect = dialects.Scala211

  test("case classes without a parameter list") {
    templStat("case class A")
    templStat("case class A[T]")
    templStat("case class A[T] private")
  }

  test("case classes with an empty parameter list") {
    templStat("case class A()")
    templStat("case class A @Inject() ()")
    templStat("case class A private ()")
  }
}
