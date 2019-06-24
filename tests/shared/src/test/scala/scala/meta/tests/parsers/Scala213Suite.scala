package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala213

class Scala213Suite extends ParseSuite {
  checkOK("def foo(implicit x: => Int) = 1")
  checkOK("def foo(implicit y: Int, x: => Int) = 1")
}
