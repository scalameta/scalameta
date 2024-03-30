package scala.meta.tests.testkit
package contrib

import scala.meta._
import scala.meta.contrib._
import scala.meta.testkit.SyntaxAnalysis

import munit.FunSuite

class TreeOpsProps extends FunSuite {
  test("find uses Structural equality") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      val names = ast.collect { case t: Defn.Val => t.copy(pats = t.pats) }
      names.filterNot(name => ast.contains[Structurally](name))
    }
    assert(errors.isEmpty)
  }
}
