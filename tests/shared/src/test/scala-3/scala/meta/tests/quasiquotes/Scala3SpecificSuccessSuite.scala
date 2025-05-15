package scala.meta.tests

package quasiquotes

import scala.meta._
import scala.meta.dialects.Scala3

class Scala3SpecificSuccessSuite extends TreeSuiteBase {

  test("multiline - parse when inlined") {
    inline def inlinedMultiline(name: Type.Name) =
      q"""
        class $name:
          val a: Int = 1
      """
    assertEquals(
      compileErrors("""inlinedMultiline(Type.Name("AAA"))"""),
      s"""error: double quotes are not allowed in single-line quasiquotes
         |package quasiquotes
         |^""".stripMargin
    )
  }

  test("single-line - parse when inlined") {
    inline def inlinedSingleline() = q"val aaa: Int = 1"
    assertEquals(
      compileErrors("""inlinedSingleline()"""),
      s"""error: `;` expected but `string constant` found
         |package scala.meta.tests
         |                  ^""".stripMargin
    )
  }

}
