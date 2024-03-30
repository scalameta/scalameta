package scala.meta.tests

package quasiquotes

import scala.meta._

class DottySuccessSuite extends TreeSuiteBase {

  import dialects.Scala3Future

  test("1 t\"(..tpes) => tpe [param clause]\"") {
    val t"(..$tpes) => $tpe" = t"(x: X, y: Y) => Z"

    val tpeX = Type.TypedParam(pname("x"), pname("X"))
    val tpeY = Type.TypedParam(pname("y"), pname("Y"))
    checkTree(tpes, "(x: X, y: Y)")(Type.FuncParamClause(List(tpeX, tpeY)))
    checkTree(tpe, "Z")(pname("Z"))

    checkTree(t"(..${tpes.values}) => $tpe", "(x: X, y: Y) => Z")(Type.Function(tpes, tpe))
  }

}
