package scala.meta.tests

package quasiquotes

import scala.meta._

class DottySuccessSuite extends TreeSuiteBase {

  import dialects.Scala3Future

  test("1 t\"(..tpes) => tpe [param clause]\"") {
    val t"(..$tpes) => $tpe" = t"(x: X, y: Y) => Z"

    val tpeX = Type.TypedParam(Type.Name("x"), Type.Name("X"))
    val tpeY = Type.TypedParam(Type.Name("y"), Type.Name("Y"))
    checkTreesWithSyntax(tpes: _*)("x: X", "y: Y")(tpeX, tpeY)
    checkTree(tpe, "Z")(Type.Name("Z"))

    checkTree(t"(..${tpes.values}) => $tpe", "(x: X, y: Y) => Z")(
      Type.Function(tpes, tpe)
    )
  }

}
