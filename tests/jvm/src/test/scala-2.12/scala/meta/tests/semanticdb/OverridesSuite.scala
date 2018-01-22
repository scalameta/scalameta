package scala.meta.tests
package semanticdb

import scala.meta.internal.semanticdb.OverrideMode
import scala.meta.internal.semanticdb.SemanticdbMode

class OverridesSuite extends DatabaseSuite(SemanticdbMode.Slim, overrides = OverrideMode.Def) {
//  overrides(
//     """
//       |object a {
//       |  val foo = new java.lang.Runnable {
//       |    def run(): Unit = ()
//       |  }
//       |}""".stripMargin,
//    ""
//  )
  overrides(
    """
      |trait Foo {
      |  def foo: Int
      |}
      |class Foobar extends Foo {
      |  def foo = 3
      |}
    """.stripMargin,
    """
      |_empty_.Foobar#foo()I.{
      |   _empty_.Foo#foo()I.
      |}
    """.stripMargin
  )
}
