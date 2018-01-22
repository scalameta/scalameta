package scala.meta.tests
package semanticdb

import scala.meta.internal.semanticdb.OverrideMode
import scala.meta.internal.semanticdb.SemanticdbMode

class OverridesSuite extends DatabaseSuite(SemanticdbMode.Slim, overrides = OverrideMode.Def) {
  overrides(
    """
       |trait A { def foo: Int }
       |trait B { def foo: Int }
       |class C extends A with B { def foo: Int = 1 }
       |class D extends C { def foo: Int = 2 }
       """.stripMargin,
    """
       |_empty_.D#foo()I.{
       |  _empty_.C#foo()I.
       |  _empty_.B#foo()I.
       |  _empty_.A#foo()I.
       |}
       |_empty_.C#foo()I.{
       |  _empty_.B#foo()I.
       |  _empty_.A#foo()I.
       |}""".stripMargin
  )
  overrides(
     """
       |object a {
       |  val foo = new java.util.TimerTask {
       |    def run(): Unit = ()
       |  }
       |}""".stripMargin,
     """
       |_empty_.a.foo.$anon#run()V.{
       |  _root_.java.util.TimerTask#run()V.
       |  _root_.java.lang.Runnable#run()V.
       |}""".stripMargin
  )
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
