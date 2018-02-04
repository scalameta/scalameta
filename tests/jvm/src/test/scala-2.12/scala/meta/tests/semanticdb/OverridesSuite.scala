package scala.meta.tests
package semanticdb

import scala.meta.internal.semanticdb.scalac._

class OverridesSuite extends DatabaseSuite(SemanticdbMode.Slim, overrides = OverrideMode.All) {
  // Dont save overrides for reference, only for definition
  overrides(
    "object a { Array.empty[Int].headOption }",
    ""
  )
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
  // the denotation of B.foo overrides A.foo
  symbols(
    """
       |trait A { def foo: Int }
       |class B() extends A { def foo: Int = 2 }
       """.stripMargin,
    """
       |_empty_.A# => trait A
       |_empty_.A#foo()I. => abstract def foo: Int
       |  [0..3): Int => _root_.scala.Int#
       |_empty_.B# => class B
       |_empty_.B#`<init>`()V. => primaryctor <init>: (): B
       |  [4..5): B => _empty_.B#
       |_empty_.B#foo()I. => def foo: Int
       |  override _empty_.A#foo()I.
       |  [0..3): Int => _root_.scala.Int#
       |_root_.java.lang.Object#`<init>`()V. => javadefined primaryctor <init>: (): Object
       |  [4..10): Object => _root_.java.lang.Object#
       |_root_.scala.Int# => abstract final class Int""".stripMargin
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
       |}
       |_root_.java.util.TimerTask#run()V.{
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
