package scala.meta.tests
package transversers

import scala.meta._

class TraverserSuite extends TreeSuiteBase {

  test("Traverser Ok") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    val log = scala.collection.mutable.ListBuffer[String]()
    object traverser extends Traverser {
      override def apply(tree: Tree): Unit = {
        log += tree.toString.trim.replace("\n", " ")
        super.apply(tree)
      }
    }
    traverser(tree0)
    assertEquals(
      log.mkString("", "\n", "\n").nl2lf,
      """|def foo(x: x)(x: Int) = x + x       class C(x: x) {         def bar(x: x) = ???       }
         |def foo(x: x)(x: Int) = x + x
         |foo
         |(x: x)(x: Int)
         |
         |(x: x)
         |x: x
         |x
         |x
         |(x: Int)
         |x: Int
         |x
         |Int
         |x + x
         |x
         |+
         |
         |x
         |x
         |class C(x: x) {         def bar(x: x) = ???       }
         |C
         |
         |def this(x: x)
         |
         |(x: x)
         |x: x
         |x
         |x
         |{         def bar(x: x) = ???       }
         |{         def bar(x: x) = ???       }
         |def bar(x: x) = ???
         |bar
         |(x: x)
         |
         |(x: x)
         |x: x
         |x
         |x
         |???
         |""".stripMargin
    )
  }
}
