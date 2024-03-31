package scala.meta.tests.testkit
package transversers

import scala.meta.Tree
import scala.meta.testkit.SyntaxAnalysis
import scala.meta.tests.testkit.contrib.ContribSuite
import scala.meta.transversers.SimpleTraverser
import scala.meta.transversers.Traverser

import scala.collection.mutable

import munit.FunSuite

class TraverserOrder extends FunSuite {

  /**
   * Traversal doesn't work correctly for @binaryCompatFields currently. This will be an issue for
   * anyone using it for Scala 3 sources, but should not be a problem otherwise. To fix it we need
   * to make traversal work with those fields or bump the version to 5.0.0 and turn all of those
   * fields to real ones or just drop the traverser API.
   * https://github.com/scalameta/scalameta/issues/2247
   */
  test("traversal order is preserved") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      val trees1 = asList_Traverser(ast)
      val trees2 = asList_SimpleTraverser(ast)

      if (trees1.size != trees2.size) List(ast)
      else {
        val firstDiff = trees1.zip(trees2).collectFirst { case (a, b) if a != b => a }
        firstDiff.toList
      }
    }
    assert(errors.isEmpty)
  }

  private def asList_Traverser(tree: Tree): List[Tree] = {
    val buf = mutable.ListBuffer[Tree]()
    object traverser extends Traverser {
      override def apply(tree: Tree): Unit = { buf += tree; super.apply(tree) }
    }
    traverser(tree)
    buf.toList
  }

  private def asList_SimpleTraverser(tree: Tree): List[Tree] = {
    val buf = mutable.ListBuffer[Tree]()
    object traverser extends SimpleTraverser {
      override def apply(tree: Tree): Unit = { buf += tree; super.apply(tree) }
    }
    traverser(tree)
    buf.toList
  }
}
