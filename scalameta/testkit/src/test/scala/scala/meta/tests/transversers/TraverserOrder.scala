package scala.meta.tests
package transversers

import org.scalatest.FunSuite

import scala.collection.mutable
import scala.meta.Tree
import scala.meta.testkit.SyntaxAnalysis
import scala.meta.tests.contrib.ContribSuite
import scala.meta.transversers.{SimpleTraverser, Traverser}

class TraverserOrder extends FunSuite {
  test("traversal order is preserved") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      val trees1 = asList_Traverser(ast)
      val trees2 = asList_SimpleTraverser(ast)

      if (trees1.size != trees2.size) List(ast)
      else {
        val firstDiff =
          trees1
            .zip(trees2)
            .collectFirst { case (a, b) if a != b => a }
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
