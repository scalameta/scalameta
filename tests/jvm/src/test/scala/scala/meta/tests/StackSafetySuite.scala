package scala.meta.tests

import scala.meta._
import scala.meta.contrib._

import munit.FunSuite

/**
 * Deeply nested trees must not overflow the stack in any core tree algorithm. See
 * https://github.com/scalameta/scalameta/issues/2509.
 *
 * These cases assert the current behavior -- overflow -- and later commits make each algorithm
 * stack-safe and flip the assertions to expect success. `intercept`/`interceptMessage` use
 * `NonFatal`, which does not catch `StackOverflowError`, so the overflow cases use an explicit
 * `try`/`catch`.
 */
class StackSafetySuite extends FunSuite {

  private val depth = 20000

  private def expectStackOverflow(load: => Any): Unit =
    try {
      load
      fail("expected StackOverflowError")
    } catch { case _: StackOverflowError => }

  test("deeply nested tree - structural equality") {
    val a = TestHelpers.deepTree(depth)
    val b = TestHelpers.deepTree(depth)
    def load() = a.isEqual(b)
    assert(load())
  }

  test("deeply nested tree - transform") {
    val tree = TestHelpers.deepTree(depth)
    // renaming the leaf must rebuild the whole spine without overflowing
    def load() = tree.transform { case Term.Name("x") => Term.Name("y") }
    val renamed = load()
    assert(renamed ne tree) // the spine was actually rebuilt
  }

  test("deeply nested tree - structure") {
    // build the structure Result rather than rendering it: a deep tree's
    // `.structure` string is O(depth^2) and would exhaust the heap. This
    // exercises the construction; the render is covered by the syntax case.
    val tree = TestHelpers.deepTree(depth)
    def load() = implicitly[Structure[Tree]].apply(tree)
    expectStackOverflow(load())
  }

  test("deeply nested tree - syntax") {
    val tree = TestHelpers.deepTree(depth)
    def load() = tree.printSyntaxFor(scala.meta.dialects.Scala213)
    expectStackOverflow(load())
  }

}
