package org

import scala.language.experimental.macros

package object scalameta {
  // Statically indicates that a given code path is unreachable.
  // Throws an exception if the execution reaches this invocation at runtime.
  def unreachable: Nothing = macro UnreachableMacros.unreachable

  // Does the same as its parameterless analogue, but generates a better exception message.
  //
  // The message is generated from the `dsl` argument by finding and printing out
  // all occurrences of calls to the `org.scalameta.debug` method.
  // The actual runtime value of the `dsl` argument is completely irrelevant,
  // because it's never computed.
  //
  // Example: `unreachable(debug(foo + bar) && debug(baz))` will throw an exception
  // that will say "this code path should've been unreachable" and will print something like:
  //   baz = 42
  //   foo + bar = "foobar"
  def unreachable(dsl: Boolean): Nothing = macro UnreachableMacros.unreachableWithDebug

  def unreachable(dsl: Boolean, clue: String): Nothing =
    macro UnreachableMacros.unreachableWithDebugAndClue

  // A marker method used to denote debugging boundaries for `org.scalameta.unreachable`
  // and `org.scalameta.invariants.requireXXX`. See corresponding documentation for more info.
  def debug(xs: Any*): Boolean = true
}
