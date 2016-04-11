package org

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.compat.Platform.EOL
import org.scalameta.internal.MacroHelpers

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

  // A marker method used to denote debugging boundaries for `org.scalameta.unreachable`
  // and `org.scalameta.invariants.requireXXX`. See corresponding documentation for more info.
  // TODO: change this from `always true` to `@compileTimeOnly`.
  def debug(xs: Any*): Boolean = true
}

package scalameta {
  class UnreachableError(message: String) extends Error(message)

  object UnreachableError {
    def raise(debuggees: Map[String, Any]): Nothing = {
      def relevantValues = debuggees.toList.sortBy(_._1).map({ case (k, v) => s"where $k = $v"}).mkString(EOL)
      val mandatory = "this code path should've been unreachable"
      val optional = if (debuggees.nonEmpty) EOL + relevantValues else ""
      throw new UnreachableError(mandatory + optional)
    }
  }

  class UnreachableMacros(val c: Context) extends MacroHelpers {
    import c.universe._
    def unreachable: c.Tree = q"$UnreachableErrorModule.raise(${Map.empty[String, Tree]})"
    def unreachableWithDebug(dsl: c.Tree): c.Tree = q"$UnreachableErrorModule.raise(${debuggees(dsl)})"
  }
}