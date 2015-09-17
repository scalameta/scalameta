package scala.meta
package semantic

import org.scalameta.adt._
import org.scalameta.invariants._
import scala.meta.semantic.{Context => SemanticContext}

// TODO: Expand this accordingly to evolve into a full-fledged hygiene system for Scala.
// The most promising approach to hygiene at the moment is the new development in the Racketland:
// http://www.cs.utah.edu/~mflatt/scope-sets-5/index.html.

@root trait Environment
object Environment {
  @leaf object Zero extends Environment
}