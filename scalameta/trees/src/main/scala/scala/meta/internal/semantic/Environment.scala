package scala.meta
package internal
package semantic

import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._
import scala.meta.common._

// TODO: Expand this accordingly to evolve into a full-fledged hygiene system for Scala.
// The most promising approach to hygiene at the moment is the new development in the Racketland:
// http://www.cs.utah.edu/~mflatt/scope-sets-5/index.html.

@root trait Environment extends Optional
object Environment {
  @none object None extends Environment
}

// NOTE: Need this code in this very file in order to avoid issues with knownDirectSubclasses.
// Without this, compilation order may unexpectedly affect compilation success.
trait EnvironmentLiftables extends adt.Liftables {
  lazy implicit val liftableEnvironment: u.Liftable[Environment] = materializeAdt[Environment]
}
