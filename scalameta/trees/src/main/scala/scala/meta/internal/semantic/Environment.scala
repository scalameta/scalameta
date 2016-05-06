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

// TODO: This unrelated code is here because of the limitation of knownDirectSubclasses.
// We would like move it to scala/meta/internal/quasiquotes/ast/ReificationMacros.scala where it belongs,
// but then we have problems with compilation order.
trait EnvironmentLiftables extends adt.Liftables {
  lazy implicit val liftableEnvironment: u.Liftable[Environment] = materializeAdt[Environment]
}
