package scala.meta
package internal
package semantic

import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._

@root trait Expansion
object Expansion {
  @leaf object Identity extends Expansion
}

// TODO: This unrelated code is here because of the limitation of knownDirectSubclasses.
// We would like move it to scala/meta/internal/quasiquotes/ast/ReificationMacros.scala where it belongs,
// but then we have problems with compilation order.
trait ExpansionLiftables extends adt.Liftables {
  lazy implicit val liftableExpansion: u.Liftable[Expansion] = materializeAdt[Expansion]
}
