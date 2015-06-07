package scala.meta
package internal
package semantic

import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._

@root trait Status
object Status {
  @leaf object Unknown extends Status
}

// TODO: This unrelated code is here because of the limitation of knownDirectSubclasses.
// We would like move it to scala/meta/internal/quasiquotes/ast/ReificationMacros.scala where it belongs,
// but then we have problems with compilation order.
trait StatusLiftables extends adt.Liftables {
  lazy implicit val liftableStatus: u.Liftable[Status] = materializeAdt[Status]
}
