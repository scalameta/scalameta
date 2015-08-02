package scala.meta
package internal
package semantic

import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._

@monadicRoot trait Expansion
object Expansion {
  @noneLeaf object Zero extends Expansion
  @someLeaf class Desugaring(term: Term) extends Expansion
}

// TODO: This unrelated code is here because of the limitation of knownDirectSubclasses.
// We would like move it to scala/meta/internal/quasiquotes/ast/ReificationMacros.scala where it belongs,
// but then we have problems with compilation order.
trait ExpansionLiftables extends adt.Liftables {
  implicit def liftableSubTree[T <: Tree]: u.Liftable[T]
  lazy implicit val liftableExpansion: u.Liftable[Expansion] = materializeAdt[Expansion]
}
