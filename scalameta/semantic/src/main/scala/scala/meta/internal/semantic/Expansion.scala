package scala.meta
package internal
package semantic

import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._
import scala.meta.internal.ui.Attributes

@monadicRoot trait Expansion
object Expansion {
  @noneLeaf object Zero extends Expansion
  @noneLeaf object Identity extends Expansion
  @someLeaf class Desugaring(term: Term) extends Expansion {
    require(term.isTypechecked && debug(term.show[Attributes]))
    override def canEqual(other: Any): Boolean = other.isInstanceOf[Desugaring]
    override def equals(that: Any): Boolean = that match {
      case that: Desugaring => equality.Semantic.equals(this.term, that.term)
      case _ => false
    }
    override def hashCode: Int = equality.Semantic.hashCode(this.term)
  }
}

// TODO: This unrelated code is here because of the limitation of knownDirectSubclasses.
// We would like move it to scala/meta/internal/quasiquotes/ast/ReificationMacros.scala where it belongs,
// but then we have problems with compilation order.
trait ExpansionLiftables extends adt.Liftables {
  implicit def liftableSubTree[T <: Tree]: u.Liftable[T]
  lazy implicit val liftableExpansion: u.Liftable[Expansion] = materializeAdt[Expansion]
}
