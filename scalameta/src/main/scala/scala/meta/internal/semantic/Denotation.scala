package scala.meta
package internal
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._

// In our sketch, symbols are split into global and local. Global symbols can be observed from multiple
// compilation units, so we need a scheme to make observers arrive at the same representation for them.
// Towards that end, we represent public symbols with their full paths and erased signatures.
// For local symbols, things are much simpler - we require unique ids, whose generation is feasible,
// because everyone who can create those symbols is localized to the same compilation unit.

@root trait Signature
object Signature {
  @leaf object Type extends Signature
  @leaf object Term extends Signature
  @leaf class Method(jvmSignature: String) extends Signature
  @leaf object TypeParameter extends Signature
  @leaf object TermParameter extends Signature
}

@root trait Symbol
object Symbol {
  @leaf object Zero extends Symbol
  @leaf object RootPackage extends Symbol
  @leaf object EmptyPackage extends Symbol
  @leaf class Global(owner: Symbol, name: String, signature: Signature) extends Symbol
  @leaf class Local(id: String) extends Symbol
}

// upd. It should've been obvious from the very beginning, but symbols alone don't cut it.
// Even though symbols and symbol-based sigmas can preserve enough information to ensure hygiene,
// that information isn't useful in itself - only a full-fledged typechecker can make sense of it
// (in the paper, such a typechecker includes a symbol table and a list of prefixes for all symbols).
// However, when comparing trees, we don't have a typechecker, so we need to figure out what else is necessary.
// Another essential thing apart from symbols is prefixes. Let's hope that this is it.

@root trait Prefix
object Prefix {
  @leaf object Zero extends Prefix
  @leaf class Type(tpe: scala.meta.Type) extends Prefix
}

// upd. The bunch of information that should be enough for hygienic comparison?
// We define it here and call it denotation. Hopefully, prefix and symbol are all that it takes.
// Also, hopefully, it's only necessary to define denotations for all names in a tree
// to guarantee hygienic comparisons and name lookups.

@root trait Denotation { def prefix: Prefix; def symbols: List[Symbol] }
object Denotation {
  @leaf object Zero extends Denotation { def prefix = Prefix.Zero; def symbols = Nil; }
  @leaf class Single(prefix: Prefix, symbol: Symbol) extends Denotation { def symbols = List(symbol) }
  @leaf class Multi(prefix: Prefix, symbols: List[Symbol]) extends Denotation { require(symbols.length > 1) }
}

// TODO: This unrelated code is here because of the limitation of knownDirectSubclasses.
// We would like move it to scala/meta/internal/quasiquotes/ast/ReificationMacros.scala where it belongs,
// but then we have problems with compilation order.
trait DenotationLiftables extends adt.Liftables {
  implicit def liftableSubTree[T <: Tree]: u.Liftable[T]
  lazy implicit val liftableDenotation: u.Liftable[Denotation] = materializeAdt[Denotation]
}
