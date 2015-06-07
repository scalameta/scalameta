package scala.meta
package internal
package semantic

import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._
import scala.meta.internal.{ast => impl}

// In Denys's thesis, sigmas are lists of renamings (a renaming maps an attributed named to a name).
// These renamings allow to easily resolve names into symbols, providing basis for hygienic name lookup.
// While working just fine for a calculus, eager lists of renamings are going to be impractical
// for real programs, because of the sheer number of identifiers that are typically in scope.

// Ideally, here we'd want to design an abstraction that would capture all kinds of scopes
// that are available in Scala, so that most of the renamings could be calculated lazily
// (there is still going to be a need for renamings that arise from macro expansions, but those are few).
// However, in order to achieve that, we need either: A) to partially reimplement reify,
// B) or to have AST persistence in place.

// A is too hard and is conflicting with B, and B is still in development, therefore we need a workaround.
// I tried several designs to provide some approximation of renamings suitable for hygienic comparisons.
// Unfortunately, all of them failed, because, as it turns out, to compare trees hygienically,
// one not only needs symbols and sigmas, but also type signatures for all symbols
// E.g. try to compare `Int` and `scala.Int` without having a symbol table at hand.
// Renamings will get you as far as resolving `Int` in the first tree and `scala` in the second tree,
// but you need the entire symbol table to resolve `scala.Int`.

// Therefore, for now our sigmas going to be really stupid. Instead of doing anything to resolve names,
// they will just look into `tree.denotation` and return that value. We are also going to pre-typecheck
// quasiquotes and pre-populate denotations, so that these naive sigmas can get at least some job done.

@root trait Sigma { def resolve(name: Name): Denotation }
object Sigma {
  @leaf object Zero extends Sigma { def resolve(name: Name) = Denotation.Zero }
  @leaf object Naive extends Sigma { def resolve(name: Name) = name.require[impl.Name].denot }
}

// TODO: This unrelated code is here because of the limitation of knownDirectSubclasses.
// We would like move it to scala/meta/internal/quasiquotes/ast/ReificationMacros.scala where it belongs,
// but then we have problems with compilation order.
trait SigmaLiftables extends adt.Liftables {
  lazy implicit val liftableSigma: u.Liftable[Sigma] = materializeAdt[Sigma]
}