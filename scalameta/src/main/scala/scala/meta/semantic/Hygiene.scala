package scala.meta
package internal
package hygiene

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._
import org.scalameta.invariants._

// Our current understanding of hygiene operates based on two primitives:
// 1) Symbols (dumb unique tokens that can be associated with definitions and references to them)
// 2) Sigmas (structures that can be attached to trees in order to remember their lexical contexts)

// In his master thesis, Denys has explored a calculus based on symbols and sigmas
// that ensured hygienic macro expansion for a small Scala-like language.
// The key idea is that having sigmas associated with quasiquotes provides us with a facility
// to perform robust name resolution that accounts for tree origins in a macro-enabled setting.
// Now we want to adapt Denys's results to account for: 1) the entirety of Scala,
// 2) the non-only-for-macros nature for scala.meta.

// We probably won't have time to implement hygiene before M1 (ScalaDays SF),
// but we have to have at least a sketch to make Tree.== at least partially hygienic
// (or we won't be able to write things like `q"case $List(1, 2, 3)" if List == q"List" => ...`).

// In our sketch, symbols are split into global and local. Global symbols can be observed from multiple
// compilation units, so we need a scheme to make observers arrive at the same representation for them.
// Towards that end, we represent public symbols with their full paths and erased signatures.
// For local symbols, things are much simpler - we require unique ids, whose generation is feasible,
// because everyone who can create those symbols is localized to the same compilation unit.

@root trait Signature
object Signature {
  @leaf object Type extends Signature
  @leaf object Val extends Signature
  @leaf class Method(params: Seq[String], ret: String) extends Signature
  @leaf object Object extends Signature
  @leaf object Package extends Signature
}

@root trait Symbol
object Symbol {
  @leaf object Zero extends Symbol
  @leaf object Root extends Symbol
  @leaf class Global(owner: Symbol, signature: Signature) extends Symbol
  @leaf class Local(id: String) extends Symbol
}

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
// they will just look into `tree.symbol` and return that value. We are also going to pre-typecheck
// quasiquotes and pre-populate symbols, so that these naive sigmas can get at least some job done.

@root trait Sigma { def symbol(tree: Tree): Symbol }
object Sigma {
  @leaf object Zero extends Sigma { override def symbol(tree: Tree): Symbol = Symbol.Zero }
  // @leaf object Naive extends Sigma { override def symbol(tree: Tree): Symbol = tree.symbol }
}

object equals {
  def apply(tree1: Tree, tree2: Tree): Boolean = false
}

object hashcode {
  def apply(tree: Tree): Int = System.identityHashCode(tree)
}
