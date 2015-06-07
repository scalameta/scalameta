package scala.meta
package internal

package object semantic {
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

  // See Denotation.scala to learn about how we model symbols.
  // See Sigma.scala to learn about how we model sigmas.
  // See Equality.scala to learn how we apply this model to hygienically compare trees.
}
