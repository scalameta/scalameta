### Macros used in the public API of scala.meta

Essential:
  * Quasiquotes (string interpolators, auxiliary Lift/Unlift macros)

Convenience:
  * Materializer of `AllowEquality`

### Macros used when compiling scala.meta

Essential:
  * `@root`, `@branch` and `@leaf` (or its analogues) for ADTs, ASTs and tokens
  * `@transformer` and `@traverser`
  * Materializers of `AstInfo` and `TokenInfo`

Convenience:
  * `unreachable`
  * `require`
  * `@quasiquote`
  * `@classifier`
  * Some implementation details of `Show`
  * Materializers of `Liftable` for core data structures

### Macros used when testing scala.meta

  * `publicTopLevelDefinitions`
  * `typecheckError`