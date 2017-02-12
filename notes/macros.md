### Macros used in the public API of scala.meta

Essential:
  * Quasiquotes (string interpolators, auxiliary `Lift`/`Unlift` macros)

### Macros used when compiling scala.meta

Essential:
  * `@root`, `@branch`, `@leaf` and supporting infrastructure for ADTs, ASTs and tokens
  * `@transformer` and `@traverser`

Convenience:
  * `unreachable`
  * `require`
  * `@quasiquote`
  * `@classifier`
  * Some implementation details of `Show`
  * Materializers of `Liftable` for core data structures
  * Materializers of `AstInfo` and `TokenInfo`

### Macros used when testing scala.meta

  * `wildcardImportStatics`
  * `allStatics`
  * `allSurface`
  * `typecheckError`