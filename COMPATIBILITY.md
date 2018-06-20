## Public API

"Public API" means all public definitions in the artifacts under the
`org.scalameta` Maven groupId whole fully-qualified name starts with
`scala.meta.`, but does not include `.internal.` and `.contrib.`,
e.g. `scala.meta.Tree` or `scala.meta.inputs.Position`:
  * `{x+1}.0.0` is strongly encouraged to be backward compatible with `x.y.z`
    modulo deprecation warnings:
    * It is desirable for every Scala program compilable against `x.y.z`
      to be compilable against `{x+1}.0.0` modulo deprecation warnings.
    * It is desirable for every Scala program linkable against `x.y.z`
      to be linkable with `{x+1}.0.0`.
  * `x.{y+1}.0` must be backward compatible with `x.y.0`:
    * Every Scala program compilable against `x.y.0` must be compilable against `x.{y+1}.z`
      without deprecation warnings.
    * Every Scala program linkable against `x.y.0` must be linkable against `x.{y+1}.z`.
  * `x.y.z1` must be compatible with `x.y.z2`:
    * Every Scala program compilable against `x.y.z1` must be compilable against `x.y.z2`
      without deprecation warnings.
    * Every Scala program linkable against `x.y.z1` must be linkable against `x.y.z2`.
  * Any other version doesn't have to be compatible with any other version in any way.

## Internal API

"Internal API" means all definitions in the artifacts under the `org.scalameta`
Maven groupId that don't qualify as public API (see above),
e.g. `scala.meta.internal.semanticdb`, `scala.meta.contrib.Extract` or
`scala.meta.Tree.privatePrototype`:
  * Any version doesn't have to be compatible with any version in any way.

## SemanticDB schema

"SemanticDB schema" means the Protocol Buffers schema provided
in [the SemanticDB specification](semanticdb/README.md).
"SemanticDB payload" means binary data that conforms to the SemanticDB schema:
  * `x.y.z` must be backward compatible with `x.0.0`:
    * Every `x.0.0` payload must be a valid `x.y.z` payload.
  * `x.0.0` must be forward compatible with `x.y.z`:
    * Every `x.y.z` payload must be a valid `x.0.0` payload.
  * Any other version doesn't have to be compatible with any other version in any way.
