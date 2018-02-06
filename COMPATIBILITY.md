## Public API

"Public API" means all public definitions in the artifacts under the
`org.scalameta` Maven groupId whose fully-qualified name does not include
`internal` and `contrib`, e.g. `scala.meta.Tree` or `org.langmeta.Position`:
  * `x.y.z` must be backward compatible with `x.0.0`:
    * Every program compilable against `x.0.0` must be compilable against `x.y.z`.
    * Every program linkable against `x.0.0` must be linkable against `x.y.z`.
  * `x.0.0` doesn't have to be forward compatible with `x.y.z`:
    * A program compilable against `x.y.z` may fail to compile against `x.0.0`.
    * A program linkable against `x.y.z` may fail to link against `x.0.0`.
  * `{x+1}.0.0` is strongly encouraged to be backward compatible with `x.y.z`:
    * It is desirable for every program compilable against `x.y.z`
      to be compilable against `{x+1}.0.0` modulo deprecation warnings.
    * It is desirable for every program linkable against `x.y.z`
      to be linkable with `{x+1}.0.0`.
  * `x.y.z` doesn't have to be forward compatible with `{x+1}.0.0`:
    * A program compilable against `{x+1}.0.0` may fail to compile against `x.y.z`.
    * A program linkable against `{x+1}.0.0` may fail to link against `x.y.z`.
  * Any other version doesn't have to be compatible with any other version in any way.

## Internal API

"Internal API" means all definitions in the artifacts under the `org.scalameta`
Maven groupId that don't qualify as public API (see above),
e.g. `scala.meta.internal.semanticdb3`, `scala.meta.contrib.Extract` or
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
