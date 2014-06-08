### Host implementor notes

Palladium provides foundational data structures for metaprogramming defined in [Trees.scala](/reflection/Trees.scala) along with several levels of APIs (syntactic and semantic).

While syntactic services are implemented in Palladium itself, semantic services require external implementations called hosts, because it would be unreasonable for us to, say, implement Scala's type inference or implicit resolution algorithms from scratch. In [Hosts.scala](reflection/semantic/Hosts.scala) we have encapsulated a minimalistic API surface that's required from hosts.

Here is some preliminary documentation on the functionality expected from hosts along with certain background information about Palladium's data structures.

### Trees

Palladium trees provide comprehensive coverage of syntactic structures that comprise Scala. They are predefined in [Trees.scala](/reflection/Trees.scala) in the form of a sealed hierarchy, which means that hosts neither need nor can create custom subclasses of `Tree`.

Hosts are, however, required to create instances of Palladium trees to be returned from various host APIs, so here we will outline the guidelines that were used to design Palladium trees in order to allude to expected usage scenarios:

  1. Trees are fully immutable in the sense that they: a) don't contain any observationally mutable fields, b) aren't supposed contain any references to anything that might be mutable. This means that after creation trees can never change and as such can be easily reasoned about. In order to "modify" an existing tree, one is supposed to use either `copy` or one of the tree transformers (at the moment, tree transformer functionality isn't designed yet). Both of these approaches create a copy of an original tree with fields changed appropriately.

  NB! Since trees can't be mutated in-place, reference equality (`Tree.eq` and `Tree.ne`) should not be used to work with trees. The only reliable way of comparing trees for equality is `Tree.==` and `Tree.!=`.

  1. Trees strongly emphasize safety by construction in the sense that they disallow creation of syntactically invalid language constructs (e.g. it's impossible to create trees that represent classes without primary constructors or applications of types to terms). As much validation as possible is pushed to compile time via precise types of tree fields, and some leftovers whose validation was too heavyweight to encode in types are verified at runtime. For you as a host implementor this means that you'll have to be really precise in arguments that you provide to our tree creation facilities.

  1. Palladium strives for fully faithful representation of language concepts in its reflection API. Among other things, this means that our trees comprehensively describe all syntactic sugar defined by Scala spec. For instance, `(x, y)` in `val (x, y) = 2` remains `(x, y)`, not a pair of `x` and `y` split over two vals, and `List[_]` is actually represented as a type application to `Type.Placeholder`, not as a `Type.Existential`. Depending on the amount of desugarings performed by your host, this aspect of Palladium might be either trivial or extremely challenging.

  1. Palladium attempts to unify as much as possible in order to reduce the surface of the API visible to the users. This overarching design theme manifests itself in trees in two different ways:

    1. In Scala 2.10 and 2.11, reflection features three core concepts: trees (syntax), symbols (unique identifiers of definitions) and types (semantic descriptors of trees and symbols). Palladium unifies all those into just trees. Syntax is represented by trees, symbols are represented by names that are part of definition trees (this neatly expresses the fact that one shouldn't be able to refer to anonymous definitions such as, say, unnamed parameters of functions), and types are represented by their syntax. This unification means that APIs that traditionally live in symbols and types end up on corresponding tree nodes (e.g. `Type.<:<` or `Member.overrides`). Another consequence is that this saves us the redundancies of having duplications like `Method` vs `MethodSymbol`, `ExistentialTypeTree` vs `ExistentialType`, etc. Finally, there's a benefit of having syntax fully describe semantics - e.g. in Palladium there are no `MethodTypes` or `PolyTypes`, which don't intuitively map onto what the users see in their programs. For host implementors this means that certain trees (e.g. ones converted from host symbols and types) might need to cache semantic information, and we provide a dedicated API for that: `Tree.scratchpad` and `Tree.withScratchpad`/`Tree.mapScratchpad`. This API is `private[reflect]`, so be sure to use it from an appropriately named package (one convention that's worth following is to put your host implementation into the `scala.reflect.internal.hosts` package).

    1. Again, if we look into existing reflection facilities of Scala, we'll observe that trees begin their lives naked (just syntax) and then get attributed by the typechecker (i.e. have their `var tpe: Type` and `var symbol: Symbol` fields assigned, typically in place). Semantic operations are only available for attributed trees, and there are some operations that are only available on unattributed trees, which means that the users need to be aware of the distinction. Palladium unifies these concepts, exposing semantic methods like `Type.<:<` or `Scope.members` that take care of attribution transparently from the user and providing `Tree.attrs` that can be used to ask the host for type information. As a host implementor, you will most probably want to cache the results of semantic operations used the API mentioned above. Also, in order to correctly implement attribution, for every tree you will need to track the context in which it lives using the facilities described below.

  1. Finally, trees are aware of their context. First of all, there's the `Tree.parent` method that can go up the tree structure if the given tree is a part of a bigger tree (as a host implementor, you don't need to worry about maintaining `parent` at all - it is maintained automatically by Palladium's infrastructure when a tree is inserted into another tree). Also, there's the notion of hygiene (not yet implemented) that postulates that trees should generally remember the lexical context of their creation site and respect that context even when they are put into parent that comes from a different context.

### Guarantees

One of the main design goals of Palladium is to provide a purely functional API to metaprogramming.

Native Palladium services (parsing, quasiquotes - essentially, everything syntactic) either don't have mutable state at all or have it localized to areas that don't affect publicly observable behavior.

The same level of robustness is expected from hosts. Concretely: 1) semantic operations provided by hosts must be thread-safe, 2) data associated with trees using the scratchpad API must not be mutable. At the moment `scratchpad` and `withScratchpad` use `Any`, but later on we might refine the type signature or reshape the API altogether.

### Host API

<!-- TODO: explain ordering guarantees for all Seq[T] results both in Host and in all our APIs -->

| Method                                                    | Notes
|-----------------------------------------------------------|-----------------------------------------------------------------
| `def defns(ref: Ref): Seq[Tree]`                          | Goes from a reference to the associated definiton or definitions. Might have to return multiple results to account for overloading or situations like imports, super/this qualifiers and accessibility boundaries. If necessary, overloads can be resolved later via `Overload.resolve` or `term.attrs`, but this function shouldn't attempt to do that.
| `def attrs(tree: Tree): Seq[Attr]`                        | Computes and returns semantic information for a given tree: resolved, i.e. non-overloaded reference to a definition, type, inferred type and value arguments, macro expansion, etc.
| `def owner(tree: Tree): Scope`                            | A scope that owns the provided tree, be it a definition or a statement in some definition.
| `def stats(scope: Scope): Seq[Tree]`                      | This isn't actually a method in `Host`, and it's here only to emphasize a peculiarity of our API. This particular method in unnecessary, because all lists in trees (e.g. the list of statements in a block or a list of declarations in a package or a class) are actually `Seq`'s, which means that the host can choose between eager and lazy population of scope contents when returning trees to the user of the Palladium API (e.g. in `root`).
| `def members(scope: Scope): Seq[Member]`                  | Returns all members belonging to the specified scope. This API could query something as simple as parameters of a method or as complex as all members of a given class (accounting for inherited members and overriding). When called on a `Type` (types can be viewed as scopes as well), this method should return members that are adjusted to the type's type arguments and self type. E.g. `t"List".members` should return `Seq(q"def head: A = ...", ...)`, whereas `t"List[Int]".members` should return `Seq(q"def head: Int = ...", ...)`.
| `def members(scope: Scope, name: Name): Seq[Tree]`        | Same as the previous method, but indexed by name. This method would be trivially implementable on top of the previous method if performance didn't matter, but it does, so we expose two methods instead of one. In the future we could have change the signature of the previous method to `def members(scope: Scope): Query[Tree]` and remove this method altogether.
| `def overrides(member: Member): Seq[Member]`              | All definitions overridden by a given definition in linearization order. If the provided member has been obtained by instantiating certain type parameters, then the results of this method should also have corresponding type parameters instantiated.
| `def overriddenBy(member: Member): Seq[Member]`           | All definitions overriding a given definition in the closed world reflected by the host. If the provided member has been obtained by instantiating certain type parameters, then the results of this method should also have corresponding type parameters instantiated.
| `def <:<(tpe1: Type, tpe2: Type): Boolean`                | Subtyping check
| `def supertypes(tpe: Type): Seq[Type]`                    | All supertypes of a given type in linearization order, with type arguments instantiated accordingly.
| `def subclasses(tpe: Type): Seq[Member.Template]`         | All subclasses of a given type in the closed world reflected by the host.
| `def linearization(tpes: Seq[Type]): Seq[Type]`           | Linearization.
| `def self(tpe: Type): Aux.Self`                           | Self type of a given type, again with type arguments instantiated accordingly.
| `def lub(tpes: Seq[Type]): Type`                          | Least upper bound.
| `def glb(tpes: Seq[Type]): Type`                          | Greatest lower bound.
| `def widen(tpe: Type): Type`                              | Goes from a singleton type to a type underlying that singleton. E.g. `t"x.type".widen` in a context that features `val x = 2` should return `t"Int"`. Widenings should be performed shallowly (i.e. `List[x.type]` shouldn't be changed), but to the maximum possible extent (e.g. for `val x = 2; val y: x.type = x`, `t"y.type".widen` should return `t"Int"`, not `t"x.type"`).
| `def dealias(tpe: Type): Type`                            | Goes from a type alias or an application of a type alias to the underlying type. Again, this should work shallowly, but to the maximum possible extent.
| `def erasure(tpe: Type): Type`                            | Erasure.
| `def warning(msg: String): Unit`                          | Produces a warning with a given message. For now, hosts are free to choose the presentation for warnings. Later we will provide a notion of positions.
| `def error(msg: String): Unit`                            | Produces an error with a given message at the position of the macro application. For now, host are free to choose the presentation for errors. Later we will provide a notion of positions.
| `def abort(msg: String): Nothing`                         | Does the same as `error`, additionally terminating the host.
| `def resources: Seq[String]`                              | Returns a list of urls of build resources. Hosts are advised to strive for compatibility between each other. If the same project is compiled, say, by SBT and then by Intellij IDEA plugin, then it is mandatory for urls emitted by `resources` to be the same.
| `def resourceAsBytes(url: String): Array[Byte]`           | Reads the specified resource into an array of byte. Users of the Palladium API will then decide whether/how to convert these bytes into strings or something else.

### Error handling

Palladium expects hosts to signal errors by throwing exceptions derived from `scala.reflect.core.ReflectionException`. Users of Palladium might be shielded from these exceptions by an additional error handling layer inside Palladium, but that shouldn't be a concern for host implementors. At the moment, we don't expose any exception hierarchy, and the only way for the host to elaborate on the details of emitted errors is passing a custom error message. This might change later.
