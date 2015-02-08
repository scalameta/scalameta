### Host implementor notes

scala.meta provides foundational data structures for metaprogramming defined in [Trees.scala](/scalameta/Trees.scala) along with several levels of APIs (syntactic and semantic).

While syntactic services are implemented in scala.meta itself, semantic services require external implementations called hosts, because it would be unreasonable for us to, say, implement Scala's type inference or implicit resolution algorithms from scratch. In [semantic/Context.scala](/scalameta/semantic/Context.scala) and  [macros/Context.scala](/scalameta/macros/Context.scala) we have encapsulated a minimalistic API surface that's required from hosts.

Here is some preliminary documentation on the functionality expected from hosts along with certain background information about scala.meta's data structures.

### Trees

scala.meta trees provide comprehensive coverage of syntactic structures that comprise Scala. They are predefined in [Trees.scala](/scalameta/Trees.scala) in the form of a sealed hierarchy, which means that hosts neither need nor can create custom subclasses of `Tree`.

Hosts are, however, required to create instances of scala.meta trees to be returned from various host APIs, so here we will outline the guidelines that were used to design scala.meta trees in order to allude to expected usage scenarios:

  1. Trees are fully immutable in the sense that they: a) don't contain any observationally mutable fields, b) aren't supposed contain any references to anything that might be mutable. This means that after creation trees can never change and as such can be easily reasoned about. In order to "modify" an existing tree, one is supposed to use either `copy` or one of the tree transformers (at the moment, tree transformer functionality isn't designed yet). Both of these approaches create a copy of an original tree with fields changed appropriately.

  NB! Since trees can't be mutated in-place, reference equality (`Tree.eq` and `Tree.ne`) should not be used to work with trees. The only reliable way of comparing trees for equality is `Tree.==` and `Tree.!=`.

  1. Trees strongly emphasize safety by construction in the sense that they disallow creation of syntactically invalid language constructs (e.g. it's impossible to create trees that represent classes without primary constructors or applications of types to terms). As much validation as possible is pushed to compile time via precise types of tree fields, and some leftovers whose validation was too heavyweight to encode in types are verified at runtime. For you as a host implementor this means that you'll have to be really precise in arguments that you provide to our tree creation facilities.

  1. scala.meta strives for fully faithful representation of language concepts in its reflection API. Among other things, this means that our trees comprehensively describe all syntactic sugar defined by Scala spec. For instance, `(x, y)` in `val (x, y) = 2` remains `(x, y)`, not a pair of `x` and `y` split over two vals, and `List[_]` is actually represented as a type application to `Type.Placeholder`, not as a `Type.Existential`. Depending on the amount of desugarings performed by your host, this aspect of scala.meta might be either trivial or extremely challenging.

  1. scala.meta attempts to unify as much as possible in order to reduce the surface of the API visible to the users. This overarching design theme manifests itself in trees in two different ways:

    1. In Scala 2.10 and 2.11, reflection features three core concepts: trees (syntax), symbols (unique identifiers of definitions) and types (semantic descriptors of trees and symbols). scala.meta unifies all those into just trees. Syntax is represented by trees, symbols are represented by names that are part of definition trees (this neatly expresses the fact that one shouldn't be able to refer to anonymous definitions such as, say, unnamed parameters of functions), and types are represented by their syntax. This unification means that APIs that traditionally live in symbols and types end up on corresponding tree nodes (e.g. `Type.<:<` or `Member.overrides`). Another consequence is that this saves us the redundancies of having duplications like `Method` vs `MethodSymbol`, `ExistentialTypeTree` vs `ExistentialType`, etc. Finally, there's a benefit of having syntax fully describe semantics - e.g. in scala.meta there are no `MethodTypes` or `PolyTypes`, which don't intuitively map onto what the users see in their programs.

    1. Again, if we look into existing reflection facilities of Scala, we'll observe that trees begin their lives naked (just syntax) and then get attributed by the typechecker (i.e. have their `var tpe: Type` and `var symbol: Symbol` fields assigned, typically in place). Semantic operations are only available for attributed trees, and there are some operations that are only available on unattributed trees, which means that the users need to be aware of the distinction. scala.meta unifies these concepts, exposing semantic methods like `Type.<:<` or `Scope.members` that take care of attribution transparently from the user.

  1. Finally, trees are aware of their context. First of all, there's the `Tree.parent` method that can go up the tree structure if the given tree is a part of a bigger tree (as a host implementor, you don't need to worry about maintaining `parent` at all - it is maintained automatically by scala.meta's infrastructure: when a tree is inserted into another tree, it is cloned and gets its `parent` updated accordingly). Also, there's the notion of hygiene (not yet fully implemented) that postulates that trees should generally remember the lexical context of their creation site and respect that context even when they are put into parent that comes from a different context. At this point, the only thing that you need to do to support hygiene is to set denotations in the trees that you return to users (see comments in [Denotations.scala](/scalameta/src/main/scala/scala/meta/internal/hygiene/Denotations.scala) for more information about denotations).

### Immutability

One of the main design goals of scala.meta is to provide a purely functional API to metaprogramming.

Native scala.meta services (parsing, quasiquotes - essentially, everything syntactic) either don't have mutable state at all or have it localized to areas that don't affect publicly observable behavior.

The same level of robustness is expected from hosts. Concretely: 1) semantic operations provided by hosts must be thread-safe, 2) data associated with trees using the scratchpad API (see below) must not be mutable. At the moment `scratchpad` and `withScratchpad` use `Any`, but later on we might refine the type signature or reshape the API altogether.

### Correlation with native trees

When implementing a host for scala.meta, you will most likely have a need to correlate scala.meta trees and your native trees (and/or other metaprogramming artifacts such as e.g. scala.reflect symbols or types) in order to do things like resolving references, computing supertypes, etc.

While, in the ideal world, for that we would like to have a host-independent calculus based on hygiene, designing and implementing it is a huge undertaking, which we don't have time for at the moment. Therefore, for the time being, we expose a theoretically unsatisfying workaround API that can be used to attach arbitrary data to scala.meta trees.

Concretely, every `Tree` has an associated `Seq[Any]` collection that can be accessed via `Tree.scratchpad` and modified in a copy-on-write fashion via `Tree.appendScratchpad(datum: Any)` and `Tree.withScratchpad(data: Seq[Any])`. These APIs are `private[meta]`, so be sure to use it from an appropriately named package (a convention that we use is putting host implementations into the `scala.meta.internal.hosts` package).

We understand that this API is at odds with the goals of statelessness and platform independence of scala.meta trees, but we have to accept this for the time being. Providing a principled API for this area of scala.meta is high on our priority list.

### Context API

<!-- TODO: explain ordering guarantees for all Seq[T] results both in Host and in all our APIs -->

| Method                                                    | Notes
|-----------------------------------------------------------|-----------------------------------------------------------------
| `def dialect: Dialect`                                    | See [dialects/Dialect.scala](/scalameta/dialects/Dialect.scala)
| `def tpe(term: Term): Type`                               | Type of a given term.
| `def defns(ref: Ref): Seq[Member]`                        | Definitions that a given reference refers to. Can return multiple results if a reference resolves to several overloaded members.
| `def owner(member: Member): Scope`                        | This isn't actually a method in `Context`, and it's here only to emphasize a peculiarity of our API. <br/><br/> The reason for that is that scala.meta trees always track their parents, so with a tree in hand it's very easy to navigate its enclosures up until an owning scope.
| `def stats(scope: Scope): Seq[Member]`                    | This isn't actually a method in `Context`, and it's here only to emphasize a peculiarity of our API. <br/><br/> The thing is that trees returned by `def members(tpe: Type): Seq[Member]` must have their contents prepopulated, which makes this method unnecessary. All lists in our API (e.g. the list of statements in a block or a list of declarations in a package or a class) are actually `Seq`'s, which means that prepopulation of contents isn't going to incur prohibitive performance costs.
| `def members(tpe: Type): Seq[Member]`                     | Returns all members defined by a given type. This method should return members that are adjusted to the type arguments and the self type of the provided type. E.g. `t"List".members` should return `Seq(q"def head: A = ...", ...)`, whereas `t"List[Int]".members` should return `Seq(q"def head: Int = ...", ...)`.
| `def isSubType(tpe1: Type, tpe2: Type): Boolean`          | Subtyping check.
| `def lub(tpes: Seq[Type]): Type`                          | Least upper bound.
| `def glb(tpes: Seq[Type]): Type`                          | Greatest lower bound.
| `def widen(tpe: Type): Type`                              | If a given type is a singleton type, widen it. Otherwise, return the input type back.
| `def dealias(tpe: Type): Type`                            | If a given type is a type alias or an application thereof, resolve it. Otherwise, return the input type back.
| `def parents(member: Member): Seq[Member]`                | Direct parents (i.e. superclasses or overriddens) of a given member. If the provided member has been obtained using `members` via some prefix or by instantiating some type parameters, then the results of this method should also have corresponding type parameters instantiated.
| `def children(member: Member): Seq[Member]`               | Direct children (i.e. subclasses or overriders) of a given member in the closed world reflected by the host. If the provided member has been obtained using `members` via some prefix by instantiating some type parameters, then the results of this method should also have corresponding type parameters instantiated.
| `def warning(msg: String): Unit`                          | Produces a warning with a given message. For now, hosts are free to choose the presentation for warnings. Later we will provide a notion of positions.
| `def error(msg: String): Unit`                            | Produces an error with a given message at the position of the macro application. For now, host are free to choose the presentation for errors. Later we will provide a notion of positions.
| `def abort(msg: String): Nothing`                         | Does the same as `error`, additionally terminating the host.
| `def resources: Map[String, Array[Byte]]`                 | Returns a map from resource urls to resource contents. Hosts are advised to strive for compatibility between each other. If the same project is compiled, say, by SBT and then by Intellij IDEA plugin, then it is mandatory for urls emitted by `resources` to be the same.

### Error handling

scala.meta expects hosts to signal errors by throwing exceptions derived from `scala.meta.ui.Exception`. Users of scala.meta might be shielded from these exceptions by an additional error handling layer inside scala.meta, but that shouldn't be a concern for host implementors. At the moment, we don't expose any exception hierarchy, and the only way for the host to elaborate on the details of emitted errors is passing a custom error message. This might change later.
