### Host implementor notes

scala.meta provides foundational data structures for metaprogramming defined in [Trees.scala](/scalameta/src/main/scala/scala/meta/Trees.scala) along with several levels of APIs (syntactic and semantic).

While syntactic services are implemented in scala.meta itself, semantic services require external implementations called hosts, because it would be unreasonable for us to, say, implement Scala's type inference or implicit resolution algorithms from scratch. In [semantic/Context.scala](/scalameta/src/main/scala/scala/meta/semantic/Context.scala) and  [macros/Context.scala](/scalameta/src/main/scala/scala/meta/macros/Context.scala) we have encapsulated a minimalistic API surface that's required from hosts.

Here is some preliminary documentation on the functionality expected from hosts along with certain background information about scala.meta's data structures.

### Trees

scala.meta trees provide comprehensive coverage of syntactic structures that comprise Scala. They are predefined in [Trees.scala](/scalameta/src/main/scala/scala/meta/Trees.scala) in the form of a sealed hierarchy, which means that hosts neither need nor can create custom subclasses of `Tree`.

Hosts are, however, required to create instances of scala.meta trees to be returned from various host APIs, so here we will outline the guidelines that were used to design scala.meta trees in order to allude to expected usage scenarios:

  1. Trees are fully immutable in the sense that they: a) don't contain any observationally mutable fields, b) aren't supposed contain any references to anything that might be mutable. This means that after creation trees can never change and as such can be easily reasoned about. In order to "modify" an existing tree, one is supposed to use either `copy` or one of the tree transformers (at the moment, tree transformer functionality isn't designed yet). Both of these approaches create a copy of an original tree with fields changed appropriately.

  NB! Since trees can't be mutated in-place, reference equality (`Tree.eq` and `Tree.ne`) should not be used to work with trees. The only reliable way of comparing trees for equality is `Tree.==` and `Tree.!=`.

  1. Trees strongly emphasize safety by construction in the sense that they disallow creation of syntactically invalid language constructs (e.g. it's impossible to create trees that represent classes without primary constructors or applications of types to terms). As much validation as possible is pushed to compile time via precise types of tree fields, and some leftovers whose validation was too heavyweight to encode in types are verified at runtime. For you as a host implementor this means that you'll have to be really precise in arguments that you provide to our tree creation facilities.

  1. scala.meta strives for fully faithful representation of language concepts in its reflection API. Among other things, this means that our trees comprehensively describe all syntactic sugar defined by Scala spec. For instance, `(x, y)` in `val (x, y) = 2` remains `(x, y)`, not a pair of `x` and `y` split over two vals, and `List[_]` is actually represented as a type application to `Type.Placeholder`, not as a `Type.Existential`. Depending on the amount of desugarings performed by your host, this aspect of scala.meta might be either trivial or extremely challenging.

  1. scala.meta attempts to unify as much as possible in order to reduce the surface of the API visible to the users. This overarching design theme manifests itself in trees in two different ways:

    1. In Scala 2.10 and 2.11, reflection features three core concepts: trees (syntax), symbols (unique identifiers of definitions) and types (semantic descriptors of trees and symbols). scala.meta unifies all those into just trees. Syntax is represented by trees, symbols are represented by names that are part of definition trees (this neatly expresses the fact that one shouldn't be able to refer to anonymous definitions such as, say, unnamed parameters of functions), and types are represented by their syntax. This unification means that APIs that traditionally live in symbols and types end up on corresponding tree nodes (e.g. `Type.<:<` or `Member.overrides`). Another consequence is that this saves us the redundancies of having duplications like `Method` vs `MethodSymbol`, `ExistentialTypeTree` vs `ExistentialType`, etc. Finally, there's a benefit of having syntax fully describe semantics - e.g. in scala.meta there are no `MethodTypes` or `PolyTypes`, which don't intuitively map onto what the users see in their programs.

    1. Again, if we look into existing reflection facilities of Scala, we'll observe that trees begin their lives naked (just syntax) and then get attributed by the typechecker (i.e. have their `var tpe: Type` and `var symbol: Symbol` fields assigned, typically in place). Semantic operations are only available for attributed trees, and there are some operations that are only available on unattributed trees, which means that the users need to be aware of the distinction. scala.meta unifies these concepts, exposing semantic methods like `Type.<:<` or `Scope.members` that take care of attribution transparently from the user.

  1. Finally, trees are aware of their context. First of all, there's the `Tree.parent` method that can go up the tree structure if the given tree is a part of a bigger tree (as a host implementor, you don't need to worry about maintaining `parent` at all - it is maintained automatically by scala.meta's infrastructure: when a tree is inserted into another tree, it is cloned and gets its `parent` updated accordingly). Also, there's the notion of hygiene (not yet fully implemented) that postulates that trees should generally remember the lexical context of their creation site and respect that context even when they are put into parent that comes from a different context. At this point, the only thing that you need to do to support hygiene is to set denotations in the trees that you return to users (see the "Semantics" section for more information about denotations).

### Immutability

One of the main design goals of scala.meta is to provide a purely functional API to metaprogramming.

Native scala.meta services (parsing, quasiquotes - essentially, everything syntactic) either don't have mutable state at all or have it localized to areas that can't affect publicly observable behavior. The same level of robustness is expected from hosts, i.e. all semantic operations provided by hosts must be thread-safe.

### Semantics

In order to implement semantic operations on scala.meta trees (e.g. resolving references, computing supertypes, etc), you will need to correlate them with your native metaprogramming artifacts such as e.g. scala.reflect symbols or types. To accommodate this need, we provide three facilities to store semantic information in scala.meta trees: 1) denotations, 2) typings, 3) expansions.

  1. Denotations are exclusive to names (i.e. to trees that inherit from `Name`: Term.Name, Type.Name, Ctor.Name, Name.Anonymous and Name.Indeterminate) and represent definitions that are referenced by those names. More concretely, every name tree has a `private[meta]` field: `val denot: Denotation`. A `Denotation` consists of a prefix and one or more symbols. A prefix is a type of a term from which a name is selected (`Prefix.Type`) or nothing in case when a name is local to a block (`Prefix.Zero`). A symbol is a unique identifier of a definition referenced by a name (check out the [sources](/scalameta/src/main/scala/scala/meta/internal/semantic/Denotations.scala) to learn more about the structure of symbols). There can be `Denotation.Zero` that stands for an unknown denotation, `Denotation.Single` to express unambiguously resolved references and `Denotation.Multi` for imports and overloaded methods.

        In the example below, we can see a tree that represents List[Int], with denotations filled in correctly. The numbers in square brackets next to name trees refer to denotations that are printed below, with the parts before :: standing for prefixes and the parts after :: standing for symbols (dots in fully-qualified names are term selections and hashes are type selections).

        ```
        scala> t"List[Int]".show[Semantics]
        res0: String =
        Type.Apply(Type.Name("List")[1], List(Type.Name("Int")[2]))
        [1] Type.Singleton(Term.Name("package")[3])::scala.package#List
        [2] Type.Singleton(Term.Name("scala")[4])::scala#Int
        [3] Type.Singleton(Term.Name("scala")[4])::scala.package
        [4] Type.Singleton(Term.Name("_root_")[5])::scala
        [5] 0::_root_
        ```

        While global symbols (i.e. the ones that are visible from other files) are more or less straightforward (you represent them with data structures equivalent to fully-qualified names, with a slight complication for overloaded methods), local symbols require more effort. The tricky thing here is generating unique identifiers for local definitions that symbols refer to and then making sure that you return the same identifier if the same local definition is converted more than once.

  1. Typings are exclusive to terms (i.e. to trees that inherit from `Term`) and provide types for these terms. This piece of information is not mandatory, because it is possible to recompute all typings that have been done during typechecking having denotations and expansions at hand, but you may find it useful to precache type information when converting your native representation of terms to scala.meta trees.

  1. Expansions are also exclusive to terms (i.e. to trees that inherit from `Term`) and specify expanded forms for these terms. Such forms may include, but are not limited to: method applications with fully inferred type arguments and implicit arguments, low-level representations of language features (applications, string interpolations, dynamics, etc), macro expansions and so son.

When processing a semantic request from a user, you may receive a tree that is in one of three states: A) fully attributed (i.e. every name has a filled-in denotation and every term has a filled-in type - this is most likely a consequence of the tree coming completely from your host), B) unattributed (this can happen when a tree is created by the user, e.g. in a quasiquote), C) a mix of A and B.

In scala.reflect, the metaprogramming API behaves differently depending on the state of input trees (with some functionality only working in state A and some functionality failing in incomprehensible ways for B), so the user has to have a detailed understanding of what's going and to possibly patch the their trees to upgrade to status A. In order to address this problem, scala.meta requires hosts to gracefully handle any combination of A, B and C. Your implementation must be able to produce correct results regardless of the state that input trees are in, doing its best to infer missing semantic information taking into account the lexical context where the tree came from.

### Context API

<!-- TODO: explain ordering guarantees for all Seq[T] results both in Host and in all our APIs -->

| Method                                                    | Notes
|-----------------------------------------------------------|-----------------------------------------------------------------
| `def dialect: Dialect`                                    | See [dialects/Dialect.scala](/scalameta/src/main/scala/scala/meta/dialects/Dialect.scala)
| `def desugar(term: Term): Term`                           | Expands a given term into its full form, introducing inferred term and type arguments, calls to magic methods, such as `apply` or `update`, etc. Language features to be supported: implicit conversion/argument inference, type argument inference, apply insertion, empty argument list insertion, assignment desugaring (`_=`, `update`), string interpolation desugaring, for loop desugaring, dynamic desugaring.
| `def tpe(term: Term): Type`                               | Type of a given term.
| `def tpe(param: Term.Param): Type.Arg`                    | Type of a given term parameter, possibly inferred.
| `def defns(ref: Ref): Seq[Member]`                        | Definitions that a given reference refers to. Can return multiple results if a reference resolves to several overloaded members.
| `def owner(member: Member): Scope`                        | This isn't actually a method in `Context`, and it's here only to emphasize a peculiarity of our API. <br/><br/> The reason for that is that scala.meta trees always track their parents, so with a tree in hand it's very easy to navigate its enclosures up until an owning scope.
| `def stats(scope: Scope): Seq[Member]`                    | This isn't actually a method in `Context`, and it's here only to emphasize a peculiarity of our API. <br/><br/> The thing is that trees returned by `def members(tpe: Type): Seq[Member]` must have their contents prepopulated, which makes this method unnecessary. All lists in our API (e.g. the list of statements in a block or a list of declarations in a package or a class) are actually `Seq`'s, which means that prepopulation of contents isn't going to incur prohibitive performance costs.
| `def members(tpe: Type): Seq[Member]`                     | Returns all members defined by a given type. This method should return members that are adjusted to the type arguments and the self type of the provided type. E.g. `t"List".members` should return `Seq(q"def head: A = ...", ...)`, whereas `t"List[Int]".members` should return `Seq(q"def head: Int = ...", ...)`.
| `def isSubType(tpe1: Type, tpe2: Type): Boolean`          | Subtyping check.
| `def lub(tpes: Seq[Type]): Type`                          | Least upper bound.
| `def glb(tpes: Seq[Type]): Type`                          | Greatest lower bound.
| `def parents(tpe: Type): Seq[Type]`                       | Direct supertypes of a given type. If the given type has some type parameters instantiated, then the results of this method should also have corresponding type parameters instantiated.
| `def widen(tpe: Type): Type`                              | If a given type is a singleton type, widen it. Otherwise, return the input type back.
| `def dealias(tpe: Type): Type`                            | If a given type is a type alias or an application thereof, resolve it. Otherwise, return the input type back.
| `def parents(member: Member): Seq[Member]`                | Direct parents (i.e. superclasses or overriddens) of a given member. If the provided member has been obtained using `members` via some prefix or by instantiating some type parameters, then the results of this method should also have corresponding type parameters instantiated.
| `def children(member: Member): Seq[Member]`               | Direct children (i.e. subclasses or overriders) of a given member in the closed world reflected by the host. If the provided member has been obtained using `members` via some prefix by instantiating some type parameters, then the results of this method should also have corresponding type parameters instantiated.
| `def project: Project`                                    | Returns an instance of [scala.meta.projects.Project](/scalameta/src/main/scala/scala/meta/projects/Project.scala) describing the project that's underlying the host. Hosts are advised to strive for compatibility between each other. If the same project is compiled, say, by SBT and then by Intellij IDEA plugin, then it is mandatory for the sequence of urls emitted by `Project.resources` to be the same.

### Error handling

scala.meta expects hosts to signal errors by throwing exceptions of type [scala.meta.SemanticException](/scalameta/src/main/scala/scala/meta/Exceptions.scala). Users of scala.meta might be shielded from these exceptions by an additional error handling layer inside scala.meta, but that shouldn't be a concern for host implementors. At the moment, we don't expose any exception hierarchy, and the only way for the host to elaborate on the details of emitted errors is passing a custom error message. This might change later.

Hosts must expect scala.meta to signal fatal errors by throwing exceptions of type [scala.meta.AbortException](/scalameta/src/main/scala/scala/meta/Exceptions.scala). If a metaprogram that throws such an exception is run within a host, the host might want to handle the situation in a special way. For instance, it would make sense for an aborted macro expansion tp result in a diagnostic message at a given location.