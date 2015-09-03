### Host implementor notes

scala.meta provides foundational data structures for metaprogramming defined in [Trees.scala](/scalameta/src/main/scala/scala/meta/Trees.scala) along with several levels of APIs (syntactic and semantic).

While syntactic services are implemented in scala.meta itself, semantic services require external implementations called hosts, because it would be unreasonable for us to, say, implement Scala's type inference or implicit resolution algorithms from scratch. In [semantic/Context.scala](/scalameta/src/main/scala/scala/meta/semantic/Context.scala) and [interactive/Context.scala](/scalameta/src/main/scala/scala/meta/interactive/Context.scala) we have encapsulated a minimalistic API surface that's required from hosts.

Here is some preliminary documentation on the functionality expected from hosts along with certain background information about scala.meta's data structures.

### Trees

scala.meta trees provide comprehensive coverage of syntactic structures that comprise Scala. They are predefined in [Trees.scala](/scalameta/src/main/scala/scala/meta/Trees.scala) in the form of a sealed hierarchy, which means that hosts neither need nor can create custom subclasses of `Tree`.

Hosts are, however, required to create instances of scala.meta trees to be returned from various host APIs, so here we will outline the guidelines that were used to design scala.meta trees in order to allude to expected usage scenarios:

  1. Trees are fully immutable in the sense that they: a) don't contain any observationally mutable fields, b) aren't supposed contain any references to anything that might be mutable. This means that after creation trees can never change and as such can be easily reasoned about. In order to "modify" an existing tree, one is supposed to use either `copy` or one of the tree transformers (e.g. `Tree.transform` or other, more advanced, functionality of the TQL framework). Both of these approaches create a copy of an original tree with fields changed appropriately.

  NB! Since trees can't be mutated in-place, reference equality (`Tree.eq` and `Tree.ne`) should not be used to work with trees. On a related note, standard equality (`Tree.==`) is as helpful as reference equality, because it's deliberately implemented to redirect to `Tree.eq`. Structural comparisons should be performed with quasiquotes, and semantic comparisons should be done with `Tree.=:=` or `Tree.=~=`.

  1. Trees strongly emphasize safety by construction in the sense that they disallow creation of syntactically invalid language constructs (e.g. it's impossible to create trees that represent classes without primary constructors or applications of types to terms). As much validation as possible is pushed to compile time via precise types of tree fields, and some leftovers whose validation was too heavyweight to encode in types are verified at runtime. For you as a host implementor this means that you'll have to be really precise in arguments that you provide to our tree creation facilities. Don't worry though. Even if you make a mistake, it'll manifest itself by a compilation error or a crash immediately upon creation of the malformed tree, not by corrupting some internal state and crashing mysteriously afterwards.

  1. scala.meta strives for fully faithful representation of language concepts in its reflection API. Among other things, this means that our trees comprehensively describe all syntactic sugar defined by Scala spec. For instance, `(x, y)` in `val (x, y) = 2` remains `(x, y)`, not a pair of `x` and `y` split over two vals, and `List[_]` is actually represented as a type application to `Type.Placeholder`, not as a `Type.Existential`. Depending on the amount of desugarings performed by your host, this aspect of scala.meta might be either trivial or extremely challenging.

  1. scala.meta attempts to unify as much as possible in order to reduce the surface of the API visible to the users. This overarching design theme manifests itself in trees in two different ways:

    1. In Scala 2.10 and 2.11, reflection features three core concepts: trees (syntax), symbols (unique identifiers of definitions) and types (semantic descriptors of trees and symbols). scala.meta unifies all those into just trees. Syntax is represented by trees, symbols are represented by names that are part of definition trees (this neatly expresses the fact that one shouldn't be able to refer to anonymous definitions such as, say, unnamed parameters of functions), and types are represented by their syntax. This unification means that APIs that traditionally live in symbols and types end up on corresponding tree nodes (e.g. `Type.<:<` or `Member.overrides`). Another consequence is that this saves us the redundancies of having duplications like `Method` vs `MethodSymbol`, `ExistentialTypeTree` vs `ExistentialType`, etc. Finally, there's a benefit of having syntax fully describe semantics - e.g. in scala.meta there are no `MethodTypes` or `PolyTypes`, which don't intuitively map onto what the users see in their programs.

    1. Again, if we look into existing reflection facilities of Scala, we'll observe that trees begin their lives naked (just syntax) and then get attributed by the typechecker (i.e. have their `var tpe: Type` and `var symbol: Symbol` fields assigned, typically in place). Semantic operations are only available for attributed trees, and there are some operations that are only available on unattributed trees, which means that the users need to be aware of the distinction. scala.meta unifies these concepts, exposing semantic methods like `Type.<:<` or `Scope.members` that take care of attribution transparently from the user.

  1. Finally, trees are aware of their context. First of all, there's the `Tree.parent` method that can go up the tree structure if the given tree is a part of a bigger tree (as a host implementor, you don't need to worry about maintaining `parent` at all - it is maintained automatically by scala.meta's infrastructure: when a tree is inserted into another tree, it is cloned and gets its `parent` updated accordingly).

### Immutability

One of the main design goals of scala.meta is to provide a purely functional API to metaprogramming.

Native scala.meta services (parsing, quasiquotes - essentially, everything syntactic) either don't have mutable state at all or have it localized to areas that can't affect publicly observable behavior. The same level of robustness is expected from hosts, e.g. all semantic operations provided by hosts must be thread-safe.

### Syntax

One of the important goals of scala.meta is providing a representation of Scala that accommodates all syntactic peculiarities without introducing desugarings. As a result, the structure of scala.meta trees might look a bit extravagant and not always obvious if you're used to metaprogramming APIs designed in a different way.

The ultimate way of learning about the shape of scala.meta trees is experimentation with: a) building trees manually or via `"...".parse[...]"`, b) exploring trees via `Tree.show[Syntax]` and `Tree.show[Structure]`:

```
scala> "class C { def x = 2 }".parse[Stat]
res0: scala.meta.Stat = class C { def x = 2 }

scala> res0.show[Syntax]
res1: String = class C { def x = 2 }

scala> res0.show[Structure]
res2: String = Defn.Class(Nil, Type.Name("C"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name("this"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(List(Defn.Def(Nil, Term.Name("x"), Nil, Nil, None, Lit.Int(2))))))
```

scala.meta also supports quasiquotes, e.g. `"class C { def x = 2 }".parse[Stat]` from the above snippet can be equivalently rewritten as `q"class C { def x = 2 }"`. Take a look at [the documentation](/docs/quasiquotes.md) to learn more about different kinds of supported quasiquoting interpolators and rules of unquoting and splicing. Like any complicated feature, quasiquotes  may have bugs. If you notice quasiquotes misbehaving, the most robust way of constructing trees would be using `"...".parse[...]"`.

### Semantics

Another important goal of scala.meta is making typechecking transparent. For the users, the API simply exposes things like hygienic tree comparison, `Term.tpe` or `Ref.defn`, and it's the goal of the underlying infrastructure to magically make things work without having the user to understand and manage internal compiler state.

Of course, at the lowest level there's no magic, and internally scala.meta trees feature three private fields equipped with getters and copy-on-write setters that carry semantic attributes: denotations (`.denot` and `.withDenot`), typings (`.typing` and `.withTyping`) and expansions (`.expansion` and `.withExpansion`). In order to set attributes in bulk, you can use `.withAttrs` that has multiple overloads which call into individual setters.

  1. Denotations are exclusive to names (i.e. to trees that inherit from `Name`: Term.Name, Type.Name, Ctor.Name, Name.Anonymous and Name.Indeterminate) and represent definitions that are referenced by those names.

    There can be `Denotation.Zero` that stands for an unknown denotation, `Denotation.Single` to express unambiguously resolved references and `Denotation.Multi` for imports and overloaded methods. A `Denotation` consists of a prefix and one or more symbols. A prefix is a type of a term from which a name is selected (`Prefix.Type`) or nothing in case when a name is local to a block (`Prefix.Zero`). A symbol is a unique identifier of a definition referenced by a name. Check out the [sources](/scalameta/src/main/scala/scala/meta/internal/semantic/Denotation.scala) to learn more.

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

    While global symbols (i.e. the ones that are visible from other files) are more or less straightforward (you represent them with data structures equivalent to fully-qualified names, with a slight complication for overloaded methods), local symbols require more effort. The tricky thing here is generating unique identifiers for local definitions that symbols refer to and then making sure that you return the same identifier if the same local definition is referred to from different places.

  1. Typings are exclusive to terms and term parameters (i.e. to trees that inherit from `Term` and `Term.Param`) and provide types for these trees. scala.meta does not have a notion of typings for types, definitions or nodes from other syntactic categories, because we haven't found a way for such typings to convey useful information to the users.

    There can be `Typing.Zero` that stands for an unknown typing, `Typing.Nonrecursive` that wraps a plain `Type` assigned to the associated tree and `Typing.Recursive` that is used to express types of modules. Check out the [sources](/scalameta/src/main/scala/scala/meta/internal/semantic/Typing.scala) to learn more.

  1. Expansions are exclusive to terms (i.e. to trees that inherit from `Term`) and specify expanded forms for these terms. Language features that are supported by expansions are: implicit conversion/argument inference, type argument inference, apply insertion, empty argument list insertion, assignment desugaring (`_=`, `update`), string interpolation desugaring, for loop desugaring, `Dynamic` desugaring, macro expansions. scala.meta does not have a notion of expansions for definitions (even though the language specification explicitly talks about canonical desugarings for some definitions), because we consider them to be too low-level to be exposed publicly.

    There can be `Expansion.Zero` that stands for an unknown expansion, `Expansion.Identity` that stands for absence of a desugaring and `Expansion.Desugaring` that wraps a `Term` that represents a desugaring of the associated tree.  Check out the [sources](/scalameta/src/main/scala/scala/meta/internal/semantic/Expansion.scala) to learn more.

### Context API

<!-- TODO: explain ordering guarantees for all Seq[T] results both in Host and in all our APIs -->

| Method                                                    | Notes
|-----------------------------------------------------------|-----------------------------------------------------------------
| `def dialect: Dialect`                                    | See [dialects/package.scala](/tokens/src/main/scala/scala/meta/dialects/package.scala)
| `def domain: Domain`                                      | See [taxonomic/Domain.scala](/scalameta/src/main/scala/scala/meta/taxonomic/Domain.scala)
| `def typecheck(tree: Tree): Tree`                         | Checks wellformedness of the input tree, computes semantic attributes (denotations, typings and expansions) for it and all its subnodes, and returns a copy of the tree with the attributes assigned. See subsequent sections for implementation tips.
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

### Implementing Context APIs

The first two methods of Context don't involve anything too tricky and just provide metadata about your host - the language profile that you're supporting (pick from a list in [dialects/package.scala](/tokens/src/main/scala/scala/meta/dialects/package.scala)) and the environment that you're reflecting (create one or more artifacts from a list in [taxonomic/Artifact.scala](/scalameta/src/main/scala/scala/meta/taxonomic/Artifact.scala) and wrap then in a [taxonomic/Domain.scala](/scalameta/src/main/scala/scala/meta/taxonomic/Domain.scala)).

In order to implement other methods that involve semantic operations on scala.meta trees (e.g. resolving references, computing supertypes, etc), you need to be able to do three things:

  1. Read semantic attributes, i.e. understand denotations, typings and expansions, as described above, and correlate them with your native metaprogramming artifacts such as e.g. scala.reflect symbols or types. Note that scala.meta's representations for such attributes is platform-independent, so you can't expect that attributes on input trees will originate from your host or even from your compilation run or the current JVM instance.

  2. Write semantic attributes, i.e. emit denotations, typings and expansions based on your native metaprogramming artifacts. Again it is worth noting that scala.meta's data structures are platform-independent, which means that you can't sneak your native, platform-dependent data structures in there.

  3. Generate semantic attributes, i.e. typecheck trees that are possibly partially attributed. Your implementation must be able to produce correct results regardless of the state that input trees are in, taking into account: I) pre-existing semantic information (i.e. denotations for names, typings for terms and parameters, and expansions for terms), II) your own scope (e.g. classpath of the underlying context, etc).

### Error handling

scala.meta expects hosts to signal errors by throwing exceptions of type [scala.meta.SemanticException](/scalameta/src/main/scala/scala/meta/Exceptions.scala). Users of scala.meta might be shielded from these exceptions by an additional error handling layer inside scala.meta, but that shouldn't be a concern for host implementors. At the moment, we don't expose any exception hierarchy, and the only way for the host to elaborate on the details of emitted errors is passing a custom error message. This might change later.

Hosts must expect scala.meta to signal fatal errors by throwing exceptions of type [scala.meta.AbortException](/scalameta/src/main/scala/scala/meta/Exceptions.scala). If a metaprogram that throws such an exception is run within a host, the host might want to handle the situation in a special way. For instance, it would make sense for an aborted macro expansion to result in a diagnostic message or UI element at a given location.
