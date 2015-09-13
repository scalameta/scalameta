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

Of course, at the lowest level there's no magic, and internally scala.meta trees feature several private fields equipped with getters and copy-on-write setters that carry semantic attributes: denotations, typings and expansions. This section describes what these attributes stand for and what are their possible values. Subsequent sections explain how you can create attributed trees.

  1. Denotations are exclusive to names (i.e. to trees that inherit from `Name`: Term.Name, Type.Name, Ctor.Name, Name.Anonymous and Name.Indeterminate) and represent definitions that are referenced by those names.

    There can be `Denotation.Zero` that stands for an unknown denotation, `Denotation.Single` to express unambiguously resolved references and `Denotation.Multi` for imports and overloaded methods. A `Denotation` consists of a prefix and one or more symbols. A prefix is a type of a term from which a name is selected (`Prefix.Type`) or nothing in case when a name is local to a block (`Prefix.Zero`). A symbol is a unique identifier of a definition referenced by a name. Check out the [sources](/scalameta/src/main/scala/scala/meta/internal/semantic/Denotation.scala) to learn more.

    While global symbols (i.e. the ones that are visible from other files) are more or less straightforward (you represent them with data structures equivalent to fully-qualified names, with a slight complication for overloaded methods), local symbols require more effort. The tricky thing here is generating unique identifiers for local definitions that symbols refer to and then making sure that you return the same identifier if the same local definition is referred to from different places.

  1. Typings are exclusive to terms and term parameters (i.e. to trees that inherit from `Term` and `Term.Param`) and provide types for these trees. scala.meta does not have a notion of typings for types, definitions or nodes from other syntactic categories, because we haven't found a way for such typings to convey useful information to the users.

    There can be `Typing.Zero` that stands for an unknown typing, `Typing.Nonrecursive` that wraps a plain `Type` assigned to the associated tree and `Typing.Recursive` that is used to express types of modules. Check out the [sources](/scalameta/src/main/scala/scala/meta/internal/semantic/Typing.scala) to learn more.

  1. Expansions are exclusive to terms (i.e. to trees that inherit from `Term`) and specify expanded forms for these terms. Language features that are supported by expansions are: implicit conversion/argument inference, type argument inference, apply insertion, empty argument list insertion, assignment desugaring (`_=`, `update`), string interpolation desugaring, for loop desugaring, `Dynamic` desugaring, macro expansions. scala.meta does not have a notion of expansions for definitions (even though the language specification explicitly talks about canonical desugarings for some definitions), because we consider them to be too low-level to be exposed publicly.

    There can be `Expansion.Zero` that stands for an unknown expansion, `Expansion.Identity` that stands for absence of a desugaring and `Expansion.Desugaring` that wraps a `Term` that represents a desugaring of the associated tree.  Check out the [sources](/scalameta/src/main/scala/scala/meta/internal/semantic/Expansion.scala) to learn more.

In the example below, we can see a tree that represents `List[Int]`, with attributes exposed via `show[Semantics]`. The numbers in square brackets next to name trees refer to denotations that are printed below, with the parts before :: standing for prefixes and the parts after :: standing for symbols (dots in fully-qualified names are term selections and hashes are type selections). The numbers in curly braces next to terms refer to typings that are also printed below. Finally, the numbers in angle brackets next to terms represent expansions and since there are no desugarings involved in this example, they are all identities denoted by empty angle brackets.

```
scala> implicit val mirror = Mirror(Artifact("~/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.7.jar"))
mirror: scala.meta.Mirror = Mirror(Artifact(...))

scala> t"List[Int]".show[Semantics]
res1: String =
Type.Apply(Type.Name("List")[1], Seq(Type.Name("Int")[2]))
[1] {1}::scala.package#List
[2] {2}::scala#Int
[3] {2}::scala.package
[4] {3}::scala
[5] {0}::_root_
{1} Type.Singleton(Term.Name("package")[3]{1}<>)
{2} Type.Singleton(Term.Name("scala")[4]{2}<>)
{3} Type.Singleton(Term.Name("_root_")[5]{3}<>)
```

### Internals

In scala.meta, trees have three fields that are pertinent to typechecking: `privateDenot` (declared only in subclasses of `Name`), `privateTyping` (declared only in subclasses of `Term` and `Term.Param`) and `privateExpansion` (declared only in subclasses of `Term`). You will never be working with these fields directly, always going through getters (`denot`, `typing`, `expansion`) and copy-on-write setters (`withAttrs` to set the former two and `withExpansion` to set the latter one). However, it is necessary to understand the existence of these fields and possible combinations of their state.

The aforementioned three private fields that store semantic attributes give rise to three valid states of scala.meta trees. Users of scala.meta don't need to be concerned about that at all, because you, as a host author, are going to make sure that semantic APIs always work correctly regardless of what state input trees are in:

  1. (U) Unattributed: denot, typing and expansion set to their default values: `Denotation.Zero`, `Typing.Zero`, `Expansion.Zero`. In this state, an AST node is devoid of semantics, and typechecking (see the section about `Context.typecheck` below) is required to compute it. This is the state that the trees are in after being created manually (via `import scala.meta.internal.ast._`) or with quasiquotes (via `q"..."`, `t"..."`, etc).

  2. (PA) Partially attributed: denot and typing are set to non-trivial values, expansion is `Expansion.Identity`. In this state, an AST node knows about its semantics, but is unaware of its potential desugarings. This is the state that the trees are in after being loaded from TASTY, because TASTY doesn't care about original code as written in Scala sources, but only cares about desugared code as seen by the Dotty typechecker.

  3. (FA) Fully attributed: denot, typing and expansion are set to non-trivial values. In this state, no desugarings should be present in trees - desugarings should all be stuffed into `Term.privateExpansion` fields. This is the state that the trees should be when returned from a host to the user.

Here's the exhaustive list of scala.meta APIs that can be used to produce trees:

| API                                 | State    | Notes
|-------------------------------------|----------|------------------------------------------------------
| Manual tree construction            | U        | Using case class factories from `scala.meta.internal.ast._` creates trees with empty attributes.
| Quasiquotes                         | U        | At the moment, quasiquotes desugar into calls to case class factories, so they also create trees without attributes.
| Parsing                             | U        | Scala.meta's parser doesn't do any semantic analysis, so its outputs end up being in their default state - unattributed.
| Deserialization from TASTY          | PA       | TASTY trees have all their names resolved, which translates to correctly filled-in denotations. Having all the names resolved, the TASTY deserializer computes all the necessary typings. However, TASTY doesn't care about original code as written in Scala sources, but only cares about desugared code as seen by the Dotty typechecker, so it is impossible to calculate expansions just from TASTY alone.
| Domain.sources                      | FA or PA | Domains are comprised of artifacts, and every artifact that involves TASTY also has a way to load corresponding sources, so its contents will most likely be fully attributed. If the sources are missing, then the best that we can get is partially attributed trees.
| Semantic APIs                       | FA or PA | A host is typically built on top of a domain, so the state of the results that it provides to the users is exactly the same as the state of the trees that are provided to it by the domain.

Here's the exhaustive list of scala.meta APIs that can be used to transition between states:

| API                                 | Notes
|-------------------------------------|------------------------------------------------------------------------------
| Unquoting                           | U -> U, PA -> PA, FA -> FA<br/><br/> Inserting a tree into a bigger tree (either via using it in another tree's constructor or via unquoting it in a quasiquote), creates a deep clone of the tree. Following the principle of hygiene, this shouldn't change the semantics of the original tree, so its attributes don't change.
| `Tree.copy`                         | U -> U, PA -> U, FA -> U<br/><br/> Explicit calls to `copy` create a deep clone of the input tree. Unlike in the case of unquoting, copy can change some or all subtrees of the input trees, so we reset the attributes of the original tree to be safe. Children of the input tree are left untouched.
| `Context.typecheck`                 | U -> FA, PA -> FA, FA -> FA<br/><br/> Typechecking a tree returns a fully attributed deep clone of the input tree, regardless of the state that the tree was in originally (or produces an error if the tree can't be typechecked). This notion is elaborated in a dedicated section.
| `.withAttrs`                        | U -> PA<br/><br/> withAttrs produces a deep clone of the tree with denot (if applicable) and typing (if applicable) attributes assigned to the specified values, also assigning expansion (if applicable) to `Expansion.Identity`. For consistency, withAttrs is disallowed for PA and FA trees - if it's necessary to change attributes of an existing tree, call `.copy()` first.
| `.withExpansion`                    | PA -> FA<br/><br/> withExpansion deeply clones a partially attributed input tree and assigns the expansion attribute to the provided value. Again, for consistency reasons it's disallowed for U and FA - call withAttrs first, and only then withExpansion. If this scheme of things proves too restrictive, it will be changed.
| `.setTypechecked`                   | PA -> PA, FA -> FA<br/><br/> setTypechecked validates that the tree and all its subtrees are attributed (partially or fully) and then sets the TYPECHECKED flag, which indicates that the tree's state is final, and that it is okay for `Context.typecheck` to skip typechecking for this tree.

### Context APIs

Here's an exhaustive list of methods that belong to scala.meta context APIs. Information on how to implement them can be found in subsequent sections. S means [semantic/Context.scala](/scalameta/src/main/scala/scala/meta/semantic/Context.scala), I means [interactive/Context.scala](/scalameta/src/main/scala/scala/meta/interactive/Context.scala).

| Method                                                    | Context | Notes
|-----------------------------------------------------------|---------|-------------------------------------------------------
| `def dialect: Dialect`                                    | S       | See [dialects/package.scala](/tokens/src/main/scala/scala/meta/dialects/package.scala)
| `def domain: Domain`                                      | S       | See [taxonomic/Domain.scala](/scalameta/src/main/scala/scala/meta/taxonomic/Domain.scala)
| `def typecheck(tree: Tree): Tree`                         | S       | Checks wellformedness of the input tree, computes semantic attributes (denotations, typings and expansions) for it and all its subnodes, and returns a copy of the tree with the attributes assigned. See subsequent sections for implementation advice.
| `def defns(ref: Ref): Seq[Member]`                        | S       | Definitions that a given reference refers to. Can return multiple results if a reference resolves to several overloaded members.
| `def owner(member: Member): Scope`                        | S       | This isn't actually a method in `Context`, and it's here only to emphasize a peculiarity of our API. <br/><br/> The reason for that is that scala.meta trees always track their parents, so with a tree in hand it's very easy to navigate its enclosures up until an owning scope.
| `def stats(scope: Scope): Seq[Member]`                    | S       | This isn't actually a method in `Context`, and it's here only to emphasize a peculiarity of our API. <br/><br/> The thing is that trees returned by `def members(tpe: Type): Seq[Member]` must have their contents prepopulated, which makes this method unnecessary. All lists in our API (e.g. the list of statements in a block or a list of declarations in a package or a class) are actually `Seq`'s, which means that prepopulation of contents isn't going to incur prohibitive performance costs.
| `def members(tpe: Type): Seq[Member]`                     | S       | Returns all members defined by a given type. This method should return members that are adjusted to the type arguments and the self type of the provided type. E.g. `t"List".members` should return `List(q"def head: A = ...", ...)`, whereas `t"List[Int]".members` should return `List(q"def head: Int = ...", ...)`.
| `def supermembers(member: Member): Seq[Member]`           | S       | Direct parents (i.e. superclasses or overriddens) of a given member. If the provided member has been obtained using `members` via some prefix or by instantiating some type parameters, then the results of this method should also have corresponding type parameters instantiated.
| `def submembers(member: Member): Seq[Member]`             | S       | Direct children (i.e. subclasses or overriders) of a given member in the closed world reflected by the host. If the provided member has been obtained using `members` via some prefix by instantiating some type parameters, then the results of this method should also have corresponding type parameters instantiated.
| `def isSubtype(tpe1: Type, tpe2: Type): Boolean`          | S       | Subtyping check.
| `def lub(tpes: Seq[Type]): Type`                          | S       | Least upper bound.
| `def glb(tpes: Seq[Type]): Type`                          | S       | Greatest lower bound.
| `def supertypes(tpe: Type): Seq[Type]`                    | S       | Direct supertypes of a given type. If the given type has some type parameters instantiated, then the results of this method should also have corresponding type parameters instantiated.
| `def widen(tpe: Type): Type`                              | S       | If a given type is a singleton type, widen it. Otherwise, return the input type back.
| `def dealias(tpe: Type): Type`                            | S       | If a given type is a type alias or an application thereof, resolve it. Otherwise, return the input type back.
| `def load(artifacts: Seq[Artifact]): Seq[Artifact]`       | I       | Append given artifacts to the domain underlying the context, typechecking them if necessary.

<!-- TODO: explain ordering guarantees for all Seq[T] results both in Host and in all our APIs -->

### Implementing Context APIs (dialect and domain)

The first two methods of Context don't involve anything too tricky and just provide metadata about your host - the language profile that you're supporting (pick from a list in [dialects/package.scala](/tokens/src/main/scala/scala/meta/dialects/package.scala)) and the environment that you're reflecting (create one or more artifacts from a list in [taxonomic/Artifact.scala](/scalameta/src/main/scala/scala/meta/taxonomic/Artifact.scala) and wrap then in a [taxonomic/Domain.scala](/scalameta/src/main/scala/scala/meta/taxonomic/Domain.scala)).

### Implementing Context APIs (typecheck)

Along with a bidirectional converter from platform-dependent to platform-independent metaprogramming artifacts, a typechecker constitutes the lynchpin of a host. Building on our experience with insufficiently advanced typechecking in the previous version of Scala's metaprogramming platform, we require unprecedented levels of smartness from `Context.typecheck` in scala.meta.

While some metaprogramming APIs require inputs to the typechecker to be in particular state in order for typechecking to operate correctly, in scala.meta, `Context.typecheck` is supposed to operate correctly regardless of the internal state of its input trees. More precisely, input trees and their subtrees can be in arbitrary combinations of U, PA and FA states. This means that the implementation of typecheck is going to be harder than just converting the input tree to your native format and calling into your native typechecker.

In order to implement `Context.typecheck`, you need to be able to do three steps, of which the first is probably going to be the hardest and the fourth is going to be the easiest:

  1. Read semantic attributes, i.e. understand denotations (`Name.denot`), typings (`Term.typing` and `Term.Param.typing`) and expansions (`Term.expansion`), as described above, and correlate them with your native metaprogramming artifacts such as e.g. scala.reflect symbols or types. Note that scala.meta's representations for such attributes is platform-independent, so you can't expect that attributes on input trees will originate from your host or even from your compilation run or the current JVM instance.

  If done correctly, after the first step, you will have a native tree that is equivalent to the input scala.meta tree, with native attributes initialized in accordance with pre-existing scala.meta attributes of the input tree. In scalahost, we call this part a scala.meta -> scala.reflect converter.

  2. Perform typechecking taking into account the underlying domain of your context, i.e. call into your native typechecker with your analogue of lexical scope initialized properly. Depending on the architecture of your metaprogramming system this might be either trivial or significantly hard.

  If done correctly, after the second step, you will have a native tree that has native attributes filled in for itself and all its subtrees, ready to be converted to a platform-independent representation. Alternatively, you will get a native typecheck error and will report it to the user as described below, skipping steps 3 and 4 altogether.

  3. Write semantic attributes, i.e. emit denotations, typings and expansions based on your native metaprogramming artifacts. Again it is worth noting that scala.meta's data structures are platform-independent, which means that you can't sneak your native, platform-dependent data structures in there.

  If done correctly, after the third step, you will have a scala.meta tree that has exactly the same shape as the input tree, but with all semantic attributes set to correct values corresponding to the results of typechecking from the previous step. In scalahost, we call this part a scala.reflect -> scala.meta converter.

  4. Finalize the result, i.e. call setTypechecked on the output of the third step. By doing this, we make sure that subsequent typechecks of this tree are going to be very cheap, which makes it possible for the scala.meta framework as well as the corresponding hosts to not worry about the performance of typechecking.

### Implementing Context APIs (members and types)

In order to implement methods that involve semantic queries on scala.meta trees (e.g. resolving references, computing supertypes, etc), you'll be reusing the infrastructure built for `Context.typecheck`, and the rest should be really easy. For instance, here is how `Context.defns(ref)` is implemented in scalahost (note that steps 1, 3 and 5 trivially follow from the aforementioned infrastructure, and steps 2 and 4 are quite easy):

  1. Typecheck the input tree. This is necessary to accommodate unattributed trees, but in the case if the tree is already attributed, this is effectively going to be a no-op, as explained above.

  2. Look up the underlying denotation in the typechecked tree. Since there's no `Ref.denot`, this requires a pattern match on all possible refs and extracting the denotation manually (e.g. for a `Name`, it's just `Name.denot`; for a `Term.Select` it's a denot of the underlying name, etc).

  3. Obtain the corresponding scala.reflect artifact using the converter from the first step of `Context.typecheck` - in this case, the Symbol that denotes the definition referenced by the input Ref. Things get a bit more complicated if a Ref refers to multiple definitions, e.g. in the case of imports, but the ideas here can be scaled accordingly.

  4. Look up the definition referenced by the obtained symbol. For definitions that are contained in the underlying domain, this is as easy as searching in an index. For definitions without accompanying sources (e.g. JRE classes or Scala classes that don't have TASTY sections), this requires reverse engineering of ScalaAnnotation signatures.

  5. Convert the native definition into a platform-independent representation using the converter from the third step of `Context.typecheck`. Nothing else has to be done in this step, and it yields the final result.

### Implementing Context APIs (load)

In comparison with typechecking, `Context.load` not only makes sure that the inputs are well-typed and populated with semantic attributes, but also adds them to the underlying state of the context, so that future calls to typecheck and load can refer definitions from the inputs. The necessity to integrate with the list of the compilation units or something else that you might have internally in your metaprogramming framework is the only twist in comparison with `Context.typecheck`.

### Error handling and reporting

scala.meta expects hosts to signal errors by throwing exceptions of type [scala.meta.SemanticException](/scalameta/src/main/scala/scala/meta/Exceptions.scala). Users of scala.meta might be shielded from these exceptions by an additional error handling layer inside scala.meta, but that shouldn't be a concern for host implementors. At the moment, we don't expose any exception hierarchy, and the only way for the host to elaborate on the details of emitted errors is passing a custom error message. This might change later.

Hosts must expect scala.meta to signal fatal errors by throwing exceptions of type [scala.meta.AbortException](/scalameta/src/main/scala/scala/meta/Exceptions.scala). If a metaprogram that throws such an exception is run within a host, the host might want to handle the situation in a special way. For instance, it would make sense for an aborted macro expansion to result in a diagnostic message or UI element at a given location.
