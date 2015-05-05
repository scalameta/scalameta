### scala.meta

[![Join the chat at https://gitter.im/scalameta/scalameta](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scalameta/scalameta?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Scala.meta is based on a principle that metaprogramming should be completely decoupled from compiler internals.
This project is a clean-room implementation of a metaprogramming toolkit for Scala, designed to be simple, robust and portable.
We are striving for scala.meta to become a successor of scala.reflect, the current de facto standard in Scala ecosystem.

Scala.meta is an experiment into a fundamental hypothesis that programs in Scala can be represented, analyzed
and manipulated using their concrete syntax and nothing else.
This document explains our current progress with realizing this hypothesis and shows how our vision translates into code.

### How to download

This project is still in a pre-alpha stage, so it's not supposed to be immediately usable just yet.
Nevertheless, we are regularly publishing snapshot builds to Sonatype, so it is already possible to experiment with scala.meta
as described at [https://github.com/scalameta/example](https://github.com/scalameta/example).

As with any snapshot software, things may crash and burn and APIs may noticeably change overnight,
but if you're brave enough, proceed using the instructions, and you may find hidden treasures!
If you have any comments or suggestions during your journey, let us know at
[https://groups.google.com/forum/#!forum/scalameta](https://groups.google.com/forum/#!forum/scalameta).

### How to use

Scala.meta provides services that can be categorized into two big groups: syntactic APIs and semantic APIs.
The former are only interested in how the code looks like, whereas the latter are also concerned about what the code actually means.
This conceptual difference leads to a significant difference in how these APIs are used.

Syntactic APIs are implemented completely within scala.meta, so their use is as simple as importing the right package
and selecting a syntactic profile, called *dialect*. This enables quasiquotes, as well as functionality for tokenization, parsing and prettyprinting.

```
import scala.meta._
import scala.meta.dialects.Scala211
```

Semantic APIs require a full-blown typechecker to make sense of code, enabling scala.meta to resolve references, list members and so on.
Implementing such a typechecker would be a humongous task, so we piggyback on existing implementations (e.g. scalac or Intellij),
called *hosts*, to obtain necessary information.
This means that, in order to use semantic APIs, you will have to have a host (an instance of scala.meta.semantic.Context) at hand.
Good news is that, when you have a host, you don't need to select a dialect - each host knows the dialect that it works with:.

```
import scala.meta._
implicit val c: scala.meta.semantic.Context = ...
```

At the moment, there exist implementations of compile-time and runtime hosts based on scala.tools.nsc.Global (the standard scalac compiler).
Consult [https://github.com/scalameta/scalahost](https://github.com/scalameta/scalahost) to see how to instantiate and use these hosts.
In the future, we plan to have implementations of hosts for the Intellij Scala plugin and the Dotty compiler.

### Working with syntax

In scala.reflect, our current metaprogramming toolkit, quasiquotes have proven to be an excellent tool
to construct and deconstruct syntax trees, so we're utilizing quasiquotes in scala.meta as well.
A specification can be found in [/docs/quasiquotes.md](/docs/quasiquotes.md).

At the moment, we don't have a reliably working implementation of quasiquotes for scala.meta,
but there exists some rudimentary support for constructing terms and types with `q"..."` and `t"..."`
(don't even try deconstruction in pattern matches - it's not going to work).

```
scala> q"class C { def x = 2 }"
res0: meta.internal.ast.Defn.Class = class C { def x = 2 }
```

In the meanwhile, if our rudimentary quasiquotes end up not working for your scenario,
you will have to assemble and take apart syntax trees by hand.
For that, you might want to enlist the help of good old friends showCode and showRaw from scala.reflect,
which are now generalized and called `show[Code]` and `show[Raw]`:

```
scala> res0.show[Code]
res1: String = class C { def x = 2 }

scala> res0.show[Raw]
res2: String = Defn.Class(Nil, Type.Name("C"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name("this"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(List(Defn.Def(Nil, Term.Name("x"), Nil, Nil, None, Lit.Int(2))))))
```

The full list of nodes that comprise scala.meta syntax trees can be found in [Trees.scala](/scalameta/src/main/scala/scala/meta/Trees.scala),
and it is quite involved, because we have to model the entire wealth of Scala's syntax.
However, most of the classes there are just an implementation detail that will be hidden
once we implement full-fledged support for quasiquotes.

### Tokenization and parsing

Scala.meta features its own parser, heavily based on the one from scalac, but customized
to provide fine-grained control over tokenization and ensure ultimate ease of use.
Functionality of our parser is exposed in two simple methods: `tokens` and `parse`.

```
scala> "class C { def x = 2 }".tokens
res3: meta.Tokens = Tokens(class (0..4),   (5..5), C (6..6),   (7..7), { (8..8),   (9..9), def (10..12),   (13..13), x (14..14),   (15..15), = (16..16),   (17..17), 2 (18..18),   (19..19), } (20..20), EOF (21..20))

scala> "class C { def x = 2 }".parse[Stat]
res4: scala.meta.Stat = class C { def x = 2 }

scala> "class C { def x = 2 }".parse[Term]
scala.meta.ParseException: illegal start of simple expression at class (0..4)
  at scala.meta.ParseException$.apply(Exception.scala:5)
  at scala.meta.syntactic.parsers.Reporter$class.syntaxError(Reporter.scala:12)
  at scala.meta.syntactic.parsers.Reporter$$anon$1.syntaxError(Reporter.scala:17)
  at scala.meta.syntactic.parsers.AbstractParser.simpleExpr(Parsers.scala:1256)
  ...
```

The full list of tokens returned by the tokenizer can be found in
[Token.scala](/scalameta/src/main/scala/scala/meta/syntactic/Token.scala).
The full list of non-terminals that can be used as targets for the parser is provided
as a list of implicit instances of the Parse[T] typeclass
in [syntactic/Api.scala](/scalameta/src/main/scala/scala/meta/syntactic/Api.scala).

As a quick remark, you can tokenize and parse anything that can be converted to
an [Input](/scalameta/src/main/scala/scala/meta/syntactic/Input.scala) by the means of
the [Convert](/foundation/src/main/scala/org/scalameta/convert/Convert.scala) typeclass.
At the moment, it's just strings and files, but you can provide your own instances
that will then seamlessly work with the existing API.

### Working with semantics

Central to the semantic API of scala.meta are the notions of `Name`, `Member` and `Type`.
This area of scala.meta is just freshly implemented and is barely tested, so,
in this section, we will only briefly explain the fundamental notions and their interactions.
The full list of semantic APIs can be found in [semantic/Api.scala](/scalameta/src/main/scala/scala/meta/semantic/Api.scala).

### Names

Everything related to bindings is modeled with names. If a program element introduces a definition
or refers to a definition, that will be represented by a name of some sort. Let's see a quick example
that will illustrate the point.

```
scala> q"class C { def x = 2; def y = new C().x }"
res6: meta.internal.ast.Defn.Class =
class C {
  def x = 2
  def y = new C().x
}

scala> res6.show[Raw]
res7: String = Defn.Class(
  Nil, Type.Name("C"), Nil,
  Ctor.Primary(Nil, Ctor.Ref.Name("this"), Nil),
  Template(
    Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None),
    Some(List(
      Defn.Def(Nil, Term.Name("x"), Nil, Nil, None, Lit.Int(2)),
      Defn.Def(Nil, Term.Name("y"), Nil, Nil, None, Term.Select(Term.New(Template(Nil, List(Term.Apply(Ctor.Ref.Name("C"), Nil)), Term.Param(Nil, Name.Anonymous(), None, None), None)), Term.Name("x")))))))
```

We can see that all definitions, including the class C and methods x and y have names at their core.
Even a self-type definition for the class C, which wasn't specified and was set to a default value,
has a name, albeit an anonymous one.
In the same vein, all references, i.e. a constructor that we're invoking and a method that we're calling
on a constructed object, have names in them.

Now, one of the main goals of the semantic API is to match names of references with their corresponding definitions.
This is done under the covers with the help of a host that users provide when invoking semantic APIs.
Once the semantics of a snippet of code has been established, you can compare names for equality with `==`,
and this represents the way how tree comparison works in scala.meta - trees only compare equal if they mean exactly the same thing.

### Members

Once we have a naked name (represented by `Name`) or a qualified name (represented by subtypes of `Ref`),
we can ask the semantic API to resolve it to a definition that it refers to. If the meaning of a name
can be figured out by scala.meta, we'll get a result represented by `Member`.

```
scala> q"scala.collection.immutable.List".defn
res8: scala.meta.Member.Term = object List extends SeqFactory[List] with Serializable { ... }
```

We can also go back from a member to a name by simply calling `Member.name`.
Unlike `Ref.defn`, this is a conceptually trivial operation - we simply look into the syntax tree of a member,
find its name there and return it.

```
scala> res8.name
res9: scala.meta.Name with scala.meta.Term.Ref = List
```

If you're familiar with the scala.reflect API, you will notice that this looks very similar to how symbols work.
In scala.reflect, when we have a RefTree, i.e. a Tree with a Name, we can inspect its Symbol, and that will provide us with
a representation of the corresponding definition.
Again, in scala.reflect, we can also easily reference the symbol by unquoting it into a tree.

Note, however, that in scala.meta we have just a single concept to represent everything (both names and members are trees),
whereas in scala.reflect there are three different concepts of Name, Tree and Symbol to describe bindings.
We consider the design of scala.meta to be particularly elegant in this regard.

Also, just as one would expect, scala.meta makes it possible to inspect various properties of members.
For instance, let's dig into the List that we obtained and get its member method called apply.

```
scala> res8.defs("apply")
res10: scala.meta.Member.Term = override def apply[A](xs: A*): List[A] = ???
```

Note that in addition to signatures, members also carry their bodies. In fact, a `Member` obtained via `Ref.defn`
and a `Member` obtained by parsing and typechecking corresponding source code are supposed to be the same
because of the overarching principle of scala.meta - everything should be represented and operated on via concrete syntax.
This is going to be somewhat tricky to implement, but with the help of the recent initiative of persisting
typed syntax trees we will get there. In the meanwhile, `Ref.defns` is going to include full bodies only
for members declared in the currently processed compilation run.

### Hygiene

Now let's consider the topic of "if the meaning of a name can be figured out by scala.meta" that we glossed over
in the previous section. In order to resolve references, scala.meta needs to know what names mean, and that's
what is planned to be taken care of with hygiene.

In a nutshell, hygiene is about remembering lexical contexts of syntax trees. This enables metaprograms
to operate with snippets of code and not worry that meaning of names might change if these snippets
are split apart and/or inserted into bigger snippets that come from somewhere else.

While a full-fledged design of hygiene in Scala that works with arbitrary untyped code snippets
remains to be developed, there is a reasonable approximation that we have implemented for the time being.
If a tree comes from a host (e.g. during macro expansion), then all names in that tree are going to be hygienic.
Otherwise, if a tree is created in scala.meta (e.g. via quasiquotes), then the names in that tree will be hygienic
only if the tree can be statically typechecked.

In the examples below, we are going to expose information about hygiene in names defined by simple quasiquotes.
This information is hidden under the covers and is unavailable for reading or writing via the public API,
but `show[Summary]` can shed light on it.

```
scala> t"List[Int]".show[Semantics]
res11: String =
Type.Apply(Type.Name("List")[1], List(Type.Name("Int")[2]))
[1] Type.Singleton(Term.Name("package")[3])::scala.package#List
[2] Type.Singleton(Term.Name("scala")[4])::scala#Int
[3] Type.Singleton(Term.Name("scala")[4])::scala.package
[4] Type.Singleton(Term.Name("_root_")[5])::scala
[5] 0::_root_
```

The first example features a type quasiquote that represents List[Int]. This quasiquote can be statically typechecked,
so its names are hygienic. We can see that the internal bookkeeping data structures associated with names
not only include fully-qualified names, but also prefixes, which are essential to hygiene due to Scala's object-oriented nature.

```
scala> t"List[X]".show[Semantics]
res12: String = Type.Apply(Type.Name("List")[0], List(Type.Name("X")[0]))
```

The second example showcases a type quasiquote that represents an application of an unknown type X to the type constructor List.
We don't know what X is at the point where we write a quasiquote, but later on, when we insert it into a bigger tree,
it will make sense. This pattern is very frequently used in popular Scala macros and is crucial to their modularity.
Unfortunately, this pattern also prevents us from typechecking this quasiquote in advance and, as a result, from populating
its names with hygiene information. Consequently, if you try to dereference one of those names with `Ref.defn`, you will get an error.

A smart implementation of hygiene could analyze List[X] piece-by-piece, trying to resolve every name in isolation
and remembering as much information as possible about those, but that's much harder to implement than it sounds,
so we will get to this later. We consider hygiene to be a very important part of a metaprogramming API, so getting it right
is very high on our list of priorities.

### Types

Drawing inspiration from the negative experience with scala.reflect,
where types are special data structures that are opaque to quasiquotes and hence hard to analyze and create,
in scala.meta we demystify types by: 1) asserting that all publicly available types can be represented with syntax,
2) noting that most types are nothing but one or several names bunched together, and describing those names is all that's needed
to completely describe semantics of types.

```
scala> t"List[Int]".show[Semantics]
res13: String =
Type.Apply(Type.Name("List")[1], List(Type.Name("Int")[2]))
[1] Type.Singleton(Term.Name("package")[3])::scala.package#List
[2] Type.Singleton(Term.Name("scala")[4])::scala#Int
[3] Type.Singleton(Term.Name("scala")[4])::scala.package
[4] Type.Singleton(Term.Name("_root_")[5])::scala
[5] 0::_root_
```

If we go back to the example with List[Int], we can see that `t"List[Int]"` contains all the information that we would like to know
about the List[Int] type (and, to reiterate, all that information is ultimately stored in names).
So, without further ado of calling c.typecheck or typeOf, like we'd do in scala.reflect,
we can take our type quasiquote and use it like we'd use a type in scala.reflect.

```
scala> t"List[Int]" <:< t"List[Any]"
res14: Boolean = true

scala> t"List[Int]".defs
res15: scala.collection.immutable.Seq[scala.meta.Member.Term] = List(...)

scala> res15.foreach(println)
override def companion: GenericCompanion[List] = ...
def ::[B >: Int](x: B): List[B] = ...
...
```

Two things in this printout are noteworthy: 1) when we go through a list of members of a `Type`, we get see our old friends `Member`s
that were discussed several sections above, 2) type signatures of these members are adjusted accordingly to accommodate
instantiated type parameters and/or prefixes of enclosing classes.

### Putting this all together

Now that we're well-versed in the fundamental concepts of `Name`/`Ref`, `Member` and `Type`, let's take a final example
that showcase the interaction of these concepts within the semantic API of scala.meta.

```
scala> val tpe = t"List"
tpe: meta.internal.ast.Type.Name = List
```

We start with something as simple as just a `List` (note the absense of the `[_]` part that should be familiar
to those of you who've been working with typeOf in scala.reflect). `List` is a simple `Name`, but, as we know,
names carry a wealth of semantic information. With this information, we can, for example, resolve a name to a member.

```
scala> val member = tpe.defn
member: scala.meta.Member = type List[+A] = List[A]
```

The member that we obtain is nothing magical - it's just the syntax of the definition provided in the `scala` package object.
We can inspect this syntax via usual means (currently, we have to use manual pattern matching and/or show[Raw], but soon
we'll implement quasiquotes to fill this role).

```
scala> member.show[Raw]
res18: String = Defn.Type(Nil, Type.Name("List"), List(Type.Param(List(Mod.Covariant()), Type.Name("A"), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Apply(Type.Name("List"), List(Type.Name("A"))))
```

When inspecting the structure of the member, we can see that it has a name. As we recollect, both definition and reference
parts of bindings are represented with names, and now we can make a practical experiment to illustrate this design.
If you can understand how a type can be equal to a name, then you understand the main ideas of scala.meta.

```
// NOTE: in the future, we'll be using quasiquotes here!
scala> import scala.meta.internal.ast._
import scala.meta.internal.ast._

scala> val Defn.Type(_, name, _, _) = member
name: meta.internal.ast.Type.Name = List

scala> tpe == name
res21: Boolean = true
```

Here we observe how scala.meta unifies trees, symbols and types into one coherent concept,
which gives rise to scala.meta's semantic API, a clearly defined and intuitive way of navigating program structure.
All the bookkeeping information that makes this possible is hidden behind names and only names,
which provides a simple and reliable mental model for understanding scala.meta programs.

### Roadmap

  1. Replace manual tree construction/deconstruction via `import scala.meta.internal.ast._` with familiar quasiquote-based API. The `internal` API will either be hidden and discouraged or will go into oblivion completely.

  2. Remember all the details of how underlying programs were written (formatting, comments, etc). After this is implemented, it will become possible to implement precise code rewritings that don't lose any formatting. Also, we will get position information, which will allow to emit targetted warning and error messages.

  3. Transparently and automatically persist typed syntax trees of programs being compiled, so that they can be redistributed via Maven repositories and then obtained by metaprograms on demand.

  4. Develop more hosts. Firstly, we plan to allow writing macros in scala.meta, which means that it will be possible to write metaprograms against syntactic and semantic APIs of scala.meta, and then have the macro engine automatically call those metaprograms and pass them an instance of `scala.meta.semantic.Context`. Secondly, we may want to explore and expose other kinds of hosts (e.g. a host based on an SBT project).

  5. Implement full-fledged support for hygiene. This will make names remember and preserve their meaning regardless of whether the enclosing quasiquotes can or cannot be typechecked statically.

### Summary

Scala.meta is a next-generation metaprogramming toolkit for Scala that focuses on simplicity, robustness and portability.
Its syntactic APIs provide facilities to create and analyze program elements via a uniform interface based on syntax trees.
Its semantic APIs turn syntax trees into graphs with references and definitions connected by names.

We are building scala.meta to supersede scala.reflect, the de facto standard for metaprogramming Scala,
learning from the experience that we gained from designing, implementing and supporting compile-time and runtime reflection
in production releases of Scala 2.10 and 2.11.

At the moment, scala.meta is still in a pre-alpha stage, so it's not supposed to be immediately usable just yet.
However, it is already possible to experiment with our design using nightly builds of scalameta/scalameta and scalameta/scalahost
as described at [https://github.com/scalameta/example](https://github.com/scalameta/example).
If you would like to share your comments, experiences and ideas, you're always welcome on our mailing list at
[https://groups.google.com/forum/#!forum/scalameta](https://groups.google.com/forum/#!forum/scalameta).
