---
id: guide
sidebar_label: Guide
title: Tree Guide
---

A core functionality of Scalameta is syntax trees, which enable you to read,
analyze, transform and generate Scala programs at a level of abstraction. In
this guide, you will learn how to

- parse source code into syntax trees
- construct new syntax trees
- pattern match syntax trees
- traverse syntax trees
- transform syntax trees

## Installation

Add a dependency to Scalameta in your build to get started. Scalameta supports
Scala 2.11, Scala 2.12, Scala 2.13, Scala.js and Scala Native.

### sbt

```scala
// build.sbt
libraryDependencies += "org.scalameta" %% "scalameta" % "@VERSION@"

// For Scala.js, Scala Native
libraryDependencies += "org.scalameta" %%% "scalameta" % "@VERSION@"
```

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.scalameta/scalameta_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.scalameta/scalameta_2.12)

All code examples assume you have the following import

```scala mdoc:silent
import scala.meta._
```

### Ammonite REPL

A great way to experiment with Scalameta is to use the
[Ammonite REPL](http://ammonite.io/#Ammonite-REPL).

```scala
// Ammonite REPL
import $ivy.`org.scalameta::scalameta:@VERSION@`, scala.meta._
```

### ScalaFiddle

You can try out Scalameta online with the
[ScalaFiddle playground](scalafiddle.html).

## What is a syntax tree?

Syntax trees are a representation of source code that makes it easier to
programmatically analyze programs. Scalameta has syntax trees that represent
Scala programs.

![](assets/tree.svg)

Scalameta trees are **lossless**, meaning that they represent Scala programs in
sufficient detail to go from text to trees and vice-versa. Lossless syntax trees are
great for fine-grained analysis of source code, which is useful for a range of
applications including formatting, refactoring, linting and documentation tools

## Parse trees

Scalameta comes with a parser to produce syntax trees from Scala source code.
You can parse trees from a variety of sources into different kinds of tree
nodes.

### From strings

The simplest way to parse source code is from a string. As long as you have
`import scala.meta._` in your scope, you can use the `parse[Source]` extension
method

```scala mdoc:silent
val program = """object Main extends App { print("Hello!") }"""
val tree = program.parse[Source].get
```

Once parsed, you can print the tree back into its original source code

```scala mdoc
println(tree.syntax)
```

The problem with parsing from strings it that error messages don't include a
filename

```scala mdoc
println(
  "object Main {".parse[Source]
)
```

To make error messages more helpful it's recommended to always use virtual files
when possible, as explained below.

### From files

To parse a file into a tree it's recommended to first read the file contents
into a string and then construct a virtual file

```scala mdoc:silent
val path = java.nio.file.Paths.get("docs", "example.scala")
val bytes = java.nio.file.Files.readAllBytes(path)
val text = new String(bytes, "UTF-8")
val input = Input.VirtualFile(path.toString, text)
val exampleTree = input.parse[Source].get
```

```scala mdoc
print(exampleTree.syntax)
```

The difference between `text.parse[Source]` and `input.parse[Source]` is that
the filename appears in error messages for `Input.VirtualFile`.

```scala mdoc
println(
  Input.VirtualFile("example.scala", "object Main {").parse[Source]
)
```

### From expressions

To parse a simple expressions such as `a + b` use `parse[Stat]` The name `Stat`
stands for "statement".

```scala mdoc
println("a + b".parse[Stat].get.structure)
```

If we try to parse an expression with `parse[Source]` we get an error because
`a + b` is not valid at the top-level for Scala programs

```scala mdoc
println("a + b".parse[Source])
```

The same solution can be used to parse other tree nodes such as types

```scala mdoc
println("A with B".parse[Type].get.structure)
```

If we use `parse[Stat]` to parse types we get an error

```scala mdoc
println("A with B".parse[Stat])
```

### From programs with multiple top-level statements

To parse programs with multiple top-level statements such as `build.sbt` files
or Ammonite scripts we use the `Sbt1` dialect. By default, we get an error when
using `parse[Source]`.

```scala mdoc:silent
val buildSbt = """
val core = project
val cli = project.dependsOn(core)
"""
```

```scala mdoc
println(buildSbt.parse[Source])
```

This error happens because vals are not allowed as top-level statements in
normal Scala programs. To fix this problem, wrap the input with `dialects.Sbt1`

```scala mdoc
println(dialects.Sbt1(buildSbt).parse[Source].get.stats)
```

The same solution works for virtual files

```scala mdoc
println(
  dialects.Sbt1(
    Input.VirtualFile("build.sbt", buildSbt)
  ).parse[Source].get.stats
)
```

The difference between `dialects.Sbt1(input)` and `parse[Stat]` is that
`parse[Stat]` does not allow multiple top-level statements

```scala mdoc
println(buildSbt.parse[Stat])
```

Note that `dialects.Sbt1` does not accept programs with package declarations

```scala mdoc
println(
  dialects.Sbt1("package library; object Main").parse[Source]
)
```

## Construct trees

Sometimes we need to dynamically construct syntax trees instead of parsing them
from source code. There are two primary ways to construct trees: normal
constructors and quasiquotes.

### With normal constructors

Normal tree constructors as plain functions

```scala mdoc
println(Term.Apply(Term.Name("function"), List(Term.Name("argument"))))
```

Although normal constructors are verbose, they give most flexibility when
constructing trees.

To learn tree node names you can use `.structure` on existing tree nodes

```scala mdoc
println("function(argument)".parse[Stat].get.structure)
```

The output of structure is safe to copy-paste into programs.

Another good way to learn the structure of trees is
[AST Explorer](http://astexplorer.net/#/gist/ec56167ffafb20cbd8d68f24a37043a9/97da19c8212688ceb232708b67228e3839dadc7c).

### With quasiquotes

Quasiquotes are string interpolators that expand at compile-time into normal
constructor calls

```scala mdoc
println(q"function(argument)".structure)
```

You can write multiline quasiquotes to construct large programs

```scala mdoc
println(
  q"""
  object Example extends App {
    println(42)
  }
  """.structure
)
```

> It's important to keep in mind that quasiquotes expand at compile-time into
> the same program as if you had written normal constructors by hand. This means
> for example that formatting details or comments are not preserved

```scala mdoc
println(q"function  (    argument   ) // comment")
```

Quasiquotes can be composed together like normal string interpolators with
dollar splices `$`

```scala mdoc
val left = q"Left()"
val right = q"Right()"
println(q"$left + $right")
```

A list of trees can be inserted into a quasiquote with double dots `..$`

```scala mdoc
val arguments = List(q"arg1", q"arg2")
println(q"function(..$arguments)")
```

A curried argument argument lists can be inserted into a quasiquotes with triple
dots `...$`

```scala mdoc
val arguments2 = List(q"arg3", q"arg4")
val allArguments = List(arguments, arguments2)
println(q"function(...$allArguments)")
```

A common mistake is to splice an empty type parameter list into type application
nodes . Imagine we have a list of type arguments that happens to be empty

```scala mdoc:silent
val typeArguments = List.empty[Type]
```

If we directly splice the lists into a type application we get a cryptic error
message "invariant failed"

```scala mdoc:crash
q"function[..$typeArguments]()"
```

The quasiquote above is equivalent to calling the normal constructor
`Type.ApplyType(.., typeArguments)`. Scalameta trees perform strict runtime
validation for invariants such as "type application arguments must be
non-empty". To fix this problem, guard the splice against the length of the list

```scala mdoc
println(
  (if (typeArguments.isEmpty) q"function()"
   else q"function[..$typeArguments]()").structure
)
```

To learn more about quasiquotes, consult the
[quasiquote spec](quasiquotes.html).

## Pattern match trees

Use pattern matching to target interesting tree nodes and deconstruct them. A
core design principle of Scalameta trees is that tree pattern matching is the
dual of tree construction. If you know how to construct a tree, you know how to
de-construct it.

### With normal constructors

Normal constructors work in pattern position the same way they work in regular
term position.

```scala mdoc
"function(arg1, arg2)".parse[Term].get match {
  case Term.Apply(function, List(arg1, arg2)) =>
    println("1 " + function)
    println("2 " + arg1)
    println("3 " + arg2)
}
```

Repeated fields are always `List[T]`, so you can safely deconstruct trees with
the `List(arg1, arg2)` syntax or if you prefer the `arg1 :: arg2 :: Nil` syntax.
There is no need to use `Seq(arg1, arg2)` or `arg1 +: arg2 +: Nil`.

### With quasiquotes

Quasiquotes expand at compile-time and work the same way in pattern position as
in term position.

```scala mdoc
Term.Apply(
  Term.Name("function"),
  List(Term.Name("arg1"), Term.Name("arg2"))
) match {
  case q"$function(..$args)" =>
    println("1 " + function)
    println("2 " + args)
}
```

Use triple dollar splices `...$` to extract curried argument lists

```scala mdoc
"function(arg1, arg2)(arg3, arg4)".parse[Term].get match {
  case q"$function(...$args)" =>
    println("1 " + function)
    println("2 " + args)
}
```

> Pattern matching with quasiquotes is generally discouraged because it's easy
> to write patterns that result in unintended match errors.

```scala mdoc:crash
q"final val x = 2" match {
  case q"val x = 2" => // boom!
}
```

To fix this pattern, we specify that the `final` modifier should be ignored
using `$_`

```scala mdoc
q"final val x = 2" match {
  case q"$_ val x = 2" => println("OK")
}
```

## Compare trees for equality

Scalameta trees use reference equality by default, which may result in
surprising behavior. A common mistake is to use `==` between parsed syntax trees
and quasiquotes

```scala mdoc
"true".parse[Term].get == q"true"
```

Comparing trees by `==` is the same as comparing them with `eq`. Even identical
quasiquotes produce different references

```scala mdoc
q"true" == q"true"
```

Equality checks with `==` will only return true when the reference is the same.

```scala mdoc
{ val treeReference = q"true"
  treeReference == treeReference }
```

The idiomatic way to compare trees for structural equality is to use pattern
matching

```scala mdoc
q"true" match { case q"true" => println("YAY!") }
```

If you can't use pattern matching to compare trees by structural equality, you
can use `.structure`

```scala mdoc
q"true".structure == q"true".structure
```

The `.structure` method produces large strings for large programs, which may
become prohibitively slow. The Scalameta contrib module contains a more
efficient `isEqual` helper method to compare trees structurally.

```scala mdoc
import scala.meta.contrib._
q"true".isEqual(q"true")
```

## Traverse trees

Scalameta includes utilities to recursively visit tree nodes for both simple and
advanced use-cases. Simple use-cases have high-level APIs that require minimal
ceremony while advanced use-cases use lower-level APIs that typically involve
more side-effects.

### Simple traversals

Use `.traverse` to visit every tree node and perform a side-effect, similarly to
`.foreach`

```scala mdoc
q"val x = 2".traverse {
  case node =>
    println(s"${node.productPrefix}: $node")
}
```

Use `.collect` to visit every tree node and collect a value instead of
performing a side-effect

```scala mdoc
q"val x = 2".collect {
  case node => node.productPrefix -> node.toString
}
```

The methods `.traverse` and `.collect` don't support customizing the recursion.
For more fine-grained control over which tree nodes to visit implement a custom
`Traverser`.

### Custom traversals

Extend `Traverser` if you need to implement a custom tree traversal

```scala mdoc:silent
val traverser = new Traverser {
  override def apply(tree: Tree): Unit = tree match {
    case Pat.Var(name) =>
      println(s"stop: $name")
    case node =>
      println(s"${node.productPrefix}: $node")
      super.apply(node)
  }
}
```

The `super.apply(node)` call continues the recursion, so in this case we will
recursively visit all nodes except children of `Pat.Var` nodes.

```scala mdoc
traverser(q"val x = 2")
```

There is no `.collect` equivalent for custom traversals. To collect a value,
it's recommended to use `List.newBuilder[T]` for the type you are interested in
and append values inside the `apply` method.

## Transform trees

Scalameta includes utilities to transform trees for simple and advanced
use-cases.

> Transformed trees do not preserve comments and formatting details when
> pretty-printed. Look into [Scalafix](https://scalacenter.github.io/scalafix/)
> if you need to implement fine-grained refactorings that preserve comments and
> formatting details.

### Simple transformations

Use `.transform` to visit every tree node and transform interesting tree nodes.

```scala mdoc
println(
  q"val x = 2".transform { case q"2" => q"42" }
)
```

The contract of `.transform` is that it will recursively visit all tree nodes,
including the transformed trees. Due to this behavior, a common mistake is to
introduce infinite recursion in `.transform`

```scala
q"a + b".transform {
  case name @ Term.Name("b") => q"function($name)"
}.toString
// [error] java.lang.StackOverflowError
// at scala.meta.transversers.Api$XtensionCollectionLikeUI$transformer$2$.apply(Api.scala:10)
// at scala.meta.transversers.Transformer.apply(Transformer.scala:4)
```

The best solution to fix this problem is to implement a custom transformer to
gain fine-grained control over the recursion.

### Custom transformations

Extend `Transformer` if you need to implement a custom tree transformation

```scala mdoc:silent
val transformer = new Transformer {
  override def apply(tree: Tree): Tree = tree match {
    case name @ Term.Name("b") => q"function($name)"
    case node => super.apply(node)
  }
}
```

By avoiding the call to `super.transform` in the first case, we prevent a stack
overflow.

```scala mdoc
println(
  transformer(q"a + b")
)
```
