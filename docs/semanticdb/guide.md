---
id: guide
title: SemanticDB Guide
sidebar_label: Guide
---

SemanticDB is a data model for semantic information such as symbols and types
about programs in Scala and other languages. SemanticDB decouples production and
consumption of semantic information, establishing documented means for
communication between tools.

In this document, we introduce practical aspects of working with SemanticDB. We
describe the tools that can be used to produce SemanticDB payloads, the tools
can be used to consume SemanticDB payloads and useful tips & tricks for working
with SemanticDB. If you're looking for a comprehensive reference of SemanticDB
features, check out [the specification](specification.html).

## Installation

This guide covers several non-standard command-line tools: `metac` and `metap`.
First, install the `coursier` command-line tool by following the
[instructions here](https://get-coursier.io/docs/cli-installation). Next, use
coursier to install metac and metap.

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.scalameta/scalameta_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.scalameta/scalameta_2.12)

```
cs install metac metap
```

## Example

Let's generate SemanticDB for a simple Scala program. (At the moment, our
SemanticDB producers provide full Scala support and partial Java support.
Theoretically,
[the SemanticDB protobuf schema](https://github.com/scalameta/scalameta/blob/main/semanticdb/semanticdb/semanticdb.proto)
can accommodate other languages as well, but we haven't attempted to do that
yet).

```scala
object Test {
  def main(args: Array[String]): Unit = {
    println("hello world")
  }
}
```

In order to obtain a SemanticDB corresponding to this program, let's use the
Metac command-line tool. For more information on other tools that can produce
SemanticDB, [see below](#producing-semanticdb).

```sh
metac Test.scala
```

`metac` is a thin wrapper over the Scala compiler. It supports the same
command-line arguments as `scalac` supports, but instead of generating .class
files it generates .semanticdb files.

```
$ tree
.
├── META-INF
│   └── semanticdb
│       └── Test.scala.semanticdb
└── Test.scala
```

If we take a look inside Test.scala.semanticdb, we'll see a weird mix of
legible-looking text and special characters. That's because .semanticdb files
store protobuf payloads.

```
$ xxd META-INF/semanticdb/Test.scala.semanticdb
00000000: 0aaa 0408 0412 0a54 6573 742e 7363 616c  .......Test.scal
00000010: 612a 580a 1a5f 656d 7074 795f 2f54 6573  a*X.._empty_/Tes
00000020: 742e 6d61 696e 2829 2e28 6172 6773 2918  t.main().(args).
00000030: 082a 0461 7267 7380 0101 8a01 2e22 2c0a  .*.args......",.
00000040: 2a12 2812 0c73 6361 6c61 2f41 7272 6179  *.(..scala/Array
00000050: 231a 1812 1612 1473 6361 6c61 2f50 7265  #......scala/Pre
00000060: 6465 662e 5374 7269 6e67 232a 530a 0d5f  def.String#*S.._
00000070: 656d 7074 795f 2f54 6573 742e 180a 2008  empty_/Test... .
00000080: 2a04 5465 7374 8001 018a 012f 0a2d 0a00  *.Test...../.-..
00000090: 1211 120f 120d 7363 616c 612f 416e 7952  ......scala/AnyR
000000a0: 6566 2322 160a 145f 656d 7074 795f 2f54  ef#"..._empty_/T
000000b0: 6573 742e 6d61 696e 2829 2e92 0102 3a00  est.main()....:.
000000c0: 2a5c 0a14 5f65 6d70 7479 5f2f 5465 7374  *\.._empty_/Test
...
```

In order to make sense of .semanticdb files, we can use the Metap command-line
tool. For more information on other tools that can consume SemanticDB,
[see below](#consuming-semanticdb).

```
$ metap .
Test.scala
----------

Summary:
Schema => SemanticDB v4
Uri => Test.scala
Text => empty
Language => Scala
Symbols => 3 entries
Occurrences => 7 entries

Symbols:
_empty_/Test. => final object Test extends AnyRef { +1 decls }
_empty_/Test.main(). => method main(args: Array[String]): Unit
_empty_/Test.main().(args) => param args: Array[String]

Occurrences:
[0:7..0:11) <= _empty_/Test.
[1:6..1:10) <= _empty_/Test.main().
[1:11..1:15) <= _empty_/Test.main().(args)
[1:17..1:22) => scala/Array#
[1:23..1:29) => scala/Predef.String#
[1:33..1:37) => scala/Unit#
[2:4..2:11) => scala/Predef.println(+1).
```

Metap prettyprints various parts of the SemanticDB payload in correspondence
with [the SemanticDB specification](specification.html). Here are the most
important parts:

- `Uri` stores the URI of the source file relative to the directory where the
  SemanticDB producer was invoked.
- `Symbols` contains information about definitions in the source file, including
  modifiers, signatures, etc.

  For example,
  `_empty_/Test.main(). => method main: (args: Array[String]): Unit` says that
  `main` is a method with one parameter of type `Array[String]`.

- `Occurrences` contains a list of identifiers from the source file with their
  line/column-based positions and unique identifiers pointing to corresponding
  definitions resolved by the compiler.

  For example, `[2:4..2:11): println => scala/Predef.println(+1).` says that the
  identifier `println` on line 3 (zero-based numbering scheme!) refers to the
  second overload of `println` from `scala/Predef`.

## What is SemanticDB good for?

SemanticDB decouples producers and consumers of semantic information about
programs and establishes [a rigorous specification](specification.html) of the
interchange format.

Thanks to that, SemanticDB-based tools like [Scalafix](#scalafix),
[Metadoc](#metadoc) and [Metals](#metals) don't need to know about compiler
internals and can work with any compiler that supports SemanticDB. This
demonstrably improves developer experience, portability and scalability.
Next-generation semantic tools at Twitter are based on SemanticDB.

For more information about the SemanticDB vision, check out our talks:

- [Semantic Tooling at Twitter](https://www.youtube.com/watch?v=4yqDFsdKciA)
  (June 2017) by Eugene Burmako & Stu Hood.
- [SemanticDB for Scala developer tools](https://t.co/Z080F6JxyQ?amp=1)
  (April 2018) by Ólafur Páll Geirsson.
- [How We Built Tools That Scale to Millions of Lines of Code](https://na.scaladays.org/schedule/how-we-built-tools-that-scale-to-millions-of-lines-of-code)
  (June 2018) by Eugene Burmako.

## Producing SemanticDB

### Scalac compiler plugin

The `semanticdb-scalac` compiler plugin injects itself immediately after the
`typer` phase of the Scala compiler and then harvests and dumps semantic
information from Scalac in SemanticDB format.

```
scalac -Xplugin:path/to.jar -Yrangepos [<pluginOption> ...] [<scalacOption> ...] [<sourceFile> ...]
```

The compiler plugin supports the following options that can be passed through
Scalac in the form of
```
-P:semanticdb:<option>:<value>
```
(in the table below, the "Option" column refers only to the `<option>` part,
and the "Value" column describes the `<value>` component):

<table>
  <tr>
    <td>Option</td>
    <td>Value</td>
    <td>Explanation</td>
    <td>Default</td>
  </tr>
  <tr>
    <td><code>failures</code></td>
    <td>
      <code>error</code>,<br/>
      <code>warning</code>,<br/>
      <code>info</code>,<br/>
      <code>ignore</code></td>
    <td>
      The level at which the Scala compiler should report crashes that may
      happen during SemanticDB generation.
    </td>
    <td><code>warning</code></td>
  </tr>
  <tr>
    <td><code>profiling</code></td>
    <td>
      <code>on</code>,<br/>
      <code>off</code><br/>
    </td>
    <td>
      Controls basic profiling functionality that computes the overhead of
      SemanticDB generation relative to regular compilation time
      (<code>on</code> for dumping profiling information to console,
      <code>off</code> for disabling profiling).
    </td>
    <td><code>off</code></td>
  </tr>
  <tr>
    <td><code>include</code></td>
    <td>Java regex</td>
    <td>Which source files to include in SemanticDB generation?</td>
    <td><code>.*</code></td>
  </tr>
  <tr>
    <td><code>exclude</code></td>
    <td>Java regex</td>
    <td>Which source files to exclude from SemanticDB generation?</td>
    <td><code>^$</code></td>
  </tr>
  <tr>
    <td><code>sourceroot</code></td>
    <td>Absolute or relative path</td>
    <td>
      Used to relativize source file paths into
      <code>TextDocument.uri</code>.
      <br/>
      If the value starts with <code>targetroot:</code>, the remaining part is 
      interpreted as relative to <code>targetroot</code> (see below; must be
      specified first) instead of current working directory.
    </td>
    <td>Current working directory</td>
  </tr>
  <tr>
    <td><code>targetroot</code></td>
    <td>Absolute or relative path</td>
    <td>
      The output directory to produce <code>META-INF/semanticdb/**/*.semanticdb</code>
      files.
    </td>
    <td>
      The compiler output directory, matches the sbt setting key <code>classDirectory</code>
      and scalac command-line option <code>-d</code>.
    </td>
  </tr>
  <tr>
    <td><code>text</code></td>
    <td>
        <code>on</code>,<br/>
        <code>off</code>
    </td>
    <td>
      Specifies whether to save source code in
      <code>TextDocument.text</code> (<code>on</code> for yes,
      <code>off</code> for no).
    </td>
    <td><code>off</code></td>
  </tr>
  <tr>
    <td><code>md5</code></td>
    <td>
        <code>on</code>,<br/>
        <code>off</code>
    </td>
    <td>
      Specifies whether to save a hexadecimal formatted MD5 fingerprint of the source
      file contents in <code>TextDocument.md5</code> (<code>on</code> for yes,
      <code>off</code> for no).
    </td>
    <td><code>on</code></td>
  </tr>
  <tr>
    <td><code>symbols</code></td>
    <td>
        <code>all</code>,<br/>
        <code>local-only</code>,<br/>
        <code>none</code>
    </td>
    <td>
      Specifies which symbol informations to save in
      <code>TextDocument.symbols</code> (<code>all</code> for both local and global symbols,
      <code>local-only</code> for only local symbols and
      <code>none</code> for no symbols).
    </td>
    <td><code>all</code></td>
  </tr>
  <tr>
    <td><code>diagnostics</code></td>
    <td>
        <code>on</code>,<br/>
        <code>off</code>
    </td>
    <td>
      Specifies whether to save compiler messages in
      <code>TextDocument.diagnostics</code> (<code>on</code> for yes,
      <code>off</code> for no).
    </td>
    <td><code>on</code></td>
  </tr>
  <tr>
    <td><code>synthetics</code></td>
    <td>
        <code>on</code>,<br/>
        <code>off</code>
    </td>
    <td>
      Specifies whether to save some of the compiler-synthesized code in
      the currently unspecified <code>TextDocument.synthetics</code>
      section (<code>on</code> for yes, <code>off</code> for no).
    </td>
    <td><code>off</code></td>
  </tr>
</table>

`semanticdb-scalac` can be hooked into Scala builds in a number of ways. Read
below for more information on command-line tools as well as integration into
Scala build tools.

### Metac

Metac is a command-line tool that serves as a drop-in replacement for `scalac`
and produces `*.semanticdb` files instead of `*.class` files. It supports the
same command-line arguments as `scalac`, including the compiler plugin options
[described above](#scalac-compiler-plugin).

```
metac [<pluginOption> ...] [<scalacOption> ...] [<sourceFile> ...]
```

With metac, it is not necessary to provide the flags `-Xplugin:/path/to.jar` and
`-Yrangepos`, which makes it ideal for quick experiments with SemanticDB. For an
example of using Metac, check out [Example](#example).

### sbt

In order to enable `semanticdb-scalac` for your sbt project, add the following
to your build. Note that the compiler plugin requires the `-Yrangepos` compiler
option to be enabled.

```scala
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "@VERSION@" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
```

## Consuming SemanticDB

### Scala bindings

The `semanticdb` library contains [ScalaPB](https://scalapb.github.io/) bindings
to
[the SemanticDB protobuf schema](https://github.com/scalameta/scalameta/blob/main/semanticdb/semanticdb/semanticdb.proto).
Using this library, one can model SemanticDB entities as Scala case classes and
serialize/deserialize them into bytes and streams.

```
libraryDependencies += "org.scalameta" %% "semanticdb" % "@VERSION@"
```

Caveats:

- At the moment, there are no compatibility guarantees for Scala bindings to the
  SemanticDB schema. The current package of the schema
  (`scala.meta.internal.semanticdb`) is considered internal, so we do not
  provide any guarantees about compatibility across different versions of the
  `semanticdb` library. We are planning to
  [improve the situation](https://github.com/scalameta/scalameta/issues/1300) in
  the future.
- At the moment, SemanticDB-based tools are responsible for implementing
  discovery of SemanticDB payloads on their own. For example, the non-trivial
  logic that Metap uses to traverse its inputs and detect SemanticDB files must
  be reproduced by Scalafix, Metadoc and others. We are planning to
  [improve the situation](https://github.com/scalameta/scalameta/issues/1566) in
  the future.

### Metap

Metap is a command-line tool that takes a list of paths and then prettyprints
all .semanticdb files that it finds in these paths. Advanced options control
prettyprinting format.

```
metap [options] <classpath>
```

<table>
  <tr>
    <td width="125px">Option</td>
    <td>Value</td>
    <td>Explanation</td>
    <td width="125px">Default</td>
  </tr>
  <tr>
    <td><code>&lt;classpath&gt;</code></td>
    <td>Pseudo classpath</td>
    <td>
      Supported classpath entries:
      <ul>
        <li>
          .semanticdb files (prettyprinted directly)
        </li>
        <li>
          Directories (traversed recursively, all found .semanticdb files
          prettyprinted)
        </li>
        <li>
          .jar files (traversed recursively, all found .semanticdb files
          uncompressed and prettyprinted)
        </li>
      </ul>
    </td>
    <td></td>
  </tr>
  <tr>
    <td><code>-compact</code>,<br/><code>-detailed</code>,<br/><code>-proto</code></td>
    <td></td>
    <td>
      Specifies prettyprinting format, which can be either <code>-compact</code>
      (prints the most important parts of the payload in a condensed fashion),
      <code>-detailed</code> (more detailed than -compact, but still pretty
      condensed), or <code>-proto</code> (prints the same output as
      <code>protoc</code> would print, <a href="#protoc">see below</a>).
    </td>
    <td>
      <code>-compact</code>
    </td>
  </tr>
</table>

For an example of using Metap, check out [Example](#example).

### Protoc

The Protocol Compiler tool (`protoc`) can inspect protobuf payloads in
`--decode` (takes a schema) and `--decode_raw` (doesn't need a schema) modes.
For the reference, here's
[the SemanticDB protobuf schema](https://github.com/scalameta/scalameta/blob/main/semanticdb/semanticdb/semanticdb.proto).

```
$ tree
.
├── META-INF
│   └── semanticdb
│       └── Test.scala.semanticdb
└── Test.scala

$ protoc --proto_path <directory with the .proto file>\
--decode scala.meta.internal.semanticdb.TextDocuments\
semanticdb.proto < META-INF/semanticdb/Test.scala.semanticdb

documents {
  schema: SEMANTICDB4
  uri: "Test.scala"
  symbols {
    symbol: "_empty_/Test.main().(args)"
    kind: PARAMETER
    name: "args"
    access {
      publicAccess {
      }
    }
    language: SCALA
    signature {
      valueSignature {
        tpe {
          typeRef {
            symbol: "scala/Array#"
            type_arguments {
              typeRef {
                symbol: "scala/Predef.String#"
              }
            }
          }
        }
      }
    }
  }
  symbols {
    symbol: "_empty_/Test."
    kind: OBJECT
    properties: 8
...
```

`protoc` was useful for getting things done in the early days of SemanticDB, but
nowadays it's a bit too low-level. It is recommended to use `metap` instead of
`protoc`.

## SemanticDB-based tools

### Scalafix

[Scalafix](https://github.com/scalacenter/scalafix) is a rewrite and linting
tool for Scala developed at the Scala Center with the goal to help automate
migration between different Scala compiler and library versions.

Scalafix provides syntactic and semantic APIs that tool developers can use to
write custom rewrite and linting rules. Syntactic information is obtained from
[the Scalameta parser](https://github.com/scalameta/scalameta), and semantic
information is loaded from SemanticDB files produced by
[the Scalac compiler plugin](#scalac-compiler-plugin).

Thanks to SemanticDB, Scalafix is:

- **Accessible**: Scalafix enables novices to implement advanced rules without
  learning compiler internals.
- **Portable**: Scalafix is not tied to compiler internals, which means that it
  can seamlessly work with any compiler / compiler version that supports
  [the SemanticDB compiler plugin](#scalac-compiler-plugin).
- **Scalable**: Scalafix does not need a running Scala compiler, so it can
  perform rewrites and lints in parallel. (Unlike compiler plugin-based linters
  that are limited by the single-threaded architecture of Scalac).

### Metadoc

[Metadoc](https://github.com/scalameta/metadoc) is an experiment with SemanticDB
to build online code browser with IDE-like features. Check out
[the demo](http://scalameta.org/metadoc/) for more information.

Metadoc takes Scala sources and corresponding SemanticDB files generated by
[the Scalac compiler plugin](#scalac-compiler-plugin). It then generates a
static site that is possible to serve via GitHub pages, supporting jump to
definition, find usages and search by symbol.

Thanks to SemanticDB, Metadoc is:

- **Cross-platform**: [Scala bindings to SemanticDB](#scala-bindings) are
  cross-compiled to JVM and Scala.js, which means that the site generator and
  the online code browser can reuse the same logic to work with SemanticDB
  payloads.
- **Portable**: Just like Scalafix, Metadoc is not tied to compiler internals,
  which means that it can seamlessly work with any compiler / compiler version
  that supports [the SemanticDB compiler plugin](#scalac-compiler-plugin).

### Metals

[Metals](https://github.com/scalameta/metals) is an experiment to implement a
[language server](https://github.com/Microsoft/language-server-protocol) for
Scala using [Scalameta](https://github.com/scalameta) projects such as Scalafmt,
Scalafix and SemanticDB. Check out
[the presentation](https://geirsson.com/assets/metals/) for more information.

Metals uses SemanticDB to:

- Index project dependencies for intelligent jump to definition and quick
  lookups of project symbols.
- Communicate with the Scala compiler regarding semantic information about the
  opened project.
- Feed semantic information into [Scalafix](#scalafix)-based refactorings.

Thanks to SemanticDB, Metals is:

- **Mostly portable**: Unlike Scalafix and Metadoc, Metals has modules that
  interface directly with compiler internals, but the majority its functionality
  is based on SemanticDB, so it can work with any compiler / compiler version
  that supports [the SemanticDB compiler plugin](#scalac-compiler-plugin).
- **Surprisingly fast**: A well-defined schema for semantic information that can
  come from multiple locations (dependency classpath, uncompiled files, compiled
  files, etc) allows for a robust implementation of indexing, which reliably
  speeds up operations like jump to definition and find usages. We have even
  experimented with
  [a relational index for SemanticDB data](https://github.com/scalameta/metals/pull/94),
  which further improves performance characteristics.
- **Resilient**: Reification of semantic information makes it possible to
  consult results of previous typechecks and accommodate certain edits by simply
  shifting offsets in old SemanticDB snapshots. This technique is surprisingly
  effective for supporting minor edits that result in temporarily invalid code.
