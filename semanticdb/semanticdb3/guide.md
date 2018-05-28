# SemanticDB Guide

SemanticDB is a data model for semantic information such as symbols and types
about programs in Scala and other languages. SemanticDB decouples production
and consumption of semantic information, establishing documented means for 
communication between tools.

In this document, we introduce practical aspects of working with SemanticDB.
We describe the tools that can be used to produce SemanticDB payloads, the tools
can be used to consume SemanticDB payloads and useful tips & tricks for working
with SemanticDB. If you're looking for a comprehensive reference of SemanticDB
features, check out [the specification](semanticdb3.md).

* [Install](#install)
* [SemanticDB example](#semanticdb-example)
* [What is SemanticDB good for?](#what-is-semanticdb-good-for)
* [Producing SemanticDB](#producing-semanticdb)
  * [Scalac compiler plugin](#scalac-compiler-plugin)
  * [Metac](#metac)
  * [sbt](#sbt)
  * [Metacp](#metacp)
* [Consuming SemanticDB](#consuming-semanticdb)
  * [Scalameta](#scalameta)
  * [Protoc](#protoc)
  * [Metap](#metap)

## Install

This guide covers several SemanticDB-based command-line tools: `metac`, `metacp`, `metap`.
To install these tools locally:
1. install the `coursier` command-line tool by following the [instructions here](https://github.com/coursier/coursier/#command-line).
Make sure you are using the latest coursier version (1.1.0-M4 or newer).
2. add the following aliases to your `~/.zshrc` or `~/.bashrc`:

```bash
alias metac="coursier launch org.scalameta:metac_2.11:3.7.4 -- -cp $(coursier fetch -p org.scala-lang:scala-library:2.11.12)"
alias metacp="coursier launch org.scalameta:metacp_2.11:3.7.4 --"
alias metap="coursier launch org.scalameta:metap_2.11:3.7.4 --"
```

(Optional) Instead of running `metap` on the JVM, build a native binary of on macOS or Linux:

1. setup the [environment for Scala Native](http://www.scala-native.org/en/latest/user/setup.html#installing-clang-and-runtime-dependencies)
2. link a native `metap` binary

```bash
coursier bootstrap org.scalameta:metap_native0.3_2.11:3.7.4 -o metap -f --native --main scala.meta.cli.Metap
```

## SemanticDB example

Let's generate SemanticDB for a simple Scala program. (At the moment,
our SemanticDB producers provide full Scala support and partial Java support.
Theoretically, [the SemanticDB protobuf schema](semanticdb3.proto)
can accommodate other languages as well, but we haven't attempted to do that yet).

```scala
object Test {
  def main(args: Array[String]): Unit = {
    println("hello world")
  }
}
```

In order to obtain a SemanticDB corresponding to this program, let's use
the Metac command-line tool. For instructions on setting up the `metac`
shell command and more information on other tools that can produce
SemanticDB, [see below](#producing-semanticdb).

```
$ metac Test.scala
```

`metac` is a thin wrapper over the Scala compiler. It supports the same
command-line arguments as `scalac` supports, but instead of generating .class
files it generates .semanticdb files. Newer versions of Metac may also generate
an accompanying .semanticidx file, but we won't be discussing it in this
document.

```
$ tree
.
├── META-INF
│   └── semanticdb
│       └── Test.scala.semanticdb
└── Test.scala
```

If we take a look inside Test.scala.semanticdb, we'll see a weird mix of
legible-looking text and special characters. That's because .semanticdb
files store protobuf payloads.

```
$ xxd META-INF/semanticdb/Test.scala.semanticdb
00000000: 0a96 0508 0312 0a54 6573 742e 7363 616c  .......Test.scal
00000010: 611a 596f 626a 6563 7420 5465 7374 207b  a.Yobject Test {
00000020: 0a20 2064 6566 206d 6169 6e28 6172 6773  .  def main(args
00000030: 3a20 4172 7261 795b 5374 7269 6e67 5d29  : Array[String])
00000040: 3a20 556e 6974 203d 207b 0a20 2020 2070  : Unit = {.    p
00000050: 7269 6e74 6c6e 2822 6865 6c6c 6f20 776f  rintln("hello wo
00000060: 726c 6422 290a 2020 7d0a 7d0a 2a3f 0a0d  rld").  }.}.*?..
00000070: 5f65 6d70 7479 5f2e 5465 7374 2e18 0a20  _empty_.Test...
00000080: 082a 0454 6573 745a 1308 0112 0f12 0d5f  .*.TestZ......._
...
```

In order to make sense of .semanticdb files, we can use the Metap
command-line tool. For instructions on setting up the `metap`
shell command and more information on other tools that can consume
SemanticDB, [see below](#consuming-semanticdb).

```
$ metap .
Test.scala
----------

Summary:
Schema => SemanticDB v3
Uri => Test.scala
Text => non-empty
Language => Scala
Symbols => 3 entries
Occurrences => 7 entries

Symbols:
_empty_.Test. => final object Test
_empty_.Test.main(Array). => method main: (args: Array[String]): Unit
  args => _empty_.Test.main(Array).(args)
  Array => scala.Array#
  String => scala.Predef.String#
  Unit => scala.Unit#
_empty_.Test.main(Array).(args) => param args: Array[String]
  Array => scala.Array#
  String => scala.Predef.String#

Occurrences:
[0:7..0:11): Test <= _empty_.Test.
[1:6..1:10): main <= _empty_.Test.main(Array).
[1:11..1:15): args <= _empty_.Test.main(Array).(args)
[1:17..1:22): Array => scala.Array#
[1:23..1:29): String => scala.Predef.String#
[1:33..1:37): Unit => scala.Unit#
[2:4..2:11): println => scala.Predef.println(Any).
```

Metap prettyprints various parts of the SemanticDB payload in correspondence
with [the SemanticDB protobuf schema](semanticdb3.proto). Here are the most
important parts:
  * `Uri` stores the URI of the source file relative to
    the directory where the SemanticDB producer was invoked.
  * `Symbols` contains information about definitions in the source
    file, including modifiers, signatures, etc.

    For example, `_empty_.Test.main(Array). => method main: (args: Array[String]): Unit`
    says that `main` is a method with one parameter of type `Array[String]`.
    Further lines of the printout establish that `Array` in that type
    refers to `scala.Array#`, etc.
  * `Occurrences` contains a list of identifiers from the source file with
    their line/column-based positions and unique identifiers pointing to
    corresponding definitions resolved by the compiler.

    For example, `[2:4..2:11): println => scala.Predef.println(Any).` says that
    the identifier `println` on line 3 (zero-based numbering scheme!) refers
    to the one-argument `println` overload from `scala.Predef`.
  * For more information on various parts of SemanticDB payloads,
    check out [the specification](semanticdb3.md).

## What is SemanticDB good for?

SemanticDB decouples producers and consumers of semantic information about
programs and establishes a rigorous specification of the interchange
format.

Thanks to that, SemanticDB-based tools like [Scalafix](https://github.com/scalacenter/scalafix),
[Metadoc](https://github.com/scalameta/metadoc)
and [Metals](https://github.com/scalameta/metals) don't need to know about compiler
internals and can work with any compiler that supports SemanticDB.
This demonstrably improves developer experience, portability and scalability.
Next-generation semantic tools at Twitter are based on SemanticDB.

For more information about the SemanticDB vision, check out our talks:
  * [Semantic Tooling at Twitter](https://www.youtube.com/watch?v=4yqDFsdKciA)
    (June 2017) by Eugene Burmako & Stu Hood.
  * [SemanticDB for Scala developer tools](https://t.co/Z080F6JxyQ?amp=1)
    (April 2018) by Ólafur Páll Geirsson.
  * [How We Built Tools That Scale to Millions of Lines of Code](https://na.scaladays.org/schedule/how-we-built-tools-that-scale-to-millions-of-lines-of-code)
    (June 2018) by Eugene Burmako.

## Producing SemanticDB

### Scalac compiler plugin

Scalameta includes the `semanticdb-scalac` compiler plugin.
This plugin injects itself immediately after the `typer` phase of the Scala
compiler and then harvests and dumps semantic information from Scalac
in SemanticDB format. The plugin supports the following options that can
be passed through Scalac:

```
Usage: scalac -Xplugin:semanticdb-scalac.jar -Yrangepos [options] [<path> ...]
       scalac -Xplugin:semanticdb-scalac.jar -Yrangepos -P:semanticdb:failures:ignore [<path> ...]
       scalac -Xplugin:semanticdb-scalac.jar -Yrangepos -P:semanticdb:symbols:all [<path> ...]
```

<table>
  <tr>
    <td width="250px">Option</td>
    <td>Value</td>
    <td>Explanation</td>
    <td>Default</td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:sourceroot</code></td>
    <td>Absolute or relative path</td>
    <td>
      Used to relativize source file paths into
      <code>TextDocument.uri</code>.
    </td>
    <td>Current working directory</td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:mode</code></td>
    <td><code>fat | slim</code></td>
    <td>
      Specifies whether to include source code in
      <code>TextDocument.text</code> (<code>fat</code> for yes,
      <code>slim</code> for no).
    </td>
    <td><code>fat</code></td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:failures</code></td>
    <td><code>error | warning | info | ignore</code></td>
    <td>
      The level at which the Scala compiler should report failures arise during
      SemanticDB generation.
    </td>
    <td><code>warning</code></td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:symbols</code></td>
    <td>Enumeration</td>
    <td>
      Specifies what symbols will appear <code>TextDocument.symbols</code>
      (<code>none</code> for none,
      <code>definitions</code> for just symbols defined in the current program,
      <code>all</code> for symbols either defined or referenced from the current
      program).
    </td>
    <td><code>definitions</code></td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:types</code></td>
    <td>Enumeration</td>
    <td>
      Says whether to save type signatures in
      <code>SymbolInformation.tpe</code> (<code>all</code> for yes,
      <code>none</code> for no).
    </td>
    <td><code>all</code></td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:profiling</code></td>
    <td>Enumeration</td>
    <td>
      Controls basic profiling functionality that computes the overhead of
      SemanticDB generation relative to regular compilation time
      (<code>console</code> for dumping profiling information to console,
      <code>off</code> for disabling profiling).
    </td>
    <td><code>off</code></td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:include</code></td>
    <td>Java regex</td>
    <td>Which source files to include in SemanticDB generation?</td>
    <td><code>.*</code></td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:exclude</code></td>
    <td>Java regex</td>
    <td>Which source files to exclude from SemanticDB generation?</td>
    <td><code>^$</code></td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:diagnostics</code></td>
    <td>Enumeration</td>
    <td>
      Says whether to save compiler messages in
      <code>TextDocument.diagnostics</code> (<code>all</code> for yes,
      <code>none</code> for no).
    </td>
    <td><code>all</code></td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:synthetics</code></td>
    <td>Enumeration</td>
    <td>
      Specifies whether to save compiler-generated code in
      <code>TextDocument.synthetics</code> (<code>all</code> for yes,
      <code>none</code> for no).
    </td>
    <td><code>all</code></td>
  </tr>
  <tr>
    <td><code>-P:semanticdb:owners</code></td>
    <td>Enumeration</td>
    <td>
      Specifies whether to save <code>SymbolInformation.owner</code>
      (<code>all</code> for yes, <code>none</code> for no).
    </td>
    <td><code>all</code></td>
  </tr>
</table>

`semanticdb-scalac` can be hooked into Scala builds in a number of ways.
Read below for more information on command-line tools as well as integration
into Scala build tools.

### Metac

Metac is a command-line tool that serves as a drop-in replacement for `scalac`
and produces `*.semanticdb` files instead of `*.class` files. It supports the same
command-line arguments as `scalac` and automatically installs `semanticdb-scalac` 
into the compilation pipeline. With metac, it is not necessary to provide the flags
`-Xplugin:/path/to/semanticdb.jar` and `-Yrangepos`, making it ideal for quick experiments 
with SemanticDB. For an example of using Metac, check out [SemanticDB example](#semanticdb-example).

```
Usage: metac [options] [<path> ...]
       metac -P:semanticdb:synthetics:none [<path> ...]
       metac -P:semanticdb:symbols:all -P:semanticdb:failures:ignore [<path> ...]
```

### sbt

In order to enable `semanticdb-scalac` for your sbt project, add the following
to your build. Note that the compiler plugin requires the `-Yrangepos` compiler
option to be enabled.

```scala
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "3.7.4" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
```

### Metacp


Metacp is a command-line tool that takes a classpath and returns a new classpath of SemanticDB files containing only the `Symbols` section.
Metacp understands classfiles produced by both the Scala and Java compiler.
The compiler plugin `semanticdb-scalac` is not required when using metacp.

Results from processed jar files are cached per machine while directories are never cached.
By convention, cached artifacts are stored in the OS cache directory keyed by the project name "semanticdb", Scalameta version and MD5 fingerprint.
The exact cache location depends on the OS and is computed using the library [directories-jvm](https://github.com/soc/directories-jvm).
For example, on macOS the location for scala-library would be `$HOME/Library/Caches/semanticdb/3.7.4/scala-library-MD5_FINGERPRINT.jar`.
Advanced command-line options control caching, parallelization and interaction with some quirks of the Scala standard library.

As an example of using Metacp, let's compile Test.scala from
[SemanticDB example](#semanticdb-example) using Scalac and then convert
the resulting classfiles to SemanticDB. Note that newer versions of Metac
may also generate an accompanying .semanticidx file, but we won't be discussing
it in this document.

```
$ scalac Test.scala
<success>

$ tree
.
├── Test$.class
├── Test.class
└── Test.scala

$ metacp .
/var/folders/pg/v06175sd2_qb9jbg7c28xr040000gn/T/semanticdb8798662205463788559

$ tree /var/folders/pg/v06175sd2_qb9jbg7c28xr040000gn/T/semanticdb8798662205463788559
/var/folders/pg/v06175sd2_qb9jbg7c28xr040000gn/T/semanticdb8798662205463788559
└── META-INF
    └── semanticdb
        └── Test.class.semanticdb

$ metap /var/folders/pg/v06175sd2_qb9jbg7c28xr040000gn/T/semanticdb8798662205463788559
Test.class
----------

Summary:
Schema => SemanticDB v3
Uri => Test.class
Text => empty
Language => Scala
Symbols => 5 entries

Symbols:
_empty_. => package _empty_
_empty_.Test. => final object Test.{+2 decls}
  extends AnyRef
_empty_.Test.main(Array). => method main: (args: Array[String]): Unit
  args => _empty_.Test.main(Array).(args)
  Array => scala.Array#
  String => scala.Predef.String#
  Unit => scala.Unit#
_empty_.Test.main(Array).(args) => param args: Array[String]
  Array => scala.Array#
  String => scala.Predef.String#
_root_. => package _root_
```

Note how the resulting SemanticDB payload is missing the `Text` attribute and
the `Occurrences` section. That's because Metacp doesn't know about sources
and only works with classes.

```
Usage: metacp [options] <classpath>
       metacp <classpath>
       metacp --include-scala-library-synthetics <classpath>
       metacp --par --cache-dir <dir> <classpath>
```

<table>
  <tr>
    <td width="140px">Option</td>
    <td>Value</td>
    <td>Explanation</td>
    <td>Default</td>
  </tr>
  <tr>
    <td><code>&lt;classpath&gt;</code></td>
    <td>Java classpath</td>
    <td>
      Specifies classpath to be converted to SemanticDB. Can contain classfiles
      created both by the Scala compiler and the Java compiler. In both cases,
      Metacp produces an adequate representation of the metadata.
    </td>
    <td></td>
  </tr>
  <tr>
    <td><code>--cache-dir</code></td>
    <td>Absolute or relative path</td>
    <td>
      Says where Metacp should cache results of converting jar files.
      In order to speed up conversion, Metacp fingerprints jars and then
      caches their converted counterparts in a configurable directory.
      This only works for jars - class directories are never cached.
    </td>
    <td>
      Platform-specific user-specific cache directory whose name includes
      <code>semanticdb</code> and version number of Metacp.
    </td>
  </tr>
  <tr>
    <td>
      <code>--exclude-scala-library-synthetics</code>,<br/>
      <code>--include-scala-library-synthetics</code>
    </td>
    <td></td>
    <td>
      Specifies whether the output classpath should include a jar that contains
      SemanticDB files corresponding to definitions missing from
      <code>scala-library.jar</code>, e.g. <code>scala.Any</code>,
      <code>scala.AnyRef</code> and others.
    </td>
    <td>
      <code>--exclude-scala-library-synthetics</code>
    </td>
  </tr>
  <tr>
    <td><code>--par</code>,<br/><code>--no-par</code></td>
    <td></td>
    <td>
      Toggles parallel processing. If enabled, classpath entries
      will be converted in parallel according to the strategy chosen
      by parallel collections of the Scala standard library.
      <br/><br/>
      NOTE: Some of our users <a href="https://github.com/scalameta/scalameta/issues/1398">have reported deadlocks</a> supposedly caused
      by enabling <code>--par</code>. Proceed at your own risk.
    </td>
    <td>
      <code>--no-par</code>
    </td>
  </tr>
</table>

## Consuming SemanticDB

### Scalameta

Scalameta includes the `semanticdb3` library that contains
[ScalaPB](https://scalapb.github.io/) bindings to
[the SemanticDB protobuf schema](semanticdb3.proto). Using this library,
one can model SemanticDB entities as Scala case classes and serialize/deserialize
them into bytes and streams. For more information, check out [the Scaladoc](https://static.javadoc.io/org.scalameta/semanticdb3_2.11/3.7.4/index.html#scala.meta.internal.semanticdb3.TextDocuments).

NOTE: At this point, there is no stable public library API for loading SemanticDB
payloads. SemanticDB-based tools are currently responsible for implementing discovery
and de-serialization on their own using internal APIs that are subject to binary and
source breaking changes between releases. However, we recognize this as a usability
problem and are planning to [improve the situation](https://github.com/scalameta/scalameta/issues/1566)
soon.

For examples of developer tools that consume SemanticDB, take a look at:
  * [Scalafix](https://github.com/scalacenter/scalafix/)
  * [Metadoc](https://github.com/scalameta/metadoc)
  * [Metals](https://github.com/scalameta/metals)

### Protoc

The Protocol Compiler tool (`protoc`) can inspect protobuf payloads in
`--decode` (takes a schema) and `--decode_raw` (doesn't need a schema) modes.
For the reference, here's [the SemanticDB protobuf schema](semanticdb3.proto).

```
$ tree
.
├── META-INF
│   └── semanticdb
│       └── Test.scala.semanticdb
└── Test.scala

$ protoc --proto_path <directory with the .proto file>\
--decode scala.meta.internal.semanticdb3.TextDocuments\
semanticdb3.proto < META-INF/semanticdb/Test.scala.semanticdb

documents {
  schema: SEMANTICDB3
  uri: "Test.scala"
  text: "object Test {\n  def main(args: Array[String]): Unit = {\n    println(\"hello world\")\n  }\n}\n"
  symbols {
    symbol: "_empty_.Test."
    kind: OBJECT
    properties: 8
    name: "Test"
    tpe {
      tag: TYPE_REF
      typeRef {
        symbol: "_empty_.Test."
      }
    }
    accessibility {
      tag: PUBLIC
    }
    owner: "_empty_."
    language: SCALA
  }
  ...
}
```

`protoc` was useful for getting things done in the early days of SemanticDB,
but nowadays it's a bit too low-level. In order to provide SemanticDB users
more control over prettyprinting and automate classpath traversal, we developed
Metap.

### Metap

Metap is a command-line tool that takes a list of paths and then prettyprints
all .semanticdb files that it finds in these paths. Advanced options control
prettyprinting format.

For an example of using Metap, check out [SemanticDB example](#semanticdb-example).

```
Usage: metap [options] <classpath>
       metap --proto <classpath>
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
    <td>Java classpath</td>
    <td>
      Supported paths:
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
    <td><code>--pretty</code>,<br/><code>--proto</code></td>
    <td></td>
    <td>
      Specifies prettyprinting format, which can be either <code>--pretty</code>
      (prints the most important parts of the payload in a condensed fashion)
      or <code>--proto</code> (prints the same output as <code>protoc</code>
      would print, <a href="#protoc">see above</a>).
    </td>
    <td>
      <code>--pretty</code>
    </td>
  </tr>
</table>
