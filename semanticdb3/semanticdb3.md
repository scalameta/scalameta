# SemanticDB Specification, Version 3.0.0

  * [Motivation](#motivation)
  * [Overview](#overview)
  * [Model](#model)
    * [TextDocument](#textdocument)
    * [Range](#range)
    * [Symbol](#symbol)
    * [Identifier](#identifier)
    * [SymbolInformation](#symbolinformation)
    * [Diagnostic](#diagnostic)
    * [Synthetic](#synthetic)
  * [Schemas](#schemas)
    * [Protobuf](#protobuf)
    * [JSON](#json)
    * [SQL](#sql)
  * [Changelog](#changelog)
    * [3.0.0](#300)

## Motivation

Nowadays, there is a clear trend towards standards for communication between developer tools.
Language Server Protocol (LSP) [\[2\]][2], a protocol that connects programming
language implementations and text editors, has gained strong industrial support
and at the time of writing has implementations for over 25 programming languages.
Build Server Protocol (BSP) [\[3\]][3] follows in LSP's tracks with an ambition to
define a protocol for communication between language servers and build tools.

While lots of work in the open-source community has been invested in unifying
user experience (by codifying commonly used operations like go to definition or
find all references), relatively less work went into unifying implementor experience.
For example, at the moment, there exist five different LSP implementations for Scala
[[4][4], [5][5], [6][6], [7][7], [8][8]]. They all implement the same protocol
that works with code, but they all use different data structures to represent that code.

Without a standard way to share information between tools, implementors have
two unpleasant choices. First, they can use compiler internals, which are often
underdocumented and lack compatibility guarantees. Otherwise, they reimplement
compiler internals, which usually leads to duplication of effort and inconsistent UX.
For example, Scala IDE [\[9\]][9] uses Scala compiler internals, which has known
stability problems in interactive mode. To the contrast, IntelliJ [\[10\]][10] has
its own Scala typechecker, which is more stable but is known for spurious red squiggles.

This demonstrates the necessity for portable metaprogramming APIs -
something that we have been working on within Scalameta [\[11\]][11].
In the previous years, we shipped portable syntactic APIs for Scala,
including abstract syntax trees, parsing and prettyprinting [\[12\]][12].
SemanticDB is our take on portable semantic APIs.

## Overview

SemanticDB is a data model for semantic information about programs in Scala and
other languages. SemanticDB decouples production and consumption of semantic information,
establishing documented means for communication between tools.

TODO: Once we upgrade semanticdb-scalac to support SemanticDB v3,
demonstrate a hello world example. Show the original program, a prettyprinted
version of its SemanticDB data, very briefly explain the sections.

TODO: Once we ship metac and metap that support SemanticDB v3, explain that
the SemanticDB data model is materialized as Protocol Buffers [\[13\]][13], JSON [\[14\]][14]
and SQL [\[15\]][15]. Demonstrate how we can use metac to generate `.semanticdb` files
from the hello world example above, show the binary payload, decode the files with metap.

TODO: Explain how these protobuf files can be used in tools like scalafix, metadoc and metals.
Mention that beyond protobuf files, there are many ways to use SemanticDB as an
interchange format, including passing objects around in memory, communication via JSON, etc etc.

## Model

In this section, we describe the SemanticDB data model by going through the
individual sections of the associated protobuf schema. However, in addition to protobuf,
we also support other kinds of schemas, including JSON and SQL.
See [Schemas](#schemas) for more information.

### TextDocument

```protobuf
message TextDocuments {
  repeated TextDocument textDocuments = 1;
}

message TextDocument {
  string model = 255;
  string uri = 1;
  string text = 2;
  string language = 3;
  repeated Identifier identifiers = 4;
  repeated SymbolInformation symbols = 5;
  repeated Diagnostic diagnostics = 6;
  repeated Synthetic synthetics = 7;
}
```

`TextDocument` is the central data structure in the SemanticDB model.
It provides semantic information about a snippet of code written in a programming
language `language`.

SemanticDB payloads must include the descriptor of the data model that was used
to produce them in the `model` field. The following descriptors are supported:

<table>
  <tr>
    <td><b>Descriptor</b></td>
    <td><b>Explanation</b></td>
    <td><b>Data model<b></td>
  </tr>
  <tr>
    <td>&lt;missing&gt;</td>
    <td>Legacy SemanticDB payloads</td>
    <td><a href="https://github.com/scalameta/scalameta/blob/v2.1.5/langmeta/langmeta/shared/src/main/protobuf/semanticdb.proto">semanticdb.proto</a></td>
  </tr>
  <tr>
    <td><code>semanticdb3</code></td>
    <td>Current version of the model</td>
    <td>Described in this document</td>
  </tr>
</table>

Snippets of code referenced in `TextDocument` can be defined
in one of three ways: 1) via a URI [\[16\]][16] provided in `uri`,
2) via a string provided in `text`, 3) via a combination of both.

Semantic information is stored in so called sections - repeated fields within
the message definition - as described below. These sections are optional, which
means that documents providing only part of semantic information for the corresponding snippet
(or no semantic information at all) are completely legal.

### Range

```protobuf
message Range {
  int32 start_line = 1;
  int32 start_character = 2;
  int32 end_line = 3;
  int32 end_character = 4;
}
```

`Range` in SemanticDB directly corresponds to `Range` in LSP [\[2\]][2].
It represents a range between start and end points in a document. Both points
are represented by zero-based line and zero-based character offsets.
The start point is inclusive, while the end point is exclusive.

### Symbol

Symbols are tokens that are used to correlate identifiers and definitions.
In the SemanticDB model, symbols are represented as strings.
At the moment, the symbol format is defined by the needs of the Scala implementations.
In the future, we are planning to pay more attention to other languages,
and we may decide to expand the format to accommodate namespacing rules not present in Scala.

**Global symbols**. Correspond to a definition that can be referenced outside
the compilation unit where the definition is defined.
Global symbol format is a concatenation of signatures of the owner chain
of the corresponding global definition, where:
  * The owner chain of a definition is a list of its enclosing definitions
    starting with the outermost one, with the outermost definition being
    either `_empty_` (the special empty package [\[17\]][17]) or
    `_root_` (the special root package [\[18\]][18]). For example,
    for the standard `Int` class, the owner chain is `[_root_, scala, Int]`.
  * The signature of a definition is:
    * For a method, concatenation of its name, its JVM method descriptor [\[19\]][19]
      and a dot (`.`). The JVM method descriptor is used to distinguish
      overloaded methods as mandated by the Scala Language Specification [20].
      For any other term definition (package, object, val or var),
      concatenation of its name and a dot (`.`).
    * For a type definition (class, type alias or type member),
      concatentation of its name and a pound sign (`#`).

For example, the standard `Int` class must be modelled by a global symbol `_root_.scala.Int#`.

**Local symbols**. Correspond to a definition that isn't global (see above).
Local symbol format is deliberately unspecified except for two restrictions:
  * Local symbols must start with `local`, so that they can be easily
    distinguished from global symbols.
  * Local symbols must be unique within the underlying document.

For example, `x` in `def identity[T](x: T): T` may be modelled by local symbols
`local0`, `local_x`, `local_identity_x`, as long as these names are unique within
the underlying document. The same logic applies to the type parameter `T`,
which is also a local definition.

**Placeholder symbols**. Are used to model original snippets of code in [Synthetics](#synthetic).
Must not be used outside `Synthetic.text` documents. Placeholder symbols are
always equal to an asterisk (`*`).

### Identifier

```protobuf
message Identifier {
  Range range = 1;
  string symbol = 2;
  int64 role = 3;
}
```

"Identifiers" is a section of a [TextDocument](#textdocument) that represents
the results of name resolution for identifiers in the underlying snippet of code.

`Identifier` refers to a [Range](#range) in the code and has a symbol as explained
in [Symbol](#symbol). `role` is an enumeration that describes the semantic role
that the identifier performs in the snippet of code. Like many other enumerations
in SemanticDB, this one is usecase-driven and will likely be updated in the future.

<table>
  <tr>
    <td><b>Value</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>1 << 1</code></td>
    <td>Reference</td>
  </tr>
  <tr>
    <td><code>1 << 2</code></td>
    <td>Definition</td>
  </tr>
</table>

### SymbolInformation

```protobuf
message SymbolInformation {
  string symbol = 1;
  int32 kind = 2;
  int64 properties = 3;
  string name = 4;
  Range range = 5;
  TextDocument signature = 6;
  repeated string members = 7;
}
```

"Symbols" is a section of a [TextDocument](#textdocument) that stores information
about [Symbols](#symbol) that are defined in the underlying snippet of code.
In a sense, this section is analogous to symbol tables [\[21\]][21] in compiler.

`Definition` contains assorted metadata for a `symbol`, as explained below.
At the moment, the supported metadata is usecase-driven and is not supposed to
be comprehensive or language-agnostic. In the future, we may add support for
more metadata, for example information about overriding, documentation strings
or features from other languages.

`kind`. Enumeration that defines the kind of the symbol:
<table>
  <tr>
    <td width="100px"><b>Value</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>1</code></td>
    <td>Value.</td>
  </tr>
  <tr>
    <td><code>2</code></td>
    <td>Variable.</td>
  </tr>
  <tr>
    <td><code>3</code></td>
    <td>Method.</td>
  </tr>
  <tr>
    <td><code>4</code></td>
    <td>Primary constructor.</td>
  </tr>
  <tr>
    <td><code>5</code></td>
    <td>Auxiliary constructor.</td>
  </tr>
  <tr>
    <td><code>6</code></td>
    <td>Macro.</td>
  </tr>
  <tr>
    <td><code>7</code></td>
    <td>Abstract type or a type alias.</td>
  </tr>
  <tr>
    <td><code>8</code></td>
    <td>Parameter.</td>
  </tr>
  <tr>
    <td><code>9</code></td>
    <td>Type parameter.</td>
  </tr>
  <tr>
    <td><code>10</code></td>
    <td>Object.</td>
  </tr>
  <tr>
    <td><code>11</code></td>
    <td>Package.</td>
  </tr>
  <tr>
    <td><code>12</code></td>
    <td>Package object.</td>
  </tr>
  <tr>
    <td><code>13</code></td>
    <td>Class.</td>
  </tr>
  <tr>
    <td><code>14</code></td>
    <td>Trait.</td>
  </tr>
</table>

`properties`. Bitmask of miscellaneous bits of metadata:

<table>
  <tr>
    <td width="100px"><b>Value</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>1 << 1</code></td>
    <td>Has a <code>private</code> modifier?</td>
  </tr>
  <tr>
    <td><code>1 << 2</code></td>
    <td>Has a <code>protected</code> modifier?</td>
  </tr>
  <tr>
    <td><code>1 << 3</code></td>
    <td>Has an <code>abstract</code> modifier, or is effectively abstract,
    i.e. is an abstract value, variable, method or type?</td>
  </tr>
  <tr>
    <td><code>1 << 4</code></td>
    <td>Has a <code>final</code> modifier, or is effectively final,
    i.e. is an object or a package object?</td>
  </tr>
  <tr>
    <td><code>1 << 5</code></td>
    <td>Has a <code>sealed</code> modifier?</td>
  </tr>
  <tr>
    <td><code>1 << 6</code></td>
    <td>Has an <code>implicit</code> modifier?</td>
  </tr>
  <tr>
    <td><code>1 << 7</code></td>
    <td>Has a <code>lazy</code> modifier?</td>
  </tr>
  <tr>
    <td><code>1 << 8</code></td>
    <td>Has a <code>case</code> modifier?</td>
  </tr>
  <tr>
    <td><code>1 << 9</code></td>
    <td>Has a covariant (<code>+</code>) modifier?</td>
  </tr>
  <tr>
    <td><code>1 << 10</code></td>
    <td>Has a contravariant (<code>-</code>) modifier?</td>
  </tr>
</table>

`name`. String that represents the name of the symbol.

`range`. [Range](#range) that represents the extent of the definition of the symbol.

`signature`. [TextDocument](#textdocument) that represents the type signature of the definition.
In this document, `text` contains a string prettyprinted by a producer and various
sections, e.g. [Identifiers](#identifier), contain semantic information associated
with that string. This document does not correspond to any compilation unit and
is created solely for the purposes of storing an attributed snippet of text.

For example, for `def x = 42`, the corresponding signature may be a document with
`text` equal to `Int` and `identifiers` featuring an identifier with `range`
equal to `0:0..0:3`, `symbol` equal `_root_.scala.Int#` and `role` equal to `Reference`.

At the moment, the signature format is unspecified, but we intend to
improve that in the future.

`members`. At the moment, the format and the explanation of this field are unspecified.
We intend to improve that in the future.

### Diagnostic

```protobuf
message Diagnostic {
  enum Severity {
    Unknown = 0;
    Error = 1;
    Warning = 2;
    Information = 3;
    Hint = 4;
  }
  Range range = 1;
  Severity severity = 2;
  string message = 3;
}
```

"Diagnostics" is a section of a [TextDocument](#textdocument) that stores
diagnostic messages produced by compilers, linters and other developer tools.

`Diagnostic` in SemanticDB directly corresponds to `Diagnostic` in LSP [\[2\]][2].
It has a [Range](#range), a severity and an associated message. If the severity
is unknown, it is up to the consumer to interpret diagnostics as error, warning, info or hint.

### Synthetic

```protobuf
message Synthetic {
  Range range = 1;
  TextDocument text = 2;
}
```

"Synthetics" is a section of a [TextDocument](#textdocument) that stores snippets
of code synthesized by compilers, code rewriters and other developer tools.

`range` refers to a [Range](#range) in the original code of the underlying document,
and its value is determined as follows:
  * If the synthetic replaces a snippet of code (e.g. if it represents an
    implicit conversion applied to an expression), then its range must be equal
    to that snippet's range.
  * If the synthetic inserts new code (e.g. if it represents an inferred type argument
    or implicit argument), then its range must be an empty range specifying the insertion point.

`text` is a [TextDocument](#textdocument) that represents a synthetic snippet
of code as follows:
  * Its text contains a string prettyprinted by a producer.
  * Its sections, e.g. [Identifiers](#identifiers), contain semantic information
    associated with that string.
  * An occurrence of a placeholder symbol means that the snippet of code includes
    the fragment of the original code defined by `Synthetic.range`.

Synthetics are unspecified in the Scala Language Specification, so we leave the
synthetic format deliberately unspecified as well. Our experience [\[22\]][22] shows
that reverse engineering Scala synthetics is very hard. We may improve on this
in the future, but this is highly unlikely.

## Schemas

### Protobuf

[semanticdb3.proto][semanticdb3.proto]

### JSON

TODO: Generate a JSON schema from the protobuf schema.

### SQL

TODO: Adapt https://github.com/scalameta/scalameta/pull/1174 to the new
protobuf schema.

## Changelog

### 3.0.0
  * Codified the first specification of SemanticDB.
    Previously (in Scalameta 1.x and 2.x), SemanticDB was loosely specified by
    [an internal protobuf schema][semanticdb.proto] and the reference
    implementation in `semanticdb-scalac`.
  * Changed the package of the protobuf schema to `scala.meta.semanticdb3`.
  * Significantly changed the schema to perform long-awaited cleanups and ensure
    consistency with LSP [\[2\]][2]. Some changes were
    inspired by the design of Index-While-Building in Clang [[23][23], [24][24]].

[semanticdb.proto]: https://github.com/scalameta/scalameta/blob/v2.1.5/langmeta/langmeta/shared/src/main/protobuf/semanticdb.proto
[semanticdb3.proto]: https://github.com/xeno-by/scalameta/tree/topic/semanticdb3/semanticdb3/semanticdb3.proto
[semanticdb3.json]: https://github.com/xeno-by/scalameta/tree/topic/semanticdb3/semanticdb3/semanticdb3.json
[semanticdb3.ddl]: https://github.com/xeno-by/scalameta/tree/topic/semanticdb3/semanticdb3/semanticdb3.ddl
[1]: https://semver.org/
[2]: https://microsoft.github.io/language-server-protocol/
[3]: https://scalacenter.github.io/bsp/
[4]: https://github.com/dragos/dragos-vscode-scala
[5]: http://dotty.epfl.ch/docs/usage/ide-support.html
[6]: http://www.scala-sbt.org/1.x-beta/docs/sbt-server.html
[7]: https://github.com/ensime/ensime-server/pull/1888
[8]: https://github.com/scalameta/language-server
[9]: http://scala-ide.org/
[10]: https://confluence.jetbrains.com/display/SCA/Scala+Plugin+for+IntelliJ+IDEA
[11]: http://scalameta.org/
[12]: http://scalameta.org/tutorial/
[13]: https://developers.google.com/protocol-buffers/
[14]: https://www.json.org/
[15]: https://en.wikipedia.org/wiki/SQL
[16]: http://tools.ietf.org/html/rfc3986
[17]: https://www.scala-lang.org/files/archive/spec/2.12/09-top-level-definitions.html#packagings
[18]: https://www.scala-lang.org/files/archive/spec/2.12/09-top-level-definitions.html#package-references
[19]: https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.3.3
[20]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#class-members
[21]: https://en.wikipedia.org/wiki/Symbol_table
[22]: http://scalamacros.org/paperstalks/2016-02-11-WhatDidWeLearnInScalaMeta.pdf
[23]: https://docs.google.com/document/d/1cH2sTpgSnJZCkZtJl1aY-rzy4uGPcrI-6RrUpdATO2Q/edit
[24]: https://www.youtube.com/watch?v=jGJhnIT-D2M
