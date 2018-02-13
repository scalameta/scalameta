# SemanticDB Specification

* [Motivation](#motivation)
* [Data Model](#data-model)
  * [TextDocument](#textdocument)
  * [Language](#language)
  * [Range](#range)
  * [Location](#location)
  * [Symbol](#symbol)
  * [Annotation](#annotation)
  * [Type](#type)
  * [SymbolInformation](#symbolinformation)
  * [SymbolOccurrence](#symboloccurrence)
  * [Diagnostic](#diagnostic)
  * [Synthetic](#synthetic)
* [Data Schemas](#data-schemas)
  * [Protobuf](#protobuf)
* [Languages](#languages)
  * [Scala](#scala)
    * [Language](#scala-language)
    * [Symbol](#scala-symbol)
    * [Annotation](#scala-annotation)
    * [Type](#scala-type)
    * [SymbolInformation](#scala-symbolinformation)
    * [Synthetic](#scala-synthetic)

## Motivation

Nowadays, there is a clear trend towards standards for communication between
developer tools. Language Server Protocol (LSP) [\[2\]][2], a protocol
that connects programming language implementations and text editors, has gained
strong industrial support and at the time of writing has implementations
for many programming languages and editors. Build Server Protocol (BSP)
[\[3\]][3] follows in LSP's tracks with an ambition to define a protocol
for communication between language servers and build tools.

While lots of work in the open-source community has been invested in unifying
user experience (by codifying commonly used operations like go to definition or
find all references), relatively less work went into unifying implementor
experience. For example, at the moment, there exist five different LSP
implementations for Scala [[4][4], [5][5], [6][6], [7][7], [8][8]].
They all implement the same protocol that works with code, but they all use
different data structures to represent that code.

Without a standard way to share information between tools, implementors have
two unpleasant choices. First, they can use compiler internals, which are often
underdocumented and lack compatibility guarantees. Otherwise, they reimplement
compiler internals, which usually leads to duplication of effort and
inconsistent UX. For example, Scala IDE [\[9\]][9] uses Scala compiler internals,
which has known stability problems in interactive mode. To the contrast,
IntelliJ [\[10\]][10] has its own Scala typechecker, which is more stable but
is known for spurious red squiggles.

This demonstrates the necessity for portable metaprogramming APIs -
something that we have been working on within Scalameta [\[11\]][11].
In the previous years, we shipped portable syntactic APIs for Scala,
including abstract syntax trees, parsing and prettyprinting [\[12\]][12].
SemanticDB is our take on portable semantic APIs.

## Data Model

SemanticDB is a data model for semantic information about programs in Scala and
other languages. SemanticDB decouples production and consumption of semantic
information, establishing documented means for communication between tools.

In this section, we describe the SemanticDB data model by going through the
individual sections of the associated Protocol Buffers [\[13\]][13] schema.
In the future, we may also support other kinds of schemas, including
JSON [\[14\]][14] and SQL [\[15\]][15]. See [Data Schemas](#data-schemas)
for more information.

The types in the SemanticDB data model are language-agnostic, but the entities
corresponding to these types may be language-dependent. For example,
[Range](#range) and [Location](#location) mean the same thing in all languages,
while [Symbol](#symbol) format relies on scoping rules that are generally vary
across languages. See [Languages](#languages) for more information about
language-dependent aspects of the specification.

### TextDocument

```protobuf
message TextDocuments {
  repeated TextDocument documents = 1;
}

message TextDocument {
  reserved 4;
  Schema schema = 1;
  string uri = 2;
  string text = 3;
  Language language = 9;
  repeated SymbolOccurrence occurrences = 5;
  repeated SymbolInformation symbols = 6;
  repeated Diagnostic diagnostics = 7;
  repeated Synthetic synthetics = 8;
}
```

`TextDocument` is the central data structure in the SemanticDB model.
It provides semantic information about a snippet of code written in a
[Language](#language).

SemanticDB payloads must include the version of the data model that was used
to produce them in the `schema` field. The following versions are supported:

<table>
  <tr>
    <td><b>Version</b></td>
    <td><b>Explanation</b></td>
    <td><b>Data model<b></td>
  </tr>
  <tr>
    <td><code>LEGACY<code></td>
    <td>Legacy SemanticDB payloads</td>
    <td><a href="https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb2/semanticdb2.proto">semanticdb2.proto</a></td>
  </tr>
  <tr>
    <td><code>SEMANTICDB3</code></td>
    <td>SemanticDB v3</td>
    <td><a href="https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb3/semanticdb3.proto">semanticdb3.proto</a> (described in this document)</td>
  </tr>
</table>

Snippets of code referenced in `TextDocument` can be defined
in one of three ways: 1) via a URI [\[16\]][16] provided in `uri`,
2) via a string provided in `text`, 3) via a combination of both.

Semantic information is stored in so called sections - repeated fields within
the message definition - as described below. These sections are optional, which
means that documents providing only part of semantic information for the
corresponding snippet (or no semantic information at all) are completely legal.

### Language

```protobuf
message Language {
  string name = 1;
}
```

`Language` represents a programming language that defines certain SemanticDB
entities, e.g. [Document](#document) or [Symbol](#symbol).
See [Languages](#languages) for a list of supported programming languages.

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

### Location

```protobuf
message Location {
  string uri = 1;
  Range range = 2;
}
```

`Location` in SemanticDB directly corresponds to `Location` in LSP [\[2\]][2].
It represents a location inside a document, such as a line inside a text file.

### Symbol

Symbols are tokens that are used to correlate references and definitions.
In the SemanticDB model, symbols are represented as strings.

At the moment, symbols are not guaranteed to be globally unique, which means
that there may be limitations for using symbols as keys in maps or databases.
Read below for more information about uniqueness guarantees for different
symbol categories.

**Global symbols**. Correspond to a definition that can be referenced outside
the document where the definition is located.

Global symbol format accommodates scoping rules of the underlying language,
and is therefore language-dependent. For example, the `Int` class in
the Scala standard library must be modelled as `_root_.scala.Int#`.
The `_root_` prefix stands for the root package defined in the Scala Language
Specification [\[18\]][18], and the pound sign (`#`) at the end of the symbol
means that the symbol corresponds to a class as opposed to objects that end
with a dot (`.`). See [Languages](#languages) for more information.

Global symbols must be unique across the universe of documents that
a tool is working with at any given time. For example, if in such a universe,
there exist multiple definitions of `Int` - e.g. coming from multiple different
versions of Scala - then all references to those definitions will have
`SymbolOccurrence.symbol` equal to the same `_root_.scala.Int#` symbol,
and SemanticDB will not be able to provide information to disambiguate
these references.

In the future, we may extend SemanticDB to allow for multiple definitions
that under current rules would correspond to the same global symbol.
In the meanwhile, when global uniqueness is required, tool authors are advised to
accompany global symbols with `SymbolInformation.location`.

**Local symbols**. Correspond to a definition that isn't global (see above).

Local symbol format is language-agnostic and is a concatenation of `local`
and a decimal number. For example, `x` in a Scala method
`def identity[T](x: T): T` may be modelled by local symbols `local0`, `local1`,
etc. The same logic applies to the type parameter `T`, which is also a local
definition.

Local symbols must be unique within the underlying document, but they don't have
to be unique across multiple documents. For example, at the time of writing
`semanticdb-scalac` produces local symbols named `local0`, `local1`, etc,
with the counter resetting to zero for every new document.

In the future, we may extend SemanticDB to make local symbols more unique,
but we haven't yet found a way to do that without sacrificing performance
and payload size. In the meanwhile, when global uniqueness is required,
tool authors are advised to accompany local symbols with `TextDocument.uri`.

**Multi symbols**. Are used to model references to a set of multiple definitions
at once. For example, this is occasionally useful to support corner cases
of Scala, e.g. identifiers in imports that can refer to both a class and
an object with the same name, or references to unresolved overloaded methods.

Multi symbol format is a concatentation of the underlying symbol formats
interspersed with a semicolon (`;`). Within a multi symbol, the underlying
symbols must be ordered lexicographically in ascending order.

For example, a reference to both the `Int` class in the Scala standard library
and its companion object must be modelled by
`_root_.scala.Int#;_root_.scala.Int.`.
Because of the order requirement, `_root_.scala.Int.;_root_.scala.Int#`
is not a valid symbol.

**Placeholder symbols**. Are used to model original snippets of code in
[Synthetics](#synthetic). Must not be used outside `Synthetic.text` documents.
Placeholder symbols are always equal to an asterisk (`*`).

### Annotation

```protobuf
message Annotation {
  Type tpe = 1;
}
```

`Annotation` represents annotations. See [Languages](#languages) for
information on how annotations in supported languages map onto this
data structure.

### Type

```protobuf
message Type {
  enum Tag {
    reserved 2, 3, 4, 5;
    UNKNOWN_TAG = 0;
    TYPE_REF = 1;
    SINGLETON_TYPE = 15;
    STRUCTURAL_TYPE = 6;
    ANNOTATED_TYPE = 7;
    EXISTENTIAL_TYPE = 8;
    TYPE_LAMBDA = 9;
    CLASS_INFO_TYPE = 10;
    METHOD_TYPE = 11;
    BY_NAME_TYPE = 12;
    REPEATED_TYPE = 13;
    TYPE_TYPE = 14;
  }
  reserved 3, 4, 5, 6;
  Tag tag = 1;
  TypeRef typeRef = 2;
  SingletonType singletonType = 16;
  StructuralType structuralType = 7;
  AnnotatedType annotatedType = 8;
  ExistentialType existentialType = 9;
  UniversalType universalType = 10;
  ClassInfoType classInfoType = 11;
  MethodType methodType = 12;
  ByNameType byNameType = 13;
  RepeatedType repeatedType = 14;
  TypeType typeType = 15;
}
```

`Type` represents expression types and signatures of definitions.

The SemanticDB type system is a language-agnostic superset of the type systems
of supported languages - currently modelled after the Scala type system
[\[19\]][19]. This section describes the language-agnostic model, while
[Languages](#languages) elaborates on how language types map onto this model.

```protobuf
message TypeRef {
  Type prefix = 1;
  string symbol = 2;
  repeated Type type_arguments = 3;
}
```

`TypeRef` is bread-and-butter type in SemanticDB. It represents a reference
to a [Symbol](#symbol), possibly parameterized by `type_arguments`. To model
references to nested type definitions, e.g. path-dependent types in Scala,
typerefs include `prefix`.

```protobuf
message SingletonType {
  enum Tag {
    UNKNOWN_TAG = 0;
    SYMBOL = 1;
    THIS = 2;
    SUPER = 3;
    UNIT = 4;
    BOOLEAN = 5;
    BYTE = 6;
    SHORT = 7;
    CHAR = 8;
    INT = 9;
    LONG = 10;
    FLOAT = 11;
    DOUBLE = 12;
    STRING = 13;
    NULL = 14;
  }
  Tag tag = 1;
  Type prefix = 2;
  string symbol = 3;
  int64 primitive = 4;
  string string = 5;
}
```

`SingletonType` represents a singleton type. Singletons may be specified: 1)
via references (by [Symbol](#symbol) accompanied with a `prefix`), 2) via
keywords (by `this` or `super`) or 3) via literals.

```protobuf
message StructuralType {
  repeated Type parents = 1;
  repeated string declarations = 2;
}
```

`StructuralType` represents a structural type specified by its `parents`
and `declarations`. Declarations are modelled as [Symbols](#symbol) whose
metadata must be provided via [SymbolInformation](#symbolinformation).

```protobuf
message AnnotatedType {
  reserved 2;
  repeated Annotation annotations = 3;
  Type tpe = 1;
}
```

`AnnotatedType` represents a type `tpe` annotated by several
[Annotations](#annotation).

```protobuf
message ExistentialType {
  repeated string type_parameters = 2;
  Type tpe = 1;
}
```

`ExistentialType` represents a type `tpe` existentially quantified
over `type_parameters`. Type parameters are modelled as [Symbols](#symbol) whose
metadata must be provided via [SymbolInformation](#symbolinformation).

```protobuf
message UniversalType {
  repeated string type_parameters = 1;
  Type tpe = 2;
}
```

`UniversalType` represents a type `tpe` universally quantified
over `type_parameters`. Type parameters are modelled as [Symbols](#symbol) whose
metadata must be provided via [SymbolInformation](#symbolinformation).

```protobuf
message ClassInfoType {
  repeated string type_parameters = 1;
  repeated Type parents = 2;
  repeated string declarations = 3;
}
```

`ClassInfoType` represents signatures of a class, a trait or the like.
It is a nominal equivalent of `StructuralType`.

```protobuf
message MethodType {
  message ParameterList {
    repeated string symbols = 1;
  }
  repeated string type_parameters = 1;
  repeated ParameterList parameters = 2;
  Type returnType = 3;
}
```

`MethodType` represents a signature of a method, a constructor or the like.
It features `type_parameters`, `parameters` and a `returnType`. Both type
parameters and paramteres are modelled as [Symbols](#symbol) whose
metadata must be provided via [SymbolInformation](#symbolinformation).
Moreover, in order to support multiple parameter lists in Scala methods,
`parameters` is a list of lists.

```protobuf
message ByNameType {
  Type tpe = 1;
}
```

`ByNameType` represents a signature of a by-name parameter.

```protobuf
message RepeatedType {
  Type tpe = 1;
}
```

`RepeatedType` represents a signature of a repeated parameter.

```protobuf
message TypeType {
  repeated string type_parameters = 1;
  Type lower_bound = 2;
  Type upper_bound = 3;
}
```

`TypeType` represents a signature of a type parameter or a type member.
It features `type_parameters` as well as `lower_bound` and `upper_bound`.
Type parameters are modelled as [Symbols](#symbol) whose metadata must be
provided via [SymbolInformation](#symbolinformation).

### SymbolInformation

```protobuf
message SymbolInformation {
  reserved 2, 6;
  string symbol = 1;
  Language language = 12;
  Kind kind = 3;
  int32 properties = 4;
  string name = 5;
  Location location = 10;
  TextDocument signature = 7;
  repeated string members = 8;
  repeated string overrides = 9;
  Type tpe = 11;
}
```

"Symbols" is a section of a [TextDocument](#textdocument) that stores
information about [Symbols](#symbol) that are defined in the underlying
document. In a sense, this section is analogous to a symbol table
[\[20\]][20] in a compiler.

`SymbolInformation` contains assorted metadata for a `symbol`,
as explained below. At the moment, the supported metadata is usecase-driven
and is not supposed to be comprehensive. In the future,
we may add support for more metadata, e.g. documentation strings.

`language`. [Language](#language) that defines this symbol.

`kind`. Enumeration that defines the kind of the symbol.
See [Languages](#language) to learn how definitions in supported
languages map onto these kinds.

<table>
  <tr>
    <td width="100px"><b>Value</b></td>
    <td><b>Name</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>1</code></td>
    <td><code>VAL</code></td>
    <td>Value, e.g. <code>val x = 42</code>.</td>
  </tr>
  <tr>
    <td><code>2</code></td>
    <td><code>VAR</code></td>
    <td>Variable, e.g. <code>var x = 42</code>.</td>
  </tr>
  <tr>
    <td><code>3</code></td>
    <td><code>DEF</code></td>
    <td>Method, e.g. <code>def x = 42</code>.</td>
  </tr>
  <tr>
    <td><code>15</code></td>
    <td><code>GETTER</code></td>
    <td>Getter method, e.g. <code>def x: Int</code> generated for <code>val x = 42</code>.</td>
  </tr>
  <tr>
    <td><code>16</code></td>
    <td><code>SETTER</code></td>
    <td>Setter method, e.g. <code>def x_=(x$1: Int): Unit</code> generated for <code>var x = 42</code>.</td>
  </tr>
  <tr>
    <td><code>4</code></td>
    <td><code>PRIMARY_CONSTRUCTOR</code></td>
    <td>Primary constructor, e.g. <code>(x: Int)</code> in <code>class C(x: Int)</code>.</td>
  </tr>
  <tr>
    <td><code>5</code></td>
    <td><code>SECONDARY_CONSTRUCTOR</code></td>
    <td>Secondary constructor, e.g. <code>def this() = this(0)</code>.</td>
  </tr>
  <tr>
    <td><code>6</code></td>
    <td><code>MACRO</code></td>
    <td>Macro, e.g. <code>def m = macro impl</code>.</td>
  </tr>
  <tr>
    <td><code>7</code></td>
    <td><code>TYPE</code></td>
    <td>Type member, e.g. <code>type T <: Int</code> or <code>type T = Int</code>.</td>
  </tr>
  <tr>
    <td><code>8</code></td>
    <td><code>PARAMETER</code></td>
    <td>Parameter, e.g. <code>x</code> in <code>class C(x: Int)</code>.</td>
  </tr>
  <tr>
    <td><code>9</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td>Type parameter, e.g. <code>T</code> in <code>class C[T](x: T)</code>.</td>
  </tr>
  <tr>
    <td><code>10</code></td>
    <td><code>OBJECT</code></td>
    <td>Object, e.g. <code>object M</code>.</td>
  </tr>
  <tr>
    <td><code>11</code></td>
    <td><code>PACKAGE</code></td>
    <td>Package, e.g. <code>package p</code>.</td>
  </tr>
  <tr>
    <td><code>12</code></td>
    <td><code>PACKAGE_OBJECT</code></td>
    <td>Package object, e.g. <code>package object p</code>.</td>
  </tr>
  <tr>
    <td><code>13</code></td>
    <td><code>CLASS</code></td>
    <td>Class, e.g. <code>class C</code>.</td>
  </tr>
  <tr>
    <td><code>14</code></td>
    <td><code>TRAIT</code></td>
    <td>Trait, e.g. <code>trait T</code>.</td>
  </tr>
</table>

`properties`. Bitmask of miscellaneous bits of metadata:

<table>
  <tr>
    <td width="100px"><b>Value</b></td>
    <td><b>Name</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>0x1</code></td>
    <td><code>PRIVATE</code></td>
    <td>Has a <code>private</code> modifier?</td>
  </tr>
  <tr>
    <td><code>0x2</code></td>
    <td><code>PROTECTED</code></td>
    <td>Has a <code>protected</code> modifier?</td>
  </tr>
  <tr>
    <td><code>0x4</code></td>
    <td><code>ABSTRACT</code></td>
    <td>Has an <code>abstract</code> modifier, or is effectively abstract,
    i.e. is an abstract value, variable, method or type?</td>
  </tr>
  <tr>
    <td><code>0x8</code></td>
    <td><code>FINAL</code></td>
    <td>Has a <code>final</code> modifier, or is effectively final,
    i.e. is an object or a package object?</td>
  </tr>
  <tr>
    <td><code>0x10</code></td>
    <td><code>SEALED</code></td>
    <td>Has a <code>sealed</code> modifier?</td>
  </tr>
  <tr>
    <td><code>0x20</code></td>
    <td><code>IMPLICIT</code></td>
    <td>Has an <code>implicit</code> modifier?</td>
  </tr>
  <tr>
    <td><code>0x40</code></td>
    <td><code>LAZY</code></td>
    <td>Has a <code>lazy</code> modifier?</td>
  </tr>
  <tr>
    <td><code>0x80</code></td>
    <td><code>CASE</code></td>
    <td>Has a <code>case</code> modifier?</td>
  </tr>
  <tr>
    <td><code>0x100</code></td>
    <td><code>COVARIANT</code></td>
    <td>Has a covariant (<code>+</code>) modifier?</td>
  </tr>
  <tr>
    <td><code>0x200</code></td>
    <td><code>CONTRAVARIANT</code></td>
    <td>Has a contravariant (<code>-</code>) modifier?</td>
  </tr>
</table>

`name`. String that represents the name of the corresponding definition.

`location`. [Location](#location) that represents the extent of
the corresponding definition.

`signature`. Superseded by `SymbolInformation.tpe`.

`members`. Superseded by `ClassInfoType.members` in `SymbolInformation.tpe`.

`overrides`. Symbols that are extended or overridden by this symbol
either directly or transitively.

`tpe`. [Type](#type) that represents the type signature of the definition.
See [Languages](#language) for more information on which definitions have
which type signatures in supported languages.

### SymbolOccurrence

```protobuf
message SymbolOccurrence {
  Range range = 1;
  string symbol = 2;
  Role role = 3;
}
```

"Occurrences" is a section of a [TextDocument](#textdocument) that represents
the results of name resolution for identifiers in the underlying snippet of code.

`SymbolOccurrence` refers to a [Range](#range) in the code and has a symbol
as explained in [Symbol](#symbol). `role` is an enumeration that describes
the semantic role that the identifier performs in the snippet of code.
Like many other enumerations in SemanticDB, this one is usecase-driven
and will likely be updated in the future.

<table>
  <tr>
    <td><b>Value</b></td>
    <td><b>Name</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>1</code></td>
    <td><code>REFERENCE</code></td>
    <td>Reference, e.g. <code>y</code> in <code>val x = y</code>.</td>
  </tr>
  <tr>
    <td><code>2</code></td>
    <td><code>DEFINITION</code></td>
    <td>Definition, e.g. <code>x</code> in <code>val x = y</code>.</td>
  </tr>
</table>

### Diagnostic

```protobuf
message Diagnostic {
  Range range = 1;
  Severity severity = 2;
  string message = 3;
}
```

"Diagnostics" is a section of a [TextDocument](#textdocument) that stores
diagnostic messages produced by compilers, linters and other developer tools.

`Diagnostic` in SemanticDB directly correspond to `Diagnostic`
in LSP [\[2\]][2]. It has a [Range](#range), a severity and an associated
message. If the severity is unknown, it is up to the consumer to interpret
diagnostics as error, warning, info or hint.

<table>
  <tr>
    <td><b>Value</b></td>
    <td><b>Name</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>1</code></td>
    <td><code>ERROR</code></td>
    <td>Error.</td>
  </tr>
  <tr>
    <td><code>2</code></td>
    <td><code>WARNING</code></td>
    <td>Warning.</td>
  </tr>
  <tr>
    <td><code>3</code></td>
    <td><code>INFORMATION</code></td>
    <td>Information.</td>
  </tr>
  <tr>
    <td><code>4</code></td>
    <td><code>HINT</code></td>
    <td>Hint.</td>
  </tr>
</table>

### Synthetic

```protobuf
message Synthetic {
  Range range = 1;
  TextDocument text = 2;
}
```

"Synthetics" is a section of a [TextDocument](#textdocument) that stores
code snippets synthesized by compilers, code rewriters and other
developer tools.

`range` refers to a [Range](#range) in the original code of
the underlying document, and its value is determined as follows:

* If the synthetic replaces a code snippet (e.g. if it represents an
  implicit conversion applied to an expression), then its range must be equal
  to that snippet's range.
* If the synthetic inserts new code (e.g. if it represents an inferred type
  argument or implicit argument), then its range must be an empty range specifying the insertion point.

`text` is a [TextDocument](#textdocument) that represents a synthetic code
snippet as follows:

* Its text contains a language-dependent string that represents the synthetic
  code snippet. See [Languages](#languages) for more information.
* Its sections, e.g. [Occurences](#symboloccurrence), contain semantic
  information associated with that string.
* An occurrence of a placeholder symbol means that the synthetic code snippet
  includes the original code snippet located at `Synthetic.range`.

## Data Schemas

### Protobuf

[semanticdb3.proto][semanticdb3.proto]

## Languages

In this section, we describe language-dependent aspects of SemanticDB entities,
namely:
  * Valid names for [Language](#language).
  * Format for global [Symbols](#symbol).
  * Supported [Annotations](#annotation).
  * Supported [Types](#type).
  * Kinds and types of [SymbolInformation](#symbolinformation).
  * Format for [Synthetics](#synthetic).

We will be using a simple notation to describe SemanticDB entities.
In this notation, `M(v1, v2, ...)` corresponds a Protocol Buffers message
`M` with fields set to values `v1`, `v2`, etc. Literals correspond to
scalar values, and `List(x1, x2, ...)` corresponds to repeated values.
Moreover, `<X>` corresponds to a message that represents `X`.

### Scala

<a name="scala-language"></a>
#### Language

<table>
  <tr>
    <td><b>Value</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>Language("Scala")</code></td>
    <td>Scala of unknown or unspecified version.</td>
  </tr>
  <tr>
    <td><code>Language("ScalaXYZ")</code></td>
    <td>Scala, version <code>X.Y.Z</code>.</td>
  </tr>
</table>

<a name="scala-symbol"></a>
#### Symbol

In Scala, global symbol format is a concatenation of signatures of
the owner chain of the corresponding global definition, where:

* The owner chain of a definition is a list of its enclosing definitions
  starting with the outermost one, with the outermost definition being
  either `_empty_` (the special empty package [\[17\]][17]) or
  `_root_` (the special root package [\[18\]][18]). For example,
  for the `Int` class in the Scala standard library,
  the owner chain is `[_root_, scala, Int]`.
* The signature of a definition is:
  * For a val, var, object, package or package object, concatenation of its
    encoded name and a dot (`.`).
  * For a method, primary constructor, secondary constructor or macro,
    concatenation of its encoded name, its SemanticDB method descriptor
    and a dot (`.`). The method descriptor is used to distinguish
    overloaded methods as described below.
  * For an abstract type, type alias, class or trait, concatenation of its
    encoded name and a pound sign (`#`).
  * For a parameter, concatenation of a left parenthesis (`(`), its
    encoded name and a right parenthesis (`)`).
  * For a type parameter, concatenation of a left bracket (`[`), its
    encoded name and a right bracket (`]`).
* The encoded name of a definition is:
  * For a Java identifier [\[21\]][21], the name itself.
  * Otherwise, concatenation of a backtick, the name itself and another backtick.
* The SemanticDB descriptor of a method is a concatenation of simplifications of
  its parameter types interspersed by a comma (`,`), where a simplification
  of a type is:
  * For a type ref, the name of the underlying symbol without the prefix.
  * For a singleton type, its syntax without the prefix.
  * For a structural type, `{}`.
  * For an annotated type, the simplification of the underlying type.
  * For an existential type, the simplification of the underlying type.
  * For a type lambda, the simplification of the underlying type.
  * For a by-name type, the concatenation of the arrow sign (`=>`) and
    the simplification of the underlying type.
  * For a repeated type, the concatenation of the simplification of the
    underlying type and a star (`*`).

For example, this is how some of the definitions from the Scala standard library
must be modelled:

* The `scala` package: `_root_.scala.`
* The `Int` class: `_root_.scala.Int#`
* The `def println(x: Any)` method: `_root_.scala.Predef.println(Any).`
* The `def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That` method: ``_root_.scala.collection.immutable.List#`+:`(B,CanBuildFrom)``.
* The integer addition method: ``_root_.scala.Int#`+`(Int).``

<a name="scala-annotation"></a>
#### Annotation

<table>
  <tr>
    <td><b>Value</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>Annotation(&lt;ann&gt;)</code></td>
    <td>Definition annotation, e.g. <code>@ann def m: T</code>.</td>
  </tr>
  <tr>
    <td><code>Annotation(&lt;ann&gt;)</code></td>
    <td>Type annotation, e.g. <code>T @ann</code>.</td>
  </tr>
</table>

At the moment, `Annotation` can't represent annotation arguments,
which means that the annotation in `@ann(x, y, z) def m: T` will be represented
as `Annotation(<ann>)`. We intend to improve on this in the future

<a name="scala-type"></a>
#### Type

<table>
  <tr>
    <td width="220px"><b>Category</b></td>
    <td><b>Examples</b></td>
  </tr>
  <tr>
    <td valign="top">Singleton types [<a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#singleton-types">22</a>, <a href="https://github.com/scala/scala/pull/5310">23</a>]</td>
    <td>
      <ul>
        <li><code>x.type</code> ~ <code>SingletonType(SYMBOL, null, &lt;x&gt;, null, null)</code>.</li>
        <li><code>p.x.type</code> ~ <code>SingletonType(SYMBOL, &lt;p.type&gt;, &lt;x&gt;, null, null)</code>.</li>
        <li><code>this.type</code> ~ <code>SingletonType(THIS, null, null, null, null)</code>.</li>
        <li><code>C.this.type</code> ~ <code>SingletonType(THIS, &lt;C&gt;, null, null, null)</code>.</li>
        <li>Type of <code>super</code> ~ <code>SingletonType(SUPER, ThisType(...), null, null, null)</code>.</li>
        <li>Type of <code>super[M]</code> ~ <code>SingletonType(SUPER, ThisType(...), &lt;M&gt;, null, null)</code>.</li>
        <li>Type of <code>C.super[M]</code> ~ <code>SingletonType(SUPER, ThisType(&lt;C&gt;), &lt;M&gt;, null, null)</code>.</li>
        <li>Literal type ~ <code>SingletonType(&lt;TAG&gt;, null, null, &lt;value&gt;, null)</code> or <code>SingletonType(&lt;TAG&gt;, null, null, null, &lt;value&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Type projections <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#type-projection">[24]</a></td>
    <td>
      <ul>
        <li><code>T#C</code> ~ <code>TypeRef(&lt;T&gt;, &lt;C&gt;, List())</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Type designators <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#type-designators">[25]</a></td>
    <td>
      <ul>
        <li><code>t</code> ~ <code>TypeRef(null, &lt;t&gt;, List())</code>.</li>
        <li><code>Int</code> ~ <code>TypeRef(&lt;scala.type&gt;, &lt;Int&gt;, List())</code>.</li>
        <li><code>scala.Int</code> ~ <code>TypeRef(&lt;scala.type&gt;, &lt;Int&gt;, List())</code>.</li>
        <li><code>p.C</code> ~ <code>TypeRef(&lt;p.type&gt;, &lt;C&gt;, List())</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Parameterized types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#parameterized-types">[26]</a></td>
    <td>
      <ul>
        <li><code>T#C[T1, ..., Tn]</code> ~ <code>TypeRef(&lt;T&gt;, &lt;C&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
        <li><code>M[T1, ..., Tn]</code> ~ <code>TypeRef(null, &lt;M&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
        <li><code>List[Int]</code> ~ <code>TypeRef(&lt;scala.type&gt;, &lt;List&gt;, List(&lt;Int&gt;))</code>.</li>
        <li><code>scala.List[Int]</code> ~ <code>TypeRef(&lt;scala.type&gt;, &lt;List&gt;, List(&lt;Int&gt;))</code>.</li>
        <li><code>p.C[T1, ..., Tn]</code> ~ <code>TypeRef(&lt;p.type&gt;, &lt;C&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Tuple types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#tuple-types">[27]</a></td>
    <td>
      <ul>
        <li><code>(T1, ..., Tn)</code> ~ <code>TypeRef(&lt;scala.type&gt;, &lt;TupleN&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Annotated types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#annotated-types">[28]</a></td>
    <td>
      <ul>
        <li><code>T @ann1 ... @annN</code> ~ <code>AnnotatedType(List(&lt;ann1&gt;, ..., &lt;annN&gt;), &lt;T&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Compound types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#compound-types">[29]</a></td>
    <td>
      <ul>
        <li><code>{ M1; ...; Mm }</code> ~ <code>StructuralType(List(), List(&lt;M1&gt;, ..., &lt;Mm&gt;))</code>.</li>
        <li><code>T1 with ... with Tn</code> ~ <code>StructuralType(List(&lt;T1&gt;, ..., &lt;Tn&gt;), List())</code>.</li>
        <li><code>T1 with ... with Tn { M1; ...; Mm }</code> ~ <code>StructuralType(List(&lt;T1&gt;, ..., &lt;Tn&gt;), List(&lt;M1&gt;, ..., &lt;Mm&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Infix types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#infix-types">[30]</a></td>
    <td>
      <ul>
        <li><code>A L B</code> ~ <code>TypeRef(..., &lt;L&gt;, List(&lt;A&gt;, &lt;B&gt;))</code> for left-associative <code>L</code>.</li>
        <li><code>A R B</code> ~ <code>TypeRef(..., &lt;R&gt;, List(&lt;B&gt;, &lt;A&gt;))</code> for right-associative <code>R</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Function types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#function-types">[31]</a></td>
    <td>
      <ul>
        <li><code>(T1, ..., Tn) =&gt; R</code> ~ <code>TypeRef(&lt;scala.type&gt;, &lt;FunctionN&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;, &lt;R&gt;))</code>.</li>
        <li><code>=&gt; Ti</code> ~ <code>ByNameType(&lt;Ti&gt;)</code>.</li>
        <li><code>Ti*</code> ~ <code>RepeatedType(&lt;Ti&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Existential types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#existential-types">[32]</a></td>
    <td>
      <ul>
        <li><code>T forSome { M1; ...; Mm }</code> ~ <code>ExistentialType(List(&lt;T&gt;), List(&lt;M1&gt;, ..., &lt;Mm&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Method types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#method-types">[33]</a></td>
    <td>
      <ul>
        <li><code>(P1s)...(Pns)U</code> ~ <code>MethodType(List(), List(List(&lt;T11&gt;, ...), ..., List(&lt;Tn1&gt;, ...)), &lt;U&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Polymorphic method types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#polymorphic-method-types">[34]</a></td>
    <td>
      <ul>
        <li><code>[ts](P1s)...(Pns)U</code> ~ <code>MethodType(List(&lt;t1&gt;, ..., &lt;tm&gt;), List(List(&lt;T11&gt;, ...), ..., List(&lt;Tn1&gt;, ...)), &lt;U&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Type constructors <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#type-constructors">[35]</a></td>
    <td>
      <ul>
        <li><code>[ts]T</code> ~ <code>UniversalType(List(&lt;t1&gt;, ..., &lt;tm&gt;), &lt;T&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
</table>

<a name="scala-symbolinformation"></a>
#### SymbolInformation

TODO

<a name="scala-synthetic"></a>
#### Synthetic

Synthetics are unspecified in the Scala Language Specification, so we leave the
synthetic format deliberately unspecified as well. Our experience [\[36\]][36]
shows that reverse engineering Scala synthetics is very hard. We may improve
on this in the future, but this is highly unlikely.

[semanticdb2.proto]: https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb2/semanticdb2.proto
[semanticdb3.proto]: https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb3/semanticdb3.proto
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
[19]: https://www.scala-lang.org/files/archive/spec/2.11/03-types.html
[20]: https://en.wikipedia.org/wiki/Symbol_table
[21]: https://docs.oracle.com/javase/specs/jls/se9/html/jls-3.html#jls-3.8
[36]: http://scalamacros.org/paperstalks/2016-02-11-WhatDidWeLearnInScalaMeta.pdf
