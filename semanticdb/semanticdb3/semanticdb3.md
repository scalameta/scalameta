# SemanticDB Specification

  * [Motivation](#motivation)
  * [Data Model](#data-model)
    * [TextDocument](#textdocument)
    * [Range](#range)
    * [Location](#location)
    * [Symbol](#symbol)
    * [Type](#type)
    * [SymbolInformation](#symbolinformation)
    * [SymbolOccurrence](#symboloccurrence)
    * [Diagnostic](#diagnostic)
    * [Synthetic](#synthetic)
  * [Data Schemas](#data-schemas)
    * [Protobuf](#protobuf)

## Motivation

Nowadays, there is a clear trend towards standards for communication between developer tools.
Language Server Protocol (LSP) [\[2\]][2], a protocol that connects programming
language implementations and text editors, has gained strong industrial support
and at the time of writing has implementations for many programming languages and editors.
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

## Data Model

SemanticDB is a data model for semantic information about programs in Scala and
other languages. SemanticDB decouples production and consumption of semantic information,
establishing documented means for communication between tools.

In this section, we describe the SemanticDB data model by going through the
individual sections of the associated Protocol Buffers [\[13\]][13] schema.
In the future, we may also support other kinds of schemas, including
JSON [\[14\]][14] and SQL [\[15\]][15]. See [Data Schemas](#data-schemas)
for more information.

### TextDocument

```protobuf
message TextDocuments {
  repeated TextDocument documents = 1;
}

message TextDocument {
  Schema schema = 1;
  string uri = 2;
  string text = 3;
  string language = 4;
  repeated SymbolOccurrence occurrences = 5;
  repeated SymbolInformation symbols = 6;
  repeated Diagnostic diagnostics = 7;
  repeated Synthetic synthetics = 8;
}
```

`TextDocument` is the central data structure in the SemanticDB model.
It provides semantic information about a snippet of code written in a programming
language `language`.

SemanticDB payloads must include the version of the data model that was used
to produce them in the `schema` field. The following versions are supported:

<table>
  <tr>
    <td><b>Version</b></td>
    <td><b>Explanation</b></td>
    <td><b>Data model<b></td>
  </tr>
  <tr>
    <td>LEGACY</td>
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
At the moment, the symbol format is defined by the needs of the Scala implementations.
In the future, we are planning to pay more attention to other languages.

At the moment, symbols are not guaranteed to be globally unique, which means
that there may be limitations for using symbols as keys in maps or databases.
Read below for more information about uniqueness guarantees for different
symbol categories.

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
    * For a val, var, object, package or package object, concatenation of its
    encoded name and a dot (`.`).
    * For a method, primary constructor, secondary constructor or macro,
    concatenation of its encoded name, its JVM method descriptor [\[19\]][19]
    and a dot (`.`). The JVM method descriptor is used to distinguish overloaded
    methods as mandated by the Scala Language Specification [\[20\]][20].
    * For an abstract type, type alias, class or trait, concatenation of its
    encoded name and a pound sign (`#`).
    * For a parameter, concatenation of a left parenthesis (`(`), its
    encoded name and a right parenthesis (`)`).
    * For a type parameter, concatenation of an left bracket (`[`), its
    encoded name and a right bracket (`]`).
  * The encoded name of a definition is:
    * For a Java identifier [\[25\]][25], the name itself.
    * Otherwise, concatenation of a backtick (`````), the name itself and
    another backtick (`````).

For example, this is how some of the definitions from the Scala standard library
must be modelled:
  * The `scala` package: `_root_.scala.`
  * The `Int` class: `_root_.scala.Int#`
  * The single-parameter `println` method: `_root_.scala.Predef.println(Ljava/lang/Object;)V.`
  * The integer addition method: ``_root_.scala.Int#`+`(I)I.``.

Global symbols must be unique across the universe of documents that
a tool is working with at any given time. For example, if in such a universe,
there exist multiple definitions of `Int` - e.g. coming from multiple different
versions of Scala - then all references to those definitions will have
`SymbolOccurrence.symbol` equal to the same `_root_.scala.Int#` symbol,
and SemanticDB will not be able to provide information to disambiguate these references.

In the future, we may extend SemanticDB to allow for multiple definitions
that under current rules would correspond to the same global symbol.
In the meanwhile, when global uniqueness is required, tool authors are advised to
accompany global symbols with `SymbolInformation.location`.

**Local symbols**. Correspond to a definition that isn't global (see above).
Local symbol format is defined by the following two rules:
  * Local symbols must start with `local`, so that they can be easily
    distinguished from global symbols.
  * Local symbols must be unique within the underlying document.

For example, `x` in `def identity[T](x: T): T` may be modelled by local symbols
`local0`, `local_x`, `local_identity_x`, as long as these names are unique within
the underlying document. The same logic applies to the type parameter `T`,
which is also a local definition.

Local symbols must be unique within the underlying document, but they don't have
to be unique across multiple documents. For example, at the time of writing
`semanticdb-scalac` produces local symbols named `local0`, `local1`, etc,
with the counter resetting to zero for every new document.

In the future, we may extend SemanticDB to make local symbols more unique,
but we haven't yet found a way to do that without sacrificing performance
and payload size. In the meanwhile, when global uniqueness is required,
tool authors are advised to accompany local symbols with `TextDocument.uri`.

**Multi symbols**. Are used to model references to a set of multiple definitions at once.
This is occasionally useful to support corner cases of Scala, e.g. identifiers
in imports that can refer to both a class and an object with the same name,
or references to unresolved overloaded methods.

Multi symbol format is a concatentation of the underlying symbol formats
interspersed with a semicolon (`;`). Within a multi symbol, the underlying
symbols must be ordered lexicographically in ascending order.

For example, a reference to both the standard `Int` class and its companion object
must be modelled by `_root_.scala.Int#;_root_.scala.Int.`. Because of the order
requirement, `_root_.scala.Int.;_root_.scala.Int#` is not a valid symbol.

**Placeholder symbols**. Are used to model original snippets of code in [Synthetics](#synthetic).
Must not be used outside `Synthetic.text` documents. Placeholder symbols are
always equal to an asterisk (`*`).

### Type

```protobuf
message Type {
  enum Tag {
    UNKNOWN_TAG = 0;
    TYPE_REF = 1;
    SINGLE_TYPE = 2;
    THIS_TYPE = 3;
    SUPER_TYPE = 4;
    LITERAL_TYPE = 5;
    COMPOUND_TYPE = 6;
    ANNOTATED_TYPE = 7;
    EXISTENTIAL_TYPE = 8;
    TYPE_LAMBDA = 9;
    CLASS_INFO_TYPE = 10;
    METHOD_TYPE = 11;
    BY_NAME_TYPE = 12;
    REPEATED_TYPE = 13;
    TYPE_TYPE = 14;
  }
  Tag tag = 1;
  TypeRef typeRef = 2;
  SingleType singleType = 3;
  ThisType thisType = 4;
  SuperType superType = 5;
  LiteralType literalType = 6;
  CompoundType compoundType = 7;
  AnnotatedType annotatedType = 8;
  ExistentialType existentialType = 9;
  TypeLambda typeLambda = 10;
  ClassInfoType classInfoType = 11;
  MethodType methodType = 12;
  ByNameType byNameType = 13;
  RepeatedType repeatedType = 14;
  TypeType typeType = 15;
}
```

`Type` represents expression types and signatures of definitions.
At the moment, types are are modelled after the Scala pickle format [\[26\]][26].
In the future, we are planning to pay more attention to other languages.

In this section, we describe various alternatives of `Type`, providing example
SemanticDB data that corresponds to different Scala types,
inspired by the scala.reflect documentation [\[27\]][27].

In these examples, we will be using a simple notation to describe SemanticDB data.
In this notation, `M(v1, v2, ...)` corresponds a message `M` with fields set to values
`v1`, `v2`, etc. Literals correspond to scalar values, and `List(x1, x2, ...)`
corresponds to repeated values. Moreover, `<X>` corresponds to a message that
represents `X`.

```protobuf
message TypeRef {
  Type prefix = 1;
  string symbol = 2;
  repeated Type arguments = 3;
}
```

`TypeRef` is bread-and-butter type in SemanticDB. It represents identifiers,
paths [\[28\]][28], parameterized types [\[29\]][29] and type projections
[\[30\]][30]. Infix types [\[31\]][31], tuple types [\[32\]][32] and
function types [\[33\]][33] are also represented by typerefs via desugaring
to their canonical parameterized form:
  * `C` ~ `TypeRef(null, <C>, List())`.
  * `p.C` ~ `TypeRef(<p.type>, <C>, List())`.
  * `T#C` ~ `TypeRef(<T>, <C>, List())`.
  * `C[T1, ... Tn]` ~ `TypeRef(null, <C>, List(<T1>, ..., <TN>))`.
  * `p.C[T1, ... Tn]` ~ `TypeRef(<p.type>, <C>, List(<T1>, ..., <TN>))`.
  * `T#C[T1, ... Tn]` ~ `TypeRef(<T>, <C>, List(<T1>, ..., <TN>))`.

```protobuf
message SingleType {
  Type prefix = 1;
  string symbol = 2;
}
```

`SingleType` represents the majority of singleton types [\[34\]][34]:
  * `x.type` ~ `SingleType(null, <x>)`.
  * `p.x.type` ~ `SingleType(<p.type>, <x>)`.

```protobuf
message ThisType {
  string symbol = 1;
}
```

`ThisType` represents `this.type`:
  * `this.type` ~ `ThisType(null)`.
  * `C.this.type` ~ `ThisType(<C>)`.

```protobuf
message SuperType {
  Type prefix = 1;
  Type mix = 2;
}
```

`SuperType` represents types of `super` qualifiers [\[28\]][28]:
  * Type of the qualifier in `super.x` ~ `SuperType(ThisType(...), null)`.
  * Type of the qualifier in `super[M].x` ~ `SuperType(ThisType(...), <M>)`.
  * Type of the qualifier in `C.super[M].x` ~ `SuperType(ThisType(<C>), <M>)`.

```protobuf
message LiteralType {
  enum Tag {
    UNKNOWN_TAG = 0;
    UNIT = 1;
    BOOLEAN = 2;
    BYTE = 3;
    SHORT = 4;
    CHAR = 5;
    INT = 6;
    LONG = 7;
    FLOAT = 8;
    DOUBLE = 9;
    STRING = 10;
    NULL = 11;
  }
  Tag tag = 1;
  int64 primitive = 2;
  string string = 3;
}
```

`LiteralType` represents literal types [\[35\]][35].

```protobuf
message CompoundType {
  repeated Type parents = 1;
  repeated string declarations = 2;
}
```

`CompoundType` represents compound types [\[36\]][36]:
  * `{ M1; ...; Mm }` ~ `CompoundType(List(), List(<M1>, ..., <Mm>))`.
  * `T1 with ... with Tn` ~ `CompoundType(List(<T1>, ..., <Tn>), List())`.
  * `T1 with ... with Tn { M1; ...; Mm }` ~ `CompoundType(List(<T1>, ..., <Tn>), List(<M1>, ..., <Mm>))`.

```protobuf
message AnnotatedType {
  Type tpe = 1;
  repeated Type annotations = 2;
}
```

`AnnotatedType` represents annotated types [\[37\]][37] with the caveat
that annotation arguments are not represented in the corresponding payload.
We may remove this limitation in the future:
  * `T @ann(x1, ... xM)` ~ `AnnotatedType(<T>, List(<ann>))`.
  * `T @ann1 ... @annN` ~ `AnnotatedType(<T>, List(<ann1>, ..., <annN>))`.

```protobuf
message ExistentialType {
  Type tpe = 1;
  repeated string declarations = 2;
}
```

`ExistentialType` represents existential types [\[38\]][38]:
  * `T forSome { type T }` ~ `ExistentialType(<T>, List(<T>))`.

```protobuf
message TypeLambda {
  repeated string type_parameters = 1;
  Type tpe = 2;
}
```

`TypeLambda` represents types that are colloquially called "type lambdas"
in the Scala community [\[39\]][39].

```protobuf
message ClassInfoType {
  repeated string type_parameters = 1;
  repeated Type parents = 2;
  repeated string declarations = 3;
}
```

`ClassInfoType` represents signatures of objects, package objects, classes
and traits. It works along the same lines as `CompoundType`:
  * Signature of `object M` ~ `ClassInfoType(List(), List(), List())`.
  * Signature of `class C extends B { def x = 42 }` ~ `ClassInfoType(List(), List(<B>), List(<m>))`.
  * Signature of `trait X[T]` ~ `ClassInfoType(List(<T>), List(), List())`.

```protobuf
message MethodType {
  message ParameterList {
    repeated string symbols = 1;
  }
  repeated string type_parameters = 1;
  repeated ParameterList parameters = 2;
  Type result = 3;
}
```

`MethodType` represents signatures of vals, vars, methods, primary constructors,
secondary constructors and macros:
  * Signature of `val x: Int` ~ `MethodType(List(), List(), <Int>)`
  * Signature of `var x: Int` ~ `MethodType(List(), List(), <Int>)`
  * Signature of `def m: Int` ~ `MethodType(List(), List(), <Int>)`.
  * Signature of `def m(): Int` ~ `MethodType(List(), List(List()), <Int>)`.
  * Signature of `def m(x: Int): Int` ~ `MethodType(List(), List(List(<x>)), <Int>)`.
  * Signature of `def m[T](x: T): T` ~ `MethodType(List(<T>), List(List(<x>)), <T>)`.

```protobuf
message ByNameType {
  Type tpe = 1;
}
```

`ByNameType` represents signatures of by-name parameters [\[40\]][40]:
  * Signature of `x` in `def m(x: => Int): Int` ~ `ByNameType(<Int>)`.

```protobuf
message RepeatedType {
  Type tpe = 1;
}
```

`RepeatedType` represents signatures of repeated parameters [\[41\]][41]:
  * Signature of `xs` in `def m(xs: Int*): Int` ~ `RepeatedType(<Int>)`.

```protobuf
message TypeType {
  repeated string type_parameters = 1;
  Type lower_bound = 2;
  Type upper_bound = 3;
}
```

`TypeType` represents signatures of abstract type members and type parameters,
but not type aliases:
  * Signature of `type T` ~ `TypeBounds(List(), null, null)`.
  * Signature of `T` in `def m[T <: C]` ~ `TypeBounds(List(), null, <C>)`.
  * Signature of `M` in `def m[M[_]]` ~ `TypeBounds(List(<_>), null, null)`.
  * Signature of `type T = C` ~ `TypeRef(..., <C>, List())`.
  * Signature of `type T[U] = U` ~ `TypeLambda(List(<U>), TypeRef(null, <U>, List())`.

### SymbolInformation

```protobuf
message SymbolInformation {
  reserved 6;
  string symbol = 1;
  string language = 2;
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

"Symbols" is a section of a [TextDocument](#textdocument) that stores information
about [Symbols](#symbol) that are defined in the underlying snippet of code.
In a sense, this section is analogous to symbol tables [\[21\]][21] in compiler.

`SymbolInformation` contains assorted metadata for a `symbol`, as explained below.
At the moment, the supported metadata is usecase-driven and is not supposed to
be comprehensive or language-agnostic. In the future, we may add support for
more metadata, for example information about overriding, documentation strings
or features from other languages.

`language`. Language that defines this symbol.

`kind`. Enumeration that defines the kind of the symbol:
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
    <td>Abstract type or a type alias, e.g. <code>type T <: Int</code> or <code>type T = Int</code>.</td>
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
  <tr>
    <td><code>0x400</code></td>
    <td><code>VALPARAM</code></td>
    <td>Is a `val` parameter of a primary constructor?</td>
  </tr>
  <tr>
    <td><code>0x800</code></td>
    <td><code>VARPARAM</code></td>
    <td>Is a `var` parameter of a primary constructor?</td>
  </tr>
</table>

`name`. String that represents the name of the symbol.

`location`. [Location](#location) that represents the extent of the definition of the symbol.

`signature`. [TextDocument](#textdocument) that represents the type signature of the definition.
In this document, `text` contains a string prettyprinted by a producer and various
sections, e.g. [Occurrences](#symboloccurrence), contain semantic information associated
with that string. This document does not correspond to any compilation unit and
is created solely for the purposes of storing an attributed snippet of text.

For example, for `def x = 42`, the corresponding signature may be a document with
`text` equal to `Int` and `occurrences` featuring an identifier with `range`
equal to `0:0..0:3`, `symbol` equal `_root_.scala.Int#` and `role` equal to `Reference`.

The signature format was historically unspecified. When we got around
to specifying the format, we found out that representing type signatures
with documents was an evolutionary dead end. Therefore, we superseded this
field with `tpe`.

`members`. This field was historically unspecified. When we got around to
specifying it, we superseded it with `ClassInfoType.members` in `SymbolInformation.tpe`.

`overrides`. Symbols that are overridden by this symbol either directly or transitively.

`tpe`. [Type](#type) that represents the type signature of the definition.

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

`SymbolOccurrence` refers to a [Range](#range) in the code and has a symbol as explained
in [Symbol](#symbol). `role` is an enumeration that describes the semantic role
that the identifier performs in the snippet of code. Like many other enumerations
in SemanticDB, this one is usecase-driven and will likely be updated in the future.

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

`Diagnostic` in SemanticDB directly corresponds to `Diagnostic` in LSP [\[2\]][2].
It has a [Range](#range), a severity and an associated message. If the severity
is unknown, it is up to the consumer to interpret diagnostics as error, warning, info or hint.

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
  * Its sections, e.g. [Occurences](#symboloccurrence), contain semantic information
    associated with that string.
  * An occurrence of a placeholder symbol means that the snippet of code includes
    the fragment of the original code defined by `Synthetic.range`.

Synthetics are unspecified in the Scala Language Specification, so we leave the
synthetic format deliberately unspecified as well. Our experience [\[22\]][22] shows
that reverse engineering Scala synthetics is very hard. We may improve on this
in the future, but this is highly unlikely.

## Data Schemas

### Protobuf

[semanticdb3.proto][semanticdb3.proto]

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
[19]: https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.3.3
[20]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#class-members
[21]: https://en.wikipedia.org/wiki/Symbol_table
[22]: http://scalamacros.org/paperstalks/2016-02-11-WhatDidWeLearnInScalaMeta.pdf
[23]: https://docs.google.com/document/d/1cH2sTpgSnJZCkZtJl1aY-rzy4uGPcrI-6RrUpdATO2Q/edit
[24]: https://www.youtube.com/watch?v=jGJhnIT-D2M
[25]: https://docs.oracle.com/javase/specs/jls/se9/html/jls-3.html#jls-3.8
[26]: https://github.com/scala/scala/blob/v2.12.4/src/reflect/scala/reflect/internal/pickling/PickleFormat.scala
[27]: https://docs.scala-lang.org/overviews/reflection/overview.html
[28]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#paths
[29]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#parameterized-types
[30]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#type-projection
[31]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#infix-types
[32]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#tuple-types
[33]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#function-types
[34]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#singleton-types
[35]: https://github.com/scala/scala/pull/5310
[36]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#compound-types
[37]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#annotated-types
[38]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#existential-types
[39]: https://stackoverflow.com/questions/8736164/what-are-type-lambdas-in-scala-and-what-are-their-benefits
[40]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#by-name-parameters
[41]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#repeated-parameters
