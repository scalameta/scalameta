# SemanticDB Specification

* [Motivation](#motivation)
* [Data Model](#data-model)
  * [TextDocument](#textdocument)
  * [Language](#language)
  * [URI](#uri)
  * [Range](#range)
  * [Location](#location)
  * [Symbol](#symbol)
  * [Type](#type)
  * [SymbolInformation](#symbolinformation)
  * [Annotation](#annotation)
  * [Accessibility](#accessibility)
  * [SymbolOccurrence](#symboloccurrence)
  * [Diagnostic](#diagnostic)
  * [Synthetic](#synthetic)
* [Data Schemas](#data-schemas)
  * [Protobuf](#protobuf)
* [Languages](#languages)
  * [Scala](#scala)
    * [Symbol](#scala-symbol)
    * [Type](#scala-type)
    * [SymbolInformation](#scala-symbolinformation)
    * [Annotation](#scala-annotation)
    * [Accessibility](#scala-accessibility)
    * [Synthetic](#scala-synthetic)
  * [Java](#java)
    * [Symbol](#java)
    * [Type](#java)
    * [SymbolInformation](#java)
    * [Annotation](#java)
    * [Accessibility](#java)
    * [Synthetic](#java)

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
underdocumented and lack compatibility guarantees. Otherwise, they have to
reimplement compiler internals, which usually leads to duplication of effort and
inconsistent user experience. For example, Scala IDE [\[9\]][9] uses Scala
compiler internals, which has known stability problems in interactive mode.
To the contrast, IntelliJ [\[10\]][10] has its own Scala typechecker, which
is more stable but is known for spurious red squiggles.

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

The data structures in the SemanticDB data model are language-agnostic, but
the entities corresponding to these data structures may be language-dependent.
For example, [Range](#range) and [Location](#location) mean the same thing in
all languages, while [Symbol](#symbol) format relies on scoping rules that
generally vary across languages. See [Languages](#languages)
for more information about language-dependent aspects of the specification.

### TextDocument

```protobuf
message TextDocuments {
  repeated TextDocument documents = 1;
}

message TextDocument {
  reserved 4, 9;
  Schema schema = 1;
  string uri = 2;
  string text = 3;
  Language language = 10;
  repeated SymbolInformation symbols = 5;
  repeated SymbolOccurrence occurrences = 6;
  repeated Diagnostic diagnostics = 7;
  repeated Synthetic synthetics = 8;
}
```

`TextDocument` provides semantic information about a code snippet.
It is the central data structure of the SemanticDB model, and its entities
are also called "SemanticDB payloads".

SemanticDB payloads must include the version of the SemanticDB model in the
`schema` field. The following versions of the model are supported:

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
    <td>SemanticDB v3 payloads</td>
    <td><a href="https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb3/semanticdb3.proto">semanticdb3.proto</a> (described in this document)</td>
  </tr>
</table>

Code snippets referenced in `TextDocument` can be defined
in one of three ways: 1) via a [URI](#uri) provided in `uri`,
2) via a string provided in `text`, 3) via a combination of both.

`language` defines the [Language](#language) in which the code snippet
is written. See [Languages](#languages) for the list of supported programming
languages.

Semantic information about code snippets is stored in so called sections -
repeated fields within `TextDocument` - as described below.
These sections are optional, which means that documents providing only part
of semantic information for the corresponding snippet
(or no semantic information at all) are completely legal.

### Language

```protobuf
enum Language {
  UNKNOWN_LANGUAGE = 0;
  SCALA = 1;
  JAVA = 2;
}
```

`Language` represents a programming language that defines certain SemanticDB
entities, e.g. [Document](#document) or [Symbol](#symbol). Currently,
See [Languages](#languages) for the details of how features of supported
programming languages map onto SemanticDB.

At the moment, SemanticDB does not have official support for modelling languages
that are not included in the list above. Moreover, `Language` does not have
capabilities to specify the associated language or compiler version. We may
improve on this in the future.

### URI

URIs are unique resource identifiers as defined in [\[16\]][16].
In the SemanticDB model, URIs are represented as strings.

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
The start point is inclusive, and the end point is exclusive.

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
the Scala standard library is modelled as `scala.Int#`, where
the pound sign (`#`) at the end of the symbol means that the symbol corresponds
to a class as opposed to an object that would end with a dot (`.`).
See [Languages](#languages) for more information.

Global symbols must be unique across the universe of documents that
a SemanticDB-based is working with at any given time. For example,
if in such a universe, there exist multiple definitions of `Int` -
e.g. coming from multiple different versions of Scala - then all references
to those definitions will have `SymbolOccurrence.symbol` equal to the same
`scala.Int#` symbol, and SemanticDB will not be able to provide
information to distinguish these references from each other.

In the future, we may extend SemanticDB to allow for multiple definitions
that under current rules would correspond to the same global symbol.
In the meanwhile, when global uniqueness is required, tool authors are advised
to accompany global symbols with `SymbolInformation.location`.

**Local symbols**. Correspond to a definition that isn't global (see above).

Local symbol format is language-agnostic and is a concatenation of `local`
and a decimal number. For example, `x` in a Scala method
`def identity[T](x: T): T` may be modelled by local symbols `local0`, `local1`,
etc. The same logic applies to the type parameter `T`, which is also a local
definition.

Local symbols must be unique within the underlying document, but they don't have
to be unique across multiple documents. For example, at the time of writing
the Scalac-based SemanticDB producer generates local symbols named `local0`,
`local1`, etc, with the counter resetting to zero for every new document.

In the future, we may extend SemanticDB to make local symbols more unique,
but we haven't yet found a way to do that without sacrificing performance
and payload size. In the meanwhile, when global uniqueness is required,
tool authors are advised to accompany local symbols with `TextDocument.uri`.

**Multi symbols**. Represent references to multiple definitions at once.
For example, this is occasionally useful to support corner cases of Scala,
e.g. identifiers in imports that can refer to both a class and an object with
the same name, or references to unresolved overloaded methods.

Multi symbol format is a concatentation of underlying symbols
interspersed with a semicolon (`;`). Within a multi symbol, the underlying
symbols must be ordered lexicographically in ascending order.

For example, a reference to both the `Int` class in the Scala standard library
and its companion object must be modelled by `scala.Int#;.scala.Int.`.
Because of the order requirement, `scala.Int.;scala.Int#` is not a valid symbol.

**Placeholder symbols**. Are used to model original code snippets in
[Synthetics](#synthetic). Must not be used outside `Synthetic.text` documents.
Placeholder symbols are always equal to an asterisk (`*`).

### Type

```protobuf
message Type {
  enum Tag {
    reserved 2, 3, 4, 5;
    UNKNOWN_TYPE = 0;
    TYPE_REF = 1;
    SINGLETON_TYPE = 15;
    INTERSECTION_TYPE = 16;
    UNION_TYPE = 17;
    WITH_TYPE = 18;
    STRUCTURAL_TYPE = 6;
    ANNOTATED_TYPE = 7;
    EXISTENTIAL_TYPE = 8;
    UNIVERSAL_TYPE = 9;
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
  IntersectionType intersectionType = 17;
  UnionType unionType = 18;
  WithType withType = 19;
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

`Type` represents expression types and definition signatures.

The SemanticDB type system is a superset of the type systems of supported
languages - currently modelled after the Scala type system [\[18\]][18].
This section describes the model, while [Languages](#languages) elaborates
on how language types map onto this model.

```protobuf
message TypeRef {
  Type prefix = 1;
  string symbol = 2;
  repeated Type type_arguments = 3;
}
```

`TypeRef` is the bread-and-butter type of SemanticDB. It represents a reference
to a [Symbol](#symbol), possibly parameterized by `type_arguments`. To model
references to nested type definitions, e.g. path-dependent types in Scala,
type refs include `prefix`.

```protobuf
message SingletonType {
  enum Tag {
    UNKNOWN_SINGLETON = 0;
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
via a [Symbol](#symbol) accompanied with a `prefix`, 2) via
a keyword (by `this` or `super`) or 3) via a literal.

```protobuf
message IntersectionType {
  repeated Type types = 1;
}
```

`IntersectionType` represents an intersection of `types`.

```protobuf
message UnionType {
  repeated Type types = 1;
}
```

`UnionType` represents a union of `types`.

```protobuf
message WithType {
  repeated Type types = 1;
}
```

`WithType` represents a Scala-like compound type [\[31\]][31] based on `types`.
Unlike intersection types, compound types are not commutative.

```protobuf
message StructuralType {
  reserved 1, 2;
  Type tpe = 4;
  repeated string declarations = 3;
}
```

`StructuralType` represents a structural type specified by its base type `tpe`
and `declarations`. Declarations are modelled as [Symbols](#symbol)
whose metadata must be provided via [SymbolInformation](#symbolinformation).

```protobuf
message AnnotatedType {
  reserved 2;
  repeated Annotation annotations = 3;
  Type tpe = 1;
}
```

`AnnotatedType` represents a type `tpe` annotated by one or more
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

`ClassInfoType` represents a signature of a class, a trait or the like.

```protobuf
message MethodType {
  message ParameterList {
    repeated string symbols = 1;
  }
  repeated string type_parameters = 1;
  repeated ParameterList parameters = 2;
  Type return_type = 3;
}
```

`MethodType` represents a signature of a method, a constructor or the like.
It features `type_parameters`, `parameters` and a `return_type`. Both type
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
  reserved 2, 6, 7, 8, 12;
  string symbol = 1;
  Language language = 16;
  Kind kind = 3;
  int32 properties = 4;
  string name = 5;
  Location location = 10;
  repeated string overrides = 9;
  Type tpe = 11;
  repeated Annotation annotations = 13;
  Accessibility accessibility = 14;
  string owner = 15;
}
```

"Symbols" is a section of a [TextDocument](#textdocument) that stores
information about [Symbols](#symbol) that are defined in the underlying
document. In a sense, this section is analogous to a symbol table
[\[19\]][19] in a compiler.

`language`. [Language](#language) that defines the corresponding definition.

`kind`. Enumeration that defines the kind of the corresponding definition.
See [Languages](#language) for information on how definitions in supported
languages map onto these kinds.

<table>
  <tr>
    <td width="100px"><b>Value</b></td>
    <td><b>Name</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>19</code></td>
    <td><code>LOCAL</code></td>
    <td>Local value or variable, e.g. <code>val x = 42</code> or
    <code>var x = 42</code> inside a method.</td>
  </tr>
  <tr>
    <td><code>20</code></td>
    <td><code>FIELD</code></td>
    <td>Member value or variable, e.g. <code>val x = 42</code> or
    <code>var x = 42</code> inside a class or an object</td>
  </tr>
  <tr>
    <td><code>3</code></td>
    <td><code>METHOD</code></td>
    <td>Method, e.g. <code>def x = 42</code>.</td>
  </tr>
  <tr>
    <td><code>21</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td>Constructor, e.g. <code>(x: Int)</code> or
    <code>def this() = this(42)</code> in <code>class C(x: Int)</code>.</td>
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
    <td><code>17</code></td>
    <td><code>SELF_PARAMETER</code></td>
    <td>Self parameter, e.g. <code>self</code> in <code>class C { self => ... }</code>.</td>
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
  <tr>
    <td><code>18</code></td>
    <td><code>INTERFACE</code></td>
    <td>Interface, e.g. <code>interface I</code>.</td>
  </tr>
</table>

`properties`. Bitmask of miscellaneous bits of metadata.
See [Languages](#language) for information on how definitions in supported
languages map onto these properties.

<table>
  <tr>
    <td width="100px"><b>Value</b></td>
    <td><b>Name</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>0x1</code></td>
    <td><code>RESERVED</code></td>
    <td>Reserved for backward compatibility.</td>
  </tr>
  <tr>
    <td><code>0x2</code></td>
    <td><code>RESERVED</code></td>
    <td>Reserved for backward compatibility.</td>
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
    <td><code>VAL</code></td>
    <td>Is a <code>val</code> (local value, member value or
    <code>val</code> parameter of a constructor)?</td>
  </tr>
  <tr>
    <td><code>0x800</code></td>
    <td><code>VAR</code></td>
    <td>Is a <code>var</code> (local variable, member variable or
    <code>var</code> parameter of a constructor)?</td>
  </tr>
  <tr>
    <td><code>0x1000</code></td>
    <td><code>STATIC</code></td>
    <td>Is a <code>static</code> field, method or class?</td>
  </tr>
  <tr>
    <td><code>0x2000</code></td>
    <td><code>PRIMARY</code></td>
    <td>Is a primary constructor?</td>
  </tr>
</table>

`name`. String that represents the name of the corresponding definition.

`location`. [Location](#location) that represents the extent of
the corresponding definition.

`overrides`. Symbols that are extended or overridden by this symbol
either directly or transitively.

`tpe`. [Type](#type) that represents the definition signature.
See [Languages](#language) for more information on which definitions have
which signatures in supported languages.

`annotation`. [Annotations](#annotation) of the corresponding definition.

`accessibility`. [Accessibility](#accessibility) of the corresponding definition.

`owner`. See [Languages](#languages) for more information on which definitions
have which owners in supported languages.

### Annotation

```protobuf
message Annotation {
  Type tpe = 1;
}
```

`Annotation` represents annotations. See [Languages](#languages) for
information on how annotations in supported languages map onto this
data structure.

### Accessibility

```protobuf
message Accessibility {
  enum Tag {
    UNKNOWN_ACCESSIBILITY = 0;
    PRIVATE = 1;
    PRIVATE_THIS = 2;
    PRIVATE_WITHIN = 3;
    PROTECTED = 4;
    PROTECTED_THIS = 5;
    PROTECTED_WITHIN = 6;
    PUBLIC = 7;
  }
  Tag tag = 1;
  string symbol = 2;
}
```

`Accessibility` represents accessibility of definitions, including `PRIVATE` and
`PROTECTED`, as well as variants of these accessibilities: 1) limited to the
current object instance (`PRIVATE_THIS` and `PROTECTED_THIS`), and 2) limited
to the given `symbol` (`PRIVATE_WITHIN` and `PROTECTED_WITHIN`).
See [Languages](#languages) for information on how accessibilities in supported
languages map onto this data structure.

### SymbolOccurrence

```protobuf
message SymbolOccurrence {
  Range range = 1;
  string symbol = 2;
  Role role = 3;
}
```

"Occurrences" is a section of a [TextDocument](#textdocument) that represents
the results of name resolution for identifiers in the underlying code snippet.

`SymbolOccurrence` refers to a [Range](#range) in the
[TextDocument](#textdocument) and has a symbol as explained in
[Symbol](#symbol). `role` is an enumeration that describes the semantic role
that the identifier performs in the code.

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

There may be situations when: 1) a range references multiple symbols, e.g. when
an identifier in a Scala import refers to both a class and an object with the
same name, or 2) when a range defines multiple symbols, e.g. when a Scala `val`
or `var` gives rise to multiple symbols. In these situations, multiple symbols
must be packed into a [multi symbol](#symbol).

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
diagnostics as error, warning, information or hint.

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
the enclosing document, and its value is determined as follows:

* If the synthetic replaces a code snippet (e.g. if it represents an
  implicit conversion applied to an expression), then its range must be equal
  to that snippet's range.
* If the synthetic inserts new code (e.g. if it represents an inferred type
  argument or implicit argument), then its range must be an empty range specifying the insertion point.

`text` is a synthetic [TextDocument](#textdocument) that represents a synthetic
code snippet as follows:

* Its text contains a language-dependent string that represents the
  synthetic code snippet. See [Languages](#languages) for more information.
* Its sections, e.g. [Occurences](#symboloccurrence), contain semantic
  information associated with the synthetic code snippet.
* An occurrence of a placeholder symbol means that the synthetic code snippet
  includes the original code snippet located at `Synthetic.range`.

## Data Schemas

### Protobuf

[semanticdb3.proto][semanticdb3.proto]

## Languages

In this section, we describe language-dependent aspects of SemanticDB entities,
namely:
  * Format for global [Symbols](#symbol).
  * Supported [Annotations](#annotation).
  * Supported [Types](#type).
  * Kinds, properties, signatures and accessibilities of [SymbolInformation](#symbolinformation).
  * Supported [Accessibilities](#accessibility).
  * Format for [Synthetics](#synthetic).

We use a simple notation to describe SemanticDB entities.
In this notation, `M(v1, v2, ...)` corresponds a Protocol Buffers message
`M` with fields set to values `v1`, `v2`, etc. Literals correspond to
scalar values, `List(x1, x2, ...)` corresponds to repeated values, `None`
corresponds to missing optional values.
Moreover, `<X>` corresponds to an entity that represents `X`.

### Scala

In this section, we exhaustively map Scala language features onto SemanticDB.
As a reference, we use the Scala Language Specification [\[17\]][17]
(referred to as "SLS" in the text below), as well as additional resources
[[25][25], [40][40], [51][51], [56][56], [57][57]] in the areas where SLS
is incomplete or out of date.

<a name="scala-symbol"></a>
#### Symbol

In this section, we describe Scala symbol format, but don't cover the details
of how Scala definitions map onto symbols (e.g. which symbols are created for
which Scala definitions, what their metadata is, etc). See
[SymbolInformation](#scala-symbolinformation) for more information about that.

<table>
  <tr>
    <td><b>Symbols</b></td>
    <td><b>Format</b></td>
  </tr>
  <tr>
    <td>
      Global symbols
      <a href="#symbol">↑</a>
    </td>
    <td>
      Concatenation of definition descriptors of the owner chain.
    </td>
  </tr>
  <tr>
    <td>
      Local symbols
      <a href="#symbol">↑</a>
    </td>
    <td>
      Concatenation of <code>local</code> and a decimal number.
    </td>
  </tr>
  <tr>
    <td>
      Multi symbols
      <a href="#symbol">↑</a>
    </td>
    <td>
      Concatentation of underlying symbols interspersed with a
      semicolon (<code>;</code>). Within a multi symbol, the underlying
      symbols must be ordered lexicographically in ascending order.
    </td>
  </tr>
  <tr>
    <td>
      Placeholder symbols
      <a href="#symbol">↑</a>
    </td>
    <td>
      <code>*</code>
    </td>
  </tr>
</table>

**Owner chain** is:
  * For the root package [\[20\]][20], list that contains just that package.
  * For the empty package [\[21\]][21], list that contains just that package.
  * For top-level package, list that contains just that package.
  * For other global definition, owner chain of its owner appended
    by the original definition.
  * For other definition, empty list.

**Owner** is:
  * For the root package [\[20\]][20], `None`.
  * For the empty package [\[21\]][21], root package.
  * For top-level package, root package.
  * For other package, parent package.
  * For package object, its package, e.g. `package scala`
    for `package object scala`.
  * For other top-level definition, its package.
  * For other global definition, the innermost enclosing definition,
    i.e. the definition whose [Location](#location) in source code most
    tightly encloses the [Location](#location) of the original definition.
  * For other definition, `None`.

**Definition descriptor** is:
  * For `LOCAL`, `FIELD`, `OBJECT`, `PACKAGE` or `PACKAGE_OBJECT`,
    concatenation of its encoded name and a dot (`.`).
  * For `METHOD`, `CONSTRUCTOR`, or `MACRO`,
    concatenation of its encoded name, a left parenthesis (`(`),
    its type descriptor, a right parenthesis (`)`) and a dot (`.`).
    In the case when multiple methods have the same name and type descriptor,
    the type descriptor is appended with `+N`, with `+1` going to the method
    that is defined first in the source code, `+2` going to the method that
    is defined second, etc.
  * For `TYPE`, `CLASS` or `TRAIT`, concatenation of its
    encoded name and a pound sign (`#`).
  * For `PARAMETER`, concatenation of a left parenthesis (`(`), its
    encoded name and a right parenthesis (`)`).
  * For `SELF_PARAMETER`, unsupported.
  * For `TYPE_PARAMETER`, concatenation of a left bracket (`[`), its
    encoded name and a right bracket (`]`).
  * See [SymbolInformation](#scala-symbolinformation) for details on
    which Scala definitions are modelled by which symbols.

**Encoded name** is:
  * If name is a Java identifier [\[22\]][22], the name itself.
  * Otherwise, concatenation of a backtick, the name itself and
    another backtick.

**Name** is:
  * For the root package [\[20\]][20], `_root_`.
  * For the empty package [\[21\]][21], `_empty_`.
  * For package object, `package`.
  * For constructor, `<init>`.
  * For anonymous parameter, self parameter or type parameter,
    the underscore sign (`_`).
  * For other definition, the name of the binding introduced by the definition
    [\[70\]][70].

**Type descriptor** is:

  * For `TYPE_REF`, encoded name of `symbol`.
  * For `SINGLETON_TYPE`, `.type`.
  * For `STRUCTURAL_TYPE`, `{}`.
  * For `ANNOTATED_TYPE`, type descriptor of `tpe`.
  * For `EXISTENTIAL_TYPE`, type descriptor of `tpe`.
  * For `UNIVERSAL_TYPE`, type descriptor of `tpe`.
  * For `CLASS_INFO_TYPE`, unsupported.
  * For `METHOD_TYPE`, concatenation of type descriptors of
    its parameter types interspersed with a comma (`,`).
  * For `BY_NAME_TYPE`, concatenation of the arrow sign (`=>`) and
    the type descriptor of `tpe`.
  * For `REPEATED_TYPE`, concatenation of type descriptor of `tpe`
    and a star (`*`).
  * For `TYPE_TYPE`, unsupported.
  * See [Type](#scala-type) for details on
    which Scala types are modelled by which `Type` entities.

For example, this is how some of the definitions from the Scala standard library
must be modelled:

* The `scala` package: `scala.`
* The `Int` class: `scala.Int#`
* The `def implicitly[T](implicit e: T)` method: `scala.Predef.implicitly(T).`
* The `e` parameter of that method: `scala.Predef.implicitly(T).(e)`
* The `T` type parameter of that method: `scala.Predef.implicitly(T).[T]`
* The `def contains[A: Ordering](tree: Tree[A, _], x: A): Boolean` method:
  `scala.collection.immutable.RedBlackTree#contains(Tree,A,Ordering).`

<a name="scala-type"></a>
#### Type

In Scala, `Type` represents types [\[18\]][18].

In the examples below:
  * `E` is the lexically enclosing class of the location where the example
    types are defined or computed.
  * `C` is a class that extends a trait `M`.
  * `T`, `T1`, `T2`, etc are type aliases.
  * `t` is a type parameter.
  * `p` and `x` are local values.
  * `scala` is the `scala` package from the Scala standard library.
  * `Int`, `List`, `TupleN` and `FunctionN` are classes from the Scala
    standard library.
  * `@ann1`, `@ann2`, etc are annotations.
  * `M1`, `M2`, etc are members.
  * `ts` is a type parameter list that consists of
    type parameters `t1`, `t2`, etc.
  * `P1s`, `P2s`, etc are parameter lists that consists of parameter types
    `P11`, `P12`, etc.

<table>
  <tr>
    <td width="220px"><b>Category</b></td>
    <td><b>Examples</b></td>
  </tr>
  <tr>
    <td valign="top">Singleton types [<a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#singleton-types">24</a>, <a href="https://github.com/scala/scala/pull/5310">25</a>]</td>
    <td>
      <ul>
        <li><code>x.type</code> ~ <code>SingletonType(SYMBOL, None, &lt;x&gt;, None, None)</code>.</li>
        <li><code>p.x.type</code> ~ <code>SingletonType(SYMBOL, &lt;p.type&gt;, &lt;x&gt;, None, None)</code>.</li>
        <li><code>this.type</code> ~ <code>SingletonType(THIS, None, &lt;E&gt;, None, None)</code>.</li>
        <li><code>C.this.type</code> ~ <code>SingletonType(THIS, None, &lt;C&gt;, None, None)</code>.</li>
        <li>Type of <code>super</code> ~ <code>SingletonType(SUPER, &lt;E&gt;, None, None, None)</code>.</li>
        <li>Type of <code>super[M]</code> ~ <code>SingletonType(SUPER, &lt;E&gt;, &lt;M&gt;, None, None)</code>.</li>
        <li>Type of <code>C.super[M]</code> ~ <code>SingletonType(SUPER, &lt;C&gt;, &lt;M&gt;, None, None)</code>.</li>
        <li>Literal type ~ <code>SingletonType(&lt;TAG&gt;, None, None, &lt;value&gt;, None)</code> or <code>SingletonType(&lt;TAG&gt;, None, None, None, &lt;value&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Type projections <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#type-projection">[26]</a></td>
    <td>
      <ul>
        <li><code>T#C</code> ~ <code>TypeRef(&lt;T&gt;, &lt;C&gt;, List())</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Type designators <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#type-designators">[27]</a></td>
    <td>
      <ul>
        <li><code>t</code> ~ <code>TypeRef(None, &lt;t&gt;, List())</code>.</li>
        <li><code>Int</code> ~ <code>TypeRef(None, &lt;Int&gt;, List())</code>.</li>
        <li><code>scala.Int</code> ~ <code>TypeRef(None, &lt;Int&gt;, List())</code>.</li>
        <li><code>p.C</code> ~ <code>TypeRef(&lt;p.type&gt;, &lt;C&gt;, List())</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Parameterized types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#parameterized-types">[28]</a></td>
    <td>
      <ul>
        <li><code>T#C[T1, ..., Tn]</code> ~ <code>TypeRef(&lt;T&gt;, &lt;C&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
        <li><code>t[T1, ..., Tn]</code> ~ <code>TypeRef(None, &lt;t&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
        <li><code>List[Int]</code> ~ <code>TypeRef(None, &lt;List&gt;, List(&lt;Int&gt;))</code>.</li>
        <li><code>scala.List[Int]</code> ~ <code>TypeRef(None, &lt;List&gt;, List(&lt;Int&gt;))</code>.</li>
        <li><code>p.C[T1, ..., Tn]</code> ~ <code>TypeRef(&lt;p.type&gt;, &lt;C&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Tuple types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#tuple-types">[29]</a></td>
    <td>
      <ul>
        <li><code>(T1, ..., Tn)</code> ~ <code>TypeRef(None, &lt;TupleN&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Annotated types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#annotated-types">[30]</a></td>
    <td>
      <ul>
        <li><code>T @ann1 ... @annN</code> ~ <code>AnnotatedType(List(&lt;ann1&gt;, ..., &lt;annN&gt;), &lt;T&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Compound types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#compound-types">[31]</a></td>
    <td>
      <ul>
        <li><code>{ M1; ...; Mm }</code> ~ <code>StructuralType(None, List(&lt;M1&gt;, ..., &lt;Mm&gt;))</code>.</li>
        <li><code>T1 with ... with Tn</code> ~ <code>WithType(List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
        <li><code>T1 with ... with Tn { M1; ...; Mm }</code> ~ <code>StructuralType(WithType(List(&lt;T1&gt;, ..., &lt;Tn&gt;)), List(&lt;M1&gt;, ..., &lt;Mm&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Infix types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#infix-types">[32]</a></td>
    <td>
      <ul>
        <li><code>A T B</code> ~ <code>TypeRef(None, &lt;T&gt;, List(&lt;A&gt;, &lt;B&gt;))</code> for left-associative <code>T</code>.</li>
        <li><code>A T B</code> ~ <code>TypeRef(None, &lt;T&gt;, List(&lt;B&gt;, &lt;A&gt;))</code> for right-associative <code>T</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Function types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#function-types">[33]</a></td>
    <td>
      <ul>
        <li><code>(T1, ..., Tn) =&gt; T</code> ~ <code>TypeRef(None, &lt;FunctionN&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;, &lt;T&gt;))</code>.</li>
        <li><code>=&gt; Ti</code> ~ <code>ByNameType(&lt;Ti&gt;)</code>.</li>
        <li><code>Ti*</code> ~ <code>RepeatedType(&lt;Ti&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Existential types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#existential-types">[34]</a></td>
    <td>
      <ul>
        <li><code>T forSome { M1; ...; Mm }</code> ~ <code>ExistentialType(List(&lt;M1&gt;, ..., &lt;Mm&gt;), List(&lt;T&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Method types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#method-types">[35]</a></td>
    <td>
      <ul>
        <li><code>(P1s)...(Pns)T</code> ~ <code>MethodType(List(), List(List(&lt;P11&gt;, ...), ..., List(&lt;Pn1&gt;, ...)), &lt;T&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Polymorphic method types <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#polymorphic-method-types">[36]</a></td>
    <td>
      <ul>
        <li><code>[ts](P1s)...(Pns)T</code> ~ <code>MethodType(List(&lt;t1&gt;, ..., &lt;tm&gt;), List(List(&lt;P11&gt;, ...), ..., List(&lt;Pn1&gt;, ...)), &lt;T&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Type constructors <a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#type-constructors">[37]</a></td>
    <td>
      <ul>
        <li><code>[ts]T</code> ~ <code>UniversalType(List(&lt;t1&gt;, ..., &lt;tm&gt;), &lt;T&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
</table>

Notes:
* We diverge from SLS on the matter of handling prefixes (see definitions of
  `TYPE_REF` and `SINGLETON_TYPE` for more information).
  * In SLS, all types that can have a prefix must have it specified
    explicitly, even if the prefix is trivial. For example in Scalac, `Int` must
    be represented as `TypeRef(<scala.this.type>, <Int>, List())` [\[27\]][27].
  * In SemanticDB, all types that have a trivial prefix must not have it
    specified explicitly. For example in SemanticDB, `Int` must be represented
    as `TypeRef(None, <Int>, List())`. Moreover, even `scala.Int` must be
    represented as `TypeRef(None, <Int>, List())`.
* We leave the mapping between type syntax written in source code and
  `Type` entities deliberately unspecified. Some producers may transform
  types in unspecified ways (e.g. Scalac transforms all `this.type` types
  into qualified `X.this.type` types), and our experience [\[38\]][38] shows
  that reverse engineering these transformations is very hard. We may improve
  on this in the future, but this is highly unlikely. In the meanwhile,
  use [Occurrences](#symboloccurrence) for figuring out semantics of syntax
  written in source code.

<a name="scala-symbolinformation"></a>
#### SymbolInformation

**Value declarations and definitions** [\[39\]][39] are represented by multiple
symbols, with the exact number of symbols, their kinds, properties, signatures
and accessibilities dependent on the corresponding value:
* Local symbol of kind `LOCAL` is created for all local values.
* Field symbol of kind `FIELD` is created for non-`ABSTRACT` member values
  to model the field generated by the Scala compiler.
* Getter symbol of kind `METHOD` is created for all non-`PRIVATE_THIS` member
  values to model the corresponding getter method generated by the Scala
  compiler.
* Parameter symbol of kind `PARAMETER` is created for `val` parameters
  of primary constructors to model the corresponding constructor parameter.

```scala
abstract class C(val xp: Int) {
  val xm: Int = ???
  val xam: Int
  private[this] val xlm: Int = ???
  def m = {
    val xl: Int = ???
    type S = { val xs: Int }
    type E = xe.type forSome { val xe: AnyRef }
  }
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td width="275px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>xp</code></td>
    <td><code>_empty_.C#xp.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>xp</code></td>
    <td><code>_empty_.C#xp().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xp</code></td>
    <td><code>_empty_.C#`&lt;init&gt;`(Int).(xp)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>xm</code></td>
    <td><code>_empty_.C#xm.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>xm</code></td>
    <td><code>_empty_.C#xm().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xam</code></td>
    <td><code>_empty_.C#xam().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xlm</code></td>
    <td><code>_empty_.C#xlm.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>xl</code></td>
    <td><code>local0</code></td>
    <td><code>LOCAL</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>xs</code></td>
    <td><code>local1</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xe</code></td>
    <td><code>local2</code></td>
    <td><code>METHOD</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
</table>

Notes:
* As described in SLS [\[39\]][39], there are some language constructs
  that are desugared into values. For these language constructs,
  symbols are created from desugared values.
    * `val p = e` (symbols are created for bound variables `x1`, ...,
      `xm` that are defined in `p` in order of their appearance in source code;
      symbols are NOT created for the synthetic value used in the desugaring).
    * `val x1, ..., xn: T` (symbols are created for `x1`, ..., `xn`
      in order of their appearance in source code).
    * `val p1, ..., pn = e` (symbols are created for bound variables `x1`, ...,
      `xm` that are defined in patterns `p1`, ..., `pn` in order of their
      appearance in source code).
    * `val p1, ..., pn: T = e` (symbols are created for bound variables `x1`,
       ..., `xm` that are defined in patterns `p1`, ..., `pn` in order of their
       appearance in source code).
* Supported properties for value symbols are:
  * `ABSTRACT`: set for all corresponding symbols of value declarations.
  * `FINAL`: set for all corresponding symbols of `final` values.
  * `IMPLICIT`:
    * If a corresponding parameter symbol exists, set for the parameter symbol.
    * If a corresponding getter symbol exists, set for the getter symbol.
    * If a corresponding local symbol exists, set for the local symbol.
  * `LAZY`: set for all corresponding symbols of `lazy` values.
  * `VAL`: set for all corresponding symbols.
* `override` relationships exist only for getter symbols.
  Corresponding local, field and parameter symbols never override or
  get overridden.
* If the type of the value is unspecified, it is inferred from the
  right-hand side of the value according to the rules described
  in SLS [\[39\]][39]. Corresponding signature is computed from the inferred
  type as explained in [Type](#scala-type).
* Depending on their meta annotations, value annotations may end up as
  `Annotation` entities associated with multiple corresponding symbols.
  See [\[40\]][40] for more information.
* Supported accessibilities for value symbols are:
  * `PRIVATE`: set for getters of `private` values.
  * `PRIVATE_THIS`: set for vals of value members.
  * `PRIVATE_WITHIN`: set for getters of `private[...]` values.
  * `PROTECTED`: set for getters of `protected` values.
  * `PROTECTED_THIS`: set for getters of `protected[this]` values.
  * `PROTECTED_WITHIN`: set for getters of `protected[...]` values.

**Variable declarations and definitions** [\[41\]][41] are represented by
multiple symbols, with the exact number of symbols, their kinds, properties,
signatures and accessibilities dependent on the corresponding value:
* Local symbol of kind `LOCAL` is created for all local variables.
* Field symbol of kind `FIELD` is created for non-`ABSTRACT` member variables
  to model the field generated by the Scala compiler.
* Getter and setter symbols of kind `METHOD` are created for all
  non-`PRIVATE_THIS` member variables to model the corresponding
  getter and setter methods generated by the Scala compiler.
* Parameter symbol of kind `PARAMETER` is created for `var` parameters
  of primary constructors to model the corresponding constructor parameter.

Notes:
* Variable symbols are modelled exactly the same as value symbols
  (see "Value symbols and declarations"), with the exceptions described below.
* Setter symbols have the following metadata:
  * `kind`: `METHOD`.
  * `properties`: see below.
  * `name`: concatenation of the name of the variable followed by `_=`.
  * `tpe`: `MethodType(List(), List(List(<x$1>)), <Unit>)`, where `x$1` is
    a `PARAMETER` symbol having `tpe` equal to the type of the variable.
  * `annotations` and `accessibility`: same as value symbols.
* Supported properties for variable symbols are:
  * `ABSTRACT`: set for all corresponding symbols of variable declarations.
  * `FINAL`: set for all corresponding symbols of `final` variables.
  * `IMPLICIT`:
    * If a corresponding parameter symbol exists, set for the parameter symbol.
    * If a corresponding getter symbol exists, set for the getter symbol.
    * If a corresponding local symbol exists, set for the local symbol.
  * `LAZY`: never set for variable symbols, since variable declarations and
    definitions cannot be `lazy`.
  * `VAR`: set for all corresponding symbols.
* `override` relationships exist only for getter and setter symbols.
  Corresponding local, field and parameter symbols never override or
  get overridden.

**Pattern variables** [\[65\]][65] are represented differently depending
on their location:
* Local symbol is created for pattern variables in pattern matching
  expressions [\[66\]][66].
* A combination of local, field, getter and setter symbols is created
  for pattern variables in pattern definitions [\[39\]][39].

```scala
class C {
  ??? match { case List(x) => ??? }
  val List(xval) = ???
  var List(xvar) = ???
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>x</code></td>
    <td><code>local0</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;Nothing&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>xval</code></td>
    <td><code>_empty_.C#xval.</code></td>
    <td><code>LOCAL</code></td>
    <td><code>TypeRef(None, &lt;Nothing&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>xval</code></td>
    <td><code>_empty_.C#xval().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Nothing&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xvar</code></td>
    <td><code>_empty_.C#xvar.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;Nothing&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>xvar</code></td>
    <td><code>_empty_.C#xvar().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Nothing&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xvar</code></td>
    <td><code>_empty_.C#xvar_=(Nothing).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(&lt;x$1&gt;), TypeRef(None, &lt;Unit&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* In the future, we may decide to introduce a dedicated symbol kind
  for regular pattern variables, so that they can be distinguished from
  local value definitions.
* Pattern variable symbols don't support any properties.
* Pattern definitions [\[39\]][39] do not exist as a first-class language
  feature. Instead, they are desugared into zero or more synthetic value
  definitions and only then encoded into symbols as described in
  "Value declarations and definitions" and "Variable declarations and
  definitions".
* Pattern variable symbols don't support any accessibilities.

**Type declarations and type aliases** [\[42\]][42] are represented with
`TYPE` symbols.

```scala
class C {
  type T1 <: Hi
  type T2 >: Lo
  type T = Int
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>T1</code></td>
    <td><code>_empty_.C#T1#</code></td>
    <td><code>TYPE</code></td>
    <td><code>TypeType(List(), None, &lt;Hi&gt;)</code></td>
  </tr>
  <tr>
    <td><code>T2</code></td>
    <td><code>_empty_.C#T2#</code></td>
    <td><code>TYPE</code></td>
    <td><code>TypeType(List(), &lt;Lo&gt;, None)</code></td>
  </tr>
  <tr>
    <td><code>T</code></td>
    <td><code>_empty_.C#T#</code></td>
    <td><code>TYPE</code></td>
    <td><code>TypeType(List(), TypeRef(None, &lt;Int&gt;, List()), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Supported properties for type symbols are:
  * `ABSTRACT`: set for type declarations.
  * `FINAL`: set for `final` type aliases.
* We leave the mapping between type syntax written in source code and
  `Type` entities deliberately unspecified. For example, a producer may
  represent the signature of `T1` as `TypeType(List(), <Nothing>, <Hi>)`.
  See [Types](#scala-type) for more information.
* If present, type parameters of type declarations and type aliases are
  represented as described below in order of their appearance in source code.
* Type symbols support [all Scala accessibilities](#scala-accessibility).

**Type variables** [\[67\]][67] are represented with `TYPE` symbols.

```scala
class C {
  ??? match { case _: List[t] => }
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>t</code></td>
    <td><code>local0</code></td>
    <td><code>TYPE</code></td>
    <td><code>TypeType(List(), None, None)</code></td>
  </tr>
</table>

Notes:
* In the future, we may decide to introduce a dedicated symbol kind
  for type variables, so that they can be distinguished from
  local type definitions.
* Type variable symbols are always `ABSTRACT`.
* We leave the mapping between type syntax written in source code and
  `Type` entities deliberately unspecified. For example, a producer may
  represent the signature of `t` as `TypeType(List(), <Nothing>, <Any>)`.
  See [Types](#scala-type) for more information.
* Type variable symbols don't support any accessibilities.

**Self parameters** [\[64\]][64] are represented with `SELF_PARAMETER` symbols.

```scala
class C1 {
  self1 =>
}

class C2 {
  self2: T =>
}
```
<table>
  <tr>
    <td><b>Definition</b></td>
    <td><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>self1</code></td>
    <td><code>local0</code></td>
    <td><code>SELF_PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;C1&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>self2</code></td>
    <td><code>local0</code></td>
    <td><code>SELF_PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;T&gt;, List())</code></td>
  </tr>
</table>

Notes:
* Self parameters cannot be referenced outside the document where they are
  located, which means that they are represented by local symbols.
* Self parameter symbols don't support any properties.
* We leave the mapping between type syntax written in source code and
  `Type` entities deliberately unspecified. For example, a producer may
  represent the signature of `self2` as
  `StructuralType(List(), List(<C2>, <T>), List())`.
  See [Types](#scala-type) for more information.
* Self parameter symbols don't support any accessibilities.

**Type parameters** [\[43\]][43] are represented with `TYPE_PARAMETER` symbols.

```scala
class C[T1] {
  def m[T2[T3] <: Hi] = ???
  type T[T4 >: Lo] = ???
}
```
<table>
  <tr>
    <td><b>Definition</b></td>
    <td><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>T1</code></td>
    <td><code>_empty_.C#[T1]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeType(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>T2</code></td>
    <td><code>_empty_.C#m()[T2]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeType(List(), None, &lt;Hi&gt;)</code></td>
  </tr>
  <tr>
    <td><code>T3</code></td>
    <td><code>_empty_.C#m()[T2][T3]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeType(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>T4</code></td>
    <td><code>_empty_.C#T#[T4]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeType(List(), &lt;Lo&gt;, None)</code></td>
  </tr>
</table>

Notes:
* Supported properties for type parameter symbols are:
  * `COVARIANT`: set for covariant type parameters.
  * `CONTRAVARIANT`: set for contravariant type parameters.
* If present, (higher-order) type parameters of type parameters are
  represented as described here in order of their appearance in source code.
* We leave the mapping between type syntax written in source code and
  `Type` entities deliberately unspecified. For example, a producer may
  represent the signature of `T1` as `TypeType(List(), <Nothing>, <Any>)`.
  See [Types](#scala-type) for more information.
* If present, context bounds and value bounds of type parameters are desugared
  into parameters of the enclosing definition as described in [\[44\]][44] and
  are represented with corresponding `PARAMETER` symbols.

**Parameters** are represented with `PARAMETER` symbols. (There is no section in SLS dedicated to parameters, so we aggregate information about parameters
from multiple sections).

```scala
class C(p1: Int) {
  def m2(p2: Int) = ???
  def m3(p3: Int = 42) = ???
  def m4(p4: => Int) = ???
  def m5(p5: Int*) = ???
  def m6[T: C <% V] = ???
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td width="275px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>p1</code></td>
    <td><code>_empty_.C#`&lt;init&gt;`(Int).(p1)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>p2</code></td>
    <td><code>_empty_.C#m2(Int).(p2)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>p3</code></td>
    <td><code>_empty_.C#m3(Int).(p3)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>m3$default$1</code></td>
    <td><code>_empty_.C#m3$default$1().</code></td>
    <td><code>METHOD</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>p4</code></td>
    <td><code>_empty_.C#m4(=>Int).(p4)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ByNameType(TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>p5</code></td>
    <td><code>_empty_.C#m5(Int*).(p5)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>RepeatedType(TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td>Context bound</td>
    <td><code>_empty_.C#m6(C,V).(x$1)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;C&gt;, List(&lt;T&gt;))</code></td>
  </tr>
  <tr>
    <td>View bound</td>
    <td><code>_empty_.C#m7(C,V).(x$2)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;Function1&gt;, List(&lt;T&gt;, &lt;V&gt;))</code></td>
  </tr>
</table>

Notes:
* As described above, some values and variables are represented with multiple
  symbols, including parameter symbols. For more information, see
  "Value declarations and definitions" and "Variable declarations and
  definitions".
* Supported properties for parameter symbols are:
  * `IMPLICIT`: set for `implicit` parameters, as well as desugared context
    bounds and view bounds (see above).
  * `VAL`: set for `val` parameters of primary constructors.
  * `VAR`: set for `var` parameters of primary constructors.
* Unlike some other metaprogramming systems for Scala, we do not
  distinguish regular parameters from parameters with default arguments
  [\[45\]][45]. However, we do create method symbols for synthetic methods
  that compute default arguments with names and signatures defined
  by [\[45\]][45].
* Signatures of by-name parameters [\[46\]][46] and repeated parameters
  [\[47\]][47] are represented with special types (`ByNameType` and
  `RepeatedType` correspondingly).
* According to [\[44\]][44], context bounds and view bounds are desugared
  as parameters of enclosing definitions. Since SLS does not specify
  the names for such parameters (only their signatures), we also leave
  the names unspecified.

**Function declarations and definitions** [\[48\]][48] are represented
with `METHOD` symbols.

```scala
abstract class C {
  def m1: Int = ???
  def m2(): Int = ???
  def m3(x: Int): Int = ???
  def m4(x: Int)(y: Int): Int = ???
}
```
<table>
  <tr>
    <td><b>Definition</b></td>
    <td><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>m1</code></td>
    <td><code>_empty_.C#m1().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m2</code></td>
    <td><code>_empty_.C#m2().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(List()), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>_empty_.C#m3(Int).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(List(&lt;x&gt;)), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m4</code></td>
    <td><code>_empty_.C#m4(Int,Int).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(List(&lt;x&gt;), List(&lt;y&gt;)), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* According to SLS, some language features involve synthetic methods that
  are not written in source code. Symbols for synthetic methods must be included
  in SemanticDB payloads alongside normal methods. Detailed information about
  synthetic methods is provided in various subsections of
  [SymbolInformation](#scala-symbolinformation) together with related language
  features, and here we provide a comprehensive list of such methods:
    * Getters for vals and vars.
    * Setters for vals and vars.
    * Methods that compute default arguments.
    * Methods synthesized for `case` classes and objects.
    * Implicit methods synthesized for `implicit` classes.
    * Methods synthesized for value classes.
* Supported properties for method symbols are:
  * `ABSTRACT`: set for function declarations.
  * `FINAL`: set for `final` methods.
  * `IMPLICIT`: set for `implicit` methods.
* If present, type parameters of methods are
  represented as described above in order of their appearance in source code.
* If present, parameters of methods are
  represented as described above in order of their appearance in source code.
* For procedures [\[49\]][49], the return type is assumed to be `Unit`.
  Corresponding signature is computed using the assumed retyrb
  type as explained in [Type](#scala-type).
* If the return type is unspecified, it is inferred from the
  right-hand side of the method according to the rules described
  in SLS [\[50\]][50]. Corresponding signature is computed using the inferred
  retyrb type as explained in [Type](#scala-type).
* Method symbols support [all Scala accessibilities](#scala-accessibility).

**Macro definitions** [\[51\]][51] are represented with `MACRO` symbols
similarly to function definitions (see above).

```scala
object M {
  def m: Int = macro impl
}

```
<table>
  <tr>
    <td><b>Definition</b></td>
    <td><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>m1</code></td>
    <td><code>_empty_.M.m().</code></td>
    <td><code>MACRO</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Supported properties for macro symbols are the same as for method symbols,
  except for `ABSTRACT` because macros cannot be `abstract`.
* Return type inference for macros is not supported.
* At the moment, `SymbolInformation` for macros does not contain information
  about corresponding macro implementations. We intend to improve this in the
  future.
* Macro symbols support [all Scala accessibilities](#scala-accessibility).

**Constructors** [[52][52], [53][53]] are represented with `CONSTRUCTOR`
symbols similarly to function definitions (see above).

```scala
class C(x: Int) {
  def this() = this(42)
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td>Primary constructor</td>
    <td><code>_empty_.C#`&lt;init&gt;`(Int).</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td><code>MethodType(List(), List(List(&lt;x&gt;)), TypeRef(None, &lt;C&gt;, List()))</code></td>
  </tr>
  <tr>
    <td>Secondary constructor</td>
    <td><code>_empty_.C#`&lt;init&gt;`().</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;C&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Unlike some other metaprogramming systems for Scala, we do not create
  synthetic constructor symbols for traits and objects.
* Supported properties for constructor symbols are:
  * `PRIMARY`: set for primary constructors.
* Constructors don't have type parameters and return types, but we still
  represent their signatures with `MethodType`. In these signatures,
  type parameters are equal to `List()` and the return type
  is the type of the enclosing class parameterized with references to its
  type parameters.
* Primary constructor parameters with `val` and `var` modifiers give rise
  to multiple different symbols as described above.
* Constructor symbols support [all Scala accessibilities](#scala-accessibility).

**Class definitions** [\[54\]][54] are represented with `CLASS` symbols.

```scala
class C[T](x: T, val y: T, var z: T) extends B with X {
  def m: Int = ???
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td width="280px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>C</code></td>
    <td><code>_empty_.C#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(&lt;T&gt;), List(&lt;B&gt;, &lt;X&gt;), List(&lt;x&gt;, &lt;y&gt;, &lt;y&gt;, &lt;z&gt;, &lt;z&gt;, &lt;z_=&gt;, &lt;&lt;init&gt;&gt;, &lt;m&gt;))</code></td>
  </tr>
  <tr>
    <td><code>T</code></td>
    <td><code>_empty_.C#[T]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeType(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>x</code></td>
    <td><code>_empty_.C#x.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;T&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>y</code></td>
    <td><code>_empty_.C#y.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;T&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>y</code></td>
    <td><code>_empty_.C#x().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>z</code></td>
    <td><code>_empty_.C#z.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;T&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>z</code></td>
    <td><code>_empty_.C#z().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>z</code></td>
    <td><code>_empty_.C#z_=(T).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(List(&lt;x$1&gt;)), TypeRef(None, &lt;Unit&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>z</code></td>
    <td><code>_empty_.C#z_=(T).(x$1)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;T&gt;, List())</code></td>
  </tr>
  <tr>
    <td>Primary constructor</td>
    <td><code>_empty_.C#`&lt;init&gt;`(T,T,T).</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td><code>TypeRef(None, &lt;Int&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>x</code></td>
    <td><code>_empty_.C#`&lt;init&gt;`(T,T,T).(x)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;T&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>y</code></td>
    <td><code>_empty_.C#`&lt;init&gt;`(T,T,T).(y)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;T&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>z</code></td>
    <td><code>_empty_.C#`&lt;init&gt;`(T,T,T).(z)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;T&gt;, List())</code></td>
  </tr>
  <tr>
    <td>m</td>
    <td><code>_empty_.C#m().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Supported properties for class symbols are:
  * `ABSTRACT`: set for `abstract` classes.
  * `FINAL`: set for `final` classes.
  * `SEALED`: set for `sealed` classes.
  * `IMPLICIT`: set for `implicit` classes.
  * `CASE`: set for `case` classes.
* We leave the mapping between parent syntax written in source code and
  `ClassInfoType.parents` deliberately unspecified. Some producers are known
  to insert `<AnyRef>` into `parents` under certain circumstances, so we can't
  guarantee a one-to-one mapping of parent clauses in source code and
  entities in `parents`. We may improve on this in the future.
* `ClassInfoType.declarations` must be ordered as follows:
  * For every parameter of the primary constructor, its field symbol, then
    its getter symbol, then its setter symbol.
  * Symbol of the primary constructor.
  * Symbols of declared members in order of their appearance in source code.
    (Inherited members must not be part of `declarations`.)
  * Synthetic symbols in unspecified order and positions in the declarations
    list. We may provide more structure here in the future.
* In some cases, SLS and its extensions mandate generation of synthetic members
  and/or companions for certain classes. Symbols for such synthetic definitions
  must be included in SemanticDB payloads alongside normal definitions. For
  details, see:
    * Case classes [\[55\]][55].
    * Implicit classes [\[56\]][56].
    * Value classes [\[57\]][57].
* Class symbols support [all Scala accessibilities](#scala-accessibility).

**Traits** [\[58\]][58] are represented by `TRAIT` symbols
similarly to class definitions (see above). Concretely, the differences
between trait symbols and class symbols are:
* Trait symbols only support `SEALED` property.
* Traits don't have constructors.

**Object definitions** [\[59\]][59] are represented by `OBJECT` symbols
similarly to class definitions (see above). Concretely, the differences
between object symbols and class symbols are:
* Object symbols are always `FINAL`.
* Apart from `FINAL`, object symbols only support `CASE` and `IMPLICIT`
  properties.
* Objects don't have type parameters, but we still represent their signatures
  with `ClassInfoType`. In these signatures, type parameters are equal
  to `List()`.
* Objects don't have constructors.

**Package objects** [\[60\]][60] are represented by `PACKAGE_OBJECT` symbols
similarly to object definitions (see above). Concretely, the differences
between package object symbols and object symbols are:
* Package object symbols are always `FINAL`.
* Apart from `FINAL`, package object symbols don't support any properties.
* Package objects don't have annotations.
* Package objects don't support any accessibilities.

**Packages** [\[61\]][61] are represented by `PACKAGE` symbols.
In Scala, `SymbolInformation` for `PACKAGE` symbols is very modest -
the only non-empty fields must be:
  * `symbol` (as described in [Symbol](#scala-symbol)).
  * `language` (`SCALA`).
  * `kind` (`PACKAGE`).
  * `name` (as described in [Symbol](#scala-symbol)).
  * `owner` (as described in [Symbol](#scala-symbol)).

<a name="scala-annotation"></a>
#### Annotation

In Scala, `Annotation` represents annotations [\[23\]][23].

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
  <tr>
    <td>Not supported</td>
    <td>Expression annotation, e.g. <code>e: @ann</code>.</td>
  </tr>
</table>

* At the moment, `Annotation` can't represent annotation arguments,
  which means that the annotation in `@ann(x, y, z) def m: T` is
  represented as `Annotation(<ann>)`. We intend to improve on this
  in the future.
* At the moment, SemanticDB cannot represent expressions, which means
  that it cannot represent expression annotations as well. We do not
  plan to add support for expressions in SemanticDB, so it is highly
  unlikely that expression annotations will be supported in the future.

<a name="scala-accessibility"></a>
#### Accessibility

In Scala, `Accessibility` represents accessibility of definitions.

<table>
  <tr>
    <td><b>Accessibility</b></td>
    <td><b>Code</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>PRIVATE</code></td>
    <td><code>private def x = ???</code></td>
    <td>
      Can be accessed only from within the directly enclosing template
      and its companion object or companion class
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#private">[62]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PRIVATE_THIS</code></td>
    <td><code>private[this] def x = ???</code></td>
    <td>
      Can be accessed only from within the object in which the definition
      is defined.
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#private">[62]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PRIVATE_WITHIN</code></td>
    <td><code>private[X] def x = ???</code></td>
    <td>
      Can be accessed respectively only from code inside the package
      <code>X</code> or only from code inside the class <code>X</code>
      and its companion object.
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#private">[62]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PROTECTED</code></td>
    <td><code>protected def x = ???</code></td>
    <td>
      Can be accessed from within: 1) the template of the defining class,
      2) all templates that have the defining class as a base class,
      3) the companion object of any of those classes.
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#protected">[63]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PROTECTED_THIS</code></td>
    <td><code>protected[this] def x = ???</code></td>
    <td>
      Can be accessed as <code>PROTECTED</code> AND
      only from within the object in which the definition is defined.
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#protected">[63]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PROTECTED_WITHIN</code></td>
    <td><code>protected[X] def x = ???</code></td>
    <td>
      Can be accessed as <code>PROTECTED</code> OR
      from code inside the package <code>X</code> or from code inside
      the class <code>X</code> and its companion object.
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#protected">[63]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PUBLIC</code></td>
    <td><code>def x = ???</code></td>
    <td>None of the above.</td>
  </tr>
</table>

Notes:
* Not all kinds of symbols support all accessibilities. See
  [SymbolInformation](#scala-symbolinformation) for more information.

<a name="scala-synthetic"></a>
#### Synthetic

In Scala, we leave the synthetic format deliberately unspecified.
Synthetics are unspecified in SLS, and our experience [\[38\]][38] shows that
reverse engineering synthetics is very hard. We may improve
on this in the future, but this is highly unlikely.

### Java

Java `class` file format [\[68\]][68] can be mapped onto SemanticDB.
This has been validated in an experimental implementation [\[69\]][69],
but we do not yet have a specification that comprehensively describes how
Java language features map onto SemanticDB. We intend to improve on this
in the future.

[semanticdb2.proto]: https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb2/semanticdb2.proto
[semanticdb3.proto]: semanticdb3.proto
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
[17]: https://www.scala-lang.org/files/archive/spec/2.12/
[18]: https://www.scala-lang.org/files/archive/spec/2.11/03-types.html
[19]: https://en.wikipedia.org/wiki/Symbol_table
[20]: https://www.scala-lang.org/files/archive/spec/2.12/09-top-level-definitions.html#package-references
[21]: https://www.scala-lang.org/files/archive/spec/2.12/09-top-level-definitions.html#packagings
[22]: https://docs.oracle.com/javase/specs/jls/se9/html/jls-3.html#jls-3.8
[23]: https://www.scala-lang.org/files/archive/spec/2.12/11-annotations.html
[24]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#singleton-types
[25]: https://github.com/scala/scala/pull/5310
[26]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#type-projection
[27]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#type-designators
[28]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#parameterized-types
[29]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#tuple-types
[30]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#annotated-types
[31]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#compound-types
[32]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#infix-types
[33]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#function-types
[34]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#existential-types
[35]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#method-types
[36]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#polymorphic-method-types
[37]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#type-constructors
[38]: http://scalamacros.org/paperstalks/2016-02-11-WhatDidWeLearnInScalaMeta.pdf
[39]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#value-declarations-and-definitions
[40]: http://www.scala-lang.org/api/2.12.0/scala/annotation/meta/index.html
[41]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#variable-declarations-and-definitions
[42]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#type-declarations-and-type-aliases
[43]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#type-parameters
[44]: https://www.scala-lang.org/files/archive/spec/2.12/07-implicits.html#context-bounds-and-view-bounds
[45]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#default-arguments
[46]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#by-name-parameters
[47]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#repeated-parameters
[48]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#function-declarations-and-definitions
[49]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#procedures
[50]: https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#method-return-type-inference
[51]: https://docs.scala-lang.org/overviews/macros/overview.html
[52]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#class-definitions
[53]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#constructor-definitions
[54]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#class-definitions
[55]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#case-classes
[56]: https://docs.scala-lang.org/overviews/core/implicit-classes.html
[57]: https://docs.scala-lang.org/overviews/core/value-classes.html
[58]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#traits
[59]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#object-definitions
[60]: https://www.scala-lang.org/files/archive/spec/2.12/09-top-level-definitions.html#package-objects
[61]: https://www.scala-lang.org/files/archive/spec/2.12/09-top-level-definitions.html#packagings
[62]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#private
[63]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#protected
[64]: https://www.scala-lang.org/files/archive/spec/2.11/05-classes-and-objects.html#templates
[65]: https://www.scala-lang.org/files/archive/spec/2.11/08-pattern-matching.html#variable-patterns
[66]: https://www.scala-lang.org/files/archive/spec/2.11/08-pattern-matching.html#pattern-matching-expressions
[67]: https://www.scala-lang.org/files/archive/spec/2.11/08-pattern-matching.html#type-patterns
[68]: https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html
[69]: https://github.com/scalameta/scalameta/blob/master/semanticdb/metacp/src/main/scala/scala/meta/internal/javacp/Javacp.scala
[70]: https://www.scala-lang.org/files/archive/spec/2.12/02-identifiers-names-and-scopes.html
