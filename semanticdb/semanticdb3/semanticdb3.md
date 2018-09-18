# SemanticDB Specification

* [Motivation](#motivation)
* [Data Model](#data-model)
  * [TextDocument](#textdocument)
  * [Language](#language)
  * [URI](#uri)
  * [Range](#range)
  * [Location](#location)
  * [Symbol](#symbol)
  * [Scope](#scope)
  * [Constant](#constant)
  * [Type](#type)
  * [Signature](#signature)
  * [SymbolInformation](#symbolinformation)
  * [Annotation](#annotation)
  * [Access](#access)
  * [SymbolOccurrence](#symboloccurrence)
  * [Diagnostic](#diagnostic)
  * [Synthetic](#synthetic)
  * [Tree](#tree)
* [Data Schemas](#data-schemas)
  * [Protobuf](#protobuf)
* [Languages](#languages)
* [Scala](#scala)
  * [Symbol](#scala-symbol)
  * [Type](#scala-type)
  * [Signature](#scala-signature)
  * [SymbolInformation](#scala-symbolinformation)
  * [Annotation](#scala-annotation)
  * [Access](#scala-access)
  * [SymbolOccurrence](#scala-symboloccurrence)
  * [Synthetic](#scala-synthetic)
* [Java](#java)
  * [Symbol](#java-symbol)
  * [Type](#java-type)
  * [Signature](#java-signature)
  * [SymbolInformation](#java-symbolinformation)
  * [Annotation](#java-annotation)
  * [Access](#java-access)
  * [SymbolOccurrence](#java-symboloccurrence)
  * [Synthetic](#java-synthetic)

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

SemanticDB is a data model for semantic information such as symbols and types
about programs in Scala and other languages. SemanticDB decouples production
and consumption of semantic information, establishing documented means for
communication between tools.

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
  reserved 4, 8, 9;
  Schema schema = 1;
  string uri = 2;
  string text = 3;
  string md5 = 11;
  Language language = 10;
  repeated SymbolInformation symbols = 5;
  repeated SymbolOccurrence occurrences = 6;
  repeated Diagnostic diagnostics = 7;
  repeated Synthetic synthetics = 12;
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
    <td><a href="https://github.com/scalameta/scalameta/blob/v3.0.0/semanticdb/semanticdb2/semanticdb2.proto">semanticdb2.proto</a></td>
  </tr>
  <tr>
    <td><code>SEMANTICDB3</code></td>
    <td>SemanticDB v3 payloads</td>
    <td><a href="https://github.com/scalameta/scalameta/blob/v3.7.4/semanticdb/semanticdb3/semanticdb3.proto">semanticdb3.proto</a></td>
  </tr>
  <tr>
    <td><code>SEMANTICDB4</code></td>
    <td>SemanticDB v4 payloads</td>
    <td><a href="https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb/semanticdb.proto">semanticdb.proto</a></td> (described in this document)
  </tr>
</table>

`uri` defines the relative URI encoded path to the text document. The path is
relativized by the project sourceroot, which by convention is the root directory
of the project's workspace.

`text` optionally defines the full string contents of the text document. When
`text` is empty, the combination of the `uri` field and a sourceroot enables
tools to retrieve the text document contents from disk.

`md5` defines the hexadecimal formatted MD5 fingerprint of the source file
contents. When `text` is empty, the MD5 fingerprint can be used to ensure
a SemanticDB payload is up-to-date with the file contents on disk.

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
entities, e.g. [TextDocument](#textdocument) or [Symbol](#symbol). Currently,
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
to accompany global symbols with out-of-band metadata.

**Local symbols**. Correspond to a definition that isn't global (see above).

Local symbol format is language-agnostic and is a concatenation of `local`,
a decimal number and an optional suffix that consists of a plus (`+`) and
another decimal number. For example, `x` in a Scala method
`def identity[T](x: T): T` may be modelled by local symbols `local0`, `local1`,
`local2+1`, etc. The same logic applies to the type parameter `T`, which is
also a local definition.

Local symbols must be unique within the underlying document, but they don't have
to be unique across multiple documents. For example, at the time of writing
the Scalac-based SemanticDB producer generates local symbols named `local0`,
`local1`, etc, with the counter resetting to zero for every new document.

In the future, we may extend SemanticDB to make local symbols more unique,
but we haven't yet found a way to do that without sacrificing performance
and payload size. In the meanwhile, when global uniqueness is required,
tool authors are advised to accompany local symbols with `TextDocument.uri`.

### Scope

```protobuf
message Scope {
  repeated string symlinks = 1;
  repeated SymbolInformation hardlinks = 2;
}
```

`Scope` represents a container for definitions such as type parameters,
parameters or class declarations. Depending on the [Language](#language)
and the implementation, scopes specify their members as follows:
  * Via symbolic links to members, i.e. using [Symbol](#symbol).
  * Or via direct embedding of member metadata, i.e. using
    [SymbolInformation](#symbolinformation).

Hardlinking comes in handy in situations when an advanced type such
as `StructuralType`, `ExistentialType` or `UniversalType` ends up being
part of the type signature of a global symbol. For example, in the following
Scala program, method `m` has a structural type `AnyRef { def x: Int }`:

```scala
class C {
  def m = new { def x: Int = ??? }
}
```

At the time of writing, we haven't found a way to model the method `x`,
which constitutes a logical part of this structural type, as a global symbol.
Therefore, this method has to be modelled as a local symbol.

However, turning `x` into a local symbol that is part of the
["Symbols"](#symbolinformation) section presents certain difficulties.
In that case, in order to analyze public signatures of the containing
[TextDocument](#textdocument), SemanticDB consumers have to load the entire
"Symbols" section, which has adverse performance characteristics.

Hardlinking solves this conundrum by storing symbol metadata related to advanced
types directly inside the payloads representing these types. Thanks to
hardlinking, we don't need to invent global symbols to remain convenient to
SemanticDB consumers.

### Constant

```protobuf
message Constant {
  oneof sealed_value {
    UnitConstant unit_constant = 1;
    BooleanConstant boolean_constant = 2;
    ByteConstant byte_constant = 3;
    ShortConstant short_constant = 4;
    CharConstant char_constant = 5;
    IntConstant int_constant = 6;
    LongConstant long_constant = 7;
    FloatConstant float_constant = 8;
    DoubleConstant double_constant = 9;
    StringConstant string_constant = 10;
    NullConstant null_constant = 11;
  }
}

message UnitConstant {
}

message BooleanConstant {
  bool value = 1;
}

message ByteConstant {
  int32 value = 1;
}

message ShortConstant {
  int32 value = 1;
}

message CharConstant {
  int32 value = 1;
}

message IntConstant {
  int32 value = 1;
}

message LongConstant {
  int64 value = 1;
}

message FloatConstant {
  float value = 1;
}

message DoubleConstant {
  double value = 1;
}

message StringConstant {
  string value = 1;
}

message NullConstant {
}
```

`Constant` represents compile-time constants. Compile-time constants include
values of the nine primitive types on the JVM, as well as strings and `null`.

### Type

```protobuf
message Type {
  reserved 1, 3, 4, 5, 6, 11, 12, 15, 16;
  oneof sealed_value {
    TypeRef type_ref = 2;
    SingleType single_type = 20;
    ThisType this_type = 21;
    SuperType super_type = 22;
    ConstantType constant_type = 23;
    IntersectionType intersection_type = 17;
    UnionType union_type = 18;
    WithType with_type = 19;
    StructuralType structural_type = 7;
    AnnotatedType annotated_type = 8;
    ExistentialType existential_type = 9;
    UniversalType universal_type = 10;
    ByNameType by_name_type = 13;
    RepeatedType repeated_type = 14;
  }
}
```

`Type` represents expression types. Definition signatures are modelled
with [Signature](#signature).

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
message SingleType {
  Type prefix = 1;
  string symbol = 2;
}
```

`SingleType` is a singleton type that is inhabited only by the value of
the definition represented by `symbol` and the accompanying `prefix`.

```protobuf
message ThisType {
  string symbol = 1;
}
```

`ThisType` is a singleton type that is inhabited only by the value
of `this` reference to the definition represented by `symbol`,
only visible inside the lexical extent of the enclosing definition.

```protobuf
message SuperType {
  Type prefix = 1;
  string symbol = 2;
}
```

`SuperType` is a singleton type that is inhabited only by the value
of `super` reference to the definition represented by `symbol`,
prefixed by an optional `prefix`, only visible inside the lexical extent
of the enclosing definition.

```protobuf
message ConstantType {
  Constant constant = 1;
}
```

`ConstantType` is a singleton type that is inhabited only by the value
of a [Constant](#constant).

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
  reserved 1, 2, 3;
  Type tpe = 4;
  Scope declarations = 5;
}
```

`StructuralType` represents a structural type specified by its base type `tpe`
and `declarations`. Declarations are modelled by a [Scope](#scope).

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
  reserved 2;
  Type tpe = 1;
  Scope declarations = 3;
}
```

`ExistentialType` represents a type `tpe` existentially quantified
over `declarations`. Declarations are modelled by a [Scope](#scope).

```protobuf
message UniversalType {
  reserved 1;
  Scope type_parameters = 3;
  Type tpe = 2;
}
```

`UniversalType` represents a type `tpe` universally quantified
over `type_parameters`. Type parameters are modelled by a [Scope](#scope).

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

### Signature

```protobuf
message Signature {
  oneof sealed_value {
    ClassSignature class_signature = 1;
    MethodSignature method_signature = 2;
    TypeSignature type_signature = 3;
    ValueSignature value_signature = 4;
  }
}
```

`Signature` represents definition signatures. Expression types are modelled
with [Type](#type).

The SemanticDB type system is a superset of the type systems of supported
languages - currently modelled after the Scala type system [\[18\]][18].
This section describes the model, while [Languages](#languages) elaborates
on how language types map onto this model.

```protobuf
message ClassSignature {
  Scope type_parameters = 1;
  repeated Type parents = 2;
  Scope declarations = 3;
  Type self = 4;
}
```

`ClassSignature` represents signatures of objects, package objects, classes,
traits and interfaces. Both type parameters and declarations are modelled by
a [Scope](#scope). `self` represents an optional self-type [\[99\]][99].

```protobuf
message MethodSignature {
  Scope type_parameters = 1;
  repeated Scope parameter_lists = 2;
  Type return_type = 3;
}
```

`MethodSignature` represents signatures of methods (including getters and
setters), constructors and macros. It features `type_parameters`,
`parameter_lists` and a `return_type`. Both type parameters and parameters
are modelled by [Scopes](#scope). Moreover, in order to support multiple
parameter lists in Scala methods, `parameter_lists` is a list of lists.

```protobuf
message TypeSignature {
  Scope type_parameters = 1;
  Type lower_bound = 2;
  Type upper_bound = 3;
}
```

`TypeSignature` represents signatures of type parameters or type members.
It features `type_parameters` as well as `lower_bound` and `upper_bound`.
Type parameters are modelled by a [Scope](#scope).

```protobuf
message ValueSignature {
  Type tpe = 1;
}
```

`ValueSignature` represents signatures of locals, fields and self parameters.
It encapsulates an underlying type of the definition.

### SymbolInformation

```protobuf
message SymbolInformation {
  reserved 2, 6, 7, 8, 9, 10, 11, 12, 14, 15;
  string symbol = 1;
  Language language = 16;
  Kind kind = 3;
  int32 properties = 4;
  string display_name = 5;
  Signature signature = 17;
  repeated Annotation annotations = 13;
  Access access = 18;
}
```

"Symbols" is a section of a [TextDocument](#textdocument) that stores
information about [Symbols](#symbol) that are defined in the underlying
document. In a sense, this section is analogous to a symbol table
[\[19\]][19] in a compiler.

`language`. [Language](#language) that defines the corresponding definition.

`kind`. Enumeration that defines the kind of the corresponding definition.
See [Languages](#languages) for information on how definitions in supported
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
    <td>Field, e.g. <code>int x = 42</code>.</td>
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
See [Languages](#languages) for information on how definitions in supported
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
  <tr>
    <td><code>0x4000</code></td>
    <td><code>ENUM</code></td>
    <td>Is an <code>enum</code> field or class?</td>
  </tr>
  <tr>
    <td><code>0x8000</code></td>
    <td><code>DEFAULT</code></td>
    <td>Is a default parameter or a default method?</td>
  </tr>
</table>

`display_name`. Display name of the definition, i.e. how this definition
should be presented to humans, e.g. in code browsers or integrated development
environments.

This is not necessarily the same as the symbol name of the corresponding
[Symbol](#symbol). See [Languages](#languages) for more information on which
definitions have which display and symbol names in supported languages.

`signature`. [Signature](#signature) that represents the definition signature.
See [Languages](#languages) for more information on which definitions have
which signatures in supported languages.

`annotation`. [Annotations](#annotation) of the corresponding definition.

`access`. [Access](#access) modifier of the corresponding definition.

### Annotation

```protobuf
message Annotation {
  Type tpe = 1;
}
```

`Annotation` represents annotations. See [Languages](#languages) for
information on how annotations in supported languages map onto this
data structure.

### Access

```protobuf
message Access {
  oneof sealed_value {
    PrivateAccess private_access = 1;
    PrivateThisAccess private_this_access = 2;
    PrivateWithinAccess private_within_access = 3;
    ProtectedAccess protected_access = 4;
    ProtectedThisAccess protected_this_access = 5;
    ProtectedWithinAccess protected_within_access = 6;
    PublicAccess public_access = 7;
  }
}

message PrivateAccess {
}

message PrivateThisAccess {
}

message PrivateWithinAccess {
  string symbol = 1;
}

message ProtectedAccess {
}

message ProtectedThisAccess {
}

message ProtectedWithinAccess {
  string symbol = 1;
}

message PublicAccess {
}
```

`Access` represents access modifiers of definitions, including `private` and
`protected`, as well as variants: 1) limited to the current object instance,
and 2) limited to the given `symbol`. See [Languages](#languages) for
information on how access modifiers in supported languages map onto this
data structure.

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

See [Languages](#languages) for information on how language features in
supported languages map onto this data structure.

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
  Tree tree = 2;
}
```

"Synthetics" is a section of a [TextDocument](#textdocument) that stores
trees added by compilers that do not appear in the original source.
Examples include inferred type arguments, implicit parameters, or desugarings of
for loops.

`Synthetic` models one of these synthetics as a transformation of a piece of the
original source file to a synthetic AST that may still use quotes of the
original source.
The piece of the source file is given as a [Range](#range), and the new
synthetic AST is given as a [Tree](#tree).

### Tree

```protobuf
message Tree {
  oneof sealed_value {
    ApplyTree apply_tree = 1;
    FunctionTree function_tree = 2;
    IdTree id_tree = 3;
    LiteralTree literal_tree = 4;
    MacroExpansionTree macro_expansion_tree = 5;
    OriginalTree original_tree = 6;
    SelectTree select_tree = 7;
    TypeApplyTree type_apply_tree = 8;
  }
}
```

A `Tree` represents a typed abstract syntax tree.
The trees are similar to Scalameta and Rsc trees, except for
`OriginalTree`, which represents a quote of the original source file.
We only support a small subset of Scala syntax necessary to model
[Synthetics](#synthetic).
At the moment, we do not have plans to add more trees.

```protobuf
message ApplyTree {
  Tree function = 1;
  repeated Tree arguments = 2;
}
```

An `ApplyTree` represents a method application.

```protobuf
message FunctionTree {
  repeated IdTree parameters = 1;
  Tree body = 2;
}
```

A `FunctionTree` represents a function literal with parameter declarations and
a body.

```protobuf
message IdTree {
  string symbol = 1;
}
```

An `IdTree` represents a reference to a [Symbol](#symbol) in an identifier.

```protobuf
message LiteralTree {
  Constant constant = 1;
}
```

A `LiteralTree` represents a [Constant](#constant) literal.

```protobuf
message MacroExpansionTree {
  Tree before_expansion = 1;
  Type tpe = 2;
}
```

A `MacroExpansionTree` represents a macro expansion. The `before_expansion` can be
an `OriginalTree` (expansion of original code) or any other `Tree`
(expansion of synthetic code).

```protobuf
message OriginalTree {
  Range range = 1;
}
```

An `OriginalTree` represents a quote from the text of the enclosing
`TextDocument`, given as the range of that quote from the original text.
These represent trees that have direct correspondents with the original
source file.

```protobuf
message SelectTree {
  Tree qualifier = 1;
  IdTree id = 2;
}
```

A `SelectTree` represents a method or field selection on a qualifier.

```protobuf
message TypeApplyTree {
  Tree function = 1;
  repeated Type type_arguments = 2;
}
```

A `TypeApplyTree` represents the type application of a method, providing
that method with type arguments.

## Data Schemas

### Protobuf

[semanticdb.proto][semanticdb.proto]

## Languages

In this section, we describe language-dependent SemanticDB entities, i.e.
symbols, types, symbol informations, annotations, access modifiers and
symbol occurrences:

  * [Scala](#scala)
    * [Symbol](#scala-symbol)
    * [Type](#scala-type)
    * [Signature](#scala-signature)
    * [SymbolInformation](#scala-symbolinformation)
    * [Annotation](#scala-annotation)
    * [Access](#scala-access)
    * [SymbolOccurrence](#scala-symboloccurrence)
    * [Synthetic](#scala-synthetic)
  * [Java](#java)
    * [Symbol](#java-symbol)
    * [Type](#java-type)
    * [Signature](#java-signature)
    * [SymbolInformation](#java-symbolinformation)
    * [Annotation](#java-annotation)
    * [Access](#java-access)
    * [SymbolOccurrence](#java-symboloccurrence)

### Notation

We use a concise notation to describe SemanticDB entities.
In this notation, `M(v1, v2, ...)` corresponds a Protocol Buffers message
`M` with fields set to values `v1`, `v2`, etc. Literals correspond to
scalar values, `List(x1, x2, ...)` corresponds to repeated values, `None`
corresponds to missing optional values.
Moreover, `<X>` corresponds to an entity that represents `X`.

## Scala

In this section, we exhaustively map Scala language features onto SemanticDB.
As a reference, we use the Scala Language Specification [\[17\]][17]
(referred to as "SLS" in the text below), as well as additional resources
[[25][25], [40][40], [51][51], [56][56], [57][57]] in the areas where SLS
is incomplete or out of date.

<a name="scala-symbol"></a>
### Symbol

In this section, we describe the Scala symbol format, but don't cover the
details of how Scala definitions map onto symbols (e.g. which symbols are
created for which Scala definitions, what their metadata is, etc). See
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
      <ul>
        <li>
          For root package <a href="https://www.scala-lang.org/files/archive/spec/2.12/09-top-level-definitions.html#package-references">[20]</a>,
          its descriptor.
        </li>
        <li>
          For empty package <a href="https://www.scala-lang.org/files/archive/spec/2.12/09-top-level-definitions.html#packagings">[21]</a>,
          its descriptor.
        </li>
        <li>
          For top-level package, its descriptor.
        </li>
        <li>
          For other definition, concatenation of owner symbol and
          definition descriptor.
        </li>
      </ul>
    </td>
  </tr>
  <tr>
    <td>
      Local symbols
      <a href="#symbol">↑</a>
    </td>
    <td>
      Concatenation of <code>local</code> and an implementation-dependent
      suffix that doesn't contain slashes (`/`) and semicolons (`;`).
    </td>
  </tr>
</table>

**Owner** is:
  * For root package, `None`.
  * For empty package, root package.
  * For top-level package, root package.
  * For other package, parent package.
  * For package object, its associated package.
  * For other top-level definition, its package.
  * For other global definition, the innermost enclosing definition,
    i.e. the definition whose [Location](#location) in source code most
    tightly encloses the [Location](#location) of the original definition.
  * For other definition, `None`.

**Descriptor** is:
  * For `LOCAL`, unsupported.
  * For `PACKAGE`, concatenation of its symbol name and a forward slash (`/`).
  * For `OBJECT` or `PACKAGE_OBJECT`,
    concatenation of its symbol name and a dot (`.`).
  * Exceptionally, for `VAL` `METHOD`,
    concatenation of its symbol name and a dot (`.`).
  * For other `METHOD`, `CONSTRUCTOR`, or `MACRO`,
    concatenation of its symbol name, a disambiguator and a dot (`.`).
  * For `TYPE`, `CLASS` or `TRAIT`, concatenation of its
    symbol name and a pound sign (`#`).
  * For `PARAMETER`, concatenation of a left parenthesis (`(`), its
    symbol name and a right parenthesis (`)`).
  * For `SELF_PARAMETER`, unsupported.
  * For `TYPE_PARAMETER`, concatenation of a left bracket (`[`), its
    symbol name and a right bracket (`]`).
  * See [SymbolInformation](#scala-symbolinformation) for details on
    which Scala definitions are modelled by which symbols.

**Disambiguator** is:
  * Concatenation of a left parenthesis (`(`), a tag
    and a right parenthesis (`)`).
    If the definition is not overloaded, the tag is empty.
    Two definitions are overloaded if they have the same name and both require
    a disambiguator. If the definition is overloaded, the tag is computed from
    the order of appearance of overloads in the source code (see
    "Function declarations and definitions" below for an example):
      * Empty string for the definition that appears first.
      * `+1` for the definition that appears second.
      * `+2` for the definition that appears third.
      * ...

**Symbol name** is:
  * For root package, `_root_`.
  * For empty package, `_empty_`.
  * For package object, `package`.
  * For constructor, `<init>`.
  * For anonymous definition, implementation-dependent name.
  * For other definition, the name of the binding introduced by the definition
    [\[70\]][70]. If the name is not a Java identifier [\[22\]][22], it is
    wrapped in backticks.

For example, this is how some of the definitions from the Scala standard library
must be modelled:

* The `scala` package: `scala/`
* The `Int` class: `scala/Int#`
* The `def implicitly[T](implicit e: T)` method: `scala/Predef.implicitly().`
* The `e` parameter of that method: `scala/Predef.implicitly().(e)`
* The `T` type parameter of that method: `scala/Predef.implicitly().[T]`
* The `def contains[A: Ordering](tree: Tree[A, _], x: A): Boolean` method:
  `scala/collection/immutable/RedBlackTree#contains().`

<a name="scala-type"></a>
### Type

```protobuf
  reserved 1, 3, 4, 5, 6, 11, 12, 15, 16;
  oneof sealed_value {
    TypeRef typeRef = 2;
    SingleType singleType = 20;
    ThisType thisType = 21;
    SuperType superType = 22;
    ConstantType constantType = 23;
    IntersectionType intersectionType = 17;
    UnionType unionType = 18;
    WithType withType = 19;
    StructuralType structuralType = 7;
    AnnotatedType annotatedType = 8;
    ExistentialType existentialType = 9;
    UniversalType universalType = 10;
    ByNameType byNameType = 13;
    RepeatedType repeatedType = 14;
  }
```

In Scala, [Type](#type) represents value types [\[18\]][18].
Non-value types [\[18\]][18] are modelled with a separate data structure
called [Signature](#signature) (see [below](#scala-signature) for examples).

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

<table>
  <tr>
    <td width="220px"><b>Category</b></td>
    <td><b>Examples</b></td>
  </tr>
  <tr>
    <td valign="top">Singleton types [<a href="https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#singleton-types">24</a>, <a href="https://github.com/scala/scala/pull/5310">25</a>]</td>
    <td>
      <ul>
        <li><code>x.type</code> ~ <code>SingleType(None, &lt;x&gt;)</code>.</li>
        <li><code>p.x.type</code> ~ <code>SingleType(&lt;p.type&gt;, &lt;x&gt;)</code>.</li>
        <li><code>this.type</code> ~ <code>ThisType(&lt;E&gt;)</code>.</li>
        <li><code>C.this.type</code> ~ <code>ThisType(THIS, None, &lt;C&gt;, None, None)</code>.</li>
        <li>Type of <code>super</code> ~ <code>SuperType(&lt;E&gt;, None)</code>.</li>
        <li>Type of <code>super[M]</code> ~ <code>SuperType(&lt;E&gt;, &lt;M&gt;)</code>.</li>
        <li>Type of <code>C.super[M]</code> ~ <code>SuperType(&lt;C&gt;, &lt;M&gt;)</code>.</li>
        <li>Literal type ~ <code>ConstantType(&lt;value&gt;)</code>.</li>
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
</table>

Notes:
* We diverge from SLS by having different data structures to model value types
  ([Type](#type)) and non-value types ([Signature](#signature)).
* We diverge from SLS on the matter of handling prefixes (see definitions of
  `TypeRef` and `SingleType` for more information).
  * In SLS, all types that can have a prefix must have it specified
    explicitly, even if the prefix is trivial. For example in Scalac, `Int` must
    be represented as `TypeRef(<scala.this.type>, <Int>, List())` [\[27\]][27].
  * In SemanticDB, all types that have a trivial prefix must not have it
    specified explicitly. For example in SemanticDB, `Int` must be represented
    as `TypeRef(None, <Int>, List())`. Moreover, even `scala.Int` must be
    represented as `TypeRef(None, <Int>, List())`.
  * By a trivial prefix, we mean either empty prefix (for definitions that
    aren't members of any other definition, e.g. parameters or type parameters)
    or `ThisType` of the enclosing class, trait, interface, object, package object
    or package (for all other definitions), as well as types equivalent to them.
* We leave the mapping between type syntax written in source code and
  `Type` entities deliberately unspecified. Some producers may transform
  types in unspecified ways (e.g. Scalac transforms all `this.type` types
  into qualified `X.this.type` types), and our experience [\[38\]][38] shows
  that reverse engineering these transformations is very hard. We may improve
  on this in the future, but this is highly unlikely. In the meanwhile,
  use [Occurrences](#symboloccurrence) for figuring out semantics of syntax
  written in source code.

<a name="scala-signature"></a>
### Signature

In Scala, [Signature](#signature) represents definition signatures,
which also includes non-value types [\[18\]][18].
See [below](#scala-symbolinformation) to learn which Scala definitions
have which signatures.

<a name="scala-symbolinformation"></a>
### SymbolInformation

```protobuf
message SymbolInformation {
  reserved 2, 6, 7, 8, 9, 10, 11, 12, 14, 15;
  string symbol = 1;
  Language language = 16;
  Kind kind = 3;
  int32 properties = 4;
  string display_name = 5;
  Signature signature = 17;
  repeated Annotation annotations = 13;
  Access access = 18;
}
```

<table>
  <tr>
    <td><b>Field</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>symbol</code></td>
    <td>See <a href="#scala-symbol">Symbol</a>.</td>
  </tr>
  <tr>
    <td><code>language</code></td>
    <td><code>SCALA</code>.</td>
  </tr>
  <tr>
    <td><code>kind</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>properties</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>display_name</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>signature</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>annotations</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>access</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
</table>

**Value declarations and definitions** [\[39\]][39] are represented by multiple
symbols, with the exact number of symbols, their kinds, properties, signatures
and access modifiers dependent on the corresponding value:
* Local symbol of kind `LOCAL` is created for all local values.
* Getter symbol of kind `METHOD` is created for all member values to model
  the getter method associated with the corresponding member value.
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
    <td><code>_empty_/C#xp().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xp</code></td>
    <td><code>_empty_/C#`&lt;init&gt;`().(xp)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xm</code></td>
    <td><code>_empty_/C#xm().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xam</code></td>
    <td><code>_empty_/C#xam().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xlm</code></td>
    <td><code>_empty_/C#xlm().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xl</code></td>
    <td><code>local0</code></td>
    <td><code>LOCAL</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xs</code></td>
    <td><code>local1</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xe</code></td>
    <td><code>local2</code></td>
    <td><code>METHOD</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Int&gt;, List()))</code></td>
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
* Display name for value symbols is equal to the name of the binding
  introduced by the definition [\[70\]][70].
* If the type of the value is not provided in source code, it is inferred
  from the right-hand side of the value according to the rules described
  in SLS [\[39\]][39]. Corresponding signature is computed from the inferred
  type as explained in [Type](#scala-type).
* Depending on their meta annotations, value annotations may end up as
  `Annotation` entities associated with multiple corresponding symbols.
  See [\[40\]][40] for more information.
* Supported access modifiers for value symbols are:
  * `PrivateAccess`: set for getters of `private` values.
  * `PrivateThisAccess`: set for vals of value members.
  * `PrivateWithinAccess`: set for getters of `private[...]` values.
  * `ProtectedAccess`: set for getters of `protected` values.
  * `ProtectedThisAccess`: set for getters of `protected[this]` values.
  * `ProtectedWithinAccess`: set for getters of `protected[...]` values.

**Variable declarations and definitions** [\[41\]][41] are represented by
multiple symbols, with the exact number of symbols, their kinds, properties,
signatures and access modifiers dependent on the corresponding value:
* Local symbol of kind `LOCAL` is created for all local variables.
* Getter and setter symbols of kind `METHOD` are created for all member
  variables to model the getter and setter methods associated with the
  corresponding member variable.
* Parameter symbol of kind `PARAMETER` is created for `var` parameters
  of primary constructors to model the corresponding constructor parameter.

Notes:
* Variable symbols are modelled exactly the same as value symbols
  (see "Value symbols and declarations"), with the exceptions described below.
* Setter symbols have the following metadata:
  * `kind`: `METHOD`.
  * `properties`: see below.
  * `display_name`: concatenation of the display name of the variable and `_=`.
  * `signature`: `MethodSignature(List(), List(List(<x$1>)), <Unit>)`, where
    `x$1` is a `PARAMETER` symbol having `signature` equal to the type of the
     variable.
  * `annotations` and `access`: same as value symbols.
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

**Pattern variables** [\[65\]][65] are represented differently depending
on where they are defined:
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
    <td><code>LOCAL</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Nothing&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xval</code></td>
    <td><code>_empty_/C#xval().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Nothing&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xvar</code></td>
    <td><code>_empty_/C#xvar().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Nothing&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>xvar</code></td>
    <td><code>_empty_/C#xvar_=().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(&lt;x$1&gt;), TypeRef(None, &lt;Unit&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* In the future, we may decide to introduce a dedicated symbol kind
  for regular pattern variables, so that they can be distinguished from
  local value definitions.
* Pattern variable symbols don't support any properties.
* Display name for pattern variable symbols is equal to the name of the binding
  introduced by the definition [\[70\]][70].
* Pattern definitions [\[39\]][39] do not exist as a first-class language
  feature. Instead, they are desugared into zero or more synthetic value
  definitions and only then modelled as symbols as described in
  "Value declarations and definitions" and "Variable declarations and
  definitions".
* Pattern variable symbols don't support any access modifiers.

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
    <td><code>_empty_/C#T1#</code></td>
    <td><code>TYPE</code></td>
    <td><code>TypeSignature(List(), None, &lt;Hi&gt;)</code></td>
  </tr>
  <tr>
    <td><code>T2</code></td>
    <td><code>_empty_/C#T2#</code></td>
    <td><code>TYPE</code></td>
    <td><code>TypeSignature(List(), &lt;Lo&gt;, None)</code></td>
  </tr>
  <tr>
    <td><code>T</code></td>
    <td><code>_empty_/C#T#</code></td>
    <td><code>TYPE</code></td>
    <td><code>TypeSignature(List(), TypeRef(None, &lt;Int&gt;, List()), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Supported properties for type symbols are:
  * `ABSTRACT`: set for type declarations.
  * `FINAL`: set for `final` type aliases.
* Display name for type symbols is equal to the name of the binding
  introduced by the definition [\[70\]][70].
* We leave the mapping between type syntax written in source code and
  `Type` entities deliberately unspecified. For example, a producer may
  represent the signature of `T1` as `TypeSignature(List(), <Nothing>, <Hi>)`.
  See [Types](#scala-type) for more information.
* If present, type parameters of type declarations and type aliases are
  represented as described below in order of their appearance in source code.
* Type symbols support [all Scala access modifiers](#scala-access).

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
    <td><code>TypeSignature(List(), None, None)</code></td>
  </tr>
</table>

Notes:
* In the future, we may decide to introduce a dedicated symbol kind
  for type variables, so that they can be distinguished from
  local type definitions.
* Type variable symbols are always `ABSTRACT`.a
* Display name for type variable symbols is equal to:
  * The name of the binding introduced by the definition [\[70\]][70].
  * Except, in case of anonymous type variables via the `_` syntax, `_`.
* We leave the mapping between type syntax written in source code and
  `Type` entities deliberately unspecified. For example, a producer may
  represent the signature of `t` as `TypeSignature(List(), <Nothing>, <Any>)`.
  See [Types](#scala-type) for more information.
* Type variable symbols don't support any access modifiers.

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
    <td><code>ValueSignature(TypeRef(None, &lt;C1&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>self2</code></td>
    <td><code>local0</code></td>
    <td><code>SELF_PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Self parameters cannot be referenced outside the document where they are
  located, which means that they are represented by local symbols.
* Self parameter symbols don't support any properties.
* Display name for self parameter symbols is equal to:
  * The name of the binding introduced by the definition [\[70\]][70].
  * Except, in case of anonymous self parameters via `_: T =>`, `this: T =>`
    or corresponding typeless syntaxes, `_`.
* Self parameter symbols don't support any access modifiers.

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
    <td><code>_empty_/C#[T1]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeSignature(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>T2</code></td>
    <td><code>_empty_/C#m()[T2]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeSignature(List(), None, &lt;Hi&gt;)</code></td>
  </tr>
  <tr>
    <td><code>T3</code></td>
    <td><code>_empty_/C#m()[T2][T3]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeSignature(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>T4</code></td>
    <td><code>_empty_/C#T#[T4]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeSignature(List(), &lt;Lo&gt;, None)</code></td>
  </tr>
</table>

Notes:
* Supported properties for type parameter symbols are:
  * `COVARIANT`: set for covariant type parameters.
  * `CONTRAVARIANT`: set for contravariant type parameters.
* If present, (higher-order) type parameters of type parameters are
  represented as described here in order of their appearance in source code.
* Display name for type parameter symbols is equal to:
  * The name of the binding introduced by the definition [\[70\]][70].
  * Except, in case of anonymous type variables via the `_` syntax, `_`.
* We leave the mapping between type syntax written in source code and
  `Type` entities deliberately unspecified. For example, a producer may
  represent the signature of `T1` as `TypeSignature(List(), <Nothing>, <Any>)`.
  See [Types](#scala-type) for more information.
* If present, context bounds and value bounds of type parameters are desugared
  into parameters of the enclosing definition as described in [\[44\]][44] and
  are represented with corresponding `PARAMETER` symbols.

**Parameters** are represented with `PARAMETER` symbols.
(There is no section in SLS dedicated to parameters, so we aggregate information
about parameters from multiple sections).

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
    <td><code>_empty_/C#`&lt;init&gt;`().(p1)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>p2</code></td>
    <td><code>_empty_/C#m2().(p2)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>p3</code></td>
    <td><code>_empty_/C#m3().(p3)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3$default$1</code></td>
    <td><code>_empty_/C#m3$default$1().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>p4</code></td>
    <td><code>_empty_/C#m4().(p4)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(ByNameType(TypeRef(None, &lt;Int&gt;, List())))</code></td>
  </tr>
  <tr>
    <td><code>p5</code></td>
    <td><code>_empty_/C#m5().(p5)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(RepeatedType(TypeRef(None, &lt;Int&gt;, List())))</code></td>
  </tr>
  <tr>
    <td>Context bound</td>
    <td><code>_empty_/C#m6().(x$1)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;C&gt;, List(&lt;T&gt;)))</code></td>
  </tr>
  <tr>
    <td>View bound</td>
    <td><code>_empty_/C#m7().(x$2)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Function1&gt;, List(&lt;T&gt;, &lt;V&gt;)))</code></td>
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
  * `DEFAULT`: set for parameters with default values.
* Scalac semantic model does not distinguish parameters in `class C(x: Int)`
  and `class C(private[this] val x: Int)`. As a result, due to implementation
  restrictions `private[this] val` parameters currently don't have the `VAL`
  property.
* Display name for parameter symbols is equal to:
  * The name of the binding introduced by the definition [\[70\]][70].
  * Except, in case of anonymous type variables via the `_` syntax, `_`.
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
  object m3
  def m3(x: Int): Int = ???
  def m3(x: org.Int): Int = ???
  val m4: Int
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
    <td><code>_empty_/C#m1().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m2</code></td>
    <td><code>_empty_/C#m2().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(List()), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>_empty_/C#m3.</code></td>
    <td><code>OBJECT</code></td>
    <td><code>ClassSignature(List(), List(), None, List())</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>_empty_/C#m3().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(List(&lt;x&gt;)), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>_empty_/C#m3(+1).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(List(&lt;x&gt;)), TypeRef(None, &lt;org.Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m4</code></td>
    <td><code>_empty_/C#m4.</code></td>
    <td><code>METHOD</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m4</code></td>
    <td><code>_empty_/C#m4().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(List(&lt;x&gt;), List(&lt;y&gt;)), TypeRef(None, &lt;Int&gt;, List()))</code></td>
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
* Display name for method symbols is equal to the name of the binding
  introduced by the definition [\[70\]][70].
* If present, type parameters of methods are
  represented as described above in order of their appearance in source code.
* If present, parameters of methods are
  represented as described above in order of their appearance in source code.
* For procedures [\[49\]][49], the return type is assumed to be `Unit`.
  Corresponding signature is computed using the assumed return
  type as explained in [Type](#scala-type).
* If the return type is not provided in source code, it is inferred from the
  right-hand side of the method according to the rules described
  in SLS [\[50\]][50]. Corresponding signature is computed using the inferred
  retyrb type as explained in [Type](#scala-type).
* Method symbols support [all Scala access modifiers](#scala-access).
* The `OBJECT` symbol `m3.` and `VAL METHOD` symbol `m4.` do not contribute
  to the disambiguator tag for the method symbols `m3().`, `m3(+1).` and `m4().`
  because `OBJECT` and (exceptionally) `VAL METHOD` symbols do not require a
  disambiguator.

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
    <td><code>_empty_/M.m().</code></td>
    <td><code>MACRO</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Supported properties for macro symbols are the same as for method symbols,
  except for `ABSTRACT` because macros cannot be `abstract`.
* Display name for macro symbols is equal to the name of the binding
  introduced by the definition [\[70\]][70].
* Return type inference for macros is not supported.
* At the moment, `SymbolInformation` for macros does not contain information
  about corresponding macro implementations. We may improve this in the future.
* Macro symbols support [all Scala access modifiers](#scala-access).

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
    <td><code>_empty_/C#`&lt;init&gt;`().</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td><code>MethodSignature(List(), List(List(&lt;x&gt;)), None)</code></td>
  </tr>
  <tr>
    <td>Secondary constructor</td>
    <td><code>_empty_/C#`&lt;init&gt;`(+1).</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td><code>MethodSignature(List(), List(), None)</code></td>
  </tr>
</table>

Notes:
* Unlike some other metaprogramming systems for Scala, we do not create
  synthetic constructor symbols for traits and objects.
* Supported properties for constructor symbols are:
  * `PRIMARY`: set for primary constructors.
* Display name for constructor symbols is equal to `<init>`.
* Constructors don't have type parameters and return types, but we still
  represent their signatures with `MethodSignature`. In these signatures,
  type parameters are equal to `List()` and the return type is `None`.
* Primary constructor parameters with `val` and `var` modifiers give rise
  to multiple different symbols as described above.
* Constructor symbols support [all Scala access modifiers](#scala-access).

**Class definitions** [\[54\]][54] are represented with `CLASS` symbols.

```scala
class C[T](x: T, val y: T, var z: T) extends B with X { self: Y =>
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
    <td><code>_empty_/C#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassSignature(List(&lt;T&gt;), List(&lt;B&gt;, &lt;X&gt;), &lt;Y&gt;, List(&lt;x&gt;, &lt;y&gt;, &lt;y&gt;, &lt;z&gt;, &lt;z&gt;, &lt;z_=&gt;, &lt;&lt;init&gt;&gt;, &lt;m&gt;))</code></td>
  </tr>
  <tr>
    <td><code>T</code></td>
    <td><code>_empty_/C#[T]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeSignature(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>x</code></td>
    <td><code>_empty_/C#x().</code></td>
    <td><code>METHOD</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>y</code></td>
    <td><code>_empty_/C#x().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>z</code></td>
    <td><code>_empty_/C#z().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>z</code></td>
    <td><code>_empty_/C#z_=().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(List(&lt;x$1&gt;)), TypeRef(None, &lt;Unit&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>z</code></td>
    <td><code>_empty_/C#z_=().(x$1)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
  <tr>
    <td>Primary constructor</td>
    <td><code>_empty_/C#`&lt;init&gt;`().</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td><code>MethodSignature(List(), List(), None)</code></td>
  </tr>
  <tr>
    <td><code>x</code></td>
    <td><code>_empty_/C#`&lt;init&gt;`().(x)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>y</code></td>
    <td><code>_empty_/C#`&lt;init&gt;`().(y)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>z</code></td>
    <td><code>_empty_/C#`&lt;init&gt;`().(z)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m</code></td>
    <td><code>_empty_/C#m().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Int&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Supported properties for class symbols are:
  * `ABSTRACT`: set for `abstract` classes.
  * `FINAL`: set for `final` classes.
  * `SEALED`: set for `sealed` classes.
  * `IMPLICIT`: set for `implicit` classes.
  * `CASE`: set for `case` classes.
* Display name for class symbols is equal to the name of the binding
  introduced by the definition [\[70\]][70].
* We leave the mapping between parent syntax written in source code and
  `ClassSignature.parents` deliberately unspecified. Some producers are known
  to insert `<AnyRef>` into `parents` under certain circumstances, so we can't
  guarantee a one-to-one mapping of parent clauses in source code and
  entities in `parents`. We may improve on this in the future.
* `ClassSignature.declarations` must be ordered as follows:
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
* Class symbols support [all Scala access modifiers](#scala-access).

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
* Display name for object symbols is equal to the name of the binding
  introduced by the definition [\[70\]][70].
* Objects don't have type parameters, but we still represent their signatures
  with `ClassSignature`. In these signatures, type parameters are equal
  to `List()`.
* Objects don't have constructors.

**Package objects** [\[60\]][60] are represented by `PACKAGE_OBJECT` symbols
similarly to object definitions (see above). Concretely, the differences
between package object symbols and object symbols are:
* Package object symbols are always `FINAL`.
* Apart from `FINAL`, package object symbols don't support any properties.
* Display name for package object symbols is equal to the name of the binding
  introduced by the definition [\[70\]][70].
* Package objects don't have annotations.
* Package objects don't support any access modifiers.

**Packages** [\[61\]][61] are not included in the
["Symbols"](#symbolinformation) section.

<a name="scala-annotation"></a>
### Annotation

```protobuf
message Annotation {
  Type tpe = 1;
}
```

In Scala, [Annotation](#annotation) represents annotations [\[23\]][23].

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
  represented as `Annotation(<ann>)`. We may improve on this in the future.
* At the moment, SemanticDB cannot represent expressions, which means
  that it cannot represent expression annotations as well. We do not
  plan to add support for expressions in SemanticDB, so it is highly
  unlikely that expression annotations will be supported in the future.

<a name="scala-access"></a>
### Access

```protobuf
message Access {
  oneof sealed_value {
    PrivateAccess privateAccess = 1;
    PrivateThisAccess privateThisAccess = 2;
    PrivateWithinAccess privateWithinAccess = 3;
    ProtectedAccess protectedAccess = 4;
    ProtectedThisAccess protectedThisAccess = 5;
    ProtectedWithinAccess protectedWithinAccess = 6;
    PublicAccess publicAccess = 7;
  }
}

message PrivateAccess {
}

message PrivateThisAccess {
}

message PrivateWithinAccess {
  string symbol = 1;
}

message ProtectedAccess {
}

message ProtectedThisAccess {
}

message ProtectedWithinAccess {
  string symbol = 1;
}

message PublicAccess {
}
```

In Scala, [Access](#access) represents access modifiers of definitions.

<table>
  <tr>
    <td><b>Access</b></td>
    <td><b>Code</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>None</code></td>
    <td><code></code></td>
    <td>
      Definitions that can't have access modifiers, i.e. `LOCAL`, `PARAMETER`,
      `SELF_PARAMETER`, `TYPE_PARAMETER`, `PACKAGE` and `PACKAGE_OBJECT`.
      Definitions that can have access modifiers, but don't have them will
      have `PublicAccess` as described below.
    </td>
  </tr>
  <tr>
    <td><code>PrivateAccess()</code></td>
    <td><code>private def x = ???</code></td>
    <td>
      Can be accessed only from within the directly enclosing template
      and its companion object or companion class
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#private">[62]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PrivateThisAccess()</code></td>
    <td><code>private[this] def x = ???</code></td>
    <td>
      Can be accessed only from within the object in which the definition
      is defined.
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#private">[62]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PrivateWithinAccess(<X>)</code></td>
    <td><code>private[X] def x = ???</code></td>
    <td>
      Can be accessed respectively only from code inside the package
      <code>X</code> or only from code inside the class <code>X</code>
      and its companion object.
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#private">[62]</a>.
    </td>
  </tr>
  <tr>
    <td><code>ProtectedAccess()</code></td>
    <td><code>protected def x = ???</code></td>
    <td>
      Can be accessed from within: 1) the template of the defining class,
      2) all templates that have the defining class as a base class,
      3) the companion object of any of those classes.
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#protected">[63]</a>.
    </td>
  </tr>
  <tr>
    <td><code>ProtectedThisAccess()</code></td>
    <td><code>protected[this] def x = ???</code></td>
    <td>
      Can be accessed as <code>protected</code> AND
      only from within the object in which the definition is defined.
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#protected">[63]</a>.
    </td>
  </tr>
  <tr>
    <td><code>ProtectedWithinAccess(<X>)</code></td>
    <td><code>protected[X] def x = ???</code></td>
    <td>
      Can be accessed as <code>protected</code> OR
      from code inside the package <code>X</code> or from code inside
      the class <code>X</code> and its companion object.
      <a href="https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#protected">[63]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PublicAccess()</code></td>
    <td><code>def x = ???</code></td>
    <td>None of the above.</td>
  </tr>
</table>

Notes:
* Not all kinds of symbols support all access modifiers. See
  [SymbolInformation](#scala-symbolinformation) for more information.

<a name="scala-symboloccurrence"></a>
### SymbolOccurrence

```protobuf
message SymbolOccurrence {
  Range range = 1;
  string symbol = 2;
  Role role = 3;
}
```

There is a Scala compiler plugin
that generates [SymbolOccurrences](#symboloccurrence) for Scala code.
The implementation is used at Twitter scale, and it works well -
both in terms of handling sizeable codebases and understanding esoteric
language constructs and idioms. However, but we do not yet have a specification
that comprehensively describes how Scala language features map onto symbol
occurrences. We intend to improve on this in the future.

<a name="scala-synthetic"></a>

### Synthetic

The following is an exhaustive list of the kinds of synthetics that are
generated by the Scala compiler plugin, along with examples.

Synthetics generated from represent for loops are quite massive,
so we use a shorthand notation for them.
In that notation, OriginalTree trees are represented by `orig(<code>)`
and other trees are represented by their syntax.

<table>
<tr>
  <td width="220px"><b>Category</b></td>
  <td><b>Source</b></td>
  <td><b>Synthetic</b></td>
</tr>
<tr>
  <td valign="top" rowspan="2">

Inferred method calls

  </td>
  <td>

```scala
List(1)
```

  </td>
  <td>

```scala
Synthetic(
  <List>,
  TypeApplyTree(
    SelectTree(
      OriginalTree(<List>),
      Some(IdTree(<List.apply>))),
    List(TypeRef(None, <Int>, List()))))
```

  </td>
</tr>
<tr>
  <td>

```scala
val List(a, b) = ...
```
  </td>
  <td>

```scala
Synthetic(
  <List>,
  TypeApplyTree(
    SelectTree(
      OriginalTree(<List>),
      Some(IdTree(<SeqFactory.unapplySeq>))),
    List(TypeRef(None, <Nothing>, List()))))
```

  </td>
</tr>
<tr>
<td valign="top" rowspan="2">

Inferred type applications

  </td>
  <td>

```scala
List(1).map(_ + 2)
```

  </td>
  <td>

```scala
Synthetic(
  <List(1).map>,
  TypeApplyTree(
    OriginalTree(<List(1).map>),
    List(
      TypeRef(None, <Int>, List()),
      TypeRef(None, <List>,
        List(TypeRef(None, <Int>, List()))))))
```

  </td>
</tr>
<tr>
<td>

```scala
1 #:: 2 #:: Stream.empty
```

</td>
<td>

```scala
Synthetic(
  <#:: 2 #:: Stream.empty>,
  TypeApplyTree(
    OriginalTree(<#:: 2 #:: Stream.empty>),
    List(
      TypeRef(None, <Int>, List()))))
```

</td>
</tr>
<tr>
<td valign="top" rowspan="1">

Implicit parameters

  </td>
  <td>

```scala
List(1).map(_ + 2)
```

  </td>
  <td>

```scala
Synthetic(
  <List(1).map(_ + 2)>,
  ApplyTree(
    OriginalTree(<List(1).map(_ + 2)>),
    List(
      TypeApplyTree(
        IdTree(<List.canBuildFrom>),
        List(TypeRef(None, <Int>, List()))))))
```

  </td>
</tr>
<tr>
<td valign="top" rowspan="1">

Implicit views/conversions

  </td>
  <td>

```scala
"fooo".stripPrefix("o")
```

  </td>
  <td>

```scala
Synthetic(
  <"fooo">,
  ApplyTree(
    IdTree(<Predef.augmentString>),
    List(OriginalTree(<"fooo">))))
```

  </td>
</tr>
<tr>
<td valign="top" rowspan="1">

Macro expansions

  </td>
  <td>

```scala
Array.empty[Int]
```

  </td>
  <td>

```scala
Synthetic(
  <Array.empty[Int]>,
  ApplyTree(
    OriginalTree(<Array.empty[Int]>),
    List(
      MacroExpansionTree(
        IdTree(<ClassTag.Int>),
        TypeRef(None, <ClassTag>,
          List(TypeRef(None, <Int>, List())))))))
```

  </td>
</tr>
<tr>
<td valign="top" rowspan="2">

For loop desugarings

  </td>
  <td>

```scala
for {
  i <- 1 to 10
  j <- 0 until 10
  if i % 2 == 0
} yield (i, j)
```

  </td>
  <td>

```scala
orig(1 to 10).flatMap[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]]({
  (i) =>
    orig(0 until 10)
      .withFilter({ (j) =>
        orig(i % 2 == 0)
      })
      .map[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]]({ (j) =>
        orig((i, j))
      })(canBuildFrom[Tuple2[Int, Int]])
})(canBuildFrom[Tuple2[Int, Int]])
```

  </td>
</tr>
<tr>
<td>

```scala
for {
  a <- Future.successful(1)
  b <- Future.successful(2)
} println(a)
```

</td>
<td>

```scala
orig(scala.concurrent.Future.successful(1)).flatMap[Int]({ (a) =>
  orig(scala.concurrent.Future.successful(2))
    .withFilter({ (b) =>
      orig(a < b)
    })(global)
    .map[Int]({ (b) =>
      orig(a)
    })(global)
})(global)
```

</td>
<tr>

</table>

## Java

In this section, we exhaustively map Java language features onto SemanticDB.
As a reference, we use the Java Language Specification [\[85\]][85] (referred
to as "JLS" in the text below) and Java Virtual Machine Specification
[\[91\]][91] (referred to as "JVMS" in the text below).

<a name="java-symbol"></a>
### Symbol

In this section, we describe the Java symbol format.

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
      <ul>
        <li>
          For <a href="#java-root-package">root package</a>, its descriptor.
        </li>
        <li>
          For unnamed package, <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-7.html#jls-7.4.2">[97]</a>,
          its descriptor.
        </li>
        <li>
          For top-level package, its descriptor.
        </li>
        <li>
          For other definition, concatenation of owner symbol and
          definition descriptor.
        </li>
      </ul>
    </td>
  </tr>
  <tr>
    <td>
      Local symbols
      <a href="#symbol">↑</a>
    </td>
    <td>
      Concatenation of <code>local</code> and an implementation-dependent
      suffix that doesn't contain slashes (`/`) and semicolons (`;`).
    </td>
  </tr>
</table>

**Owner** is:
  * For root package, `None`.
  * For unnamed package, root package.
  * For top-level named package, root package.
  * For other named package, parent package.
  * For other top-level definition, its package.
  * For other global definition, the innermost enclosing definition,
    i.e. the definition whose [Location](#location) in source code most
    tightly encloses the [Location](#location) of the original definition.
  * For other declarations, `None`.

**Descriptor** is:
  * For `LOCAL`, unsupported.
  * For `PACKAGE`, concatenation of its symbol name and a forward slash (`/`).
  * For `FIELD`, concatenation of its symbol name and a dot (`.`).
  * For `METHOD` or `CONSTRUCTOR`, concatenation of its symbol name,
    a disambiguator and a dot (`.`).
  * For `CLASS` or `INTERFACE`, concatenation of its symbol name and
    a pound sign (`#`).
  * For `PARAMETER`, concatenation of a left parenthesis (`(`), its symbol
    name and a right parenthesis (`)`).
  * For `TYPE_PARAMETER`, concatenation of a left bracket (`[`), its symbol
    name and a right bracket (`]`).
  * See [SymbolInformation](#java-symbolinformation) for details on
    which Java definitions are modelled by which symbols.

**Disambiguator** is:
  * Concatenation of a left parenthesis (`(`), a tag
    and a right parenthesis (`)`).
    If the definition is not overloaded, the tag is empty.
    If the definition is overloaded, the tag is computed depending on where
    the definition appears in the following order:

      * non-static overloads first, following the same order as they
        appear in the original source,
      * static overloads secondly, following the same order as they
        appear in the original source

    `FIELD` definitions are not included in the list of overloads.
    Given this order, the tag becomes

      * Empty string for the definition that appears first.
      * `+1` for the definition that appears second.
      * `+2` for the definition that appears third.
      * ...

    See "Class declarations" below for an example.

**Symbol name** is:
  * For root package, `_root_`.
  * For unnamed package, `_empty_`.
  * For constructor, `<init>`.
  * For anonymous definition, implementation-dependent name.
  * For other definition, the name of the binding introduced by the definition.
    If the name is not a Java identifier [\[22\]][22], it is wrapped
    in backticks.

For example, this is how some of the definitions from the Java standard library
must be modelled:

  * The `java` package: `java.`
  * The `Integer` class: `java.lang.Integer#`
  * The `int` primitive: `scala.Int#`
  * The `Arrays.asList` method: `java.util.Arrays#asList().`
  * The `a` parameter of that method: `java.util.Arrays#asList().(a)`
  * The `T` type parameter of that method: `java.util.Arrays#asList().[T]`

<a name="java-type"></a>
### Type

```protobuf
  reserved 1, 3, 4, 5, 6, 11, 12, 15, 16;
  oneof sealed_value {
    TypeRef typeRef = 2;
    SingleType singleType = 20;
    ThisType thisType = 21;
    SuperType superType = 22;
    ConstantType constantType = 23;
    IntersectionType intersectionType = 17;
    UnionType unionType = 18;
    WithType withType = 19;
    StructuralType structuralType = 7;
    AnnotatedType annotatedType = 8;
    ExistentialType existentialType = 9;
    UniversalType universalType = 10;
    ByNameType byNameType = 13;
    RepeatedType repeatedType = 14;
  }
```

In Java, [Type](#type) represents types [\[74\]][74].

In the examples below:
  * `byte`, `short` and friends are standard primitive types [\[75\]][75].
  * `A` is a top-level class.
  * `B` is a inner class defined in `A`.
  * `C` is a top-level class that has multiple type parameters.
  * `T`, `T1`, ... `Tn` are type variables.

<table>
  <tr>
    <td width="220px"><b>Category</b></td>
    <td><b>Examples</b></td>
  </tr>
  <tr>
    <td valign="top">Primitive types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.2">75</a>]</td>
    <td>
      <ul>
        <li><code>byte</code> ~ <code>TypeRef(None, &lt;scala.Byte#&gt;, List())</code>.</li>
        <li><code>short</code> ~ <code>TypeRef(None, &lt;scala.Short#&gt;, List())</code>.</li>
        <li><code>int</code> ~ <code>TypeRef(None, &lt;scala.Int#&gt;, List())</code>.</li>
        <li><code>long</code> ~ <code>TypeRef(None, &lt;scala.Long#&gt;, List())</code>.</li>
        <li><code>char</code> ~ <code>TypeRef(None, &lt;scala.Char#&gt;, List())</code>.</li>
        <li><code>float</code> ~ <code>TypeRef(None, &lt;scala.Float#&gt;, List())</code>.</li>
        <li><code>double</code> ~ <code>TypeRef(None, &lt;scala.Double#&gt;, List())</code>.</li>
        <li><code>boolean</code> ~ <code>TypeRef(None, &lt;scala.Boolean#&gt;, List())</code>.</li>
        <li><code>void</code> ~ <code>TypeRef(None, &lt;scala.Unit#&gt;, List())</code>.</li>
      </ul>
    </td> </tr>
  <tr>
    <td valign="top">Reference types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.3">96</a>]</td>
    <td>
      <ul>
        <li><code>A</code> ~ <code>TypeRef(None, &lt;A&gt;, List())</code>.</li>
        <li><code>A.B</code> ~ <code>TypeRef(None, &lt;B&gt;, List())</code>.</li>
        <li><code>C&lt;T&gt;</code> ~ <code>TypeRef(None, &lt;C&gt;, List(&lt;T&gt;))</code>.</li>
        <li><code>T[]</code> ~ <code>TypeRef(None, &lt;scala.Array#&gt;, List(&lt;T&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Type variable [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.4">78</a>]</td>
    <td>
      <ul>
        <li>Signature of <code>T</code> ~ <code>TypeSignature(List(), None, None)</code>.</li>
        <li>Signature of <code>T extends A</code> ~ <code>TypeSignature(List(), None, Some(&lt;A&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Parameterized types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.5">79</a>]</td>
    <td>
      <ul>
        <li><code>C&lt;T1, ..., Tn&gt;</code> ~ <code>TypeRef(None, &lt;C&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
        <li><code>C&lt;?&gt;</code> ~ <code>ExistentialType(List(&lt;T&gt;), TypeRef(None, &lt;C&gt;, List(&lt;T&gt;))</code> where signature of <code>&lt;T&gt;</code> is <code>TypeSignature(List(), None, None)</code>.</li>
        <li><code>C&lt;? extends A&gt;</code> ~ <code>ExistentialType(List(&lt;T&gt;), TypeRef(None, &lt;C&gt;, List(&lt;T&gt;))</code> where signature of <code>&lt;T&gt;</code> is <code>TypeSignature(List(), None, &lt;A&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Raw types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.8">95</a>]</td>
    <td>
      <ul>
        <li><code>C</code> ~ <code>TypeRef(None, &lt;C&gt;, List())</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Array types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-10.html#jls-10.1">80</a>]</td>
    <td>
      <ul>
        <li><code>T[]</code> ~ <code>TypeRef(None, &lt;scala.Array#&gt;, List(&lt;T&gt;))</code>.</li>
        <li><code>int[]</code> ~ <code>TypeRef(None, &lt;scala.Array#&gt;, List(&lt;scala.Int#&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Intersection types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.9">81</a>]</td>
    <td>
      <ul>
        <li><code>T1 &amp; ... &amp; Tn</code> ~ <code>IntersectionType(&lt;T1&gt;, ..., &lt;Tn&gt;)</code>.</li>
      </ul>
    </td>
  </tr>
</table>

Notes:
* Primitive and array types are converted to their equivalent
  [Scala type](#scala-type) representations. We may improve on this
  in the future.
* Since Java doesn't support path-dependent types, prefixes in type refs are
  always empty.

<a name="java-signature"></a>
### Signature

In Java, [Signature](#signature) represents definition signatures.
See [below](#java-symbolinformation) to learn which Java definitions
have which signatures.

<a name="java-symbolinformation"></a>
### SymbolInformation

```protobuf
message SymbolInformation {
  reserved 2, 6, 7, 8, 9, 10, 11, 12, 14, 15;
  string symbol = 1;
  Language language = 16;
  Kind kind = 3;
  int32 properties = 4;
  string display_name = 5;
  Signature signature = 17;
  repeated Annotation annotations = 13;
  Access access = 18;
}
```

<table>
  <tr>
    <td><b>Field</b></td> <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>symbol</code></td>
    <td>See <a href="#java-symbol">Symbol</a>.</td>
  </tr>
  <tr>
    <td><code>language</code></td>
    <td><code>JAVA</code>.</td>
  </tr>
  <tr>
    <td><code>kind</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>properties</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>display_name</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>signature</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>annotations</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>access</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
</table>

**Class declarations** [\[76\]][76] are represented by a single symbol with
the `CLASS` kind.

```java
package a;
class C extends S1 implements I {
  T1 m1;
  static T2 m2();
  zero.Overload m3;
  T3 m3(one.Overload e1);
  static T4 m3(two.Overload e2);
  T5 m3(three.Overload e3);
  static class D1<T6 extends S2 & S3, T7> { }
  class D2 { }
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
    <td><code>C</code></td>
    <td><code>a/C#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassSignature(List(), List(&lt;S1&gt;, &lt;I&gt;), None, List(&lt;m1&gt;, &lt;m2&gt;, &lt;m3(Overload)&gt;, &lt;m3(Overload+2)&gt;, &lt;m3(Overload+1)&gt;, &lt;D1&gt;, &lt;D2&gt;))</code></td>
  </tr>
  <tr>
    <td><code>m1</code></td>
    <td><code>a/C#m1.</code></td>
    <td><code>FIELD</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;T1&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m2</code></td>
    <td><code>a/C#m2().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;T2&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a/C#m3.</code></td>
    <td><code>FIELD</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;zero.Overload&gt;))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a/C#m3().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(&lt;e1&gt;), TypeRef(None, &lt;T3&gt;))</code></td>
  </tr>
  <tr>
    <td><code>e1</code></td>
    <td><code>a/C#m3().(e1)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;one.Overload&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a/C#m3(+2).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(&lt;e2&gt;) TypeRef(None, &lt;T4&gt;))</code></td>
  </tr>
  <tr>
    <td><code>e2</code></td>
    <td><code>a/C#m3(+2).(e2)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;two.Overload&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a/C#m3(+1).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(&lte3&gt;), TypeRef(None, &lt;T5&gt;))</code></td>
  </tr>
  <tr>
    <td><code>e3</code></td>
    <td><code>a/C#m3(+1).(e3)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;three.Overload&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>T6</code></td>
    <td><code>a/C#D1#[T6]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeSignature(List(), None, Some(IntersectionType(List(&lt;S2&gt;, &lt;S3&gt;))))</code></td>
  </tr>
  <tr>
    <td><code>T7</code></td>
    <td><code>a/C#D1#[T7]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeSignature(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>D1</code></td>
    <td><code>a/C#D1#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassSignature(List(&lt;T6&gt;, &lt;T7&gt;), List(&lt;java.lang.Object#&gt;), None, List())</code></td>
  </tr>
  <tr>
    <td><code>D2</code></td>
    <td><code>a/C#D2#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassSignature(List(), List(), None, List())</code></td>
  </tr>
</table>

Notes:
* A Java class maps to a single symbol with type `ClassSignature` including all
  static and non-static members. This departs from the Scala compiler internal
  representation of Java classes where non-static members are grouped under a
  `CLASS` symbol and static members are grouped under an `OBJECT` symbol.
* The method `m3(+2)` has a higher disambiguator tag number than `m3(+1)` even
  if `m3(+2)` appears earlier in the source code. This is because `m3(+2)` is
  static and the disambiguator tag is computed from non-static members first
  and static members second. The reason for this required order is to ensure
  that it's possible to compute correct method symbols inside the Scala
  compiler, which groups static member under an `OBJECT` symbol and non-static
  members under a `CLASS` symbol.
* Supported properties for `CLASS` symbols are
  * `FINAL` set for all final classes
  * `ABSTRACT` set for all abstract classes
  * `STATIC` set for static inner classes
  * `ENUM` set for enum types
* Display name for class symbols is equal to the name of the binding
  introduced by the definition.
* Class declarations support [all Java access modifiers](#java-access).
* Class members without explicit access modifiers have access
  `PrivateWithinAccess` within the enclosing package.
* The disambiguators for `m3()`, `m3(+1)` and `m3(+2)` do not take into account
  overloaded field `m3.`.

**Enum declarations** [\[84\]][84] are represented by a single symbol
with the `CLASS` kind.

```java
package a;
public enum Coin {
  PENNY, NICKEL
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td width="200px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>Coin</code></td>
    <td><code>a/Coin#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassSignature(List(), List(&lt;Enum&lt;Coin&gt;&gt;), None, List(&lt;PENNY&gt;, &lt;NICKEL&gt;))</code></td>
  </tr>
  <tr>
    <td><code>PENNY</code></td>
    <td><code>a/Coin#PENNY.</code></td>
    <td><code>FIELD</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Coin&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>NICKEL</code></td>
    <td><code>a/Coin#NICKEL.</code></td>
    <td><code>FIELD</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;Coin&gt;, List()))</code></td>
  </tr>
  <tr>
    <td></td>
    <td><code>a/Coin#values().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Array&gt;, List(&lt;Coin&gt;)))</code></td>
  </tr>
  <tr>
    <td></td>
    <td><code>a/Coin#valueOf().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;Coin&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Enum declarations follow the same rules as class declarations.
* Supported properties for enum declarations are:
  * `FINAL`: implicitly set for all enum declarations.
  * `STATIC`: implicitly set for all enum declarations.
  * `ENUM`: implicitly set for all enum declarations.
* Display name for enum symbols is equal to the name of the binding
  introduced by the definition.
* JLS mandates the following synthetic members for enum declarations
  [\[86\]][86]:
  * Enum fields have kind `FIELD`, properties `FINAL`, `STATIC` and `ENUM`,
    are named after the corresponding enum constants, have the type of the
    enum declaration and `PublicAccess` access.
  * `valueOf` has kind `METHOD`, property `STATIC`, have a method type that
    goes from a `<String>` parameter to the enum declaration and
    `PublicAccess` access.
  * `values` has kind `METHOD`, property `STATIC`, have a method type that
    goes from an empty parameter list to an array of the enum declaration
    and `PublicAccess` access.
* Enum declarations support [all Java access modifiers](#java-access).

**Interface declarations** [\[77\]][77] are represented by a single symbol
like classes but with the `INTERFACE` kind.

```java
package a;
public interface List<T> extends I {
  T head();
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td width="200px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>List</code></td>
    <td><code>a/List#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassSignature(List(&lt;T&gt;), List(&lt;I&gt;), None, List(&lt;head&gt;))</code></td>
  </tr>
  <tr>
    <td><code>head</code></td>
    <td><code>a/List#head().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
</table>

The differences between interface symbols and class symbols are:

* Interfaces do not have constructors.
* Supported properties for interface symbols are:
  * `ABSTRACT`: implicitly set for all interface symbols.
* Display name for interface symbols is equal to the name of the binding
  introduced by the definition.
* Interface declarations support
  [all Java access modifiers](#java-access).
* Interface members without explicit access modifiers have access
  `PublicAccess` by default instead of `PrivateWithinAccess`.

**Method declarations** [\[82\]][82] are represented by a single symbol with
the `METHOD` kind and one symbol for each type parameter with kind
`TYPE_PARAMETER` and formal parameter with kind `PARAMETER`.

```java
package a;
class A {
  A m1();
  A m2(T1 t1) throws E;
  <T2> T2 m3(T2 t2);
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td width="200px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>A</code></td>
    <td><code>a/A#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassSignature(List(), List(), None, List(&lt;m1&gt;, &lt;m2&gt;, &lt;m3&gt;))</code></td>
  </tr>
  <tr>
    <td><code>m1</code></td>
    <td><code>a/A#m1().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(), TypeRef(None, &lt;A&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m2</code></td>
    <td><code>a/A#m2().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(), List(&lt;t1&gt;), TypeRef(None, &lt;A&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>t1</code></td>
    <td><code>a/A#m2().(t1)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;T1&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a/A#m3().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodSignature(List(&lt;T2&gt;), List(&lt;t2&gt;), TypeRef(None, &lt;T2&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a/A#m3().[T2]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeSignature(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>t2</code></td>
    <td><code>a/A#m3().(t2)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;T2&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* For type bounds of type parameters, we leave the mapping between type syntax
  written in source code and `Type` entities deliberately unspecified.
  For example, a producer may represent the signature of `T2` as
  `TypeSignature(List(), None, <Object>)` instead of
  `TypeSignature(List(), None, None)`.
* When compiled with the compiler option `-parameters`, both display and symbol
  names of method parameters match their names written in source. Otherwise,
  parameters have both display and symbol names `paramN` where `N` is the index
  of that given parameter starting at index 0.
* Variable arity parameters have the type equals to `RepeatedType(<tpe>)`,
  where `<tpe>` is their type as declared in original source.
* Method throws clauses are not modelled in SemanticDB. We may improve on this
  in the future.
* Supported properties for method symbols are:
  * `FINAL`: set for `final` methods.
  * `STATIC`: set for `static` methods.
  * `ABSTRACT`: set for `abstract` methods.
  * `DEFAULT`: set for `default` methods.
* Display name for method symbols is equal to the name of the binding
  introduced by the definition.
* Method declarations support [all Java access modifiers](#java-access),
  however method declarations in interfaces can only be `PublicAccess`.

**Field declarations** [\[83\]][83] are represented by a single symbol with
the `FIELD` kind.

```java
package a;
class A {
  A field;
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td width="200px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>A</code></td>
    <td><code>a/A#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassSignature(List(), List(), None, List(&lt;field&gt;))</code></td>
  </tr>
  <tr>
    <td><code>field</code></td>
    <td><code>a/A#field.</code></td>
    <td><code>FIELD</code></td>
    <td><code>ValueSignature(TypeRef(None, &lt;A&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Supported properties for field symbols are:
  * `FINAL`: set for `final` fields and interface fields.
  * `STATIC`: set for `static` fields and interface fields.
* Display name for field symbols is equal to the name of the binding
  introduced by the definition.
* Field declarations support [all Java access modifiers](#java-access).
  However, field declarations in interfaces can only be `PublicAccess`.

**Constructor declarations** [\[90\]][90] are represented by a single symbol
with display and symbol name `<init>` and the `CONSTRUCTOR` kind. Constructor
formal parameters are represented the same way as method declaration formal
parameters.

```java
package a;
class Outer {
  Outer() {}
  class Inner {
    Inner() {}
  }
}
```

<table>
  <tr>
    <td><b>Definition</b></td>
    <td width="200px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>Outer</code></td>
    <td><code>a/Outer#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassSignature(List(), List(), None, List(&lt;a.Outer#&lt;init&gt;, &lt;Inner&gt;))</code></td>
  </tr>
  <tr>
    <td>Constructor of <code>Outer</code></td>
    <td><code>a/Outer#&lt;init&gt;().</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td><code>MethodSignature(List(), List(), None)</code></td>
  </tr>
  <tr>
    <td><code>Inner</code></td>
    <td><code>a/Outer#Inner#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassSignature(List(), List(), None, List(&lt;a.Outer#Inner#&lt;init&gt;))</code></td>
  </tr>
  <tr>
    <td>Constructor of <code>Inner</code></td>
    <td><code>a/Outer#Inner#&lt;init&gt;().</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td><code>MethodSignature(List(), List(), None)</code></td>
  </tr>
</table>

Notes:
* Constructors don't have type parameters and return types, but we still
  represent their signatures with `MethodSignature`. In these signatures,
  type parameters are equal to `List()` and the return type is `None`.
* Constructor declarations support no properties.
* Display name for constructor symbols is equal to `<init>`.
* Constructor declarations support
  [all Java access modifiers](#java-access).

**Packages** [\[94\]][94] are not included in the
["Symbols"](#symbolinformation) section.

<a name="java-root-package"></a>
#### Root package

The root package is a synthetic package that does not exist in the JLS but
has an equivalent in the SLS [\[20\]][20].
The root package is the owner of all unnamed and all top-level named packages.
The motivation to define a root package for the Java language is to keep
consistency with how package owners are modelled in
[Scala symbols](#scala-symbol).

<a name="java-annotation"></a>
### Annotation

```protobuf
message Annotation {
  Type tpe = 1;
}
```

In Java, [Annotation](#annotation) represents `access_flags` in the JVMS `class`
file format [\[92\]][92] but not the actual annotations [\[93\]][93].
We may improve on this in the future.

<table>
  <tr>
    <td><b>Value</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>Annotation(TypeRef(None, &lt;scala.annotation.strictfp&gt;, List()))</code></td>
    <td> Declared <code>strictfp</code>; floating-point mode is FP-strict e.g. <code>strictfp class MyClass</code>.</td>
  </tr>
  <tr>
    <td>Not supported</td>
    <td>JLS annotations <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-9.html#jls-9.7">[93]</a></td>
  </tr>
</table>

<a name="java-access"></a>
### Access

```protobuf
message Access {
  oneof sealed_value {
    PrivateAccess privateAccess = 1;
    PrivateThisAccess privateThisAccess = 2;
    PrivateWithinAccess privateWithinAccess = 3;
    ProtectedAccess protectedAccess = 4;
    ProtectedThisAccess protectedThisAccess = 5;
    ProtectedWithinAccess protectedWithinAccess = 6;
    PublicAccess publicAccess = 7;
  }
}

message PrivateAccess {
}

message PrivateThisAccess {
}

message PrivateWithinAccess {
  string symbol = 1;
}

message ProtectedAccess {
}

message ProtectedThisAccess {
}

message ProtectedWithinAccess {
  string symbol = 1;
}

message PublicAccess {
}
```

In Java, [Access](#access) represents access control [\[87\]][87]
of names.
<table>
  <tr>
    <td><b>Access</b></td>
    <td width="125px"><b>Code</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>None</code></td>
    <td><code></code></td>
    <td>
      Definitions that can't have access modifiers, i.e. `LOCAL`, `PARAMETER`,
      `TYPE_PARAMETER` and `PACKAGE`.
      Definitions that can have access modifiers, but don't have them will
      have `PrivateWithinAccess` or `PublicAccess` as described below.
    </td>
  </tr>
  <tr>
    <td><code>PrivateAccess</code></td>
    <td><code>private F f;</code></td>
    <td>
      Can be accessed only from within the directly enclosing class.
      <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-6.html#jls-6.6.1">[88]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PrivateWithinAccess(<x>)</code></td>
    <td><code>package x; class A {}</code></td>
    <td>
      A class, interface, class member or constructor declared without an access
      modifier is implicitly private within the package in which is declared.
      <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-6.html#jls-6.6.1">[88]</a>.
    </td>
  </tr>
  <tr>
    <td><code>ProtectedAccess()</code></td>
    <td><code>protected F f;</code></td>
    <td>
      A protected member of constructor of an object can be accessed from
      within: 1) the enclosing class, 2) all classes that are responsible for
      the implementation of that object.
      <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-6.html#jls-6.6.2">[89]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PublicAccess()</code></td>
    <td><code>public F f;</code></td>
    <td>
      Can be accessed from from any code provided that the compilation unit
      in which it is declared is observable. Packages are always implicitly
      public. Members of interfaces lacking interface modifiers are
      implicitly public. Other members are public only if explicitly declared
      `public`.
      <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-6.html#jls-6.6.1">[88]</a>.
    </td>
  </tr>
</table>

Notes:
* `PrivateThisAccess`, `ProtectedThisAccess`, `ProtectedWithinAccess`
are not supported in the Java language.

<a name="java-symboloccurrence"></a>
### SymbolOccurrence

At this moment, there is no tool that supports SymbolOccurrences for
the Java language. We intend to improve on this in the future.

<a name="java-synthetic"></a>

### Synthetic
At this moment, there is no tool that supports Synthetic for
the Java language. We may improve on this in the future.

[semanticdb.proto]: semanticdb.proto
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
[72]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-10.html#jls-10.1
[73]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-PrimitiveType
[74]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html
[75]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.2
[76]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.1
[77]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-9.html#jls-9.1
[78]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.4
[79]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.5
[80]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-10.html#jls-10.1
[81]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.9
[82]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.4
[83]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.3
[84]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.9
[85]: https://docs.oracle.com/javase/specs/jls/se8/html
[86]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.9.3
[87]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-6.html#jls-6.6
[88]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-6.html#jls-6.6.1
[89]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-6.html#jls-6.6.2
[90]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.8
[91]: https://docs.oracle.com/javase/specs/jvms/se8/html/index.html
[92]: https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html
[93]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-9.html#jls-9.7
[94]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-7.html
[95]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.8
[96]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.3
[97]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-7.html#jls-7.4.2
[99]: https://www.scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html#templates
