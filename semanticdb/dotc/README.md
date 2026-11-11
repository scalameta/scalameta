# semanticdb-dotc

## Use from Scala 3

scalameta publishes `org.scalameta:semanticdb-dotc_3` with each release. Scala 3 takes the
sources jar, through the `sourcedeps` configuration, and compiles the classes into its compiler.

- The message classes are in `dotty.tools.dotc.semanticdb`, and the runtime is in
  `dotty.tools.dotc.semanticdb.internal`, the packages of Scala 3's copy.
- The classes depend on nothing but the Scala 3 library.
- The classes follow scalameta's `semanticdb.proto`. A release that changes the schema can need
  edits in Scala 3's code that builds or matches these classes, such as for a new field or a new
  enum value.

## The runtime

The classes in `src/main/scala-3/dotty/tools/dotc/semanticdb/internal`, except `MD5`, are adapted
by hand. To adapt them to a new upstream version, diff each upstream file below between the
version named here and the new one, and carry over each change to a member that we kept.

Each class with a counterpart in Scala 3's copy of these classes has the name of that
counterpart, because Scala 3's compiler and `mtags-shared` call the classes by those names. Each
class compiles under Scala 3's own options, `-Yexplicit-nulls` included, so a value that can be
`null` has a type `T | Null`.

### SemanticdbInputStream

- Upstream:
  - `java/core/src/main/java/com/google/protobuf/CodedInputStream.java` of protobuf at tag
    `v35.0`, which is protobuf-java 4.35.0;
  - `Utf8.java` of protobuf at `v35.0`, in the same directory, for the UTF-8 check of
    `readStringRequireUtf8`;
  - `shared/src/main/scala/com/google/protobuf/CodedInputStream.scala` of
    `scalapb/protobuf-scala-runtime` at tag `v0.9.0`.
- Kept: the members that the generated classes, Scala 3 and `mtags-shared` call: `newInstance`
  for a stream and for an array, `readTag`, `skipField`, `checkLastTagWas`, `isAtEnd`,
  `pushLimit`, `popLimit`, the reads of the schema's scalar types, `readString`,
  `readStringRequireUtf8` and `readRawVarint32`.
- Changed:
  - one class for arrays and streams, with the port's buffer and limit code;
  - no varint fast path, which changes only the speed;
  - no size limit and no setters: protobuf-java's default limit is `Integer.MAX_VALUE`, and its
    default recursion limit of 100 is fixed here;
  - `readStringRequireUtf8` checks the bytes only if the string has U+FFFD, unlike the port and
    `Utf8.SafeProcessor`. `Utf8.UnsafeProcessor`, which a standard JVM uses, does the same. To
    check the bytes, it encodes the string again, where we run the JDK's strict decoder.

### SemanticdbOutputStream

- Upstream: `CodedOutputStream.java` and `Utf8.java` of protobuf at `v35.0`, and
  `CodedOutputStream.scala` of `scalapb/protobuf-scala-runtime` at `v0.9.0`, in the directories
  above.
- Kept: `newInstance` for a stream and for an array, `flush`, `checkNoSpaceLeft`, the writes and
  the `compute*Size` methods of the schema's scalar types, `writeTag` and `writeUInt32NoTag`.
- Changed: one class for arrays and streams, and no fast path for strings. `writeString` uses
  `String.getBytes`, which writes the same bytes as protobuf-java's `Utf8.java`, `?` for a lone
  surrogate. The message of `OutOfSpaceException` is our own.

### WireFormat and InvalidProtocolBufferException

- Upstream: `WireFormat.java` and `InvalidProtocolBufferException.java` of protobuf at `v35.0`.
- Kept: the wire types and the three tag methods; the errors that `SemanticdbInputStream`
  throws.
- Changed: the constants have Scala names. The messages are protobuf-java's, with
  `SemanticdbInputStream` for `CodedInputStream`, and without the hint to `setRecursionLimit`,
  which does not exist here. `invalidWireType` returns an `InvalidProtocolBufferException`, not
  protobuf-java's subclass `InvalidWireTypeException`.

### The ScalaPB traits

`SemanticdbGeneratedEnum`, `SemanticdbGeneratedEnumCompanion`, `SemanticdbUnrecognizedEnum`,
`SemanticdbGeneratedMessage`, `SemanticdbGeneratedMessageCompanion`, `SemanticdbGeneratedOneof`,
`SemanticdbGeneratedSealedOneof`, `SemanticdbTypeMapper` and `LiteParser`:

- Upstream: `GeneratedMessageCompanion.scala`, `TypeMapper.scala` and `LiteParser.scala` in
  `scalapb-runtime/src/main/scala/scalapb` of ScalaPB at tag `v0.11.20`.
- Kept: the members that the generated classes and Scala 3 call, with ScalaPB's code, `merge`
  included.
- Changed: the rewriter removes the generated members that need ScalaPB's descriptors or its
  text format, so the traits do not declare them. Scala 3's copy has no
  `SemanticdbGeneratedEnumCompanion`. Our trait of that name lets the generated enums keep
  `companion`.

### MD5

Our own code. `MD5.compute` returns the MD5 digest of the UTF-8 bytes of a string, in upper-case
hex. Scala 3 writes it into `TextDocument.md5`, so the case must stay what Scala 3's own copy
wrote, which is also what `semanticdb-scalac` writes.
