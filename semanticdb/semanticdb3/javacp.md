### Java

In this section, we exhaustively map Java language features onto SemanticDB.
As a reference, we use the Java Language Specification [\[85\]][85] (referred
to as "JLS" in the text below) and Java Virtual Machine Specification [\[91\]][91] (referred to as "JVMS" in the text below).

<a name="java-symbol"></a>
#### Symbol

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
          For unnamed package <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-7.html#jls-7.4.2">[21]</a>,
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
      Concatenation of <code>local</code> and a decimal number.
    </td>
  </tr>
</table>

**Owner** is:
  * For root package, `None`.
  * For unnamed package, root package.
  * For top-level named package, root package.
  * For other named package, parent package.
  * For other top-level declaration, its package.
  * For other global definition, the innermost enclosing definition,
    i.e. the definition whose [Location](#location) in source code most
    tightly encloses the [Location](#location) of the original definition.
  * For other declarations, `None`.

**Descriptor** is:
  * For `FIELD` or `PACKAGE`, concatenation of its simple name and a dot (`.`).
  * For `METHOD` or `CONSTRUCTOR`, concatenation of its identifier, a disambiguator, and a dot (`.`).
  * For `CLASS` or `INTERFACE`, concatenation of its identifier and a pound sign (`#`).
  * For `PARAMETER`, concatenation of a left parenthesis (`(`), its identifier and a right parenthesis (`)`).
  * For `TYPE_PARAMETER`, concatenation of a left bracket (`[`), its identifier and a right bracket (`]`).
  * See [SymbolInformation](#java-symbolinformation) for details on
    which Java definitions are modelled by which symbols.

**Disambiguator** is:
  * Concatenation of a left parenthesis (`(`), a type descriptor
    and a right parenthesis (`)`).
    In the case when multiple definitions have the same kind, name and
    type descriptor, the type descriptor is appended with `+N`,
    with `+1` going to the method that is defined first in the source code,
    `+2` going to the method that is defined second, etc.

**Name** is:
  * For root package, `_root_`.
  * For unnamed package, `_empty_`.
  * For constructor, `<init>`.
  * For other definition, the identifer of the binding introduced by the definition.

**Type descriptor** is:
  * For `TYPE_REF`, encoded name of `symbol`.
  * For `ANNOTATED_TYPE`, type descriptor of `tpe`.
  * For `EXISTENTIAL_TYPE`, type descriptor of `tpe`.
  * For `METHOD_TYPE`, concatenation of type descriptor of its formal parameter types
    interspered with a comma (`,`).
  * For `REPEATED_TYPE`, concatenation of type descriptor of `tpe` and a star (`*`).
  * See [Type](#java-type) for details on which Java types are modelled by which `Type` entities.

  For example, this is how some of the definitions from the Java standard library must be modeled.

  * The `java` package: `java.`
  * The `Integer` class: `java.lang.Integer#`
  * The `int` primitive: `scala.Int#`
  * The `Arrays.asList` method: `java.util.Arrays#asList(T*).`
  * The `a` parameter of that method: `java.util.Arrays#asList(T*).(a)`
  * The `T` type parameter of that method: `java.util.Arrays#asList(T*).[T]`


<a name="java-root-package"></a>
##### Root package

The root package is a synthetic package that does not exist in the JLS but
has an equivalent in the SLS [\[20\]][20].
The root package is the owner of all unnamed and all top-level named packages.
The motivation to define a root package for the Java language is to keep
consistency with how package owners are encoded in [Scala symbols](#scala-symbol).

<a name="java-type"></a>
#### Type

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

In Java, [Type](#type) represents types [\[74\]][74].

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
        <li><code>T</code> ~ <code>TypeRef(None, &lt;T&gt;, List())</code>.</li>
        <li><code>S.T</code> ~ <code>TypeRef(Some(&lt;S&gt;), &lt;T&gt;, List())</code>.</li>
        <li><code>C&lt;T&gt;</code> ~ <code>TypeRef(None, &lt;C&gt;, List(&lt;T&gt;))</code>.</li>
        <li><code>T[]</code> ~ <code>TypeRef(None, &lt;scala.Array#&gt;, List(&lt;T&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Type variable [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.4">78</a>]</td>
    <td>
      <ul>
        <li>Signature of <code>T</code> ~ <code>TypeType(List(), None, None)</code>.</li>
        <li>Signature of <code>T extends S</code> ~ <code>TypeType(List(), None, Some(&lt;S&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Type variable [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.4">78</a>]</td>
    <td>
      <ul>
        <li>Signature of <code>T</code> ~ <code>TypeType(List(), None, None)</code>.</li>
        <li>Signature of <code>T extends S</code> ~ <code>TypeType(List(), None, Some(&lt;S&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Parameterized types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.5">79</a>]</td>
    <td>
      <ul>
        <li><code>C&lt;T1, ..., Tn&gt;</code> ~ <code>TypeRef(None, &lt;C&gt;, List(&lt;T1&gt;, ..., &lt;Tn&gt;))</code>.</li>
        <li><code>C&lt;?&gt;</code> ~ <code>ExistentialType(List(&lt;T&gt;), TypeRef(None, &lt;C&gt;, List(&lt;T&gt;))</code> where signature of <code>&lt;T&gt;</code> is <code>TypeType(List(), None, None)</code>.</li>
        <li><code>C&lt;? extends T&gt;</code> ~ <code>TypeRef(None, &lt;C&gt;, List(&lt;local_wildcard&gt;))</code>.</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td valign="top">Raw types [<a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-4.html#jls-4.8">95</a>]</td>
    <td>
      <ul>
        <li><code>T</code> ~ <code>TypeRef(None, &lt;T&gt;, List())</code>.</li>
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
* Primitive and array types are converted to their equivalent [Scala type](#scala-type) representations. We may improve on this in the future.
* Wildcard type arguments of parameterized types are currently incorrectly represented as `local_wildcard`,
  this behavior is expected to change in the future and should not be relied upon.

<a name="java-symbolinformation"></a>
#### SymbolInformation

```protobuf
message SymbolInformation {
  reserved 2, 6, 7, 8, 9, 10, 12;
  string symbol = 1;
  Language language = 16;
  Kind kind = 3;
  int32 properties = 4;
  string name = 5;
  Type tpe = 11;
  repeated Annotation annotations = 13;
  Accessibility accessibility = 14;
  string owner = 15;
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
    <td><code>name</code></td>
    <td>See <a href="#java-symbol">Symbol</a>.</td>
  </tr>
  <tr>
    <td><code>tpe</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>annotations</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>accessibility</code></td>
    <td>Explained below on per-definition basis.</td>
  </tr>
  <tr>
    <td><code>owner</code></td>
    <td>See <a href="#java-symbol">Symbol</a>.</td>
  </tr>
</table>

**Class declarations** [\[76\]][76] are represented by a single symbol with the `CLASS` kind.

```java
package a;
class C extends S1 implements I {
  T1 m1;
  static T2 m2();
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
    <td><code>a.C#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(), List(&lt;S1&gt;, &lt;I&gt;), List(&lt;m1&gt;, &lt;m2&gt;, &lt;m3(Overload)&gt;, &lt;m3(Overload+1)&gt;, &lt;m3(Overload+2)&gt;, &lt;D1&gt;, &lt;D2&gt;))</code></td>
  </tr>
  <tr>
    <td><code>m1</code></td>
    <td><code>a.C#m1.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;T1&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>m2</code></td>
    <td><code>a.C#m2().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;T2&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a.C#m3(Overload).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(&lt;a.C#m3(one.Overload).(e1)&gt;), TypeRef(None, &lt;T3&gt;))</code></td>
  </tr>
  <tr>
    <td><code>e1</code></td>
    <td><code>a.C#m3(Overload).(e1)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;one.Overload&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a.C#m3(Overload+1).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(&lt;a.C#m3(Overload+1).(e3)&gt;), TypeRef(None, &lt;T5&gt;))</code></td>
  </tr>
  <tr>
    <td><code>e3</code></td>
    <td><code>a.C#m3(Overload+1).(e3)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;three.Overload&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a.C#m3(Overload+2).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(&lt;a.C#m3(Overload+2).(e3)&gt;) TypeRef(None, &lt;T4&gt;))</code></td>
  </tr>
  <tr>
    <td><code>e2</code></td>
    <td><code>a.C#m3(Overload+2).(e2)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;two.Overload&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>T6</code></td>
    <td><code>a.C#D1#[T6]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeType(List(), None, Some(IntersectionType(List(&lt;S2&gt;, &lt;S3&gt;))))</code></td>
  </tr>
  <tr>
    <td><code>T7</code></td>
    <td><code>a.C#D1#[T7]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeType(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>D1</code></td>
    <td><code>a.C#D1#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(&lt;T6&gt;, &lt;T7&gt;), List(&lt;java.lang.Object#&gt;), List())</code></td>
  </tr>
  <tr>
    <td><code>D2</code></td>
    <td><code>a.C#D2#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(), List(), List())</code></td>
  </tr>
</table>

Notes:
* Class members must appear in the order specified below. This requirement is
  necessary to compute consistent method symbol disambiguators in the Scala
  compiler where static and non-static members have separate owners.
  * non-static members first, following the same order as they appear in the original source. In the example above, observe that 
  * static members secondly, following the same order as they appear in the original source
* A Java class maps to a single symbol with type `ClassInfoType` including all static and non-static members.
  This departs from the Scala compiler internal representation of Java classes where non-static members
  are grouped under a `CLASS` symbol and static members are grouped under an `OBJECT` symbol.
* Supported properties for `CLASS` symbols are
  * `FINAL` set for all final classes
  * `ABSTRACT` set for all abstract classes
  * `STATIC` set for static inner classes
  * `ENUM` set for enum types
* Supported accessibilities for `CLASS` symbols are
  * `PRIVATE`: set for `private` inner classes
  * `PRIVATE_WITHIN`: set for classes without explicit access modifiers
  * `PUBLIC`: set for `public` classes

**Enum types** [\[84\]][84] are represented by a single symbol with the `CLASS` kind.

```java
package a;

public enum Coin {
  PENNY, NICKEL
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
    <td><code>Coin</code></td>
    <td><code>a.Coin#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(), List(&lt;Enum&lt;Coin&gt;&gt;), List(&lt;PENNY&gt;, &lt;NICKEL&gt;))</code></td>
  </tr>
  <tr>
    <td><code>PENNY</code></td>
    <td><code>a.Coin#PENNY.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;Coin&gt;), List())</code></td>
  </tr>
  <tr>
    <td><code>NICKEL</code></td>
    <td><code>a.Coin#NICKEL.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;Coin&gt;), List())</code></td>
  </tr>
  <tr>
    <td><code>values</code></td>
    <td><code>a.Coin#values().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Array&gt;, List(&lt;Coin&gt;)))</code></td>
  </tr>
  <tr>
    <td><code>valueOf</code></td>
    <td><code>a.Coin#valueOf(String).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Coin&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Enum types follow the same rules as class declarations.
* Enum types and enum fields must have the property `ENUM` set.
* Enum fields have the type of their enclosing class.
* Enum members include the implicitly declared methods [\[86\]][86] `valueOf` and `values`.
* Supported properties for enum symbols are:
  * `STATIC`: implicitly set for all enum types.
  * `FINAL`: implicitly set for all enum types.

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
    <td width="275px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>List</code></td>
    <td><code>a.List#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(&lt;T&gt;), List(&lt;I&gt;), List(&lt;head&gt;))</code></td>
  </tr>
  <tr>
    <td><code>head</code></td>
    <td><code>a.List#head().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;T&gt;, List()))</code></td>
  </tr>
</table>

The differences between interface symbols and class symbols are:

* Interfaces do not have constructors.
* Supported properties for interface symbols are:
  * `ABSTRACT`: implicitly set for all interface symbols.
* Interface members without explicit access modifiers have accessibility
  `PUBLIC` by default instead of `PRIVATE_WITHIN`.

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
    <td width="275px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>A</code></td>
    <td><code>a.A#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(), List(), List(&lt;m1&gt;, &lt;m2&gt;, &lt;m3&gt;))</code></td>
  </tr>
  <tr>
    <td><code>m1</code></td>
    <td><code>a.A#m1().</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;A&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m2</code></td>
    <td><code>a.A#m2(T1).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(), List(&lt;t1&gt;), TypeRef(None, &lt;A&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>t1</code></td>
    <td><code>a.A#m2(T1).(t1)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;T1&gt;, List())</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a.A#m3(T2).</code></td>
    <td><code>METHOD</code></td>
    <td><code>MethodType(List(&lt;T2&gt;), List(&lt;t2&gt;), TypeRef(None, &lt;T2&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>m3</code></td>
    <td><code>a.A#m3(T2).[T2]</code></td>
    <td><code>TYPE_PARAMETER</code></td>
    <td><code>TypeType(List(), None, None)</code></td>
  </tr>
  <tr>
    <td><code>t2</code></td>
    <td><code>a.A#m3(T2).(t2)</code></td>
    <td><code>PARAMETER</code></td>
    <td><code>TypeRef(None, &lt;T2&gt;, List())</code></td>
  </tr>
</table>

Notes:
* When compiled with the compiler option `-parameters`, the name of method
  parameters matches their name written in source. Otherwise, parameters have
  the name `paramN` where `N` is the index of that given parameter starting at
  index 0.
* Method throws clauses are discarded.
* Supported properties for method symbols are:
  * `FINAL`: set for `final` methods.
  * `STATIC`: set for `static` methods.
  * `ABSTRACT`: set for `abstract` methods.
* Method declarations support [all Java accessibilities](#java-accessibility).

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
    <td width="275px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>A</code></td>
    <td><code>a.A#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(), List(), List(&lt;head&gt;))</code></td>
  </tr>
  <tr>
    <td><code>field</code></td>
    <td><code>a.A#field.</code></td>
    <td><code>FIELD</code></td>
    <td><code>TypeRef(None, &lt;A&gt;, List())</code></td>
  </tr>
</table>

Notes:
* Supported properties for field symbols are:
  * `FINAL`: set for `final` fields.
  * `STATIC`: set for `static` fields.
* Field declarations support [all Java accessibilities](#java-accessibility).

**Constructor declarations** [\[90\]][90] are represented by a single symbol with
name `<init>` and the `CONSTRUCTOR` kind. Constructor formal parameters are represented
the same way as method declaration formal parameters.

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
    <td width="275px"><b>Symbol</b></td>
    <td><b>Kind</b></td>
    <td><b>Signature</b></td>
  </tr>
  <tr>
    <td><code>Outer</code></td>
    <td><code>a.Outer#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(), List(), List(&lt;a.Outer#&lt;init&gt;, &lt;Inner&gt;))</code></td>
  </tr>
  <tr>
    <td><code>&lt;init&gt;</code></td>
    <td><code>a.Outer#&lt;init&gt;().</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Outer&gt;, List()))</code></td>
  </tr>
  <tr>
    <td><code>Inner</code></td>
    <td><code>a.Outer#Inner#</code></td>
    <td><code>CLASS</code></td>
    <td><code>ClassInfoType(List(), List(), List(&lt;a.Outer#Inner#&lt;init&gt;))</code></td>
  </tr>
  <tr>
    <td><code>&lt;init&gt;</code></td>
    <td><code>a.Outer#Inner#&lt;init&gt;().</code></td>
    <td><code>CONSTRUCTOR</code></td>
    <td><code>MethodType(List(), List(), TypeRef(None, &lt;Inner&gt;, List()))</code></td>
  </tr>
</table>

Notes:
* Constructors don't have type parameters and return types, but we still
  represent their signatures with `MethodType`. In these signatures,
  type parameters are equal to `List()` and the return type
  is the type of the enclosing class parameterized with references to its
  type parameters.
* Constructor declarations support no properties.
* Constructor declarations support [all Java accessibilities](#java-accessibility).

**Packages** [\[94\]][94] are represented by `PACKAGE` symbols.
* Packages have no `tpe`
* Package only support `PUBLIC` accessibility
* Package support no properties

<a name="java-annotation"></a>
#### Annotation

```protobuf
message Annotation {
  Type tpe = 1;
}
```

In Java, [Annotation](#annotation) represents `access_flags` in the JVMS `class` file format [\[92\]][92] but not the actual annotations [\[93\]][93].
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

<a name="java-accessibility"></a>
#### Accessibility

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

In Java, [Accessibility](#accessibility) represents access control [\[87\]][87] of names
<table>
  <tr>
    <td><b>Accessibility</b></td>
    <td><b>Code</b></td>
    <td><b>Explanation</b></td>
  </tr>
  <tr>
    <td><code>PRIVATE</code></td>
    <td><code>private void m() {}</code></td>
    <td>
      Can be accessed only from within the directly enclosing class.
      <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-6.html#jls-6.6.1">[88]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PRIVATE_WITHIN</code></td>
    <td><code>package x; class A {}</code></td>
    <td>
      A class, interface, class member or constructor declared without an access
      modifier is implicitly private within within the package in which is
      declared.
      <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-6.html#jls-6.6.1">[88]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PROTECTED</code></td>
    <td><code>protected void m() {}</code></td>
    <td>
      A protected member of constructor of an object can be accessed from
      within: 1) the enclosing class, 2) all classes that are responsible for
      the implementation of that object.
      <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-6.html#jls-6.6.2">[89]</a>.
    </td>
  </tr>
  <tr>
    <td><code>PUBLIC</code></td>
    <td><code>public void m() {}</code></td>
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
* `PRIVATE_THIS`, `PROTECTED_THIS`, `PROTECTED_WITHIN` are not supported in the Java language.

<a name="java-symboloccurrence"></a>
#### SymbolOccurrence

At this moment, there is no tool that supports SymbolOccurrences for the Java language.

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