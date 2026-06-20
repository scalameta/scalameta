package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.Scala._

import munit.FunSuite

class DescriptorSuite extends FunSuite {

  // Builds a stable label for test names without relying on Descriptor.toString.
  private def label(desc: Descriptor): String = desc match {
    case Descriptor.None => "None"
    case Descriptor.Term(value) => s"Term($value)"
    case Descriptor.Method(value, disambiguator) => s"Method($value,$disambiguator)"
    case Descriptor.Type(value) => s"Type($value)"
    case Descriptor.Package(value) => s"Package($value)"
    case Descriptor.Parameter(value) => s"Parameter($value)"
    case Descriptor.TypeParameter(value) => s"TypeParameter($value)"
  }

  def checkValue(desc: Descriptor, expected: String): Unit =
    test("  value: " + label(desc))(assertEquals(desc.value, expected))

  def checkName(desc: Descriptor, expected: Names.Name): Unit =
    test("   name: " + label(desc))(assertEquals(desc.name, expected))

  def checkToString(desc: Descriptor, expected: String): Unit =
    test("toString: " + label(desc))(assertEquals(desc.toString, expected))

  def checkEncode(value: String, expected: String): Unit =
    test(" encode: [" + value + "]")(assertEquals(Names.encode(value), expected))

  // value: the descriptor's bare name; Method drops its disambiguator.
  checkValue(Descriptor.Term("x"), "x")
  checkValue(Descriptor.Method("f", "()"), "f")
  checkValue(Descriptor.Type("T"), "T")
  checkValue(Descriptor.Package("p"), "p")
  checkValue(Descriptor.Parameter("x"), "x")
  checkValue(Descriptor.TypeParameter("A"), "A")

  // name: Term/Method/Package/Parameter -> TermName; Type/TypeParameter -> TypeName. Method drops the
  // disambiguator.
  checkName(Descriptor.Term("x"), Names.TermName("x"))
  checkName(Descriptor.Method("f", "()"), Names.TermName("f"))
  checkName(Descriptor.Type("T"), Names.TypeName("T"))
  checkName(Descriptor.Package("p"), Names.TermName("p"))
  checkName(Descriptor.Parameter("x"), Names.TermName("x"))
  checkName(Descriptor.TypeParameter("A"), Names.TypeName("A"))

  // predicates: each descriptor satisfies its own predicate and no sibling's. In particular a Method
  // is isMethod but NOT isTerm.
  test("pred: Term") {
    assert(Descriptor.Term("x").isTerm)
    assert(!Descriptor.Term("x").isMethod)
    assert(!Descriptor.Term("x").isType)
    assert(!Descriptor.Term("x").isNone)
  }
  test("pred: Method") {
    assert(Descriptor.Method("f", "()").isMethod)
    assert(!Descriptor.Method("f", "()").isTerm)
    assert(!Descriptor.Method("f", "()").isType)
  }
  test("pred: Type") {
    assert(Descriptor.Type("T").isType)
    assert(!Descriptor.Type("T").isTerm)
    assert(!Descriptor.Type("T").isTypeParameter)
  }
  test("pred: Package") {
    assert(Descriptor.Package("p").isPackage)
    assert(!Descriptor.Package("p").isTerm)
    assert(!Descriptor.Package("p").isType)
  }
  test("pred: Parameter") {
    assert(Descriptor.Parameter("x").isParameter)
    assert(!Descriptor.Parameter("x").isTypeParameter)
    assert(!Descriptor.Parameter("x").isTerm)
  }
  test("pred: TypeParameter") {
    assert(Descriptor.TypeParameter("A").isTypeParameter)
    assert(!Descriptor.TypeParameter("A").isParameter)
    assert(!Descriptor.TypeParameter("A").isType)
  }
  test("pred: None") {
    assert(Descriptor.None.isNone)
    assert(!Descriptor.None.isTerm)
    assert(!Descriptor.None.isType)
  }

  // toString: formats the descriptor as it appears inside a symbol, backtick-encoding awkward names.
  checkToString(Descriptor.Term("foo"), "foo.")
  checkToString(Descriptor.Method("foo", "()"), "foo().")
  checkToString(Descriptor.Method("foo", "(+1)"), "foo(+1).")
  checkToString(Descriptor.Type("Foo"), "Foo#")
  checkToString(Descriptor.Package("foo"), "foo/")
  checkToString(Descriptor.Parameter("x"), "(x)")
  checkToString(Descriptor.TypeParameter("A"), "[A]")
  checkToString(Descriptor.Term("a b"), "`a b`.")
  checkToString(Descriptor.Term("+"), "`+`.")
  checkToString(Descriptor.Type(""), "``#")
  checkToString(Descriptor.Type("::"), "`::`#")
  checkToString(Descriptor.Method("<init>", "()"), "`<init>`().")
  checkToString(Descriptor.Method("???", "()"), "`???`().")
  checkToString(Descriptor.Method("value_=", "()"), "`value_=`().")
  checkToString(Descriptor.Term("a;b"), "`a;b`.")

  // Names.encode: "" -> ``; valid Java identifiers pass through (including $-led names); anything with
  // an illegal start or part is backtick-wrapped.
  checkEncode("", "``")
  checkEncode("foo", "foo")
  checkEncode("Foo123", "Foo123")
  checkEncode("foo_bar", "foo_bar")
  checkEncode("$anonfun", "$anonfun")
  // A compiler-encoded name is itself a valid Java identifier, so encode leaves it
  // untouched — which is why producers must decode (e.g. $qmark$qmark$qmark -> ???)
  // before encoding. See the "Symbol name" rule in docs/semanticdb/specification.md.
  checkEncode("$qmark$qmark$qmark", "$qmark$qmark$qmark")
  checkEncode("a b", "`a b`")
  checkEncode("+", "`+`")
  checkEncode("???", "`???`")
  checkEncode("value_=", "`value_=`")
  checkEncode("1abc", "`1abc`")
  checkEncode("<init>", "`<init>`")
  checkEncode("a;b", "`a;b`")
  // Unicode letters are valid Java identifier characters, so they pass through unquoted.
  checkEncode("naïve", "naïve")

  test("Names constants") {
    assertEquals(Names.RootPackage.value, "_root_")
    assertEquals(Names.EmptyPackage.value, "_empty_")
    assertEquals(Names.PackageObject.value, "package")
    assertEquals(Names.Constructor.value, "<init>")
  }

  test("Name toString is its value") {
    assertEquals(Names.TermName("x").toString, "x")
    assertEquals(Names.TypeName("A").toString, "A")
  }

}
