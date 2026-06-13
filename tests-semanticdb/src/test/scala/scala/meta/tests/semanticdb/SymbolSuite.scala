package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.Scala._

import munit.FunSuite

class SymbolSuite extends FunSuite {

  def checkMultiSyntax(symbols: List[String], expected: String): Unit =
    test(" syntax: " + symbols.toString()) {
      val obtained = Symbols.Multi(symbols)
      assertEquals(obtained, expected)
    }

  // Symbols.Multi(symbols).asMulti must reconstruct the original members. Note that it flattens
  // nesting: an already-multi member (e.g. ";a.;b.") is concatenated verbatim by Multi and then
  // re-split by asMulti, so the round-trip target is symbols.distinct.flatMap(_.asMulti). The empty
  // list is the one asymmetric case: Multi(Nil) == "" while "".asMulti == List(""), not Nil.
  def checkMultiRoundtrip(symbols: List[String]): Unit = test("  multi: " + symbols.toString) {
    val expected = symbols.distinct.flatMap(_.asMulti) match {
      case Nil => List("")
      case xs => xs
    }
    assertEquals(Symbols.Multi(symbols).asMulti, expected)
  }

  def checkGlobal(symbol: String): Unit = test(" global: " + symbol)(assert(symbol.isGlobal))

  def checkNotGlobal(symbol: String): Unit = test("!global: " + symbol)(assert(!symbol.isGlobal))

  def checkLocal(symbol: String): Unit = test("  local: " + symbol)(assert(symbol.isLocal))

  def checkNotLocal(symbol: String): Unit = test(" !local: " + symbol)(assert(!symbol.isLocal))

  def check(sym: String)(f: String => Boolean): Unit = test(sym)(assert(f(sym)))

  // Named boolean assertion, used when a predicate is exercised on a symbol that other tests also
  // use: test names must be unique, so we cannot reuse `check`, which names the test after the symbol.
  def checkProp(name: String)(cond: => Boolean): Unit = test(name)(assert(cond))

  // Symbols.Global builds a global symbol from an owner and a descriptor, and the result deconstructs
  // back: .desc returns the descriptor and .owner returns the owner. The RootPackage owner is elided
  // from the string (Global("_root_/", Package("scala")) == "scala/", not "_root_/scala/"), yet .owner
  // still reconstructs "_root_/". The only symbol whose .owner does not round-trip is the root package
  // itself, which has no owner (Symbols.None).
  def checkGlobalSym(owner: String, desc: Descriptor, expected: String): Unit =
    test(" Global: " + expected) {
      assertEquals(Symbols.Global(owner, desc), expected)
      assertEquals(expected.desc, desc)
      if (!expected.isRootPackage) assertEquals(expected.owner, owner)
    }

  def checkLocalSym(id: Int, expected: String): Unit =
    test("  Local: " + expected)(assertEquals(Symbols.Local(id), expected))

  def checkOwner(symbol: String, expected: String): Unit =
    test("  owner: " + symbol)(assertEquals(symbol.owner, expected))

  def checkOwnerChain(symbol: String, expected: List[String]): Unit =
    test("  chain: " + symbol)(assertEquals(symbol.ownerChain, expected))

  def checkDesc(symbol: String, expected: Descriptor): Unit =
    test("   desc: " + symbol)(assertEquals(symbol.desc, expected))

  checkMultiSyntax(Nil, "")
  checkMultiSyntax("a." :: Nil, "a.")
  checkMultiSyntax("a." :: "a." :: Nil, "a.")
  checkMultiSyntax("a." :: "b." :: Nil, ";a.;b.")
  checkMultiSyntax(";a.;b." :: ";c.;d." :: Nil, ";a.;b.;c.;d.")
  // A single element that is itself a multi-symbol is returned unchanged.
  checkMultiSyntax(";a.;b." :: Nil, ";a.;b.")

  checkMultiRoundtrip(Nil)
  checkMultiRoundtrip("com/Bar#" :: Nil)
  checkMultiRoundtrip("com/Bar#" :: "com.Bar." :: Nil)
  checkMultiRoundtrip("com/`; ;`#" :: "com.`; ;`." :: Nil)
  checkMultiRoundtrip("a" :: "b" :: "" :: Nil)
  checkMultiRoundtrip(";_root_/;_empty_/" :: "_star_." :: Nil)
  checkMultiRoundtrip("com/Bar#" :: "com/Bar#" :: Nil)
  checkMultiRoundtrip("a." :: "b." :: "a." :: Nil)
  checkMultiRoundtrip(";a.;b." :: Nil)

  checkGlobal("com/Bar#")
  checkGlobal("com/Bar.")
  checkGlobal("com/Bar.(a)")
  checkGlobal("com/Bar.[a]")
  checkGlobal(Symbols.RootPackage)
  checkGlobal(Symbols.EmptyPackage)
  checkNotGlobal(";com/Bar#;com/Bar.")
  checkNotGlobal("local1")
  checkNotGlobal(Symbols.None)

  checkLocal("local1")
  checkNotLocal(";local1;local2")
  checkNotLocal("com/Bar#")
  checkNotLocal(";com/Bar#;com/Bar.")
  checkNotLocal(Symbols.None)
  checkNotLocal(Symbols.RootPackage)
  checkNotLocal(Symbols.EmptyPackage)

  check("com/Predef.")(_.isTerm)
  check("com/Class#")(_.isType)
  check("com/")(_.isPackage)
  check(";com/;org/")(!_.isPackage)
  check("com/Class#(a)")(_.isParameter)
  check("com/Class#[A]")(_.isTypeParameter)

  // isNone / isRootPackage / isEmptyPackage / isMulti, positive and negative.
  checkProp("isNone: empty")(Symbols.None.isNone)
  checkProp("!isNone: root")(!Symbols.RootPackage.isNone)
  checkProp("!isNone: local")(!"local1".isNone)
  checkProp("!isNone: global")(!"com/Bar#".isNone)

  checkProp("isRootPackage: _root_")(Symbols.RootPackage.isRootPackage)
  checkProp("!isRootPackage: _empty_")(!Symbols.EmptyPackage.isRootPackage)
  checkProp("!isRootPackage: empty")(!Symbols.None.isRootPackage)

  checkProp("isEmptyPackage: _empty_")(Symbols.EmptyPackage.isEmptyPackage)
  checkProp("!isEmptyPackage: _root_")(!Symbols.RootPackage.isEmptyPackage)
  checkProp("!isEmptyPackage: empty")(!Symbols.None.isEmptyPackage)

  checkProp("isMulti: two")(";a.;b.".isMulti)
  checkProp("!isMulti: single")(!"a.".isMulti)
  checkProp("!isMulti: empty")(!Symbols.None.isMulti)
  checkProp("!isMulti: local")(!"local1".isMulti)

  // The type predicates are all guarded by !isNone && !isMulti: confirm both guards bite, and that a
  // term is not a type (and vice versa).
  checkProp("!isTerm: empty")(!Symbols.None.isTerm)
  checkProp("!isType: empty")(!Symbols.None.isType)
  checkProp("!isPackage: empty")(!Symbols.None.isPackage)
  checkProp("!isParameter: empty")(!Symbols.None.isParameter)
  checkProp("!isTypeParameter: empty")(!Symbols.None.isTypeParameter)
  checkProp("!isTerm: multi")(!";com/Predef.;org/Predef.".isTerm)
  checkProp("!isType: multi")(!";com/A#;org/B#".isType)
  checkProp("!isType: term")(!"com/Predef.".isType)
  checkProp("!isTerm: type")(!"com/Class#".isTerm)

  // Symbols.Global construction + round-trip through .owner/.desc.
  checkGlobalSym(Symbols.RootPackage, Descriptor.Package("scala"), "scala/")
  checkGlobalSym("scala/", Descriptor.Type("Int"), "scala/Int#")
  checkGlobalSym("scala/", Descriptor.Term("Predef"), "scala/Predef.")
  checkGlobalSym("scala/Int#", Descriptor.Method("foo", "()"), "scala/Int#foo().")
  checkGlobalSym("scala/Int#", Descriptor.Method("foo", "(+1)"), "scala/Int#foo(+1).")
  checkGlobalSym("scala/Int#foo().", Descriptor.Parameter("x"), "scala/Int#foo().(x)")
  checkGlobalSym("scala/Int#foo().", Descriptor.TypeParameter("A"), "scala/Int#foo().[A]")
  // EmptyPackage is NOT elided (only RootPackage is).
  checkGlobalSym(Symbols.EmptyPackage, Descriptor.Type("Foo"), "_empty_/Foo#")
  // The constructor name <init> is not a valid identifier, so Global backtick-encodes it.
  checkGlobalSym(
    "scala/Foo#",
    Descriptor.Method(Names.Constructor.value, "()"),
    "scala/Foo#`<init>`().",
  )

  // The root, empty, and none symbols match the SemanticDB spec. Both the root and empty package
  // symbols are constructible via Symbols.Global from the root package (their names are
  // Names.RootPackage / Names.EmptyPackage); the RootPackage-owner elision even collapses
  // Global(_root_/, Package("_root_")) back to "_root_/" itself.
  test("spec values") {
    assertEquals(Symbols.RootPackage, "_root_/")
    assertEquals(Symbols.EmptyPackage, "_empty_/")
    assertEquals(Symbols.None, "")
  }
  checkGlobalSym(
    Symbols.RootPackage,
    Descriptor.Package(Names.RootPackage.value),
    Symbols.RootPackage,
  )
  checkGlobalSym(
    Symbols.RootPackage,
    Descriptor.Package(Names.EmptyPackage.value),
    Symbols.EmptyPackage,
  )

  checkLocalSym(0, "local0")
  checkLocalSym(1, "local1")
  checkLocalSym(42, "local42")
  // A local symbol is neither global nor carries a descriptor.
  checkProp("local isLocal")(Symbols.Local(1).isLocal)
  checkProp("local !isGlobal")(!Symbols.Local(1).isGlobal)

  // owner drops the trailing descriptor; a top-level symbol's owner is RootPackage; RootPackage has
  // no owner; and non-global symbols (None / local / multi) have no owner.
  checkOwner("scala/Int#", "scala/")
  checkOwner("scala/Predef.x.", "scala/Predef.")
  checkOwner("scala/Int#foo().", "scala/Int#")
  checkOwner("scala/", Symbols.RootPackage)
  checkOwner(Symbols.EmptyPackage, Symbols.RootPackage)
  checkOwner(Symbols.RootPackage, Symbols.None)
  checkOwner(Symbols.None, Symbols.None)
  checkOwner("local1", Symbols.None)
  checkOwner(";com/A#;org/B#", Symbols.None)
  checkOwner("com/`a;b`#", "com/")
  checkOwner("com/`a#b`#", "com/")
  checkOwner("a/``#", "a/")

  // ownerChain is root-to-self and, for globals, always begins with _root_/ even though _root_/ is
  // never written into the symbol string.
  checkOwnerChain("scala/Int#", List(Symbols.RootPackage, "scala/", "scala/Int#"))
  checkOwnerChain(
    "scala/Predef.x.",
    List(Symbols.RootPackage, "scala/", "scala/Predef.", "scala/Predef.x."),
  )
  checkOwnerChain(Symbols.RootPackage, List(Symbols.RootPackage))
  checkOwnerChain(Symbols.EmptyPackage, List(Symbols.RootPackage, Symbols.EmptyPackage))
  checkOwnerChain(Symbols.None, Nil)
  checkOwnerChain("local1", List("local1"))

  // desc extracts the trailing descriptor; non-global symbols have Descriptor.None.
  checkDesc("scala/Int#", Descriptor.Type("Int"))
  checkDesc("scala/Predef.", Descriptor.Term("Predef"))
  checkDesc("scala/", Descriptor.Package("scala"))
  checkDesc("scala/Int#foo().", Descriptor.Method("foo", "()"))
  checkDesc("scala/Int#foo(+1).", Descriptor.Method("foo", "(+1)"))
  checkDesc("scala/m().(x)", Descriptor.Parameter("x"))
  checkDesc("scala/m().[A]", Descriptor.TypeParameter("A"))
  // Backtick-quoted descriptor values: a ; or # inside backticks is part of the name, not a
  // delimiter, and an empty identifier is a pair of backticks.
  checkDesc("com/`a;b`#", Descriptor.Type("a;b"))
  checkDesc("com/`a#b`#", Descriptor.Type("a#b"))
  checkDesc("a/``#", Descriptor.Type(""))
  // Non-global strings are handled gracefully: .desc is None, with no parsing and no error.
  checkDesc("foo", Descriptor.None)
  checkDesc("local1", Descriptor.None)
  checkDesc(Symbols.None, Descriptor.None)
  checkDesc(";com/A#;org/B#", Descriptor.None)

}
