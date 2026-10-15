package scala.meta.tests.symtab

import scala.meta.internal.symtab._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.Classpath
import scala.meta.tests.BuildInfo
import scala.meta.tests.metacp.Library

import munit.FunSuite

class SymbolTableSuite extends FunSuite {

  private val classpath = Library.scalaLibrary.classpath() ++
    Classpath(BuildInfo.databaseClasspath +: BuildInfo.classDirectories: _*)

  private val globalSymtab = GlobalSymbolTable(classpath, includeJdk = true)

  def checkNotExists(symbol: String)(implicit loc: munit.Location): Unit = {
    val name = if (symbol.isEmpty) "<nosymbol>" else symbol
    test(name) {
      val obtained = globalSymtab.info(symbol)
      assert(obtained.isEmpty, symbol)
    }
  }

  def check(symbol: String)(f: s.SymbolInformation => Boolean)(implicit loc: munit.Location): Unit =
    test(symbol) {
      val obtained = globalSymtab.info(symbol)
      assert(obtained.nonEmpty, symbol)
      assert(f(obtained.get), obtained.get.toProtoString)
    }

  // jar classpath entries
  check("_empty_/")(_.isPackage)
  check("_root_/")(_.isPackage)
  check("java/util/ArrayList#size.")(_.isField)
  check("java/util/Map#Entry#")(_.isInterface)
  check("scala/Any#asInstanceOf().")(_.isMethod)
  check("scala/Any#`==`().")(_.isMethod)
  check("scala/AnyRef#")(_.isClass)
  check("scala/Option#[A]")(_.isTypeParameter)
  check("scala/Predef.assert().(assertion)")(_.isParameter)
  check("scala/Predef.assume().")(_.isMethod)
  check("scala/Predef.`???`().")(_.isMethod)
  check("scala/collection/immutable/`::`.")(_.isObject)
  check("scala/collection/immutable/`::`#")(_.isClass)
  check("scala/collection/mutable/StringBuilder#`<init>`().")(_.isConstructor)
  check("scala/concurrent/Future#")(_.isTrait)
  check("scala/package.")(_.isPackageObject)
  check("scala/package.Either#")(_.isType)
  check("scala/package.Either.")(_.isMethod)
  check("scala/reflect/package.materializeClassTag().")(_.isMacro)
  check("scala/util/")(_.isPackage)

  // directory classpath entries
  check("example/Example.")(_.isObject)
  check("_empty_/A#")(_.isClass)
  check("org/scalameta/data/data#")(_.isClass)

  checkNotExists("")
  checkNotExists("local20")
  checkNotExists("foo/bar/")
  checkNotExists("foo.bar/")
  checkNotExists("does/not/Exist#")
  checkNotExists("does/not/Exist.")
  checkNotExists("does/not/Exist.(a)")
  checkNotExists("does/not/`e-x-i-s-t`#")

  test("LocalSymbolTable.info") {
    val runtimeClass = globalSymtab.info("scala/reflect/ClassTag#runtimeClass().").get
    val returnType = runtimeClass.signature.asInstanceOf[s.MethodSignature].returnType
    val scope = returnType.asInstanceOf[s.ExistentialType].declarations.get
    val localSymtab = LocalSymbolTable(scope.hardlinks)
    val aggregateSymtab = AggregateSymbolTable(List(localSymtab, globalSymtab))
    val local0 = aggregateSymtab.info("local0").get
    assert(local0.isType)
    assertEquals(local0.displayName, "_")
  }

  test("SymbolTable.toString") {
    val localSymtab = LocalSymbolTable(List(s.SymbolInformation("local3")))
    val aggregateSymbolTable = AggregateSymbolTable(List(localSymtab, globalSymtab))
    val string = aggregateSymbolTable.toString
    assert(string.contains("LocalSymbolTable"))
    assert(string.contains("GlobalSymbolTable"))
    assert(string.contains("AggregateSymbolTable"))
    assert(string.contains("entries"))
  }

  // https://github.com/scalameta/scalameta/issues/1298
  // `scala.Predef.assert` is overloaded: assert(assertion) and assert(assertion, message).
  // Overloads come back in declaration order, matching the disambiguator sequence (the base
  // overload, then +1, +2, ...).
  private val assertOverloads = List("scala/Predef.assert().", "scala/Predef.assert(+1).")

  test("overloads: overloaded method returns all same-owner candidates")(
    assertEquals(globalSymtab.overloads("scala/Predef.assert()."), assertOverloads),
  )

  test("overloads: querying via the resolved overload returns the same candidates")(assertEquals(
    // proves the issue's use case: scalac resolves to one overload, caller recovers the whole set
    globalSymtab.overloads("scala/Predef.assert(+1)."),
    assertOverloads,
  ))

  test("overloads: non-overloaded method returns just itself")(assertEquals(
    globalSymtab.overloads("scala/Any#asInstanceOf()."),
    List("scala/Any#asInstanceOf()."),
  ))

  test("overloads: method with an unknown owner is Nil, not a fake singleton")(
    // "could not inspect" must stay distinct from "no overloads"
    assertEquals(globalSymtab.overloads("does/not/Exist#foo()."), Nil),
  )

  test("overloads: a method absent from its owner is Nil, not fabricated")(
    // owner (scala/Predef.) resolves, but it declares no such method
    assertEquals(globalSymtab.overloads("scala/Predef.thisIsNotAMethod()."), Nil),
  )

  test("overloads: a local symbol is Nil")(
    // local overloads can't be inspected from a symbol alone
    assertEquals(globalSymtab.overloads("local0"), Nil),
  )

  test("overloads: unresolved multi-symbol returns its members")(
    assertEquals(globalSymtab.overloads(";a/B#f().;a/B#f(+1)."), List("a/B#f().", "a/B#f(+1).")),
  )

  test("overloads: empty symbol is Nil")(assertEquals(globalSymtab.overloads(""), Nil))

  test("overloads: reads overloads from a hardlinked declarations scope") {
    // GlobalSymbolTable declarations are symlinks; this covers the hardlinks arm of Scope.symbols
    // and the default method running through Aggregate/Local tables.
    val foo1 = "_empty_/Box#foo()."
    val foo2 = "_empty_/Box#foo(+1)."
    val box = s.SymbolInformation(
      symbol = "_empty_/Box#",
      kind = s.SymbolInformation.Kind.CLASS,
      signature = s.ClassSignature(declarations =
        Some(s.Scope(hardlinks = List(s.SymbolInformation(foo1), s.SymbolInformation(foo2)))),
      ),
    )
    val symtab = AggregateSymbolTable(List(LocalSymbolTable(List(box)), globalSymtab))
    assertEquals(symtab.overloads(foo1), List(foo1, foo2))
  }

  test("overloads: a non-method global symbol is Nil")(
    assertEquals(globalSymtab.overloads("scala/Option#"), Nil),
  )

  test("overloads: an owner without a class signature is Nil") {
    // defensive: the owner resolves but its signature can't list method declarations
    val owner = s.SymbolInformation(
      symbol = "_empty_/v.",
      kind = s.SymbolInformation.Kind.METHOD,
      signature = s.ValueSignature(s.NoType),
    )
    val symtab = LocalSymbolTable(List(owner))
    assertEquals(symtab.overloads("_empty_/v.foo()."), Nil)
  }

  test("overloads: overloaded constructors are candidates") {
    val ctors = globalSymtab.overloads("scala/collection/mutable/StringBuilder#`<init>`().")
    assert(ctors.lengthCompare(1) > 0, ctors.toString)
    assert(ctors.contains("scala/collection/mutable/StringBuilder#`<init>`()."), ctors.toString)
  }

  // ---- hierarchy-aware overloads(tpe, name) ----

  // a class `sym` extending `parents`, declaring (symlinks to) the methods `decls`
  private def synthClass(sym: String, parents: List[String], decls: List[String]) = s
    .SymbolInformation(
      symbol = sym,
      kind = s.SymbolInformation.Kind.CLASS,
      signature = s.ClassSignature(
        parents = parents.map(p => s.TypeRef(symbol = p)),
        declarations = Some(s.Scope(symlinks = decls)),
      ),
    )

  // a one-parameter method `sym` whose parameter has type `paramType` (carried inline as a hardlink)
  private def synthMethod(sym: String, paramType: s.Type): s.SymbolInformation = s.SymbolInformation(
    symbol = sym,
    kind = s.SymbolInformation.Kind.METHOD,
    signature = s.MethodSignature(parameterLists =
      List(s.Scope(hardlinks =
        List(s.SymbolInformation(
          symbol = sym + "(x)",
          kind = s.SymbolInformation.Kind.PARAMETER,
          signature = s.ValueSignature(paramType),
        )),
      )),
    ),
  )
  private def synthMethod(sym: String, paramType: String): s.SymbolInformation =
    synthMethod(sym, s.TypeRef(symbol = paramType))

  test("overloads(tpe, name): inherited and own overloads are both returned, most-derived first") {
    val symtab = LocalSymbolTable(List(
      synthClass("_empty_/T#", Nil, List("_empty_/T#foo().")),
      synthMethod("_empty_/T#foo().", "_empty_/A#"),
      synthClass("_empty_/C#", List("_empty_/T#"), List("_empty_/C#foo().")),
      synthMethod("_empty_/C#foo().", "_empty_/B#"),
    ))
    assertEquals(symtab.overloads("_empty_/C#", "foo"), List("_empty_/C#foo().", "_empty_/T#foo()."))
  }

  test("overloads(tpe, name): an override is deduplicated to the most-derived method") {
    val symtab = LocalSymbolTable(List(
      synthClass("_empty_/Base#", Nil, List("_empty_/Base#f().")),
      synthMethod("_empty_/Base#f().", "scala/Int#"),
      synthClass("_empty_/Sub#", List("_empty_/Base#"), List("_empty_/Sub#f().")),
      synthMethod("_empty_/Sub#f().", "scala/Int#"), // same erased signature -> override
    ))
    assertEquals(symtab.overloads("_empty_/Sub#", "f"), List("_empty_/Sub#f()."))
  }

  test("overloads(tpe, name): a diamond ancestor is not double-counted") {
    val symtab = LocalSymbolTable(List(
      synthClass("_empty_/Top#", Nil, List("_empty_/Top#f().")),
      synthMethod("_empty_/Top#f().", "scala/Int#"),
      synthClass("_empty_/L#", List("_empty_/Top#"), Nil),
      synthClass("_empty_/R#", List("_empty_/Top#"), Nil),
      synthClass("_empty_/D#", List("_empty_/L#", "_empty_/R#"), Nil),
    ))
    assertEquals(symtab.overloads("_empty_/D#", "f"), List("_empty_/Top#f()."))
  }

  test("overloads(tpe, name): cyclic parents terminate") {
    val symtab = LocalSymbolTable(List(
      synthClass("_empty_/A#", List("_empty_/B#"), List("_empty_/A#f().")),
      synthMethod("_empty_/A#f().", "scala/Int#"),
      synthClass("_empty_/B#", List("_empty_/A#"), Nil),
    ))
    assertEquals(symtab.overloads("_empty_/A#", "f"), List("_empty_/A#f()."))
  }

  test("overloads(tpe, name): unknown or non-class type is Nil") {
    assertEquals(globalSymtab.overloads("does/not/T#", "foo"), Nil)
    // a method symbol is not a type
    assertEquals(globalSymtab.overloads("scala/Predef.assert().", "assert"), Nil)
  }

  test("overloads(tpe, name): resolves overloads on a real classpath type")(
    // exercises the GlobalSymbolTable path end to end (object module-class signature, parameter
    // symbols resolved via symlinks, erased-signature dedup not wrongly merging the two asserts)
    assertEquals(
      globalSymtab.overloads("scala/Predef.", "assert"),
      List("scala/Predef.assert().", "scala/Predef.assert(+1)."),
    ),
  )

  // `trait Base[A] { def foo(a: A) }; class Sub extends Base[baseArg] { def foo(a: subParam) }`
  private def genericHierarchy(subParam: s.Type, baseArg: s.Type): SymbolTable = {
    def method(sym: String, paramType: s.Type) = s.SymbolInformation(
      symbol = sym,
      kind = s.SymbolInformation.Kind.METHOD,
      signature = s.MethodSignature(parameterLists =
        List(s.Scope(hardlinks =
          List(s.SymbolInformation(
            symbol = sym + "(a)",
            kind = s.SymbolInformation.Kind.PARAMETER,
            signature = s.ValueSignature(paramType),
          )),
        )),
      ),
    )
    LocalSymbolTable {
      List(
        s.SymbolInformation(
          symbol = "_empty_/Base#",
          kind = s.SymbolInformation.Kind.TRAIT,
          signature = s.ClassSignature(
            typeParameters = Some(s.Scope(symlinks = List("_empty_/Base#[A]"))),
            declarations = Some(s.Scope(symlinks = List("_empty_/Base#foo()."))),
          ),
        ),
        s.SymbolInformation(
          symbol = "_empty_/Base#[A]",
          kind = s.SymbolInformation.Kind.TYPE_PARAMETER,
          signature = s.TypeSignature(upperBound = s.TypeRef(symbol = "scala/Any#")),
        ),
        // parameter typed by the type parameter A
        method("_empty_/Base#foo().", s.TypeRef(symbol = "_empty_/Base#[A]")),
        s.SymbolInformation(
          symbol = "_empty_/Sub#",
          kind = s.SymbolInformation.Kind.CLASS,
          signature = s.ClassSignature(
            parents = List(s.TypeRef(symbol = "_empty_/Base#", typeArguments = List(baseArg))),
            declarations = Some(s.Scope(symlinks = List("_empty_/Sub#foo()."))),
          ),
        ),
        method("_empty_/Sub#foo().", subParam),
      )
    }
  }
  private def genericHierarchy(subParam: String): SymbolTable =
    genericHierarchy(s.TypeRef(symbol = subParam), s.TypeRef(symbol = "scala/Predef.String#"))

  test("overloads(tpe, name): a generic override is deduplicated via type-argument substitution")(
    // Sub#foo(String) overrides Base#foo(A) with A := String, so only the most-derived survives
    assertEquals(
      genericHierarchy("scala/Predef.String#").overloads("_empty_/Sub#", "foo"),
      List("_empty_/Sub#foo()."),
    ),
  )

  test("overloads(tpe, name): a generic hierarchy keeps genuine overloads")(
    // Sub#foo(Int) is a real overload of Base#foo(String-after-substitution), so both survive,
    // most-derived first
    assertEquals(
      genericHierarchy("scala/Int#").overloads("_empty_/Sub#", "foo"),
      List("_empty_/Sub#foo().", "_empty_/Base#foo()."),
    ),
  )

  test("overloads(tpe, name): constructors are not inherited") {
    val symtab = LocalSymbolTable(List(
      synthClass("_empty_/P#", Nil, List("_empty_/P#`<init>`().")),
      synthMethod("_empty_/P#`<init>`().", "scala/Int#"),
      synthClass("_empty_/Q#", List("_empty_/P#"), List("_empty_/Q#`<init>`().")),
      synthMethod("_empty_/Q#`<init>`().", "scala/Long#"),
    ))
    assertEquals(symtab.overloads("_empty_/Q#", "<init>"), List("_empty_/Q#`<init>`()."))
  }

  // `trait A { def g(x: Int) }; trait B extends A { override def g(x: Int) }; trait C extends A`
  private def diamond(parentsOfD: List[String]) = List(
    synthClass("_empty_/A#", Nil, List("_empty_/A#g().")),
    synthMethod("_empty_/A#g().", "scala/Int#"),
    synthClass("_empty_/B#", List("_empty_/A#"), List("_empty_/B#g().")),
    synthMethod("_empty_/B#g().", "scala/Int#"),
    synthClass("_empty_/C#", List("_empty_/A#"), Nil),
    synthClass("_empty_/D#", parentsOfD, Nil),
  )

  test("overloads(tpe, name): a diamond with the override in the first parent") {
    val symtab = LocalSymbolTable(diamond(List("_empty_/B#", "_empty_/C#")))
    assertEquals(symtab.overloads("_empty_/D#", "g"), List("_empty_/B#g()."))
  }

  test("overloads(tpe, name): a diamond with the override in the last parent") {
    val symtab = LocalSymbolTable(diamond(List("_empty_/C#", "_empty_/B#")))
    assertEquals(symtab.overloads("_empty_/D#", "g"), List("_empty_/B#g()."))
  }

  test("overloads(tpe, name): a diamond with an overload in the last parent") {
    val symtab = LocalSymbolTable(
      diamond(List("_empty_/B#", "_empty_/C#")).map(info =>
        if (info.symbol != "_empty_/C#") info
        else synthClass("_empty_/C#", List("_empty_/A#"), List("_empty_/C#g().")),
      ) :+ synthMethod("_empty_/C#g().", "scala/Predef.String#"),
    )
    assertEquals(symtab.overloads("_empty_/D#", "g"), List("_empty_/C#g().", "_empty_/B#g()."))
  }

  private val intType = s.TypeRef(symbol = "scala/Int#")
  private def typeOf(sym: String, arg: String) = s
    .TypeRef(symbol = sym, typeArguments = List(s.TypeRef(symbol = arg)))

  // `class C { def f(x: param1); def f(x: param2) }`
  private def declared(param1: s.Type, param2: s.Type) = LocalSymbolTable(List(
    synthClass("_empty_/C#", Nil, List("_empty_/C#f().", "_empty_/C#f(+1).")),
    synthMethod("_empty_/C#f().", param1),
    synthMethod("_empty_/C#f(+1).", param2),
  )).overloads("_empty_/C#", "f")

  test("overloads(tpe, name): same-owner by-value and by-name parameters")(
    assertEquals(declared(intType, s.ByNameType(intType)), List("_empty_/C#f().", "_empty_/C#f(+1).")),
  )

  test("overloads(tpe, name): same-owner arrays of different elements")(assertEquals(
    declared(typeOf("scala/Array#", "scala/Int#"), typeOf("scala/Array#", "java/lang/String#")),
    List("_empty_/C#f().", "_empty_/C#f(+1)."),
  ))

  test("overloads(tpe, name): same-owner plain and repeated parameters")(assertEquals(
    declared(intType, s.RepeatedType(intType)),
    List("_empty_/C#f().", "_empty_/C#f(+1)."),
  ))

  test("overloads(tpe, name): same-owner parameters with different type arguments")(assertEquals(
    declared(
      typeOf("scala/package.List#", "scala/Int#"),
      typeOf("scala/package.List#", "java/lang/String#"),
    ),
    List("_empty_/C#f().", "_empty_/C#f(+1)."),
  ))

  // `class Base { def f(x: baseParam) }; class Sub extends Base { def f(x: subParam) }`
  private def inherited(baseParam: s.Type, subParam: s.Type, others: s.SymbolInformation*) =
    LocalSymbolTable(
      List(
        synthClass("_empty_/Base#", Nil, List("_empty_/Base#f().")),
        synthMethod("_empty_/Base#f().", baseParam),
        synthClass("_empty_/Sub#", List("_empty_/Base#"), List("_empty_/Sub#f().")),
        synthMethod("_empty_/Sub#f().", subParam),
      ) ++ others,
    ).overloads("_empty_/Sub#", "f")

  test("overloads(tpe, name): inherited by-value and by-name parameters")(assertEquals(
    inherited(intType, s.ByNameType(intType)),
    List("_empty_/Sub#f().", "_empty_/Base#f()."),
  ))

  test("overloads(tpe, name): inherited by-name parameters")(
    assertEquals(inherited(s.ByNameType(intType), s.ByNameType(intType)), List("_empty_/Sub#f().")),
  )

  test("overloads(tpe, name): inherited arrays of different elements")(assertEquals(
    inherited(typeOf("scala/Array#", "scala/Int#"), typeOf("scala/Array#", "java/lang/String#")),
    List("_empty_/Sub#f().", "_empty_/Base#f()."),
  ))

  test("overloads(tpe, name): inherited arrays of the same element")(assertEquals(
    inherited(typeOf("scala/Array#", "scala/Int#"), typeOf("scala/Array#", "scala/Int#")),
    List("_empty_/Sub#f()."),
  ))

  test("overloads(tpe, name): inherited plain and repeated parameters")(assertEquals(
    inherited(intType, s.RepeatedType(intType)),
    List("_empty_/Sub#f().", "_empty_/Base#f()."),
  ))

  // `type IA = Array[Int]`
  private val intArrayAlias = s.SymbolInformation(
    symbol = "_empty_/p.IA#",
    kind = s.SymbolInformation.Kind.TYPE,
    signature = s.TypeSignature(
      lowerBound = typeOf("scala/Array#", "scala/Int#"),
      upperBound = typeOf("scala/Array#", "scala/Int#"),
    ),
  )

  test("overloads(tpe, name): inherited array and an alias of another array")(assertEquals(
    inherited(
      typeOf("scala/Array#", "java/lang/String#"),
      s.TypeRef(symbol = "_empty_/p.IA#"),
      intArrayAlias,
    ),
    List("_empty_/Sub#f().", "_empty_/Base#f()."),
  ))

  test("overloads(tpe, name): inherited array and an alias of the same array")(assertEquals(
    inherited(
      typeOf("scala/Array#", "scala/Int#"),
      s.TypeRef(symbol = "_empty_/p.IA#"),
      intArrayAlias,
    ),
    List("_empty_/Sub#f()."),
  ))

  test("overloads(tpe, name): a generic override with an array type argument")(assertEquals(
    genericHierarchy(typeOf("scala/Array#", "scala/Int#"), typeOf("scala/Array#", "scala/Int#"))
      .overloads("_empty_/Sub#", "foo"),
    List("_empty_/Sub#foo()."),
  ))

  test("overloads(tpe, name): a generic overload with an array type argument")(assertEquals(
    genericHierarchy(
      typeOf("scala/Array#", "java/lang/String#"),
      typeOf("scala/Array#", "scala/Int#"),
    ).overloads("_empty_/Sub#", "foo"),
    List("_empty_/Sub#foo().", "_empty_/Base#foo()."),
  ))

}
