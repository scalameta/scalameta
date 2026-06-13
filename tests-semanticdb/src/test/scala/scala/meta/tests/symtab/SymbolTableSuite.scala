package scala.meta.tests.symtab

import scala.meta.internal.symtab.{AggregateSymbolTable, GlobalSymbolTable, LocalSymbolTable}
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
  check("scala/AnyRef#")(_.isClass)
  check("scala/Option#[A]")(_.isTypeParameter)
  check("scala/Predef.assert().(assertion)")(_.isParameter)
  check("scala/Predef.assume().")(_.isMethod)
  check("scala/collection/immutable/`::`.")(_.isObject)
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
  private val assertOverloads = Set("scala/Predef.assert().", "scala/Predef.assert(+1).")

  test("overloads: overloaded method returns all same-owner candidates")(
    assertEquals(globalSymtab.overloads("scala/Predef.assert().").toSet, assertOverloads),
  )

  test("overloads: querying via the resolved overload returns the same candidates")(assertEquals(
    // proves the issue's use case: scalac resolves to one overload, caller recovers the whole set
    globalSymtab.overloads("scala/Predef.assert(+1).").toSet,
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

}
