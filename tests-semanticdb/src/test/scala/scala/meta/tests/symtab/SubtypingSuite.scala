package scala.meta.tests.symtab

import scala.meta.internal.semanticdb.{AnnotatedType, ClassSignature, ExistentialType,
  StructuralType, SymbolInformation, Type, TypeRef, TypeSignature, UniversalType, WithType}
import scala.meta.internal.symtab.{GlobalSymbolTable, LocalSymbolTable}
import scala.meta.io.Classpath
import scala.meta.tests.BuildInfo
import scala.meta.tests.metacp.Library

import munit.FunSuite

/**
 * Erased-subtyping tests for `SymbolTable.isSubtypeOf` (scalameta#1213). Covers positive and
 * negative cases, including the cross-language (Scala -> Java) boundary, type aliases on the parent
 * path, and cycle safety. Uses `assert` per project convention.
 */
class SubtypingSuite extends FunSuite {

  private val classpath = Library.scalaLibrary.classpath() ++
    Classpath(BuildInfo.databaseClasspath +: BuildInfo.classDirectories: _*)
  private val symtab = GlobalSymbolTable(classpath, includeJdk = true)

  private def isSubtype(child: String, parent: String)(implicit loc: munit.Location): Unit =
    test(s"$child <: $parent")(assert(symtab.isSubtypeOf(child, parent)))

  private def isNotSubtype(child: String, parent: String)(implicit loc: munit.Location): Unit =
    test(s"$child </: $parent")(assert(!symtab.isSubtypeOf(child, parent)))

  isSubtype("scala/Option#", "scala/Option#") // reflexive
  isSubtype("scala/Some#", "scala/Option#") // direct parent
  isSubtype("scala/Some#", "scala/AnyRef#") // transitive to the top
  isSubtype("scala/Some#", "scala/Any#")
  isSubtype("scala/collection/immutable/List#", "scala/collection/immutable/Seq#") // transitive
  isSubtype("scala/collection/immutable/List#", "scala/collection/Seq#")
  // cross-language Scala -> Java *through a scala-library type alias*: scala/MatchError# extends
  // scala/package.RuntimeException#, an alias of java/lang/RuntimeException#.
  isSubtype("scala/MatchError#", "java/lang/RuntimeException#")
  isSubtype("scala/MatchError#", "java/lang/Throwable#") // and on up the Java chain
  isSubtype("scala/collection/immutable/List#", "java/io/Serializable#") // -> Java interface

  isNotSubtype("scala/Some#", "scala/Int#") // unrelated
  isNotSubtype("scala/Option#", "scala/Some#") // a parent is not a subtype of its child
  isNotSubtype("does/not/Exist#", "scala/Any#") // unknown child
  isNotSubtype("scala/Option#", "does/not/Exist#") // unknown parent
  isNotSubtype("does/not/Exist#", "does/not/Exist#") // unknown symbol fails closed, even reflexive

  test("cycle-safe over LocalSymbolTable") {
    // A extends B, B extends A — the traversal must terminate.
    def cls(sym: String, parent: String): SymbolInformation = SymbolInformation(
      symbol = sym,
      signature = ClassSignature(parents = List(TypeRef(symbol = parent))),
    )
    val local = LocalSymbolTable(List(cls("a/A#", "a/B#"), cls("a/B#", "a/A#")))
    assert(local.isSubtypeOf("a/A#", "a/B#"))
    assert(local.isSubtypeOf("a/A#", "a/A#")) // reflexive
    assert(!local.isSubtypeOf("a/A#", "scala/Any#")) // terminates, returns false
  }

  test("follows type aliases whose upper bound is a wrapped type") {
    def cls(sym: String, parents: String*): SymbolInformation = SymbolInformation(
      symbol = sym,
      signature = ClassSignature(parents = parents.map(p => TypeRef(symbol = p))),
    )
    def aliasOf(sym: String, upper: Type): SymbolInformation =
      SymbolInformation(symbol = sym, signature = TypeSignature(upperBound = upper))
    val base = TypeRef(symbol = "x/Base#")
    // the type wrappers semanticdb codegen emits (see scalacp/TypeOps.scala); a refined type is
    // StructuralType(WithType(...)). The walk must unwrap each to reach x/Base#.
    val local = LocalSymbolTable {
      List(
        cls("x/Base#"),
        aliasOf("x/Structural#", StructuralType(WithType(List(base)))),
        aliasOf("x/Annotated#", AnnotatedType(tpe = base)),
        aliasOf("x/Existential#", ExistentialType(tpe = base)),
        aliasOf("x/Universal#", UniversalType(tpe = base)),
        cls("x/ViaStructural#", "x/Structural#"),
        cls("x/ViaAnnotated#", "x/Annotated#"),
        cls("x/ViaExistential#", "x/Existential#"),
        cls("x/ViaUniversal#", "x/Universal#"),
      )
    }
    assert(local.isSubtypeOf("x/ViaStructural#", "x/Base#"))
    assert(local.isSubtypeOf("x/ViaAnnotated#", "x/Base#"))
    assert(local.isSubtypeOf("x/ViaExistential#", "x/Base#"))
    assert(local.isSubtypeOf("x/ViaUniversal#", "x/Base#"))
  }
}
