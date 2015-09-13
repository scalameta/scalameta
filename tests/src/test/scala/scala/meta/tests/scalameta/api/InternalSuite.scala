// package scala.meta.tests
// package scalameta
// package api

import org.scalatest._
import org.scalameta.tests._

class InternalSuite extends FunSuite {
  test("Prefix.ContentType") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      def test[T >: Type <: Type] = ???
      test[s.Prefix.ContentType]
      test[s.Prefix#ContentType]
    """) === "")
  }

  test("Prefix.map") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      val x: s.Prefix = ???
      val x1: s.Prefix = x.map(tpe => tpe: Type)
    """) === "")
  }

  test("Prefix.flatMap") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      val x: s.Prefix = ???
      val x1: s.Prefix = x.flatMap(tpe => ??? : s.Prefix)
    """) === "")
  }

  test("Denotation.ContentType") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      type T1 = s.Denotation.ContentType
      type T2 = s.Denotation#ContentType
    """) === "type ContentType is not a member of object scala.meta.internal.semantic.Denotation")
  }

  test("Denotation.map") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      val x: s.Denotation = ???
      val x1: s.Denotation = x.map((prefix, symbol) => prefix)
      val x2: s.Denotation = x.map((prefix, symbol) => symbol)
      val x3: s.Denotation = x.map((prefix, symbol) => List(symbol))
      val x4: s.Denotation = x.map((prefix, symbol) => (prefix, symbol))
      val x5: s.Denotation = x.map((prefix, symbol) => (prefix, List(symbol)))
    """) === "")
  }

  test("Denotation.flatMap") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      val x: s.Denotation = ???
      val x1: s.Denotation = x.flatMap((prefix, symbol) => ??? : s.Denotation)
    """) === "")
  }

  test("Typing.ContentType") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      val x: s.Typing = ???
      println(x.map(tpe => q"List[$tpe]"))
    """) === """
|type mismatch when unquoting;
| found   : x.ContentType
|    (which expands to)  scala.meta.Type.Arg
| required: scala.meta.Type
    """.trim.stripMargin)
  }

  test("Typing.map") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      val x: s.Typing = ???
      val x1: s.Typing = x.map(tpe => tpe: Type.Arg)
    """) === "")
  }

  test("Typing.flatMap") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      val x: s.Typing = ???
      val x1: s.Typing = x.flatMap(tpe => ??? : s.Typing)
    """) === "")
  }

  test("Expansion.ContentType") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      def test[T >: Term <: Term] = ???
      test[s.Expansion.ContentType]
      test[s.Expansion#ContentType]
    """) === "")
  }

  test("Expansion.map") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      val x: s.Expansion = ???
      val x1: s.Expansion = x.map(term => term: Term)
    """) === "")
  }

  test("Expansion.flatMap") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      import scala.meta.internal.{semantic => s}
      val x: s.Expansion = ???
      val x1: s.Expansion = x.flatMap(term => ??? : s.Expansion)
    """) === "")
  }
}
