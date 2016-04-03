// package scala.meta.tests
// package api

import org.scalatest._
import org.scalameta.tests._

class PublicSuite extends FunSuite {
  test("quasiquotes without import") {
    assert(typecheckError("""
      q"hello"
    """) === "value q is not a member of StringContext")
  }

  test("quasiquotes without static dialect") {
    assert(typecheckError("""
      import scala.meta._
      implicit val dialect: scala.meta.Dialect = ???
      q"hello"
    """) === "dialect does not have precise enough type to be used in quasiquotes (to fix this, import something from scala.dialects, e.g. scala.meta.dialects.Scala211)")
  }

  test("quasiquotes when everything's correct") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"hello"
    """) === "")
  }

  test("parse without import") {
    assert(typecheckError("""
      "".parse[scala.meta.Term]
    """) === "value parse is not a member of String")
  }

  test("parse without input-likeness") {
    assert(typecheckError("""
      import scala.meta._
      1.parse[Term]
    """) === "don't know how to convert Int to scala.meta.inputs.Input")
  }

  test("parse without parseability") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      "".parse[Int]
    """) === "don't know how to parse into Int")
  }

  test("parse when everything's correct (static dialect)") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      "".parse[Term]
    """) === "")
  }

  test("parse when everything's correct (dynamic dialect)") {
    assert(typecheckError("""
      import scala.meta._
      implicit val dialect: scala.meta.Dialect = ???
      "".parse[Term]
    """) === "")
  }

  test("parse with various input types") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      (??? : Input).parse[Term]
      (??? : String).parse[Term]
      (??? : java.io.File).parse[Term]
      (??? : Tokens).parse[Term]
      (??? : Array[Char]).parse[Term]
    """) === "")
  }

  test("tokens without import") {
    assert(typecheckError("""
      "".tokens
    """) === "value tokens is not a member of String")
  }

  test("tokens without input-likeness") {
    assert(typecheckError("""
      import scala.meta._
      1.tokens
    """) === "don't know how to convert Int to scala.meta.inputs.Input")
  }

  test("tokens when everything's correct (static dialect)") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      "".tokens
    """) === "")
  }

  test("tokens when everything's correct (dynamic dialect)") {
    assert(typecheckError("""
      import scala.meta._
      implicit val dialect: scala.meta.Dialect = ???
      "".tokens
    """) === "")
  }

  test("tokens with various input types") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      (??? : Input).tokens
      (??? : String).tokens
      (??? : java.io.File).tokens
      (??? : Tokens).tokens
      (??? : Array[Char]).tokens
    """) === "")
  }

  test("show[Code] without import") {
    assert(typecheckError("""
      (??? : scala.meta.Tree).show[Code]
    """) === "not found: type Code")
  }

  test("show[Code] when everything's correct (static dialect)") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      (??? : Tree).show[Code]
    """) === "")
  }

  test("show[Code] when everything's correct (dynamic dialect)") {
    assert(typecheckError("""
      import scala.meta._
      implicit val dialect: scala.meta.Dialect = ???
      (??? : Tree).show[Code]
    """) === "")
  }

  test("show[Syntax] without import") {
    assert(typecheckError("""
      (??? : scala.meta.Tree).show[Syntax]
    """) === "not found: type Syntax")
  }

  test("show[Syntax] when everything's correct (static dialect)") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      (??? : Tree).show[Syntax]
    """) === "")
  }

  test("show[Syntax] when everything's correct (dynamic dialect)") {
    assert(typecheckError("""
      import scala.meta._
      implicit val dialect: scala.meta.Dialect = ???
      (??? : Tree).show[Syntax]
    """) === "")
  }

  test("show[Raw] without import") {
    assert(typecheckError("""
      (??? : scala.meta.Tree).show[Raw]
    """) === "not found: type Raw")
  }

  test("show[Raw] when everything's correct") {
    assert(typecheckError("""
      import scala.meta._
      (??? : Tree).show[Raw]
    """) === "")
  }

  test("show[Structure] without import") {
    assert(typecheckError("""
      (??? : scala.meta.Tree).show[Structure]
    """) === "not found: type Structure")
  }

  test("show[Structure] when everything's correct") {
    assert(typecheckError("""
      import scala.meta._
      (??? : Tree).show[Structure]
    """) === "")
  }
}
