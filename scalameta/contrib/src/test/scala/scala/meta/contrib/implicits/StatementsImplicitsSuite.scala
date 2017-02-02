package scala.meta.contrib.implicits

import org.scalatest.FunSuite

import scala.meta._
import scala.meta.contrib._

class StatementsImplicitsSuite extends FunSuite {

  private val classA = q"class a"

  private val valA = q"val a = 2"
  private val valB = q"val b = 2"

  private val classWithVal = q"class a { val a = 2 }"
  private val classWith2Vals = q"class a { val a = 2; val b = 2 }"
  private val classWith2ValsInverted = q"class a { val b = 2; val a = 2 }"

  test("getStats") {
    assert(classA.getStats.isEmpty)
    assert(classWithVal.getStats.size === 1)
  }

  test("replaceStats") {
    assert(classA.replaceStats(valA :: Nil).equal[Structurally](classWithVal))
  }

  test("removeAllStats") {
    assert(classWithVal.deleteAllStats.getStats.isEmpty)
  }

//  TODO: Enable
//  test("removeStat") {
//    assert(classWith2Vals.removeStat(valA).getStats.size === 1)
//  }
//
//  test("removeStats") {
//    assert(classWith2Vals.removeStats(valA :: valB :: Nil).getStats.isEmpty)
//  }

  test("appendStat") {
    assert(classA.appendStat(valA).equal[Structurally](classWithVal))
    assert(classWithVal.appendStat(valB).equal[Structurally](classWith2Vals))
  }

  test("appendStats") {
    assert(classA.appendStats(valA :: valB :: Nil).equal[Structurally](classWith2Vals))
    assert(classA.appendStats(valB :: valA :: Nil).equal[Structurally](classWith2ValsInverted))
  }

  test("prependStat") {
    assert(classA.prependStat(valA).equal[Structurally](classWithVal))
    assert(classWithVal.prependStat(valB).equal[Structurally](classWith2ValsInverted))
  }

  test("prependStats") {
    assert(classA.prependStats(valB :: valA :: Nil).equal[Structurally](classWith2ValsInverted))
    assert(classA.prependStats(valA :: valB :: Nil).equal[Structurally](classWith2Vals))
    assert(classWithVal.prependStats(valB :: Nil).equal[Structurally](classWith2ValsInverted))
  }

  test("classDefs") {
    assert(q"object foo { }".classDefs.isEmpty)
    assert(q"object foo { class a }".classDefs.size === 1)
    assert(q"object foo { class a; object b }".classDefs.size === 1)
  }

  test("objectDefs") {
    assert(q"object foo { }".objectDefs.isEmpty)
    assert(q"object foo { object a }".objectDefs.size === 1)
    assert(q"object foo { class a; object b }".objectDefs.size === 1)
  }

  test("traitDefs") {
    assert(q"object foo { }".classDefs.isEmpty)
    assert(q"object foo { trait a }".traitDefs.size === 1)
    assert(q"object foo { class a; trait b }".traitDefs.size === 1)
  }

  test("varDefs") {
    assert(q"object foo { }".varDefs.isEmpty)
    assert(q"object foo { var a = 1 }".varDefs.size === 1)
    assert(q"object foo { class a; var b = 1 }".varDefs.size === 1)
  }

  test("valDefs") {
    assert(q"object foo { }".valDefs.isEmpty)
    assert(q"object foo { val a = 1 }".valDefs.size === 1)
    assert(q"object foo { class a; val b = 1 }".valDefs.size === 1)
  }

  test("defDefs") {
    assert(q"object foo { }".defDefs.isEmpty)
    assert(q"object foo { def a = 1 }".defDefs.size === 1)
    assert(q"object foo { class a; def b = 1 }".defDefs.size === 1)
  }

  test("typeDefs") {
    assert(q"object foo { }".typeDefs.isEmpty)
    assert(q"object foo { type A = Foo }".typeDefs.size === 1)
    assert(q"object foo { class a; type A = Foo }".typeDefs.size === 1)
  }
}
