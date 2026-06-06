package scala.meta.tests
package trees

import scala.meta._
import scala.meta.internal.trees._

import munit._

class TreeSuite extends TreeSuiteBase {
  test("Name.unapply") {
    assert(Name.unapply(Term.Name("a")).contains("a"))
    assert(Name.unapply(Type.Name("a")).contains("a"))
  }

  // https://github.com/scalameta/scalameta/issues/1193
  // The same `Term.Name` shape appears both at binding sites (definitions) and use sites
  // (references); `isDefinition`/`isReference` let callers tell them apart.
  test("Name.isDefinition/isReference: val binding vs use") {
    val tree = dialects.Scala213("val x = y").parse[Stat].get
    val names = tree.collect { case n: Term.Name => n.value -> n }.toMap
    assert(names("x").isDefinition)
    assert(!names("x").isReference)
    assert(names("y").isReference)
    assert(!names("y").isDefinition)
  }

  test("Name.isDefinition/isReference: def binding vs use") {
    val tree = dialects.Scala213("def foo = bar").parse[Stat].get
    val names = tree.collect { case n: Term.Name => n.value -> n }.toMap
    assert(names("foo").isDefinition)
    assert(names("bar").isReference)
  }

  test("Name.isReference: a parentless name is a reference") {
    val name = Term.Name("x")
    assert(name.isReference)
    assert(!name.isDefinition)
  }

  Seq(("+", Unary.Plus), ("-", Unary.Minus), ("~", Unary.Tilde), ("!", Unary.Not)).foreach {
    case (op, unary) => test(s"Unary.$unary") {
        assertEquals(unary.op, op)
        assertEquals(Unary.opMap.get(op), Some(unary))
        assert(op.isUnaryOp)
      }
  }

  test(s"Unary opMap size")(assertEquals(Unary.opMap.size, 4))

  test("#4351 copy preserves dialect: scala3-specific tree with inline") {
    implicit val dialect: Dialect = dialects.Scala3
    val tree = Defn
      .Def(Nil, "m", Nil, None, blk(Defn.Def(Mod.Inline() :: Nil, "n", Nil, None, Lit.Unit())))
    val copy = {
      implicit val dialect: Dialect = dialects.Scala213
      tree.copy(body = tree.body)
    }
    assertEquals(tree.origin.dialectOpt, Some(dialects.Scala3))
    assertEquals(copy.origin.dialectOpt, Some(dialects.Scala3))
    assertEquals(tree.structure, copy.structure)

    val expectedSyntax =
      """|def m = {
         |  inline def n = ()
         |}""".stripMargin.lf2nl
    // `.syntax` uses implicit dialect unless the tree is parsed
    assertEquals(tree.syntax, expectedSyntax)
    assertEquals(copy.syntax, expectedSyntax)
    // however, `.toString` attempts to take the dialect from the tree
    assertEquals(tree.toString, expectedSyntax)
    assertEquals(copy.toString, expectedSyntax)
  }

}
