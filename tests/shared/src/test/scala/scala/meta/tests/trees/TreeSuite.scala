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

  test("parentOrNull/parentOrNoTree walk the parent chain without Option") {
    val tree = dialects.Scala213("val x = y").parse[Stat].get
    val name = tree.collect { case n: Term.Name => n }.find(_.value == "y").get
    // one step up reaches the enclosing tree, matching the Option-based parent
    assertEquals(name.parentOrNull, name.parent.get)
    assertEquals(name.parentOrNoTree, name.parent.get)
    // `y`'s parent is the root `Defn.Val`, whose own parent is missing
    assert(name.parentOrNull ne null)
    assertEquals(name.parentOrNull.parentOrNull, null: Tree)
    assertEquals(name.parentOrNoTree.parentOrNoTree, Tree.NoTree)
    assertEquals(tree.parentOrNull, null: Tree)
    assertEquals(tree.parentOrNoTree, Tree.NoTree)
  }

  test("parentOrNull is null-safe; both bottom out correctly at the root/NoTree fixed point") {
    // parentOrNull guards a null receiver (its chain can yield null); parentOrNoTree does not
    assertEquals((null: Tree).parentOrNull, null: Tree)
    assertEquals(Tree.NoTree.parentOrNull, null: Tree)
    assertEquals(Tree.NoTree.parentOrNoTree, Tree.NoTree)
    // the sentinel is inert: it is not any real node category
    assert(!Tree.NoTree.is[Term])
    assert(!Tree.NoTree.is[Stat])
  }

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

  test("Comment: newlines and a blank line after it") {
    def comment(text: String, newlinesAfter: Int) = Tree.Comment.newBuilder(List(Lit.String(text)))
      .newlinesAfter(newlinesAfter).result()
    val mlc0 = comment("/* c */", 0)
    val mlc1 = comment("/* c */", 1)
    val mlc2 = comment("/* c */", 2)
    val slc0 = comment("// c", 0)
    assertEquals(List(mlc0, mlc1, mlc2, slc0).map(_.hasNewlinesAfter), List(false, true, true, true))
    assertEquals(List(mlc0, mlc1, mlc2, slc0).map(_.hasBlankAfter), List(false, false, true, false))
    assertEquals(List(mlc1, mlc0).hasNewlinesAfter, false)
    assertEquals(List(mlc0, mlc1).hasNewlinesAfter, true)
    assertEquals(List(mlc1).hasNewlinesBeforeLast, false)
    assertEquals(List(mlc0, mlc0).hasNewlinesBeforeLast, false)
    assertEquals(List(mlc1, mlc0).hasNewlinesBeforeLast, true)
    assertEquals(List(mlc0, slc0, mlc0).hasNewlinesBeforeLast, true)
  }

  test("Comments: newlines and a blank line around the group") {
    def comment(newlinesAfter: Int) = Tree.Comment.newBuilder(List(Lit.String("/* c */")))
      .newlinesAfter(newlinesAfter).result()
    def comments(newlinesBefore: Int, newlinesAfter: Int*) = Tree.Comments
      .newBuilder(newlinesAfter.map(comment).toList).newlinesBefore(newlinesBefore).result()
    val attached = comments(0, 0)
    val detached = comments(1, 0)
    val blank = comments(2, 0)
    val inner = comments(0, 1, 0)
    val after = comments(0, 0, 1)
    val all = List(attached, detached, blank, inner, after)
    assertEquals(all.map(_.hasBlankBefore), List(false, false, true, false, false))
    assertEquals(all.map(_.hasNewlinesBeforeLast), List(false, true, true, true, false))
    assertEquals(all.map(_.hasNewlinesAfter), List(false, false, false, false, true))
    assertEquals(all.map(_.hasNewlinesBeforeOrAfter), List(false, true, true, false, true))
  }

}
