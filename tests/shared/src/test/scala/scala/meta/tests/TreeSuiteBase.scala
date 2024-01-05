package scala.meta.tests

import munit._

import scala.meta._
import scala.meta.tests.parsers.CommonTrees

abstract class TreeSuiteBase extends FunSuite with CommonTrees {

  def emptyArgClause = Seq.empty[Term.ArgClause]

  protected def assertStruct(obtained: Tree)(expected: String)(implicit loc: munit.Location): Unit =
    assertNoDiff(obtained.structure, expected)

  protected def assertTree(obtained: Tree)(expected: Tree)(implicit loc: munit.Location): Unit =
    assertStruct(obtained)(expected.structure)

  protected def assertTrees(
      obtained: Tree*
  )(expected: Tree*)(implicit loc: munit.Location): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case (o, e) => assertTree(o)(e) }
  }

  protected def assertTree(obtained: Option[Tree])(expected: Option[Tree])(
      implicit loc: munit.Location
  ): Unit =
    (obtained, expected) match {
      case (Some(o), Some(e)) => assertTree(o)(e)
      case _ => assertEquals(obtained, expected)
    }

  protected def assertSyntax(obtained: Tree, syntax: String = null)(expected: Tree)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit =
    assertNoDiff(obtained.reprint, Option(syntax).getOrElse(expected.reprint), expected.structure)

  protected def checkTree(obtained: Tree, syntax: String = null)(expected: Tree)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    assertTree(obtained)(expected)
    assertSyntax(obtained, syntax)(expected)
  }

  protected def checkTrees(obtained: Tree*)(expected: Tree*)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case (o, e) => checkTree(o)(e) }
  }

  protected def checkTreesWithSyntax(obtained: Tree*)(syntax: String*)(expected: Tree*)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    assertEquals(obtained.length, syntax.length)
    checkTreesWithSyntax(obtained.zip(syntax): _*)(expected: _*)
  }

  protected def checkTreesWithSyntax(obtained: (Tree, String)*)(expected: Tree*)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case ((o, s), e) => checkTree(o, s)(e) }
  }

  protected implicit class implicitTree[T <: Tree](tree: T) {
    def reprint(implicit dialect: Dialect): String =
      scala.meta.internal.prettyprinters.TreeSyntax.reprint(tree).toString
  }

  protected def assertOriginalSyntax(tree: Tree, original: String)(
      implicit loc: munit.Location
  ): Unit = {
    assertNoDiff(tree.toString, original, tree.structure)
  }

  protected def assertWithOriginalSyntax(tree: Tree, original: String, reprinted: String)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    val struct = tree.structure
    assertNoDiff(tree.reprint, reprinted, struct)
    assertNoDiff(tree.toString, original, struct)
  }

  protected def assertWithOriginalSyntax(trees: Tree*)(original: String*)(reprinted: String*)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    assertEquals(trees.length, original.length)
    assertEquals(trees.length, reprinted.length)
    trees.zip(original).zip(reprinted).foreach { case ((t, o), r) =>
      assertWithOriginalSyntax(t, o, r)
    }
  }

}
