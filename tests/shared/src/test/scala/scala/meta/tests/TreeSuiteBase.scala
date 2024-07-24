package scala.meta.tests

import org.scalameta.internal.ScalaCompat
import scala.meta._
import scala.meta.tests.parsers.CommonTrees
import scala.meta.trees.Origin

import munit._

abstract class TreeSuiteBase extends FunSuite with CommonTrees {

  def emptyArgClause = Seq.empty[Term.ArgClause]

  protected def assertStruct(obtained: Tree, extraClue: String = "")(
      expected: String
  )(implicit loc: munit.Location): Unit = {
    def msg = TestHelpers.getMessageWithExtraClue("tree structure not equal", extraClue)
    assertNoDiff(obtained.structure, expected, msg)
  }

  protected def assertTree(obtained: Tree, extraClue: String = "")(expected: Tree)(implicit
      loc: munit.Location
  ): Unit = assertTreeStruct(obtained, extraClue)(expected, expected.structure)

  protected def assertTreeStruct(
      obtained: Tree,
      extraClue: String = ""
  )(expected: Tree, expectedStruct: String)(implicit loc: munit.Location): Unit = {
    assertStruct(obtained, extraClue)(expectedStruct)
    expected.origin match {
      case Origin.None =>
        fail(TestHelpers.getMessageWithExtraClue("origin should not be None", extraClue))
      case _ =>
    }
  }

  protected def assertTrees(
      obtained: Tree*
  )(expected: Tree*)(implicit loc: munit.Location): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case (o, e) => assertTree(o)(e) }
  }

  protected def assertTree(
      obtained: Option[Tree]
  )(expected: Option[Tree])(implicit loc: munit.Location): Unit = (obtained, expected) match {
    case (Some(o), Some(e)) => assertTree(o)(e)
    case _ => assertEquals(obtained, expected)
  }

  protected def assertSyntax(obtained: Tree, syntax: String = null)(
      expected: Tree
  )(implicit loc: munit.Location, dialect: Dialect): Unit =
    assertSyntaxWithClue(obtained, syntax)(expected, expected.structure)

  protected def assertSyntaxWithClue(
      obtained: Tree
  )(syntax: String)(clue: => Any)(implicit loc: munit.Location, dialect: Dialect): String = {
    val reprinted = obtained.reprint
    if (syntax.nonEmpty) assertNoDiff(reprinted, syntax, clue)
    reprinted
  }

  protected def assertSyntaxWithClue(
      obtained: Tree,
      syntax: String = null
  )(expected: Tree, clue: => Any)(implicit loc: munit.Location, dialect: Dialect): Unit =
    assertSyntaxWithClue(obtained)(TestHelpers.getSyntax(expected.reprint, syntax))(clue)

  protected def checkTree(obtained: Tree, syntax: String = null)(
      expected: Tree
  )(implicit loc: munit.Location, dialect: Dialect): Unit = {
    val expectedStruct = expected.structure
    assertTreeStruct(obtained)(expected, expectedStruct)
    assertSyntaxWithClue(obtained, syntax)(expected, expectedStruct)
  }

  protected def checkTrees(
      obtained: Tree*
  )(expected: Tree*)(implicit loc: munit.Location, dialect: Dialect): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case (o, e) => checkTree(o)(e) }
  }

  protected def checkTreesWithSyntax(
      obtained: Tree*
  )(syntax: String*)(expected: Tree*)(implicit loc: munit.Location, dialect: Dialect): Unit = {
    assertEquals(obtained.length, syntax.length)
    checkTreesWithSyntax(obtained.zip(syntax): _*)(expected: _*)
  }

  protected def checkTreesWithSyntax(
      obtained: (Tree, String)*
  )(expected: Tree*)(implicit loc: munit.Location, dialect: Dialect): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case ((o, s), e) => checkTree(o, s)(e) }
  }

  protected implicit class implicitTree[T <: Tree](tree: T) {
    def reprint(implicit dialect: Dialect): String = scala.meta.internal.prettyprinters.TreeSyntax
      .reprint(tree).toString
  }

  protected implicit class ImplicitString(value: String) {
    def tq(repl: String = "QQQ"): String = value.replace(repl, "\"\"\"")
    def lf2nl: String = if (ScalaCompat.EOL == "\n") value else value.replace("\n", ScalaCompat.EOL)
  }

  protected def assertOriginalSyntax(tree: Tree, original: String)(implicit
      loc: munit.Location
  ): Unit = assertNoDiff(tree.toString, original, tree.structure)

  protected def assertWithOriginalSyntax(tree: Tree, original: String, reprinted: String)(implicit
      loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    val struct = tree.structure
    assertNoDiff(tree.reprint, reprinted, struct)
    assertNoDiff(tree.toString, original, struct)
  }

  protected def assertWithOriginalSyntax(
      trees: Tree*
  )(original: String*)(reprinted: String*)(implicit loc: munit.Location, dialect: Dialect): Unit = {
    assertEquals(trees.length, original.length)
    assertEquals(trees.length, reprinted.length)
    trees.zip(original).zip(reprinted).foreach { case ((t, o), r) =>
      assertWithOriginalSyntax(t, o, r)
    }
  }

}
